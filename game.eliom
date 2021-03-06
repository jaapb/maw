[%%shared
	open Eliom_lib
	open Eliom_content
	open Html.D
	open Eliom_service
	open Eliom_parameter
	open Utils
	open Services
]

[%%server
	open CalendarLib
	open Maw
]

let game_menu game_id isu is_dsg is_bk =
	if is_dsg then
	[
		tr [td [a ~service:design_service [pcdata "Edit design"] game_id]];
		tr [td [a ~service:show_inscriptions_service [pcdata "Show inscriptions"] game_id]];
		tr [td [a ~service:designer_message_service [pcdata "Message players"] game_id]]
	]
	else
	(
		if not is_bk then []
		else if isu then [
			tr [td [a ~service:signup_service [pcdata "Edit inscription"] game_id]];
			tr [td [a ~service:cancel_service [pcdata "Cancel inscription"] game_id]]
		]
		else [
			tr [td [a ~service:signup_service [pcdata "Sign up"] game_id]]
		]
	)
;;

let game_page game_id () =
  let standard_game_data title loc date dsg_str d nr_inscr max_pl fn =
		h1 [pcdata title]::
		p [pcdata (Printf.sprintf "%s, %s" loc (date_or_tbd date))]::
		p [i [pcdata (Printf.sprintf "Designed by %s" dsg_str)]]::
		(match fn with
		| None -> p [pcdata "[no image]"]
		| Some f -> img ~a:[a_height 200; a_width 320] ~alt:"[Image]" ~src:(make_uri ~service:(Eliom_service.static_dir ()) [f]) ()
		)::
		p [pcdata d]::
    [if nr_inscr >= max_pl
    then p [i [pcdata "This game has reached its maximum number of inscriptions. You can still sign up, but you will be placed on a waiting list."]]
    else p [pcdata (Printf.sprintf "This game currently has %ld inscription(s), for %ld places." nr_inscr max_pl)]
		]
		in
	let%lwt u = Eliom_reference.get Maw.user in
	Lwt.catch (fun () ->
		let%lwt (title, date, loc, d, _, max_pl, _) =
			Maw_db.get_game_data game_id in
		let%lwt dsgs = Maw_db.get_game_designers game_id in
    let%lwt nr_inscr = Maw_db.get_nr_inscriptions game_id in
		let%lwt (id, _, _) = Maw_db.get_game_deadlines game_id in
		let%lwt (visible, bookable) = Maw_db.get_game_visibility game_id in
		let%lwt roles = Maw_db.get_game_roles game_id in
		let%lwt fn = Maw_db.get_picture_filename game_id in
		let dsg_str = designer_string dsgs in
		if not visible then
			unknown_game ()
		else
		match u with
	  | Not_logged_in -> container (standard_menu (game_menu game_id false false false)) 
			(standard_game_data title loc date dsg_str d nr_inscr max_pl fn)
	  | User (uid, _, _, _)
	  | Admin (_, (uid, _, _, _)) ->
			let is_dsg = is_designer uid dsgs in
			let%lwt sus = Maw_db.sign_up_status uid game_id in
			if not (visible || is_dsg)
			then unknown_game ()
			else container (standard_menu (game_menu game_id (match sus with | `Yes (_, _, _) -> true | _ -> false) is_dsg bookable))
			(standard_game_data title loc date dsg_str d nr_inscr max_pl fn @
				if not is_dsg then
				begin
					h2 [pcdata "Available teams and roles"]::
					(List.flatten (List.map (fun (team_name, roles) ->
						[
							h3 [pcdata team_name];
							ul (List.map (fun rc ->
								li [pcdata rc] 
							) (List.sort_uniq compare (remove_null (List.map snd roles))))
						]
					) roles) @
					(match sus with
					| `Yes (team, role, status) -> [
						p [
							i [pcdata (match status with
							| `Interested -> "You have registered your interest for this game."
							| `Confirmed -> "Your place for this game is confirmed."
							| `Paid -> "You have paid for this game and your place is confirmed."
							| `Potential -> "You have no account, but are still seeing this. That shouldn't happen. Weird."
							| `Waiting -> "You are on the waiting list for this game."
							| `Attended -> "You attended this game."
							| `No_show -> "You signed up for this game, but did not attend.")
							]
						];
						p [
							pcdata (Printf.sprintf "Your team preference is %s and your role preference is %s." (default "Any" team) (default "Any" role))
						]
					]
					| `Cancelled -> [
						p [pcdata "You have cancelled your inscription for this game."]
					]
					| `No -> 
						match id with
						| Some ddl when Date.compare ddl (Date.today ()) < 0 ->
							[p [pcdata "The inscription deadline for this game has passed."]]
						| _ ->
							(if not bookable
							then [p [pcdata "Inscriptions for this game will be opened later."]]
							else [])
				))
				end
				else
					[]
			)
	)
	(function
	| Not_found -> unknown_game ()
	| e -> error_page (Printexc.to_string e)
	)
;;

let _ =
	Maw_app.register ~service:game_service game_page
;;

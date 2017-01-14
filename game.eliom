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

let game_page game_id () =
  let standard_game_data title loc date dsg_fname dsg_lname d nr_inscr max_pl roles =
		h1 [pcdata title]::
		p [pcdata (Printf.sprintf "%s, %s" loc (date_or_tbd date))]::
		p [i [pcdata (Printf.sprintf "Designed by %s %s" dsg_fname dsg_lname)]]::
		p [pcdata d]::
    (if nr_inscr >= max_pl
    then p [i [pcdata "This game has reached its maximum number of inscriptions. You can still sign up, but you will be placed on a waiting list."]]
    else p [pcdata (Printf.sprintf "This game currently has %ld inscription(s), for %ld places." nr_inscr max_pl)]
		)::
		h2 [pcdata "Available teams and roles"]::
		List.flatten (List.map (fun (team_name, role_names) ->
			[
				h3 [pcdata team_name];
				ul (List.map (fun rn ->
					li [pcdata rn] 
				) role_names)
			]
		) roles)
		in
	let%lwt u = Eliom_reference.get Maw.user in
	Lwt.catch (fun () ->
		let%lwt (title, date, loc, dsg_fname, dsg_lname, dsg, d, _, max_pl, _) =
		Database.get_game_data game_id in
    let%lwt nr_inscr = Database.get_nr_inscriptions game_id in
		let%lwt (id, _, _) = Database.get_game_deadlines game_id in
		let%lwt (visible, bookable) = Database.get_game_visibility game_id in
		let%lwt roles = Database.get_game_roles game_id in
		match u with
	  | None -> container (standard_menu ()) 
			(standard_game_data title loc date dsg_fname dsg_lname d nr_inscr max_pl roles)
	  | Some (uid, _, _, _) ->
			let%lwt isu = Database.sign_up_status uid game_id in
			if not visible && uid <> dsg 	
			then unknown_game ()
			else container (standard_menu ()) 
			(standard_game_data title loc date dsg_fname dsg_lname d nr_inscr max_pl roles @
		  	if uid = dsg then
				[
					p [a ~service:design_service [pcdata "Edit the game design"] game_id];
					p [a ~service:show_inscriptions_service [pcdata "Show inscriptions for this game"] game_id];
					p [a ~service:message_service [pcdata "Send a message to players"] game_id]
				]
				else 
				begin
					match isu with
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
							];
							pcdata (Printf.sprintf " Your team preference is %s and your role preference is %s." (default "Any" team) (default "Any" role))
						];
						a ~service:signup_service [pcdata "Edit my inscription"] game_id;
						p [pcdata " "];
						a ~service:cancel_service [pcdata "Cancel my inscription"] game_id
					]
					| `Cancelled -> [
						p [pcdata "You have cancelled your inscription for this game."]
					]
					| `No -> [
						match id with
						| Some ddl when Date.compare ddl (Date.today ()) < 0 ->
							p [pcdata "The inscription deadline for this game has passed."]
						| _ ->
							(if bookable
							then a ~service:signup_service [pcdata "Sign up for this game"] game_id
							else p [pcdata "Inscriptions for this game will be opened later."])
					]
				end
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

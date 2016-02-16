{shared{
  open Eliom_lib
  open Eliom_content
  open Html5.D
	open Eliom_service.App
	open Eliom_parameter
}}

{server{
	open CalendarLib
	open Maw
	open Database
}}

let game_service = service ~path:["game"] ~get_params:(suffix (int32 "game_id")) ();;
let signup_service = service ~path:["signup"] ~get_params:(suffix (int32 "game_id")) ();;
let do_signup_service = post_service ~fallback:signup_service ~post_params:(
	bool "edit" **
	list "person" (sum (string "search") (int32 "uid") ** string "group" ** string "role_type" ** string "note")
) ();;
let show_inscriptions_service = service ~path:["inscriptions"] ~get_params:(suffix (int32 "game_id")) ();;
(*let add_friend_service = post_service ~fallback:signup_service ~post_params:(string "identifier") ();;*)

let game_page game_id () =
  let standard_game_data title loc date dsg_name d full =
		h1 [pcdata title]::
		p [pcdata (Printf.sprintf "%s, %s" loc (date_or_tbd date))]::
		p [i [pcdata (Printf.sprintf "Designed by %s" dsg_name)]]::
		p [pcdata d]::
    (if full
    then [p [i [pcdata "This game has reached its maximum number of inscriptions. You can still sign up, but you will be placed on a waiting list."]]]
    else []) in
	lwt u = Eliom_reference.get Maw.user in
	Lwt.catch (fun () -> lwt (title, date, loc, dsg_name, dsg, d, _, max_pl) =
		Database.get_game_data game_id in
    lwt nr_inscr = Database.get_nr_inscriptions game_id in
    match u with
	  | None -> container (standard_menu ()) 
			(standard_game_data title loc date dsg_name d (nr_inscr >= max_pl))
	  | Some (uid, _) ->
			lwt (signed_up, l) = Database.get_inscription_data uid game_id in
			container (standard_menu ()) 
			(standard_game_data title loc date dsg_name d (nr_inscr >= max_pl) @
		  	if uid = dsg then
				[
					p [a ~service:Design.design_service [pcdata "Edit the game design"] game_id];
					p [a ~service:show_inscriptions_service [pcdata "Show inscriptions for this game"] game_id]
				]
				else if signed_up then
				[
					p [
						i [pcdata "You are signed up for this game. "] (*;
						pcdata (Printf.sprintf "Your group preference is %s and your role preference is %s." (default "Any" g) (default "Any" r))*)
					];
					a ~service:signup_service [pcdata "Edit my inscription"] game_id
				]
				else
				[
					a ~service:signup_service [pcdata "Sign up for this game"] game_id
				]
			)
	)
	(function
	| Not_found -> unknown_game ()
	| e -> error_page (Printexc.to_string e)
	)
;;

let signup_page game_id () =
	lwt u = Eliom_reference.get Maw.user in
	Lwt.catch (fun () -> match u with
	| None -> not_logged_in ()
	| Some (uid, uname) -> 
		lwt (title, date, loc, dsg_name, dsg_id, d, _, _)  =
			Database.get_game_data game_id in
    lwt groups = Database.get_game_groups game_id in
		lwt role_types = Database.get_game_role_types game_id in
		lwt (signed_up, inscr) = Database.get_inscription_data uid game_id in
		container (standard_menu ())
		[
			h1 [pcdata title];
			p [pcdata (Printf.sprintf "%s, %s" loc (date_or_tbd date))];
		  p [i [pcdata (Printf.sprintf "Designed by %s" dsg_name)]];
			p [pcdata d];
			h2 [pcdata (if signed_up then "Edit inscription" else "Sign up")];
			Form.post_form ~service:do_signup_service
			(fun (edit, person) -> [
				table (
					tr [
						th [pcdata "Name"];
						th [pcdata "Group preference"];
						th [pcdata "Role type preference"];
						th [pcdata "Note"];
					]::
					person.it (fun ((search, uid), (group, (role_type, note))) v init ->
						let (ex_uid, ex_name, g, r, ex_note, _) = v in
						let ex_group = default "Any" g in
						let ex_role = default "Any" r in
						tr [
							if Int32.compare ex_uid 0l = 0 then
								td [
									Form.input ~input_type:`Text ~name:search Form.string
								]
							else
								td [
									Form.input ~input_type:`Hidden ~name:uid ~value:ex_uid Form.int32;
									pcdata ex_name
								]; 
							td [
								Form.select ~name:group Form.string
								(Form.Option ([], "Any", None, ex_group = "Any"))
								(List.map (fun g ->
									Form.Option ([], g, None, ex_group = g)
								) groups)
							];
							td [
								Form.select ~name:role_type Form.string
								(Form.Option ([], "Any", None, ex_role = "Any"))
								(List.map (fun r ->
									Form.Option ([], r, None, ex_role = r)
								) role_types)
							];
							td [Form.input ~input_type:`Text ~name:note ~value:ex_note Form.string]
						]::
						init
					) (inscr @ [0l, "", None, None, "", None])
					[
						tr [
							td ~a:[a_colspan 4] (
								Form.input ~input_type:`Submit ~value:(if signed_up then "Save changes" else "Sign up") Form.string::
								if signed_up
								then [Form.input ~input_type:`Hidden ~name:edit ~value:true Form.bool]
								else []
							)
						]	
					]
				)
			]) game_id
		]
	)
	(function 
	| Not_found -> unknown_game ()
	| e -> error_page (Printexc.to_string e)
	)
;;

			(*if edit then
				Database.edit_inscription game_id uid
					(if String.lowercase group = "any" then None else Some group)
					(if String.lowercase role_type = "any" then None else Some role_type)
					note
			else
				Database.signup game_id uid
					(if String.lowercase group = "any" then None else Some group)
					(if String.lowercase role_type = "any" then None else Some role_type)
					note*)

let do_signup_page game_id (edit, users) =
	let rec handle_inscriptions edit users =
		match users with
		| (Inj1 s, (g, (r, n)))::t ->
			Ocsigen_messages.console (fun () -> Printf.sprintf "%s, %s, %s, %s"
				s g r n);
			handle_inscriptions edit t
		| (Inj2 s, (g, (r, n)))::t ->
			Ocsigen_messages.console (fun () -> Printf.sprintf "%ld, %s, %s, %s"
				s g r n);
			handle_inscriptions edit t
		| _ -> Lwt.return ()
	in
	lwt u = Eliom_reference.get Maw.user in
	Lwt.catch (fun () -> match u with
	| None -> not_logged_in ()
	| Some (uid, _) -> handle_inscriptions edit users >>=
		fun () -> container (standard_menu ())
		[
			p [
				pcdata (
					if edit
					then "Changes successfully saved."
					else "You have successfully signed up for this game."
				)
			]
		]
	)
	(fun e -> error_page (Printexc.to_string e))
;;

let show_inscriptions_page game_id () =
	lwt u = Eliom_reference.get Maw.user in
	Lwt.catch (fun () -> match u with
	| None -> not_logged_in ()
	| Some (uid, _) -> lwt (title, date, loc, _, dsg_id, d, min_nr, max_nr) =
			Database.get_game_data game_id in
		lwt inscr = Database.get_inscription_list game_id in
		if uid = dsg_id then
			container (standard_menu ())
			(
				(h1 [pcdata	title])::
				[
					p [pcdata (Printf.sprintf "There are currently %d inscriptions." (List.length inscr))];
					table (
						tr [
							th [pcdata "Name"];
							th [pcdata "Group"];
							th [pcdata "Role"];
							th [pcdata "Note"]
						]::
						List.map (fun (nm, g, r, nt, _) ->
							tr [
								td [pcdata nm];
								td [pcdata (default "Any" g)];
								td [pcdata (default "Any" r)];
								td [i [pcdata nt]]
							]
						) inscr
					)
				]
			)
		else
			container (standard_menu ())
			[
				p [pcdata "You are not the designer of this game."]
			]
	)
	(function
	| Not_found -> unknown_game ()
	| e -> error_page (Printexc.to_string e)
	)
;;

let _ =
	Maw_app.register ~service:game_service game_page;
	Maw_app.register ~service:signup_service signup_page;
	Maw_app.register ~service:do_signup_service do_signup_page;
	Maw_app.register ~service:show_inscriptions_service show_inscriptions_page
;;

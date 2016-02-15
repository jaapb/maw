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
let do_signup_service = post_service ~fallback:signup_service ~post_params:(bool "edit" ** string "group" ** string "role_type" ** string "note") ();;

let game_page game_id () =
	Lwt.catch (fun () -> Eliom_reference.get Maw.user >>=
	fun u -> lwt signed_up = match u with
	| None -> Lwt.return []
	| Some (uid, _) -> Database.is_signed_up uid game_id in
	lwt data = Database.get_game_data game_id in
	match data with 
	| [(title, Some date, loc, dsg_name, dsg, d, _, _)] ->
			container (standard_menu ()) 
				((h1 [pcdata title])::
				(p [pcdata (Printf.sprintf "%s, " loc);
					pcdata (Printer.Date.sprint "%d %B %Y" date)])::
				(p [i [pcdata (Printf.sprintf "Designed by %s" dsg_name)]])::
				(p [pcdata d])::
				(match u, signed_up with
				| None, _ -> []
				| Some (uid, _), l -> 
					if uid = dsg then
						[p [a ~service:Design.design_service [pcdata "Edit the game design"] game_id]]
					else if List.mem game_id l then
				 		[p [pcdata "YOU ARE SIGNED UP FOR THIS GAME"]]
					else
						[a ~service:signup_service [pcdata "Sign up for this game"] game_id]
				))
	| _ -> unknown_game ())
	(fun e -> error_page (Printexc.to_string e))
	;;

let default d o =
	match o with
	| None -> d
	| Some x -> x;;

let signup_page game_id () =
	lwt u = Eliom_reference.get Maw.user in
	match u with
	| None -> not_logged_in ()
	| Some (uid, _) ->
		lwt data = Database.get_game_data game_id in
    lwt groups = Database.get_game_groups game_id in
		lwt role_types = Database.get_game_role_types game_id in
		lwt inscription = Database.get_inscription uid game_id in
		let ex_group, ex_role, ex_note, signed_up =
			match inscription with
			| [(g, r, n)] -> default "Any" g, default "Any" r, n, true
			| _ -> "Any", "Any", "", true in
		match data with
		| [(title, Some date, loc, dsg_name, dsg_id, d, _, _)] ->
			container (standard_menu ())
				[
					h1 [pcdata title];
					p [pcdata (Printf.sprintf "%s, " loc);
						pcdata (Printer.Date.sprint "%d %B %Y" date)];
				  p [i [pcdata (Printf.sprintf "Designed by %s" dsg_name)]];
					p [pcdata d];
					h2 [pcdata (if signed_up then "Edit inscription" else "Sign up")];
					Form.post_form ~service:do_signup_service
					(fun (edit, (group, (role_type, note))) ->
					[table [	
						tr [
							td [pcdata "Group preference:"];
							td [Form.select ~name:group Form.string
								(Form.Option ([], "Any", None, ex_group = "Any"))
								(List.map (fun g ->
									Form.Option ([], g, None, ex_group = g)
								) groups)
							]
						];
						tr [
							td [pcdata "Role type preference:"];
							td [Form.select ~name:role_type Form.string
								(Form.Option ([], "Any", None, ex_role = "Any"))
									(List.map (fun r ->
										Form.Option ([], r, None, ex_role = r)
									) role_types)
							]
						];
						tr [
							td [pcdata "Note:"];
							td [Form.input ~input_type:`Text ~name:note ~value:ex_note Form.string]
						];
						tr [
							td ~a:[a_colspan 2]
								[Form.input ~input_type:`Submit ~value:(if signed_up then "Save changes" else "Sign up") Form.string;
								Form.input ~input_type:`Hidden ~name:edit ~value:signed_up Form.bool]
						]
					]]) game_id
				]
		| _ -> unknown_game ()
	;;

let do_signup game_id (edit, (group, (role_type, note))) =
	lwt u = Eliom_reference.get Maw.user in
	match u with
	| None -> Lwt.return ()
	| Some (uid, _) -> if edit then
			Database.edit_inscription game_id uid
				(if String.lowercase group = "any" then None else Some group)
				(if String.lowercase role_type = "any" then None else Some role_type)
				note
		else
			Database.signup game_id uid
				(if String.lowercase group = "any" then None else Some group)
				(if String.lowercase role_type = "any" then None else Some role_type)
				note;;

let _ =
	Maw_app.register ~service:game_service game_page;
	Maw_app.register ~service:signup_service signup_page;
	Eliom_registration.Action.register ~service:do_signup_service do_signup
;;

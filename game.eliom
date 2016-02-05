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

let game_page game_id () =
	lwt u = Eliom_reference.get Maw.user in
	lwt signed_up = match u with
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
	| _ -> unknown_game ()
	;;

let signup_page game_id () =
	lwt u = Eliom_reference.get Maw.user in
	match u with
	| None -> not_logged_in ()
	| Some (uid, _) ->
		lwt signed_up = Database.is_signed_up uid game_id in
		lwt data = Database.get_game_data game_id in
		match signed_up, data with
		| l, [(title, Some date, loc, dsg_name, dsg_id, d, _, _)] ->
			if List.mem game_id l then
				container (standard_menu ())
				[
					p [pcdata "You have already signed up for this game."]
				]
			else
				container (standard_menu ())
				[
					p [pcdata "WIP"]
				]
		| _, _ -> unknown_game ()
	;;

let _ =
	Maw_app.register ~service:game_service game_page;
	Maw_app.register ~service:signup_service signup_page;;

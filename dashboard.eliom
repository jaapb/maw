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

let location_bar id title date loc =
	[
		a ~service:Game.game_service [pcdata title] id;
		pcdata (Printf.sprintf " (%s, " loc);
		pcdata (Printer.Date.sprint "%d %b %Y)" date)
	];;

let format_upcoming_games ug =
	table (
		List.flatten (List.map (function
		| (id, title, Some date, loc) -> 
			[tr [td (location_bar id title date loc)]]
		| _ -> []	
		) ug)
	);;

let format_my_games mg dg =
	Lwt.return (
		h2 [pcdata "My games"]::
		(match mg with
		| [] -> p [pcdata "You are not signed up for any games at the moment."]
		| l -> table (List.flatten (List.map
			(function 
			| (id, title, Some date, loc) ->
				[tr [
					td (location_bar id title date loc);
					td [pcdata "Edit inscription"]
				]]
			| _ -> []
			) l)))::
		(match dg with
		| [] -> []
		| l -> [
				h2 [pcdata "My designs"];
				table (List.flatten (List.map
				(function
				| (id, title, Some date, loc) ->
					[tr [
						td (location_bar id title date loc);
						td [a ~service:Design.design_service [pcdata "Edit design"] id] 
					]]
				| _ -> []
				) l))
			])
	);;

let dashboard_page () () =
	Lwt.catch 
	(fun () -> Database.get_upcoming_games () >>=
	fun ug -> lwt u = Eliom_reference.get Maw.user in
	lwt mg_fmt = match u with
	| None -> Lwt.return []
	| Some (uid, _) ->
		Database.get_user_games uid >>=
		fun mg -> Database.get_designer_games uid >>=
		fun dg -> format_my_games mg dg
	in
	container (standard_menu ()) (
		(h1 [pcdata "Upcoming games"])::
		(format_upcoming_games ug)::
		mg_fmt
	))
	(fun e -> error_page (Printexc.to_string e));;

let () =
	Maw_app.register ~service:dashboard_service dashboard_page;;

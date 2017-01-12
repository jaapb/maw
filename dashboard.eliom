open Eliom_lib
open Eliom_content
open Html.D
open Eliom_service
open Eliom_parameter
open Utils

[%%server
	open CalendarLib
	open Maw
	open Database
]

let location_bar id title date loc =
	[
		a ~service:Game.game_service [pcdata title] id;
		pcdata (Printf.sprintf " (%s, %s)" loc (date_or_tbd date))
	];;

let format_upcoming_games ug =
	match ug with
	| [] -> p [pcdata "Strangely, there are no games planned at all."]
	| l ->
		table (
			List.flatten (List.map (function
			| (id, title, date, loc, _, _) -> 
				[tr [td (location_bar id title date loc)]]
			) l)
		)
;;

let format_my_games mg dg =
	Lwt.return (
		h2 [pcdata "My games"]::
		(match mg with
		| [] -> p [pcdata "You are not signed up for any games at the moment."]
		| l -> table (List.flatten (List.map
			(function 
			| (id, title, date, loc, cast) ->
				[tr (
					td (location_bar id title date loc)::
					td [a ~service:Game.signup_service [pcdata "Edit inscription"] id]::
					td [a ~service:Game.cancel_service [pcdata "Cancel inscription"] id]::
					if cast
					then [td [a ~service:Game.show_casting_service [pcdata "Show casting"] id]]
					else []
				)]
			) l)))::
		(match dg with
		| [] -> []
		| l -> [
				h2 [pcdata "My designs"];
				table (List.flatten (List.map
				(function
				| (id, title, date, loc) ->
					[tr [
						td (location_bar id title date loc);
						td [a ~service:Design.design_service [pcdata "Edit design"] id];
						td [a ~service:Game.show_inscriptions_service [pcdata "Show inscriptions"] id];
						td [a ~service:Design.cast_service [pcdata "Casting"] id];
						td [a ~service:Design.message_service [pcdata "Messages"] id]
					]]
				) l))
			])
	);;

let dashboard_page () () =
	Lwt.catch (fun () ->
	 	let%lwt ug = Database.get_upcoming_games () in
		let%lwt u = Eliom_reference.get Maw.user in
		let%lwt mg_fmt = match u with
		| None -> Lwt.return []
		| Some (uid, _, _, _) -> Database.get_user_games uid >>=
				fun mg -> Database.get_designer_games uid >>=
				fun dg -> format_my_games mg dg
		in
		container (standard_menu ())
		(
			h1 [pcdata "Upcoming games"]::
			format_upcoming_games ug::
			mg_fmt
		)
	)
	(fun e -> error_page (Printexc.to_string e));;

let () =
	Maw_app.register ~service:dashboard_service dashboard_page;;

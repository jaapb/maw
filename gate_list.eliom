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

let gate_list_service =
	create ~path:(Path ["gate_list"]) ~meth:(Get (suffix (int32 "game_id"))) ();;

let gate_list_page game_id () =
	Lwt.catch (fun () ->
		let%lwt u = Eliom_reference.get Maw.user in
		let%lwt (title, date, loc, _, _, dsg_uid, _, _, _, _) =
			Database.get_game_data game_id in
		let%lwt cast = Database.get_casting game_id in
		let date_str = match date with
		| None -> "NO DATE"
		| Some d -> Printer.Date.sprint "%d %B %Y" d in
		match u with
		| None -> not_logged_in ()
		| Some (uid, _, _, _) ->
			if uid <> dsg_uid
			then error_page "You are not the designer of this game."
			else
				container (standard_menu ())
				(
					h1 [pcdata (Printf.sprintf "Gate list for %s" title)]::
					p [pcdata (Printf.sprintf "%s, %s" loc date_str)]::
					List.map (fun (t, _) ->
						h2 [pcdata t]
					) cast
				)
	)
	(fun e -> error_page (Printexc.to_string e))
;;

let () =
	Maw_app.register ~service:gate_list_service gate_list_page;;

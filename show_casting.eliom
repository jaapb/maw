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

let show_casting_page game_id () =
	Lwt.catch (fun () -> let%lwt (title, _, _, _, _, _, cp) =
			Database.get_game_data game_id in
		if cp then
		begin
			let%lwt casting = Database.get_casting game_id in
			(*let teams = List.sort_uniq (fun (t1, _, _, _, _, _, _) (t2, _, _, _, _, _, _) ->
				compare t1 t2) casting in*)
			container (standard_menu [])
			(
				h1 [pcdata (Printf.sprintf "Casting for %s" title)]::
				List.map (fun (t, roles) ->
					table ~a:[a_class ["team_table"]] (
						tr [
							th ~a:[a_class ["team_name"]; a_colspan 2] [pcdata t]
						]::
						tr [
							th ~a:[a_class ["header"]] [pcdata "Role"];
							th ~a:[a_class ["header"]] [pcdata "Name"]
						]::
						List.map (fun (rn, fname, lname, _, _, _) ->
							tr [
								td [pcdata rn];
								td [match fname, lname with
								| Some f, Some l -> pcdata (Printf.sprintf "%s %s" f l)
								| _, _ -> b [pcdata "vacancy"]
								]
							]
						) roles
					)
				) casting
			)
		end
		else error_page "The casting for this game has not been published."
	)
	(function
	| Not_found -> unknown_game ()
	| e -> error_page (Printexc.to_string e)
	)
;;

let _ =
	Maw_app.register ~service:show_casting_service show_casting_page
;;

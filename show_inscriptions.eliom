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

let rec confirm_page game_id () user_id =
	let%lwt u = Eliom_reference.get Maw.user in
	Lwt.catch (fun () -> match u with
	| None -> not_logged_in ()
	| Some (uid, _, _, _) -> 
		Database.change_status game_id user_id `Confirmed >>=
		fun () -> show_inscriptions_page game_id ())
	(fun e -> error_page (Printexc.to_string e))
and show_inscriptions_page game_id () =
	let confirm_service = create_attached_post
		~fallback:(preapply show_inscriptions_service game_id)
		~post_params:(int32 "user_id") () in
	let status_word st =
		match (Database.inscr_status_of_int32 st) with
		| `Potential-> "Potential"
		| `Interested-> "Interested"
		| `Waiting -> "Waiting"
		| `Confirmed -> "Confirmed"
		| `Paid-> "Paid"
		| `No_show -> "No-show" 
		| `Attended -> "Attended" in
	Maw_app.register ~scope:Eliom_common.default_session_scope
		~service:confirm_service (confirm_page game_id);
	let%lwt u = Eliom_reference.get Maw.user in
	Lwt.catch (fun () -> match u with
	| None -> not_logged_in ()
	| Some (uid, _, _, _) ->
		let%lwt (title, date, loc, _, _, dsg_id, d, min_nr, max_nr, _) =
			Database.get_game_data game_id in
		let%lwt inscr = Database.get_inscription_list game_id in
		if uid = dsg_id then
			container (standard_menu ())
			(
				(h1 [pcdata	title])::
				[
					p [pcdata (Printf.sprintf "There are currently %d inscriptions." (List.length inscr))];
					table (
						tr [
							th [];
							th [pcdata "Status"];
							th [pcdata "Group name"];
							th [pcdata "Player name"];
							th [pcdata "Team"];
							th [pcdata "Role"];
							th [pcdata "Note"]
						]::
						List.map (fun (fname, lname, ex_uid, t, r, nt, g, st) ->
							tr [
								td (match (Database.inscr_status_of_int32 st) with
									| `Potential | `Interested | `Waiting ->
										[Form.post_form ~service:confirm_service
										(fun u ->
											[
												Form.input ~input_type:`Hidden ~name:u ~value:ex_uid Form.int32;
												Form.input ~input_type:`Submit ~value:"Confirm" Form.string
											]
										) ()]
									| _ -> []);
								td [pcdata (status_word st)];
								td [pcdata (default "" g)];
								td [pcdata (Printf.sprintf "%s %s" fname lname)];
								td [pcdata (default "Any" t)];
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
	Maw_app.register ~service:show_inscriptions_service show_inscriptions_page
;;

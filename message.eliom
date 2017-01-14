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

let do_message_page game_id _ =
  let%lwt u = Eliom_reference.get Maw.user in
  match u with
  | None -> not_logged_in ()
  | Some (uid, _, _, _) -> 
		let%lwt (title, date, loc, _, _, dsg_id, d, min_nr, max_nr, _) =
			Database.get_game_data game_id in
		if uid <> dsg_id then error_page "You are not the designer of this game."
    else
		container (standard_menu ())
		[
			h1 [pcdata "Message sent"];
			p [pcdata "Aren't you proud of me?"] 
		]
;;

let message_page game_id () =
	let do_message_service = create ~path:(Path ["messaging"]) ~meth:(Post (suffix (int32 "game_id"), string "team" ** string "subject" ** string "contents")) () in
	Maw_app.register ~scope:Eliom_common.default_session_scope
		~service:do_message_service do_message_page;
  let%lwt u = Eliom_reference.get Maw.user in
  match u with
  | None -> not_logged_in ()
  | Some (uid, _, _, _) -> 
		let%lwt (title, date, loc, _, _, dsg_id, d, min_nr, max_nr, _) =
			Database.get_game_data game_id in
		if uid <> dsg_id then error_page "You are not the designer of this game."
    else
			let%lwt teams = Database.get_game_teams game_id in
		container (standard_menu ())
		[
			h1 [pcdata "Send message"];
			Form.post_form ~service:do_message_service (fun (dest, (subject, text)) ->
			[
				table
				[	
					tr [
						td [
							pcdata "Send to:";
							Form.select ~name:dest Form.string
							(Form.Option ([], "all", Some (pcdata "All players"), false))
							(List.map (fun t ->
								Form.Option ([], "team:t", Some (pcdata t), false)
								) teams
							)
						]
					];
					tr [
						td [
							pcdata "Subject: ";
							Form.input ~input_type:`Text ~name:subject Form.string
						];
					];
					tr [
						td [
							Form.textarea ~a:[a_cols 60; a_rows 10] ~name:text ()
						]
					];
					tr [
						td [
							Form.input ~input_type:`Submit ~value:"Send" Form.string
						]
					]
				]
			]) game_id
		]
;;

let () =
	Maw_app.register ~service:message_service message_page
;;

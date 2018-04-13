[%%shared
	open Eliom_lib
	open Eliom_content
	open Html.D
	open Eliom_service
	open Eliom_parameter
	open Services
]

[%%server
	open CalendarLib
	open Maw
]

let do_set_game_data () games =
	(try
		Lwt_list.iter_s (fun (game_id, (date_str, (location, (visible, bookable)))) ->
			let date = Printer.Date.from_fstring "%Y-%m-%d" date_str in
			Maw_db.set_game_data game_id date location visible bookable
		) games
	with Invalid_argument s ->
		Lwt.return (ignore ([%client (Eliom_lib.alert "Error: %s" ~%s: unit)])))
;;

let rec set_game_data_page () () =
	let set_game_data_service = create_attached_post ~fallback:dashboard_service
 	 ~post_params:(list "game" (int32 "game_id" ** string "date" **
		string "location" ** bool "visible" ** bool "bookable")) () in
	Eliom_registration.Action.register ~scope:Eliom_common.default_session_scope
		~service:set_game_data_service do_set_game_data;
  Lwt.catch (fun () -> let%lwt u = Eliom_reference.get Maw.user in
    match u with
    | Not_logged_in -> not_logged_in ()
    | User (_, _, _, is_admin)
    | Admin (_, (_, _, _, is_admin)) -> if not is_admin
      then error_page "You must be an administrator to access this page."
      else
      let%lwt games = Maw_db.get_upcoming_games ~all:true () in
      begin
        container (standard_menu [])
        [
          h1 [pcdata "Set game data"];
          Form.post_form ~service:set_game_data_service
          (fun (game) ->
						[table (
							tr [
								th [pcdata "Game title"];
								th [pcdata "Date"];
								th [pcdata "Location"];
								th [pcdata "V"];
								th [pcdata "B"];
							]::
							game.it (fun (game_id, (date, (location, (visible, bookable)))) (i, t, d, l, v, b) init ->
								let dstr = match d with
								| Some dt -> Printer.Date.sprint "%Y-%m-%d" dt
								| None -> "" in
								tr [
									td [
										Form.input ~input_type:`Hidden ~name:game_id ~value:i Form.int32;
										pcdata t
									];
									td [
										Form.input ~input_type:`Date ~name:date ~value:dstr Form.string
									];
									td [
										Form.input ~input_type:`Text ~name:location ~value:l Form.string
									];
									td [
										bool_checkbox visible v
									];
									td [
										bool_checkbox bookable b
									]
								]::
								init
							) games
          		[
								tr [
           	 	  td ~a:[a_colspan 3]
           	 	  [Form.input ~input_type:`Submit ~value:"Save changes" Form.string]
								]
							]
						)]
					) ()
        ]
      end
  )
  (function
  | e -> error_page (Printexc.to_string e)
  )
;;

let _ =
	Maw_app.register ~service:set_game_data_service set_game_data_page
;;

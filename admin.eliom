[%%shared
	open Eliom_lib
	open Eliom_content
	open Html.D
	open Eliom_service
	open Eliom_parameter
]

[%%server
	open CalendarLib
	open Maw
]

let add_game_service = create ~id:(Fallback admin_service)
	~meth:(Post (unit, string "title" ** int32 "designer")) ();;
let set_game_data_service = create ~id:(Fallback admin_service)
  ~meth:(Post (unit, list "game" (int32 "game_id" ** string "date" ** string "location"))) ();;

let add_game_page f () (title, designer) =
	Database.add_game title designer >>=
	fun () -> f () ()
;;

let set_game_data_page f () games =
	(try
		Lwt_list.iter_s (fun (game_id, (date_str, location)) ->
			let date = Printer.Date.from_fstring "%d/%m/%Y" date_str in
			Database.set_game_data game_id date location
		) games
	with Invalid_argument s ->
		Lwt.return (ignore ([%client (Eliom_lib.alert "Gniarf: %s" ~%s: unit)]))) >>=
	fun () -> f () ()
;;

let rec admin_page () () =
  Lwt.catch (fun () -> let%lwt u = Eliom_reference.get Maw.user in
    match u with
    | None -> not_logged_in ()
    | Some (_, _, is_admin) -> if not is_admin
      then error_page "You must be an administrator to access this page."
      else
      let%lwt games = Database.get_upcoming_games ~no_date:true () in
      let%lwt users = Database.get_user_list () in
      let (uhid, uhn) = List.hd users in
      begin
				Maw_app.register ~scope:Eliom_common.default_session_scope
					~service:add_game_service (add_game_page admin_page);
				Maw_app.register ~scope:Eliom_common.default_session_scope
					~service:set_game_data_service (set_game_data_page admin_page);
        container (standard_menu ())
        [
          h1 [pcdata "Administration"];
          h2 [pcdata "Set game date and location"];
          Form.post_form ~service:set_game_data_service
          (fun (game) ->
						[table (
							tr [
								th [pcdata "Game title"];
								th [pcdata "Date"];
								th [pcdata "Location"];
							]::
							game.it (fun (game_id, (date, location)) (i, t, d, l) init ->
								let dstr = match d with
								| Some dt -> Printer.Date.sprint "%d/%m/%Y" dt
								| None -> "" in
								tr [
									td [
										Form.input ~input_type:`Hidden ~name:game_id ~value:i Form.int32;
										pcdata t
									];
									td [
										Form.input ~input_type:`Text ~name:date ~value:dstr Form.string
									];
									td [
										Form.input ~input_type:`Text ~name:location ~value:l Form.string
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
					) ();
          h2 [pcdata "Create new game"];
          Form.post_form ~service:add_game_service (fun (title, designer) -> [
            table [
              tr
              [
                th [pcdata "Title:"];
                td [Form.input ~input_type:`Text ~name:title Form.string]
              ];
              tr
              [
                th [pcdata "Designer:"];
                td [
                  Form.select ~name:designer Form.int32
                  (Form.Option ([], uhid, Some (pcdata uhn), false))
                  (List.map (fun (id, n) ->
                    Form.Option ([], id, Some (pcdata n), false)
                  ) (List.tl users))
                ]
              ];
              tr
              [
                td ~a:[a_colspan 2]
                [Form.input ~input_type:`Submit ~value:"Save changes" Form.string]
              ]
            ]
          ]) ();
					h2 [pcdata "Manually confirm users"];
        ]
      end
  )
  (function
  | e -> error_page (Printexc.to_string e)
  )
;;

let _ =
	Maw_app.register ~service:admin_service admin_page
;;

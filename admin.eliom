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

let add_game_page f () (title, designer) =
	Database.add_game title designer >>=
	fun () -> f () ()
;;

let set_game_data_page f () games =
	(try
		Lwt_list.iter_s (fun (game_id, (date_str, (location, (visible, bookable)))) ->
			let date = Printer.Date.from_fstring "%Y-%m-%d" date_str in
			Database.set_game_data game_id date location visible bookable
		) games
	with Invalid_argument s ->
		Lwt.return (ignore ([%client (Eliom_lib.alert "Error: %s" ~%s: unit)]))) >>=
	fun () -> f () ()
;;

let rec admin_page () () =
	let add_game_service = create_attached_post ~fallback:admin_service
		~post_params:(string "title" ** int32 "designer") () in
	let set_game_data_service = create_attached_post ~fallback:admin_service
 	 ~post_params:(list "game" (int32 "game_id" ** string "date" **
		string "location" ** bool "visible" ** bool "bookable")) () in
  Lwt.catch (fun () -> let%lwt u = Eliom_reference.get Maw.user in
    match u with
    | None -> not_logged_in ()
    | Some (_, _, _, is_admin) -> if not is_admin
      then error_page "You must be an administrator to access this page."
      else
      let%lwt games = Database.get_upcoming_games ~all:true () in
      let%lwt users = Database.get_users ~unconfirmed:true () in
			let nonconf = List.filter (fun (_, _, _, _, s) -> s = Some "U") users in
			let%lwt confirm_strs = Lwt_list.map_s (fun (id, _, _, _, _) ->
				Database.get_confirmation id) nonconf in
      let (uhid, uhfname, uhlname, _, _) = List.hd users in
      begin
				Maw_app.register ~scope:Eliom_common.default_session_scope
					~service:add_game_service (add_game_page admin_page);
				Maw_app.register ~scope:Eliom_common.default_session_scope
					~service:set_game_data_service (set_game_data_page admin_page);
        container (standard_menu ())
        [
          h1 [pcdata "Administration"];
          h2 [pcdata "Set game data"];
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
                  (Form.Option ([], uhid, Some (pcdata (Printf.sprintf "%s %s" uhfname uhlname)), false))
                  (List.map (fun (id, fname, lname, _, _) ->
                    Form.Option ([], id, Some (pcdata (Printf.sprintf "%s %s" fname lname)), false)
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
					table
					(
						tr [
							td ~a:[a_colspan 3]
								[pcdata (Printf.sprintf "Currently %d users are waiting for confirmation." (List.length nonconf))]
						]::
						tr [
							th [pcdata "Name"];
							th [pcdata "E-mail address"];
							th [pcdata ""]
						]::
						List.map2 (fun (id, fname, lname, email, _) cstr ->
							tr
							[
								td [pcdata (Printf.sprintf "%s %s" fname lname)];
								td [pcdata email];
								td [a ~service:confirm_user_service [pcdata "Confirm"] (id, cstr)]
							]
						) nonconf confirm_strs 
					)
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

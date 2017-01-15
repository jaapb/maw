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

let do_new_game () (title, designer) =
	Database.add_game title designer
;;

let rec new_game_page () () =
	let add_game_service = create_attached_post ~fallback:dashboard_service
		~post_params:(string "title" ** int32 "designer") () in
	Eliom_registration.Action.register ~scope:Eliom_common.default_session_scope
		~service:add_game_service do_new_game;
  Lwt.catch (fun () -> let%lwt u = Eliom_reference.get Maw.user in
    match u with
    | None -> not_logged_in ()
    | Some (_, _, _, is_admin) -> if not is_admin
      then error_page "You must be an administrator to access this page."
      else
      let%lwt users = Database.get_users ~unconfirmed:true () in
      let (uhid, uhfname, uhlname, _, _) = List.hd users in
      begin
        container (standard_menu [])
        [
          h1 [pcdata "Create new game"];
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
        ]
      end
  )
  (function
  | e -> error_page (Printexc.to_string e)
  )
;;

let _ =
	Maw_app.register ~service:new_game_service new_game_page
;;

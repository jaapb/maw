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

let do_new_provisional_user () (email, (first_name, (last_name, game_id))) =
	Database.add_provisional_user email first_name last_name game_id >>=
	fun pid -> container (standard_menu [])
	[
		h1 [pcdata "Provisional user created"];
		p [pcdata (Printf.sprintf "Please enter the number %ld as the username in the group inscription form (in the original window) and close this window." pid)];
		p [pcdata "(Yes, this should be automated - as soon as I find out how...)"]
	]
;;

let rec new_provisional_user_page ex_game_id () =
	let add_user_service = create_attached_post ~fallback:dashboard_service
		~post_params:(string "email" ** string "first_name" ** string "last_name" ** int32 "game_id") () in
	Maw_app.register ~scope:Eliom_common.default_session_scope
		~service:add_user_service do_new_provisional_user;
  Lwt.catch (fun () -> let%lwt u = Eliom_reference.get Maw.user in
    match u with
    | None -> not_logged_in ()
    | Some (uid, _, _, _) -> 
      begin
        container (standard_menu [])
        [
          h1 [pcdata "Create provisional user"];
          Form.post_form ~service:add_user_service
					(fun (email, (first_name, (last_name, game_id))) -> [
            table [
              tr
              [
                th [pcdata "E-mail:"];
                td [Form.input ~input_type:`Text ~name:email Form.string]
              ];
              tr
              [
                th [pcdata "First name:"];
                td [Form.input ~input_type:`Text ~name:first_name Form.string]
              ];
              tr
              [
                th [pcdata "Last name:"];
                td [Form.input ~input_type:`Text ~name:last_name Form.string]
              ];
              tr
              [
                td ~a:[a_colspan 2]
                [
									Form.input ~input_type:`Hidden ~name:game_id
										~value:ex_game_id Form.int32;
									Form.input ~input_type:`Submit ~value:"Create" Form.string
								]
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
	Maw_app.register ~service:new_provisional_user_service new_provisional_user_page
;;

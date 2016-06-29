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

let add_game_service = create ~id:(Path ["admin"])
  ~meth:(Post (unit, string "title" ** int32 "designer")) ();;
let set_game_data_service = create ~id:(Path ["admin"])
  ~meth:(Post (unit, int32 "game_id" ** string "date" ** string "location")) ();;

let admin_page () () =
  Lwt.catch (fun () -> let%lwt u = Eliom_reference.get Maw.user in
    match u with
    | None -> not_logged_in ()
    | Some (_, _, is_admin) -> if not is_admin
      then error_page "You must be an administrator to access this page."
      else
      let%lwt games = Database.get_upcoming_games ~no_date:true () in
      let%lwt users = Database.get_user_list () in
      let (ghid, ght, _, _) = List.hd games in
      let (uhid, uhn) = List.hd users in
      begin
        container (standard_menu ())
        [
          h1 [pcdata "Administration"];
          h2 [pcdata "Set game date and location"];
          Form.post_form ~service:set_game_data_service
          (fun (game_id, (date, location)) -> [
            table [
              tr [
                th [pcdata "Game:"];
                td [Form.select ~name:game_id Form.int32
                (Form.Option ([], ghid, Some (pcdata ght), false))
                (List.map (fun (id, t, _, _) ->
                  Form.Option ([], id, Some (pcdata t), false) 
                ) (List.tl games))
                ]
              ];
              tr [
                th [pcdata "Date:"];
                td [Form.input ~input_type:`Date ~name:date Form.string]
              ];
              tr [
                th [pcdata "Location:"];
                td [Form.input ~input_type:`Text ~name:location Form.string]
              ];
              tr [
                td ~a:[a_colspan 2]
                [Form.input ~input_type:`Submit ~value:"Save changes" Form.string]
              ] 
            ]
          ]) ();
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
          ]) ()
        ]
      end
  )
  (function
  | e -> error_page (Printexc.to_string e)
  )
;;

let add_game_page () (title, designer) =
  container (standard_menu ())
  [
    p [pcdata "Yeah."]
  ]
;;

let set_game_data_page () (game_id, (date, location)) =
  container (standard_menu ())
  [
    p [pcdata "Yeah."]
  ]
;;

let _ =
	Maw_app.register ~service:admin_service admin_page;
	Maw_app.register ~service:add_game_service add_game_page;
	Maw_app.register ~service:set_game_data_service set_game_data_page
;;

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

let do_new_game () (title, (abbr, designers)) =
	Database.add_game title abbr designers
;;

let%client add_designer users ev =
	let designer_table = Dom_html.getElementById "designer_table" in
	let m = ref 0 in
	List.iter (fun designer_tr ->
		incr m
	) (Dom.list_of_nodeList designer_table##.childNodes);
	let new_select = Html.To_dom.of_element (tr [td [
                  			Raw.select ~a:[a_name (Printf.sprintf "__co_eliom_designer.id[%d]" !m)]
                  			(List.map (fun (id, fname, lname, _, s) ->
													match s with
													| Some "H" -> option ~a:[a_value (Int32.to_string id)] (pcdata (Printf.sprintf "%s %s (HIDDEN)" fname lname))
													| _ -> option ~a:[a_value (Int32.to_string id)] (pcdata (Printf.sprintf "%s %s" fname lname))
                  			) (users))
											]]) in
	Dom.appendChild designer_table new_select
;;

let rec new_game_page () () =
	let add_game_service = create_attached_post ~fallback:dashboard_service
		~post_params:(string "title" ** string "abbreviation" ** list "designer" (int32 "id")) () in
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
          Form.post_form ~service:add_game_service (fun (title, (abbr, designers)) -> [
            table [
              tr
              [
                th [pcdata "Title:"];
                td ~a:[a_colspan 2]
									[Form.input ~input_type:`Text ~name:title Form.string]
              ];
							tr
							[
								th [pcdata "Abbreviation:"];
								td ~a:[a_colspan 2]
									[Form.input ~input_type:`Text ~name:abbr Form.string]
							];
              tr
              [
                th [pcdata "Designer(s):"];
                td [
									table ~a:[a_id "designer_table"] []
								];
								td [Raw.input ~a:[a_input_type `Button; a_value "Add designer"; a_onclick [%client add_designer ~%users]] ()]
              ];
              tr
              [
                td ~a:[a_colspan 3]
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

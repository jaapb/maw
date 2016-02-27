[%%shared
	open Eliom_lib
	open Eliom_content
	open Html5.D
	open Eliom_service.App
	open Eliom_parameter
]

[%%server
	open CalendarLib
	open Maw
]

let design_service = service ~path:["design"] ~get_params:(suffix (int32 "game_id")) ();;
let update_descr_service = post_service ~fallback:design_service ~post_params:(string "description") ();;
let update_numbers_service = post_service ~fallback:design_service ~post_params:(int32 "min" ** int32 "max") ();;
let remove_teams_service = post_service ~fallback:design_service ~post_params:(set string "teams") ();;
let add_team_service = post_service ~fallback:design_service ~post_params:(string "team") ();;
let remove_role_types_service = post_service ~fallback:design_service ~post_params:(set string "role_types") ();;
let add_role_type_service = post_service ~fallback:design_service ~post_params:(string "role_type") ();;
let casting_service = service ~path:["casting"] ~get_params:(suffix (int32 "game_id")) ();;

let design_page game_id () = 
	let%lwt u = Eliom_reference.get Maw.user in
	Lwt.catch (fun () -> match u with
	| None -> not_logged_in ()
	| Some (uid, _, _) -> 
		let%lwt (title, date, loc, _, dsg_id, d, min_nr, max_nr) =
			Database.get_game_data game_id in
		if uid <> dsg_id then error_page "You are not the designer of this game."
    else
		let%lwt teams = Database.get_game_teams game_id in
		let%lwt role_types = Database.get_game_role_types game_id in
			container (standard_menu ())
			[
				h1 [pcdata title];
				p [pcdata (Printf.sprintf "%s, %s" loc (date_or_tbd date))];
        p [a ~service:casting_service [pcdata "Cast this game"] game_id];
				Form.post_form ~service:update_descr_service (fun descr -> [
					table [
						tr [
							td [pcdata "Game description:"]
						];
						tr [	
							td [
								Form.textarea ~a:[a_cols 60; a_rows 10] ~name:descr ~value:d ()
							]
						];
						tr [
							td [
								Form.input ~input_type:`Submit ~value:"Update" Form.string
							]
						]
					]
				]) game_id;
				Form.post_form ~service:update_numbers_service (fun (min, max) -> [
					table [
						tr [
							td ~a:[a_colspan 5] [pcdata "Numbers:"]
						];
						tr [
							td [pcdata "Minimum:"];
							td [Form.input ~a:[a_size 5] ~input_type:`Text ~name:min ~value:min_nr Form.int32];
							td [pcdata "Maximum:"];
							td [Form.input ~a:[a_size 5] ~input_type:`Text ~name:max ~value:max_nr Form.int32];
							td [Form.input ~input_type:`Submit ~value:"Update" Form.string]
						]
					]
				]) game_id;
				Form.post_form ~service:remove_teams_service (fun team -> [
					table [
						tr [
							td [pcdata "Teams:"]
						];
						tr (
							match teams with
							| [] -> [td [pcdata "No teams have been entered"]]
							| h::t -> 
								[
									td [
										Form.multiple_select ~name:team Form.string
										(Form.Option ([], h, None, false))
										(List.map (fun x -> (Form.Option ([], x, None, false))) t)
									];
									td [
										Form.input ~input_type:`Submit ~value:"Remove" Form.string
									]
								]
						)
					]
				]) game_id;
				Form.post_form ~service:add_team_service (fun team -> [
					table [
						tr [
							td [Form.input ~a:[a_size 50] ~input_type:`Text ~name:team
								Form.string];
							td [Form.input ~input_type:`Submit ~value:"Add" Form.string]
						]
					]
				]) game_id;
				Form.post_form ~service:remove_role_types_service (fun role_type -> [
					table [
						tr [
							td [pcdata "Role types:"]
						];
						tr (match role_types with
						| [] -> [td [pcdata "No role types have been entered"]]
						| h::t -> 
							[
								td [
									Form.multiple_select ~name:role_type Form.string
									(Form.Option ([], h, None, false))
									(List.map (fun x -> (Form.Option ([], x, None, false))) t)
								];
								td [
									Form.input ~input_type:`Submit ~value:"Remove" Form.string
								]
							]
						)
					]
				]) game_id;
				Form.post_form ~service:add_role_type_service (fun role_type -> [
					table [
						tr [
							td [Form.input ~a:[a_size 50] ~input_type:`Text ~name:role_type
								Form.string];
							td [Form.input ~input_type:`Submit ~value:"Add" Form.string]
						]
					]
				]) game_id
			]
	)
	(function
	| Not_found -> unknown_game ()
	| e -> error_page (Printexc.to_string e)
	)
;;

let update_description game_id descr =
	let%lwt u = Eliom_reference.get Maw.user in
	match u with
	| None -> Lwt.return ()
	| Some (uid, _, _) -> Database.set_game_description game_id descr
;;

let update_numbers game_id (min, max) =
	let%lwt u = Eliom_reference.get Maw.user in
	match u with
	| None -> Lwt.return ()
	| Some (uid, _, _) -> Database.set_game_numbers game_id min max
;;

let remove_teams game_id teams =
	let%lwt u = Eliom_reference.get Maw.user in
	match u with
	| None -> Lwt.return ()
	| Some (uid, _, _) -> Database.remove_game_teams game_id teams
;;

let add_team game_id team =
	let%lwt u = Eliom_reference.get Maw.user in
	match u with
	| None -> Lwt.return ()
	| Some (uid, _, _) -> Database.add_game_team game_id team
;;

let remove_role_types game_id role_types =
	let%lwt u = Eliom_reference.get Maw.user in
	match u with
	| None -> Lwt.return ()
	| Some (uid, _, _) -> Database.remove_game_role_types game_id role_types
;;

let add_role_type game_id role_type =
	Lwt_log.ign_info_f "ADD_GROUP %ld %s" game_id role_type;
	let%lwt u = Eliom_reference.get Maw.user in
	match u with
	| None -> Lwt.return ()
	| Some (uid, _, _) -> Database.add_game_role_type game_id role_type
;;

let casting_page game_id () =
  let color_of group =
    match group with
    | None -> "white"
    | Some 1l -> "red"
    | Some 2l -> "orange"
    | Some 3l -> "yellow"
    | Some 4l -> "green"
    | Some 5l -> "cyan"
    | Some 6l -> "blue"
    | Some 7l -> "magenta"
    | _ -> "grey"
  in
  let%lwt u = Eliom_reference.get Maw.user in
  match u with
  | None -> not_logged_in ()
  | Some (uid, _, _) -> let%lwt (title, _, _, _, dsg_id, _, _, _) =
      Database.get_game_data game_id in
    if uid <> dsg_id then error_page "You are not the designer of this game."
    else
    let%lwt inscr = Database.get_inscription_list game_id in
    let%lwt teams = Database.get_game_teams game_id in
    container (standard_menu ())
    [
      table ~a:[a_id "inscr_table"]
      (tr [
        th [pcdata "Players:"]
       ]:: 
       (List.map (fun (n, _, _, _, g) ->
         tr [
           td ~a:[a_style (Printf.sprintf "background-color: %s" (color_of g))]
             [pcdata n]
         ] 
       ) inscr)
      );
      div ~a:[a_id "groups"]
      (List.map (fun t ->
        table ~a:[a_class ["group_table"]] [
          tr [
            th [pcdata t]
          ]
        ]
      ) teams)
    ]
;;

let () =
	Maw_app.register ~service:design_service design_page;;
	Eliom_registration.Action.register ~service:update_descr_service
		update_description;
	Eliom_registration.Action.register ~service:update_numbers_service
		update_numbers;
	Eliom_registration.Action.register ~service:remove_teams_service
		remove_teams;
	Eliom_registration.Action.register ~service:add_team_service
		add_team;
	Eliom_registration.Action.register ~service:remove_role_types_service
		remove_role_types;
	Eliom_registration.Action.register ~service:add_role_type_service
		add_role_type;
  Maw_app.register ~service:casting_service casting_page
;;

{shared{
	open Eliom_lib
	open Eliom_content
	open Html5.D
	open Eliom_service.App
	open Eliom_parameter
}}

{server{
	open CalendarLib
	open Maw
	open Database
}}

let design_service = service ~path:["design"] ~get_params:(suffix (int32 "game_id")) ();;
let update_descr_service = post_service ~fallback:design_service ~post_params:(string "description") ();;
let update_numbers_service = post_service ~fallback:design_service ~post_params:(int32 "min" ** int32 "max") ();;
let remove_groups_service = post_service ~fallback:design_service ~post_params:(set string "groups") ();;
let add_group_service = post_service ~fallback:design_service ~post_params:(string "group") ();;
let remove_role_types_service = post_service ~fallback:design_service ~post_params:(set string "role_types") ();;
let add_role_type_service = post_service ~fallback:design_service ~post_params:(string "role_type") ();;

let design_page game_id () = 
	lwt u = Eliom_reference.get Maw.user in
	match u with
	| None -> not_logged_in ()
	| Some (uid, _) -> lwt ni = Database.get_nr_inscriptions game_id in
		let nr_inscr = match ni with
		| [Some x] -> x
		| _ -> 0L in
		lwt groups = Database.get_game_groups game_id in
		lwt role_types = Database.get_game_role_types game_id in
		lwt data = Database.get_game_data game_id in
		match data with
		| [(title, Some date, loc, _, dsg_id, d, min_nr, max_nr)] ->
			if uid = dsg_id then
				container (standard_menu ())
				[
					h1 [pcdata title];
					p [pcdata loc; pcdata (Printer.Date.sprint ", %d %B %Y" date)];
					p [pcdata (Printf.sprintf "Currently, %Ld person(s) has/have signed up for this game." nr_inscr)];
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
						]]
					) game_id;
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
					Form.post_form ~service:remove_groups_service (fun group -> [
					table [
						tr [
							td [pcdata "Groups:"]
						];
						tr (
							match groups with
							| [] -> [td [pcdata "No groups have been entered"]]
							| h::t -> 
								[
									td [
										Form.multiple_select ~name:group Form.string
										(Form.Option ([], h, None, false))
										(List.map (fun x -> (Form.Option ([], x, None, false))) t)
									];
									td [
										Form.input ~input_type:`Submit ~value:"Remove" Form.string
									]
								]
						)
					]]) game_id;
					Form.post_form ~service:add_group_service (fun group -> [
					table [
						tr [
							td [Form.input ~a:[a_size 50] ~input_type:`Text ~name:group
								Form.string];
							td [Form.input ~input_type:`Submit ~value:"Add" Form.string]
						]
					]]) game_id;
					Form.post_form ~service:remove_role_types_service (fun role_type -> [
					table [
						tr [
							td [pcdata "Role types:"]
						];
						tr (
							match role_types with
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
					]]) game_id;
					Form.post_form ~service:add_role_type_service (fun role_type -> [
					table [
						tr [
							td [Form.input ~a:[a_size 50] ~input_type:`Text ~name:role_type
								Form.string];
							td [Form.input ~input_type:`Submit ~value:"Add" Form.string]
						]
					]]) game_id
				]
			else container (standard_menu ())
				[
					p [pcdata "You are not the designer of this game."]
				]
		| _ -> unknown_game ()
;;

let update_description game_id descr =
	lwt u = Eliom_reference.get Maw.user in
	match u with
	| None -> Lwt.return ()
	| Some (uid, _) -> Database.set_game_description game_id descr
;;

let update_numbers game_id (min, max) =
	lwt u = Eliom_reference.get Maw.user in
	match u with
	| None -> Lwt.return ()
	| Some (uid, _) -> Database.set_game_numbers game_id min max
;;

let remove_groups game_id groups =
	lwt u = Eliom_reference.get Maw.user in
	match u with
	| None -> Lwt.return ()
	| Some (uid, _) -> Database.remove_game_groups game_id groups
;;

let add_group game_id group =
	Lwt_log.ign_info_f "ADD_GROUP %ld %s" game_id group;
	lwt u = Eliom_reference.get Maw.user in
	match u with
	| None -> Lwt.return ()
	| Some (uid, _) -> Database.add_game_group game_id group
;;

let remove_role_types game_id role_types =
	lwt u = Eliom_reference.get Maw.user in
	match u with
	| None -> Lwt.return ()
	| Some (uid, _) -> Database.remove_game_role_types game_id role_types
;;

let add_role_type game_id role_type =
	Lwt_log.ign_info_f "ADD_GROUP %ld %s" game_id role_type;
	lwt u = Eliom_reference.get Maw.user in
	match u with
	| None -> Lwt.return ()
	| Some (uid, _) -> Database.add_game_role_type game_id role_type
;;

let () =
	Maw_app.register ~service:design_service design_page;;
	Eliom_registration.Action.register ~service:update_descr_service
		update_description;
	Eliom_registration.Action.register ~service:update_numbers_service
		update_numbers;
	Eliom_registration.Action.register ~service:remove_groups_service
		remove_groups;
	Eliom_registration.Action.register ~service:add_group_service
		add_group;
	Eliom_registration.Action.register ~service:remove_role_types_service
		remove_role_types;
	Eliom_registration.Action.register ~service:add_role_type_service
		add_role_type
;;

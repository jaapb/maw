[%%shared
  open Eliom_content.Html.F
	open CalendarLib
	open Maw_utils
]

(* Local actions *)
let%server edit_game_action =
	Eliom_service.create_attached_post
		~fallback:Maw_services.edit_game_service
		~post_params:(Eliom_parameter.string "blurb") ()

let%client edit_game_action =
	~%edit_game_action

(* Database functions *)
let%server get_games upcoming =
	Maw_games_db.get_games upcoming

let%client get_games =
	~%(Eliom_client.server_function [%derive.json : bool]
			(Os_session.connected_wrapper get_games))

let%server get_game_info game_id =
	Maw_games_db.get_game_info game_id

let%client get_game_info =
	~%(Eliom_client.server_function [%derive.json : int64]
			(Os_session.connected_wrapper get_game_info))

let%server get_game_designers game_id =
	Maw_games_db.get_game_designers game_id

let%client get_game_designers =
	~%(Eliom_client.server_function [%derive.json : int64]
			(Os_session.connected_wrapper get_game_designers))

let%server get_designed_games userid =
	Maw_games_db.get_designed_games userid

let%client get_designed_games =
	~%(Eliom_client.server_function [%derive.json : int64]
			(Os_session.connected_wrapper get_designed_games))

let%server get_inscription_opt (game_id, userid) =
	Lwt.catch (fun () ->
		let%lwt i = Maw_games_db.get_inscription game_id userid in
		Lwt.return_some i
	)
	(function
	| Not_found -> Lwt.return_none
	| e -> Lwt.fail e)

let%client get_inscription_opt =
	~%(Eliom_client.server_function [%derive.json : int64 * int64]
			(Os_session.connected_wrapper get_inscription_opt))

let%server sign_up (game_id, userid, message, group) =
	Ocsigen_messages.console (fun () -> Printf.sprintf "[sign_up] game_id: %Ld userid: %Ld message: %s group: %s" game_id userid message (default "<none>" group));
	Maw_games_db.sign_up game_id userid message group

let%client sign_up =
	~%(Eliom_client.server_function [%derive.json : int64 * int64 * string * string option]
			(Os_session.connected_wrapper sign_up))

let%server cancel_inscription (game_id, userid) =
	Maw_games_db.cancel_inscription game_id userid

let%client cancel_inscription =
	~%(Eliom_client.server_function [%derive.json : int64 * int64]
			(Os_session.connected_wrapper cancel_inscription))

(* Handlers *)
let%shared location_line location date =
	p [i [pcdata (default [%i18n S.tbc] location); pcdata ", "; pcdata (match date with None -> [%i18n S.tbc] | Some date -> Printer.Date.sprint "%B %d, %Y" date)]]

let%shared game_info_handler myid_o game_id () =
	let%lwt (title, location, date, blurb) = get_game_info game_id in
	let%lwt designers = get_game_designers game_id in
  Maw_container.page
    ~a:[ a_class ["os-page-main"]]
    myid_o [
			div ~a:[a_class ["content-box"]] [
				h1 [pcdata title];
				location_line location date;
				p [pcdata [%i18n S.designed_by]; pcdata " "; pcdata (String.concat " and " (List.map (fun (_, fn, ln) -> Printf.sprintf "%s %s" fn ln) designers))];
				p [pcdata (default [%i18n S.no_description] blurb)]
			]
    ]

let%shared do_edit_game game_id blurb =
	Os_msg.msg ~level:`Msg ~onload:true [%i18n S.data_saved];
	Eliom_registration.Redirection.send (Eliom_registration.Redirection Os_services.main_service)

let%shared edit_game_handler myid game_id () =
	Eliom_registration.Any.register ~service:edit_game_action 
		do_edit_game

let%shared real_edit_game_handler myid game_id () =
	let%lwt designers = get_game_designers game_id in
	if List.exists (fun (id, _, _) -> id = myid) designers then
	let%lwt (title, location, date, blurb) = get_game_info game_id in
		Maw_container.page (Some myid)
		[
			div ~a:[a_class ["content-box"]]
			[
				h1 [pcdata title];
				Form.post_form ~service:edit_game_action (fun (new_blurb) -> [
					table ~a:[a_class ["form-table"]] [
						tr [
							th [pcdata [%i18n S.game_location]];
							td [Raw.input ~a:[a_disabled (); a_input_type `Text; a_value (default [%i18n S.tbc] location)] ()]
						];
						tr [
							th [pcdata [%i18n S.game_date]];
							td [Raw.input ~a:[a_disabled (); a_input_type `Text;
								a_value (match date with None -> [%i18n S.tbc] | Some date -> Printer.Date.sprint "%B %d, %Y" date)] ()]
						];
						tr [
							th [pcdata [%i18n S.game_description]];
							td [Form.textarea ~a:[a_rows 20; a_cols 60] ~name:new_blurb ~value:(default "" blurb) ()]
						];
						tr [
							td ~a:[a_colspan 2] [Form.input ~a:[a_class ["button"]] ~input_type:`Submit ~value:[%i18n S.save] Form.string]
						]
					]
				]) game_id
			]
		]
	else
		Maw_container.page None
		[p [pcdata [%i18n S.not_game_designer]]]
			
let%server edit_game_handler myid_o game_id () =
	Eliom_registration.Any.register ~scope:Eliom_common.default_session_scope
		~service:edit_game_action do_edit_game;
	real_edit_game_handler myid_o game_id ()

let%client edit_game_handler =
	real_edit_game_handler

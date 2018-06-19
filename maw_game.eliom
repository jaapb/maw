[%%shared
  open Eliom_content.Html.F
	open CalendarLib
	open Maw_utils
]

let%server sign_up_action = Eliom_service.create_attached_post
	~fallback:Maw_services.sign_up_service
	~post_params:(Eliom_parameter.list "users" (Eliom_parameter.int64 "id")) ()

let%client sign_up_action = ~%sign_up_action

let%server get_games upcoming =
	if upcoming
	then Maw_games_db.get_upcoming_games ()
	else Maw_games_db.get_games ()

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

let%server get_designed_games user_id =
	Maw_games_db.get_designed_games user_id

let%client get_designed_games =
	~%(Eliom_client.server_function [%derive.json : int64]
			(Os_session.connected_wrapper get_designed_games))

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

let%shared do_edit_game () (game_id, blurb) =
	Os_msg.msg ~level:`Msg ~onload:true [%i18n S.data_saved];
	Eliom_registration.Redirection.send (Eliom_registration.Redirection Os_services.main_service)

let%shared edit_game_handler myid_o game_id () =
	Eliom_registration.Any.register ~service:Maw_services.edit_game_action do_edit_game;
	match myid_o with
	| None -> Maw_container.page None
			[p [pcdata [%i18n S.must_be_connected_to_see_page]]]
	| Some myid -> 
		let%lwt designers = get_game_designers game_id in
		if List.exists (fun (id, _, _) -> id = myid) designers then
		let%lwt (title, location, date, blurb) = get_game_info game_id in
			Maw_container.page (Some myid)
			[
				div ~a:[a_class ["content-box"]]
				[
					h1 [pcdata title];
					Form.post_form ~service:Maw_services.edit_game_action (fun (p_game_id, new_blurb) -> [
						Form.input ~input_type:`Hidden ~name:p_game_id ~value:game_id Form.int64;
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
								td ~a:[a_colspan 2] [Form.input ~a:[a_class ["button"]] ~input_type:`Submit ~value:"Save" Form.string]
							]
						]
					]) ()
				]
			]
		else
			Maw_container.page None
			[p [pcdata [%i18n S.not_game_designer]]]

let%shared do_sign_up game_id users =
	Eliom_registration.Action.send ()

let%shared add_to_group_form f =
	let btn = Eliom_content.Html.(
		D.button ~a:[D.a_class ["button"]] [D.pcdata "Add group member"]
	) in
	ignore [%client 
		((Lwt.async @@ fun () ->
			let btn = Eliom_content.Html.To_dom.of_element ~%btn in
			Lwt_js_events.clicks btn @@ fun _ _ ->
			let%lwt () = ~%f () in
			Lwt.return_unit)
		: unit)
	];
	Eliom_content.Html.D.div [btn]

let%shared display_group_table l =
	let rows = Eliom_shared.ReactiveData.RList.map
		[%shared
				((fun s -> Eliom_content.Html.(
						D.tr [
							D.td [Raw.input ~a:[a_input_type `Text] ()]
						]
				)) : _ -> _)
		]
		l in
	Eliom_content.Html.R.table rows

let%shared real_sign_up_handler myid_o game_id () = 
	let (group_l, group_h) = Eliom_shared.ReactiveData.RList.create [] in
	let%lwt (title, location, date, _) = get_game_info game_id in
	let group_table = display_group_table group_l in
	let form = add_to_group_form 
		[%client ((fun v -> Lwt.return (Eliom_shared.ReactiveData.RList.cons v
			~%group_h))
			: unit -> unit Lwt.t)
		] in 
	Maw_container.page myid_o
	[
		div ~a:[a_class ["content-box"]] [
			h1 [pcdata "Signing up for "; pcdata title];
			location_line location date;
		];
		div ~a:[a_class ["content-box"]] [
			div ~a:[a_class ["text-bar"]] [ 
				table [
					tr [
						td [pcdata "Group inscription"];
						td [Maw_icons.D.expand ~a:[a_title "Expand"] ()]
					]
				]
			];
			group_table;
			form
		]
	]

let%server sign_up_handler myid_o game_id () =
	Eliom_registration.Any.register ~scope:Eliom_common.default_session_scope
		~service:sign_up_action do_sign_up;
	real_sign_up_handler myid_o game_id ()

let%client sign_up_handler =
	real_sign_up_handler

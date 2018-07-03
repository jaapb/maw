[%%shared
  open Eliom_content.Html.F
	open CalendarLib
	open Maw_utils
]

let%server sign_up_action = Eliom_service.create_attached_post
	~fallback:Maw_services.sign_up_service
	~post_params:(Eliom_parameter.any) ()

let%client sign_up_action = ~%sign_up_action

let%server edit_game_action =
	Eliom_service.create_attached_post
		~fallback:Maw_services.edit_game_service
		~post_params:(Eliom_parameter.string "blurb") ()

let%client edit_game_action =
	~%edit_game_action

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

let%shared do_edit_game game_id blurb =
	Os_msg.msg ~level:`Msg ~onload:true [%i18n S.data_saved];
	Eliom_registration.Redirection.send (Eliom_registration.Redirection Os_services.main_service)

let%shared edit_game_handler myid_o game_id () =
	Eliom_registration.Any.register ~service:edit_game_action 
		do_edit_game

let%shared real_edit_game_handler myid_o game_id () =
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
								td ~a:[a_colspan 2] [Form.input ~a:[a_class ["button"]] ~input_type:`Submit ~value:"Save" Form.string]
							]
						]
					]) game_id
				]
			]
		else
			Maw_container.page None
			[p [pcdata [%i18n S.not_game_designer]]]

let%shared do_sign_up game_id users =
	Eliom_registration.Html.send (
		Eliom_content.Html.F.(html 
			(head (title (pcdata "DSU")) [])
			(body [
				table (
					List.map (fun (x, y) ->
						tr [
							td [pcdata x]; td [pcdata y]
						]
					) users
				)
			])
		))

let%shared add_to_group_button nr_s f =
	let btn = Eliom_content.Html.(
		D.button ~a:[D.a_class ["button"]; D.a_button_type `Button] [D.pcdata "Add group member"]
	) in
	ignore [%client 
		((Lwt.async @@ fun () ->
			let btn = Eliom_content.Html.To_dom.of_element ~%btn in
			Lwt_js_events.clicks btn @@ fun _ _ ->
			let nr = Eliom_shared.React.S.value ~%nr_s in
			let%lwt () = ~%f nr in
			Lwt.return_unit)
		: unit)
	];
	Eliom_content.Html.D.div [btn]

let%shared display_group_table nr l =
	let rows = Eliom_shared.ReactiveData.RList.map
		[%shared
				((fun s -> Eliom_content.Html.(
						D.tr [
							D.td [Maw_user.user_input_widget ~nr:s ()]
						]
				)) : _ -> _)
		]
		l in
	Eliom_content.Html.R.table rows

let%shared real_sign_up_handler myid_o game_id () = 
	let (group_l, group_h) = Eliom_shared.ReactiveData.RList.create [] in
	let (nr_s, nr_f) = Eliom_shared.React.S.create 0 in
	let (gh_s, gh_f) = Eliom_shared.React.S.create true in
	let%lwt (title, location, date, _) = get_game_info game_id in
	let group_table = display_group_table nr_s group_l in
	let add_btn = add_to_group_button nr_s
		[%client ((fun v ->
				Eliom_shared.ReactiveData.RList.snoc v ~%group_h;
				~%nr_f (v + 1);
				Eliom_lib.alert "v: %d nr_value: %d" v (Eliom_shared.React.S.value ~%nr_s);
				Lwt.return_unit
			)
			: int -> unit Lwt.t)
		] in 
	let title_bar = Eliom_content.Html.D.div ~a:[a_class ["text-bar"]] [
		table [
			tr [
				td [pcdata "Group inscription"];
				td [Eliom_content.Html.R.node (Eliom_shared.React.S.map
					[%shared ((fun h -> if h
						then Maw_icons.D.expand ~a:[a_title "Expand"] ()
						else Maw_icons.D.collapse ~a:[a_title "Collapse"] ()
					) : _ -> _)
					] gh_s)
				]
			]
		]] in
	ignore [%client
		((Lwt.async @@ fun () ->
			let title_bar = Eliom_content.Html.To_dom.of_element ~%title_bar in
			Lwt_js_events.clicks title_bar @@ fun _ _ ->
			let () = ~%gh_f (not (Eliom_shared.React.S.value ~%gh_s)) in
			Lwt.return_unit)
		: unit)
	];
	Maw_container.page myid_o
	[
		Form.post_form ~service:sign_up_action (fun () -> [
			div ~a:[a_class ["content-box"]] [
				h1 [pcdata "Signing up for "; pcdata title];
				location_line location date;
				div ~a:[a_class ["group-inscription-box"]] [
					title_bar;
					div ~a:[Eliom_content.Html.R.filter_attrib (a_class ["hidden"]) gh_s] [
						group_table;
						add_btn
					]
				];
				button ~a:[a_class ["button"]] [pcdata "Sign up"]
			]
		]) game_id
	]

let%server sign_up_handler myid_o game_id () =
	Eliom_registration.Any.register ~scope:Eliom_common.default_session_scope
		~service:sign_up_action do_sign_up;
	real_sign_up_handler myid_o game_id ()

let%client sign_up_handler =
	real_sign_up_handler
			
let%server edit_game_handler myid_o game_id () =
	Eliom_registration.Any.register ~scope:Eliom_common.default_session_scope
		~service:edit_game_action do_edit_game;
	real_edit_game_handler myid_o game_id ()

let%client edit_game_handler =
	real_edit_game_handler

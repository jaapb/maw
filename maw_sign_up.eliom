[%%shared
  open Eliom_content.Html.F
	open CalendarLib
	open Maw_utils
]

(* Local actions *)
let%server sign_up_action = Eliom_service.create_attached_post
	~fallback:Maw_services.sign_up_service
	~post_params:(Eliom_parameter.any) ()

let%client sign_up_action = ~%sign_up_action

let%server cancel_inscription_action = Eliom_service.create_attached_post
	~fallback:Maw_services.sign_up_service
	~post_params:(Eliom_parameter.unit) ()

let%client cancel_inscription_action = ~%cancel_inscription_action

(* Handlers *)
let%shared do_sign_up game_id params =
	Lwt.catch (fun () ->
		Os_session.connected_fun (fun myid game_id params ->
			let message = List.assoc_opt "message" params in
		  let%lwt () = Maw_game.sign_up (game_id, myid, message) in
			Os_msg.msg ~level:`Msg ~onload:true [%i18n S.data_saved];
			Eliom_registration.Redirection.send (Eliom_registration.Redirection Os_services.main_service)
		) game_id params
	)
	(function
	| Maw_games_db.Duplicate_inscription ->
		Os_msg.msg ~level:`Err ~duration:5.0 ~onload:true
		  "You have a previous inscription for this game which was cancelled. Please contact the Megagames administration.";
		Eliom_registration.Redirection.send (Eliom_registration.Redirection Os_services.main_service)
	| e ->
		Os_msg.msg ~level:`Err ~onload:true (Printexc.to_string e);
		Eliom_registration.Action.send ()
	)

let%shared do_cancel_inscription game_id () =
	Lwt.catch (fun () ->
		Os_session.connected_fun (fun myid game_id () ->
			let%lwt () = Maw_game.cancel_inscription (game_id, myid) in
			Os_msg.msg ~level:`Msg [%i18n S.cancel_inscription];
			Eliom_registration.Redirection.send (Eliom_registration.Redirection Os_services.main_service)	
		) game_id ()
	)
	(function
	| e ->
		Os_msg.msg ~level:`Err ~onload:true (Printexc.to_string e);
		Eliom_registration.Action.send ()
	)

let%shared add_to_group_button nr_s f =
	let btn = Eliom_content.Html.(
		D.button ~a:[D.a_class ["button"]; D.a_button_type `Button] [D.pcdata [%i18n S.add_group_member]]
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
	Eliom_content.Html.R.table ~thead:(Eliom_shared.React.S.const (thead [tr [th [pcdata "Name"]]])) rows

let%shared real_sign_up_handler myid game_id () = 
	let (group_l, group_h) = Eliom_shared.ReactiveData.RList.create [] in
	let (nr_s, nr_f) = Eliom_shared.React.S.create 0 in
	let (gh_s, gh_f) = Eliom_shared.React.S.create true in
	let%lwt (title, location, date, _) = Maw_game.get_game_info game_id in
	let%lwt inscr = Maw_game.get_inscription_opt (game_id, myid) in
	let%lwt (signed_up, message) = match inscr with
	| None -> Lwt.return (false, "")
	| Some x -> Lwt.return (true, x) in
	let group_table = display_group_table nr_s group_l in
	let add_btn = add_to_group_button nr_s
		[%client ((fun v ->
				Eliom_shared.ReactiveData.RList.snoc v ~%group_h;
				~%nr_f (v + 1);
				Lwt.return_unit
			)
			: int -> unit Lwt.t)
		] in 
	let title_bar = Eliom_content.Html.D.div ~a:[a_class ["text-bar"]] [
		table [
			tr [
				td [pcdata [%i18n S.group_inscription]];
				td [Eliom_content.Html.R.node (Eliom_shared.React.S.map
					[%shared ((fun h -> if h
						then Maw_icons.D.expand ~a:[a_title [%i18n S.expand]] ()
						else Maw_icons.D.collapse ~a:[a_title [%i18n S.collapse]] ()
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
	Maw_container.page (Some myid)
	[
		Form.post_form ~service:sign_up_action (fun () -> [
			div ~a:[a_class ["content-box"]] (
				h1 [pcdata (if signed_up then [%i18n S.edit_inscription_for] else [%i18n S.sign_up_for]); pcdata " "; pcdata title]::
				Maw_game.location_line location date::
				(if signed_up
				then p ~a:[a_class ["edit-inscription"]] [pcdata [%i18n S.edit_inscription_text]]
				else p [pcdata [%i18n S.signup_text]])::
				table ~a:[a_class ["signup-table"]] [
					tr [
						th ~a:[a_class ["no-expand"]] [pcdata [%i18n S.message_for_designer]];
						td [Raw.input ~a:[a_input_type `Text; a_name "message"; a_value message] ()]
					]
				]::
				p [pcdata [%i18n S.message_for_designer_text]]::
				p [pcdata [%i18n S.signup_group_text]]::
				div ~a:[a_class ["group-inscription-box"]] [
					title_bar;
					div ~a:[Eliom_content.Html.R.filter_attrib (a_class ["hidden"]) gh_s] [
						table [
							tr [
								th [pcdata [%i18n S.group_name]];
								td [Raw.input ~a:[a_input_type `Text; a_name "group_name"] ()]
							]
						];
						group_table;
						add_btn
					]
				]::
				p [pcdata [%i18n S.signup_warning]]::
				button ~a:[a_class ["button"]] [pcdata (if signed_up then [%i18n S.save_changes] else [%i18n S.sign_up])]::
				(if signed_up
				then [Eliom_content.Html.F.Raw.a ~a:[a_class ["button"; "cancel-inscription"]; a_onclick [%client fun _ ->
					Lwt.async (fun () ->
						Eliom_client.change_page ~service:cancel_inscription_action ~%game_id ()	
					)]] [pcdata [%i18n S.cancel_inscription]]]
				else [])
			)
		]) game_id
	]

let%server sign_up_handler myid_o game_id () =
	Eliom_registration.Any.register ~scope:Eliom_common.default_session_scope
		~service:sign_up_action do_sign_up;
	Eliom_registration.Any.register ~scope:Eliom_common.default_session_scope
		~service:cancel_inscription_action do_cancel_inscription;
	real_sign_up_handler myid_o game_id ()

let%client sign_up_handler =
	real_sign_up_handler

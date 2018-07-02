[%%shared
  open Eliom_content.Html.F
	open CalendarLib
	open Maw_utils
]

let%server add_admin_action =
	Eliom_service.create_attached_post
		~fallback:Maw_services.admin_service
		~post_params:(Eliom_parameter.int64 "user") ()

let%client add_admin_action =
	~%add_admin_action

let%shared do_add_admin myid_o () user_id =
	Eliom_registration.Action.send ()

let%shared real_admin_handler myid_o () () =
	match myid_o with
	| None -> Maw_container.page None
			[p [pcdata [%i18n S.must_be_connected_to_see_page]]]
	| Some myid ->
		let%lwt admin = Maw_user.is_admin (Some myid) in
		let%lwt form = Form.lwt_post_form ~service:add_admin_action
			(fun (user_id) -> 
				Lwt.return [
					Maw_user.user_input_widget ()
				]
			) () in
		if admin then
			Maw_container.page (Some myid)
			[
				div ~a:[a_class ["content-box"]]
				[
					h2 [pcdata [%i18n S.add_admin]];
					form
				]
			]
		else
			Maw_container.page None
			[p [pcdata [%i18n S.not_admin]]]

let%server admin_handler myid_o () () =
	Eliom_registration.Any.register ~scope:Eliom_common.default_session_scope ~service:add_admin_action
		(Os_session.Opt.connected_fun do_add_admin);
	real_admin_handler myid_o () ()

let%client admin_handler =
	real_admin_handler

[%%shared
  open Eliom_content.Html.F
	open CalendarLib
	open Maw_utils
]

(* Local services *)
let%server add_admin_action =
	Eliom_service.create_attached_post
		~fallback:Maw_services.admin_service
		~post_params:(Eliom_parameter.any) ()

let%client add_admin_action =
	~%add_admin_action

(* Database access *)

(* Service handlers *)
let%shared do_add_admin myid_o () params =
	try
		let user_id = Int64.of_string (List.assoc "user_id" params) in
		match myid_o with
		| None -> Eliom_registration.Action.send ()
		| Some myid ->
				let%lwt admin = Maw_user.is_admin (Some myid) in
				let%lwt already_admin = Maw_user.is_admin (Some user_id) in
					if admin then
					begin
						if already_admin
						then Os_msg.msg ~level:`Msg ~onload:true "User is already an admin."
						else
						begin
							Maw_user.set_admin user_id;
							Os_msg.msg ~level:`Msg ~onload:true "Admin created."
						end
					end;
					Eliom_registration.Action.send ()
	with
	| Not_found -> Eliom_registration.Action.send ()

let%shared real_admin_handler myid_o () () =
	match myid_o with
	| None -> Maw_container.page None
			[p [pcdata [%i18n S.must_be_connected_to_see_page]]]
	| Some myid ->
		let%lwt admin = Maw_user.is_admin (Some myid) in
		if admin then
			Maw_container.page (Some myid)
			[
				div ~a:[a_class ["content-box"]]
				[
					h2 [pcdata [%i18n S.add_admin]];
					Form.post_form ~service:add_admin_action (fun () -> [ 
						table [
							tr [
								td [pcdata [%i18n S.user_name]];
								td [Maw_user.user_input_widget ()]
							];
							tr [
								td ~a:[a_colspan 2] [button ~a:[a_class ["button"]] [pcdata "Confirm"]]
							]
						]
					]) ()
				]
			]
		else
			Maw_container.page (Some myid)
			[p [pcdata [%i18n S.not_admin]]]

let%server admin_handler myid_o () () =
	Eliom_registration.Any.register ~scope:Eliom_common.default_session_scope ~service:add_admin_action
		(Os_session.Opt.connected_fun do_add_admin);
	real_admin_handler myid_o () ()

let%client admin_handler =
	real_admin_handler

[%%shared
	open Eliom_lib
	open Eliom_content
	open Html.D
	open Eliom_service
	open Eliom_parameter
	open Utils
	open Services
]

[%%server
	open CalendarLib
	open Maw
	open Database
]

let update_notifications_page () (casting, before) =
	Lwt.catch (fun () ->
		let%lwt u = Eliom_reference.get Maw.user in
		match u with
		| None -> not_logged_in ()
		| Some (uid, _, _, _) -> 
			let%lwt (_, _, _, hidden) = Database.get_user_data uid in
			Database.set_notifications uid casting before >>=
			fun () -> container (standard_menu (Account.account_menu hidden))
			[
				h1 [pcdata "Notifications"];
				p [pcdata "Your changes have been saved successfully."]
			]
	)
	(function
	| Not_found -> error_page "Unknown user"
	| e -> error_page (Printexc.to_string e)
	)

let notifications_page () () =
	let update_notifications_service = create_attached_post
		~fallback:notifications_service
		~post_params:(bool "casting" ** bool "before") () in
	Maw_app.register ~scope:Eliom_common.default_session_scope
		~service:update_notifications_service update_notifications_page;
	Lwt.catch (fun () ->
		let%lwt u = Eliom_reference.get Maw.user in
		match u with
		| None -> not_logged_in ()
		| Some (uid, _, _, _) -> 
			let%lwt (_, _, _, hidden) = Database.get_user_data uid in
			let%lwt (n_c, n_b) = Database.get_notifications uid in
			container (standard_menu (Account.account_menu hidden))
			[
				h1 [pcdata "Notifications"];
				p [pcdata "Here, you can set which notifications you would like to receive. Notifications will be sent to your e-mail address."];
				Form.post_form ~service:update_notifications_service
				(fun (c, b) -> [
					table [
						tr [
							td [bool_checkbox c n_c];
							td [pcdata " Casting is published"]
						];
						tr [
							td [bool_checkbox b n_b];
							td [pcdata " One week before the game"]
						];
						tr [
							td ~a:[a_colspan 2] [Form.input ~input_type:`Submit
								~value:"Save changes" Form.string]
						]
					]
				]) ()
			]
	)
	(function
	| Not_found -> error_page "Unknown user"
	| e -> error_page (Printexc.to_string e)
	)
;;

let _ =
	Maw_app.register ~service:notifications_service notifications_page
;;

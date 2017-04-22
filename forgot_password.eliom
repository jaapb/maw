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

let reset_password_page () (email) =
	Lwt.catch (fun () ->
		let%lwt id = Database.find_user_by_email email in
		container (standard_menu [])
		[
			h1 [pcdata "Reset password"];
			p [pcdata "An e-mail has been sent to your address; please click on the
			link in it to reset your password."]
		]
	)
	(function
	| Not_found -> error_page "That e-mail address is not known."
	| e -> error_page (Printexc.to_string e)
	)
;;

let forgot_password_page () () =
	let reset_password_service = create_attached_post
		~fallback:forgot_password_service ~post_params:(string "email") () in
	Maw_app.register ~scope:Eliom_common.default_session_scope 
		~service:reset_password_service reset_password_page;
	Lwt.catch (fun () ->
		container (standard_menu [])
		[
			h1 [pcdata "Forgot password"];
			p [pcdata "Please enter your e-mail adress; you will be sent a link to a page where you can enter a new password."];
			Form.post_form ~service:reset_password_service
			(fun (email) -> [
				table
				[
					tr
					[
						td [pcdata "E-mail address: "];
						td [Form.input ~input_type:`Text ~name:email Form.string]
					];
					tr
					[
						td ~a:[a_colspan 2] [Form.input ~input_type:`Submit
							~value:"Reset password" Form.string];
					]
				]
			]) ()
		]
	)
	(function
	| e -> error_page (Printexc.to_string e)
	)

let _ =
	Maw_app.register ~service:forgot_password_service forgot_password_page
;;

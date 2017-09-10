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

let do_unhide_account_page () () =
	Lwt.catch (fun () ->
		let%lwt u = Eliom_reference.get Maw.user in
		match u with
		| Not_logged_in -> not_logged_in ()
		| User (uid, _, _, _)
		| Admin (_, (uid, _, _, _)) -> 
			let%lwt () = Database.unhide_user uid in
			container (standard_menu []) 
			[
				h1 [pcdata "Account visible"];
				p [pcdata "Your account is again visible to all."]
			]
	)
	(function
	| Not_found -> error_page "Unknown user"
	| e -> error_page (Printexc.to_string e)
	)

let unhide_account_page () () =
	let do_unhide_account_service = create_attached_post ~fallback:unhide_account_service ~post_params:unit () in
	Maw_app.register ~scope:Eliom_common.default_session_scope ~service:do_unhide_account_service do_unhide_account_page;
	Lwt.catch (fun () ->
		let%lwt u = Eliom_reference.get Maw.user in
		match u with
		| Not_logged_in -> not_logged_in ()
		| User (uid, _, _, _)
		| Admin (_, (uid, _, _, _)) -> 
			container (standard_menu [])
			[
				h1 [pcdata "Unhide account"];
				p [pcdata "This will once again render your account visible."];
				p [pcdata "Please confirm that you wish to continue by clicking the button below."];
				(Form.post_form ~service:do_unhide_account_service (fun () ->
					[
						Form.input ~input_type:`Submit ~value:"Confirm" Form.string
					]
				) ())
			]
	)
	(function
	| e -> error_page (Printexc.to_string e)
	)
;;

let _ =
	Maw_app.register ~service:unhide_account_service unhide_account_page
;;

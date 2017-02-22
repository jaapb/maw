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

let do_hide_account_page () () =
	Lwt.catch (fun () ->
		let%lwt u = Eliom_reference.get Maw.user in
		match u with
		| None -> not_logged_in ()
		| Some (uid, _, _, _) -> 
			let%lwt () = Database.hide_user uid in
			container (standard_menu []) 
			[
				h1 [pcdata "Account hidden."];
				p [pcdata "Your account has been hidden."]
			]
	)
	(function
	| Not_found -> error_page "Unknown user"
	| e -> error_page (Printexc.to_string e)
	)

let hide_account_page () () =
	let do_hide_account_service = create_attached_post ~fallback:hide_account_service ~post_params:unit () in
	Maw_app.register ~scope:Eliom_common.default_session_scope ~service:do_hide_account_service do_hide_account_page;
	Lwt.catch (fun () ->
		let%lwt u = Eliom_reference.get Maw.user in
		match u with
		| None -> not_logged_in ()
		| Some (uid, _, _, _) -> 
			container (standard_menu [])
			[
				h1 [pcdata "Hide account"];
				p [pcdata "This will hide your account. It will not delete any data, but will render your account invisible throughout the system. It will not cancel any inscriptions you may currently have for any games."];
				p [pcdata "Once hidden, you can unhide your account at any time."];
				p [pcdata "Please confirm that you wish to continue by clicking the button below."];
				(Form.post_form ~service:do_hide_account_service (fun () ->
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
	Maw_app.register ~service:hide_account_service hide_account_page
;;

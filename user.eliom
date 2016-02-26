[%%shared
	open Eliom_lib
	open Eliom_content.Html5
	open Eliom_content.Html5.D
	open Eliom_service.App
	open Eliom_parameter
]

[%%server
	open CalendarLib
	open Maw
	open Database
]

let update_user_service = post_service ~fallback:account_service
	~post_params:(string "email") ();;

let account_page () () =
	Lwt.catch (fun () ->
		let%lwt u = Eliom_reference.get Maw.user in
		match u with
		| None -> not_logged_in ()
		| Some (uid, _, _) -> 
			let%lwt (name, ex_email) = Database.get_user_data uid in
			container (standard_menu ())
			[
				h1 [pcdata "Your account"];
				Form.post_form ~service:update_user_service (fun email -> 
				[
					table [
						tr [
							th [pcdata "Name"];
							td [pcdata name]
						];
						tr [
							th [pcdata "E-mail address"];
							td [Form.input ~input_type:`Text ~name:email ~value:ex_email Form.string] 
						];
						tr 
						[
						 	td ~a:[a_colspan 2]
								[Form.input ~input_type:`Submit ~value:"Save changes" Form.string]
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

let update_user_page () email =
	Lwt.catch (fun () ->
		let%lwt u = Eliom_reference.get Maw.user in
		match u with
		| None -> not_logged_in ()
		| Some (uid, _, _) -> Database.update_user_data uid email >>=
		fun () -> container (standard_menu ())
		[
			p [pcdata "Changes successfully saved."]
		]
	)
	(function
	| e -> error_page (Printexc.to_string e)
	)
;;

let _ =
	Maw_app.register ~service:account_service account_page;
	Maw_app.register ~service:update_user_service update_user_page
;;

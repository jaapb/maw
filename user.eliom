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

let account_page () () =
	Lwt.catch (fun () ->
		let%lwt u = Eliom_reference.get Maw.user in
		match u with
		| None -> not_logged_in ()
		| Some (uid, _) -> 
			let%lwt (name, email) = Database.get_user_data uid in
			container (standard_menu ())
			[
				h1 [pcdata "Your account"];
				table [
					tr [
						th [pcdata "Name"];
						td [pcdata name]
					];
					tr [
						th [pcdata "E-mail address"];
						td [pcdata email]
					]
				]
			]
	)
	(function
	| Not_found -> error_page "Unknown user"
	| e -> error_page (Printexc.to_string e)
	)
;;

let _ =
	Maw_app.register ~service:account_service account_page
;;

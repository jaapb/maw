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

let forgot_password_page () () =
	Lwt.catch (fun () ->
		container (standard_menu [])
		[
			h1 [pcdata "Forgot password"]
		]
	)
	(function
	| e -> error_page (Printexc.to_string e)
	)

let _ =
	Maw_app.register ~service:forgot_password_service forgot_password_page
;;

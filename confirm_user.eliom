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
	open Maw_db
]

let confirm_user_page (user_id, random) () =
	Lwt.catch (fun () -> Maw_db.confirm_user user_id random >>=
	fun res -> container (standard_menu [])
	[
		h1 [pcdata "Account activated"];
		p [pcdata "You can now login normally."]
	])
	(function
	| Not_found -> error_page ("Sorry, this is not a valid confirmation request.")
	| e -> error_page (Printexc.to_string e)
	)
;;

let _ =
	Maw_app.register ~service:confirm_user_service confirm_user_page
;;

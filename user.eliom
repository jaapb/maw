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
	~post_params:(string "email" ** string "password") ();;
let add_user_service = post_service ~fallback:register_service 
	~post_params:(string "name" ** string "username" ** string "email" ** string "password") ();;
let confirm_user_service = service ~path:["confirm"] ~get_params:(suffix (int32 "user_id" ** string "random")) ();;

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
				Form.post_form ~service:update_user_service (fun (email, password) -> 
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
						tr [
							th [pcdata "Password"];
							td [Form.input ~input_type:`Password ~name:password Form.string]
						];
						tr [
							th [pcdata "Confirm password"];
							td [Raw.input ~a:[a_input_type `Password; a_id "password_confirm"] ()]
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

let update_user_page () (email, password) =
	Lwt.catch (fun () ->
		let%lwt u = Eliom_reference.get Maw.user in
		match u with
		| None -> not_logged_in ()
		| Some (uid, _, _) -> Database.update_user_data uid email password >>=
		fun () -> container (standard_menu ())
		[
			p [pcdata "Changes successfully saved."]
		]
	)
	(function
	| e -> error_page (Printexc.to_string e)
	)
;;

let register_page () () =
	Lwt.catch (fun () ->
		container (standard_menu ())
		[
			h1 [pcdata "Create a new account"];
			Form.post_form ~service:add_user_service
			(fun (name, (username, (email, password))) -> [
				table [
					tr [
						th [pcdata "Username:"];
						td [Form.input ~input_type:`Text ~name:username Form.string]
					];
					tr [
						th [pcdata "Password:"];
						td [Form.input ~input_type:`Password ~name:password Form.string]
					];
					tr [
						th [pcdata "Confirm password:"];
						td [Raw.input ~a:[a_input_type `Password; a_id "password_confirm"] ()]
					];
					tr [
						th [pcdata "Full name:"];
						td [Form.input ~input_type:`Text ~name:name Form.string] 
					];
					tr [
						th [pcdata "E-mail address:"];
						td [Form.input ~input_type:`Text ~name:email Form.string]
					];
					tr [
						td ~a:[a_colspan 2] [Form.input ~input_type:`Submit ~value:"Sign up" Form.string]
					]
				]
			]) ()
		]
	)
	(function
	| e -> error_page (Printexc.to_string e)
	)

let add_user_page () (name, (username, (email, password))) =
	Lwt.catch (fun () -> Database.add_user name username email password >>=
	fun (uid, random) -> let uri = Eliom_uri.make_string_uri ~service:confirm_user_service (uid, random) in
	Mail.send_register_mail name email uri;
	container (standard_menu ())
	[
		h1 [pcdata "Account created"];
		p [pcdata "Please reply to the confirmation mail."]
	])
	(function
	| e -> error_page (Printexc.to_string e)
	)
;;

let confirm_user_page (user_id, random) () =
	Lwt.catch (fun () -> Database.confirm_user user_id random >>=
	fun res -> container (standard_menu ())
	[
		h1 [pcdata "Account activated"];
		p [pcdata "You can now login normally."]
	])
	(function
	| e -> error_page (Printexc.to_string e)
	)
;;

let _ =
	Maw_app.register ~service:account_service account_page;
	Maw_app.register ~service:update_user_service update_user_page;
	Maw_app.register ~service:Maw.register_service register_page;
	Maw_app.register ~service:add_user_service add_user_page;
	Maw_app.register ~service:confirm_user_service confirm_user_page
;;

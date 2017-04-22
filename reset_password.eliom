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

let do_reset_password_page () (uid, password) =
	Lwt.catch (fun () ->
		let%lwt () = Database.update_user_password uid password in
		container (standard_menu [])
		[
			h1 [pcdata "Password reset"];
			p [pcdata "You can now login using your new password."]
		]
	)
	(function
	| e -> error_page (Printexc.to_string e)
	)
;;

let%client check_reset_form ev =
	let add_or_replace text = 
	let p = Dom_html.getElementById "error_paragraph" in
	let l = Dom.list_of_nodeList p##.childNodes in
	let t = Dom_html.document##createTextNode text in
	begin
 		match l with
		| [] -> Dom.appendChild p t
		| [c] -> Dom.replaceChild p t c
		| _ -> ()
	end in
	let p1 = Dom_html.getElementById "password_input1" in
	let p2 = Dom_html.getElementById "password_input2" in
	Js.Opt.iter (Dom_html.CoerceTo.input p1) (fun e1 ->
		Js.Opt.iter (Dom_html.CoerceTo.input p2) (fun e2 ->
			if e1##.value <> e2##.value then
			begin
				add_or_replace (Js.string "Passwords don't match.");
				Dom.preventDefault ev
			end
			else if Js.to_string e1##.value = "" then
			begin
				add_or_replace (Js.string "Please enter a password.");
				Dom.preventDefault ev
			end
		)
	)
;;
 
let reset_password_page (my_uid, code) () =
	let do_reset_password_service = create_attached_post
		~fallback:forgot_password_service
		~post_params:(int32 "user_id" ** string "password") () in
	Maw_app.register ~scope:Eliom_common.default_session_scope
		~service:do_reset_password_service do_reset_password_page;
	Lwt.catch (fun () ->
		let%lwt () = Database.check_reset_request my_uid code in
		container (standard_menu [])
		[
			h1 [pcdata "Reset password"];
			p ~a:[a_class ["error"]; a_id "error_paragraph"] [];
			Form.post_form ~service:do_reset_password_service
			(fun (uid, password) -> [
				table
				[
					tr [
						td [pcdata "New password: "];
						td [Form.input ~a:[a_id "password_input1"]
							~input_type:`Password ~name:password Form.string]
					];
					tr [
						td [pcdata "Confirm password: "];
						td [Raw.input ~a:[a_id "password_input2"; a_input_type `Password] ()]
					];
					tr [
						td ~a:[a_colspan 2] [
							Form.input ~input_type:`Hidden ~name:uid ~value:my_uid Form.int32;
							Form.input ~a:[a_onclick [%client check_reset_form]]
								~input_type:`Submit ~value:"Reset password" Form.string
						]
					]
				]
			]) ()
		]
	)
	(function
	| Not_found -> error_page "Sorry, this is not a valid password reset request"
	| e -> error_page (Printexc.to_string e)
	)
;;

let _ =
	Maw_app.register ~service:reset_password_service reset_password_page
;;

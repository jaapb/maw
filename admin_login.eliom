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
]

let do_admin_login_page () user_id =
  let%lwt u = Eliom_reference.get Maw.user in
	let%lwt (ufn, uln, _, _, user_is_admin) = Database.get_user_data user_id in
  match u with
  | Not_logged_in -> not_logged_in ()
	| Admin _ -> error_page "You are already logged in as another user, please log out first."
  | User (aid, afn, aln, is_admin) -> 
		if not is_admin then error_page "You must be an administrator to access this page."
    else
		begin
			Eliom_reference.set Maw.user (Admin ((aid, afn, aln), (user_id, ufn, uln, user_is_admin)));
			dashboard_page () ()
		end
;;

let admin_login_page () () =
	let do_admin_login_service = create_attached_post
		~fallback:admin_login_service
		~post_params:(int32 "user_id") () in
	Maw_app.register ~scope:Eliom_common.default_session_scope
		~service:do_admin_login_service do_admin_login_page;
  let%lwt u = Eliom_reference.get Maw.user in
  match u with
  | Not_logged_in -> not_logged_in ()
  | User (_, _, _, is_admin) -> 
		if not is_admin then error_page "You must be an administrator to access this page."
    else
		begin
			let%lwt users = Database.get_users () in
			container (standard_menu []) 
			[	
				h1 [pcdata "Login as another user"];
				p [pcdata "Temporarily login as another user."];
				match users with
				| [] -> p [b [pcdata "no users yet"]]
				| (id, fn, ln, _, _)::tl -> 
					Form.post_form ~service:do_admin_login_service (fun (user_id) ->
					[
						table
						[
							tr [td [
								Form.select ~name:user_id Form.int32
								(Form.Option ([], id, Some (pcdata (Printf.sprintf "%s %s" fn ln)), false))
								(List.map (fun (tid, tfn, tln, _, _) ->
									Form.Option ([], tid, Some (pcdata (Printf.sprintf "%s %s" tfn tln)), false)
								) tl)
							]];
							tr [td [
								Form.input ~input_type:`Submit ~value:"Switch" Form.string
							]]
						]
					]) ()				
			]
		end
	| Admin _ -> error_page "You are already logged in as another user, please log out first."
;;

let admin_logout_action () () =
  let%lwt u = Eliom_reference.get Maw.user in
  match u with
  | Not_logged_in 
	| User _ -> Lwt.return ()
	| Admin ((aid, afn, aln), _) -> Eliom_reference.set Maw.user (User (aid, afn, aln, true))
;;
	
let () =
	Maw_app.register ~service:admin_login_service admin_login_page;
	Eliom_registration.Action.register ~service:admin_logout_service admin_logout_action
;;

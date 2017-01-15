[%%shared
	open Eliom_lib
	open Eliom_content
	open Html.D
	open Eliom_service
	open Eliom_parameter
	open Services
]

[%%server
	open CalendarLib
	open Maw
]
let admin_confirm_users_page () () =
  Lwt.catch (fun () -> let%lwt u = Eliom_reference.get Maw.user in
    match u with
    | None -> not_logged_in ()
    | Some (_, _, _, is_admin) -> if not is_admin
      then error_page "You must be an administrator to access this page."
      else
      let%lwt users = Database.get_users ~unconfirmed:true () in
			let nonconf = List.filter (fun (_, _, _, _, s) -> s = Some "U") users in
			let%lwt confirm_strs = Lwt_list.map_s (fun (id, _, _, _, _) ->
				Database.get_confirmation id) nonconf in
      let (uhid, uhfname, uhlname, _, _) = List.hd users in
      begin
        container (standard_menu ())
        [
					h1 [pcdata "Manually confirm users"];
					table
					(
						tr [
							td ~a:[a_colspan 3]
								[pcdata (Printf.sprintf "Currently %d user(s) is/are waiting for confirmation." (List.length nonconf))]
						]::
						tr [
							th [pcdata "Name"];
							th [pcdata "E-mail address"];
							th [pcdata ""]
						]::
						List.map2 (fun (id, fname, lname, email, _) cstr ->
							tr
							[
								td [pcdata (Printf.sprintf "%s %s" fname lname)];
								td [pcdata email];
								td [a ~service:confirm_user_service [pcdata "Confirm"] (id, cstr)]
							]
						) nonconf confirm_strs 
					)
        ]
      end
  )
  (function
  | e -> error_page (Printexc.to_string e)
  )
;;

let _ =
	Maw_app.register ~service:admin_confirm_users_service admin_confirm_users_page
;;

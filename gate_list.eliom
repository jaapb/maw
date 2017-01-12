open Eliom_content
open Html.D
open Eliom_service
open Eliom_parameter
open Utils

[%%server
	open CalendarLib
	open Maw
	open Database
]

let gate_list_service =
	create ~path:(Path ["gate_list"]) ~meth:(Get (suffix (int32 "game_id"))) ();;
let print_gate_list_service =
	create ~path:(Path ["gate_list"; "printable"]) ~meth:(Get (suffix (int32 "game_id"))) ();;

let do_book_in game_id () uid =
	Database.change_status game_id uid `Attended
	;;

let gate_list_page game_id () =
	let book_in_service = create_attached_post
		~fallback:(preapply gate_list_service game_id)
		~post_params:(int32 "user_id") () in
	Eliom_registration.Action.register ~scope:Eliom_common.default_session_scope
		~service:book_in_service (do_book_in game_id);
	Lwt.catch (fun () ->
		let%lwt u = Eliom_reference.get Maw.user in
		let%lwt (title, date, loc, _, _, dsg_uid, _, _, _, _) =
			Database.get_game_data game_id in
		let%lwt cast = Database.get_casting game_id in
		let%lwt cast_status = Lwt_list.map_s (fun (t, roles) ->
			let%lwt new_roles = Lwt_list.map_s (fun (rn, fn, ln, uid, _, _) ->
				match uid with
				| None -> Lwt.return None
				| Some x ->
					let%lwt status = sign_up_status x game_id in
					Lwt.return (Some (rn, fn, ln, x, status))
			) roles in
			Lwt.return (t, new_roles)
		) cast in
		let date_str = match date with
		| None -> "NO DATE"
		| Some d -> Printer.Date.sprint "%d %B %Y" d in
		match u with
		| None -> not_logged_in ()
		| Some (uid, _, _, is_admin) ->
			if uid <> dsg_uid && not is_admin
			then error_page "You are not the designer of this game (or an admin)."
			else
				container (standard_menu ())
				(
					h1 [pcdata (Printf.sprintf "Gate list for %s" title)]::
					p [pcdata (Printf.sprintf "%s, %s" loc date_str)]::
					List.flatten (List.map (fun (t, roles) ->
					[
						h2 [pcdata t];
						table (
							tr [
								th [];
								th [pcdata "Name"];
								th [pcdata "Role"];
								th [pcdata "Status"]
							]::
							List.map (fun x ->
							 	match x with
								| None -> tr [td ~a:[a_colspan 4] [pcdata "blerp"]]
								| Some (role, fn, ln, ex_uid, status) ->
									tr [
										td (
											match status with
											| `Yes (_, _, `Attended) -> []
											| _ -> [
												Form.post_form ~service:book_in_service (fun uid ->
												[
													Form.input ~input_type:`Hidden ~name:uid ~value:ex_uid Form.int32;
													Form.input ~input_type:`Submit ~value:"Book in" Form.string
												]) ()
											]);
										td [pcdata (Printf.sprintf "%s %s" (default "X" fn) (default "X" ln))];
										td [pcdata role];
										td [match status with
											| `Yes (_, _, `Attended) -> pcdata "Booked in"
											| `Yes (_, _, `Confirmed) -> pcdata "Confirmed"
											| `Yes (_, _, `Paid) -> pcdata "Paid"
											| `Yes _ -> b [pcdata "Signed up, not paid"]
											| _ -> b [i [pcdata "Not signed up"]]
										]
									]
							) roles
						)
					]
					) cast_status)
				)
	)
	(fun e -> error_page (Printexc.to_string e))
;;

let print_gate_list_page game_id () =
	Lwt.catch (fun () ->
		let%lwt u = Eliom_reference.get Maw.user in
		let%lwt (title, date, loc, _, _, dsg_uid, _, _, _, _) =
			Database.get_game_data game_id in
		let%lwt cast = Database.get_casting game_id in
		let%lwt cast_status = Lwt_list.map_s (fun (t, roles) ->
			let%lwt new_roles = Lwt_list.map_s (fun (rn, fn, ln, uid, _, _) ->
				match uid with
				| None -> Lwt.return None
				| Some x ->
					let%lwt status = sign_up_status x game_id in
					Lwt.return (Some (rn, fn, ln, x, status))
			) roles in
			Lwt.return (t, new_roles)
		) cast in
		let date_str = match date with
		| None -> "NO DATE"
		| Some d -> Printer.Date.sprint "%d %B %Y" d in
		match u with
		| None -> not_logged_in ()
		| Some (uid, _, _, _) ->
			if uid <> dsg_uid && not is_admin
			then error_page "You are not the designer of this game (or an admin)."
			else
				container (standard_menu ())
				(
					h1 [pcdata (Printf.sprintf "Gate list for %s" title)]::
					p [pcdata (Printf.sprintf "%s, %s" loc date_str)]::
					List.flatten (List.map (fun (t, roles) ->
					[
						h2 [pcdata t];
						table (
							tr [
								th [pcdata "Present"];
								th [pcdata "Name"];
								th [pcdata "Role"];
								th [pcdata "Status"]
							]::
							List.map (fun x ->
							 	match x with
								| None -> tr [td ~a:[a_colspan 4] [pcdata "blerp"]]
								| Some (role, fn, ln, ex_uid, status) ->
									tr [
										td [];
										td [pcdata (Printf.sprintf "%s %s" (default "X" fn) (default "X" ln))];
										td [pcdata role];
										td [match status with
											| `Yes (_, _, `Attended) -> pcdata "Booked in"
											| `Yes (_, _, `Confirmed) -> pcdata "Confirmed"
											| `Yes (_, _, `Paid) -> pcdata "Paid"
											| `Yes _ -> b [pcdata "Signed up, not paid"]
											| _ -> b [i [pcdata "Not signed up"]]
										]
									]
							) roles
						)
					]
					) cast_status)
				)	
	)
	(fun e -> error_page (Printexc.to_string e))
;;

let () =
	Maw_app.register ~service:gate_list_service gate_list_page;
	Maw_app.register ~service:print_gate_list_service print_gate_list_page
;;

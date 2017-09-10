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

let design_menu game_id =
	[
		tr [td [a ~service:role_service [pcdata "Set up roles"] game_id]];
		tr [td [a ~service:cast_service [pcdata "Do casting"] game_id]]
	]
;;

let update_game_data game_id () (descr, (min, (max, (id, (cd, pd))))) =
	let inscr_date = if id = ""
		then None
		else Some (Printer.Date.from_fstring "%Y-%m-%d" id) in
	let cancel_date = if cd = ""
		then None
		else Some (Printer.Date.from_fstring "%Y-%m-%d" cd) in
	let pay_date = if pd = ""
		then None
		else Some (Printer.Date.from_fstring "%Y-%m-%d" pd) in
	let%lwt u = Eliom_reference.get Maw.user in
	match u with
	| Not_logged_in -> Lwt.return ()
	| User (uid, _, _, _)
	| Admin (_, (uid, _, _, _)) -> 
		begin
			Database.set_game_description game_id descr >>=
			fun () -> Database.set_game_numbers game_id min max >>=
			fun () -> Database.set_game_deadlines game_id inscr_date cancel_date pay_date
		end
;;

let upload_picture game_id () picture =
	let%lwt u = Eliom_reference.get Maw.user in
	match u with
	| Not_logged_in -> Lwt.return ()
	| User (uid, _, _, _)
	| Admin (_, (uid, _, _, _)) ->
		begin
			let new_name = Eliom_request_info.get_original_filename picture in
			let new_path = Printf.sprintf "static/%s" new_name in
			let old_path = Eliom_request_info.get_tmp_filename picture in
			Ocsigen_messages.console (fun () -> old_path);
			Ocsigen_messages.console (fun () -> new_path);
			(try
				Unix.unlink new_path
			with _ -> ());
			Lwt_unix.link old_path new_path >>=
			fun () -> Database.set_picture_filename game_id new_name
		end
		
let design_page game_id () = 
	let update_game_data_service = create_attached_post
		~fallback:(preapply design_service game_id)
		~post_params:(string "description" ** int32 "min" ** int32 "max" ** string "inscription" ** string "cancellation" ** string "payment") () in
	let upload_picture_service = create_attached_post
		~fallback:(preapply design_service game_id)
		~post_params:(file "picture") () in
	Eliom_registration.Action.register ~scope:Eliom_common.default_session_scope
		~service:update_game_data_service (update_game_data game_id);
	Eliom_registration.Action.register ~scope:Eliom_common.default_session_scope
		~service:upload_picture_service (upload_picture game_id);
	Lwt.catch (fun () -> let%lwt u = Eliom_reference.get Maw.user in
	match u with
	| Not_logged_in -> not_logged_in ()
	| User (uid, _, _, _)
	| Admin (_, (uid, _, _, _)) -> 
		let%lwt dsgs = Database.get_game_designers game_id in
		if not (is_designer uid dsgs)
		then error_page "You are not the designer of this game."
		else
		let%lwt (title, date, loc, d, min_nr, max_nr, _) =
			Database.get_game_data game_id in
		let%lwt (id, cd, pd) = Database.get_game_deadlines game_id in
		let idate = match id with
		| None -> ""
		| Some d -> Printer.Date.sprint "%Y-%m-%d" d in
		let cdate = match cd with
		| None -> ""
		| Some d -> Printer.Date.sprint "%Y-%m-%d" d in
		let pdate = match pd with
		| None -> ""
		| Some d -> Printer.Date.sprint "%Y-%m-%d" d in
		let%lwt teams = Database.get_game_teams game_id in
		let%lwt roles = Database.get_game_roles game_id in
		let%lwt fn = Database.get_picture_filename game_id in
		let nr_roles = List.fold_left (fun acc (_, r) -> acc + List.length r) 0 roles in
			container (standard_menu (design_menu game_id))
			[
				h1 [pcdata title];
				p [pcdata (Printf.sprintf "%s, %s" loc (date_or_tbd date))];
				Form.post_form ~service:update_game_data_service
				(fun (descr, (min, (max, (id, (cd, pd))))) -> [
					table [
						tr [
							td ~a:[a_colspan 4] [pcdata "Game description:"]
						];
						tr [	
							td ~a:[a_colspan 4] [
								Form.textarea ~a:[a_cols 60; a_rows 10] ~name:descr ~value:d ()
							]
						];
						tr [
							th ~a:[a_colspan 4] [pcdata "Numbers"]
						];
						tr [
							td ~a:[a_colspan 4] [pcdata (Printf.sprintf "(there are %d roles currently set up for this game)" nr_roles)]
						];
						tr [
							td [pcdata "Minimum:"];
							td [Form.input ~a:[a_size 5] ~input_type:`Text ~name:min ~value:min_nr Form.int32];
							td [pcdata "Maximum:"];
							td [Form.input ~a:[a_size 5] ~input_type:`Text ~name:max ~value:max_nr Form.int32];
						];
						tr [
							th ~a:[a_colspan 4] [pcdata "Deadlines"]
						];
						tr [
							td [pcdata "Inscription:"];
							td ~a:[a_colspan 3] [Form.input ~input_type:`Date ~name:id ~value:idate Form.string];
						];
						tr [
							td [pcdata "Cancellation:"];
							td ~a:[a_colspan 3] [Form.input ~input_type:`Date ~name:cd ~value:cdate Form.string];
						];
						tr [
							td [pcdata "Payment:"];
							td ~a:[a_colspan 3] [Form.input ~input_type:`Date ~name:pd ~value:pdate Form.string];
						];
						tr [
							td ~a:[a_colspan 4] [Form.input ~input_type:`Submit ~value:"Save" Form.string]
						]
					]
				]) ();
				Form.post_form ~service:upload_picture_service
				(fun picture -> [
					table [
						tr [
							th ~a:[a_colspan 3] [pcdata "Picture"]
						];
						tr [
							td (match fn with
							| None -> []
							| Some p -> [pcdata "[Picture uploaded]"]
							);
							td [Form.file_input ~name:picture ()];
							td [Form.input ~input_type:`Submit ~value:"Send" Form.string]
						]
					]
				]) ()
			]
	)
	(function
	| Not_found -> unknown_game ()
	| e -> error_page (Printexc.to_string e)
	)
;;

let () =
	Maw_app.register ~service:design_service design_page
;;

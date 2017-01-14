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

let design_page game_id () = 
	let%lwt u = Eliom_reference.get Maw.user in
	Lwt.catch (fun () -> match u with
	| None -> not_logged_in ()
	| Some (uid, _, _, _) -> 
		let%lwt (title, date, loc, _, _, dsg_id, d, min_nr, max_nr, _) =
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
		if uid <> dsg_id then error_page "You are not the designer of this game."
    else
		let%lwt teams = Database.get_game_teams game_id in
			container (standard_menu ())
			[
				h1 [pcdata title];
				p [pcdata (Printf.sprintf "%s, %s" loc (date_or_tbd date))];
				p [a ~service:role_service [pcdata "Set up roles and teams for this game"] game_id];
        p [a ~service:cast_service [pcdata "Cast this game"] game_id];
				Form.post_form ~service:update_descr_service (fun descr -> [
					table [
						tr [
							td [pcdata "Game description:"]
						];
						tr [	
							td [
								Form.textarea ~a:[a_cols 60; a_rows 10] ~name:descr ~value:d ()
							]
						];
						tr [
							td [
								Form.input ~input_type:`Submit ~value:"Save" Form.string
							]
						]
					]
				]) game_id;
				Form.post_form ~service:update_numbers_service (fun (min, max) -> [
					table [
						tr [
							td ~a:[a_colspan 5] [pcdata "Numbers:"]
						];
						tr [
							td [pcdata "Minimum:"];
							td [Form.input ~a:[a_size 5] ~input_type:`Text ~name:min ~value:min_nr Form.int32];
							td [pcdata "Maximum:"];
							td [Form.input ~a:[a_size 5] ~input_type:`Text ~name:max ~value:max_nr Form.int32];
							td [Form.input ~input_type:`Submit ~value:"Save" Form.string]
						]
					]
				]) game_id;
				Form.post_form ~service:update_deadlines_service (fun (id, (cd, pd)) -> [
					table [
						tr [
							td ~a:[a_colspan 2] [pcdata "Deadlines:"]
						];
						tr [
							td [pcdata "Inscription:"];
							td [Form.input ~input_type:`Date ~name:id ~value:idate Form.string];
						];
						tr [
							td [pcdata "Cancellation:"];
							td [Form.input ~input_type:`Date ~name:cd ~value:cdate Form.string];
						];
						tr [
							td [pcdata "Payment:"];
							td [Form.input ~input_type:`Date ~name:pd ~value:pdate Form.string];
						];
						tr [
							td ~a:[a_colspan 2] [Form.input ~input_type:`Submit ~value:"Save" Form.string]
						]
					]
				]) game_id
			]
	)
	(function
	| Not_found -> unknown_game ()
	| e -> error_page (Printexc.to_string e)
	)
;;

let update_description game_id descr =
	let%lwt u = Eliom_reference.get Maw.user in
	match u with
	| None -> Lwt.return ()
	| Some (uid, _, _, _) -> Database.set_game_description game_id descr
;;

let update_numbers game_id (min, max) =
	let%lwt u = Eliom_reference.get Maw.user in
	match u with
	| None -> Lwt.return ()
	| Some (uid, _, _, _) -> Database.set_game_numbers game_id min max
;;

let update_deadlines game_id (id, (cd, pd)) =
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
	| None -> Lwt.return ()
	| Some (uid, _, _, _) -> Database.set_game_deadlines game_id inscr_date cancel_date pay_date

let () =
	Maw_app.register ~service:design_service design_page;
	Eliom_registration.Action.register ~service:update_descr_service
		update_description;
	Eliom_registration.Action.register ~service:update_numbers_service
		update_numbers;
	Eliom_registration.Action.register ~service:update_deadlines_service
		update_deadlines
;;

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

let do_cancel_page game_id () user_id =
	let%lwt u = Eliom_reference.get Maw.user in
	Lwt.catch (fun () -> match u with
	| None -> not_logged_in ()
	| Some (my_uid, _, _, _) -> 
		let%lwt () = Database.cancel_inscription game_id user_id in
		let%lwt (fn, ln, email) = Database.get_user_data user_id in
		let%lwt (title, date, location, _, _, _, _, _, _, _) =
			Database.get_game_data game_id in
		let game_dstr = match date with
		| Some d -> Printer.Date.sprint "%d %B %Y" d
		| None -> "TBD" in
		Mail.send_cancellation_mail fn ln email title game_dstr location;
		container (standard_menu ())
			[
				h1 [pcdata "Cancellation complete"];
				p [pcdata "The following inscription has been cancelled:"];
				p [b [pcdata "User:"]; pcdata (Printf.sprintf " %s %s" fn ln)];
				p [b [pcdata "Game: "]; pcdata title;
					pcdata (Printf.sprintf " (%s, %s)" location game_dstr)]
			]	
	)
	(fun e -> error_page (Printexc.to_string e))
;;

let cancel_page game_id () =
	let do_cancel_service = create_attached_post ~fallback:(preapply cancel_service game_id) ~post_params:(int32 "uid") () in
	Maw_app.register ~scope:Eliom_common.default_session_scope ~service:do_cancel_service (do_cancel_page game_id);
	let%lwt u = Eliom_reference.get Maw.user in
	Lwt.catch (fun () -> match u with
	| None -> not_logged_in ()
	| Some (my_uid, _, _, _) ->
		let%lwt isu = Database.sign_up_status my_uid game_id in
		let%lwt (title, date, location, _, _, _, _, _, _, _) = Database.get_game_data game_id in
		let%lwt (_, cd, _) = Database.get_game_deadlines game_id in
		let game_dstr = match date with
		| Some d -> Printer.Date.sprint "%d %B %Y" d
		| None -> "TBD" in
				container (standard_menu ())
				(match isu with
				| `Cancelled | `No ->
					[
						h1 [pcdata "Not signed up"];
						p [pcdata "You are not signed up for the game you are trying to cancel."]
					]
				| `Yes _ ->
					(
						h1 [pcdata "Cancel inscription"]::
						p [pcdata "This will cancel your inscription for the following game:"]::
						p [b [pcdata title]; pcdata (Printf.sprintf " (%s, %s)" location game_dstr)]::
						p [pcdata "This action cannot be undone. Please confirm that you wish to continue by clicking the button below."]::
						(Form.post_form ~service:do_cancel_service (fun user_id ->
							[
								Form.input ~input_type:`Hidden ~name:user_id ~value:my_uid Form.int32;
								Form.input ~input_type:`Submit ~value:"Confirm" Form.string
							]
						) ())::
						(match cd with
						| Some ddl when Date.compare ddl (Date.today ()) < 0 ->
							[
								p [
									b [pcdata "Please note: "]; 
									i [pcdata "The cancellation deadline for this game has passed. You can still cancel, but you will not be refunded."]
								]
							]	
						| _ -> []
						)
					)
				)
	)
	(fun e -> error_page (Printexc.to_string e))
;;

let _ =
	Maw_app.register ~service:cancel_service cancel_page
;;

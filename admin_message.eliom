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

let do_message_page () (dest_type, (dest, (subject, contents))) =
  let%lwt u = Eliom_reference.get Maw.user in
  match u with
  | None -> not_logged_in ()
  | Some (_, _, _, is_admin) -> 
	 	let%lwt addressees =
			match dest_type with
			| Some "all" -> let%lwt l = Database.get_users () in
					Lwt.return (List.map (fun (_, fn, ln, email, _) ->
						(email, fn, ln)
					) l)
			| Some "game" -> begin
				match dest with
				| None -> Lwt.return []
				| Some d ->
					let%lwt l = Database.get_inscription_list (Int32.of_string d) in
					Lwt.return (List.map (fun (fn, ln, email, _, _, _, _, _, _) ->
						(email, fn, ln)
					) l)
				end
			| _ -> Lwt.return []
			in
		if not is_admin then error_page "You must be an administrator to access this page."
    else
		begin
			List.iter (fun (email, fname, lname) ->
				let new_message = Printf.sprintf
					"Dear %s,\n
\n
A message from Megagame Makers:\n
\n
%s\n
\n
All the best,\n
\n
Maw." fname contents in
				Mail.send_mail [Printf.sprintf "%s %s" fname lname, email]
					subject
					new_message
			) addressees;
			container (standard_menu [])
			[
				h1 [pcdata "Message sent"];
				p [pcdata (Printf.sprintf "Your message was sent to these persons:")];
				ul (List.map (fun (_, fname, lname) ->
					li [pcdata (Printf.sprintf "%s %s" fname lname)]
				) addressees);
				p [b [pcdata "Subject:"]; pcdata (Printf.sprintf " %s" subject)];
				p [b [pcdata "Message:"]];
				p [pcdata contents]
			]
		end
;;

let admin_message_page () () =
	let do_message_service = create_attached_post
		~fallback:admin_message_service
		~post_params:(radio string "type" ** opt (string "dest") ** string "subject" ** string "contents") () in
	Maw_app.register ~scope:Eliom_common.default_session_scope
		~service:do_message_service do_message_page;
  let%lwt u = Eliom_reference.get Maw.user in
  match u with
  | None -> not_logged_in ()
  | Some (_, _, _, is_admin) -> 
		if not is_admin then error_page "You must be an administrator to access this page."
    else
			let%lwt games = Database.get_all_games () in
		container (standard_menu [])
		[
			h1 [pcdata "Send message"];
			Form.post_form ~service:do_message_service (fun (dest_type, (dest, (subject, text))) ->
			[
				table
				[	
					tr [
						td [
							Form.radio ~checked:true ~name:dest_type ~value:"all" Form.string;
							pcdata " All users";
						]
					];
					tr [
						td [
							Form.radio ~name:dest_type ~value:"game" Form.string;
							pcdata " Players for game: ";
							match games with
							| [] -> p [b [pcdata "no games set up"]]
							| hd::tl -> Form.select ~name:dest Form.string
								(Form.Option ([], hd, Some (pcdata hd), false))
								(List.map (fun t ->
									Form.Option ([], t, Some (pcdata t), false)
								) tl)
						]
					];
					tr [
						td [
							pcdata "Subject: ";
							Form.input ~input_type:`Text ~name:subject Form.string
						];
					];
					tr [
						td [
							Form.textarea ~a:[a_cols 60; a_rows 10] ~name:text ()
						]
					];
					tr [
						td [
							Form.input ~input_type:`Submit ~value:"Send" Form.string
						]
					]
				]
			]) ()
		]
;;

let () =
	Maw_app.register ~service:admin_message_service admin_message_page
;;

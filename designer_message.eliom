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

let do_message_page game_id () (dest_type, (dest, (subject, contents))) =
  let%lwt u = Eliom_reference.get Maw.user in
  match u with
  | None -> not_logged_in ()
  | Some (uid, dsg_fn, dsg_ln, _) -> 
		let%lwt dsgs = Database.get_game_designers game_id in
		let%lwt roles = Database.get_game_roles game_id in
		if not (is_designer uid dsgs)
		then error_page "You are not the designer of this game."
		else let%lwt (title, _, _, _, _, _, _) = Database.get_game_data game_id in
	 	let%lwt addressees =
			match dest_type with
			| Some "all" -> let%lwt l = Database.get_inscription_list game_id in
					Lwt.return (List.map (fun (fn, ln, email, _, _, _, _, _, _) ->
						(email, fn, ln)
					) l)
			| Some "team" -> begin
				match dest with
				| None -> Lwt.return []
				| Some d -> Database.get_team_members game_id d
				end
			| Some "role_class" -> begin	
				match dest with
				| None -> Lwt.return []
				| Some d -> Database.get_role_class_members game_id d
				end		
			| Some "player" -> begin
				match dest with
				| None -> Lwt.return []
				| Some d -> Database.get_user_data (Int32.of_string d) >>=
				  fun (fn, ln, email, _) -> Lwt.return [email, fn, ln]
				end
			| _ -> Lwt.return []
			in
			List.iter (fun (email, fname, lname) ->
				let new_message = Printf.sprintf
					"Dear %s,\n
\n
A message from %s %s, game designer of %s:\n
\n
%s\n
\n
All the best,\n
\n
Maw." fname dsg_fn dsg_ln title contents in
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
;;

let designer_message_page game_id () =
	let do_message_service = create_attached_post
		~fallback:(preapply designer_message_service game_id)
		~post_params:(radio string "type" ** opt (string "dest") ** string "subject" ** string "contents") () in
	Maw_app.register ~scope:Eliom_common.default_session_scope
		~service:do_message_service (do_message_page game_id);
  let%lwt u = Eliom_reference.get Maw.user in
  match u with
  | None -> not_logged_in ()
  | Some (uid, _, _, _) -> 
		let%lwt dsgs = Database.get_game_designers game_id in
		if not (is_designer uid dsgs)
		then error_page "You are not the designer of this game."
    else
		let%lwt (title, date, loc, d, min_nr, max_nr, _) =
			Database.get_game_data game_id in
		let%lwt teams = Database.get_game_teams game_id in
		let%lwt x = Database.get_game_roles game_id in
		let role_classes = List.sort_uniq compare
			(remove_null (List.flatten (List.map (fun (t, l) -> List.map snd l) x)))
			in
		let%lwt y = Database.get_inscription_list game_id in
		let players = List.map (fun (fn, ln, _, uid, _, _, _, _, _) ->
			(uid, fn, ln)) y in
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
							pcdata " All players";
						]
					];
					tr [
						td [
							Form.radio ~name:dest_type ~value:"team" Form.string;
							pcdata " Team: ";
							match teams with
							| [] -> p [b [pcdata "no teams set up"]]
							| hd::tl -> Form.select ~name:dest Form.string
								(Form.Option ([], hd, Some (pcdata hd), false))
								(List.map (fun t ->
									Form.Option ([], t, Some (pcdata t), false)
								) tl)
						]
					];
					tr [
						td [
							Form.radio ~name:dest_type ~value:"role_class" Form.string;
							pcdata " Role class: ";
							match role_classes with
							| [] -> p [b [pcdata "no role classes set up"]]
							| hd::tl -> Form.select ~name:dest Form.string
								(Form.Option ([], hd, Some (pcdata hd), false))
								(List.map (fun t ->
									Form.Option ([], t, Some (pcdata t), false)
								) tl)
						]
					];
					tr [
						td [
							Form.radio ~name:dest_type ~value:"player" Form.string;
							pcdata " Player: ";
							match players with
							| [] -> p [b [pcdata "no players yet"]]
							| (id, fn, ln)::tl -> Form.select ~name:dest Form.string
								(Form.Option ([], Int32.to_string id, Some (pcdata (Printf.sprintf "%s %s" fn ln)), false))
								(List.map (fun (tid, tfn, tln) ->
									Form.Option ([], Int32.to_string tid, Some (pcdata (Printf.sprintf "%s %s" tfn tln)), false)
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
	Maw_app.register ~service:designer_message_service designer_message_page
;;

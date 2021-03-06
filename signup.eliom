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

let%client nr_ids = ref 0

let%client rec renumber_children n trs =
	match trs with
	| [] -> ()
	| h::t -> Js.Opt.iter (Dom_html.CoerceTo.element h) (fun tr ->
			if Js.to_bool (tr##.classList##contains (Js.string "group_inscription_row")) then
			begin
				let td_name::td_role::td_note::_ =
					Dom.list_of_nodeList tr##.childNodes in
				let input_uid::_ =
					Dom.list_of_nodeList td_name##.childNodes in	
				let input_role::_ = Dom.list_of_nodeList td_role##.childNodes in
				let input_note::_ = Dom.list_of_nodeList td_note##.childNodes in	
					Js.Unsafe.set input_uid "name" (Printf.sprintf "__co_eliom_person.uid[%d]" n);
					Js.Unsafe.set input_role "name" (Printf.sprintf "__co_eliom_person.role[%d]" n); 
					Js.Unsafe.set input_note "name" (Printf.sprintf "__co_eliom_person.note[%d]" n); 
					renumber_children (n+1) t
			end
			else renumber_children n t
		)
;;

let%client remove_my_row ev =
	(* The button is in a td in a tr; it's the tr that has to be deleted! *)
  Js.Opt.iter (ev##.target) (fun e ->
    Js.Opt.iter (e##.parentNode) (fun td ->
      Js.Opt.iter (td##.parentNode) (fun tr ->
        Js.Opt.iter (tr##.parentNode) (fun table ->
          Dom.removeChild table tr;
					renumber_children 1 (Dom.list_of_nodeList table##.childNodes)
        )
      )
    )
  )
;;

let%client new_row game_id id roles =
	let tps = Dom_html.getElementById "team_preference_select" in
	let r_cls = Js.Opt.case (Dom_html.CoerceTo.select tps)
		(fun () -> [])
		(fun x -> try List.assoc (Js.to_string x##.value) roles
			with Not_found -> []
		) in
	tr ~a:[a_class ["group_inscription_row"]; a_id (Printf.sprintf "gir_%d" id)]
	[
		td [
			Raw.input ~a:[a_class ["gir_text"]; a_id (Printf.sprintf "gir_text[%d]" id); a_name (Printf.sprintf "__co_eliom_person.uid[%d]" id); a_input_type `Search; a_value ""; a_list "users_list"] ();
		];
		td [
			Raw.input ~a:[a_onclick (fun e ->
				ignore (Eliom_client.window_open ~window_name:(Js.string "New user") ~service:~%new_provisional_user_service game_id));
			a_input_type `Button; a_value "New user"] ()
		];
		td [Raw.select ~a:[a_name (Printf.sprintf "__co_eliom_person.role[%d]" id)] (option (pcdata "Any")::List.map (fun x -> option (pcdata x)) (List.sort_uniq compare (remove_null (List.map snd r_cls))))];
		td [Raw.input ~a:[a_name (Printf.sprintf "__co_eliom_person.note[%d]" id); a_input_type `Text; a_value ""] ()];
		td [Raw.input ~a:[a_input_type `Button; a_value "Remove"; a_onclick remove_my_row] ()]
	]
;;

let%client add_inscription_row game_id it roles =
	let br = Dom_html.getElementById "button_row" in
  begin
    incr nr_ids;
    Dom.insertBefore it (Html.To_dom.of_element (new_row game_id !nr_ids roles)) (Js.some br);
  end
;;

let%shared group_name_row gname =
	tr ~a:[a_id "group_name_row"] [
		td ~a:[a_colspan 5] [
			pcdata "Group name: ";
			Raw.input ~a:[a_input_type `Text; a_name "__co_eliom_group_name"; a_value gname] ()
		]			
	]
;;

let%shared new_button game_id roles =
  Raw.input ~a:[a_id "add_button"; a_input_type `Button; a_value "Add group member"; a_onclick [%client (fun ev -> add_inscription_row ~%game_id (Dom_html.getElementById "inscription_table") ~%roles)]] ()
;;

let%client group_inscription_handler game_id roles gname ev =
	Js.Opt.iter (ev##.target) (fun x ->
		Js.Opt.iter (Dom_html.CoerceTo.input x) (fun i ->
			let it = Dom_html.getElementById "inscription_table" in
	    let bf = Dom_html.getElementById "button_field" in
			if Js.to_bool i##.checked then
      begin
      let sb = Dom_html.getElementById "submit_button" in
			let tpr = Dom_html.getElementById "team_preference_row" in
				nr_ids := 0;
				Dom.insertBefore it (Html.To_dom.of_element (group_name_row gname)) (Js.some tpr);
				add_inscription_row game_id it roles;
				Dom.insertBefore bf (Html.To_dom.of_element (new_button game_id roles)) (Js.some sb)
      end
      else
      begin
			let ab = Dom_html.getElementById "add_button" in
			let gnr = Dom_html.getElementById "group_name_row" in
        nr_ids := 0;
				Dom.removeChild it gnr;
        List.iter (fun x ->
          Js.Opt.iter (Dom_html.CoerceTo.element x) (fun e ->
						if Js.to_bool (e##.classList##contains (Js.string "group_inscription_row")) then
              Dom.removeChild it e
          )
        ) (Dom.list_of_nodeList it##.childNodes);
				Dom.removeChild bf ab
      end
		)
	)
;;

let send_signup_notification dsgs game_title group_name users =
	List.iter (fun (_, fname, lname, email) ->
		Mail.send_signup_notification fname lname email game_title group_name users
	) dsgs
;;

let do_signup_page game_id () (edit, (group_name, (team, users))) =
	let rec handle_inscriptions edit group_name users game_title game_loc game_dstr dsg_str status =
		match users with
		| (uid, (role, note))::tl -> 
			Lwt.catch (fun () ->
				Maw_db.get_user_data uid >>=
				fun (fname, lname, email, _, _) ->
      		Maw_db.add_inscription game_id uid group_name status
  		  		(if String.lowercase_ascii team = "any" then None else Some team)
			  		(if String.lowercase_ascii role = "any" then None else Some role) note >>=
						fun () -> 
							if not edit then
							Mail.send_simple_inscription_mail fname lname email game_title game_loc game_dstr dsg_str;
						Lwt.return ())
				(function
				| Not_found -> Maw_db.get_provisional_user_data uid >>=
					fun (email, fname, lname) ->
      		Maw_db.add_inscription game_id uid group_name status
  		  		(if String.lowercase_ascii team = "any" then None else Some team)
			  		(if String.lowercase_ascii role = "any" then None else Some role) note >>=
						fun () -> 
							if not edit then
							begin
								let uri = Eliom_uri.make_string_uri ~absolute:true
									~service:confirm_provisional_user_service uid in
								Mail.send_provisional_inscription_mail uri fname lname email game_title game_loc game_dstr dsg_str
							end;
						Lwt.return ()
				| e -> Lwt.fail e) >>=
				fun () -> handle_inscriptions edit group_name tl game_title game_loc game_dstr dsg_str status
		| _ -> Lwt.return ()
	in
	let%lwt u = Eliom_reference.get Maw.user in
	Lwt.catch (fun () -> match u with
	| Not_logged_in -> not_logged_in ()
	| User (uid, _, _, _)
	| Admin (_, (uid, _, _, _)) -> 
		let%lwt (game_title, game_date, game_loc, _, _, max_pl, _) =
			Maw_db.get_game_data game_id  in
		let%lwt dsgs = Maw_db.get_game_designers game_id in
		let dsg_str = designer_string dsgs in
		let game_dstr = match game_date with
			| Some d -> Printer.Date.sprint "%d %B %Y" d
			| _ -> "TBD" in
		let%lwt nr_inscr = Maw_db.get_nr_inscriptions game_id in
		handle_inscriptions edit group_name users game_title game_loc game_dstr dsg_str (if nr_inscr >= max_pl then `Waiting else `Interested) >>=
		fun () -> let%lwt users_ex = Lwt_list.map_s (fun (uid, (role, note)) ->
			let%lwt (fn, ln, _, _, _) = Maw_db.get_user_data uid in
			Lwt.return (uid, fn, ln, role, note)) users in 
		send_signup_notification dsgs game_title group_name users_ex;
		container (standard_menu [])
		[
			h1 [pcdata "Summary"];
			p [pcdata (match group_name with
			| None -> "Single person inscription"
			| Some x -> Printf.sprintf "Group inscription, name %s" x)];
			table (
				tr [
					td ~a:[a_colspan 3] [pcdata (Printf.sprintf "Team preference: %s" team)]
				]::
				tr [
					th [pcdata "Player"];
					th [pcdata "Role"];
					th [pcdata "Notes"]
				]::
				(List.map (fun (uid, fname, lname, role, note) ->
					tr [
						td [pcdata (Printf.sprintf "%s %s" fname lname)];
						td [pcdata role];
						td [pcdata note]
					]
				) users_ex)
			);
			p [
				pcdata (
					if edit
					then "Changes successfully saved."
					else "You have successfully signed up for this game."
				)
			]
		]
	)
	(fun e -> error_page (Printexc.to_string e))
;;

let%client check_signup_form ev =
	let email_regexp = Regexp.regexp "[^@]*@[^@]*\\.[^@]*" in
	let it = Dom_html.getElementById "inscription_table" in
	List.iter (fun tr_el ->
		Js.Opt.iter (Dom_html.CoerceTo.element tr_el) (fun tr ->
			if	Js.to_bool (tr##.classList##contains (Js.string "group_inscription_row")) then
			begin
				(* get contents of first td *)
				Js.Opt.iter (tr##.childNodes##item 0) (fun td ->
					Js.Opt.iter (td##.childNodes##item 0) (fun e ->
						Js.Opt.iter (Dom_html.CoerceTo.element e) (fun c ->
							Js.Opt.iter (Dom_html.CoerceTo.input c) (fun inp ->
								let input_name = Js.to_string inp##.value in
								Js.Opt.iter (td##.childNodes##item 1) (fun e ->
									Js.Opt.iter (Dom_html.CoerceTo.element e) (fun c ->
										Js.Opt.iter (Dom_html.CoerceTo.select c) (fun sel ->
											let si = sel##.selectedIndex in
											Js.Opt.iter (sel##.options##item si) (fun o ->
												Js.Opt.iter (o##.childNodes##item 0) (fun e ->
													Js.Opt.iter (Dom.CoerceTo.text e) (fun t ->
														let select_name = Js.to_string t##.data in
														if input_name <> select_name then
														(match Regexp.string_match email_regexp input_name 0 with
														| None -> Eliom_lib.alert "Please select a name or enter a valid e-mail address";
																Dom.preventDefault ev
														| Some _ -> ()
														)
													)
												)
											)
										)
									)
								)
							)
						)
					)
				)
			end
		)
	) (Dom.list_of_nodeList it##.childNodes)
;;

let%client initialise_signup me_inscr users e =
let udl = Dom_html.getElementById "users_list" in
	nr_ids := List.length me_inscr - 1;
	List.iter (fun (uid, fname, lname, _, s) ->
		match s with	
		| Some "P" -> Dom.appendChild udl (Html.To_dom.of_element 
			(Raw.option ~a:[a_value (Int32.to_string uid)]
			(pcdata (Printf.sprintf "%s %s (provisional)" fname lname))))
		| Some "U" -> Dom.appendChild udl (Html.To_dom.of_element
			(Raw.option ~a:[a_value (Int32.to_string uid)]
			(pcdata (Printf.sprintf "%s %s (unconfirmed)" fname lname))))
		| Some "H" -> ()
		| _ -> Dom.appendChild udl (Html.To_dom.of_element
			(Raw.option ~a:[a_value (Int32.to_string uid)]
			(pcdata (Printf.sprintf "%s %s" fname lname))))
	) users
;;

let%client change_team roles ev =
	let revamp_options sel =
		List.iter (fun opt ->
			Dom.removeChild sel opt
		) (Dom.list_of_nodeList sel##.childNodes);
		Dom.appendChild sel (Html.To_dom.of_element (Raw.option (pcdata "Any")));
		Js.Opt.iter (ev##.target) (fun e ->
			Js.Opt.iter (Dom_html.CoerceTo.select e) (fun t ->
				let rcl = try
					List.assoc (Js.to_string t##.value) roles
					with Not_found -> [] in
				List.iter (fun r ->
					Dom.appendChild sel (Html.To_dom.of_element (Raw.option (pcdata r)))
				) (List.sort_uniq compare (remove_null (List.map snd rcl)))
			)
		) in
	let git = Dom_html.getElementById "inscription_table" in
	List.iter (fun tr ->
		Js.Opt.iter (Dom_html.CoerceTo.element tr) (fun e ->
			if Js.to_bool (e##.classList##contains (Js.string "user_inscription_row"))
			then
			begin
				let name_td::role_td::_ = Dom.list_of_nodeList tr##.childNodes in
				let sel::_ = Dom.list_of_nodeList role_td##.childNodes in
				revamp_options sel
			end
			else if Js.to_bool (e##.classList##contains (Js.string "group_inscription_row"))
			then
			begin
				let name_td::_::role_td::_ = Dom.list_of_nodeList tr##.childNodes in
				let sel::_ = Dom.list_of_nodeList role_td##.childNodes in
				revamp_options sel
			end
		)
	) (Dom.list_of_nodeList git##.childNodes)
;;

let do_switch_page game_id () (new_group) =
	let%lwt u = Eliom_reference.get Maw.user in
	Lwt.catch (fun () -> match u with
	| Not_logged_in -> not_logged_in ()
	| User (uid, _, _, _)
	| Admin (_, (uid, _, _, _)) -> Lwt.catch (
		fun () -> Maw_db.find_group game_id new_group >>=
		fun ids -> if not (List.mem uid ids) then
			Maw_db.move_group game_id uid new_group >>=
			fun () -> container (standard_menu [])
			[
				h1 [pcdata "Request successful"];
				p [pcdata 
					(Printf.sprintf "Your inscription has now been moved to the group %s." new_group)]
			]
		else
			error_page "You are already a member of this group."
		)
		(function
		| Not_found -> error_page "That group does not exist."
		| e -> error_page (Printexc.to_string e))
	)
	(fun e -> error_page (Printexc.to_string e))
;;

let signup_page game_id () =
	let do_signup_service = create_attached_post
		~fallback:(preapply signup_service game_id)
		~post_params:(bool "edit" ** opt (string "group_name") **
			string "team" ** list "person" (int32 "uid" ** string "role" **
			string "note")) () in 
	let do_switch_service = create_attached_post
		~fallback:(preapply signup_service game_id)
		~post_params:(string "new_team") () in
	Maw_app.register ~scope:Eliom_common.default_session_scope ~service:do_signup_service (do_signup_page game_id);
	Maw_app.register ~scope:Eliom_common.default_session_scope ~service:do_switch_service (do_switch_page game_id);
	let%lwt u = Eliom_reference.get Maw.user in
	Lwt.catch (fun () -> match u with
	| Not_logged_in -> not_logged_in ()
	| User (my_uid, fname, lname, _)
	| Admin (_, (my_uid, fname, lname, _)) -> 
		let%lwt (title, date, loc, d, _, max_pl, _)  =
			Maw_db.get_game_data game_id in
		let%lwt dsgs = Maw_db.get_game_designers game_id in
		let dsg_str = designer_string dsgs in
    let%lwt roles = Maw_db.get_game_roles game_id in
		let%lwt nr_inscr = Maw_db.get_nr_inscriptions game_id in
		let%lwt inscr = Maw_db.get_inscription_data my_uid game_id in
		let%lwt users = Maw_db.get_users ~unconfirmed:true ~provisional:true () in
		let%lwt (id, cd, pd) = Maw_db.get_game_deadlines game_id in
		let me_inscr = if List.exists (fun (u, _, _, _, _, _, _, _) -> u = my_uid) inscr
			then inscr 
			else (my_uid, fname, lname, None, None, "", None, `Interested)::inscr in
		let signed_up = List.length inscr > 0 in
		let multiple_inscr = List.length me_inscr > 1 in
		let ex_group_name = if signed_up
			then List.hd (List.map (fun (_, _, _ , _, _, _, gn, _) -> gn)
			(List.sort_uniq (fun (u1, _, _, _, _, _, _, _) (u2, _, _, _, _, _, _, _) ->
				compare u1 u2) inscr))
			else None in
		if is_designer my_uid dsgs then
			container (standard_menu [])
			[
				h1 [pcdata title];
				p [pcdata "You are the designer of this game, and as such you do not need to sign up."]
			]
		else
			container
				~onload:[%client (fun e -> initialise_signup ~%me_inscr ~%users e)]
			(standard_menu [])
			(
				datalist ~a:[a_id "users_list"] ()::
				h1 [pcdata title]::
				p [pcdata (Printf.sprintf "%s, %s" loc (date_or_tbd date))]::
				p [i [pcdata (Printf.sprintf "Designed by %s" dsg_str)]]::
				p [pcdata d]::
				(match id with
				| Some ddl when Date.compare ddl (Date.today ()) < 0 ->
					[p [pcdata "The inscription deadline for this game has passed."]]
				| _ ->
				begin
					cond_list (nr_inscr >= max_pl)
					(p [i [pcdata "This game has reached its maximum number of participants. Any new inscriptions will be placed on the waiting list."]])
					(h2 [pcdata (if signed_up then "[dit inscription" else "Sign up")]::
					(cond_list signed_up
						(Form.post_form ~service:do_switch_service
						(fun (new_group) ->
						[
							p [pcdata "If you wish to switch your inscription to another group, please enter its name below:"];
							table
							[
								tr [
									td [pcdata "New group: "];
									td [Form.input ~input_type:`Text ~name:new_group Form.string]
								];
								tr
								[
									td ~a:[a_colspan 2]
									[Form.input ~input_type:`Submit ~value:"Switch" Form.string]
								]
							]
						]) ())
					[Form.post_form ~service:do_signup_service
					(fun (edit, (group_name, (team, person))) ->
					[
						table ~a:[a_id "inscription_table"] (
							tr [
								td ~a:[a_colspan 5] [
									Raw.input ~a:(a_input_type `Checkbox::a_onclick [%client (group_inscription_handler ~%game_id ~%roles (default "" ~%ex_group_name))]::(match ex_group_name with
									| None -> []
									| Some _ -> [a_checked ()])) ();
          				pcdata "This is a group inscription"
								]
							]::
							cond_list multiple_inscr
							(group_name_row (default "" ex_group_name))
							(tr ~a:[a_id "team_preference_row"] [
								td ~a:[a_colspan 5] [
									pcdata "Team preference: ";
									Form.select ~a:[a_id "team_preference_select";
										a_onchange [%client change_team ~%roles]] ~name:team
										Form.string
									(Form.Option ([], "Any", None, false))
									(List.map (fun (t, _) ->
										Form.Option ([], t, None, false)
									) roles)
								]
							]::
							tr [
								th ~a:[a_colspan 2] [pcdata "Name"];
								th [pcdata "Role preference"];
								th [pcdata "Note"];
            		th [];
							]::
							person.it (fun (uid, (role, note))
								(ex_uid, ex_fname, ex_lname, _, r, ex_note, _, _) init ->
								let ex_role = default "Any" r in
								tr ~a:[a_class [if ex_uid = my_uid then "user_inscription_row"
									else "group_inscription_row"]] [
									td ~a:[a_colspan 2] [
										Form.input ~input_type:`Hidden ~name:uid ~value:ex_uid Form.int32;
										pcdata (Printf.sprintf "%s %s" ex_fname ex_lname)
									]; 
									td [
										Form.select ~a:[a_class ["role_select"]] ~name:role Form.string
										(Form.Option ([], "Any", None, ex_role = "Any"))
										[]
									];
									td [Form.input ~input_type:`Text ~name:note ~value:ex_note Form.string];
    							td (if multiple_inscr then
										[Raw.input ~a:[a_input_type `Button; a_value "Remove"; a_onclick [%client remove_my_row]] ()]
									else
										[]
									)
								]::
								init
							) me_inscr
							[
								tr ~a:[a_id "button_row"] [
									td ~a:[a_id "button_field"; a_colspan 5] (
									cond_list
										(List.length me_inscr > 1)
										(new_button game_id roles)
										(Form.input ~a:[a_id "submit_button"; a_onclick [%client check_signup_form]] ~input_type:`Submit ~value:(if signed_up then "Save changes" else "Sign up") Form.string::
										if signed_up
										then [Form.input ~input_type:`Hidden ~name:edit ~value:true Form.bool]
										else [])
								)]	
							])
						)
					]) ()
				]))
			end)
		)
	)
	(function 
	| Not_found -> unknown_game ()
	| e -> error_page (Printexc.to_string e)
	)
;;

let _ =
	Maw_app.register ~service:signup_service signup_page
;;

[%%shared
	open Eliom_lib
	open Eliom_content
	open Html.D
	open Eliom_service
	open Eliom_parameter
	open Utils
]

[%%server
	open CalendarLib
	open Maw
]

let game_service = create ~path:(Path ["game"]) ~meth:(Get (suffix (int32 "game_id"))) ();;
let signup_service = create ~path:(Path ["signup"]) ~meth:(Get (suffix (int32 "game_id"))) ();;
let show_inscriptions_service = create ~path:(Path ["inscriptions"]) ~meth:(Get (suffix (int32 "game_id"))) ();;
let show_casting_service = create ~path:(Path ["casting"]) ~meth:(Get (suffix (int32 "game_id"))) ();;

let game_page game_id () =
  let standard_game_data title loc date dsg_fname dsg_lname d full =
		h1 [pcdata title]::
		p [pcdata (Printf.sprintf "%s, %s" loc (date_or_tbd date))]::
		p [i [pcdata (Printf.sprintf "Designed by %s %s" dsg_fname dsg_lname)]]::
		p [pcdata d]::
    (if full
    then [p [i [pcdata "This game has reached its maximum number of inscriptions. You can still sign up, but you will be placed on a waiting list."]]]
    else []) in
	let%lwt u = Eliom_reference.get Maw.user in
	Lwt.catch (fun () -> let%lwt (title, date, loc, dsg_fname, dsg_lname, dsg, d, _, max_pl, _) =
		Database.get_game_data game_id in
    let%lwt nr_inscr = Database.get_nr_inscriptions game_id in
    match u with
	  | None -> container (standard_menu ()) 
			(standard_game_data title loc date dsg_fname dsg_lname d (nr_inscr >= max_pl))
	  | Some (uid, _, _, _) ->
			let%lwt l = Database.get_inscription_data uid game_id in
			container (standard_menu ()) 
			(standard_game_data title loc date dsg_fname dsg_lname d (nr_inscr >= max_pl) @
		  	if uid = dsg then
				[
					p [a ~service:Design.design_service [pcdata "Edit the game design"] game_id];
					p [a ~service:show_inscriptions_service [pcdata "Show inscriptions for this game"] game_id];
					p [a ~service:Design.message_service [pcdata "Send a message to players"] game_id]
				]
				else if List.length l > 0 then
				let (_, _, _, team, role, _, _, status) = List.find
					(fun (u, _, _, _, _, _, _, _) -> uid = u) l in
				[
					p [
						i [pcdata (match status with
						| `Interested -> "You have registered your interest for this game."
						| `Confirmed -> "Your place for this game is confirmed."
						| `Paid -> "You have paid for this game and your place is confirmed."
						| `Provisional -> "You have no account, but are still seeing this. That shouldn't happen. Weird."
						| `Waiting -> "You are on the waiting list for this game.")
						];
						pcdata (Printf.sprintf "Your team preference is %s and your role preference is %s." (default "Any" team) (default "Any" role))
					];
					a ~service:signup_service [pcdata "Edit my inscription"] game_id
				]
				else
				[
					a ~service:signup_service [pcdata "Sign up for this game"] game_id
				]
			)
	)
	(function
	| Not_found -> unknown_game ()
	| e -> error_page (Printexc.to_string e)
	)
;;

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
					Js.Unsafe.set input_uid "name" (Printf.sprintf "person.uid[%d]" n);
					Js.Unsafe.set input_role "name" (Printf.sprintf "person.role_type[%d]" n); 
					Js.Unsafe.set input_note "name" (Printf.sprintf "person.note[%d]" n); 
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

let%client new_row id roles =
  tr ~a:[a_class ["group_inscription_row"]; a_id (Printf.sprintf "gir_%d" id)] [
    td [
			Raw.input ~a:[a_class ["gir_text"]; a_id (Printf.sprintf "gir_text[%d]" id); a_name (Printf.sprintf "person.uid[%d]" id); a_input_type `Search; a_value ""; a_list "users_list"] ();
		];
		td [
			pcdata "[new user]"
		];
    td [Raw.select ~a:[a_name (Printf.sprintf "person.role_type[%d]" id)] (option (pcdata "Any")::List.map (fun x -> (option (pcdata x))) roles)];
    td [Raw.input ~a:[a_name (Printf.sprintf "person.note[%d]" id); a_input_type `Text; a_value ""] ()];
    td [Raw.input ~a:[a_input_type `Button; a_value "Remove"; a_onclick remove_my_row] ()]
  ]
;;

let%client nr_ids = ref 0

let%client add_inscription_row it roles =
	let br = Dom_html.getElementById "button_row" in
  begin
    incr nr_ids;
    Dom.insertBefore it (Html.To_dom.of_element (new_row !nr_ids roles)) (Js.some br);
  end
;;

let%shared group_name_row gname =
	tr ~a:[a_id "group_name_row"] [
		td ~a:[a_colspan 5] [
			pcdata "Group name: ";
			Raw.input ~a:[a_input_type `Text; a_name "group_name"; a_value gname] ()
		]			
	]
;;

let%shared new_button roles =
  Raw.input ~a:[a_id "add_button"; a_input_type `Button; a_value "Add group member"; a_onclick [%client (fun ev -> add_inscription_row (Dom_html.getElementById "inscription_table") ~%roles)]] ()
;;

let%client group_inscription_handler roles gname ev =
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
				add_inscription_row it roles;
				Dom.insertBefore bf (Html.To_dom.of_element (new_button roles)) (Js.some sb)
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

let do_signup_page game_id () (edit, (group_name, (team, users))) =
	let rec handle_inscriptions edit group_name users game_title game_loc game_dstr dsg_fname dsg_lname status =
		match users with
		| (uid, (role, note))::tl -> 
				Database.get_user_data uid >>=
				fun (fname, lname, email) ->
      		Database.add_inscription game_id uid group_name status
  		  		(if String.lowercase_ascii team = "any" then None else Some team)
			  		(if String.lowercase_ascii role = "any" then None else Some role) note >>=
						fun () -> Mail.send_simple_inscription_mail fname lname email game_title game_loc game_dstr dsg_fname dsg_lname;
						Lwt.return () >>=
				fun () -> handle_inscriptions edit group_name tl game_title game_loc game_dstr dsg_fname dsg_lname status
		| _ -> Lwt.return ()
	in
	let%lwt u = Eliom_reference.get Maw.user in
	Lwt.catch (fun () -> match u with
	| None -> not_logged_in ()
	| Some (uid, _, _, _) -> 
		let%lwt (game_title, game_date, game_loc, dsg_fname, dsg_lname, _, _, _, max_pl, _) =
			Database.get_game_data game_id  in
		let game_dstr = match game_date with
			| Some d -> Printer.Date.sprint "%d %B %Y" d
			| _ -> "TBD" in
		let%lwt nr_inscr = Database.get_nr_inscriptions game_id in
		handle_inscriptions edit group_name users game_title game_loc game_dstr dsg_fname dsg_lname (if nr_inscr >= max_pl then `Waiting else `Interested) >>=
		fun () -> container (standard_menu ())
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
					th [pcdata "Role type"];
					th [pcdata "Notes"]
				]::
				(List.map (fun (uid, (role_type, note)) ->
					tr [
						td [pcdata (Printf.sprintf "%ld" uid)];
						td [pcdata role_type];
						td [pcdata note]
					]
				) users)
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
begin
	nr_ids := List.length me_inscr - 1;
	List.iter (fun (uid, fname, lname, _, s) ->
		let status = match s with
		| Some "P" -> " (provisional)"
		| Some "U" -> " (unconfirmed)"
		| _ -> "" in
		let thing = Raw.option ~a:[a_value (Printf.sprintf "%ld" uid)]
			(pcdata (Printf.sprintf "%s %s%s" fname lname status)) in
		Dom.appendChild udl (Html.To_dom.of_element thing)
	) users
end

let signup_page game_id () =
	let do_signup_service = create_attached_post
		~fallback:(preapply signup_service game_id)
		~post_params:(bool "edit" ** opt (string "group_name") **
			string "team" ** list "person" (int32 "uid" ** string "role_type" **
			string "note")) () in 
	Maw_app.register ~scope:Eliom_common.default_session_scope ~service:do_signup_service (do_signup_page game_id);
	let%lwt u = Eliom_reference.get Maw.user in
	Lwt.catch (fun () -> match u with
	| None -> not_logged_in ()
	| Some (my_uid, fname, lname, _) -> 
		let%lwt (title, date, loc, dsg_fname, dsg_lname, dsg_id, d, _, max_pl, _)  =
			Database.get_game_data game_id in
    let%lwt teams = Lwt.return [] (*Database.get_game_teams game_id*) in
		let%lwt role_types = Lwt.return [] (*Database.get_game_role_types game_id*) in
		let%lwt nr_inscr = Database.get_nr_inscriptions game_id in
		let%lwt inscr = Database.get_inscription_data my_uid game_id in
		let%lwt users = Database.get_users ~unconfirmed:true ~provisional:true () in
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
		container ~onload:[%client (fun e -> initialise_signup ~%me_inscr ~%users e)]
		(standard_menu ())
		(
			datalist ~a:[a_id "users_list"] ()::
			h1 [pcdata title]::
			p [pcdata (Printf.sprintf "%s, %s" loc (date_or_tbd date))]::
		  p [i [pcdata (Printf.sprintf "Designed by %s %s" dsg_fname dsg_lname)]]::
			p [pcdata d]::
			cond_list (nr_inscr >= max_pl)
			(p [i [pcdata "This game has reached its maximum number of participants. Any new inscriptions will be placed on the waiting list."]])
			[h2 [pcdata (if signed_up then "Edit inscription" else "Sign up")];
			p [i [pcdata "For group inscriptions, you can either find an existing user with the dropdown box, or enter an e-mail address. In the latter case, if it does not exist already, a new account will be created for that user."]];
			Form.post_form ~service:do_signup_service
			(fun (edit, (group_name, (team, person))) ->
      [
				table ~a:[a_id "inscription_table"] (
        	tr [
						td ~a:[a_colspan 5] [
       	       Raw.input ~a:(a_input_type `Checkbox::a_onclick [%client (group_inscription_handler ~%role_types (default "" ~%ex_group_name))]::(match ex_group_name with
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
							Form.select ~name:team Form.string
							(Form.Option ([], "Any", None, false))
							(List.map (fun t ->
								Form.Option ([], t, None, false)
							) teams)
						]
					]::
					tr [
						th ~a:[a_colspan 2] [pcdata "Name"];
						th [pcdata "Role type preference"];
						th [pcdata "Note"];
            th [];
					]::
					person.it (fun (uid, (role_type, note))
						(ex_uid, ex_fname, ex_lname, _, r, ex_note, _, _) init ->
						let ex_role = default "Any" r in
						tr ~a:[a_class [if ex_uid = my_uid then "user_inscription_row"
							else "group_inscription_row"]] [
							td ~a:[a_colspan 2] [
								Form.input ~input_type:`Hidden ~name:uid ~value:ex_uid Form.int32;
								pcdata (Printf.sprintf "%s %s" ex_fname ex_lname)
							]; 
							td [
								Form.select ~name:role_type Form.string
								(Form.Option ([], "Any", None, ex_role = "Any"))
								(List.map (fun r ->
									Form.Option ([], r, None, ex_role = r)
								) role_types)
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
								(new_button role_types)
								(Form.input ~a:[a_id "submit_button"; a_onclick [%client check_signup_form]] ~input_type:`Submit ~value:(if signed_up then "Save changes" else "Sign up") Form.string::
								if signed_up
								then [Form.input ~input_type:`Hidden ~name:edit ~value:true Form.bool]
								else [])
						)]	
					])
				)
			]) ()
		])
	)
	(function 
	| Not_found -> unknown_game ()
	| e -> error_page (Printexc.to_string e)
	)
;;

let show_inscriptions_page game_id () =
	let status_word st =
		match st with
		| "T" -> "Potential"
		| "I" -> "Interested"
		| "W" -> "Waiting"
		| "C" -> "Confirmed"
		| "P" -> "Paid"
		| "N" -> "Cancelled" 
		| "X" -> "No-show" in
	let%lwt u = Eliom_reference.get Maw.user in
	Lwt.catch (fun () -> match u with
	| None -> not_logged_in ()
	| Some (uid, _, _, _) ->
		let%lwt (title, date, loc, _, _, dsg_id, d, min_nr, max_nr, _) =
			Database.get_game_data game_id in
		let%lwt inscr = Database.get_inscription_list game_id in
		if uid = dsg_id then
			container (standard_menu ())
			(
				(h1 [pcdata	title])::
				[
					p [pcdata (Printf.sprintf "There are currently %d inscriptions." (List.length inscr))];
					table (
						tr [
							th [pcdata "Status"];
							th [pcdata "Group name"];
							th [pcdata "Player name"];
							th [pcdata "Team"];
							th [pcdata "Role"];
							th [pcdata "Note"]
						]::
						List.map (fun (fname, lname, _, t, r, nt, g, st) ->
							tr [
								td [pcdata (status_word st)];
								td [pcdata (default "" g)];
								td [pcdata (Printf.sprintf "%s %s" fname lname)];
								td [pcdata (default "Any" t)];
								td [pcdata (default "Any" r)];
								td [i [pcdata nt]]
							]
						) inscr
					)
				]
			)
		else
			container (standard_menu ())
			[
				p [pcdata "You are not the designer of this game."]
			]
	)
	(function
	| Not_found -> unknown_game ()
	| e -> error_page (Printexc.to_string e)
	)
;;

let show_casting_page game_id () =
	Lwt.catch (fun () -> let%lwt (title, _, _, _, _, _, _, _, _, cp) =
			Database.get_game_data game_id in
		if cp then
		begin
			let%lwt casting = Database.get_casting game_id in
			(*let teams = List.sort_uniq (fun (t1, _, _, _, _, _, _) (t2, _, _, _, _, _, _) ->
				compare t1 t2) casting in*)
			container (standard_menu ())
			(
				h1 [pcdata (Printf.sprintf "Casting for %s" title)]::
				List.map (fun (t, roles) ->
					table ~a:[a_class ["team_table"]] (
						tr [
							th ~a:[a_class ["team_name"]; a_colspan 2] [pcdata t]
						]::
						tr [
							th ~a:[a_class ["header"]] [pcdata "Role"];
							th ~a:[a_class ["header"]] [pcdata "Name"]
						]::
						List.map (fun (rn, fname, lname, _, _, _) ->
							tr [
								td [pcdata rn];
								td [match fname, lname with
								| Some f, Some l -> pcdata (Printf.sprintf "%s %s" f l)
								| _, _ -> b [pcdata "vacancy"]
								]
							]
						) roles
					)
				) casting
			)
		end
		else error_page "The casting for this game has not been published."
	)
	(function
	| Not_found -> unknown_game ()
	| e -> error_page (Printexc.to_string e)
	)
;;

let _ =
	Maw_app.register ~service:game_service game_page;
	Maw_app.register ~service:signup_service signup_page;
	Maw_app.register ~service:show_inscriptions_service show_inscriptions_page;
	Maw_app.register ~service:show_casting_service show_casting_page
;;

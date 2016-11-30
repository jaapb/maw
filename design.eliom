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

let design_service = create ~path:(Path ["design"]) ~meth:(Get (suffix (int32 "game_id"))) ();;
let update_descr_service = create ~path:(Path ["design"]) ~meth:(Post (suffix (int32 "game_id"), string "description")) ();;
let update_numbers_service = create ~path:(Path ["design"]) ~meth:(Post (suffix (int32 "game_id"), int32 "min" ** int32 "max")) ();;
let role_service = create ~path:(Path ["role"]) ~meth:(Get (suffix (int32 "game_id"))) ();;
let cast_service = create ~path:(Path ["cast"]) ~meth:(Get (suffix (int32 "game_id"))) ();;
let do_cast_service = create ~path:(Path ["cast"]) ~meth:(Post (suffix (int32 "game_id"), list "team" (string "name" ** list "member" (string "role" ** int32 "id")) ** bool "publish")) ();;
let message_service = create ~path:(Path ["messaging"]) ~meth:(Get (suffix (int32 "game_id"))) ();;
let do_message_service = create ~path:(Path ["messaging"]) ~meth:(Post (suffix (int32 "game_id"), string "team" ** string "subject" ** string "contents")) ();;

let design_page game_id () = 
	let%lwt u = Eliom_reference.get Maw.user in
	Lwt.catch (fun () -> match u with
	| None -> not_logged_in ()
	| Some (uid, _, _, _) -> 
		let%lwt (title, date, loc, _, _, dsg_id, d, min_nr, max_nr, _) =
			Database.get_game_data game_id in
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

let%client switch_active ev =
 	Js.Opt.iter (ev##.target) (fun e ->
		if Js.to_bool (e##.classList##contains (Js.string "active"))
		then e##.classList##remove (Js.string "active")
		else e##.classList##add (Js.string "active") 
	)
;;

let%client new_input () =
  Html.To_dom.of_element (td [
		Raw.input ~a:[a_input_type `Text; a_value ""] ()
	]);;

let%client do_move ev =
	let id = Dom_html.getElementById "inscr_div" in
	let td = Dom_html.getElementById "teams" in
	Js.Opt.iter (ev##.target) (fun gt_th ->
		Js.Opt.iter (gt_th##.parentNode) (fun gt_tr ->
			Js.Opt.iter (gt_tr##.parentNode) (fun dst ->
				List.iter (fun it ->
					List.iter (fun it_tr ->
						List.iter (fun it_td ->
							Js.Opt.iter (Dom_html.CoerceTo.element it_td) (fun e ->
								if Js.to_bool (e##.classList##contains (Js.string "active"))
								then begin
                	if List.length (Dom.list_of_nodeList it_tr##.childNodes) = 1
                	then 
                  	Dom.insertBefore it_tr (new_input ()) (Js.some it_td);
									Dom.appendChild dst it_tr;
									e##.classList##remove (Js.string "active")
								end
							)
						) (Dom.list_of_nodeList it_tr##.childNodes)
					) (Dom.list_of_nodeList it##.childNodes)
				) (Dom.list_of_nodeList id##.childNodes);
				List.iter (fun td_table ->
					List.iter (fun td_tr -> 
						List.iter (fun e -> Js.Opt.iter (Dom_html.CoerceTo.element e) (fun td_td ->
							if Js.to_bool (td_td##.classList##contains (Js.string "active"))
							then begin
								Dom.appendChild dst td_tr;
								td_td##.classList##remove (Js.string "active")
							end
						)) (Dom.list_of_nodeList td_tr##.childNodes)
					) (Dom.list_of_nodeList td_table##.childNodes)
				) (Dom.list_of_nodeList td##.childNodes)
			)
		);
	)
;;

let%client before_submit ev =
	let td = Dom_html.getElementById "teams" in
	let team_nr = ref 0 in
	List.iter (fun e -> Js.Opt.iter (Dom_html.CoerceTo.element e) (fun td_table ->
		if Js.to_bool (td_table##.classList##contains (Js.string "team_table")) then
		begin
			let player_nr = ref 0 in
			List.iter (fun e -> Js.Opt.iter (Dom_html.CoerceTo.element e) (fun td_tr ->
				if Js.to_bool (td_tr##.classList##contains (Js.string "player_row")) then
				let td_role::td_pid::_ = Dom.list_of_nodeList td_tr##.childNodes in
				let input_role::_ = Dom.list_of_nodeList td_role##.childNodes in
				let input_pid::_ = Dom.list_of_nodeList td_pid##.childNodes in	
				begin
					Js.Unsafe.set input_role "name" (Printf.sprintf "team.member[%d].role[%d]" !team_nr !player_nr);
					Js.Unsafe.set input_pid "name" (Printf.sprintf "team.member[%d].id[%d]" !team_nr !player_nr);
					incr player_nr
				end
			)) (Dom.list_of_nodeList td_table##.childNodes);
			incr team_nr
		end
	)) (Dom.list_of_nodeList td##.childNodes)
;;

let%client add_role team_nr ev =
  let new_input = Html.To_dom.of_element (Raw.input ~a:[a_class ["role"]; a_input_type `Text; a_value ""] ()) in
 	Js.Opt.iter (ev##.target) (fun e ->
		Js.Opt.iter (e##.parentNode) (fun p ->
			Dom.insertBefore p new_input (Js.some e)
		)
	)
;;

let%client add_team team_nr ev =
	let table = Dom_html.getElementById "role_table" in
	let last_row = Dom_html.getElementById "add_button_row" in
	let new_row = Html.To_dom.of_element (tr ~a:[a_class ["team_row"]] [
		td [
			Raw.input ~a:[a_input_type `Text] ()
		];
		td [
			Raw.input ~a:[a_input_type `Button; a_value "Add role";
				a_onclick (add_role team_nr)] ()
		]
	]) in
	Dom.insertBefore table new_row (Js.some last_row)
;;

let%client do_role_save ev =
	let rec renumber_children n trs =
		match trs with
		| [] -> ()
		| h::t -> Js.Opt.iter (Dom_html.CoerceTo.element h) (fun tr ->
			if Js.to_bool (tr##.classList##contains (Js.string "team_row"))
			then
			begin
				let td_team::td_role::_ = Dom.list_of_nodeList tr##.childNodes in
				let inp_team::_ = Dom.list_of_nodeList td_team##.childNodes in
				Js.Unsafe.set inp_team "name" (Printf.sprintf "__co_eliom_team.name[%d]" n);
				let m = ref 0 in
				List.iter (fun inp_role ->
					Js.Opt.iter (Dom_html.CoerceTo.element inp_role) (fun i ->
						if Js.to_bool (i##.classList##contains (Js.string "role"))
						then
						begin
							Js.Unsafe.set inp_role "name" (Printf.sprintf "__co_eliom_team.role[%d].name[%d]" n !m);
							incr m
						end
					)
				) (Dom.list_of_nodeList td_role##.childNodes);
				renumber_children (n + 1) t
			end
			else
				renumber_children n t	
			)
	in
	(
	Eliom_lib.alert "Renumbering children...";
	let table = Dom_html.getElementById "role_table" in
	renumber_children 0 (Dom.list_of_nodeList table##.childNodes)
	)
;;

let do_role_page game_id () teams =
	Database.update_teams game_id teams >>=
	fun () -> container (standard_menu ())
	(
		h1 [pcdata "Your teams"]::
		(List.flatten (List.map (fun (name, roles) ->
			[
				h2 [pcdata name];
				ul (List.map (fun role ->
						li [pcdata role]
				) roles) 
			]
		) teams))
	)
;;

let role_page game_id () =
	let do_role_service = create_attached_post ~fallback:(preapply role_service game_id)
		~post_params:(list "team" (string "name" ** list "role" (string "name"))) () in
	Maw_app.register ~scope:Eliom_common.default_session_scope
		~service:do_role_service (do_role_page game_id);
	let%lwt u = Eliom_reference.get Maw.user in
	Lwt.catch (fun () -> match u with
	| None -> not_logged_in ()
  | Some (uid, _, _, _) -> let%lwt (title, _, _, _, _, dsg_id, _, _, _, _) =
      Database.get_game_data game_id in
    if uid <> dsg_id then error_page "You are not the designer of this game."
    else
			let%lwt roles = Database.get_game_roles game_id in
			let nr = ref 0 in
			container (standard_menu ())
			[
				h1 [pcdata title];
      	Form.post_form ~service:do_role_service (fun team -> [
					table ~a:[a_class ["role_table"]; a_id "role_table"] (
						tr [
							th [pcdata "Team"];
							th [pcdata "Roles"]
						]::
						team.it (fun (t_name, role) (t, rs) init ->
							tr ~a:[a_class ["team_row"]] [
								td [
									Form.input ~input_type:`Text ~name:t_name ~value:t Form.string
								];
								td (
									role.it (fun r_name r init' ->
										Form.input ~a:[a_class ["role"]] ~input_type:`Text
											~name:r_name ~value:r Form.string::init'
									) rs
									[Raw.input ~a:[a_input_type `Button; a_value "Add role"; a_onclick [%client add_role ~%(!nr)]] ()] 
								)
							]::init
						) roles
						[tr ~a:[a_id "add_button_row"] [td [Raw.input ~a:[a_input_type `Button; a_value "Add group"; a_onclick [%client add_team ~%(List.length roles)]] ()]];
						 tr [td ~a:[a_colspan 2] [Form.input ~a:[a_onclick [%client do_role_save]] ~input_type:`Submit ~value:"Save" Form.string]]]
					)
				]) ()
			]
	)
	(function
	| Not_found -> unknown_game ()
	| e -> error_page (Printexc.to_string e)
	)
;;

let cast_page game_id () =
	let partition_groups l =
		let rec pg_aux l g (rhd: 'a list) (res: (string option * _) list): (string option * _) list = 
			match l with
			| [] -> ((g, rhd)::res)
			| (_, _, _, _, _, _, g', _) as x::l' -> 
				if g = g' then pg_aux l' g (x::rhd) res
				else pg_aux l' g' [x] ((g, rhd)::res)
		in
		match l with
		| [] -> []
		| (_, _, _, _, _, _, g, _) as x::l' -> pg_aux l' g [x] [] 
	in
  let%lwt u = Eliom_reference.get Maw.user in
	Lwt.catch (fun () -> match u with
  | None -> not_logged_in ()
  | Some (uid, _, _, _) -> let%lwt (title, _, _, _, _, dsg_id, _, _, _, _) =
      Database.get_game_data game_id in
    if uid <> dsg_id then error_page "You are not the designer of this game."
    else
    let%lwt inscr = Database.get_inscription_list ~filter_cast:true game_id in
		let%lwt casting = Database.get_casting game_id in
		let%lwt pub = Database.is_published game_id in
    container (standard_menu ())
    [
      Form.post_form ~service:do_cast_service (fun (team, publish) ->
      [
			  div ~a:[a_id "players"]
			  [
			  	h2 [pcdata "Players"];
					div ~a:[a_id "inscr_div"]
					(List.map (fun (gn, g) -> 
						table ~a:[a_class ["casting"]]
						(tr [th ~a:[a_class ["group_name"]; a_colspan 2] [pcdata (default "Ungrouped" gn)]]::
        		List.map (fun (fname, lname, p_id, _, _, n, _, _) ->
        			tr ~a:[a_class ["player_row"]] [
								td ~a:[
          	   		a_onclick [%client switch_active];
							  	a_title n
            		] [
									Raw.input ~a:[a_input_type `Hidden; a_value (Int32.to_string p_id)] (); 	
									pcdata (Printf.sprintf "%s %s" fname lname)
								]
            	]
          	) g)
					) (List.rev (partition_groups inscr)))
			  ];
        div ~a:[a_id "teams"] (
			  	h2 [pcdata "Teams"]::
					team.it (fun (t_name, member) (t, roles) init ->
       	    table ~a:[a_class ["casting"; "team_table"]] (
         	  	tr [
								th ~a:[a_class ["team_name"]; a_onclick [%client do_move];
									a_colspan 2] [
									Form.input ~input_type:`Hidden ~name:t_name ~value:t Form.string;
									pcdata t
							 	];
							]::
              tr [
                th ~a:[a_class ["header"]] [pcdata "Role"];
                th ~a:[a_class ["header"]] [pcdata "Name"]
              ]::
						  member.it (fun (p_role, p_uid) (rn, pfname, plname, pid, n, g) init ->
						 		tr ~a:[a_class ["player_row"]] [
									td [
										Form.input ~input_type:`Text ~name:p_role ~value:rn Form.string
									];
									td ~a:[
          	   			a_onclick [%client switch_active];
							  		a_title (default "" n)]
									(match pid with
									| None -> [ pcdata "vacancy" ];
									| Some p ->
										[
											Form.input ~input_type:`Hidden ~name:p_uid ~value:p Form.int32;
											pcdata (Printf.sprintf "%s %s"
												(default "" pfname) (default "" plname))
										]
									)
								]::init
						  ) roles 
							[]
						)::init
      	  ) casting
					[]
        );
        div ~a:[a_id "general"] [
					table [
						tr [
							if pub then
								td [
									pcdata "Casting has already been published.";
									Form.input ~input_type:`Hidden ~name:publish ~value:true Form.bool
								]
							else
								td [
									pcdata "Publish casting: ";
									Form.bool_checkbox_one ~name:publish ~checked:pub ()
								]
						];
						tr [
          		td [Form.input ~input_type:`Submit ~value:"Save casting"
								~a:[a_onclick [%client before_submit]] Form.string
							]
						]
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

let do_cast_page game_id (teams, publish) =
	let%lwt u = Eliom_reference.get Maw.user in
	(match u with
	| None -> Lwt.return ()
	| Some (uid, _, _, _) -> 
		Database.clear_casting game_id >>=
		fun () -> Lwt_list.iter_s (fun (name, members) ->
			Lwt_list.iter_s (fun (role, pid) ->
				Database.add_casting game_id name role pid
			) members
		) teams) >>=
	fun () -> Database.set_published game_id publish >>=
	fun () -> container (standard_menu ())
	[
		h1 [pcdata "Casting"];
		p [pcdata
			(if publish
			then "Casting saved and published."
			else "Casting saved.")]
	]
;;

let message_page game_id () =
  let%lwt u = Eliom_reference.get Maw.user in
  match u with
  | None -> not_logged_in ()
  | Some (uid, _, _, _) -> 
		let%lwt (title, date, loc, _, _, dsg_id, d, min_nr, max_nr, _) =
			Database.get_game_data game_id in
		if uid <> dsg_id then error_page "You are not the designer of this game."
    else
			let%lwt teams = Database.get_game_teams game_id in
		container (standard_menu ())
		[
			h1 [pcdata "Send message"];
			Form.post_form ~service:do_message_service (fun (dest, (subject, text)) ->
			[
				table
				[	
					tr [
						td [
							pcdata "Send to:";
							Form.select ~name:dest Form.string
							(Form.Option ([], "all", Some (pcdata "All players"), false))
							(List.map (fun t ->
								Form.Option ([], "team:t", Some (pcdata t), false)
								) teams
							)
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
			]) game_id
		]
;;

let do_message_page game_id _ =
  let%lwt u = Eliom_reference.get Maw.user in
  match u with
  | None -> not_logged_in ()
  | Some (uid, _, _, _) -> 
		let%lwt (title, date, loc, _, _, dsg_id, d, min_nr, max_nr, _) =
			Database.get_game_data game_id in
		if uid <> dsg_id then error_page "You are not the designer of this game."
    else
		container (standard_menu ())
		[
			h1 [pcdata "Message sent"];
			p [pcdata "Aren't you proud of me?"] 
		]
;;
let () =
	Maw_app.register ~service:design_service design_page;;
	Eliom_registration.Action.register ~service:update_descr_service
		update_description;
	Eliom_registration.Action.register ~service:update_numbers_service
		update_numbers;
  Maw_app.register ~service:role_service role_page;
  Maw_app.register ~service:cast_service cast_page;
	Maw_app.register ~service:do_cast_service do_cast_page;
	Maw_app.register ~service:message_service message_page;
	Maw_app.register ~service:do_message_service do_message_page
;;

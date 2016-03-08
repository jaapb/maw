[%%shared
	open Eliom_lib
	open Eliom_content.Html5
	open Eliom_content.Html5.D
	open Eliom_service.App
	open Eliom_parameter
]

[%%server
	open CalendarLib
	open Maw
]

let design_service = service ~path:["design"] ~get_params:(suffix (int32 "game_id")) ();;
let update_descr_service = post_service ~fallback:design_service ~post_params:(string "description") ();;
let update_numbers_service = post_service ~fallback:design_service ~post_params:(int32 "min" ** int32 "max") ();;
let remove_teams_service = post_service ~fallback:design_service ~post_params:(set string "teams") ();;
let add_team_service = post_service ~fallback:design_service ~post_params:(string "team") ();;
let remove_role_types_service = post_service ~fallback:design_service ~post_params:(set string "role_types") ();;
let add_role_type_service = post_service ~fallback:design_service ~post_params:(string "role_type") ();;
let cast_service = service ~path:["cast"] ~get_params:(suffix (int32 "game_id")) ();;
let do_cast_service = post_service ~fallback:cast_service ~post_params:(list "team" (string "name" ** list "member" (string "role" ** int32 "id")) ** bool "publish") ();;

let design_page game_id () = 
	let%lwt u = Eliom_reference.get Maw.user in
	Lwt.catch (fun () -> match u with
	| None -> not_logged_in ()
	| Some (uid, _, _) -> 
		let%lwt (title, date, loc, _, dsg_id, d, min_nr, max_nr, _) =
			Database.get_game_data game_id in
		if uid <> dsg_id then error_page "You are not the designer of this game."
    else
		let%lwt teams = Database.get_game_teams game_id in
		let%lwt role_types = Database.get_game_role_types game_id in
			container (standard_menu ())
			[
				h1 [pcdata title];
				p [pcdata (Printf.sprintf "%s, %s" loc (date_or_tbd date))];
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
								Form.input ~input_type:`Submit ~value:"Update" Form.string
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
							td [Form.input ~input_type:`Submit ~value:"Update" Form.string]
						]
					]
				]) game_id;
				Form.post_form ~service:remove_teams_service (fun team -> [
					table [
						tr [
							td [pcdata "Teams:"]
						];
						tr (
							match teams with
							| [] -> [td [pcdata "No teams have been entered"]]
							| h::t -> 
								[
									td [
										Form.multiple_select ~name:team Form.string
										(Form.Option ([], h, None, false))
										(List.map (fun x -> (Form.Option ([], x, None, false))) t)
									];
									td [
										Form.input ~input_type:`Submit ~value:"Remove" Form.string
									]
								]
						)
					]
				]) game_id;
				Form.post_form ~service:add_team_service (fun team -> [
					table [
						tr [
							td [Form.input ~a:[a_size 50] ~input_type:`Text ~name:team
								Form.string];
							td [Form.input ~input_type:`Submit ~value:"Add" Form.string]
						]
					]
				]) game_id;
				Form.post_form ~service:remove_role_types_service (fun role_type -> [
					table [
						tr [
							td [pcdata "Role types:"]
						];
						tr (match role_types with
						| [] -> [td [pcdata "No role types have been entered"]]
						| h::t -> 
							[
								td [
									Form.multiple_select ~name:role_type Form.string
									(Form.Option ([], h, None, false))
									(List.map (fun x -> (Form.Option ([], x, None, false))) t)
								];
								td [
									Form.input ~input_type:`Submit ~value:"Remove" Form.string
								]
							]
						)
					]
				]) game_id;
				Form.post_form ~service:add_role_type_service (fun role_type -> [
					table [
						tr [
							td [Form.input ~a:[a_size 50] ~input_type:`Text ~name:role_type
								Form.string];
							td [Form.input ~input_type:`Submit ~value:"Add" Form.string]
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
	| Some (uid, _, _) -> Database.set_game_description game_id descr
;;

let update_numbers game_id (min, max) =
	let%lwt u = Eliom_reference.get Maw.user in
	match u with
	| None -> Lwt.return ()
	| Some (uid, _, _) -> Database.set_game_numbers game_id min max
;;

let remove_teams game_id teams =
	let%lwt u = Eliom_reference.get Maw.user in
	match u with
	| None -> Lwt.return ()
	| Some (uid, _, _) -> Database.remove_game_teams game_id teams
;;

let add_team game_id team =
	let%lwt u = Eliom_reference.get Maw.user in
	match u with
	| None -> Lwt.return ()
	| Some (uid, _, _) -> Database.add_game_team game_id team
;;

let remove_role_types game_id role_types =
	let%lwt u = Eliom_reference.get Maw.user in
	match u with
	| None -> Lwt.return ()
	| Some (uid, _, _) -> Database.remove_game_role_types game_id role_types
;;

let add_role_type game_id role_type =
	Lwt_log.ign_info_f "ADD_GROUP %ld %s" game_id role_type;
	let%lwt u = Eliom_reference.get Maw.user in
	match u with
	| None -> Lwt.return ()
	| Some (uid, _, _) -> Database.add_game_role_type game_id role_type
;;

let%client switch_active ev =
 	Js.Opt.iter (ev##.target) (fun e ->
		if Js.to_bool (e##.classList##contains (Js.string "active"))
		then e##.classList##remove (Js.string "active")
		else e##.classList##add (Js.string "active") 
	)
;;

let%client new_input () =
  To_dom.of_element (td [
		Raw.input ~a:[a_input_type `Text; a_value ""] ()
	]);;

let%client do_move ev =
	let it = Dom_html.getElementById "inscr_table" in
	let td = Dom_html.getElementById "teams" in
	Js.Opt.iter (ev##.target) (fun gt_th ->
		Js.Opt.iter (gt_th##.parentNode) (fun gt_tr ->
			Js.Opt.iter (gt_tr##.parentNode) (fun dst ->
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
				) (Dom.list_of_nodeList it##.childNodes);
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

let cast_page game_id () =
  let%lwt u = Eliom_reference.get Maw.user in
  match u with
  | None -> not_logged_in ()
  | Some (uid, _, _) -> let%lwt (title, _, _, _, dsg_id, _, _, _, _) =
      Database.get_game_data game_id in
    if uid <> dsg_id then error_page "You are not the designer of this game."
    else
    let%lwt inscr = Database.get_inscription_list ~filter_cast:true game_id in
		let%lwt casting = Database.get_casting game_id in
    let%lwt teams = Database.get_game_teams game_id in
		let%lwt pub = Database.is_published game_id in
    container (standard_menu ())
    [
      Form.post_form ~service:do_cast_service (fun (team, publish) ->
      [
			  div ~a:[a_id "players"]
			  [
			  	h2 [pcdata "Players"];
        	table ~a:[a_class ["casting"]; a_id "inscr_table"]
        	(List.map (fun (nm, p_id, _, _, n, g, _) ->
        		tr ~a:[a_class ["player_row"]] [
        	  	td ~a:[
          	   	a_class (match g with None -> [] | Some g -> [(Printf.sprintf "group%ld" (Int32.rem g 7l))]);
          	   	a_onclick [%client switch_active];
							  a_title n
            	] [
								Raw.input ~a:[a_input_type `Hidden; a_value (Int32.to_string p_id)] (); 	
								pcdata nm
							]
            ]
          ) inscr)
			  ];
        div ~a:[a_id "teams"] (
			  	h2 [pcdata "Teams"]::
					team.it (fun (t_name, member) t init ->
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
						  member.it (fun (p_role, p_uid) (_, rn, pn, pid, n, g) init ->
						 		tr ~a:[a_class ["player_row"]] [
									td [
										Form.input ~input_type:`Text ~name:p_role ~value:rn Form.string
									];
									td ~a:[a_class (match g with None -> [] | Some g -> [(Printf.sprintf "group%ld" (Int32.rem g 7l))]);
          	   			a_onclick [%client switch_active];
							  		a_title n]
									[
										Form.input ~input_type:`Hidden ~name:p_uid ~value:pid Form.int32;
										pcdata pn
									]
								]::init
						  ) (List.filter (fun (n, _, _, _, _, _) -> n = t) casting)
							[]
						)::init
      	  ) teams
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
;;

let do_cast_page game_id (teams, publish) =
	let%lwt u = Eliom_reference.get Maw.user in
	(match u with
	| None -> Lwt.return ()
	| Some (uid, _, _) -> 
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

let () =
	Maw_app.register ~service:design_service design_page;;
	Eliom_registration.Action.register ~service:update_descr_service
		update_description;
	Eliom_registration.Action.register ~service:update_numbers_service
		update_numbers;
	Eliom_registration.Action.register ~service:remove_teams_service
		remove_teams;
	Eliom_registration.Action.register ~service:add_team_service
		add_team;
	Eliom_registration.Action.register ~service:remove_role_types_service
		remove_role_types;
	Eliom_registration.Action.register ~service:add_role_type_service
		add_role_type;
  Maw_app.register ~service:cast_service cast_page;
	Maw_app.register ~service:do_cast_service do_cast_page
;;

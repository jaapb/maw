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

let%client selected_player: int32 option ref = ref None;;

let%client switch_tds source target =
	(* First, go up from the clicked thing. *)
	Js.Opt.iter (source##.parentNode) (fun s_tr ->
		Js.Opt.iter (target##.parentNode) (fun t_tr ->
			Dom.replaceChild t_tr source target;
			Dom.replaceChild s_tr target source
		)
	)
;;

let%client handle_player_click p_id ev =
 	Js.Opt.iter (ev##.target) (fun e ->
		match p_id, !selected_player with
		| None, None -> (* clicked on empty field with no player selected *)
				()
		| None, Some s -> (* clicked on empty field with player selected *)
			let old_td = Dom_html.getElementById (Printf.sprintf "player_%ld" s) in
			begin
				old_td##.classList##remove (Js.string "active");
				selected_player := None;
				switch_tds old_td e
			end
		| Some p, None ->
			begin
				e##.classList##add (Js.string "active");
				selected_player := Some p
			end
		| Some p, Some s ->
			let old_td = Dom_html.getElementById (Printf.sprintf "player_%ld" s) in
			if old_td = e then begin
				e##.classList##remove (Js.string "active");
				selected_player := None;
			end
			else begin
				old_td##.classList##remove (Js.string "active");
				e##.classList##add (Js.string "active");
				selected_player := Some p
			end
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

let%client show_history service uid ev =
	let window = Eliom_client.window_open ~window_name:(Js.string "Player")
		~service uid in
		Dom.preventDefault ev
;;

let%client cast_init ev =
	selected_player := None;
;;

let do_cast_page game_id (teams, publish) =
	let%lwt u = Eliom_reference.get Maw.user in
	(match u with
	| None -> Lwt.return ()
	| Some (uid, _, _, _) -> 
		Database.update_casting game_id teams) >>=
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

let cast_page game_id () =
	let do_cast_service = create ~path:(Path ["cast"]) ~meth:(Post (suffix (int32 "game_id"), list "team" (string "name" ** list "member" (string "role" ** int32 "id")) ** bool "publish")) () in
	Maw_app.register ~scope:Eliom_common.default_session_scope
		~service:do_cast_service do_cast_page;
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
    container ~onload:[%client cast_init] (standard_menu ())
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
									a_id (Printf.sprintf "player_%ld" p_id);
          	   		a_onclick [%client (handle_player_click ~%(Some p_id))];
									a_oncontextmenu [%client (show_history ~%(user_history_service) ~%p_id)];
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
									td ~a:(
							  		a_title (default "" n)::
          	   			a_onclick [%client (handle_player_click ~%pid)]::
										(match pid with
										| None -> []
										| Some x -> [
												a_oncontextmenu [%client (show_history ~%(user_history_service) ~%x)];
												a_id (Printf.sprintf "player_%ld" x)
											]
										)
									)
									(match pid with
									| None -> [ pcdata "" ];
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

let () =
  Maw_app.register ~service:cast_service cast_page
;;

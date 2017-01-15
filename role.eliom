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
	let table = Dom_html.getElementById "role_table" in
	renumber_children 0 (Dom.list_of_nodeList table##.childNodes)
	)
;;

let do_role_page game_id () teams =
	Database.update_teams game_id teams >>=
	fun () -> container (standard_menu [])
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
			container (standard_menu [])
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

let () =
  Maw_app.register ~service:role_service role_page
;;

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
	open Database
]

let game_service = service ~path:["game"] ~get_params:(suffix (int32 "game_id")) ();;
let signup_service = service ~path:["signup"] ~get_params:(suffix (int32 "game_id")) ();;
let do_signup_service = post_service ~fallback:signup_service ~post_params:(
	bool "edit" ** bool "is_group" ** string "team" **
	list "person" (sum (string "search") (int32 "uid") ** string "role_type" ** string "note")
) ();;
let show_inscriptions_service = service ~path:["inscriptions"] ~get_params:(suffix (int32 "game_id")) ();;
(*let add_friend_service = post_service ~fallback:signup_service ~post_params:(string "identifier") ();;*)

let game_page game_id () =
  let standard_game_data title loc date dsg_name d full =
		h1 [pcdata title]::
		p [pcdata (Printf.sprintf "%s, %s" loc (date_or_tbd date))]::
		p [i [pcdata (Printf.sprintf "Designed by %s" dsg_name)]]::
		p [pcdata d]::
    (if full
    then [p [i [pcdata "This game has reached its maximum number of inscriptions. You can still sign up, but you will be placed on a waiting list."]]]
    else []) in
	let%lwt u = Eliom_reference.get Maw.user in
	Lwt.catch (fun () -> let%lwt (title, date, loc, dsg_name, dsg, d, _, max_pl) =
		Database.get_game_data game_id in
    let%lwt nr_inscr = Database.get_nr_inscriptions game_id in
    match u with
	  | None -> container (standard_menu ()) 
			(standard_game_data title loc date dsg_name d (nr_inscr >= max_pl))
	  | Some (uid, _) ->
			let%lwt (signed_up, l) = Database.get_inscription_data uid game_id in
			container (standard_menu ()) 
			(standard_game_data title loc date dsg_name d (nr_inscr >= max_pl) @
		  	if uid = dsg then
				[
					p [a ~service:Design.design_service [pcdata "Edit the game design"] game_id];
					p [a ~service:show_inscriptions_service [pcdata "Show inscriptions for this game"] game_id]
				]
				else if signed_up then
				[
					p [
						i [pcdata "You are signed up for this game. "] (*;
						pcdata (Printf.sprintf "Your team preference is %s and your role preference is %s." (default "Any" g) (default "Any" r))*)
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

let%client remove_my_row ev =
  (* button -> in td -> in tr: delete this tr! *)
  Js.Opt.iter (ev##.target) (fun e ->
    Js.Opt.iter (e##.parentNode) (fun p -> (* this is the td *)
      Js.Opt.iter (p##.parentNode) (fun gp -> (* this is the tr *)
        Js.Opt.iter (gp##.parentNode) (fun ggp -> (* this is the table *)
          Dom.removeChild ggp gp
        )
      )
    )
  )
;; 

let%client new_row id teams =
  tr ~a:[a_class ["group_inscription_row"]] [ 
    td [Raw.input ~a:[a_name (Printf.sprintf "person.search[%d]" id); a_input_type `Text; a_value ""] ()];
    td [];
    td [Raw.select ~a:[a_name (Printf.sprintf "person.role_type[%d]" id)] (option (pcdata "Any")::List.map (fun x -> (option (pcdata x))) teams)];
    td [Raw.input ~a:[a_name (Printf.sprintf "person.note[%d]" id); a_input_type `Text; a_value ""] ()];
    td [Raw.input ~a:[a_input_type `Button; a_value "Remove"; a_onclick remove_my_row] ()]
  ]
;;

let%client nr_ids = ref 0

let%client add_inscription_row it teams =
	let br = Dom_html.getElementById "button_row" in
  begin
    incr nr_ids;
    Dom.insertBefore it (To_dom.of_element (new_row !nr_ids teams)) (Js.some br);
  end
;;

let%client new_button it teams =
  Raw.input ~a:[a_id "add_button"; a_input_type `Button; a_value "Add group member"; a_onclick (fun ev -> add_inscription_row it teams)] ()
;;

let%client group_inscription_handler teams ev =
	Js.Opt.iter (ev##.target) (fun x ->
		Js.Opt.iter (Dom_html.CoerceTo.input x) (fun i ->
			let it = Dom_html.getElementById "inscription_table" in
	    let bf = Dom_html.getElementById "button_field" in
      let sb = Dom_html.getElementById "submit_button" in
			if Js.to_bool i##.checked then
      begin
        add_inscription_row it teams;
        Dom.insertBefore bf (To_dom.of_element (new_button it teams)) (Js.some sb)
      end
      else
      begin
        nr_ids := 0;
        List.iter (fun x ->
          Js.Opt.iter (Dom_html.CoerceTo.element x) (fun e ->
            if (Js.to_string e##.className) = "group_inscription_row" ||
              (Js.to_string e##.className) = "add_button" then
              Dom.removeChild it e
          )
        ) (Dom.list_of_nodeList it##.childNodes)
      end
		)
	)
;;

let signup_page game_id () =
	let%lwt u = Eliom_reference.get Maw.user in
	Lwt.catch (fun () -> match u with
	| None -> not_logged_in ()
	| Some (uid, uname) -> 
		let%lwt (title, date, loc, dsg_name, dsg_id, d, _, _)  =
			Database.get_game_data game_id in
    let%lwt teams = Database.get_game_teams game_id in
		let%lwt role_types = Database.get_game_role_types game_id in
		let%lwt (signed_up, inscr) = Database.get_inscription_data uid game_id in
		let me_inscr = if List.exists (fun (u, _, _, _, _, _) -> u = uid) inscr
			then inscr
			else (uid, uname, None, None, "", None)::inscr in
		container (standard_menu ())
		[
			h1 [pcdata title];
			p [pcdata (Printf.sprintf "%s, %s" loc (date_or_tbd date))];
		  p [i [pcdata (Printf.sprintf "Designed by %s" dsg_name)]];
			p [pcdata d];
			h2 [pcdata (if signed_up then "Edit inscription" else "Sign up")];
			Form.post_form ~service:do_signup_service
			(fun (edit, (is_group, (team, person))) ->
      [
				table ~a:[a_id "inscription_table"] (
					tr [
						th [pcdata "Name"];
						th [pcdata "Team preference"];
						th [pcdata "Role type preference"];
						th [pcdata "Note"];
            th [];
					]::
					person.it (fun ((search, uid), (role_type, note)) v init ->
						let (ex_uid, ex_name, t, r, ex_note, _) = v in
						let ex_team = default "Any" t in
						let ex_role = default "Any" r in
						tr [
							td [
								Form.input ~input_type:`Hidden ~name:uid ~value:ex_uid Form.int32;
								pcdata ex_name
							]; 
							td [
								Form.select ~name:team Form.string
								(Form.Option ([], "Any", None, ex_team = "Any"))
								(List.map (fun t ->
									Form.Option ([], t, None, ex_team = t)
								) teams)
							];
							td [
								Form.select ~name:role_type Form.string
								(Form.Option ([], "Any", None, ex_role = "Any"))
								(List.map (fun r ->
									Form.Option ([], r, None, ex_role = r)
								) role_types)
							];
							td [Form.input ~input_type:`Text ~name:note ~value:ex_note Form.string];
              td []
						]::
						init
					) me_inscr
					[
        		tr [
							td ~a:[a_colspan 5] [
       	        Form.bool_checkbox_one ~name:is_group ~checked:(List.length inscr > 1) ~a:[a_onclick [%client (group_inscription_handler ~%teams)]] (); 
          			pcdata "This is a group inscription"
							]
						];
						tr ~a:[a_id "button_row"] [
							td ~a:[a_id "button_field"; a_colspan 5] (
								Form.input ~a:[a_id "submit_button"] ~input_type:`Submit ~value:(if signed_up then "Save changes" else "Sign up") Form.string::
								if signed_up
								then [Form.input ~input_type:`Hidden ~name:edit ~value:true Form.bool]
								else []
							)
						]	
					]
				)
			]) game_id
		]
	)
	(function 
	| Not_found -> unknown_game ()
	| e -> error_page (Printexc.to_string e)
	)
;;

let do_signup_page game_id (edit, (is_group, (team, users))) =
	let rec handle_inscriptions uid edit users =
		match users with
		| (Inj1 search, (r, n))::t -> (* new user *)
        Database.search_for_user search >>=
        (function
        | [] -> Lwt.return (Ocsigen_messages.console (fun () -> Printf.sprintf "MAIL %s" search))
        | [guest_id] ->  Database.add_user game_id guest_id
            (if String.lowercase team = "any" then None else Some team)
			      (if String.lowercase r = "any" then None else Some r) n 
        | _ -> Lwt.return (Ocsigen_messages.console (fun () -> "Eh?"))) >>=
        fun () -> handle_inscriptions uid edit t
		| (Inj2 uid, (r, n))::t -> (* new user *)
      (if edit then
        Database.edit_inscription uid game_id 
  			  (if String.lowercase team = "any" then None else Some team)
				  (if String.lowercase r = "any" then None else Some r)
				  n
      else
        Database.signup game_id uid
  			  (if String.lowercase team = "any" then None else Some team)
				  (if String.lowercase r = "any" then None else Some r)
				  n) >>=
			  fun () -> handle_inscriptions uid edit t
		| _ -> Lwt.return ()
	in
	let%lwt u = Eliom_reference.get Maw.user in
	Lwt.catch (fun () -> match u with
	| None -> not_logged_in ()
	| Some (uid, _) -> handle_inscriptions uid edit users >>=
		fun () -> container (standard_menu ())
		[
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

let show_inscriptions_page game_id () =
	let%lwt u = Eliom_reference.get Maw.user in
	Lwt.catch (fun () -> match u with
	| None -> not_logged_in ()
	| Some (uid, _) -> let%lwt (title, date, loc, _, dsg_id, d, min_nr, max_nr) =
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
							th [pcdata "Name"];
							th [pcdata "Team"];
							th [pcdata "Role"];
							th [pcdata "Note"]
						]::
						List.map (fun (nm, g, r, nt, _) ->
							tr [
								td [pcdata nm];
								td [pcdata (default "Any" g)];
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

let _ =
	Maw_app.register ~service:game_service game_page;
	Maw_app.register ~service:signup_service signup_page;
	Maw_app.register ~service:do_signup_service do_signup_page;
	Maw_app.register ~service:show_inscriptions_service show_inscriptions_page
;;

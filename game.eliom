[%%shared
	open Eliom_lib
	open Eliom_content
	open Html5.D
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
	bool "edit" ** bool "is_group" **
	list "person" (sum (string "search") (int32 "uid") ** string "group" ** string "role_type" ** string "note")
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
						pcdata (Printf.sprintf "Your group preference is %s and your role preference is %s." (default "Any" g) (default "Any" r))*)
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

let signup_page game_id () =
	let%lwt u = Eliom_reference.get Maw.user in
	Lwt.catch (fun () -> match u with
	| None -> not_logged_in ()
	| Some (uid, uname) -> 
		let%lwt (title, date, loc, dsg_name, dsg_id, d, _, _)  =
			Database.get_game_data game_id in
    let%lwt groups = Database.get_game_groups game_id in
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
			(fun (edit, (is_group, person)) -> [
				table (
					tr [
						th [pcdata "Name"];
						th [pcdata "Group preference"];
						th [pcdata "Role type preference"];
						th [pcdata "Note"];
					]::
					person.it (fun ((search, uid), (group, (role_type, note))) v init ->
						let (ex_uid, ex_name, g, r, ex_note, _) = v in
						let ex_group = default "Any" g in
						let ex_role = default "Any" r in
						tr [
							td [
								Form.input ~input_type:`Hidden ~name:uid ~value:ex_uid Form.int32;
								pcdata ex_name
							]; 
							td [
								Form.select ~name:group Form.string
								(Form.Option ([], "Any", None, ex_group = "Any"))
								(List.map (fun g ->
									Form.Option ([], g, None, ex_group = g)
								) groups)
							];
							td [
								Form.select ~name:role_type Form.string
								(Form.Option ([], "Any", None, ex_role = "Any"))
								(List.map (fun r ->
									Form.Option ([], r, None, ex_role = r)
								) role_types)
							];
							td [Form.input ~input_type:`Text ~name:note ~value:ex_note Form.string]
						]::
						init
					) me_inscr
					[
        		tr [
							td ~a:[a_colspan 4] [
          			Form.bool_checkbox_one ~name:is_group ~checked:(List.length inscr > 1) ();
          			pcdata "This is a group inscription"
							]
						];
						tr [
							td ~a:[a_colspan 4] (
								Form.input ~input_type:`Submit ~value:(if signed_up then "Save changes" else "Sign up") Form.string::
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

			(*if edit then
				Database.edit_inscription game_id uid
					(if String.lowercase group = "any" then None else Some group)
					(if String.lowercase role_type = "any" then None else Some role_type)
					note
			else
				Database.signup game_id uid
					(if String.lowercase group = "any" then None else Some group)
					(if String.lowercase role_type = "any" then None else Some role_type)
					note*)

let do_signup_page game_id (edit, (is_group, users)) =
	let rec handle_inscriptions uid edit users =
		match users with
		| (Inj1 s, (g, (r, n)))::t -> (* new user *)
        Database.add_user game_id s
         (if String.lowercase g = "any" then None else Some g)
			   (if String.lowercase r = "any" then None else Some r)
			   n >>=
        fun () -> handle_inscriptions uid edit t
		| (Inj2 s, (g, (r, n)))::t -> (* new user *)
      (if edit then
        Database.edit_inscription game_id s
  			  (if String.lowercase g = "any" then None else Some g)
				  (if String.lowercase r = "any" then None else Some r)
				  n
      else
        Database.signup game_id s
  			  (if String.lowercase g = "any" then None else Some g)
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
							th [pcdata "Group"];
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

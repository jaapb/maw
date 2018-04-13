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
	open Maw_db
]

let%client handle_search users ev =
begin
	Js.Opt.iter (ev##.target) (fun x ->
		Js.Opt.iter (Dom_html.CoerceTo.input x) (fun i ->
			let rex = Regexp.regexp_string_case_fold (Js.to_string i##.value) in
			let tbl = Dom_html.getElementById "users_table" in
			List.iter (fun tbody ->
				List.iter (fun c ->
					Js.Opt.iter (Dom_html.CoerceTo.element c) (fun tr ->
						Scanf.sscanf (Js.to_string tr##.id) "uid_%ld" (fun uid ->
							Js.Opt.iter (tr##.childNodes##item 1) (fun td ->
								Js.Opt.iter (td##.childNodes##item 0) (fun c ->
									Js.Opt.iter (Dom.CoerceTo.text c) (fun e ->
										match Regexp.search rex (Js.to_string e##.data) 0 with
										| None -> tr##.style##.display := Js.string "none"
										| Some _ -> tr##.style##.display := Js.string "table-row"
									)
								)
							)
						)
					)
				) (Dom.list_of_nodeList tbody##.childNodes)
			) (Dom.list_of_nodeList tbl##.childNodes)
		)
	)
end;;

let%client handle_select id first_name last_name uid ev =
begin
	Eliom_lib.alert "Setting gir_%d to %s %s and %ld" id first_name last_name uid;
	Js.Opt.iter (Dom_html.window##.parent##.document##getElementById (Js.string (Printf.sprintf "gir_%d" id))) (fun gniarq ->
		Eliom_lib.alert "Found element";
		Js.Opt.iter (gniarq##.childNodes##item 1) (fun e ->
			Js.Opt.iter (e##.childNodes##item 0) (fun n ->
				Js.Opt.iter (Dom_html.CoerceTo.element n) (fun e ->
					Js.Opt.iter (Dom_html.CoerceTo.input e) (fun inp_uid -> 
						inp_uid##.value := Js.string (Printf.sprintf "%ld" uid)
					)
				)
			);
			Js.Opt.iter (e##.childNodes##item 1) (fun n ->
				Js.Opt.iter (Dom_html.CoerceTo.element n) (fun e ->
					Js.Opt.iter (Dom_html.CoerceTo.input e) (fun inp_search -> 
						inp_search##.value := Js.string (Printf.sprintf "%s %s" first_name last_name)
					)
				)
			)
		)
	)
end;;

(*let find_user_page id () =
	let%lwt users = Maw_db.get_users () in
	Lwt.return (Eliom_tools.F.html
		~title:"Find a user"
		~css:[["css";"maw.css"]]
		(body [
			h1 [pcdata "Find user"];
			p [
				pcdata "Search: ";
				Raw.input ~a:[a_input_type `Text; a_oninput [%client (handle_search ~%users)]] ()
			];
			table ~a:[a_id "users_table"]
			(List.map (fun (uid, first_name, last_name, email, _) ->
				tr ~a:[a_id (Printf.sprintf "uid_%ld" uid)] [
					td [Raw.input ~a:[a_input_type `Button; a_value "Select"; a_onclick [%client handle_select ~%id ~%first_name ~%last_name ~%uid]] ()];
					td [pcdata (Printf.sprintf "%s %s" first_name last_name)]
				]
			) users)
		])
	);;*)

let user_history_page uid () =
	let%lwt (fname, lname, _, _, _) = Maw_db.get_user_data uid in
	let%lwt history = Maw_db.get_user_history uid in
	Lwt.return (Eliom_tools.F.html ~title:"Player"
	(body
		[
			h1 [pcdata (Printf.sprintf "History for %s %s" fname lname)];
			table (
				tr [
					th [pcdata "Date"];
					th [pcdata "Game title"];
					th [pcdata "Team"];
					th [pcdata "Role"];
					th [pcdata "Status"]
				]::
				(List.map (fun (title, date, tname, rname, status, canc) ->
					tr [
						td [match date with
							| None -> pcdata "Unknown"
							| Some d -> pcdata (Printer.Date.sprint "%d %B %Y" d)
						];
						td [pcdata title];
						td [pcdata tname];
						td [pcdata rname];
						td [if canc then pcdata "Cancelled"
							else match (inscr_status_of_int32 status) with
							| `No_show -> b [pcdata "No-show"]
							| `Confirmed | `Paid -> pcdata "Participated"
							| _ -> pcdata "Did not participate" 
						]
					]
				) history)
			)
		]
	))
;;

let _ =
	Maw_app.register ~service:user_history_service user_history_page
;;

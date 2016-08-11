[%%shared
	open Eliom_lib
	open Eliom_content
	open Html.D
	open Eliom_service
	open Eliom_parameter
]

[%%server
	open CalendarLib
	open Maw
	open Database
]

let add_user_service = create ~id:(Path ["register"])
	~meth:(Post (unit, string "name" ** string "username" ** string "email" ** string "password")) ();;
let update_user_service = create ~id:(Fallback account_service)
	~meth:(Post (unit, string "email" ** string "password")) ();;
let confirm_user_service = create ~id:(Path ["confirm"]) ~meth:(Get (suffix (int32 "user_id" ** string "random"))) ();;
let confirm_provisional_user_service = create ~id:(Path ["confirm_provisional"]) ~meth:(Get (suffix (int32 "user_id"))) ();;
let find_user_service = Eliom_service.create ~id:Global ~meth:(Get (int "row_id")) ();;

let update_user_page () (email, password) =
	Lwt.catch (fun () ->
		let%lwt u = Eliom_reference.get Maw.user in
		match u with
		| None -> not_logged_in ()
		| Some (uid, _, _) -> Database.update_user_data uid email password >>=
		fun () -> container (standard_menu ())
		[
			p [pcdata "Changes successfully saved."]
		]
	)
	(function
	| e -> error_page (Printexc.to_string e)
	)
;;

let%client check_account_form ev =
	let add_or_replace text = 
	let p = Dom_html.getElementById "error_paragraph" in
	let l = Dom.list_of_nodeList p##.childNodes in
	let t = Dom_html.document##createTextNode text in
	begin
		match l with
		| [] -> Dom.appendChild p t
		| [c] -> Dom.replaceChild p t c
		| _ -> ()
	end in
	let p1 = Dom_html.getElementById "password_input1" in
	let p2 = Dom_html.getElementById "password_input2" in
	let ei = Dom_html.getElementById "email_input" in
	Js.Opt.iter (Dom_html.CoerceTo.input ei) (fun e ->
		if Js.to_string e##.value = "" then
		begin
			add_or_replace (Js.string "You might want to put in an e-mail addres...");
			Dom.preventDefault ev
		end
	);
	Js.Opt.iter (Dom_html.CoerceTo.input p1) (fun e1 ->
		Js.Opt.iter (Dom_html.CoerceTo.input p2) (fun e2 ->
			if e1##.value <> e2##.value then
			begin
				add_or_replace (Js.string "Passwords don't match.");
				Dom.preventDefault ev
			end
			else if Js.to_string e1##.value = "" then
			begin
				add_or_replace (Js.string "You might want to put in a password...");
				Dom.preventDefault ev
			end
		)
	)
;;

let account_page () () =
	Maw_app.register ~scope:Eliom_common.default_session_scope
		~service:update_user_service update_user_page;
	Lwt.catch (fun () ->
		let%lwt u = Eliom_reference.get Maw.user in
		match u with
		| None -> not_logged_in ()
		| Some (uid, _, _) -> 
			let%lwt (name, ex_email) = Database.get_user_data uid in
			container (standard_menu ())
			[
				h1 [pcdata "Your account"];
				p ~a:[a_class ["error"]; a_id "error_paragraph"] [];
				Form.post_form ~service:update_user_service (fun (email, password) -> 
				[
					table [
						tr [
							th [pcdata "Name"];
							td [pcdata name]
						];
						tr [
							th [pcdata "E-mail address"];
							td [Form.input ~a:[a_id "email_input"] ~input_type:`Text ~name:email ~value:ex_email Form.string] 
						];
						tr [
							th [pcdata "Password"];
							td [Form.input ~a:[a_id "password_input1"] ~input_type:`Password ~name:password Form.string]
						];
						tr [
							th [pcdata "Confirm password"];
							td [Raw.input ~a:[a_input_type `Password; a_id "password_input2"] ()]
						];
						tr 
						[
						 	td ~a:[a_colspan 2]
								[Form.input ~a:[a_onclick [%client check_account_form]] ~input_type:`Submit ~value:"Save changes" Form.string]
						]
					]
				]) ()
			]
	)
	(function
	| Not_found -> error_page "Unknown user"
	| e -> error_page (Printexc.to_string e)
	)
;;

let%client check_register_form ev =
	let add_or_replace text = 
	let p = Dom_html.getElementById "error_paragraph" in
	let l = Dom.list_of_nodeList p##.childNodes in
	let t = Dom_html.document##createTextNode text in
	begin
		match l with
		| [] -> Dom.appendChild p t
		| [c] -> Dom.replaceChild p t c
		| _ -> ()
	end in
	let ui = Dom_html.getElementById "username_input" in
	let p1 = Dom_html.getElementById "password_input1" in
	let p2 = Dom_html.getElementById "password_input2" in
	let ni = Dom_html.getElementById "name_input" in
	let ei = Dom_html.getElementById "email_input" in
	Js.Opt.iter (Dom_html.CoerceTo.input ei) (fun e ->
		if Js.to_string e##.value = "" then
		begin
			add_or_replace (Js.string "You might want to put in an e-mail addres...");
			Dom.preventDefault ev
		end
	);
	Js.Opt.iter (Dom_html.CoerceTo.input ni) (fun e ->
		if Js.to_string e##.value = "" then
		begin
			add_or_replace (Js.string "You might want to put in a full name...");
			Dom.preventDefault ev
		end
	);
	Js.Opt.iter (Dom_html.CoerceTo.input p1) (fun e1 ->
		Js.Opt.iter (Dom_html.CoerceTo.input p2) (fun e2 ->
			if e1##.value <> e2##.value then
			begin
				add_or_replace (Js.string "Passwords don't match.");
				Dom.preventDefault ev
			end
			else if Js.to_string e1##.value = "" then
			begin
				add_or_replace (Js.string "You might want to put in a password...");
				Dom.preventDefault ev
			end
		)
	);
	Js.Opt.iter (Dom_html.CoerceTo.input ui) (fun e ->
		if Js.to_string e##.value = "" then
		begin
			add_or_replace (Js.string "You might want to put in a username...");
			Dom.preventDefault ev
		end
	)
;;

let register_page () () =
	Lwt.catch (fun () ->
		container (standard_menu ())
		[
			h1 [pcdata "Create a new account"];
			p ~a:[a_class ["error"]; a_id "error_paragraph"] [];
			Form.post_form ~service:add_user_service
			(fun (name, (username, (email, password))) -> [
				table [
					tr [
						th [pcdata "Username:"];
						td [Form.input ~a:[a_id "username_input"] ~input_type:`Text ~name:username Form.string]
					];
					tr [
						th [pcdata "Password:"];
						td [Form.input ~a:[a_id "password_input1"] ~input_type:`Password ~name:password Form.string]
					];
					tr [
						th [pcdata "Confirm password:"];
						td [Raw.input ~a:[a_id "password_input2"; a_input_type `Password] ()]
					];
					tr [
						th [pcdata "Full name:"];
						td [Form.input ~a:[a_id "name_input"] ~input_type:`Text ~name:name Form.string] 
					];
					tr [
						th [pcdata "E-mail address:"];
						td [Form.input ~a:[a_id "email_input"] ~input_type:`Text ~name:email Form.string]
					];
					tr [
						td ~a:[a_colspan 2] [Form.input ~a:[a_onclick [%client check_register_form]]
						~input_type:`Submit ~value:"Sign up" Form.string]
					]
				]
			]) ()
		]
	)
	(function
	| e -> error_page (Printexc.to_string e)
	)

let add_user_page () (name, (username, (email, password))) =
	Lwt.catch (fun () -> Database.add_user name username email password >>=
	fun (uid, random) -> begin
	match random with
		| None -> Lwt.fail_with "Did not generate confirmation code"
		| Some x -> Lwt.return x
	end >>=
	fun rstr -> let uri = Eliom_uri.make_string_uri ~absolute:true ~service:confirm_user_service (uid, rstr) in
	Mail.send_register_mail name email uri;
	container (standard_menu ())
	[
		h1 [pcdata "Account created"];
		p [pcdata "Please reply to the confirmation mail."]
	])
	(function
	| e -> error_page (Printexc.to_string e)
	)
;;

let confirm_user_page (user_id, random) () =
	Lwt.catch (fun () -> Database.confirm_user user_id random >>=
	fun res -> container (standard_menu ())
	[
		h1 [pcdata "Account activated"];
		p [pcdata "You can now login normally."]
	])
	(function
	| e -> error_page (Printexc.to_string e)
	)
;;

let update_provisional_user_page user_id () (name, (username, (old_email, (email, password)))) =
	Lwt.catch (fun () ->	Database.add_user ~id:user_id ~confirm:(old_email <> email) name username email password >>=
	fun (_, c_random) -> container (standard_menu ())
	[
		h1 [pcdata "Placeholder"]
	])
	(function
	| e -> error_page (Printexc.to_string e)
	)
;;

let confirm_provisional_user_page (user_id) () =
let update_provisional_user_service = create
	~id:(Fallback (preapply confirm_provisional_user_service user_id))
	~meth:(Post (unit, string "name" ** string "username" ** string "old_email" ** string "email" ** string "password")) () in
	Maw_app.register ~scope:Eliom_common.default_session_scope ~service:update_provisional_user_service (update_provisional_user_page user_id);
	Lwt.catch (fun () -> Database.get_provisional_user_data user_id >>=
	fun ex_email -> container (standard_menu ())
	[
		h1 [pcdata "User data"];
		Form.post_form ~service:update_provisional_user_service
		(fun (name, (username, (old_email, (email, password)))) -> [
			table [
				tr [
					th [pcdata "Username:"];
					td [Form.input ~a:[a_id "username_input"] ~input_type:`Text ~name:username Form.string]
				];
				tr [
					th [pcdata "Password:"];
					td [Form.input ~a:[a_id "password_input1"] ~input_type:`Password ~name:password Form.string]
				];
				tr [
					th [pcdata "Confirm password:"];
					td [Raw.input ~a:[a_id "password_input2"; a_input_type `Password] ()]
				];
				tr [
					th [pcdata "Full name:"];
					td [Form.input ~a:[a_id "name_input"] ~input_type:`Text ~name:name Form.string] 
				];
				tr [
					th [pcdata "E-mail address:"];
					td [Form.input ~a:[a_id "email_input"] ~input_type:`Text ~name:email ~value:ex_email Form.string;
					Form.input ~input_type:`Hidden ~name:old_email ~value:ex_email Form.string]
				];
				tr [
					td ~a:[a_colspan 2] [Form.input ~a:[a_onclick [%client check_register_form]]
					~input_type:`Submit ~value:"Sign up" Form.string]
				]
			]
		]) ()			
	]) 
	(function
	| e -> error_page (Printexc.to_string e)
	)
;;

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

let%client handle_select id name uid ev =
begin
	Eliom_lib.alert "Setting gir_%d to %s and %ld" id name uid;
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
						inp_search##.value := Js.string name
					)
				)
			)
		)
	)
end;;

let find_user_page id () =
	let%lwt users = Database.get_confirmed_users () in
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
			(List.map (fun (uid, name, email) ->
				tr ~a:[a_id (Printf.sprintf "uid_%ld" uid)] [
					td [Raw.input ~a:[a_input_type `Button; a_value "Select"; a_onclick [%client handle_select ~%id ~%name ~%uid]] ()];
					td [pcdata name]
				]
			) users)
		])
	);;

let _ =
	Maw_app.register ~service:account_service account_page;
	Maw_app.register ~service:Maw.register_service register_page;
	Maw_app.register ~service:add_user_service add_user_page;
	Maw_app.register ~service:confirm_user_service confirm_user_page;
	Maw_app.register ~service:confirm_provisional_user_service confirm_provisional_user_page;
	Maw_app.register ~service:find_user_service find_user_page
;;

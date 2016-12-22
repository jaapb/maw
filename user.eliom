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
	open Database
]

let add_user_service = create ~path:(Path ["register"])
	~meth:(Post (unit, string "first_name" ** string "last_name" ** string "email" ** string "password" ** string "address" ** string "postcode" ** string "town" ** string "country" ** string "phone")) ();;
let confirm_user_service = create ~path:(Path ["confirm"]) ~meth:(Get (suffix (int32 "user_id" ** string "random"))) ();;
let confirm_provisional_user_service = create ~path:(Path ["confirm_provisional"]) ~meth:(Get (suffix (int32 "user_id"))) ();;
(* let find_user_service = Eliom_service.create ~path:No_path ~meth:(Get (int "row_id")) ();; *)
let user_history_service = create ~path:(Path ["history"]) ~meth:(Get (int32 "user_id")) ();;

let update_user_page () (fname, (lname, (email, (password, (address, (postcode, (town, (country, phone))))))))  =
	Lwt.catch (fun () ->
		let%lwt u = Eliom_reference.get Maw.user in
		match u with
		| None -> not_logged_in ()
		| Some (uid, _, _, _) -> Database.update_user_data uid fname lname email password address postcode town country phone>>=
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
	let fni = Dom_html.getElementById "first_name_input" in
	let lni = Dom_html.getElementById "last_name_input" in
	let ai = Dom_html.getElementById "address_input" in
	let pci = Dom_html.getElementById "postcode_input" in
	let ti = Dom_html.getElementById "town_input" in
	let ci = Dom_html.getElementById "country_input" in
	let phi = Dom_html.getElementById "phone_input" in
	Js.Opt.iter (Dom_html.CoerceTo.input ei) (fun e ->
		if Js.to_string e##.value = "" then
		begin
			add_or_replace (Js.string "Please enter an e-mail address.");
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
				add_or_replace (Js.string "Please enter a password.");
				Dom.preventDefault ev
			end
		)
	);
	Js.Opt.iter (Dom_html.CoerceTo.input fni) (fun e ->
		if Js.to_string e##.value = "" then
		begin
			add_or_replace (Js.string "Please enter a first name.");
			Dom.preventDefault ev
		end
	);
	Js.Opt.iter (Dom_html.CoerceTo.input lni) (fun e ->
		if Js.to_string e##.value = "" then
		begin
			add_or_replace (Js.string "Please enter a last name.");
			Dom.preventDefault ev
		end
	);
	Js.Opt.iter (Dom_html.CoerceTo.input ai) (fun e ->
		if Js.to_string e##.value = "" then
		begin
			add_or_replace (Js.string "Please enter an address.");
			Dom.preventDefault ev
		end
	);
	Js.Opt.iter (Dom_html.CoerceTo.input pci) (fun e ->
		if Js.to_string e##.value = "" then
		begin
			add_or_replace (Js.string "Please enter a postcode.");
			Dom.preventDefault ev
		end
	);
	Js.Opt.iter (Dom_html.CoerceTo.input ti) (fun e ->
		if Js.to_string e##.value = "" then
		begin
			add_or_replace (Js.string "Please enter a town.");
			Dom.preventDefault ev
		end
	);
	Js.Opt.iter (Dom_html.CoerceTo.input ci) (fun e ->
		if Js.to_string e##.value = "" then
		begin
			add_or_replace (Js.string "Please enter a country.");
			Dom.preventDefault ev
		end
	);
	Js.Opt.iter (Dom_html.CoerceTo.input phi) (fun e ->
		if Js.to_string e##.value = "" then
		begin
			add_or_replace (Js.string "Please enter a phone number.");
			Dom.preventDefault ev
		end
	)
;;

let account_page () () =
	let update_user_service = create_attached_post
		~fallback:account_service
		~post_params:(string "first_name" ** string "last_name" ** string "email" ** string "password" ** string "address" ** string "postcode" ** string "town" ** string "country" ** string "phone") () in
	Maw_app.register ~scope:Eliom_common.default_session_scope
		~service:update_user_service update_user_page;
	Lwt.catch (fun () ->
		let%lwt u = Eliom_reference.get Maw.user in
		match u with
		| None -> not_logged_in ()
		| Some (uid, _, _, _) -> 
			let%lwt (ex_fname, ex_lname, ex_email) = Database.get_user_data uid in
			let%lwt (ex_address, ex_postcode, ex_town, ex_country, ex_phone) = Database.get_extra_user_data uid in
			container (standard_menu ())
			[
				h1 [pcdata "Your account"];
				p ~a:[a_class ["error"]; a_id "error_paragraph"] [];
				Form.post_form ~service:update_user_service (fun (first_name, (last_name, (email, (password, (address, (postcode, (town, (country, phone)))))))) -> 
				[
					table [
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
						tr [
							th [pcdata "First name"];
							td [Form.input ~a:[a_id "fname_input"] ~input_type:`Text ~name:first_name ~value:ex_fname Form.string]
						];
						tr [
							th [pcdata "Last name"];
							td [Form.input ~a:[a_id "lname_input"] ~input_type:`Text ~name:last_name ~value:ex_lname Form.string]
						];
						tr [
							th [pcdata "Address"];
							td [Form.input ~a:[a_id "address_input"] ~input_type:`Text ~name:address ~value:ex_address Form.string]
						];
						tr [
							th [pcdata "Postcode"];
							td [Form.input ~a:[a_id "postcode_input"] ~input_type:`Text ~name:postcode ~value:ex_postcode Form.string]
						];
						tr [
							th [pcdata "Town"];
							td [Form.input ~a:[a_id "town_input"] ~input_type:`Text ~name:town ~value:ex_town Form.string]
						];
						tr [
							th [pcdata "Country"];
							td [Form.input ~a:[a_id "country_input"] ~input_type:`Text ~name:country ~value:ex_country Form.string]
						];
						tr [
							th [pcdata "Phone number"];
							td [Form.input ~a:[a_id "phone_input"] ~input_type:`Text ~name:phone ~value:ex_phone Form.string]
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
	let p1 = Dom_html.getElementById "password_input1" in
	let p2 = Dom_html.getElementById "password_input2" in
	let ei = Dom_html.getElementById "email_input" in
	let fni = Dom_html.getElementById "first_name_input" in
	let lni = Dom_html.getElementById "last_name_input" in
	let ai = Dom_html.getElementById "address_input" in
	let pci = Dom_html.getElementById "postcode_input" in
	let ti = Dom_html.getElementById "town_input" in
	let ci = Dom_html.getElementById "country_input" in
	let phi = Dom_html.getElementById "phone_input" in
	Js.Opt.iter (Dom_html.CoerceTo.input ei) (fun e ->
		if Js.to_string e##.value = "" then
		begin
			add_or_replace (Js.string "Please enter an address.");
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
				add_or_replace (Js.string "Please enter a password.");
				Dom.preventDefault ev
			end
		)
	);
	Js.Opt.iter (Dom_html.CoerceTo.input fni) (fun e ->
		if Js.to_string e##.value = "" then
		begin
			add_or_replace (Js.string "Please enter a first name.");
			Dom.preventDefault ev
		end
	);
	Js.Opt.iter (Dom_html.CoerceTo.input lni) (fun e ->
		if Js.to_string e##.value = "" then
		begin
			add_or_replace (Js.string "Please enter a last name.");
			Dom.preventDefault ev
		end
	);
	Js.Opt.iter (Dom_html.CoerceTo.input ai) (fun e ->
		if Js.to_string e##.value = "" then
		begin
			add_or_replace (Js.string "Please enter an address.");
			Dom.preventDefault ev
		end
	);
	Js.Opt.iter (Dom_html.CoerceTo.input pci) (fun e ->
		if Js.to_string e##.value = "" then
		begin
			add_or_replace (Js.string "Please enter a postcode.");
			Dom.preventDefault ev
		end
	);
	Js.Opt.iter (Dom_html.CoerceTo.input ti) (fun e ->
		if Js.to_string e##.value = "" then
		begin
			add_or_replace (Js.string "Please enter a town.");
			Dom.preventDefault ev
		end
	);
	Js.Opt.iter (Dom_html.CoerceTo.input ci) (fun e ->
		if Js.to_string e##.value = "" then
		begin
			add_or_replace (Js.string "Please enter a country.");
			Dom.preventDefault ev
		end
	);
	Js.Opt.iter (Dom_html.CoerceTo.input phi) (fun e ->
		if Js.to_string e##.value = "" then
		begin
			add_or_replace (Js.string "Please enter a phone number.");
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
			(fun (first_name, (last_name, (email, (password, (address, (postcode, (town, (country, phone)))))))) -> [
				table [
					tr [
						th [pcdata "E-mail address"];
						td [Form.input ~a:[a_id "email_input"] ~input_type:`Text ~name:email Form.string]
					];
					tr [
						th [pcdata "Password"];
						td [Form.input ~a:[a_id "password_input1"] ~input_type:`Password ~name:password Form.string]
					];
					tr [
						th [pcdata "Confirm password"];
						td [Raw.input ~a:[a_id "password_input2"; a_input_type `Password] ()]
					];
					tr [
						th [pcdata "First name"];
						td [Form.input ~a:[a_id "fname_input"] ~input_type:`Text ~name:first_name Form.string] 
					];
					tr [
						th [pcdata "Last name"];
						td [Form.input ~a:[a_id "lname_input"] ~input_type:`Text ~name:last_name Form.string] 
					];
					tr [
						th [pcdata "Address"];
						td [Form.input ~a:[a_id "address_input"] ~input_type:`Text ~name:address Form.string] 
					];
					tr [
						th [pcdata "Postcode"];
						td [Form.input ~a:[a_id "postcode_input"] ~input_type:`Text ~name:postcode Form.string] 
					];
					tr [
						th [pcdata "Town"];
						td [Form.input ~a:[a_id "town_input"] ~input_type:`Text ~name:town Form.string] 
					];
					tr [
						th [pcdata "Country"];
						td [Form.input ~a:[a_id "country_input"] ~input_type:`Text ~name:country Form.string] 
					];
					tr [
						th [pcdata "Phone number"];
						td [Form.input ~a:[a_id "phone_input"] ~input_type:`Text ~name:phone Form.string] 
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

let add_user_page () (first_name, (last_name, (email, (password, (address, (postcode, (town, (country, phone)))))))) =
	Lwt.catch (fun () -> Database.add_user first_name last_name email password address postcode town country phone >>=
	fun (uid, random) -> begin
	match random with
		| None -> Lwt.fail_with "Did not generate confirmation code"
		| Some x -> Lwt.return x
	end >>=
	fun rstr -> let uri = Eliom_uri.make_string_uri ~absolute:true ~service:confirm_user_service (uid, rstr) in
	Mail.send_register_mail first_name last_name email uri;
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

let update_provisional_user_page user_id () (first_name, (last_name, (old_email, (email, (password, (address, (postcode, (town, (country, phone))))))))) =
	Lwt.catch (fun () ->	Database.add_user ~id:user_id ~confirm:(old_email <> email) first_name last_name email password address postcode town country phone >>=
	fun (_, c_random) -> container (standard_menu ())
	[
		h1 [pcdata "Placeholder"]
	])
	(function
	| e -> error_page (Printexc.to_string e)
	)
;;

let confirm_provisional_user_page (user_id) () =
let update_provisional_user_service = create_attached_post
	~fallback:(preapply confirm_provisional_user_service user_id)
	~post_params:(string "first_name" ** string "last_name" ** string "old_email" ** string "email" ** string "password" ** string "address" ** string "postcode" ** string "town" ** string "country" ** string "phone") () in
	Maw_app.register ~scope:Eliom_common.default_session_scope ~service:update_provisional_user_service (update_provisional_user_page user_id);
	Lwt.catch (fun () -> Database.get_provisional_user_data user_id >>=
	fun ex_email -> container (standard_menu ())
	[
		h1 [pcdata "User data"];
		Form.post_form ~service:update_provisional_user_service
		(fun (first_name, (last_name, (old_email, (email, (password, (address, (postcode, (town, (country, phone))))))))) -> [
			table [
				tr [
					th [pcdata "E-mail address"];
					td [Form.input ~a:[a_id "email_input"] ~input_type:`Text ~name:email ~value:ex_email Form.string;
					Form.input ~input_type:`Hidden ~name:old_email ~value:ex_email Form.string]
				];
				tr [
					th [pcdata "Password"];
					td [Form.input ~a:[a_id "password_input1"] ~input_type:`Password ~name:password Form.string]
				];
				tr [
					th [pcdata "Confirm password"];
					td [Raw.input ~a:[a_id "password_input2"; a_input_type `Password] ()]
				];
				tr [
					th [pcdata "First name"];
					td [Form.input ~a:[a_id "fname_input"] ~input_type:`Text ~name:first_name Form.string] 
				];
				tr [
					th [pcdata "Last name"];
					td [Form.input ~a:[a_id "lname_input"] ~input_type:`Text ~name:last_name Form.string] 
				];
				tr [
					th [pcdata "Address"];
					td [Form.input ~a:[a_id "address_input"] ~input_type:`Text ~name:address Form.string] 
				];
				tr [
					th [pcdata "Postcode"];
					td [Form.input ~a:[a_id "postcode_input"] ~input_type:`Text ~name:postcode Form.string] 
				];
				tr [
					th [pcdata "Town"];
					td [Form.input ~a:[a_id "town_input"] ~input_type:`Text ~name:town Form.string] 
				];
				tr [
					th [pcdata "Country"];
					td [Form.input ~a:[a_id "country_input"] ~input_type:`Text ~name:country Form.string] 
				];
				tr [
					th [pcdata "Phone number"];
					td [Form.input ~a:[a_id "phone_number_input"] ~input_type:`Text ~name:phone Form.string] 
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
	let%lwt users = Database.get_users () in
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
	let%lwt (fname, lname, _) = Database.get_user_data uid in
	let%lwt history = Database.get_user_history uid in
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
	Maw_app.register ~service:account_service account_page;
	Maw_app.register ~service:Maw.register_service register_page;
	Maw_app.register ~service:add_user_service add_user_page;
	Maw_app.register ~service:confirm_user_service confirm_user_page;
	Maw_app.register ~service:confirm_provisional_user_service confirm_provisional_user_page;
	(*Maw_app.register ~service:find_user_service find_user_page*)
	Maw_app.register ~service:user_history_service user_history_page
;;

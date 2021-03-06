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

let account_menu hidden =
	[
		tr [td [a ~service:notifications_service [pcdata "Notifications"] ()]];
		if hidden then
			tr [td [a ~service:hide_account_service [pcdata "Unhide account"] ()]]
		else
			tr [td [a ~service:hide_account_service [pcdata "Hide account"] ()]]
	]
;;

let update_user_page () (fname, (lname, (email, (password, (address, (postcode, (town, (country, phone))))))))  =
	Lwt.catch (fun () ->
		let%lwt u = Eliom_reference.get Maw.user in
		match u with
		| Not_logged_in -> not_logged_in ()
		| User (uid, _, _, _) 
		| Admin (_, (uid, _, _, _))-> Maw_db.update_user_data uid fname lname email password address postcode town country phone>>=
		fun () -> container (standard_menu [])
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
		| Not_logged_in -> not_logged_in ()
		| User (uid, _, _, _)
		| Admin (_, (uid, _, _, _)) -> 
			let%lwt (ex_fname, ex_lname, ex_email, hidden, _) = Maw_db.get_user_data uid in
			let%lwt (ex_address, ex_postcode, ex_town, ex_country, ex_phone) = Maw_db.get_extra_user_data uid in
			container (standard_menu (account_menu hidden))
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

let _ =
	Maw_app.register ~service:account_service account_page
;;

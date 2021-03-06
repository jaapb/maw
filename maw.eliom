[%%shared
	open Eliom_lib
	open Eliom_content
	open Html.D
	open Eliom_parameter
	open Services
	open Utils
]

module Maw_app =
  Eliom_registration.App (
    struct
      let application_name = "maw"
			let global_data_path = None
    end)

(* Random stuff *)

let bool_checkbox name value =
	Form.input ~input_type:`Checkbox ~a:(if value then [a_checked ()] else []) ~name:name Form.bool
	;;

let date_or_tbd date =
	match date with
	| None -> "date TBD"
	| Some d -> CalendarLib.Printer.Date.sprint "%d %B %Y" d
;;

(* Configuration *)

let mail_server_el = Ocsigen_extensions.Configuration.element
	~name:"server" ~obligatory:true ~pcdata:(fun s -> Mail.mail_server := s) ();;
let mail_port_el = Ocsigen_extensions.Configuration.element
	~name:"port" ~obligatory:true
	~pcdata:(fun s -> Mail.mail_port := int_of_string s) ();;
let mail_user_el = Ocsigen_extensions.Configuration.element
	~name:"user" ~obligatory:true ~pcdata:(fun s -> Mail.mail_user := s) ();;
let mail_password_el = Ocsigen_extensions.Configuration.element
	~name:"password" ~obligatory:true ~pcdata:(fun s -> Mail.mail_password := s)
	();;
let mail_el = Ocsigen_extensions.Configuration.element
	~name:"mail" ~obligatory:true
	~elements:[mail_server_el; mail_port_el; mail_user_el; mail_password_el]
	();;

let database_server_el = Ocsigen_extensions.Configuration.element
	~name:"server" ~obligatory:true
	~pcdata:(fun s -> Maw_db.database_server := s) ();;
let database_port_el = Ocsigen_extensions.Configuration.element
	~name:"port"
	~pcdata:(fun s -> Maw_db.database_port := Some (int_of_string s)) ();;
let database_name_el = Ocsigen_extensions.Configuration.element
	~name:"name" ~obligatory:true
	~pcdata:(fun s -> Maw_db.database_name := s) ();;
let database_user_el = Ocsigen_extensions.Configuration.element
	~name:"user" ~obligatory:true
	~pcdata:(fun s -> Maw_db.database_user := s) ();;
let database_password_el = Ocsigen_extensions.Configuration.element
	~name:"password" ~pcdata:(fun s -> Maw_db.database_password := Some s) ();;
let database_el = Ocsigen_extensions.Configuration.element
	~name:"database" ~obligatory:true ~elements:[database_server_el;
		database_port_el; database_name_el; database_user_el; database_password_el]
		();;

(* References *)

type login_type =
	Not_logged_in
| User of int32 * string * string * bool
| Admin of (int32 * string * string) * (int32 * string * string * bool)

let user = Eliom_reference.eref ~scope:Eliom_common.default_session_scope
	Not_logged_in;;
let login_err = Eliom_reference.eref ~scope:Eliom_common.request_scope
	None;;

(* Login bits and pieces *)

let login_action () (name, password) =
	let%lwt u = Maw_db.check_password name password in
	match u with
	| Some (uid, fname, lname, is_admin) -> Eliom_reference.set user (User (uid, fname, lname, is_admin))
	| None -> begin
			Eliom_reference.set user Not_logged_in;
			Eliom_reference.set login_err (Some "Unknown user or wrong password")
		end
;;

let login_box () =
	let%lwt u = Eliom_reference.get user in
	let%lwt err = Eliom_reference.get login_err in
	Lwt.return (match u with
	| Not_logged_in -> [Form.post_form ~service:login_service (fun (name, password) ->
		[table (
			tr [
				td [pcdata "E-mail"];
				td ~a:[a_colspan 2]
					[Form.input ~input_type:`Text ~name:name Form.string]
			]::
			tr [
				td [pcdata "Password"];
				td [Form.input ~input_type:`Password ~name:password Form.string];
				td [Form.input ~input_type:`Submit ~value:"Login" Form.string]
			]::
			tr [
				td ~a:[a_colspan 3]
					[a ~service:register_service [pcdata "Create a new account"] ();
					pcdata " ";
					a ~service:forgot_password_service [pcdata "Forgot password"] ()]
			]::
			(match err with
			| None -> []
			| Some e -> [tr [td ~a:[a_colspan 3; a_class ["error"]] [pcdata e]]]
			)
		)]) ()]
	| User (_, fn, ln, _) -> [Form.post_form ~service:logout_service (fun () ->
		[table [
			tr [
			 	td [pcdata (Printf.sprintf "Logged in as %s %s" fn ln)]
			];
			tr [
				td [Form.input ~input_type:`Submit ~value:"Logout" Form.string]
			]
		]]) ()]
	| Admin (_, (_, fn, ln, _)) -> [Form.get_form ~service:admin_logout_service (fun () ->
		[table [
			tr [
				td [i [pcdata (Printf.sprintf "Temporarily logged in as %s %s" fn ln)]]
			];
			tr [
				td [Form.input ~input_type:`Submit ~value:"Logout" Form.string]
			]
		]])]
	);;	

(* The standard webpage container *)

let standard_menu extra_rows = 
	let%lwt u = Eliom_reference.get user in
	match u with
	| Not_logged_in -> Lwt.return []
	| User (_, _, _, is_admin)
	| Admin (_, (_, _, _, is_admin)) -> Lwt.return [
		table (
			tr [
				td [a ~service:dashboard_service [pcdata "Dashboard"] ()]
			]::
			tr [
				td [a ~service:account_service [pcdata "My account"] ()]
			]::
			tr ~a:[a_class ["separator"]] [
			]::
			(if is_admin
			then 
				List.rev
				(
					tr [td [a ~service:admin_login_service [b [pcdata "Login as another user"]] ()]]::
					tr [td [a ~service:admin_message_service [b [pcdata "Send messages"]] ()]]::
					tr [td [a ~service:admin_confirm_users_service [b [pcdata "Manually confirm users"]] ()]]::
					tr [td [a ~service:set_game_data_service [b [pcdata "Set game data"]] ()]]::
					tr [td [a ~service:new_game_service [b [pcdata "Create a new game"]] ()]]::
					tr [td [i [pcdata "Admin menu"]]]::
					tr ~a:[a_class ["separator"]] []::
					(List.rev extra_rows)
				)
			else extra_rows)
		)]
;;

let container ?onload menu_thread cts_div =
	let%lwt box = login_box () in
	let%lwt menu_div = menu_thread in
	let body_contents = [ 
		div ~a:[a_class ["layout"]; a_id "header"] [h1 [pcdata "MAW"]];
		div ~a:[a_class ["layout"]; a_id "logbox"] box;
		div ~a:[a_class ["layout"]; a_id "menu"] menu_div;
		div ~a:[a_class ["layout"]; a_id "contents"] cts_div;
		div ~a:[a_class ["layout"]; a_id "footer"] [
			img ~alt:"Powered by Ocsigen"
			~src:(make_uri ~service:(Eliom_service.static_dir ())
				["ocsigen-powered.png"]) ()
		]
	] in
	Lwt.return
	(Eliom_tools.F.html
		~title:"maw"
		~css:[["css";"maw.css"]]
		(match onload with
		| None -> Html.F.(body body_contents)
		| Some x -> Html.F.(body ~a:[a_onload x] body_contents))
	);;

let error_page e =
	container (standard_menu [])
	[
		h1 [pcdata "Error"];
		p [pcdata e]
	];;

(* Main page *)

let location_bar id title date loc =
	[
		a ~service:game_service [pcdata title] id;
		pcdata (Printf.sprintf " (%s, %s)" loc (date_or_tbd date))
	];;

let format_upcoming_games ug =
	match ug with
	| [] -> p [pcdata "Strangely, there are no games planned at all."]
	| l ->
		table (
			List.flatten (List.map (function
			| (id, title, date, loc, _, _) -> 
				[tr [td (location_bar id title date loc)]]
			) l)
		)
;;

let format_my_games mg dg =
	Lwt.return (
		h2 [pcdata "My games"]::
		(match mg with
		| [] -> p [pcdata "You are not signed up for any games at the moment."]
		| l -> table (List.flatten (List.map
			(function 
			| (id, title, date, loc, cast) ->
				[tr (
					td (location_bar id title date loc)::
					td [a ~service:signup_service [pcdata "Edit inscription"] id]::
					td [a ~service:cancel_service [pcdata "Cancel inscription"] id]::
					if cast
					then [td [a ~service:show_casting_service [pcdata "Show casting"] id]]
					else []
				)]
			) l)))::
		(match dg with
		| [] -> []
		| l -> [
				h2 [pcdata "My designs"];
				table (List.flatten (List.map
				(function
				| (id, title, date, loc) ->
					[tr [
						td (location_bar id title date loc);
						td [a ~service:design_service [pcdata "Edit design"] id];
						td [a ~service:show_inscriptions_service [pcdata "Show inscriptions"] id];
						td [a ~service:cast_service [pcdata "Casting"] id];
						td [a ~service:designer_message_service [pcdata "Messages"] id]
					]]
				) l))
			])
	);;

let dashboard_page () () =
	Lwt.catch (fun () ->
	 	let%lwt ug = Maw_db.get_upcoming_games () in
		let%lwt u = Eliom_reference.get user in
		let%lwt mg_fmt = match u with
		| Not_logged_in -> Lwt.return []
		| User (uid, _, _, _)
		| Admin (_, (uid, _, _, _)) -> Maw_db.get_user_games uid >>=
				fun mg -> Maw_db.get_designer_games uid >>=
				fun dg -> format_my_games mg dg
		in
		container (standard_menu [])
		(
			h1 [pcdata "Upcoming games"]::
			format_upcoming_games ug::
			mg_fmt
		)
	)
	(fun e -> error_page (Printexc.to_string e));;

let do_logout_page () () =
begin
	ignore (Eliom_reference.set user Not_logged_in);
	dashboard_page () ()
end;;

(* Generic messages *)

let not_logged_in () =
	container (standard_menu [])
	[
		p [pcdata "You need to be logged in for this to work. You can log in (or create a new account) on the top right of the screen."]
	];;

let unknown_game () =
	container (standard_menu [])
	[
		h1 [pcdata "Unknown game"];
		p [pcdata "Sorry, that game does not exist."]
	];;

let () =
	Eliom_config.parse_config [mail_el; database_el];
	Eliom_registration.Action.register ~service:login_service login_action;
	Maw_app.register ~service:logout_service do_logout_page;
	Maw_app.register ~service:dashboard_service dashboard_page
;;


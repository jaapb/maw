[%%shared
	open Eliom_lib
	open Eliom_content
	open Html.D
	open Eliom_parameter
	open Eliom_service
]

module Maw_app =
  Eliom_registration.App (
    struct
      let application_name = "maw"
			let global_data_path = None
    end)

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

(* Services *)

let dashboard_service = create ~path:(Path []) ~meth:(Get unit) ();;
let login_service = create ~path:No_path
	~meth:(Post (unit, (string "name" ** string "password"))) ();;
let logout_service = create ~path:No_path
	~meth:(Post (unit, unit)) ();;
let account_service = create ~path:(Path ["account"]) ~meth:(Get unit) ();;
let admin_service = create ~path:(Path ["admin"]) ~meth:(Get unit) ();;
let register_service = create ~path:(Path ["register"]) ~meth:(Get unit) ();;

(* Login bits and pieces *)

let user = Eliom_reference.eref ~scope:Eliom_common.default_session_scope
	None;;
let login_err = Eliom_reference.eref ~scope:Eliom_common.request_scope
	None;;

let login_action () (name, password) =
	let%lwt u = Database.check_password name password in
	match u with
	| Some (uid, fname, lname, is_admin) -> Eliom_reference.set user (Some (uid, fname, lname, is_admin))
	| None -> Eliom_reference.set login_err (Some "Unknown user or wrong password")
;;

let logout_action () () =
begin
	Eliom_reference.set user None
end;;

let login_box () =
	let%lwt u = Eliom_reference.get user in
	let%lwt err = Eliom_reference.get login_err in
	Lwt.return (match u with
	| None -> [Form.post_form ~service:login_service (fun (name, password) ->
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
					[a ~service:register_service [pcdata "Create a new account"] ()]
			]::
			(match err with
			| None -> []
			| Some e -> [tr [td ~a:[a_colspan 3; a_class ["error"]] [pcdata e]]]
			)
		)]) ()]
	| Some (_, fn, ln, _) -> [Form.post_form ~service:logout_service (fun () ->
		[table [
			tr [
			 	td [pcdata (Printf.sprintf "Logged in as %s %s" fn ln)]
			];
			tr [
				td [Form.input ~input_type:`Submit ~value:"Logout" Form.string]
			]
		]]) ()]
	);;	

(* The standard webpage container *)

let standard_menu () = 
	let%lwt u = Eliom_reference.get user in
	match u with
	| None -> Lwt.return []
	| Some (_, _, _, is_admin) -> Lwt.return [
		table (
			tr [
				td [a ~service:dashboard_service [pcdata "Dashboard"] ()]
			]::
			tr [
				td [a ~service:account_service [pcdata "My account"] ()]
			]::
			(if is_admin
			then [tr [td [a ~service:admin_service [b [pcdata "Admin"]] ()]]]
			else [])
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
	container (standard_menu ())
	[
		h1 [pcdata "Error"];
		p [pcdata e]
	];;

(* Generic messages *)

let not_logged_in () =
	container (standard_menu ())
	[
		p [pcdata "You need to be logged in for this to work. You can log in (or create a new account) on the top right of the screen."]
	];;

let unknown_game () =
	container (standard_menu ())
	[
		h1 [pcdata "Unknown game"];
		p [pcdata "Sorry, that game does not exist."]
	];;

(* Random stuff *)

let bool_checkbox name value =
	Form.input ~input_type:`Checkbox ~a:(if value then [a_checked ()] else []) ~name:name Form.bool
	;;

let () =
	Eliom_config.parse_config [mail_el];
	Eliom_registration.Action.register ~service:login_service login_action;
	Eliom_registration.Action.register ~service:logout_service logout_action
;;

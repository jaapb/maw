[%%shared
	open Eliom_lib
	open Eliom_content
	open Html5.D
	open Eliom_service.App
	open Eliom_parameter
]

module Maw_app =
  Eliom_registration.App (
    struct
      let application_name = "maw"
    end)

(* Utility functions *)

let default d o =
	match o with
	| None -> d
	| Some x -> x
;;

let date_or_tbd date =
	match date with
	| None -> "date TBD"
	| Some d -> CalendarLib.Printer.Date.sprint "%d %B %Y" d
;;

(* Services *)

let dashboard_service = service ~path:[] ~get_params:unit ();;
let login_service = post_coservice'
	~post_params:(string "name" ** string "password") ();;
let logout_service = post_coservice'
	~post_params:unit ();;
let account_service = service ~path:["account"] ~get_params:unit ();;
let admin_service = service ~path:["admin"] ~get_params:unit ();;

(* Login bits and pieces *)

let user = Eliom_reference.eref ~scope:Eliom_common.default_session_scope
	None;;
let login_err = Eliom_reference.eref ~scope:Eliom_common.request_scope
	None;;

let login_action () (name, password) =
	let%lwt u = Database.check_password name password in
	match u with
	| [(uid, name, is_admin)] -> Eliom_reference.set user (Some (uid, name, is_admin))
	| _ -> Eliom_reference.set login_err (Some "Unknown user")
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
				td [pcdata "Username"];
				td [Form.input ~input_type:`Text ~name:name Form.string];
				td [Form.input ~input_type:`Submit ~value:"Login" Form.string]
			]::
			tr [
				td [pcdata "Password"];
				td ~a:[a_colspan 2]
					[Form.input ~input_type:`Password ~name:password Form.string]
			]::
			(match err with
			| None -> []
			| Some e -> [tr [td ~a:[a_colspan 3] [pcdata e]]]
			)
		)]) ()]
	| Some (_, n, _) -> [Form.post_form ~service:logout_service (fun () ->
		[table [
			tr [
			 	td [pcdata (Printf.sprintf "Logged in as %s" n)]
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
	| Some (_, _, is_admin) -> Lwt.return [
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

let container menu_thread cts_div =
	let%lwt box = login_box () in
	let%lwt menu_div = menu_thread in
	Lwt.return
	(Eliom_tools.F.html
		~title:"maw"
		~css:[["css";"maw.css"]]
		Html5.F.(body [
			div ~a:[a_id "header"] [h1 [pcdata "MAW"]];
			div ~a:[a_id "logbox"] box;
			div ~a:[a_id "menu"] menu_div;
			div ~a:[a_id "contents"] cts_div;
			div ~a:[a_id "footer"] [
				img ~alt:"Powered by Ocsigen"
				~src:(make_uri ~service:(Eliom_service.static_dir ())
					["ocsigen-powered.png"]) ()
			]
		])
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
		p [pcdata "You need to be logged in for this to work."]
	];;

let unknown_game () =
	container (standard_menu ())
	[
		h1 [pcdata "Unknown game"];
		p [pcdata "Sorry, that game does not exist."]
	];;

let () =
	Eliom_registration.Action.register ~service:login_service login_action;
	Eliom_registration.Action.register ~service:logout_service logout_action
;;

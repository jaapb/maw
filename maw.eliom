{shared{
  open Eliom_lib
  open Eliom_content
  open Html5.D
	open Eliom_service.App
	open Eliom_parameter
}}

module Maw_app =
  Eliom_registration.App (
    struct
      let application_name = "maw"
    end)

(* Simple conversion functions *)

let location_text =
	function
	| None -> "Location TBD"
	| Some x -> x;;

let description_text =
	function
	| None -> "No description yet"
	| Some x -> x;;

(* Services *)

let dashboard_service = service ~path:[] ~get_params:unit ();;
let login_service = post_coservice'
	~post_params:(string "name" ** string "password") ();;

(* Login bits and pieces *)

let user = Eliom_reference.eref ~scope:Eliom_common.default_session_scope
	None;;
let login_err = Eliom_reference.eref ~scope:Eliom_common.request_scope
	None;;

let login_action () (name, password) =
begin
	lwt u = Database.check_password name password in
	match u with
	| [(uid, name)] -> Eliom_reference.set user (Some (uid, name))
	| _ -> Eliom_reference.set login_err (Some "Unknown user")
end;;

let login_box () =
	lwt u = Eliom_reference.get user in
	Lwt.return (match u with
	| None -> [Form.post_form ~service:login_service (fun (name, password) ->
		[table [
			tr [
				td [pcdata "Username"];
				td [Form.input ~input_type:`Text ~name:name Form.string];
				td [Form.input ~input_type:`Submit ~value:"Login" Form.string]
			];
			tr [
				td [pcdata "Password"];
				td ~a:[a_colspan 2]
					[Form.input ~input_type:`Password ~name:password Form.string]
			]
		]]) ()]
	| Some (_, n) -> [p [pcdata (Printf.sprintf "Logged in as %s" n)]]
	);;

(* The standard webpage container *)

let standard_menu () = 
	[table [
		tr [
			td [a ~service:dashboard_service [pcdata "Dashboard"] ()]
		];
		tr [
			td [pcdata "My account"]
		]
	]];;

let container menu_div cts_div =
	lwt box = login_box () in
	Lwt.return
	(Eliom_tools.F.html
		~title:"maw"
		~css:[["css";"maw.css"]]
		Html5.F.(body [
			div ~a:[a_id "header"] [h1 [pcdata "MAW"]];
			div ~a:[a_id "logbox"] box;
			div ~a:[a_id "menu"] menu_div;
			div ~a:[a_id "contents"] cts_div;
			div ~a:[a_id "footer"] [p [pcdata "This is not a footer"]];
		])
	);;

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
	Eliom_registration.Action.register ~service:login_service login_action;;

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

let update_provisional_user_page user_id () (first_name, (last_name, (old_email, (email, (password, (address, (postcode, (town, (country, phone))))))))) =
	Lwt.catch (fun () ->	Maw_db.add_user ~id:user_id ~confirm:(old_email <> email) first_name last_name email password address postcode town country phone >>=
	fun (_, c_random) -> container (standard_menu [])
	[
		h1 [pcdata "Success"];
		p [pcdata "Your account has been confirmed."]
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
	Lwt.catch (fun () -> Maw_db.get_provisional_user_data user_id >>=
	fun (ex_email, ex_fname, ex_lname) ->
	container (standard_menu [])
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
					td [Form.input ~a:[a_id "fname_input"] ~input_type:`Text ~name:first_name ~value:ex_fname Form.string] 
				];
				tr [
					th [pcdata "Last name"];
					td [Form.input ~a:[a_id "lname_input"] ~input_type:`Text ~name:last_name ~value:ex_lname Form.string] 
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
					td ~a:[a_colspan 2] [Form.input ~a:[a_onclick [%client Register.check_register_form]]
					~input_type:`Submit ~value:"Sign up" Form.string]
				]
			]
		]) ()			
	]) 
	(function
	| Not_found -> error_page ("Provisional user not found or expired.")
	| e -> error_page (Printexc.to_string e)
	)
;;

let _ =
	Maw_app.register ~service:confirm_provisional_user_service confirm_provisional_user_page
;;

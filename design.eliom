{shared{
	open Eliom_lib
	open Eliom_content
	open Html5.D
	open Eliom_service.App
	open Eliom_parameter
}}

{server{
	open CalendarLib
	open Maw
	open Database
}}

let design_service = service ~path:["design"] ~get_params:(suffix (int32 "game_id")) ();;
let update_descr_service = post_service ~fallback:design_service ~post_params:(string "description") ();;

let design_page game_id () = 
	lwt u = Eliom_reference.get Maw.user in
	match u with
	| None -> not_logged_in ()
	| Some (uid, _) -> lwt ni = Database.get_nr_inscriptions game_id in
		let nr_inscr = match ni with
		| [Some x] -> x
		| _ -> 0L in
		lwt data = Database.get_game_data game_id in
		match data with
		| [(title, Some date, loc, _, dsg_id, d)] ->
			if uid = dsg_id then
				container (standard_menu ())
				[
					h1 [pcdata title];
					p [pcdata (location_text loc);
						pcdata (Printer.Date.sprint ", %d %B %Y" date)];
					p [pcdata (Printf.sprintf "Currently, %Ld person(s) has/have signed up for this game." nr_inscr)];
					Form.post_form ~service:update_descr_service (fun descr ->
						[
							table [
								tr [
									td [pcdata "Game description:"]
								];
								tr [	
									td [
										Form.textarea ~a:[a_cols 60; a_rows 10] ~name:descr
										~value:(match d with
											| None -> ""
											| Some d -> d) ()
									]
								];
								tr [
									td [
										Form.input ~input_type:`Submit ~value:"Update" Form.string
									]
								]
							]
						]
					) game_id
				]
			else container (standard_menu ())
				[
					p [pcdata "You are not the designer of this game."]
				]
		| _ -> unknown_game ()
;;

let update_description game_id descr =
	lwt u = Eliom_reference.get Maw.user in
	match u with
	| None -> Lwt.return ();
	| Some (uid, _) -> Database.set_description game_id descr
;;
	
let () =
	Maw_app.register ~service:design_service design_page;;
	Eliom_registration.Action.register ~service:update_descr_service
		update_description;;

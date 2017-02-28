open CalendarLib

let dbh = PGOCaml.connect ~host:"localhost" ~database:"maw" ();;

let find_game a =
	match PGSQL(dbh) "SELECT id FROM games WHERE abbreviation = $a" with
	| [id] -> id
	| [] -> raise Not_found
	| _ -> failwith "Inconsistent database"
;;

let add_inscription g u i t r =
	PGSQL(dbh) "INSERT INTO game_inscriptions (game_id, user_id, inscription_time, preferred_team, preferred_role, note) \
	VALUES \
	($g, $u, $?i, $t, $r, '') ON CONFLICT DO NOTHING"
;;

let _ =
	let bookings = Csv.of_channel (open_in (Printf.sprintf "%s/Bookings.csv" Sys.argv.(1))) in
begin
	Csv.iter (fun sl ->
		let abbr::gamer_id::status::_::team::role::_::_::_::_::_::_::_::_::_::_::_::_::_::date_on_wl::_ = sl in
		let int_gamer_id = Int32.of_string gamer_id in
		let c_date_on_wl = 
			if date_on_wl = "" then None
			else Some (Scanf.sscanf date_on_wl "%d/%d/%d %d:%d:%d" 
				(fun d m y h min s -> Calendar.make y m d h min s)) in
		let id = find_game abbr in
		add_inscription id int_gamer_id c_date_on_wl team role;
		Printf.eprintf "Added inscription of %s to %s\n%!" gamer_id abbr
	) bookings;
	Csv.close_in bookings;
	PGOCaml.close dbh
end;;

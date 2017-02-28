open CalendarLib

let dbh = PGOCaml.connect ~host:"localhost" ~database:"maw" ();;

let find_game a =
	let la = String.lowercase a in
	match PGSQL(dbh) "SELECT id FROM games WHERE lower(abbreviation) = $la" with
	| [id] -> id
	| [] -> raise Not_found
	| _ -> failwith "Inconsistent database"
;;

let add_inscription g u i n s c =
	PGSQL(dbh) "INSERT INTO game_inscriptions (game_id, user_id, inscription_time, note, status, cancelled) \
	VALUES \
	($g, $u, $?i, $n, $s, $c) ON CONFLICT DO NOTHING"
;;

let add_casting g u t r =
	PGSQL(dbh) "INSERT INTO game_casting (game_id, user_id, team_name, role_name) \
	VALUES \
	($g, $u, $t, $r) ON CONFLICT DO NOTHING"
;;

let status_to_db s =
	if s = "Interested" || s = "Not confirmed" || s = "Paid deposit" then (4l, false)
	else if s = "Confirmed" || s = "Guest" || s = "Invited" then (2l, false)
	else if String.lowercase s = "played" then (1l, false)
	else if s = "Paid full" || s = "Paid" then (3l, false)
	else if s = "Noshow" || s = "No Show" || s = "No show-Paid" then (0l, false)
	else if s = "WAITING LIST" then (3l, false)
	else if s = "Cancelled - paid full" || s = "Cancelled-Paid" then (3l, true)
	else if s = "Cancelled - paid deposit" || s = "Cancelled - not paid" || s = "Cancelled" then (4l, true)
	else raise (Invalid_argument (Printf.sprintf "Wrong status (%s)." s))
;;

let _ =
	let bookings = Csv.of_channel (open_in (Printf.sprintf "%s/Bookings.csv" Sys.argv.(1))) in
begin
	Csv.iter (fun sl ->
		let abbr::gamer_id::status::interest::team::role::_::_::_::_::_::_::_::_::_::_::_::_::_::date_on_wl::_ = sl in
		let int_gamer_id = Int32.of_string gamer_id in
		let c_date_on_wl = 
			if date_on_wl = "" then None
			else Some (Scanf.sscanf date_on_wl "%d/%d/%d %d:%d:%d" 
				(fun d m y h min s -> Calendar.make y m d h min s)) in
		try
			let id = find_game abbr in
			let (s, c) = status_to_db status in
			add_inscription id int_gamer_id c_date_on_wl interest s c;
			(if team <> "" && role <> "" then
				add_casting id int_gamer_id team role);
			Printf.eprintf "Added inscription of %s to %s\n%!" gamer_id abbr
		with
			Not_found -> Printf.eprintf "Ignored inscription of %s to %s, nonexistent game\n%!" gamer_id abbr
	) bookings;
	Csv.close_in bookings;
	PGOCaml.close dbh
end;;

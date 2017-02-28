open CalendarLib

let dbh = PGOCaml.connect ~host:"localhost" ~database:"maw" ();;

let add_game a t d l =
 	match PGSQL(dbh) "INSERT INTO games (abbreviation, title, date, location) \
		VALUES \
		($a, $t, $d, $l) ON CONFLICT DO NOTHING RETURNING id" with
	| [id] -> id
	| [] -> raise Not_found 
	| _ -> failwith "Multiple (or no) IDs returned"
;;

let get_user_id f l =
	let x = PGSQL(dbh) "SELECT id FROM users WHERE first_name = $f \
		AND last_name = $l" in
	match x with
	| [id] -> id
	| _ -> Printf.eprintf "Unknown: %s %s\n%!" f l; raise Not_found
;;

let set_designer g i =
	PGSQL(dbh) "INSERT INTO game_designer (game_id, designer) \
		VALUES ($g, $i) ON CONFLICT DO NOTHING"
let _ =
	let megagames = Csv.of_channel (open_in (Printf.sprintf "%s/Megagames.csv" Sys.argv.(1))) in
begin
	Csv.iter (fun sl ->
		let abbr::name::_::date::designer::venue::nam::_ = sl in
		if nam = "0" then
		begin
			let c_date = Scanf.sscanf date "%d/%d/%d %d:%d:%d"
				(fun d m y h min s -> Date.make y m d) in
			try
				let game_id = add_game abbr name c_date venue in
				List.iter (fun s ->
					let id = Scanf.sscanf (String.trim s) "%s %s" (fun f l -> get_user_id f l) in
					set_designer game_id id
				) (String.split_on_char '&' designer);
				Printf.eprintf "Inserted game %s\n%!" name
			with Not_found -> ()
		end
	) megagames;
	Csv.close_in megagames;
	PGOCaml.close dbh
end;;

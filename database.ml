open Lwt

module PGOCaml = PGOCaml_generic.Make(struct include Lwt include Lwt_chan end)

let db_handler = ref None;;

let get_db () =
	let host = try Some (Sys.getenv "PGHOST") with Not_found -> None in
	let database = try Some (Sys.getenv "PGDATABASE") with Not_found -> None in
	let user = try Some (Sys.getenv "PGUSER") with Not_found -> None in
	let password = try Some (Sys.getenv "PGPASSWORD") with Not_found -> None in
	match !db_handler with
	| Some h -> return h
	| None -> begin
			PGOCaml.connect ?host ?database ?user ?password () >>=
			fun dbh -> db_handler := Some dbh; return dbh
		end
;;

let crypt_password pwd salt =
	Cryptokit.transform_string (Cryptokit.Base64.encode_compact ()) (Cryptokit.hash_string (Cryptokit.Hash.sha3 512) (salt ^ pwd))
;;

let random_string length =
	String.sub (Cryptokit.transform_string (Cryptokit.Base64.encode_compact ()) (Cryptokit.Random.string Cryptokit.Random.secure_rng length)) 0 length
;;

let int32_of_inscr_status s =
	match s with
	| `No_show -> 0l
	| `Attended -> 1l
	| `Confirmed -> 2l
	| `Paid -> 3l
	| `Interested -> 4l
	| `Potential -> 5l
	| `Waiting -> 6l
;;

let inscr_status_of_int32 s =
	if s = 0l then `No_show
	else if s = 1l then `Attended
	else if s = 2l then `Confirmed
	else if s = 3l then `Paid
	else if s = 4l then `Interested
	else if s = 5l then `Potential
	else if s = 6l then `Waiting
	else raise (Invalid_argument (Printf.sprintf "Unknown status code: %ld" s))
;;

let rec aux_assoc_list l ct cr res =
	match l with
	| [] -> (ct, cr)::res
	| (ht, hr)::t -> begin
			if ht = ct
			then aux_assoc_list t ct (hr::cr) res 
			else aux_assoc_list t ht [hr] ((ct, cr)::res)
		end
;;

let get_upcoming_games ?(all=false) () =
	let today = CalendarLib.Date.today () in
	get_db () >>= fun dbh ->
	if all then PGSQL(dbh) "SELECT id, title, date, location, visible, bookable \
    FROM games \
    WHERE date >= $today \
    ORDER BY date ASC"
	else PGSQL(dbh) "SELECT id, title, date, location, visible, bookable \
    FROM games \
    WHERE date >= $today AND visible \
    ORDER BY date ASC"
;;

let get_user_games uid =
	let today = CalendarLib.Date.today () in
	get_db () >>= fun dbh ->
	PGSQL(dbh) 
		"SELECT id, title, date, location, casting_published \
		FROM games JOIN game_inscriptions ON games.id = game_id \
		WHERE date >= $today AND user_id = $uid AND NOT cancelled \
		ORDER BY date ASC";;

let get_designer_games uid =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "SELECT id, title, date, location \
		FROM games \
		WHERE designer = $uid \
		ORDER BY date ASC";;

let get_game_data game_id =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "SELECT title, date, location, first_name, last_name, designer,
		description, min_players, max_players, casting_published \
		FROM games JOIN users ON designer = users.id \
		WHERE games.id = $game_id" >>=
	function
	| [] -> fail Not_found
	| [x] -> return x
	| _ -> fail_with "Inconsistent database"
;;

let check_password email password =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "SELECT id, password, password_salt, first_name, last_name, \
	  is_admin \
		FROM users \
		WHERE email = $email AND confirmation IS NULL" >>=
	function
	| [(id, db_password, db_salt, first_name, last_name, is_admin)] ->
		if crypt_password password db_salt = db_password then Lwt.return (Some (id, first_name, last_name, is_admin))
		else Lwt.return None
	| _ -> Lwt.return None (* don't want to fail with Inconsistent database here
	  as it would give away information *)
;;

let sign_up_status uid game_id =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "SELECT game_id, preferred_team, preferred_role, status, \
		cancelled \
		FROM games JOIN game_inscriptions ON games.id = game_id \
		WHERE user_id = $uid AND games.id = $game_id" >>=
	function
	| [(_, t, r, s, false)] -> Lwt.return (`Yes (t, r, inscr_status_of_int32 s))
	| [(_, _, _, _, true)] -> Lwt.return `Cancelled
	| _ -> Lwt.return `No
;;

let set_game_description game_id descr =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "UPDATE games SET description = $descr \
		WHERE id = $game_id";;

let get_nr_inscriptions game_id =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "SELECT COUNT(users.id) \
		FROM users JOIN game_inscriptions ON users.id = user_id \
		WHERE game_id = $game_id AND NOT cancelled" >>=
	function
	| [Some n] -> return (Int64.to_int32 n)
	| _ -> return 0l;;

let get_game_teams game_id =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "SELECT DISTINCT(team_name) \
		FROM game_casting \
		WHERE game_id = $game_id";;

let get_game_roles game_id =
	get_db () >>=
	fun dbh -> PGSQL(dbh) "SELECT team_name, role_name \
		FROM game_casting \
		WHERE game_id = $game_id \
		ORDER BY team_name" >>=
	fun l -> match l with
	| [] -> Lwt.return []
	| (ht, hr)::t -> Lwt.return (aux_assoc_list t ht [hr] [])
;;

let set_game_numbers game_id min max =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "UPDATE games \
		SET min_players = $min, max_players = $max \
		WHERE id = $game_id";;

let add_inscription game_id uid group_name status team role note =
	let change_things dbh game_id uid group_name team role =
		(match team with
		| None -> return ()
		| Some g -> PGSQL(dbh) "UPDATE game_inscriptions \
				SET preferred_team = $g WHERE game_id = $game_id AND user_id = $uid") >>=
		fun () -> (match role with
		| None -> return ()
		| Some r -> PGSQL(dbh) "UPDATE game_inscriptions \
				SET preferred_role = $r WHERE game_id = $game_id AND user_id = $uid") >>=
		fun () -> (match group_name with
		| None -> return ()
		| Some g -> PGSQL(dbh) "UPDATE game_inscriptions \
				SET group_name = $g WHERE game_id = $game_id AND user_id = $uid")
	in
	let stamp = CalendarLib.Calendar.now () in
	let st = int32_of_inscr_status status in
	get_db () >>= fun dbh ->
	PGOCaml.transact dbh (fun dbh ->
		PGSQL(dbh) "SELECT game_id, user_id \
			FROM game_inscriptions \
			WHERE game_id = $game_id AND user_id = $uid" >>=
		(function
		| [] -> PGSQL(dbh) "INSERT INTO game_inscriptions \
		  	(game_id, user_id, inscription_time, note, status) \
		  	VALUES \
		  	($game_id, $uid, $stamp, $note, $st)"
		| _ -> PGSQL(dbh) "UPDATE game_inscriptions \
				SET note = $note \
				WHERE game_id = $game_id AND user_id = $uid") >>=
	  fun () -> change_things dbh game_id uid group_name team role
	)
;;

let get_inscription_data uid game_id =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "SELECT g2.user_id, first_name, last_name, g2.preferred_team, \
		g2.preferred_role, g2.note, g2.group_name, g2.status \
		FROM game_inscriptions g1 JOIN game_inscriptions g2 \ 
		ON g1.game_id = g2.game_id AND \
			(g1.user_id = g2.user_id OR g1.group_name = g2.group_name) \
		JOIN users ON g2.user_id = users.id \
		WHERE g1.user_id = $uid AND g1.game_id = $game_id \
		AND NOT g1.cancelled AND NOT g2.cancelled" >>=
	fun l -> Lwt_list.map_p
		(fun (u, fnm, lnm, tn, rt, nt, gn, s) ->
			Lwt.return (u, fnm, lnm, tn, rt, nt, gn, inscr_status_of_int32 s)) l
;;

let get_inscription_list ?(filter_cast = false) game_id =
	get_db () >>= fun dbh ->
	if filter_cast then PGSQL(dbh) "SELECT first_name, last_name, i.user_id, \
	  i.preferred_team, i.preferred_role, note, group_name, status \
		FROM game_inscriptions i JOIN users ON user_id = users.id 
			LEFT JOIN game_casting c \
			ON i.user_id = c.user_id AND i.game_id = c.game_id \
		WHERE i.game_id = $game_id AND role_name IS NULL \
		AND NOT cancelled \
		ORDER BY status ASC, group_name ASC, inscription_time ASC"
	else PGSQL(dbh) "SELECT first_name, last_name, user_id, preferred_team, \
		preferred_role, note, group_name, status \
		FROM game_inscriptions JOIN users ON user_id = users.id \
		WHERE game_id = $game_id AND NOT cancelled \
		ORDER BY status ASC, group_name ASC, inscription_time ASC"
;;

let search_for_user search =
 	let search_expr = Printf.sprintf ".*%s.*" search in
  get_db () >>= fun dbh ->
  PGSQL(dbh) "SELECT id, first_name, last_name, email \
    FROM users \
    WHERE email = $search OR last_name ~* $search_expr"
;;

let get_group_name game_id uid_list =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "SELECT DISTINCT group_name \
		FROM game_inscriptions \
		WHERE game_id = $game_id AND user_id IN $@uid_list \
		AND NOT cancelled" >>=
	function
	| [] | [None] -> return ""
	| [Some gname] -> return gname
	| _ -> fail (Invalid_argument "Group members in two groups")
;;

let get_user_data uid =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "SELECT first_name, last_name, email \
		FROM users \
		WHERE id = $uid" >>=
	function
	| [] -> fail Not_found
	| [u] -> return u
	| _ -> fail_with "Inconsistency in database"
;;

let get_extra_user_data uid =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "SELECT address, postcode, town, country, phone_number \
	  FROM users \
		WHERE id = $uid" >>=
	function
	| [] -> fail Not_found
	| [u] -> return u
	| _ -> fail_with "Inconsistency in database"
;;

let update_user_data uid fname lname email password address postcode town country phone =
	let salt = random_string 8 in
	let c_password = crypt_password password salt in
	get_db () >>= fun dbh ->
	PGSQL(dbh) "UPDATE users \
		SET email = $email, password = $c_password, password_salt = $salt, \
		first_name = $fname, last_name = $lname, address = $address, town = $town, \
		phone_number = $phone, postcode = $postcode, country = $country \
		WHERE id = $uid"
;;

(* CLears and re-adds, not ideal *)
let update_casting game_id teams =
	get_db () >>= fun dbh ->
	PGOCaml.transact dbh (fun dbh ->
		PGSQL(dbh) "DELETE FROM game_casting \
			WHERE game_id = $game_id" >>=
		fun () -> Lwt_list.iter_s (fun (t_name, roles) ->
			Lwt_list.iter_s (fun (r_name, user_id) ->
			PGSQL(dbh) "INSERT INTO game_casting \
				(game_id, team_name, role_name, user_id) \
				VALUES \
				($game_id, $t_name, $r_name, $user_id)"
			) roles
		) teams
	)
;;

let is_published game_id =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "SELECT casting_published \
		FROM games \
		WHERE id = $game_id" >>=
	function
	| [] -> Lwt.return false
	| [b] -> Lwt.return b
	| _ -> fail_with "Inconsistency in database"
;;

let set_published game_id b =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "UPDATE games \
		SET casting_published = $b \
		WHERE id = $game_id"
;;

let get_casting game_id =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "nullable-results"
		"SELECT team_name, role_name, first_name, last_name, c.user_id, \
	  note, group_name \
		FROM game_casting c LEFT JOIN users ON c.user_id = users.id \
		LEFT JOIN game_inscriptions i ON i.user_id = c.user_id \
		  AND i.game_id = c.game_id \
		WHERE c.game_id = $game_id AND NOT cancelled \
		ORDER BY team_name, role_name DESC" >>=
	fun l -> Lwt_list.map_p (fun (ht, hr, hf, hl, hu, hn, hg) ->
		match ht, hr with
		| Some x, Some y -> Lwt.return (x, (y, hf, hl, hu, hn, hg))
		| _, _ -> Lwt.fail_with "Inconsistent database") l >>=
	fun l' -> match l' with
	| [] -> Lwt.return []
	| (ht, hr)::t ->
		Lwt.return (aux_assoc_list t ht [hr] [])
;;

let add_user ?id ?(confirm=true) fname lname email password address postcode town country phone =
	let c_random = 
		if confirm then Some (random_string 32)
		else None in
	let salt = random_string 8 in
	let c_password = crypt_password password salt in
	get_db () >>= fun dbh -> PGOCaml.begin_work dbh >>=
	fun () -> begin
		match id with
		| None -> begin
				PGSQL(dbh) "INSERT INTO user_ids DEFAULT VALUES" >>=
				fun () -> PGSQL(dbh) "SELECT MAX(id) FROM user_ids" >>=
				function 
				| [Some uid] -> Lwt.return uid
      	| _ -> begin
						PGOCaml.rollback dbh >>=
						fun () -> Lwt.fail_with "User creation did not succeed"
					end
			end
		| Some uid -> begin
				PGSQL(dbh) "DELETE FROM provisional_users WHERE id = $uid" >>=
				fun () -> Lwt.return uid
			end
	end >>=
	fun uid ->	PGSQL(dbh) "INSERT INTO users \
		(id, first_name, last_name, email, password, password_salt, confirmation, \
		address, postcode, town, country, phone_number) \
		VALUES \
		($uid, $fname, $lname, $email, $c_password, $salt, $?c_random, $address, \
		$postcode, $town, $country, $phone)" >>=
	fun () -> PGOCaml.commit dbh >>=
	fun () -> Lwt.return (uid, c_random)
;;

let confirm_user user_id random =
	get_db () >>= fun dbh -> PGSQL(dbh) "SELECT id FROM users \
		WHERE id = $user_id AND confirmation = $random" >>=
	function
	| [] -> Lwt.fail Not_found
	| [i] -> PGSQL(dbh) "UPDATE users SET confirmation = NULL WHERE id = $i" 
	| _ -> Lwt.fail_with "Inconsistency in database"
;;

let add_game title designer =
	get_db () >>= fun dbh -> PGSQL(dbh) "INSERT INTO games \
		(title, designer) VALUES \
		($title, $designer)"
;;

let set_game_data game_id date location visible bookable =
	get_db () >>= fun dbh -> PGSQL(dbh) "UPDATE games \
		SET date = $date, location = $location, visible = $visible, \
			bookable = $bookable \
		WHERE id = $game_id"
;;

let get_users ?(confirmed = true) ?(unconfirmed = false) ?(provisional = false) () =
	get_db () >>= fun dbh -> begin
		if provisional then
			PGSQL(dbh) "SELECT id, first_name, last_name, email, 'P'::char(1) \
			FROM provisional_users"
		else
			Lwt.return [] 
	end	>>=
	fun p_users -> begin
		if unconfirmed then
			PGSQL(dbh) "SELECT id, first_name, last_name, email, 'U'::char(1) \
			FROM users \
			WHERE confirmation IS NOT NULL"
		else
			Lwt.return []
	end >>=
	fun uc_users -> begin
		if confirmed then
			PGSQL(dbh) "SELECT id, first_name, last_name, email, 'C'::char(1) \
			FROM users \
			WHERE confirmation IS NULL"
		else
			Lwt.return []
	end >>=
	fun c_users -> Lwt.return (p_users @ uc_users @ c_users)
;;

let add_provisional_user email game_id =
	get_db () >>= fun dbh -> PGSQL(dbh) "INSERT INTO user_ids DEFAULT VALUES" >>=
	fun () -> PGSQL(dbh) "SELECT MAX(id) FROM user_ids" >>=
	function
	| [Some uid] -> PGSQL(dbh) "INSERT INTO provisional_users (id, email, game_id) \
		VALUES ($uid, $email, $game_id)" >>=
		fun () -> Lwt.return uid
	| _ -> Lwt.fail_with "User creation did not succeed."
;;

let get_provisional_user_data uid =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "SELECT email \
		FROM provisional_users \
		WHERE id = $uid" >>=
	function
	| [] -> fail Not_found
	| [u] -> return u
	| _ -> fail_with "Inconsistency in database"
;;

(* This is far from ideal, as it deletes existing casting -> need to
 * think about this *)
let update_teams game_id teams =
	get_db () >>= fun dbh ->
	PGOCaml.transact dbh (fun dbh ->
		PGSQL(dbh) "DELETE FROM game_casting \
			WHERE game_id = $game_id" >>=
		fun () -> Lwt_list.iter_s (fun (t_name, roles) ->
			Lwt_list.iter_s (fun r_name ->
			PGSQL(dbh) "INSERT INTO game_casting \
				(game_id, team_name, role_name) \
				VALUES \
				($game_id, $t_name, $r_name)"
			) roles
		) teams
	)
;;

let get_user_history user_id =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "SELECT title, date, c.team_name, c.role_name, status, cancelled \
		FROM games JOIN game_casting c ON games.id = c.game_id \
			JOIN game_inscriptions i ON games.id = i.game_id \
		WHERE date IS NOT NULL AND status IN (0, 2) \
		ORDER BY games.date DESC"
;;

let get_confirmation user_id =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "SELECT confirmation \
		FROM users \
		WHERE id = $user_id" >>=
	function
	| [] | [None] -> fail Not_found
	| [Some c] -> Lwt.return c
	| _ -> fail_with "Inconsistency in database"
;;

let cancel_inscription game_id user_id =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "UPDATE game_inscriptions \
		SET cancelled = true \
		WHERE game_id = $game_id AND user_id = $user_id"
;;

let change_status game_id user_id new_status =
	let st_int = int32_of_inscr_status new_status in
	get_db () >>= fun dbh ->
	PGSQL(dbh) "UPDATE game_inscriptions \
		SET status = $st_int \
		WHERE game_id = $game_id AND user_id = $user_id"
;;

let set_game_deadlines game_id inscr cancel pay =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "UPDATE games \
		SET inscription_deadline = $?inscr, cancellation_deadline = $?cancel, \
			payment_deadline = $?pay \
		WHERE id = $game_id"
;;

let get_game_deadlines game_id =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "SELECT inscription_deadline, cancellation_deadline, \
			payment_deadline \
		FROM games \
		WHERE id = $game_id" >>=
	function
	| [] -> fail Not_found
	| [x] -> return x
	| _ -> fail_with "Inconsistent database"
;;

let get_game_visibility game_id =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "SELECT visible, bookable \
		FROM games \
		WHERE id = $game_id" >>=
	function
	| [] -> fail Not_found
	| [x] -> return x
	| _ -> fail_with "Inconsistent database"
;;

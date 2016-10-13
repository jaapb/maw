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

let char_of_inscr_status s =
	match s with
	| `Potential -> "T"
	| `Interested -> "I"
	| `Waiting -> "W"
	| `Confirmed -> "C"
	| `Paid -> "P"
	| `Cancelled -> "N"
	| `No_show -> "X"
;;

let inscr_status_of_char s =
	if s = "I" then `Interested
	else if s = "W" then `Waiting
	else if s = "C" then `Confirmed
	else if s = "P" then `Paid
	else raise (Invalid_argument (Printf.sprintf "Unknown status code: %s" s))
;;

let get_upcoming_games ?no_date () =
	let today = CalendarLib.Date.today () in
	get_db () >>= fun dbh ->
  match no_date with
  | Some true -> PGSQL(dbh) "SELECT id, title, date, location \
    FROM games \
    WHERE date >= $today OR DATE IS NULL \
    ORDER BY date ASC"
  | _ -> PGSQL(dbh) "SELECT id, title, date, location \
		FROM games \
		WHERE date >= $today \
		ORDER BY date ASC"
;;

let get_user_games uid =
	let today = CalendarLib.Date.today () in
	get_db () >>= fun dbh ->
	PGSQL(dbh) 
		"SELECT id, title, date, location, casting_published \
		FROM games JOIN game_inscriptions ON games.id = game_id \
		WHERE date >= $today AND user_id = $uid \
		ORDER BY date ASC";;

let get_designer_games uid =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "SELECT id, title, date, location \
		FROM games \
		WHERE designer = $uid \
		ORDER BY date ASC";;

let get_game_data game_id =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "SELECT title, date, location, name, designer,
		description, min_players, max_players, casting_published \
		FROM games JOIN users ON designer = users.id \
		WHERE games.id = $game_id" >>=
	function
	| [] -> fail Not_found
	| [x] -> return x
	| _ -> fail_with "Inconsistent database"
;;

let check_password name password =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "SELECT id, password, password_salt, name, is_admin \
		FROM users \
		WHERE email = $name AND confirmation IS NULL" >>=
	function
	| [(id, db_password, db_salt, name, is_admin)] ->
		if crypt_password password db_salt = db_password then Lwt.return (Some (id, name, is_admin))
		else Lwt.return None
	| _ -> Lwt.return None (* don't want to fail with Inconsistent database here
	  as it would give away information *)
;;

let is_signed_up uid game_id =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "SELECT game_id \
		FROM games JOIN game_inscriptions ON games.id = game_id \
		WHERE user_id = $uid";;

let set_game_description game_id descr =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "UPDATE games SET description = $descr \
		WHERE id = $game_id";;

let get_nr_inscriptions game_id =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "SELECT COUNT(users.id) \
		FROM users JOIN game_inscriptions ON users.id = user_id \
		WHERE game_id = $game_id" >>=
	function
	| [Some n] -> return (Int64.to_int32 n)
	| _ -> return 0l;;

let get_game_teams game_id =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "SELECT name \
		FROM teams \
		WHERE game_id = $game_id";;

let remove_game_teams game_id teams =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "DELETE FROM teams \
		WHERE game_id = $game_id AND name IN $@teams";;

let add_game_team game_id team =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "INSERT INTO teams (game_id, name) \
		VALUES ($game_id, $team)";;

let get_game_role_types game_id =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "SELECT name \
		FROM role_types \
		WHERE game_id = $game_id";;

let remove_game_role_types game_id role_types =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "DELETE FROM role_types \
		WHERE game_id = $game_id AND name IN $@role_types";;

let add_game_role_type game_id role_type =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "INSERT INTO role_types (game_id, name) \
		VALUES ($game_id, $role_type)";;

let set_game_numbers game_id min max =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "UPDATE games \
		SET min_players = $min, max_players = $max \
		WHERE id = $game_id";;

let change_things dbh game_id uid group_name team role =
	(match team with
	| None -> return ()
	| Some g -> PGSQL(dbh) "UPDATE game_inscriptions \
			SET team_name = $g WHERE game_id = $game_id AND user_id = $uid") >>=
	fun () -> (match role with
	| None -> return ()
	| Some r -> PGSQL(dbh) "UPDATE game_inscriptions \
			SET role_type = $r WHERE game_id = $game_id AND user_id = $uid") >>=
	fun () -> (match group_name with
	| None -> return ()
	| Some g -> PGSQL(dbh) "UPDATE game_inscriptions \
			SET group_name = $g WHERE game_id = $game_id AND user_id = $uid")
;;

let add_inscription game_id uid group_name status team role note =
	let stamp = CalendarLib.Calendar.now () in
	let st = char_of_inscr_status status in
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
	PGSQL(dbh) "SELECT g2.user_id, name, g2.team_name, g2.role_type, g2.note, g2.group_name, g2.status \
		FROM game_inscriptions g1 JOIN game_inscriptions g2 \ 
		ON g1.game_id = g2.game_id AND \
			(g1.user_id = g2.user_id OR g1.group_name = g2.group_name) \
		JOIN users ON g2.user_id = users.id \
		WHERE g1.user_id = $uid AND g1.game_id = $game_id" >>=
	fun l -> Lwt_list.map_p
		(fun (u, nm, tn, rt, nt, gn, s) ->
			Lwt.return (u, nm, tn, rt, nt, gn, inscr_status_of_char s)) l
;;

let get_inscription_list ?(filter_cast = false) game_id =
	get_db () >>= fun dbh ->
	if filter_cast then PGSQL(dbh) "SELECT name, i.user_id, i.team_name, role_type, note, group_name, status \
		FROM game_inscriptions i JOIN users ON user_id = users.id 
			LEFT JOIN game_casting c \
			ON i.user_id = c.user_id AND i.game_id = c.game_id \
		WHERE i.game_id = $game_id AND role_name IS NULL \
		ORDER BY group_name ASC, inscription_time ASC"
	else PGSQL(dbh) "SELECT name, user_id, team_name, role_type, note, group_name, status \
		FROM game_inscriptions JOIN users ON user_id = users.id \
		WHERE game_id = $game_id \
		ORDER BY group_name ASC, inscription_time ASC"
;;

let search_for_user search =
 	let search_expr = Printf.sprintf ".*%s.*" search in
  get_db () >>= fun dbh ->
  PGSQL(dbh) "SELECT id, name, email \
    FROM users \
    WHERE email = $search OR name ~* $search_expr"
;;

let get_group_name game_id uid_list =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "SELECT DISTINCT group_name \
		FROM game_inscriptions \
		WHERE game_id = $game_id AND user_id IN $@uid_list" >>=
	function
	| [] | [None] -> return ""
	| [Some gname] -> return gname
	| _ -> fail (Invalid_argument "Group members in two groups")
;;

let get_user_data uid =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "SELECT name, email \
		FROM users \
		WHERE id = $uid" >>=
	function
	| [] -> fail Not_found
	| [u] -> return u
	| _ -> fail_with "Inconsistency in database"
;;

let update_user_data uid email password =
	let salt = random_string 8 in
	let c_password = crypt_password password salt in
	get_db () >>= fun dbh ->
	PGSQL(dbh) "UPDATE users \
		SET email = $email, password = $c_password, password_salt = $salt \
		WHERE id = $uid"
;;

let get_user_list () =
  get_db () >>= fun dbh ->
  PGSQL(dbh) "SELECT id, name \
    FROM users"
;;

let clear_casting game_id =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "DELETE FROM game_casting \
		WHERE game_id = $game_id"
;;

let add_casting game_id team_name role_name user_id =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "INSERT INTO game_casting \
		(game_id, team_name, role_name, user_id) \
		VALUES ($game_id, $team_name, $role_name, $user_id)"
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
	PGSQL(dbh) "SELECT c.team_name, role_name, name, c.user_id, note, group_name \
		FROM game_casting c JOIN users ON c.user_id = users.id \
		JOIN game_inscriptions i ON i.user_id = c.user_id AND i.game_id = c.game_id \
		WHERE c.game_id = $game_id \
		ORDER BY role_name DESC"
;;

let add_user ?id ?(confirm=true) name email password =
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
		(id, name, email, password, password_salt, confirmation) \
		VALUES \
		($uid, $name, $email, $c_password, $salt, $?c_random)" >>=
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
		(title, designer) VALUES
		($title, $designer)"
;;

let set_game_data game_id date location =
	get_db () >>= fun dbh -> PGSQL(dbh) "UPDATE games \
		SET date = $date, location = $location \
		WHERE id = $game_id"
;;

let get_unconfirmed_users () =
	get_db () >>= fun dbh -> PGSQL(dbh) "SELECT id, name, email, confirmation \
		FROM users \
		WHERE confirmation IS NOT NULL"
;;

let get_confirmed_users () =
	get_db () >>= fun dbh -> PGSQL(dbh) "SELECT id, name, email \
		FROM users \
		WHERE confirmation IS NULL"
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

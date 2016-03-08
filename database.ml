open Lwt

module PGOCaml = PGOCaml_generic.Make(struct include Lwt include Lwt_chan end)

let db_handler = ref None;;

let get_db () =
	match !db_handler with
	| Some h -> return h
	| None -> begin
			PGOCaml.connect ~host:(Sys.getenv "PGHOST")
				~database:(Sys.getenv "PGDATABASE")
				~password:(Sys.getenv "PGPASSWORD") () >>=
			fun dbh -> db_handler := Some dbh; return dbh
		end
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
	fun l -> match l with
	| [] -> fail Not_found
	| [x] -> return x
	| _ -> fail_with "Inconsistent database"
;;

let check_password name password =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "SELECT id, name, is_admin \
		FROM users \
		WHERE username = $name";;

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

let change_things dbh game_id uid group_id team role =
	(match team with
	| None -> return ()
	| Some g -> PGSQL(dbh) "UPDATE game_inscriptions \
			SET team_name = $g WHERE game_id = $game_id AND user_id = $uid") >>=
	fun () -> (match role with
	| None -> return ()
	| Some r -> PGSQL(dbh) "UPDATE game_inscriptions \
			SET role_type = $r WHERE game_id = $game_id AND user_id = $uid") >>=
	fun () -> (match group_id with
	| None -> return ()
	| Some g -> PGSQL(dbh) "UPDATE game_inscriptions \
			SET group_id = $g WHERE game_id = $game_id AND user_id = $uid")
;;

let add_user game_id uid group_id team role note =
	let stamp = CalendarLib.Calendar.now () in
	get_db () >>= fun dbh ->
	PGOCaml.transact dbh (fun dbh ->
		PGSQL(dbh) "SELECT game_id, user_id \
			FROM game_inscriptions \
			WHERE game_id = $game_id AND user_id = $uid" >>=
		(function
		| [] -> PGSQL(dbh) "INSERT INTO game_inscriptions \
		  	(game_id, user_id, inscription_time, note) \
		  	VALUES \
		  	($game_id, $uid, $stamp, $note)"
		| _ -> PGSQL(dbh) "UPDATE game_inscriptions \
				SET note = $note \
				WHERE game_id = $game_id AND user_id = $uid") >>=
	  fun () -> change_things dbh game_id uid group_id team role
	)
;;

let get_inscription_data uid game_id =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "SELECT g2.user_id, name, g2.team_name, g2.role_type, g2.note, g2.group_id \
		FROM game_inscriptions g1 JOIN game_inscriptions g2 \ 
		ON g1.game_id = g2.game_id AND \
			(g1.user_id = g2.user_id OR g1.group_id = g2.group_id) \
		JOIN users ON g2.user_id = users.id \
		WHERE g1.user_id = $uid AND g1.game_id = $game_id" >>=
	function
	| [] -> return (false, [])
	| l -> return (true, l)
;;

let get_inscription_list ?(filter_cast = false) game_id =
	get_db () >>= fun dbh ->
	if filter_cast then PGSQL(dbh) "SELECT name, i.user_id, i.team_name, role_type, note, group_id \
		FROM game_inscriptions i JOIN users ON user_id = users.id 
			LEFT JOIN game_casting c \
			ON i.user_id = c.user_id AND i.game_id = c.game_id \
		WHERE i.game_id = $game_id AND role_name IS NULL \
		ORDER BY group_id ASC, inscription_time ASC"
	else PGSQL(dbh) "SELECT name, user_id, team_name, role_type, note, group_id \
		FROM game_inscriptions JOIN users ON user_id = users.id \
		WHERE game_id = $game_id \
		ORDER BY group_id ASC, inscription_time ASC"
;;

let search_for_user search =
  get_db () >>= fun dbh ->
  PGSQL(dbh) "SELECT id \
    FROM users \
    WHERE username = $search OR email = $search" >>=
	function
	| [] -> fail Not_found
	| [uid] -> return uid
	| _ -> fail_with "Inconsistency in database"
;;

let get_group_id game_id uid_list =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "SELECT DISTINCT group_id \
		FROM game_inscriptions \
		WHERE game_id = $game_id AND user_id IN $@uid_list" >>=
	function
	| [] | [None] -> return None
	| [uid] -> return uid
	| _ -> fail (Invalid_argument "Group members in two groups")
;;

let get_new_group_id game_id =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "SELECT MAX(group_id) \
		FROM game_inscriptions \
		WHERE game_id = $game_id" >>=
	function
	| [] | [None] -> return 1l
	| [Some m] -> return (Int32.add m 1l)
	| _ -> fail_with "This should not happen"
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

let update_user_data uid email =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "UPDATE users \
		SET email = $email \
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
	PGSQL(dbh) "SELECT c.team_name, role_name, name, c.user_id, note, group_id \
		FROM game_casting c JOIN users ON c.user_id = users.id \
		JOIN game_inscriptions i ON i.user_id = c.user_id AND i.game_id = c.game_id \
		WHERE c.game_id = $game_id \
		ORDER BY role_name DESC"
;;

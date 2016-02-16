open Lwt

module PGOCaml = PGOCaml_generic.Make(struct include Lwt include Lwt_chan end)

let db_handler = ref None;;

let get_db () =
	match !db_handler with
	| Some h -> Lwt.return h
	| None -> begin
			PGOCaml.connect ~host:(Sys.getenv "PGHOST")
				~database:(Sys.getenv "PGDATABASE")
				~password:(Sys.getenv "PGPASSWORD") () >>=
			fun dbh -> db_handler := Some dbh; Lwt.return dbh
		end
;;

let get_upcoming_games () =
	let today = CalendarLib.Date.today () in
	get_db () >>= fun dbh ->
	PGSQL(dbh) "SELECT id, title, date, location \
		FROM games \
		WHERE date >= $today \
		ORDER BY date ASC";;

let get_user_games uid =
	let today = CalendarLib.Date.today () in
	get_db () >>= fun dbh ->
	PGSQL(dbh) 
		"SELECT id, title, date, location \
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
		description, min_players, max_players \
		FROM games JOIN users ON designer = users.id \
		WHERE games.id = $game_id" >>=
	fun l -> match l with
	| [] -> Lwt.fail Not_found
	| [x] -> Lwt.return x
	| _ -> Lwt.fail_with "Inconsistent database"
;;

let check_password name password =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "SELECT id, name \
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
	| [Some n] -> Lwt.return (Int64.to_int32 n)
	| _ -> Lwt.return 0l;;

let get_game_groups game_id =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "SELECT name \
		FROM groups \
		WHERE game_id = $game_id";;

let remove_game_groups game_id groups =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "DELETE FROM groups \
		WHERE game_id = $game_id AND name IN $@groups";;

let add_game_group game_id group =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "INSERT INTO groups (game_id, name) \
		VALUES ($game_id, $group)";;

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

let change_things dbh game_id uid group role note =
	(match group with
	| None -> Lwt.return ()
	| Some g -> PGSQL(dbh) "UPDATE game_inscriptions \
			SET group_name = $g WHERE game_id = $game_id AND user_id = $uid") >>=
	fun () -> (match role with
	| None -> Lwt.return ()
	| Some r -> PGSQL(dbh) "UPDATE game_inscriptions \
			SET role_type = $r WHERE game_id = $game_id AND user_id = $uid") >>=
	fun () -> PGSQL(dbh) "UPDATE game_inscriptions \
		SET note = $note WHERE game_id = $game_id AND user_id = $uid";;

let signup game_id uid group role note =
	let stamp = CalendarLib.Calendar.now () in
	get_db () >>= fun dbh ->
	PGOCaml.transact dbh (fun dbh ->
	  PGSQL(dbh) "INSERT INTO game_inscriptions \
		  (game_id, user_id, inscription_time, note) \
		  VALUES \
		  ($game_id, $uid, $stamp, $note)" >>=
	  fun () -> change_things dbh game_id uid group role note
	)
;;

let edit_inscription game_id uid group role note =
	get_db () >>= fun dbh ->
	PGOCaml.transact dbh (fun dbh ->
    change_things dbh game_id uid group role note);;

let add_user game_id search group role note =
  let stamp = CalendarLib.Calendar.now () in
  get_db () >>= fun dbh ->
  PGOCaml.transact dbh (fun dbh ->
    PGSQL(dbh) "INSERT INTO game_inscriptions \
      (game_id, user_id, inscription_time, note) \
      SELECT $game_id, id, $stamp, $note \
      FROM users \
      WHERE username = $search OR email = $search \
      LIMIT 1"
  )
;;

let get_inscription_data uid game_id =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "SELECT g2.user_id, name, g2.group_name, g2.role_type, g2.note, g2.group_id \
		FROM game_inscriptions g1 JOIN game_inscriptions g2 \ 
		ON g1.user_id = g2.user_id OR g1.group_id = g2.group_id 
		JOIN users ON g2.user_id = users.id \
		WHERE g1.user_id = $uid AND g1.game_id = $game_id" >>=
	function
	| [] -> Lwt.return (false, [])
	| l -> Lwt.return (true, l)
;;

let get_inscription_list game_id =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "SELECT name, group_name, role_type, note, group_id \
		FROM game_inscriptions JOIN users ON user_id = users.id \
		WHERE game_id = $game_id \
		ORDER BY inscription_time ASC";;

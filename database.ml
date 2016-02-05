open Lwt

module PGOCaml = PGOCaml_generic.Make(struct include Lwt include Lwt_chan end)

let db_handler = ref None;;

let get_db () =
	match !db_handler with
	| Some h -> Lwt.return h
	| None -> begin
			PGOCaml.connect ~host:"localhost" ~database:"maw" ~password:"TNnz81" () >>=
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
	PGSQL(dbh) "SELECT id, title, date, location, designer = $uid AS designer \
		FROM games LEFT JOIN game_inscriptions ON games.id = game_id \
		WHERE date >= $today AND (user_id = $uid OR designer = $uid)\
		ORDER BY date ASC";;

let get_game_data game_id =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "SELECT title, date, location, name AS des_name, \
		designer AS des_id, description, min_players, max_players \
		FROM games JOIN users ON designer = users.id \
		WHERE games.id = $game_id";;

let check_password name password =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "SELECT id, name \
		FROM users \
		WHERE username = $name";;

let is_signed_up uid game_id =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "SELECT game_id \
		FROM games LEFT JOIN game_inscriptions ON games.id = game_id \
		WHERE user_id = $uid OR designer = $uid";;

let set_game_description game_id descr =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "UPDATE games SET description = $descr \
		WHERE id = $game_id";;

let get_nr_inscriptions game_id =
	get_db () >>= fun dbh ->
	PGSQL(dbh) "SELECT COUNT(users.id) \
		FROM users JOIN game_inscriptions ON users.id = user_id \
		WHERE game_id = $game_id";;

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

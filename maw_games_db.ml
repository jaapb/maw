open Os_db
open Lwt

exception Duplicate_inscription

let get_games () =
	full_transaction_block (fun dbh ->
		PGSQL(dbh) "SELECT id, title, location, date \
			FROM maw.games")

let get_upcoming_games () =
	full_transaction_block (fun dbh ->
		PGSQL(dbh) "SELECT id, title, location, date \
			FROM maw.games \
			WHERE date >= current_date")

let get_designed_games user_id =
	full_transaction_block (fun dbh ->
		PGSQL(dbh) "SELECT id, title \
			FROM maw.games g JOIN maw.game_designers d \
        ON g.id = d.game_id \
      WHERE userid = $user_id")
	
let get_game_info game_id =
	full_transaction_block (fun dbh ->
		PGSQL(dbh) "SELECT title, location, date, blurb \
			FROM maw.games \
			WHERE id = $game_id") >>=
	function
	| [] -> Lwt.fail Not_found
	| [x] -> Lwt.return x
	| _ -> Lwt.fail_with (Printf.sprintf "Multiple games with id %Ld" game_id)

let get_game_designers game_id =
	full_transaction_block (fun dbh ->
		PGSQL(dbh) "SELECT d.userid, firstname, lastname \
			FROM maw.game_designers d JOIN ocsigen_start.users u \
				ON d.userid = u.userid \
			WHERE game_id = $game_id")

let get_inscription game_id user_id =
	full_transaction_block (fun dbh -> PGSQL(dbh) "SELECT message, group_name \
		FROM maw.game_inscriptions \
		WHERE game_id = $game_id AND userid = $user_id AND status = 'A'") >>=
	function
	| [] -> Lwt.fail Not_found
	| [x] -> Lwt.return x
	| _ -> Lwt.fail_with (Printf.sprintf "Multiple inscriptions of user %Ld for game %Ld" user_id game_id)

let sign_up game_id user_id message group =
	full_transaction_block (fun dbh ->
		PGSQL(dbh) "SELECT status FROM maw.game_inscriptions \
			WHERE game_id = $game_id AND userid = $user_id" >>=
		function
		| [] -> PGSQL(dbh) "INSERT INTO maw.game_inscriptions \
				(game_id, userid, message, group_name, status) VALUES \
				($game_id, $user_id, $message, $?group, 'A')"
		| ["A"] -> PGSQL(dbh) "UPDATE maw.game_inscriptions \
				SET message = $message, group_name = $?group \
				WHERE game_id = $game_id AND userid = $user_id"
		| ["C"] -> Lwt.fail Duplicate_inscription
		| [x] -> Lwt.fail_with (Printf.sprintf "Unknown inscription status '%s' in database" x)
		| _ -> Lwt.fail_with (Printf.sprintf "Multiple inscriptions of user %Ld for game %Ld" user_id game_id)
		)

let cancel_inscription game_id user_id =
	full_transaction_block (fun dbh ->
		PGSQL(dbh) "UPDATE maw.game_inscriptions \
			SET status = 'C' \
			WHERE game_id = $game_id AND userid = $user_id")

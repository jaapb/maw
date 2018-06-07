open Os_db
open Lwt

let get_games () =
	full_transaction_block (fun dbh ->
		PGSQL(dbh) "SELECT id, title, location, date \
			FROM maw.games")

let get_upcoming_games () =
	full_transaction_block (fun dbh ->
		PGSQL(dbh) "SELECT id, title, location, date \
			FROM maw.games \
			WHERE date >= current_date")

let get_game_info game_id =
	full_transaction_block (fun dbh ->
		PGSQL(dbh) "SELECT title, location, date, blurb \
			FROM maw.games \
			WHERE id = $game_id") >>=
	function
	| [] -> Lwt.fail Not_found
	| [x] -> Lwt.return x
	| _ -> Lwt.fail_with (Printf.sprintf "Multiple games with id %Ld" game_id)

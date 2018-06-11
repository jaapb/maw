open Os_db
open Lwt

let is_admin userid =
	full_transaction_block (fun dbh ->
		PGSQL(dbh) "SELECT is_admin FROM ocsigen_start.users WHERE userid = $userid") >>=
	function
	| [] -> Lwt.fail Not_found
	| [x] -> Lwt.return x
	| _ -> Lwt.fail_with (Printf.sprintf "Multiple users with id %Ld" userid)

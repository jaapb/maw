open CalendarLib

let dbh = PGOCaml.connect ~host:"localhost" ~database:"maw" ();;

let sod date =
	match date with
	| None -> "an unspecified date (this shouldn't happen!)"
	| Some d -> Printer.Date.sprint "%d %B %Y" d
;;

let find_notifications () =
	PGSQL(dbh) "SELECT email, first_name, last_name, title, date, location \
		FROM users u JOIN game_casting c ON u.id = c.user_id \
		JOIN games g ON g.id = c.game_id \
		WHERE g.date >= current_timestamp - u.notification_before_game \
			AND NOT c.notification_sent"
;;

let _ =
begin
	(* Send out notifications *)
	List.iter (fun (e, f, l, title, date, location) ->
		Mail.send_mail [Printf.sprintf "%s %s" f l, e]	
			(Printf.sprintf "Notification for %s" title)
			(Printf.sprintf "Hello %s,\n
\n
This is your notification for %s, which will take place on %s, \n
at %s.\n
\n
Enjoy the game!\n
\n
Kind regards,\n
\n
  Maw.\n
\n
P.S. This account is not monitored, so please don't reply to this e-mail."
f title (sod date) location)
	) (find_notifications ());
	PGOCaml.close dbh
end;;

open Unix

let mail_server = "smtp.kerguelen.org";;
let port = 25;;

let send_mail to_addrs subject contents =
	let smtp = new Netsmtp.connect (`Socket (`Sock_inet_byname (SOCK_STREAM, mail_server, port), Uq_client.default_connect_options)) 1.0 in
	Netsmtp.authenticate ~host:"marion-dufresne.kerguelen.org" smtp;
	let msg = Netsendmail.compose
		~from_addr:("MAW", "maw@kerguelen.org")
		~to_addrs
		~subject:(Printf.sprintf "[MM] %s" subject)
		contents in
	Netsmtp.sendmail smtp msg
;;

let send_register_mail name email uri =
	send_mail [name, email] "New account confirmation"
	(Printf.sprintf "Hello %s,\n
\n
To confirm your account, please click on the following link:\n
%s\n
\n
Kind regards,\n
\n
	Maw.\n
\n
P.S. This account is not monitored, so please don't reply to this e-mail." name uri) 
;;

let send_simple_inscription_mail name email game_title game_loc game_date game_designer =
	send_mail [name, email] (Printf.sprintf "Inscription for %s" game_title) 
	(Printf.sprintf "Hello,\n
\n
You have (or have been) signed up through the Megagame Makers website for the\n
following game:\n
\n
%s (by %s)\n
\n
This game will be held in %s on %s.\n
\n
You can review your inscription on the Megagame Makers website.\n
\n
Kind regards,\n
\n
  Maw.\n
\n
P.S. This account is not monitored, so please don't reply to this e-mail."
game_title game_designer game_loc game_date)
;;

let send_provisional_inscription_mail uri email game_title game_loc game_date game_designer =
	send_mail ["New User", email] (Printf.sprintf "Inscription for %s" game_title) 
	(Printf.sprintf "Hello,\n
\n
You have (or have been) signed up through the Megagame Makers website for the\n
following game:\n
\n
%s (by %s)\n
\n
This game will be held in %s on %s.\n
\n
As you did not yet have an account, a provisional account has been created for you. You can confirm this account here: %s\n
(If you already have an account or if you wish to cancel this inscription, you can also do that at the link mentioned above.)\n
\n
Kind regards,\n
\n
  Maw.\n
\n
P.S. This account is not monitored, so please don't reply to this e-mail."
game_title game_designer game_loc game_date uri)
;;

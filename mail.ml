open Unix

let mail_server = ref "";;
let mail_port = ref 0;;
let mail_user = ref "";;
let mail_password = ref "";;

let send_mail to_addrs subject contents =
	let smtp = new Netsmtp.connect (`Socket (`Sock_inet_byname (SOCK_STREAM, !mail_server, !mail_port), Uq_client.default_connect_options)) 1.0 in
	let tls_config = Netsys_tls.create_x509_config ~peer_auth:`None
		(Netsys_crypto.current_tls ()) in
	Netsmtp.authenticate ~host:"marion-dufresne.kerguelen.org"
		~sasl_mechs:[ (module Netmech_plain_sasl.PLAIN) ]
		~user:!mail_user
		~creds:[ "password", !mail_password, [] ]
		~tls_required:true
		~tls_config
		smtp;
	let msg = Netsendmail.compose
		~from_addr:("MAW", "maw@kerguelen.org")
		~to_addrs
		~subject:(Printf.sprintf "[MM] %s" subject)
		contents in
	Netsmtp.sendmail smtp msg
;;

let send_register_mail first_name last_name email uri =
	send_mail [Printf.sprintf "%s %s" first_name last_name, email]
	"New account confirmation"
	(Printf.sprintf "Hello %s,\n
\n
To confirm your account, please click on the following link:\n
%s\n
\n
Kind regards,\n
\n
	Maw.\n
\n
P.S. This account is not monitored, so please don't reply to this e-mail." first_name uri) 
;;

let send_simple_inscription_mail fname lname email game_title game_loc game_date dsg_fname dsg_lname =
	send_mail [Printf.sprintf "%s %s" fname lname, email]
	(Printf.sprintf "Inscription for %s" game_title) 
	(Printf.sprintf "Hello %s,\n
\n
You have signed up through the Megagame Makers website for the\n
following game:\n
\n
%s (by %s %s)\n
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
fname game_title dsg_fname dsg_lname game_loc game_date)
;;

let send_provisional_inscription_mail uri fname lname email game_title game_loc game_date dsg_fname dsg_lname =
	send_mail [Printf.sprintf "%s %s" fname lname, email]
	(Printf.sprintf "Inscription for %s" game_title) 
	(Printf.sprintf "Hello %s,\n
\n
You have been signed up through the Megagame Makers website for the\n
following game:\n
\n
%s (by %s %s)\n
\n
This game will be held in %s on %s.\n
\n
As you did not yet have an account, a provisional account has been created for you. You can confirm this account here: %s\n
This link will stay active for 48 hours.\n
(If you already have an account or if you wish to cancel this inscription, you can also do that at the link mentioned above.)\n
\n
Kind regards,\n
\n
  Maw.\n
\n
P.S. This account is not monitored, so please don't reply to this e-mail."
fname game_title dsg_fname dsg_lname game_loc game_date uri)
;;

let send_cancellation_mail fname lname email game_title game_loc game_date =
	send_mail [(Printf.sprintf "%s %s" fname lname), email] (Printf.sprintf "Cancellation for %s" game_title) 
	(Printf.sprintf "Hello %s,\n
\n
Your inscription for the following game has been cancelled:\n
\n
%s (%s, %s)\n
\n
You are not now signed up for this game. If for some reason this cancellation\n
was made in error, please contact Megagame Makers as soon as possible.\n
\n
Kind regards,\n
\n
  Maw.\n
\n
P.S. This account is not monitored, so please don't reply to this e-mail."
fname game_title game_loc game_date)
;;
let _ =
	Nettls_gnutls.init ()
;;

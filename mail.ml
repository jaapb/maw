open Unix

let mail_server = "smtp.mdx.ac.uk";;
let port = 25;;

let send_mail to_addrs subject contents =
	let smtp = new Netsmtp.connect (`Socket (`Sock_inet_byname (SOCK_STREAM, mail_server, port), Uq_client.default_connect_options)) 1.0 in
	Netsmtp.authenticate smtp;
	let msg = Netsendmail.compose
		~from_addr:("Jaap Boender", "J.Boender@mdx.ac.uk")
		~to_addrs
		~subject:(Printf.sprintf "[MM] %s" subject)
		contents in
	Netsmtp.sendmail smtp msg
;;

let send_register_mail to_addrs =
	send_mail to_addrs "New account confirmation"
	"Hello,"

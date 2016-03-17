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

let send_register_mail name email uri =
	send_mail [name, email] "New account confirmation"
	(Printf.sprintf "Hello %s,\n
	\n
	To confirm your account, please click on the following link:\n
	%s\n
	\n
	Kind regards,\n
	\n
	Maw." name uri) 
;;
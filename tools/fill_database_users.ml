open CalendarLib

let dbh = PGOCaml.connect ~host:"localhost" ~database:"maw" ();;

let crypt_password pwd salt =
	Cryptokit.transform_string (Cryptokit.Base64.encode_compact ()) (Cryptokit.hash_string (Cryptokit.Hash.sha3 512) (salt ^ pwd))
;;

let random_string length =
	String.sub (Cryptokit.transform_string (Cryptokit.Base64.encode_compact ()) (Cryptokit.Random.string Cryptokit.Random.secure_rng length)) 0 length
;;

let add_user_id i c =
	PGSQL(dbh) "INSERT INTO user_ids (id, creation_time) \
	VALUES ($i, $?c) ON CONFLICT DO NOTHING"
;;

let add_user i f l e p ps a pn t pc c =
	PGSQL(dbh) "INSERT INTO users (id, first_name, last_name, email, \
		password, password_salt, address, phone_number, town, postcode, country) \
		VALUES \
		($i, $f, $l, $e, $p, $ps, $a, $pn, $t, $pc, $c) \
		ON CONFLICT DO NOTHING"
;;

let _ =
begin
	let megagamers = Csv.of_channel (open_in (Printf.sprintf "%s/Megagamers.csv" Sys.argv.(1))) in
	Csv.iter (fun sl ->
		let id::forename::surname::address1::address2::town::_::country::postcode::telephone::email::_::_::_::mobile::admin::_::_::_::date_added::_::_ = sl in
		let int_id = Int32.of_string id in
		let c_email = if email = "" then Printf.sprintf "%s_%s_NO_EMAIL" (String.lowercase forename) (String.lowercase surname) else email in
		let salt = random_string 8 in
		let c_password = crypt_password telephone salt in
		let address =
			if address2 = ""
			then address1
			else Printf.sprintf "%s %s" address1 address2 in
		let c_date_added =
			if date_added = "" then None
			else Some (Scanf.sscanf date_added "%d/%d/%d %d:%d:%d"
				(fun d m y h min s -> Calendar.make y m d h min s)) in
			add_user_id int_id c_date_added;
			add_user int_id forename surname c_email c_password salt address telephone town postcode country;
			Printf.eprintf "Inserted user %s (%s %s)\n%!" id forename surname
	) megagamers;
	Csv.close_in megagamers;
	PGOCaml.close dbh
end;;

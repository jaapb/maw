[%%shared
  open Eliom_content.Html.F
]

let%server is_admin =
	function
	| None -> Lwt.return false
	| Some userid -> 
		let%lwt is_admin = Maw_users_db.is_admin userid in
		Lwt.return is_admin

let%client is_admin =
	~%(Eliom_client.server_function [%derive.json : int64 option]
			(Os_session.connected_wrapper is_admin))

let%server user_of_userid =
	Os_db.User.user_of_userid

let%client user_of_userid =
	~%(Eliom_client.server_function [%derive.json : int64]
			(Os_session.connected_wrapper user_of_userid))

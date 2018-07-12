[%%shared
  open Eliom_content.Html.D
]

(* Database access *)
let%server is_admin =
	function
	| None -> Lwt.return false
	| Some userid -> 
		let%lwt is_admin = Maw_users_db.is_admin userid in
		Lwt.return is_admin

let%client is_admin =
	~%(Eliom_client.server_function [%derive.json : int64 option]
			(Os_session.connected_wrapper is_admin))

let%server set_admin =
	Maw_users_db.set_admin

let%client set_admin =
	~%(Eliom_client.server_function [%derive.json : int64]
			(Os_session.connected_wrapper set_admin))

let%server user_of_userid =
	Os_db.User.user_of_userid

let%client user_of_userid =
	~%(Eliom_client.server_function [%derive.json : int64]
			(Os_session.connected_wrapper user_of_userid))

let%server get_users pattern =
	Os_user.get_users ~pattern ()

let%client get_users =
	~%(Eliom_client.server_function [%derive.json : string]
			(Os_session.connected_wrapper get_users))

(* User widget *)
let%shared user_input_widget ?nr () =
	let (user_l, user_h) = Eliom_shared.ReactiveData.RList.create [] in
	let user_rows = Eliom_shared.ReactiveData.RList.map 
		[%shared
			((fun (id, fn, ln) -> Eliom_content.Html.(
				D.li ~a:[a_id (Int64.to_string id)] [pcdata fn; pcdata " "; pcdata ln]
			)) : _ -> _)
		]
		user_l in
	let (id_s, id_f) = Eliom_shared.React.S.create 0L in
	let inp_name = match nr with
	| None -> "user_name"
	| Some n -> Printf.sprintf "user_name[%d]" n in
	let inp_id_name = match nr with
	| None -> "user_id"
	| Some n -> Printf.sprintf "user_id[%d]" n in
	let inp = Raw.input ~a:[a_input_type `Text; a_style "width: 15em;"; a_name inp_name; a_autocomplete false; a_placeholder "Start typing a name to search..."] () in
	let inp_id = Eliom_content.Html.(D.Raw.input ~a:[D.a_input_type `Hidden; D.a_name inp_id_name;
		R.a_value (Eliom_shared.React.S.map [%shared Int64.to_string] id_s)] ()) in
	let ddd =	Eliom_content.Html.(D.div ~a:[a_class ["dropdown-content"; "hidden"]] [
		R.ul user_rows
	]) in
	ignore [%client
		((Lwt_js_events.async @@ fun () ->
			let inp = Eliom_content.Html.To_dom.of_input ~%inp in
			Lwt_js_events.inputs inp @@ fun _ _ ->
			let ddd = Eliom_content.Html.To_dom.of_element ~%ddd in
			if (Js.to_string inp##.value) = ""
			then
			begin
				ddd##.classList##add (Js.string "hidden");
				Lwt.return_unit
			end
			else
			begin
				ddd##.classList##remove (Js.string "hidden");
				let%lwt users = get_users (Js.to_string inp##.value) in
				Lwt.return (Eliom_shared.ReactiveData.RList.set ~%user_h (List.map (fun x -> (x.Os_user.userid, x.Os_user.fn, x.Os_user.ln)) users));
			end)
		: unit)
	];
	ignore [%client
		((Lwt_js_events.async @@ fun () ->
		let inp = Eliom_content.Html.To_dom.of_input ~%inp in
		let ddd = Eliom_content.Html.To_dom.of_element ~%ddd in
		Lwt_js_events.clicks ddd @@ fun ev _ ->
		Js.Opt.iter (ev##.target) (fun e ->
			ddd##.classList##add (Js.string "hidden");
			Js.Opt.iter (e##.textContent) (fun t ->
				inp##.value := t
			);
			~%id_f (Int64.of_string (Js.to_string e##.id))
		);
		Lwt.return_unit
		)
		: unit)
	];
	Eliom_content.Html.D.(div ~a:[a_class ["dropdown"]] [
		inp;
		inp_id;
		ddd
	])


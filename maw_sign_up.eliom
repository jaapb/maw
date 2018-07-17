[%%shared
  open Eliom_content.Html.F
	open CalendarLib
	open Maw_utils
]

(* Local actions *)
let%server sign_up_action = Eliom_service.create_attached_post
	~fallback:Maw_services.sign_up_service
	~post_params:(Eliom_parameter.any) ()

let%client sign_up_action = ~%sign_up_action

let%server cancel_inscription_action = Eliom_service.create_attached_post
	~fallback:Maw_services.sign_up_service
	~post_params:(Eliom_parameter.unit) ()

let%client cancel_inscription_action = ~%cancel_inscription_action

(* Handlers *)
let%shared do_sign_up game_id params =
	let multiple_sign_up group_name =
		let id_list = ref [] in
		let message_list = ref [] in
		let%lwt () = Lwt_list.iter_s (fun (n, v) ->
			Ocsigen_messages.console (fun () -> Printf.sprintf "%s = %s" n v);
			(try
				Scanf.sscanf n "user_id[%d]" (fun nr -> id_list := (nr, Int64.of_string v)::!id_list);
			with
				Scanf.Scan_failure _ | End_of_file -> ());
			(try
				Scanf.sscanf n "message[%d]" (fun nr -> message_list := (nr, v)::!message_list);
			with
				Scanf.Scan_failure _ | End_of_file -> ());
			Lwt.return_unit
		) params in
		let merged_list =
			List.map2 (fun (n1, id) (n2, msg) ->
				if n1 = n2 then (id, msg)
				else raise (Invalid_argument (Printf.sprintf "unequal ids (%d, %d)" n1 n2))
			) (List.sort (fun (n1, _) (n2, _) -> compare n1 n2) !id_list)
				(List.sort (fun (n1, _) (n2, _) -> compare n1 n2) !message_list) in
		Lwt_list.iter_s (fun (id, message) ->
			Maw_game.sign_up (game_id, id, message, Some group_name) 
		) merged_list
	in
	Lwt.catch (fun () ->
		Os_session.connected_fun (fun myid game_id params ->
			let message = try 
				List.assoc "message" params
			with
				Not_found -> "" in
			let%lwt () = match List.assoc_opt "group_name" params with
			| None -> Maw_game.sign_up (game_id, myid, message, None)
			| Some g -> 
				let%lwt () = Maw_game.sign_up (game_id, myid, message, Some g) in
				multiple_sign_up g in
			Ocsigen_messages.console (fun () -> Printf.sprintf "data saved");
			Os_msg.msg ~level:`Msg ~onload:true [%i18n S.data_saved];
			Eliom_registration.Redirection.send (Eliom_registration.Redirection Os_services.main_service)
		) game_id params
	)
	(function
	| Maw_games_db.Duplicate_inscription ->
		Ocsigen_messages.console (fun () -> Printf.sprintf "previous inscription");
		Os_msg.msg ~level:`Err ~duration:5.0 ~onload:true
		  "You have a previous inscription for this game which was cancelled. Please contact the Megagames administration.";
		Eliom_registration.Redirection.send (Eliom_registration.Redirection Os_services.main_service)
	| e ->
		Ocsigen_messages.console (fun () -> Printf.sprintf "exception: %s" (Printexc.to_string e));
		Os_msg.msg ~level:`Err ~onload:true (Printexc.to_string e);
		Eliom_registration.Action.send ()
	)

let%shared do_cancel_inscription game_id () =
	Lwt.catch (fun () ->
		Os_session.connected_fun (fun myid game_id () ->
			let%lwt () = Maw_game.cancel_inscription (game_id, myid) in
			Os_msg.msg ~level:`Msg [%i18n S.cancel_inscription];
			Eliom_registration.Redirection.send (Eliom_registration.Redirection Os_services.main_service)	
		) game_id ()
	)
	(function
	| e ->
		Os_msg.msg ~level:`Err ~onload:true (Printexc.to_string e);
		Eliom_registration.Action.send ()
	)

let%shared add_to_group_button nr_s f =
	let btn = Eliom_content.Html.(
		D.button ~a:[D.a_class ["button"]; D.a_button_type `Button] [D.pcdata [%i18n S.add_group_member]]
	) in
	ignore [%client 
		((Lwt.async @@ fun () ->
			let btn = Eliom_content.Html.To_dom.of_element ~%btn in
			Lwt_js_events.clicks btn @@ fun _ _ ->
			let nr = Eliom_shared.React.S.value ~%nr_s in
			let%lwt () = ~%f nr in
			Lwt.return_unit)
		: unit)
	];
	Eliom_content.Html.D.div [btn]

let%shared display_group_table (* nr *) (l, h) =
	let rows = Eliom_shared.ReactiveData.RList.map
		[%shared
				((fun s -> Eliom_content.Html.(
						let (l, h) = (~%l , ~%h) in
						D.tr [
							D.td ~a:[a_class ["no-expand"]] [Maw_user.user_input_widget ~nr:s ()];
							D.td [Raw.input ~a:[a_name (Printf.sprintf "message[%d]" s)] ()];
							D.td ~a:[a_class ["no-expand"]] [Maw_icons.D.close ~a:[a_class ["remove"]; a_title [%i18n S.remove]; a_id (string_of_int s);
								a_onclick [%client fun ev ->
									Js.Opt.iter (ev##.target) (fun e ->
										Eliom_shared.ReactiveData.RList.remove_eq (~%l, ~%h) (int_of_string (Js.to_string (e##.id)))
									)
							  ]] ()] 
						]
				)) : _ -> _)
		]
		l in
	Eliom_content.Html.R.table ~a:[a_class ["signup-table"]] ~thead:(Eliom_shared.React.S.const (thead [tr [th [pcdata [%i18n S.name]];
		th [pcdata [%i18n S.message_for_designer_short]]]])) rows

let%shared real_sign_up_handler myid game_id () = 
	let (group_l, group_h) = Eliom_shared.ReactiveData.RList.create [] in
	let (nr_s, nr_f) = Eliom_shared.React.S.create 0 in
	let (gh_s, gh_f) = Eliom_shared.React.S.create true in
	let%lwt (title, location, date, _) = Maw_game.get_game_info game_id in
	let%lwt inscr = Maw_game.get_inscription_opt (game_id, myid) in
	let%lwt (signed_up, message, group) = match inscr with
	| None -> Lwt.return (false, "", None)
	| Some (m, g) -> Lwt.return (true, m, g) in
	let group_table = display_group_table (* nr_s *) (group_l, group_h) in
	let add_btn = add_to_group_button nr_s
		[%client ((fun v ->
				Eliom_shared.ReactiveData.RList.snoc v ~%group_h;
				~%nr_f (v + 1);
				Lwt.return_unit
			)
			: int -> unit Lwt.t)
		] in 
	let title_bar = Eliom_content.Html.D.div ~a:[a_class ["text-bar"]] [
		table [
			tr [
				td [pcdata [%i18n S.group_inscription]];
				td [Eliom_content.Html.R.node (Eliom_shared.React.S.map
					[%shared ((fun h -> if h
						then Maw_icons.D.expand ~a:[a_title [%i18n S.expand]] ()
						else Maw_icons.D.collapse ~a:[a_title [%i18n S.collapse]] ()
					) : _ -> _)
					] gh_s)
				]
			]
		]] in
	let gn_inp = Eliom_content.Html.D.Raw.input ~a:[a_input_type `Text; a_name "group_name"] () in
	let group_form = Eliom_content.Html.D.div
		~a:[Eliom_content.Html.R.filter_attrib (a_class ["hidden"]) gh_s] [
		table [
			tr [
				th [pcdata [%i18n S.group_name]];
				td [gn_inp]
			]
		];
		group_table;
		add_btn
	] in
	ignore [%client
		((Lwt.async @@ fun () ->
			let title_bar = Eliom_content.Html.To_dom.of_element ~%title_bar in
			Lwt_js_events.clicks title_bar @@ fun _ _ ->
			let () = ~%gh_f (not (Eliom_shared.React.S.value ~%gh_s)) in
			Lwt.return_unit)
		: unit)
	];
	let submit_btn = Eliom_content.Html.D.button
		~a:[a_class ["button"]] [pcdata (if signed_up then [%i18n S.save_changes] else [%i18n S.sign_up])] in
	ignore [%client
		((Lwt.async @@ fun () ->
			let group_form = Eliom_content.Html.To_dom.of_element ~%group_form in
			let btn = Eliom_content.Html.To_dom.of_element ~%submit_btn in
			let gt = Eliom_content.Html.To_dom.of_element ~%group_table in
			let gn_inp = Eliom_content.Html.To_dom.of_element ~%gn_inp in
			let ids = ref [Int64.to_string ~%myid] in
			Lwt_js_events.clicks ~use_capture:true btn @@ fun ev _ ->
				if Js.to_bool (group_form##.classList##contains (Js.string "hidden")) then
					Lwt.return_unit
				else
				begin
				Js.Opt.iter (Dom_html.CoerceTo.input gn_inp) (fun i ->
					if i##.value = (Js.string "") then
					begin
						(Js.Unsafe.coerce gn_inp)##(setCustomValidity [%i18n S.field_not_filled]);
					Dom.preventDefault ev
					end
					else
						(Js.Unsafe.coerce gn_inp)##(setCustomValidity "")
				);
				List.iter (fun tr ->
					if tr##.nodeName = (Js.string "TR") then
					let ntd::mtd::_ = Dom.list_of_nodeList tr##.childNodes in
					let ddd::_ = Dom.list_of_nodeList ntd##.childNodes in
					let name::id::_ = Dom.list_of_nodeList ddd##.childNodes in
					Js.Opt.iter (Dom_html.CoerceTo.element id) (fun e ->
						Js.Opt.iter (Dom_html.CoerceTo.input e) (fun inp ->
							let this_id = Js.to_string inp##.value in
							if this_id = "0" then
							begin
								(Js.Unsafe.coerce name)##(setCustomValidity [%i18n S.field_not_filled]);
								Dom.preventDefault ev
							end
							else if List.mem this_id !ids then
							begin
								(Js.Unsafe.coerce name)##(setCustomValidity [%i18n S.duplicate_user]);
								Dom.preventDefault ev
							end
							else
							begin
								(Js.Unsafe.coerce name)##(setCustomValidity "");
								ids := this_id::!ids
							end
						)	
					)
				) (Dom.list_of_nodeList gt##.childNodes); 
				Lwt.return_unit
			end)
		: unit)
	];
	Maw_container.page (Some myid)
	[
		Form.post_form ~service:sign_up_action (fun () -> [
			div ~a:[a_class ["content-box"]] (
				h1 [pcdata (if signed_up then [%i18n S.edit_inscription_for] else [%i18n S.sign_up_for]); pcdata " "; pcdata title]::
				Maw_game.location_line location date::
				(if signed_up
				then p ~a:[a_class ["edit-inscription"]] [pcdata [%i18n S.edit_inscription_text]]
				else p [pcdata [%i18n S.signup_text]])::
				table ~a:[a_class ["signup-table"]] [
					tr [
						th ~a:[a_class ["no-expand"]] [pcdata [%i18n S.message_for_designer]];
						td [Raw.input ~a:[a_input_type `Text; a_name "message"; a_value message] ()]
					]
				]::
				p [pcdata [%i18n S.message_for_designer_text]]::
				p [pcdata [%i18n S.signup_group_text]]::
				div ~a:[a_class ["group-inscription-box"]] [
					title_bar;
					group_form
				]::
				p [pcdata [%i18n S.signup_warning]]::
				submit_btn::
				(if signed_up
				then [Eliom_content.Html.F.Raw.a ~a:[a_class ["button"; "cancel-inscription"]; a_onclick [%client fun _ ->
					Lwt.async (fun () ->
						Eliom_client.change_page ~service:cancel_inscription_action ~%game_id ()	
					)]] [pcdata [%i18n S.cancel_inscription]]]
				else [])
			)
		]) game_id
	]

let%server sign_up_handler myid_o game_id () =
	Eliom_registration.Any.register ~scope:Eliom_common.default_session_scope
		~service:sign_up_action do_sign_up;
	Eliom_registration.Any.register ~scope:Eliom_common.default_session_scope
		~service:cancel_inscription_action do_cancel_inscription;
	real_sign_up_handler myid_o game_id ()

let%client sign_up_handler =
	real_sign_up_handler

[%%shared
	open CalendarLib
  open Eliom_content.Html.F
	open Maw_utils
]

let%shared format_games_list myid_o =
	let%lwt games = Maw_game.get_games true in
	let%lwt game_rows = Lwt_list.map_s (fun (id, title, location, date) ->
		Lwt.return @@ tr [
			td [pcdata title; pcdata " ("; pcdata (default [%i18n S.tbc] location); pcdata ", ";
				pcdata (match date with None -> [%i18n S.tbc] | Some d -> Printer.Date.sprint "%B %d, %Y" d);
				pcdata ")"];
			td [a ~service:Maw_services.game_info_service [Maw_icons.D.info ~a:[a_title "Game information"] ()] id];
			td (match myid_o with
			| None -> []
			| Some _ -> [a ~service:Maw_services.sign_up_service [Maw_icons.D.signup ~a:[a_title "Sign up"] ()] id]
			)
		]
	) games in
	Lwt.return (table game_rows)

let%shared format_my_games =
	function
	| None -> Lwt.return []
	| Some myid ->
		let%lwt my_games = Maw_game.get_designed_games myid in
			Lwt.return @@ [div ~a:[a_class ["content-box"]] [
				h2 [pcdata [%i18n S.my_games]];
				table (List.map (fun (game_id, title) ->
					tr [
						td [pcdata title];
						td [a ~service:Maw_services.edit_game_service [Maw_icons.D.edit ()] game_id]
					]
				) my_games)
			]]

let%shared dashboard_handler myid_o () () =
	let%lwt is_admin = try%lwt
		Maw_user.is_admin myid_o
	with
	| Not_found -> Lwt.return false in
	let%lwt games_list = format_games_list myid_o in
	let%lwt my_games = format_my_games myid_o in
  Maw_container.page
    ~a:[ a_class ["os-page-main"] ]
    myid_o (
			div ~a:[a_class ["content-box"]] [
				h2 [pcdata [%i18n S.upcoming_games]];
				games_list
			]::
			my_games
		)

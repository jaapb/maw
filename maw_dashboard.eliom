[%%shared
	open CalendarLib
  open Eliom_content.Html.F
	open Maw_utils
]

let%shared format_games_list () =
	let%lwt games = Maw_game.get_games () in
	let%lwt game_rows = Lwt_list.map_s (fun (id, title, location, date) ->
		Lwt.return @@ tr [
			td [pcdata title; pcdata " ("; pcdata (default "TBD" location); pcdata ", "; pcdata (match date with None -> "TBD" | Some d -> Printer.Date.sprint "%D/%M/%Y" d); pcdata ")"];
			td [a ~service:Maw_services.game_info_service [pcdata "info"] id]
		]
	) games in
	Lwt.return (table game_rows)

let%shared dashboard_handler myid_o () () =
	let%lwt is_admin = try%lwt
		Maw_user.is_admin myid_o
	with
	| Not_found -> Lwt.return false in
	let%lwt games_list = format_games_list () in
  Maw_container.page
    ~a:[ a_class ["os-page-main"] ]
    myid_o ([
			div ~a:[a_class ["content-box"]] [
				h2 [pcdata "Upcoming games"];
				games_list
			]
		])

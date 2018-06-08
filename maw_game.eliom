[%%shared
  open Eliom_content.Html.F
	open CalendarLib
	open Maw_utils
]

let%server get_games upcoming =
	if upcoming
	then Maw_games_db.get_upcoming_games ()
	else Maw_games_db.get_games ()

let%client get_games =
	~%(Eliom_client.server_function [%derive.json : bool]
			(Os_session.connected_wrapper get_games))

let%server get_game_info game_id =
	Maw_games_db.get_game_info game_id

let%client get_game_info =
	~%(Eliom_client.server_function [%derive.json : int64]
			(Os_session.connected_wrapper get_game_info))

let%shared game_info_handler myid_o game_id () =
	let%lwt (title, location, date, blurb) = get_game_info game_id in
  Maw_container.page
    ~a:[ a_class ["os-page-main"]]
    myid_o [
			div ~a:[a_class ["content-box"]] [
				h1 [pcdata title];
				p [i [pcdata (default "TBD" location); pcdata ", "; pcdata (match date with None -> "TBD" | Some date -> Printer.Date.sprint "%D/%M/%Y" date)]];
				p [pcdata (default "No game description has yet been entered." blurb)]
			]
    ]

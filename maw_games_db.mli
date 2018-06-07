val get_games : unit -> (int64 * string * string option * CalendarLib.Date.t option) list Lwt.t
val get_upcoming_games : unit -> (int64 * string * string option * CalendarLib.Date.t option) list Lwt.t
val get_game_info : int64 -> (string * string option * CalendarLib.Date.t option * string option) Lwt.t

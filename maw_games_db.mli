val get_games : unit -> (int64 * string * string option * CalendarLib.Date.t option) list Lwt.t
val get_upcoming_games : unit -> (int64 * string * string option * CalendarLib.Date.t option) list Lwt.t
val get_designed_games : int64 -> (int64 * string) list Lwt.t

val get_game_info : int64 -> (string * string option * CalendarLib.Date.t option * string option) Lwt.t

val get_game_designers : int64 -> (int64 * string * string) list Lwt.t

val get_inscription : int64 -> int64 -> string Lwt.t
val sign_up : int64 -> int64 -> string option -> unit Lwt.t

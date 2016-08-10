(* Utility functions *)

let default d o =
	match o with
	| None -> d
	| Some x -> x
;;

let date_or_tbd date =
	match date with
	| None -> "date TBD"
	| Some d -> CalendarLib.Printer.Date.sprint "%d %B %Y" d
;;

let cond_list c h t =
	if c then h::t else t

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
;;

let is_designer uid dsg_ids =
	List.exists (fun (x, _, _) -> uid = x) dsg_ids
;;

let designer_string dsg_ids =
	match dsg_ids with
	| [] -> "nobody"
	| [(_, fn, ln)] -> Printf.sprintf "%s %s" fn ln
	| (_, ffn, lfn)::ds -> Printf.sprintf "%s and %s %s" (String.concat ", " (List.map (fun (_, f, l) -> Printf.sprintf "%s %s" f l) ds)) ffn lfn
;;

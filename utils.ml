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
	List.exists (fun (x, _, _, _) -> uid = x) dsg_ids
;;

let designer_string dsg_ids =
	match dsg_ids with
	| [] -> "nobody"
	| [(_, fn, ln, _)] -> Printf.sprintf "%s %s" fn ln
	| (_, ffn, lfn, _)::ds -> Printf.sprintf "%s and %s %s" (String.concat ", " (List.map (fun (_, f, l, _) -> Printf.sprintf "%s %s" f l) ds)) ffn lfn
;;

let remove_null l =
	let rec remove_null_aux res l =
		match l with
		| [] -> res
		| None::t -> remove_null_aux res t
		| Some x::t -> remove_null_aux (x::res) t in
	remove_null_aux [] l
;;

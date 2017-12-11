[%%shared
	open Eliom_lib
	open Eliom_content
	open Html.D
	open Eliom_service
	open Eliom_parameter
	open Utils
	open Services
]

[%%server
	open CalendarLib
	open Maw
]

let user_select_widget users param =
	match users with
	| [] -> p [pcdata "no users yet"]
	| (id, fn, ln, _, _)::tl -> Form.select	~name:param Form.int32
		(Form.Option ([], id, Some (pcdata (Printf.sprintf "%s %s" fn ln)), false))
		(List.map (fun (tid, tfn, tln, _, _) ->
			Form.Option ([], tid, Some (pcdata (Printf.sprintf "%s %s" tfn tln)),
				false)
		) tl)
;;	

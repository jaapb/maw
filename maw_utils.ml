let default d = function
| None -> d
| Some x -> x
;;

let conditional c h t =
	if c
	then h :: t
	else t
;;
	

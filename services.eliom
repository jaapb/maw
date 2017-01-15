open Eliom_service
open Eliom_parameter

(* Services *)


let account_service = create ~path:(Path ["account"]) ~meth:(Get unit) ();;
let admin_confirm_users_service = create ~path:(Path ["admin_confirm_users"]) ~meth:(Get unit) ();;
let cancel_service = create ~path:(Path ["cancel"]) ~meth:(Get (suffix (int32 "game_id"))) ();;
let cast_service = create ~path:(Path ["cast"]) ~meth:(Get (suffix (int32 "game_id"))) ();;
let confirm_user_service = create ~path:(Path ["confirm"]) ~meth:(Get (suffix (int32 "user_id" ** string "random"))) ();;
let confirm_provisional_user_service = create ~path:(Path ["confirm_provisional"]) ~meth:(Get (suffix (int32 "user_id"))) ();;
let dashboard_service = create ~path:(Path []) ~meth:(Get unit) ();;
let design_service = create ~path:(Path ["design"]) ~meth:(Get (suffix (int32 "game_id"))) ();;
let game_service = create ~path:(Path ["game"]) ~meth:(Get (suffix (int32 "game_id"))) ();;
let login_service = create ~path:No_path
	~meth:(Post (unit, (string "name" ** string "password"))) ();;
let logout_service = create_attached_post
	~fallback:dashboard_service
	~post_params:unit ();;
let message_service = create ~path:(Path ["messaging"]) ~meth:(Get (suffix (int32 "game_id"))) ();;
let new_game_service = create ~path:(Path ["new_game"]) ~meth:(Get unit) ();;
let new_provisional_user_service = create ~path:(Path ["new_provisional_user"]) ~meth:(Get (suffix (int32 "game_id"))) ();;
let register_service = create ~path:(Path ["register"]) ~meth:(Get unit) ();;
let role_service = create ~path:(Path ["role"]) ~meth:(Get (suffix (int32 "game_id"))) ();;
let set_game_data_service = create ~path:(Path ["set_game_data"]) ~meth:(Get unit) ();;
let show_casting_service = create ~path:(Path ["casting"]) ~meth:(Get (suffix (int32 "game_id"))) ();;
let show_inscriptions_service = create ~path:(Path ["inscriptions"]) ~meth:(Get (suffix (int32 "game_id"))) ();;
let signup_service = create ~path:(Path ["signup"]) ~meth:(Get (suffix (int32 "game_id"))) ();;
let user_history_service = create ~path:(Path ["history"]) ~meth:(Get (int32 "user_id")) ();;


(* This file was generated by Ocsigen Start.
   Feel free to use it, modify it, and redistribute it as you wish. *)

let%server about_service =
  Eliom_service.create
    ~path:(Eliom_service.Path ["about"])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    ()

let%server upload_user_avatar_service
  : (unit, unit) Ot_picture_uploader.service =
  Ot_picture_uploader.mk_service
    "upload_user_avatar_service"
    [%derive.json: unit]

let%server settings_service = Eliom_service.create
  ~path:(Eliom_service.Path ["settings"])
  ~meth:(Eliom_service.Get Eliom_parameter.unit)
  ()

let%server os_github_service =
  Eliom_service.extern
    ~prefix:"http://github.com"
    ~path:["ocsigen"; "ocsigen-start"]
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    ()

let%server ocsigen_service =
  Eliom_service.extern
    ~prefix:"http://ocsigen.org"
    ~path:[]
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    ()

let%server game_info_service =
	Eliom_service.create
		~path:(Eliom_service.Path ["game_info"])
		~meth:(Eliom_service.Get Eliom_parameter.(suffix (int64 "game_id")))
		()

let%server edit_game_service =
	Eliom_service.create
		~path:(Eliom_service.Path ["edit_game"])
		~meth:(Eliom_service.Get Eliom_parameter.(suffix (int64 "game_id")))
		()

let%server edit_game_action =
	Eliom_service.create
		~path:(Eliom_service.No_path)
		~meth:(Eliom_service.Post (Eliom_parameter.unit, Eliom_parameter.(int64 "game_id" ** string "blurb"))) ()

let%server admin_service =
	Eliom_service.create
		~path:(Eliom_service.Path ["admin"])
		~meth:(Eliom_service.Get Eliom_parameter.unit)
		()

let%server sign_up_service =
	Eliom_service.create
		~path:(Eliom_service.Path ["sign_up"])
		~meth:(Eliom_service.Get Eliom_parameter.(suffix (int64 "game_id")))
		()

let%client about_service =
  ~%about_service

let%client upload_user_avatar_service =
  ~%upload_user_avatar_service

let%client settings_service =
  ~%settings_service

let%client ocsigen_service =
  ~%ocsigen_service

let%client os_github_service =
  ~%os_github_service

let%client game_info_service =
	~%game_info_service

let%client edit_game_service =
	~%edit_game_service

let%client edit_game_action =
	~%edit_game_action

let%client admin_service =
	~%admin_service

let%client sign_up_service =
	~%sign_up_service

(* The OS lib needs access to the settings service to perform
   redirections to it. We need to register it *)
let%server () = Os_services.register_settings_service settings_service
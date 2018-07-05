(* This file was generated by Ocsigen Start.
   Feel free to use it, modify it, and redistribute it as you wish. *)

let%shared () =
  (* Registering services. Feel free to customize handlers. *)
  Eliom_registration.Action.register
    ~service:Os_services.set_personal_data_service
    Maw_handlers.set_personal_data_handler;

  Eliom_registration.Redirection.register
    ~service:Os_services.set_password_service
    Maw_handlers.set_password_handler;

  Eliom_registration.Action.register
    ~service:Os_services.forgot_password_service
    Maw_handlers.forgot_password_handler;

  Eliom_registration.Action.register
    ~service:Os_services.preregister_service
    Maw_handlers.preregister_handler;

  Eliom_registration.Action.register
    ~service:Os_services.sign_up_service
    Os_handlers.sign_up_handler;

  Eliom_registration.Action.register
    ~service:Os_services.connect_service
    Os_handlers.connect_handler;

  Eliom_registration.Unit.register
    ~service:Os_services.disconnect_service
    (Os_handlers.disconnect_handler ~main_page:true);

  Eliom_registration.Any.register
    ~service:Os_services.action_link_service
    (Os_session.Opt.connected_fun
       Maw_handlers.action_link_handler);

  Eliom_registration.Action.register
    ~service:Os_services.add_email_service
    Os_handlers.add_email_handler;

  Eliom_registration.Action.register
    ~service:Os_services.update_language_service
    Maw_handlers.update_language_handler;

  Maw_base.App.register
    ~service:Os_services.main_service
    (Maw_page.Opt.connected_page Maw_dashboard.dashboard_handler);

  Maw_base.App.register
    ~service:Maw_services.about_service
    (Maw_page.Opt.connected_page Maw_handlers.about_handler);

  Maw_base.App.register
    ~service:Maw_services.settings_service
    (Maw_page.Opt.connected_page Maw_handlers.settings_handler);

	Maw_base.App.register
		~service:Maw_services.game_info_service
		(Maw_page.Opt.connected_page Maw_game.game_info_handler);

	Maw_base.App.register
		~service:Maw_services.edit_game_service
		(Maw_page.connected_page Maw_game.edit_game_handler);

	Maw_base.App.register
		~service:Maw_services.admin_service
		(Maw_page.connected_page Maw_admin.admin_handler);

	Maw_base.App.register
		~service:Maw_services.sign_up_service
		(Maw_page.connected_page Maw_game.sign_up_handler)

let%server () =
  Eliom_registration.Ocaml.register
    ~service:Maw_services.upload_user_avatar_service
    (Os_session.connected_fun Maw_handlers.upload_user_avatar_handler)

let%shared () =
	CalendarLib.Printer.day_name :=
	(function
	| Sun -> [%i18n S.sunday]
	| Mon -> [%i18n S.monday]
	| Tue -> [%i18n S.tuesday]
	| Wed -> [%i18n S.wednesday]
	| Thu -> [%i18n S.thursday]
	| Fri -> [%i18n S.friday]
	| Sat -> [%i18n S.saturday]);
	CalendarLib.Printer.month_name :=
	(function
	| Jan -> [%i18n S.january]
	| Feb -> [%i18n S.february]
	| Mar -> [%i18n S.march]
	| Apr -> [%i18n S.april]
	| May -> [%i18n S.may]
	| Jun -> [%i18n S.june]
	| Jul -> [%i18n S.july]
	| Aug -> [%i18n S.august]
	| Sep -> [%i18n S.september]
	| Oct -> [%i18n S.october]
	| Nov -> [%i18n S.november]
	| Dec -> [%i18n S.december])

(* Print more debugging information when <debugmode/> is in config file
   (DEBUG = yes in Makefile.options).
   Example of use:
   let section = Lwt_log.Section.make "Maw:sectionname"
   ...
   Lwt_log.ign_info ~section "This is an information";
   (or ign_debug, ign_warning, ign_error etc.)
 *)
(* let%server _ =
  if Eliom_config.get_debugmode ()
  then begin
    ignore
      [%client (
        (* Eliom_config.debug_timings := true; *)
        (* Lwt_log_core.add_rule "eliom:client*" Lwt_log.Debug; *)
        (* Lwt_log_core.add_rule "os*" Lwt_log.Debug; *)
        Lwt_log_core.add_rule "Maw*" Lwt_log.Debug
        (* Lwt_log_core.add_rule "*" Lwt_log.Debug *)
        : unit ) ];
    (* Lwt_log_core.add_rule "*" Lwt_log.Debug *)
    Lwt_log_core.add_rule "Maw*" Lwt_log.Debug
  end *)

(* This file was generated by Ocsigen Start.
   Feel free to use it, modify it, and redistribute it as you wish. *)

[%%shared
  open Eliom_content.Html.F
]

let%server css_name = !Maw_config.css_name
let%client css_name = try Js.to_string (Js.Unsafe.global##.___css_name_)
  with _ -> ""

let%server css_name_script =
  [script (cdata_script (Printf.sprintf "var __css_name = '%s';" css_name))]

let%client css_name_script = []

(* Warning: either we use exactly the same global node (and make sure
   global nodes work properly on client side), or we do not add the
   script on client side.  We chose the second solution. *)
let%server app_js = [Maw_base.App.application_script ~defer:true ()]
let%client app_js = []

let%server the_local_js = [
]

let%client the_local_js = [] (* in index.html *)

let%shared the_local_css = [
  [ css_name ]
]

[%%shared
  module Page_config = struct

    include Os_page.Default_config

    let title ="maw"

    let local_js = the_local_js
    let local_css = the_local_css

    let other_head =
      meta ~a:[a_name "viewport";
               a_content "width=device-width, initial-scale=1, user-scalable=no"
              ] ()
      ::css_name_script@app_js

    let default_predicate _ _ = Lwt.return_true

    let default_connected_predicate _ _ _ = Lwt.return_true

    let default_error_page _ _ exn =
      Maw_container.page None
			(match exn with
			| Os_session.Not_connected ->
				[p [pcdata [%i18n S.must_be_connected_to_see_page]]]
			| e -> [p [pcdata (Printexc.to_string e)]]
			)

    let default_connected_error_page myid_o _ _ exn =
      Maw_container.page myid_o
			(match exn with
			| Os_session.Not_connected ->
				[p [pcdata [%i18n S.must_be_connected_to_see_page]]]
			| e -> [p [pcdata (Printexc.to_string e)]]
			)

  end

  include Os_page.Make(Page_config)
]

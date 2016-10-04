let%client _ =
  if Eliom_client.is_client_app ()
  then begin
    let%lwt _ = Lwt_js_events.onload () in
    Eliom_client.change_page ~replace:true
      ~service:Os_services.main_service () ()
  end
  else Lwt.return ()

(*****************************************************************************)
(* COMET RESUMING ON NETWORK RECONNECT *)
(*****************************************************************************)

let%client on_device_ready () =
  let activate ev =
    ignore @@ Dom.addEventListener Dom_html.document ev
      (Dom_html.handler (fun _ -> Eliom_comet.activate (); Js._true)) Js._false
  in
  activate (Dom_html.Event.make "online");
  activate (Dom_html.Event.make "resume");
  MobilePush_push_notification.push_notification ();
  Cordova_statusbar.Background.color_by_hex_string "#049EE0"
    (* FIX: idle mode on offline/pause events? *)

let%client _ =
  Cordova.Event.device_ready (fun _ -> on_device_ready ())

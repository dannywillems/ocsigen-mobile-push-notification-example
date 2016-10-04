let counter = ref 0

let%client sender_id  = MobilePush_config.sender_id
let%server server_key = MobilePush_config.server_key

(* ----------------- *)
(* Register a new ID *)

let registered_id : string list ref = ref []

let save_registered_id id =
  if List.exists (fun x -> x = id) (!registered_id) then Lwt.return ()
  else
  (
    registered_id :=  (id :: (!registered_id));
    List.iter print_endline (!registered_id);
    Lwt.return ()
  )

let rpc_save_registered_id =
  Eliom_client.server_function
    ~name:"MobilePush_push_notification.save_registered_id"
    [%derive.json: string]
    save_registered_id

(* Register a new ID *)
(* ----------------- *)


(* -------------------------------------------------------------------------- *)
(* ---------- Client side ---------- *)

(* Initialization for the push notification service *)
let%client push_notification () =
  let android =
    Cordova_push.Init_options.Android.create
      ~sound:true
      ~icon_color:"red"
      ~vibrate:true
      ~clear_notifications:false
      ~force_show:true
      ~sender_ID:sender_id
      ()
  in
  let ios =
      Cordova_push.Init_options.Ios.create
      ~sender_ID:sender_id
      ~sound:true
      ~gcm_sandbox:true
     ()
  in
  let push = Cordova_push.init @@
    Cordova_push.Init_options.create ~android ~ios ()
  in
  Cordova_push.on_registration push (fun x ->
    let s =
      string_of_int (Cordova_push.Data_registration.registration_id x)
    in
    Js_core.log_string s;
    ignore (~%rpc_save_registered_id s)
  );

  Cordova_push.on_notification push (fun x ->
    let add_data = Cordova_push.Data_notification.additional_data x in
    let id = Cordova_push.Additional_data.not_id add_data in
    Js_core.log_string (Cordova_push.Data_notification.title x);
    Js_core.log_string (Cordova_push.Data_notification.message x);
    if (id = "4")
    then
    (
      let options_inappbrowser = [Cordova_in_app_browser.location true]
      in
      Cordova_in_app_browser.open_
        "https://ocsigen.org"
        Cordova_in_app_browser.System
        (Cordova_in_app_browser.options_list_to_str options_inappbrowser)
    )
  )

(* ---------- Client side ---------- *)
(* -------------------------------------------------------------------------- *)

(* -------------------------------------------------------------------------- *)
(* Test *)

(* Led color *)
let test_send_notification_led_color () =
  let option_ =
    Os_push_notifications.Options.create (!registered_id)
  in
  let notification =
    Os_push_notifications.Notification.empty ()
  |> Os_push_notifications.Notification.add_title "Led"
  |> Os_push_notifications.Notification.add_message "Led color must changed to
  Ocsigen color"
  |> Os_push_notifications.Notification.add_priority
    Os_push_notifications.Notification.Priority.Low
  |> Os_push_notifications.Notification.add_notification_id 3
  |> Os_push_notifications.Notification.add_led_color 0 4 158 224
  |> Os_push_notifications.Notification.add_raw_string "version" "0.2"
  in
  print_endline "Sending notification with led color";
  Os_push_notifications.send server_key notification option_
(* Led color *)

(* LOW PRIORITY *)
let test_send_notification_low () =
  let option_ =
    Os_push_notifications.Options.create (!registered_id)
  in
  let notification =
    Os_push_notifications.Notification.empty ()
  |> Os_push_notifications.Notification.add_title "Low priority"
  |> Os_push_notifications.Notification.add_message "Must be under other
  notifications"
  |> Os_push_notifications.Notification.add_priority
    Os_push_notifications.Notification.Priority.Low
  |> Os_push_notifications.Notification.add_notification_id 1
  in
  print_endline "Sending notification priority low...";
  Os_push_notifications.send server_key notification option_
(* LOW PRIORITY *)

(* MAXIMUM PRIORITY *)
let test_send_notification_max () =
  let action_1 =
    Os_push_notifications.Notification.Action.create
      "" "Action 1" "os_push_action_1" true
  in
  let action_2 =
    Os_push_notifications.Notification.Action.create
      "" "Action 2" "os_push_action_2" true
  in
  let option_ =
    Os_push_notifications.Options.create (!registered_id)
  in
  let notification =
    Os_push_notifications.Notification.empty ()
  |> Os_push_notifications.Notification.add_title "High priority"
  |> Os_push_notifications.Notification.add_message "Must be above other
  notifications"
  |> Os_push_notifications.Notification.add_actions action_1 action_2
  |> Os_push_notifications.Notification.add_soundname "default"
  |> Os_push_notifications.Notification.add_notification_id 2
  |> Os_push_notifications.Notification.add_priority
    Os_push_notifications.Notification.Priority.Maximum
  in
  print_endline "Sending notification...";
  Os_push_notifications.send server_key notification option_
(* MAXIMUM PRIORITY *)

(* REDIRECTION *)
let test_send_notification_redirection () =
  let option_ =
    Os_push_notifications.Options.create (!registered_id)
  in
  let notification =
    Os_push_notifications.Notification.empty ()
  |> Os_push_notifications.Notification.add_title "Redirection"
  |> Os_push_notifications.Notification.add_message "Redirection to Ocsigen.org"
  |> Os_push_notifications.Notification.add_soundname "default"
  |> Os_push_notifications.Notification.add_notification_id 4
  in
  print_endline "Sending notification for redirection";
  Os_push_notifications.send server_key notification option_

(* REDIRECTION *)
let rpc_test_send_notification_led_color =
  Eliom_client.server_function ~name:"test send notification with led color"
    [%derive.json: unit]
    test_send_notification_led_color

let rpc_test_send_notification_max =
  Eliom_client.server_function ~name:"test send notification max"
    [%derive.json: unit]
    test_send_notification_max

let rpc_test_send_notification_low =
  Eliom_client.server_function ~name:"test send notification low"
    [%derive.json: unit]
    test_send_notification_low

let rpc_test_send_notification_redirection =
  Eliom_client.server_function ~name:"test send notification redirection"
    [%derive.json: unit]
    test_send_notification_redirection

let%client _ =
  Js.Unsafe.set
    Js.Unsafe.global
      "os_push_action_1"
      (fun () -> Js_core.log_string "Action 1");
  Js.Unsafe.set
    Js.Unsafe.global
      "os_push_action_2"
      (fun () -> Js_core.log_string "Action 2")
(* Test *)
(* -------------------------------------------------------------------------- *)

let counter = ref 0

let server_key = MobilePush_config.server_key

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
      ~sender_ID:"138084667561"
      ()
  in
  let ios =
      Cordova_push.Init_options.Ios.create
      ~sender_ID:"138084667561"
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
    Js_core.log_string (Cordova_push.Data_notification.title x);
    Js_core.log_string (Cordova_push.Data_notification.message x);
  )

(* ---------- Client side ---------- *)
(* -------------------------------------------------------------------------- *)

(* -------------------------------------------------------------------------- *)
(* Test *)

(* LOW PRIORITY *)
let test_send_notification_low () =
  let option_ =
    Os_push_notifications.Options.create (!registered_id)
  |> Os_push_notifications.Options.add_collapse_key "do_not_collapse"
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
      "" "Action 1" "none" true
  in
  let action_2 =
    Os_push_notifications.Notification.Action.create
      "" "Action 2" "none" true
  in
  let option_ =
    Os_push_notifications.Options.create (!registered_id)
  |> Os_push_notifications.Options.add_collapse_key "do_not_collapse"
  in
  let notification =
    Os_push_notifications.Notification.empty ()
  |> Os_push_notifications.Notification.add_title "High priority"
  |> Os_push_notifications.Notification.add_message "Must be above other
  notifications"
  (*|> Os_push_notifications.Notification.add_actions action_1 action_2 t*)
  |> Os_push_notifications.Notification.add_soundname "default"
  |> Os_push_notifications.Notification.add_notification_id 2
  |> Os_push_notifications.Notification.add_priority
    Os_push_notifications.Notification.Priority.Maximum
  in
  print_endline "Sending notification...";
  Os_push_notifications.send server_key notification option_
(* MAXIMUM PRIORITY *)

let rpc_test_send_notification_max : (_, unit) Eliom_client.server_function =
  Eliom_client.server_function ~name:"test send notification max"
    [%derive.json: unit]
    test_send_notification_max

let rpc_test_send_notification_low : (_, unit) Eliom_client.server_function =
  Eliom_client.server_function ~name:"test send notification low"
    [%derive.json: unit]
    test_send_notification_low

(* Test *)
(* -------------------------------------------------------------------------- *)

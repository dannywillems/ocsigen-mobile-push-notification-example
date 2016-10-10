let counter = ref 0

let%client sender_id  = MobilePush_config.sender_id
let%server server_key = MobilePush_config.server_key

(* ----------------- *)
(* Register a new ID *)

let%server registered_id : string list ref = ref []

let%server save_registered_id id =
  if List.exists (fun x -> x = id) (!registered_id) then Lwt.return ()
  else
  (
    registered_id :=  (id :: (!registered_id));
    List.iter print_endline (!registered_id);
    Lwt.return ()
  )

let%server rpc_save_registered_id =
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
    let s = Cordova_push.Data_registration.registration_id x in
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
(* Debug print function *)

let%server print_results results =
  let open Os_push_notifications.Response in
  match results with
  | Results.Success s ->
    Printf.printf "\tMessage ID: %s\n" (Results.message_id_of_success s);
    (match Results.registration_id_of_success s with
    | None -> print_endline "\tRegistration ID's: None\n"
    | Some x -> Printf.printf "\tRegistration ID's: %s\n" x)
  | Results.Error err ->
    print_endline ("\tError: " ^ (Results.string_of_error err) ^ "\n")

let%server print_response response =
  let open Os_push_notifications in
  Printf.printf "Multicast ID: %s\n" (Response.multicast_id_of_t response);
  Printf.printf "Success: %d\n" (Response.success_of_t response);
  Printf.printf "Failure: %d\n" (Response.failure_of_t response);
  Printf.printf "Canonical ids: %d\n" (Response.canonical_ids_of_t response);
  List.iter print_results (Response.results_of_t response)
(* -------------------------------------------------------------------------- *)

(* -------------------------------------------------------------------------- *)
(* Test *)

let%server test_send_notification txt notification opt =
  try%lwt
    let () = print_endline txt in
    let%lwt response =
      Os_push_notifications.send server_key notification opt
    in
    print_endline "Notification sent";
    print_response response;
    Lwt.return ()
  with
  | Os_push_notifications.FCM_missing_field x ->
      Lwt_log.log ~level:Lwt_log.Error x
  | Os_push_notifications.FCM_empty_response ->
      Lwt_log.log ~level:Lwt_log.Error "Empty response"
  | Os_push_notifications.FCM_no_json_response str ->
      Lwt_log.log ~level:Lwt_log.Error ("No JSON response: " ^ str)
  | Os_push_notifications.FCM_unauthorized ->
      Lwt_log.log ~level:Lwt_log.Error "You're unauthorized. Check your server \
      key and if you switched to FCM."
  | _ -> Lwt_log.log ~level:Lwt_log.Error "Must never be the case..."


(* Led color *)
let%server test_send_notification_led_color () =
  if (!registered_id) <> [] then
    let opt =
      Os_push_notifications.Options.create (!registered_id)
    in
    let notification =
      Os_push_notifications.Notification.empty ()
    |> Os_push_notifications.Notification.add_title "Led"
    |> Os_push_notifications.Notification.add_message "Led color must changed \
    to Ocsigen color"
    |> Os_push_notifications.Notification.add_priority
      Os_push_notifications.Notification.Priority.Low
    |> Os_push_notifications.Notification.add_notification_id 0
    |> Os_push_notifications.Notification.add_led_color 0 4 158 224
    |> Os_push_notifications.Notification.add_raw_string "version" "0.2"
    in
    test_send_notification
      "Sending notification with led color"
      notification
      opt
  else Lwt.return ()
(* Led color *)

(* LOW PRIORITY *)
let%server test_send_notification_low () =
  if (!registered_id) <> [] then
    let opt =
      Os_push_notifications.Options.create (!registered_id)
    in
    let notification =
      Os_push_notifications.Notification.empty ()
    |> Os_push_notifications.Notification.add_title "Low priority"
    |> Os_push_notifications.Notification.add_message "Must be under other \
    notifications"
    |> Os_push_notifications.Notification.add_priority
      Os_push_notifications.Notification.Priority.Low
    |> Os_push_notifications.Notification.add_notification_id 1
    in
    test_send_notification
      "Sending notification with low priority..."
      notification
      opt
  else
  (
      print_endline "No regitered ID";
      Lwt.return ()
  )
(* LOW PRIORITY *)

(* MAXIMUM PRIORITY *)
let%server test_send_notification_max () =
  if (!registered_id) <> [] then
    let opt =
      Os_push_notifications.Options.create (!registered_id)
    in
    let notification =
      Os_push_notifications.Notification.empty ()
    |> Os_push_notifications.Notification.add_title "High priority"
    |> Os_push_notifications.Notification.add_message "Must be above other \
    notifications"
    |> Os_push_notifications.Notification.add_soundname "default"
    |> Os_push_notifications.Notification.add_notification_id 2
    |> Os_push_notifications.Notification.add_priority
      Os_push_notifications.Notification.Priority.Maximum
    in
    test_send_notification
      "Sending notification with high priority..."
      notification
      opt
  else
  (
      print_endline "No regitered ID";
      Lwt.return ()
  )

(* MAXIMUM PRIORITY *)

(* Export functions used by the notification *)
let%client _ =
  Js.Unsafe.set
    Js.Unsafe.global
      "os_push_action_1"
      (fun () -> Js_core.log_string "Action 1");
  Js.Unsafe.set
    Js.Unsafe.global
      "os_push_action_2"
      (fun () -> Js_core.log_string "Action 2")

(* ACTIONS *)
let%server test_send_notification_action () =
  if (!registered_id) <> [] then
    let action_1 =
      Os_push_notifications.Notification.Action.create
        "" "Action 1" "os_push_action_1" true
    in
    let action_2 =
      Os_push_notifications.Notification.Action.create
        "" "Action 2" "os_push_action_2" true
    in
    let opt =
      Os_push_notifications.Options.create (!registered_id)
    in
    let notification =
      Os_push_notifications.Notification.empty ()
    |> Os_push_notifications.Notification.add_title "Actions"
    |> Os_push_notifications.Notification.add_message "Look into the console of \
    the application after chosen the actions."
    |> Os_push_notifications.Notification.add_actions action_1 action_2
    |> Os_push_notifications.Notification.add_soundname "default"
    |> Os_push_notifications.Notification.add_notification_id 3
    in
    test_send_notification
      "Sending notification with actions..."
      notification
      opt
  else
  (
      print_endline "No regitered ID";
      Lwt.return ()
  )
(* ACTIONS *)


(* REDIRECTION *)
let%server test_send_notification_redirection () =
  if (!registered_id) <> [] then
    let opt =
      Os_push_notifications.Options.create (!registered_id)
    in
    let notification =
      Os_push_notifications.Notification.empty ()
    |> Os_push_notifications.Notification.add_title "Redirection"
    |> Os_push_notifications.Notification.add_message "Redirection to Ocsigen.org"
    |> Os_push_notifications.Notification.add_soundname "default"
    |> Os_push_notifications.Notification.add_notification_id 4
    in
    test_send_notification
      "Sending notification for redirection"
      notification
      opt
  else
  (
      print_endline "No regitered ID";
      Lwt.return ()
  )
(* REDIRECTION *)

(* REDIRECTION *)
let%server test_send_notification_bad_registered_id () =
  if (!registered_id) <> [] then
    let opt =
      Os_push_notifications.Options.create ("gsdfg" :: (!registered_id))
    in
    let notification =
      Os_push_notifications.Notification.empty ()
    |> Os_push_notifications.Notification.add_title "No sent"
    |> Os_push_notifications.Notification.add_message "No sent"
    |> Os_push_notifications.Notification.add_notification_id 5
    in
    test_send_notification
      "Sending notification to a bad ID. Must fail with an error."
      notification
      opt
  else
  (
      print_endline "No regitered ID";
      Lwt.return ()
  )
(* REDIRECTION *)

(* RPC *)
let%server rpc_test_send_notification_led_color =
  Eliom_client.server_function ~name:"test send notification with led color"
    [%derive.json: unit]
    test_send_notification_led_color

let%server rpc_test_send_notification_max =
  Eliom_client.server_function ~name:"test send notification max"
    [%derive.json: unit]
    test_send_notification_max

let%server rpc_test_send_notification_action =
  Eliom_client.server_function ~name:"test send notification action"
    [%derive.json: unit]
    test_send_notification_action

let%server rpc_test_send_notification_low =
  Eliom_client.server_function ~name:"test send notification low"
    [%derive.json: unit]
    test_send_notification_low

let%server rpc_test_send_notification_redirection =
  Eliom_client.server_function ~name:"test send notification redirection"
    [%derive.json: unit]
    test_send_notification_redirection

let%server rpc_test_send_notification_bad_registered_id =
  Eliom_client.server_function ~name:"test send notification bad registered id"
    [%derive.json: unit]
    test_send_notification_bad_registered_id
(* RPC *)

(* Test *)
(* -------------------------------------------------------------------------- *)

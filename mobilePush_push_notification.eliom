let counter = ref 0

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
  Cordova_fcm.get_token
    (fun token -> ignore (~%rpc_save_registered_id token))
    (fun error -> Js_core.log_string error);
  Cordova_fcm.on_notification
    (fun data ->
      Js_core.log_any data;
      if (Cordova_fcm.Data.was_tapped data)
      then
      (
        Js_core.log_string "Notification tapped";
        Js_core.log_any data
      )
      else Js_core.log_string "Notification not tapped"
    )
    (fun message -> Js_core.log_string
      ("Callback successfully registered: " ^ message)
    )
    (fun error -> Js_core.log_string
      ("Callback error registered: " ^ error)
    );
  ()

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

let%server test_send_notification txt notification ?data opt =
  try%lwt
    let () = print_endline txt in
    let%lwt response = match data with
    | None -> Os_push_notifications.send server_key notification opt
    | Some x -> Os_push_notifications.send server_key notification ~data:x opt
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

(* HIGH PRIORITY *)
let%server test_send_notification_high () =
  if (!registered_id) <> [] then
    let opt =
      Os_push_notifications.Options.create (!registered_id)
    |> Os_push_notifications.Options.add_collapse_key "high_priority"
    |> Os_push_notifications.Options.add_priority
       Os_push_notifications.Options.Priority.High
    in
    let notification =
      Os_push_notifications.Notification.empty ()
    |> Os_push_notifications.Notification.add_click_action "FCM_PLUGIN_ACTIVITY"
    |> Os_push_notifications.Notification.add_title "High priority"
    |> Os_push_notifications.Notification.Android.add_tag "high_priority"
    |> Os_push_notifications.Notification.add_body "Must be above other \
    notifications"
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
(* HIGH PRIORITY *)

(* NORMAL PRIORITY *)
let%server test_send_notification_normal () =
  if (!registered_id) <> [] then
    let opt =
      Os_push_notifications.Options.create (!registered_id)
    |> Os_push_notifications.Options.add_collapse_key "normal_priority"
    |> Os_push_notifications.Options.add_priority
       Os_push_notifications.Options.Priority.Normal
    in
    let notification =
      Os_push_notifications.Notification.empty ()
    |> Os_push_notifications.Notification.add_click_action "FCM_PLUGIN_ACTIVITY"
    |> Os_push_notifications.Notification.add_title "Normal priority"
    |> Os_push_notifications.Notification.Android.add_tag "normal_priority"
    |> Os_push_notifications.Notification.add_body "Must be under other \
    notifications"
    in
    test_send_notification
      "Sending notification with normal priority..."
      notification
      opt
  else
  (
      print_endline "No regitered ID";
      Lwt.return ()
  )
(* NORMAL PRIORITY *)

(* REDIRECTION *)
let%server test_send_notification_redirection () =
  if (!registered_id) <> [] then
    let opt =
      Os_push_notifications.Options.create (!registered_id)
    |> Os_push_notifications.Options.add_collapse_key "redirection"
    in
    let notification =
      Os_push_notifications.Notification.empty ()
    |> Os_push_notifications.Notification.add_click_action "FCM_PLUGIN_ACTIVITY"
    |> Os_push_notifications.Notification.add_title "Redirection"
    |> Os_push_notifications.Notification.add_body "Redirection to Ocsigen.org"
    |> Os_push_notifications.Notification.Android.add_tag "redirection"
    in
    let data =
      Os_push_notifications.Data.empty ()
    |> Os_push_notifications.Data.add_raw_string "type" "redirection"
    |> Os_push_notifications.Data.add_raw_string "url" "https://ocsigen.org"
    in
    test_send_notification
      "Sending notification for redirection"
      notification
      ~data
      opt
  else
  (
      print_endline "No regitered ID";
      Lwt.return ()
  )
(* REDIRECTION *)

(* BAD REGISTERED ID *)
let%server test_send_notification_bad_registered_id () =
  if (!registered_id) <> [] then
    let opt =
      Os_push_notifications.Options.create ["gsdfg"]
    in
    let notification =
      Os_push_notifications.Notification.empty ()
    |> Os_push_notifications.Notification.add_click_action "FCM_PLUGIN_ACTIVITY"
    |> Os_push_notifications.Notification.add_title "No sent"
    |> Os_push_notifications.Notification.add_body "No sent"
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
(* BAD REGISTERED ID *)

(* RPC *)
let%server rpc_test_send_notification_high =
  Eliom_client.server_function ~name:"test send notification high"
    [%derive.json: unit]
    test_send_notification_high

let%server rpc_test_send_notification_normal =
  Eliom_client.server_function ~name:"test send notification normal"
    [%derive.json: unit]
    test_send_notification_normal

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

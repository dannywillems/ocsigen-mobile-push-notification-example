module B = Bootstrap

let paragraph = B.p "hello world"
let button =
  Eliom_content.Html.D.(button
    ~a:
    [
      a_class ["btn" ; "btn-primary"] ;
      a_onclick [%client fun _ -> ignore (
        ~%MobilePush_push_notification.rpc_test_send_notification_low ())]
    ]
    [pcdata "Low priority"]
  )

let button2 =
  Eliom_content.Html.D.(button
    ~a:
    [
      a_class ["btn" ; "btn-primary"] ;
      a_onclick [%client fun _ -> ignore (
        ~%MobilePush_push_notification.rpc_test_send_notification_max ())]
    ]
    [pcdata "High priority"]
  )

[%%shared
  let main_service_handler () () =
    Lwt.return @@ [~%paragraph ; ~%button ; ~%button2]
]



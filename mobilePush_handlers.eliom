module B = Bootstrap

let create_button text fn =
  Eliom_content.Html.D.(button
    ~a:
    [
      a_class ["btn" ; "btn-primary" ; "margin"] ;
      a_onclick [%client fun _ -> ignore (~%fn ())]
    ]
    [pcdata text]
  )

let low_button = create_button
  "Low priority"
  (MobilePush_push_notification.rpc_test_send_notification_low)

let high_button = create_button
  "high priority"
  (MobilePush_push_notification.rpc_test_send_notification_max)

let led_color_button = create_button
  "Led color Ocsigen"
  (MobilePush_push_notification.rpc_test_send_notification_led_color)

let ocsigen_button = create_button
  "Go to ocsigen server"
  (MobilePush_push_notification.rpc_test_send_notification_redirection)

let container children =
  B.container
    ~css:["text-center"]
    ~children:
    [
      B.row
        ~children:
        (List.map
          (fun x -> B.col ~lg:12 ~children:[x] ())
          children
        )
        ()
    ]
    ()

let main_container =
  container
  [
    high_button ; low_button ; led_color_button ; ocsigen_button
  ]

[%%shared
  let main_service_handler () () =
    Lwt.return @@ [~%main_container]
]

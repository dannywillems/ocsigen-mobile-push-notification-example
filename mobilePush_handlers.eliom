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

let normal_button = create_button
  "Low priority"
  (MobilePush_push_notification.rpc_test_send_notification_normal)

let high_button = create_button
  "High priority"
  (MobilePush_push_notification.rpc_test_send_notification_high)

let ocsigen_button = create_button
  "Go to ocsigen website"
  (MobilePush_push_notification.rpc_test_send_notification_redirection)

let bad_registered_id_button = create_button
  "Bad registered ID, produces nothing"
  (MobilePush_push_notification.rpc_test_send_notification_bad_registered_id)


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
    high_button ; normal_button ; ocsigen_button ; bad_registered_id_button
  ]

[%%shared
  let main_service_handler () () =
    Lwt.return @@ [~%main_container]
]

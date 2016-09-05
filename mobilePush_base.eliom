let%server application_name = !MobilePush_config.app_name

let%client application_name = Eliom_client.get_application_name ()

let%shared displayed_app_name = "test_app2"

let () =
  Os_db.init
    ?host:!MobilePush_config.os_db_host
    ?port:!MobilePush_config.os_db_port
    ?user:!MobilePush_config.os_db_user
    ?password:!MobilePush_config.os_db_password
    ?database:!MobilePush_config.os_db_database
    ?unix_domain_socket_dir:!MobilePush_config.os_db_unix_domain_socket_dir
    ()

let () = Os_email.set_mailer "/usr/sbin/sendmail"

[%%shared
module App = Eliom_registration.App(struct
    let application_name = application_name
    let global_data_path = Some ["__global_data__"]
  end)
]

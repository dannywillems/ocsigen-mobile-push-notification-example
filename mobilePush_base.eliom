let%server application_name = !MobilePush_config.app_name

let%client application_name = Eliom_client.get_application_name ()

let%shared displayed_app_name = "mobilePush"

[%%shared
module App = Eliom_registration.App(struct
    let application_name = application_name
    let global_data_path = Some ["__global_data__"]
  end)
]

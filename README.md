Instructions
============

This Eliom project is an example for mobile push notifications using Eliom and
the binding to the plugin phonegap-plugin-push: https://github.com/dannywillems/ocaml-cordova-plugin-push.

It uses as basis the ocsigen-start template but useless services and modules for
this example has been removed.

Compile and run the application:
================================

You need to install [ocsigen-start](https://github.com/ocsigen/ocsigen-start), [cordova](https://github.com/dannywillems/ocaml-cordova), [cordova-plugin-statusbar](https://github.com/dannywillems/ocaml-cordova-plugin-statusbar), [cordova-plugin-push](https://github.com/dannywillems/ocaml-cordova-plugin-push), [gen_js_api](https://github.com/lexifi/gen_js_api), [ocaml-js-stdlib](https://github.com/dannywillems/ocaml-js-stdlib) and [yojson](http://mjambon.com/yojson.html)

```
opam pin add ocaml-js-stdlib https://github.com/dannywillems/ocaml-js-stdlib.git
opam install yojson ocsigen-start cordova cordova-plugin-statusbar cordova-plugin-push-notifications
```

**NB**: See how to install `ocsigen-start` first. It must be pinned for the
moment.

Generated files
---------------

#### Not in common with the ocsigen-start template

- Makefile.local
  Replace the APP_SERVER value with your IP and port.

- mobilePush_push_notification.eliom
  Contains client-side code for the plugin initialization. It request a
  register ID to GCM and registers it on the server with a RPC.

  Contains also test functions which create different notifications with a RPC
  to call it client-side (only for testing, normally it's called server-side).

  Don't forget to change the `server_key` (generated by the GCM API) and the
  `sender_id` (the project number in GCM).

- mobilePush.eliom
  Register the main service. It's a simple page with buttons to send
  notifications. The shared section is mandatory for mobile applications.

- mobilePush_handlers.eliom
  Main handler generating the main page.

- mobilePush_mobile.eliom
  Changes the statusbar color.

- mobilePush_page.eliom
  Comes from the `ocsigen-start` template.

#### Common with the ocsigen-start template

- mobilePush*.eliom[i]
  Initial source file of the project.
  All Eliom files (*.eliom, *.eliomi) in this directory are
  automatically compiled and included in the application.
  To add a .ml/.mli file to your project,
  append it to the variable SERVER_FILES or CLIENT_FILES in Makefile.options.

- static/
  This folder contains the static data for your Website.
  The content will be copied into the static file directory of the server.
  Put your CSS or additional JavaScript files here.

- Makefile.options
  Configure your project here.

- mobilePush.conf.in
  This file is a template for the configuration file for
  ocsigenserver. You will rarely have to edit itself - it takes its
  variables from the Makefile.options. This way, the installation
  rules and the configuration files are synchronized with respect to
  the different folders.

- mobile
  The files needed by Cordova mobile apps

- Makefile
  This contains all rules necessary to build, test, and run your
  Eliom application. See below for the relevant targets.

- README

Makefile targets
----------------

Here's some help on how to work with this basic distillery project:

 - Test your application by compiling it and running ocsigenserver locally
     $ make test.byte (or test.opt)

 - Compile it only
     $ make all (or byte or opt)

 - Deploy your project on your system
     $ sudo make install (or install.byte or install.opt)

 - Run the server on the deployed project
     $ sudo make run.byte (or run.opt)

   If WWWUSER in the Makefile.options is you, you don't need the
   `sudo'. If Eliom isn't installed globally, however, you need to
   re-export some environment variables to make this work:
     $ sudo PATH=$PATH OCAMLPATH=$OCAMLPATH LD_LIBRARY_PATH=$LD_LIBRARY_PATH make run.byte/run.opt

 - If you need a findlib package in your project, add it to the
   variables SERVER_PACKAGES and/or CLIENT_PACKAGES. The configuration
   file will be automatically updated.

 - Mobile app: Have a look in Makefile.mobile to see how to install
   the requested packages to build a mobile application,
   and the makefile rules to build and test it.

Use Camlp4 syntax
-----------------

The project is configured for using the PPX Eliom syntax. If you need
to use Camlp4, you need to provide custom targets. For example, to use
Camlp4 for the file foo_p4.eliom, add the following targets to the end
of Makefile.local:

  $(DEPSDIR)/foo_p4.eliom.server: foo_p4.eliom | $(DEPSDIR)
          $(ELIOMDEP) -server $(SERVER_INC) $< > $@

  $(DEPSDIR)/foo_p4.eliom.client: foo_p4.eliom | $(DEPSDIR)
          $(ELIOMDEP) -client $(CLIENT_INC) $< > $@

  ${ELIOM_TYPE_DIR}/foo_p4.type_mli: foo_p4.eliom
          ${ELIOMC} -infer ${SERVER_INC} $<

  ${ELIOM_SERVER_DIR}/foo_p4.cmo: foo_p4.eliom
          ${ELIOMC} -c ${SERVER_INC} $(GENERATE_DEBUG) $<

  ${ELIOM_SERVER_DIR}/foo_p4.cmx: foo_p4.eliom
          ${ELIOMOPT} -c ${SERVER_INC} $(GENERATE_DEBUG) $<

  ${ELIOM_CLIENT_DIR}/foo_p4.cmo: foo_p4.eliom
          ${JS_OF_ELIOM} -c ${CLIENT_INC} $(GENERATE_DEBUG) $<

open Std

module Default_service = struct
  let make =
    (fun () () ->
      let open Html5 in
      let content = 
        let header = [h1 [pcdata "Ocsibase Home"];] in
        let welcome = [
          h2 [pcdata "Welcome"];
          p [
            pcdata "This is Ocsibase website; if you see this, you can \
              start hacking your web site :)";
          ];
        ] in
        return (header @ welcome)
      in
      Template.default ~title:"Home" content)

end

let () =
  Eliom_services.register_eliom_module
    "ocsibase" 
    (fun () ->
      
      Lwt_preemptive.init 1 500 (eprintf "LwtP:%s\n%!");

      let _ =
        (* From the doc: http://ocsigen.org/eliom/api/server/Eliom_output
           >   Note that you should not catch every exception here
           >   since some Eliom mechanisms are done using exceptions,
           >   like redirections. Do not catch exception defined in
           >   Eliom except Eliom_common.Eliom_404,
           >   Eliom_common.Eliom_Wrong_parameter
           >   Eliom_common.Eliom_Typing_Error.

           I don't understand why we see Eliom_404 exceptions
           everywhere, and only the `real' ones get redirected to the
           404. anyway, It Works™.
        *)        
        let send ?code e =
          Lwt.bind (Template.default (error e)) (Eliom_output.Html5.send ?code)
        in
        Eliom_output.set_exn_handler (function
          | Eliom_common.Eliom_404 -> send ~code:404 `eliom_404
          | Eliom_common.Eliom_Wrong_parameter -> send `eliom_wrong_parameter
          | Eliom_common.Eliom_Typing_Error l -> send (`eliom_typing_error l)
          | e -> eprintf "EXN: %s\n%!" (Exn.to_string e); Lwt.fail e)
      in

      let () =
        let pam_service = ref None in
        let debug_mode = ref false in
        let open Simplexmlparser in
        
        let rec go_through = function
          | Element ("debug", [], []) -> debug_mode := true
          | Element ("pam-authentication-service", [], [PCData p]) ->
            pam_service := Some p
          | Element (tag, atts, inside) ->
            Ocsigen_messages.console (fun () ->
              sprintf "Unknown Config XML Tag: %s\n" tag);
            List.iter inside go_through
          | PCData s -> ()
        in
        List.iter (Eliom_config.get_config ()) ~f:go_through;
        Authentication.init ~disabled:!debug_mode ?pam_service:!pam_service ();
      in

      Services.(register default) (Default_service.make);

      Services.(register home) (Default_service.make);
      
      (* Put more services here ! *) 
      
      Services.(register_css stylesheet) Template.(css_service_handler);

    )


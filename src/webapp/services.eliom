{shared{
open Std
}}

let make = make_delayed

let default =
  make (Eliom_services.service ~path:[""] ~get_params:Eliom_parameters.unit)
let home =
  make (Eliom_services.service ~path:["home"] ~get_params:Eliom_parameters.unit)
    
let stylesheet =
  make (Eliom_services.service
          ~path:["ocsibase_stylesheet"]
          ~get_params: Eliom_parameters.unit)

let register f =
  Output_app.register 
    ~error_handler:(fun sel -> 
      List.iter sel ~f:(fun (s, e) -> 
        eprintf "Errors: %S %S\n%!" s (Exn.to_string e));
      Lwt.return 
        Html5.(html
                 (head (title (ksprintf pcdata "ocsibase: ERROR")) [])
                 (body [
                   div [
                     ksprintf pcdata "ERROR:";
                     ul (List.map sel (fun (s, e) -> 
                       li [ksprintf pcdata "%S: %S" s (Exn.to_string e)]));
                   ];
                 ])))
    ~service:(f ())


let register_css f =
  Eliom_output.CssText.register ~service:(f ())

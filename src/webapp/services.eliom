(*
 * Copyright (c) 2012 Sebastien Mondet <seb@mondet.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)
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

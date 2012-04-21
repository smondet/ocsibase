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
include Core.Std

module Html5 = struct
  include Eliom_pervasives.HTML5.M
  let pcdataf fmt = ksprintf pcdata fmt
  let codef fmt = ksprintf (fun s -> code [pcdata s]) fmt
  let a_hreff fmt = ksprintf (fun s -> a_href (XML.uri_of_string s)) fmt
end
  
module Output_app =
  Eliom_output.Eliom_appl (struct
    let application_name = "ocsibase"
  end)

module Flow_monad = struct
  include  Monad.Make2(struct 
    type ('a, 'b) t = ('a, 'b) Result.t Lwt.t

    let return x = Lwt.return (Ok x) 
    let bind x f = 
      Lwt.bind x (function
      | Error e -> Lwt.return (Error e)
      | Ok o -> f o)
  end)
    
  let error e = Lwt.return (Error e)
    
  let bind_on_error m ~f = Lwt.bind m (function
    | Ok o -> Lwt.return (Ok o)
    | Error e -> (f e))
    
  let double_bind m ~ok ~error = 
    Lwt.bind m (function
    | Ok o -> ok o
    | Error e -> error e)
      
  let catch_io ~f x =
    Lwt.catch 
      (fun () -> 
        let a_exn_m : 'a Lwt.t = f x in
        Lwt.bind a_exn_m (fun x -> Lwt.return (Ok x)))
      (fun e -> Lwt.return (Error e))
   
  let wrap_io ?(on_exn=fun e -> `io_exn e) f x =        
    let caught = catch_io f x in
    double_bind caught
      ~ok:return
      ~error:(fun exn -> error (on_exn exn)) 

end
include Flow_monad
  
let rec interleave_list ~sep = function
  | [] -> []
  | [one] -> [one]
  | l :: t -> l :: sep :: interleave_list ~sep t

let array_to_list_intermap ~sep ~f a =
  interleave_list ~sep (List.map (Array.to_list a) ~f)

{client{
let (|!) x f = f x
let lwtunit (x: unit Lwt.t) = Pervasives.ignore x
let ($) f x = f x
  
let float_of_string s =
  (* imitate ocaml's runtime: *)
  let f = float_of_string s in
  match classify_float f with
  | FP_infinite | FP_nan -> failwith "float_of_string"
  | _ -> f
    
let get_element_exn s =
  Js.Opt.get (Dom_html.document##getElementById (Js.string s))
    (fun _ -> Printf.ksprintf failwith "Getting %S -> ERROR " s)
  
let get_element s =
  Js.Opt.to_option (Dom_html.document##getElementById (Js.string s))

}}

let make_delayed f = 
  let content = ref None in
  fun () ->
    match !content with
    | None -> 
      let s = f () in          
      content := Some s;
      s
    | Some s -> s

let unique_id =
  let i = ref 0 in
  (fun s -> incr i; sprintf "%s_%d" s !i)

    

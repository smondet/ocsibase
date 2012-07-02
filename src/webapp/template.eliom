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

(* In progress animation, requires a gif animation: images/loader.gif *)
let in_progress_animation_id = unique_id "in_progress_animation"
let in_progress_animation_div () =
  let open Html5 in
  div ~a:[ a_style "position: fixed; width: 100%; height: 100%; top: 0px; left: 0px;
                    padding-top: 100px; padding-left: 45%;
                    visibility: hidden; z-index:200;
                    background-color: rgba(200, 200, 200, 0.5);";
           a_id in_progress_animation_id ]
    [ img 
        ~src:(Eliom_content.Xml.uri_of_string "images/loader.gif")
        ~alt:"in progress notification" () ]

let in_progress_animation_handler unique_elt = 
  Eliom_service.onload {{
    (Eliom_content.Html5.To_dom.of_element %unique_elt)##onclick
    <- Dom_html.(handler (fun ev ->
      begin match taggedEvent ev with
      | MouseEvent me when me##ctrlKey = Js._true || me##shiftKey = Js._true
                           || me##button = 1 ->
        (* Dirty way of avoiding the 'open in new tab/window' *)
        Eliom_lib.debug "Mouse Event! Ctrl: %b, Button: %d"
          (Js.to_bool me##ctrlKey) me##button
      | _ ->
        (get_element_exn %in_progress_animation_id) ##style##visibility <-
          Js.string "visible";
      end;
      Js._true));
  }}

    
    
let css_service_handler () () =
  let open Lwt in
  let css = Buffer.create 42 in
  let out fmt = ksprintf (fun s -> (Buffer.add_string css s)) fmt in
  out "body {font:13px Helvetica,arial,freesans,clean,sans-serif;
           left: 2px; right: 2px; line-height:1.4;}";

  Lwt.return (Buffer.contents css)

let rec html_of_error poly_error = 
  let open Html5 in
  match poly_error with
  | `eliom_404 -> [pcdataf "Error 404."]
  | `eliom_wrong_parameter -> [pcdataf "Error 404 (wrong parameter)."]
  | `eliom_typing_error _ -> [pcdataf "Error 404 (wrong parameter types)."]
  | `io_exn e -> [pcdataf "Generic I/O exception: %s" (Exn.to_string e)]
  | `auth_state_exn e ->
    [pcdataf "Authentication-state exception: %s" (Exn.to_string e)]

let a_link ?(a=[]) service content args =
  let unique_elt =
    let aa = a in
    Eliom_content.Html5.F.((*unique*)
             (span
                [Eliom_content.Html5.F.a ~a:aa ~service:(service ()) content args]))
  in
  in_progress_animation_handler unique_elt;
  unique_elt

    
let default ?(title) content =
  let page page_title auth_state html_stuff =
    Html5.(
      html
        (head (title (pcdata page_title)) [
          link ~rel:[`Stylesheet] ~href:(Eliom_content.Xml.uri_of_string "ocsibase.css") ();
          link ~rel:[`Stylesheet] ~href:(Eliom_content.Html5.F.make_uri
              ~service:Services.(stylesheet ()) ()) ();
        ])
        (body [
          in_progress_animation_div ();
          div ~a:[ a_class ["top_banner"] ] [
            div ~a:[ a_class ["top_menu"] ] [
              a_link Services.default [pcdata "Home"] ();
            div auth_state;
          ];
          div ~a:[ a_class ["main_page"]] html_stuff;
        ]]))
  in
  let html_result =
    let page_title = Option.value title ~default:"NO TITLE" in
    Authentication.display_state ~in_progress_element:in_progress_animation_id ()
    >>= fun auth_state ->
    content >>= fun good_content ->
    return (page page_title auth_state good_content)
  in
  let open Html5 in
  let error_page msg =
    page "Error" [] [
      h1 [ksprintf pcdata "Error Page"];
      p [ksprintf pcdata "An error occurred on %s:" Time.(now () |! to_string)];
      div msg;
      p [pcdata "Please complain at <ocsibaseadmin@ocsibasebasemail.com>."]
    ]
    |! Lwt.return
  in
  Lwt.bind html_result (function
  | Ok html -> 
    Lwt.return html
  | Error e -> error_page (html_of_error e))


let hide_show_div ?(a=[]) ?(display_property="block")
    ?(start_hidden=true) ~show_message ~hide_message inside =
  let more_a = a in (* "a" will be hidden while opening Html5: *)
  let open Html5 in
  let the_div_id = unique_id "hide_show_div" in
  let the_msg_id = unique_id "hide_show_msg" in
  let initial_property = if start_hidden then "none" else display_property in
  let initial_message  = if start_hidden then show_message else hide_message in
  let span_msg =
    span ~a:[
      a_id the_msg_id;
      a_class ["like_link"];
      a_style "padding: 2px";
      a_onclick {{
        let the_div = get_element_exn %the_div_id in
        let the_msg = get_element_exn %the_msg_id in
        if the_div##style##display = Js.string "none"
        then (
          the_div##style##display <- Js.string %display_property;
          the_msg##innerHTML <- Js.string %hide_message;
        ) else (
          the_div##style##display <- Js.string "none";
          the_msg##innerHTML <- Js.string %show_message;
        )
      }} ]
      [pcdata initial_message] in
  (span_msg,
   div ~a:(a_id the_div_id
           :: ksprintf a_style "display: %s" initial_property
           :: more_a)
    inside)


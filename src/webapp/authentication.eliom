{shared{
open Std
}}

type role = [ `administrator | `user ]
let string_of_role = function
  | `administrator -> "administrator"
  | `user -> "user"

type capability = [
| `view of [`any] 
| `edit of [`any]
]

let roles_allow roles (cap:capability) =
  match cap with
  | `edit something ->
    if List.exists roles (fun c -> c = `administrator) then
      true
    else
      false
  | `view something ->
    if List.exists roles (fun c -> c = `administrator) then
      true
    else
      let is_user = List.exists roles ((=) `user)  in
      if is_user then
        match something with
        | `any -> true
      else
        false

type user_logged = {
  id: string;
  roles: role list;
}
    
type authentication_state = [
| `nothing
| `user_logged of user_logged
| `insufficient_credentials of string
]

let authentication_history =
  Eliom_references.eref ~secure:true
    ~scope:Eliom_common.session ([]: authentication_state list)
   
let global_authentication_disabled = ref false
let global_pam_service = ref ("")  

let init ?(disabled=false) ?(pam_service="") () =
  global_pam_service := pam_service;
  global_authentication_disabled := disabled

  
let set_state s =
  let on_exn e = `auth_state_exn e in
  wrap_io ~on_exn Eliom_references.get authentication_history
  >>= fun ah ->
  wrap_io ~on_exn (Eliom_references.set authentication_history) (s :: ah)

let get_state () =
  let on_exn e = `auth_state_exn e in
  wrap_io ~on_exn Eliom_references.get authentication_history
  >>= function
  | [] | `nothing :: _ -> return `nothing
  | h :: t -> return h

let user_logged () =
  wrap_io Eliom_references.get authentication_history
  >>= function
  | `user_logged u :: _ -> return (Some u)
  | _ -> return None

    
let find_user login =
  match login with
  | "ocsibaseadmin" -> return { id = login; roles = [`administrator; `user] }
  | "ocsibaseuser"  -> return { id = login; roles = [`user] }
  | _ -> error `user_not_found
      
let pam_auth ?service ~user ~password () =
  let service =
    Option.value ~default:!global_pam_service service in
  let wrap_pam f a = try Ok (f a) with e -> Error (`pam_exn e) in
  let auth () =
     wrap_pam (Simple_pam.authenticate service user) (password) 
  in
  Lwt_preemptive.detach auth () 
      
let check = function
  | `user_password (identifier, password) ->
    let checking_m =
      find_user identifier >>= fun person ->
      pam_auth ~user:person.id ~password ()
      >>= fun () ->
      return person
    in
    double_bind checking_m
      ~ok:(fun person -> set_state (`user_logged person))
      ~error:(fun e -> set_state (`insufficient_credentials identifier))

let logout () =
  set_state `nothing

let authorizes (cap:capability) =
  user_logged ()
  >>= fun o ->
  begin match o with
  | Some u -> return (roles_allow u.roles cap)
  | _ -> return !global_authentication_disabled
  end

let login_coservice = 
  let coserv = ref None in
  fun () ->
    match !coserv with
    | Some s -> s
    | None ->
      let open Lwt in
      let pam_handler =
        (* This coservice is created/registered once, and then re-used
           for every login.
           The function Authentication.check handles the
           session-dependent stuff. *)
        Eliom_output.Action.register_post_coservice'
          (* ~fallback:Services.(home ()) *)
          ~https:true
          ~post_params:Eliom_parameters.(string "user" ** string "pwd")
          (fun () (user, pwd) ->
            if Eliom_request_info.get_ssl ()
            then (check (`user_password (user,pwd))
                  >>= function
                  | Ok () -> return ()
                  | Error e -> return ())
            else return ())
      in
      coserv := Some pam_handler;
      pam_handler

let logout_coservice =
  let coserv = ref None in
  fun () ->
    match !coserv with
    | Some s -> s
    | None ->
      let open Lwt in
      let handler =
        Eliom_output.Action.register_post_coservice'
          ~post_params:Eliom_parameters.unit
          (fun () () -> 
            logout () >>= function
            | Ok () -> return ()
            | Error e -> return ())
      in
      coserv := Some handler;
      handler


let login_form ?set_visibility_to () =
  let open Html5 in
  let form_span_id = "span_login_form" in
  let message_span_id = "span_login_message" in
  Eliom_output.Html5.post_form ~service:(login_coservice ())
    (fun (name, pwd) ->
      [
        span ~a:[ a_id message_span_id; a_style "visibility: hidden" ] [];
        span ~a:[ a_id form_span_id;]
          [pcdata "Login: ";
          Eliom_output.Html5.string_input ~input_type:`Text ~name ();
          pcdata " Password: ";
          Eliom_output.Html5.string_input ~input_type:`Password ~name:pwd ();
          Eliom_output.Html5.string_input
            ~a:[
              (* The onclick seems to work also when the user types <enter>
                 but onsubmit does not seem to to anything (?)
                 a_onsubmit {{debugf %dbgsrv "onsubmit!"}}; *)
              a_onclick {{
                let form_span =
                    Dom_html.document##getElementById (Js.string %form_span_id) in
                let message_span =
                    Dom_html.document##getElementById (Js.string %message_span_id) in
                Js.Opt.iter form_span (fun span ->
                  span##style##visibility  <- Js.string "hidden";);
                Js.Opt.iter message_span (fun span ->
                  span##style##visibility  <- Js.string "visible";
                  span##innerHTML <- Js.string "<b>Processing …</b>";);
                begin match %set_visibility_to with
                | Some s ->
                  (get_element_exn s)##style##visibility <- Js.string "visible";
                | None -> ()
                end
              }};
            ]
            ~input_type:`Submit ~value:"Login" ();
         ];
      ]) () 

let logout_form () =
  let open Html5 in
  Eliom_output.Html5.post_form ~service:(logout_coservice ())
    (fun () ->
      [span [
        Eliom_output.Html5.string_input ~input_type:`Submit ~value:"Logout" ()
      ]])

let display_state ?in_progress_element () =
  let open Html5 in
  get_state ()
  >>= fun s ->
  let state =
    match s with
    | `nothing -> pcdataf "No user"
    | `user_logged u -> 
      span [
        pcdata "User: ";
        pcdataf "%s" u.id;
        pcdataf " (%s)"
          (String.concat ~sep:", " (List.map u.roles string_of_role));
      ]
    | `insufficient_credentials s -> pcdataf "Wrong credentials for: %s" s
  in
  return (state
          :: (match s with
          | `user_logged _ -> 
            [pcdata "; "; logout_form () ()]
          | _ -> 
             if Eliom_request_info.get_ssl () then
               [pcdata ". ";
                login_form ?set_visibility_to:in_progress_element ()]
             else
               [pcdata ": ";
                Eliom_output.Html5.a
                  ~service:Eliom_services.https_void_coservice'
                  [pcdata "Login with HTTPS"] ();
               pcdata "."]
             
          ))


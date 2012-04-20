open Printf

let () =
  let modules = List.rev (List.tl (Array.to_list Sys.argv)) in
  let rec go_through =
    function
    | [] -> ()
    | h :: t ->
      let deps where =
        (String.concat " " (List.rev_map (sprintf "_%s/%s.cmi" where) t)) in
      printf "_server/%s.cmo: %s\n" h (deps "server");   
      printf "_server/%s.cmx: %s\n" h (deps "server");   
      printf "_client/%s.cmo: %s\n" h (deps "client");   
      printf "_client/%s.cmx: %s\n" h (deps "client"); 
      printf "_client/%s.cmi: %s.type_mli\n" h h;  
      go_through t
  in
  go_through modules

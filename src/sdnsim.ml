open Printf
open MyXML
open Lwt
open Topo

let log_server_port = 8124

let get_logging_server_attrib el = 
  try
    let log = get_attrib_fail el "logger" in
    match (Re_str.split (Re_str.regexp ":") log ) with 
      | [server] -> 
          let _ = printf "XXXXXXX returning server %s:%d\n%!" server 
                  log_server_port in 
          Some (server, log_server_port)
      | server::port:: _  -> 
          let _ = printf "XXXXXXX returning server %s:%s\n%!" server 
                  port in 
          Some (server, (int_of_string port))
      | _ -> None
    with _ -> 
      let _ = printf "XXXXXXX no logging service\n%!" in 
      None

(* load the topology description file *)
let parse_xm_file file =
  let sc = init_scenario () in 
  let _ = printf "opening file %s...\n%!" file in 

  (* open file *)
  let xml = parse_file file in

  (* read configuration files details *)
  let _ = set_scenario_name sc (get_attrib_fail xml "module") in  
  let _ = set_scenario_log_server sc 
          (get_logging_server_attrib xml) in  
  let _ = set_scenario_backend sc 
            (get_attrib_default xml "backend" "ns3-direct") in 
  let _ = 
    set_scenario_duration sc 
      (int_of_string (get_attrib_default xml "duration" "60")) in

  (* parse modules *)
  let _ = List.iter (
    fun el -> 
      try
        if ((tag el) = "modules" ) then
            iter
                (fun el -> 
                  let v = (pcdata (List.hd (children el))) in
                  if ((tag el) = "library") then
                    add_scenario_module sc v) el   
      with No_attribute _ -> ()
    ) (children xml) in

  (* parse node details *)
  let _ = List.iter (
    fun el -> 
      try
        if ((tag el) = "node" ) then
          let node = get_attrib_fail el "name" in
          let main_method = get_attrib_fail el "main" in
          let params = 
            fold 
                (fun r el -> 
                  let v = (pcdata (List.hd (children el))) in
                  if ((tag el) = "param") then
                    try 
                      r @ [(Some(attrib el "name"), v)]
                    with No_attribute _ -> r @  [(None, v)]
                  else r) [] el in 
            let _ = add_scenario_node sc node main_method params in 
              printf "read %s -> %s\n%!" node main_method 
        with No_attribute _ -> ()
    ) (children xml) in

  (* Parse link details *)
  let _ = 
    List.iter 
      (fun el ->
        try
          if ((tag el) = "link" ) then
            let node_src = get_attrib_fail el "src" in
            let node_dst = get_attrib_fail el "dst" in
            let delay = int_of_string (get_attrib_default el "delay" "10") in 
            let rate = int_of_string (get_attrib_default el "rate" "100") in 
            let pcap = bool_of_string (get_attrib_default el "pcap" "false") in
              add_scenario_link sc node_src node_dst (delay, rate, pcap) 
       with No_attribute _ -> ()
      ) (children xml) in
    sc

let generate_scenario sc = 
  match (get_scenario_backend sc) with
    | NS3 -> Ns3.generate_scenario sc
    | XEN -> Xen.generate_scenario sc 

let run_scenario sc = 
  match (get_scenario_backend sc) with
    | NS3 -> Ns3.run_scenario sc
    | XEN -> Xen.run_scenario sc 

let clean_scenario sc = 
  match (get_scenario_backend sc) with
    | NS3 -> Ns3.clean_scenario sc
    | XEN -> Xen.clean_scenario sc 


let build_tags_files sc = 
  let out = open_out "_tags" in
  let _ = output_string out "\".git\": -traverse\n\".git\": not_hygienic\n" in 
  let _ = output_string out "<*.ml{,i}>: syntax_camlp4o\n" in 
  let _ = output_string out 
      (sprintf "\"topo_%s.nobj.o\":pkg_mirage\n" (get_scenario_name sc)) in
  let _ = output_string out "<*.ml{,i}>: pkg_mirage\n" in 
  let _ = output_string out 
      (sprintf "\"topo_%s.nobj.o\":pkg_mirage-net\n" (get_scenario_name sc)) in
  let _ = output_string out "<*.ml{,i}>: pkg_mirage-net\n" in 
  let _ = 
    iter_scenario_module sc (
      fun build_module -> 
        let _ = output_string out 
            (sprintf "\"topo_%s.nobj.o\":pkg_%s\n" (get_scenario_name sc) 
               build_module) in
        output_string out (sprintf "<*.ml{,i}>: pkg_%s\n" build_module)
    ) in 
  let _ = output_string out 
      (sprintf "\"topo_%s.nobj.o\":custom\n" (get_scenario_name sc)) in
  close_out out 

let build_ocamlbuild_files sc = 
  let _ = Unix.system "cp /usr/local/share/sdnsim/myocamlbuild.ml ." in 
  let _ = build_tags_files sc in 
  ()

lwt _ =
  try_lwt
    let sc = parse_xm_file Sys.argv.(1) in
    let _ = build_tags_files sc in 
    let _ = build_ocamlbuild_files sc in 
    lwt _ = generate_scenario sc in
    lwt _ = run_scenario sc in
    lwt _ = clean_scenario sc in 
    return ()
  with ex ->
    return (eprintf "error: %s\n%s\n%!" (Printexc.to_string ex)
              (Printexc.get_backtrace ()))

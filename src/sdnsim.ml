open Printf
open Xml
open Lwt
open Topo


(* Augment the xml parsing functions *)
let get_attrib_fail el name = 
  try 
    Xml.attrib el name
  with Xml.No_attribute _ -> 
    failwith (sprintf "required attribute %s missing" name)

let get_attrib_default el name default_value = 
  try 
    Xml.attrib el name 
  with Xml.No_attribute _ -> default_value

(* load the topology description file *)
let parse_xm_file file =
  let sc = init_scenario () in 
  let _ = printf "opening file %s...\n%!" file in 

  (* open file *)
  let xml = Xml.parse_file file in

  (* read configuration files details *)
  let _ = set_scenario_name sc (get_attrib_fail xml "module") in  
  let _ = set_scenario_backend sc 
            (get_attrib_default xml "backend" "ns3-direct") in 
  let _ = 
    set_scenario_duration sc 
      (int_of_string (get_attrib_default xml "duration" "60")) in

  (* parse modules *)
  let _ = List.iter (
    fun el -> 
      try
        if ((Xml.tag el) = "modules" ) then
            Xml.iter
                (fun el -> 
                  let v = (Xml.pcdata (List.hd (Xml.children el))) in
                  if ((Xml.tag el) = "library") then
                    add_scenario_module sc v) el   
      with Xml.No_attribute _ -> ()
    ) (Xml.children xml) in

  (* parse node details *)
  let _ = List.iter (
    fun el -> 
      try
        if ((Xml.tag el) = "node" ) then
          let node = get_attrib_fail el "name" in
          let main_method = get_attrib_fail el "main" in
          let params = 
            Xml.fold 
                (fun r el -> 
                  let v = (Xml.pcdata (List.hd (Xml.children el))) in
                  if ((Xml.tag el) = "param") then
                    try 
                      r @ [(Some(Xml.attrib el "name"), v)]
                    with Xml.No_attribute _ -> r @  [(None, v)]
                  else r) [] el in 
            let _ = add_scenario_node sc node main_method params in 
              printf "read %s -> %s\n%!" node main_method 
        with Xml.No_attribute _ -> ()
    ) (Xml.children xml) in

  (* Parse link details *)
  let _ = 
    List.iter 
      (fun el ->
        try
          if ((Xml.tag el) = "link" ) then
            let node_src = get_attrib_fail el "src" in
            let node_dst = get_attrib_fail el "dst" in
            let delay = int_of_string (get_attrib_default el "delay" "10") in 
            let rate = int_of_string (get_attrib_default el "rate" "100") in 
            let pcap = bool_of_string (get_attrib_default el "pcap" "false") in
              add_scenario_link sc node_src node_dst (delay, rate, pcap) 
       with Xml.No_attribute _ -> ()
      ) (Xml.children xml) in
    sc

let generate_scenario sc = 
  match (get_scenario_backend sc) with
    | NS3 -> Ns3.generate_scenario sc
(*    | XEN -> Xen.build_scenario sc*)

let run_scenario sc = 
  match (get_scenario_backend sc) with
    | NS3 -> Ns3.run_scenario sc
(*    | XEN -> Xen.run_scenario sc*)

let _ =
  try
    let sc = parse_xm_file Sys.argv.(1) in
    let _ = generate_scenario sc in
    let _ = run_scenario sc in 
      return ()
  with ex ->
      return (eprintf "error: %s\n%s\n%!" (Printexc.to_string ex)
      (Printexc.get_backtrace ()))

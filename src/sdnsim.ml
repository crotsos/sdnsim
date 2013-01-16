open Printf
open Xml
open Lwt

let nodes = Hashtbl.create 64 
let links = ref [] 

type output_type = 
  | NS3
  | XEN

let string_to_output_type = function
  | "ns3-direct" -> NS3
  | "xen-direct" -> XEN
  | a -> failwith (sprintf "unknown output %s" a)

let output_target = ref NS3
let module_name = ref ""

let get_attrib_fail el name = 
  try 
    Xml.attrib el name
  with Xml.No_attribute _ -> 
    failwith (sprintf "required attribute %s undefined" name)

let get_attrib_default el name default_value = 
  try 
    Xml.attrib el name 
  with Xml.No_attribute _ -> default_value

let parse_xm_file file = 
  let _ = printf "opening file %s...\n%!" file in 
  let xml = Xml.parse_file file in
  let _ = module_name := (get_attrib_fail xml "module") in  
 let _ =  output_target := 
          string_to_output_type 
            (get_attrib_default xml "backend" "ns3-direct") in 
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
                         else
                           r
                ) [] el in 
            let _ = Hashtbl.add nodes node (main_method, params) in 
              printf "read %s -> %s\n%!" node main_method 
        with Xml.No_attribute _ -> ()
    ) (Xml.children xml) in 
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
              if ((Hashtbl.mem nodes node_src) &&
                  (Hashtbl.mem nodes node_dst)) then 
                let _ = links := !links @  
                        [(node_src, node_dst, delay, rate, pcap)] in 
                  printf "read %s -> %s (%d, %d, %B)\n%!" node_src node_dst delay rate pcap
              else
                  printf "Node %s or %s not define, ignoming links...\n" node_src node_dst
        with Xml.No_attribute _ -> ()
      ) (Xml.children xml) in
    ()

let run_code () = 
  match !output_target with
    | NS3 -> Ns3.run_code !module_name nodes !links 
    | XEN -> Xen.run_code !module_name nodes !links

let _ =
  try
    let _ = parse_xm_file Sys.argv.(1) in
    let _ = run_code () in 
      return ()
  with ex ->
      return (eprintf "error: %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace ()))

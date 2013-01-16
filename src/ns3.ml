open Printf
open Lwt

let generate_simulation module_name nodes links =
  let out = open_out (sprintf "topo_%s.ml" module_name) in
  let _ = output_string out "let run () =\n" in  
  let _ = List.iter (
    fun (host, main, params) ->
      let str_param = 
        List.fold_right (
        fun (n, v) r -> 
          match n with
            | None -> sprintf "%s %s" r v
            | Some(n) -> sprintf "%s ~%s:%s" r n v
        ) params "" in 
      let str_node = 
        sprintf "\tlet _ = OS.Topology.add_node \"%s\" (%s.%s %s) in\n" 
          host module_name main str_param in
        output_string out str_node
  ) (Hashtbl.fold (fun host (main, params) r-> r @  [(host, main, params)]) nodes []) in
  let _ = 
    List.iter
      ( fun (node_src, node_dst, delay, rate, pcap) -> 
          let str_node = 
            sprintf "\tlet _ = OS.Topology.add_link ~prop_delay:%d ~rate:%d ~pcap:%s \"%s\" \"%s\" in"
              delay rate (string_of_bool pcap) node_src node_dst in
            output_string out str_node
      ) links in
  let _ = output_string out "\t\t()\n" in 
  let _ = close_out out in 
  let out = open_out (sprintf "topo_%s.mir" module_name) in
  let _ = output_string out 
            (sprintf "Topo_%s.run \n"
            module_name) in  
    close_out out 

let build_simulation module_name = 
  let _ = Unix.system "ocamlbuild -clean" in 
  let _ = Unix.system (sprintf "mir-build ns3-direct/topo_%s.bin" module_name) in 
    ()

let run_simulation module_name = 
  let _ = Unix.system (sprintf "./_build/ns3-direct/topo_%s.bin" module_name) in 
    ()

let run_code module_name nodes links = 
  let _ = generate_simulation module_name nodes links in
  let _ = build_simulation module_name in 
  let _ = run_simulation module_name in 
    ()
 

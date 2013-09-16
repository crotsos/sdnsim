open Printf
open Lwt
open Topo


let generate_scenario sc =
  let module_name = get_scenario_name sc in 
  let out = open_out (sprintf "topo_%s.ml" module_name) in
  let _ = output_string out "let run () =\n" in 
  let _ = output_string out 
      (sprintf "\tlet _ = OS.Time.set_duration %d in\n" (get_scenario_duration sc)) in  
  let _ = iter_scenario_nodes sc (
      fun host main params ->
        let _ = output_string out 
            (sprintf "\tlet _ = OS.Topology.add_node \"%s\" (%s.%s "
               host module_name main) in 
        let _ =
          List.iter (
            fun (n, v) -> 
              output_string out 
                (match n with
                 | None -> sprintf "%s " v
                 | Some(n) -> sprintf "~%s:%s " n v)
          ) params in 
        output_string out ") in\n"
    ) in
  let _ = 
    iter_scenario_links sc
      ( fun src dst delay rate pcap -> 
         output_string out
           (sprintf "\tlet _ = OS.Topology.add_link ~prop_delay:%d ~rate:%d ~pcap:%s \"%s\" \"%s\" in\n"
              delay rate (string_of_bool pcap) src dst)
      ) in
  let _ = output_string out "\t\t()\n\n" in 
  let _ = 
    match (get_scenario_log_server sc) with
    | None -> output_string out "let _ = OS.Topology.load run \n" 
    | Some (server, port) -> 
      output_string out 
        (sprintf "let _ = OS.Topology.load ~debug:(Some(\"%s\",%d)) run\n"
           server port)
  in
  let _ = close_out out in 
  (* ()

     let build_simulation module_name = *)
  let _ = Unix.system (sprintf "rm %s.native" module_name) in 
  let _ = Unix.system "ocamlbuild -clean" in 
  let _ = Unix.system (sprintf "ocamlbuild topo_%s.nobj.o" module_name) in 
  let _ = Unix.system (sprintf "mir-build -b ns3-native -o %s.native _build/topo_%s.nobj.o" 
                         module_name module_name) in 
  let _ = Unix.system (sprintf "rm topo_%s.ml" module_name) in 
  return ()

let run_scenario sc = 
  let _ = Unix.system (sprintf "mpirun -n %d ./%s.native" 
                         (get_scenario_node_count sc) (get_scenario_name sc)) in 
  return ()

let clean_scenario sc = return ()

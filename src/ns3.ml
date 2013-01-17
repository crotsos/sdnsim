open Printf
open Lwt
open Topo

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
  let _ = output_string out "\t\t()\n" in 
  let _ = output_string out "let _ = OS.Topology.load run \n" in 
  let _ = close_out out in 
  let _ = build_ocamlbuild_files sc in 
  (* ()

let build_simulation module_name = *)
  let _ = Unix.system (sprintf "rm %s.native" module_name) in 
  let _ = Unix.system "ocamlbuild -clean" in 
  let _ = Unix.system (sprintf "ocamlbuild topo_%s.nobj.o" module_name) in 
  let _ = Unix.system (sprintf "mir-build -b ns3-native -o %s.native _build/topo_%s.nobj.o" 
                        module_name module_name) in 
  let _ = Unix.system (sprintf "rm topo_%s.ml" module_name) in 
    ()

let run_scenario sc = 
  let _ = Unix.system (sprintf "mpirun -n %d ./%s.native" 
                        (get_scenario_node_count sc) (get_scenario_name sc)) in 
    ()

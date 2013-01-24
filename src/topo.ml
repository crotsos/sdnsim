open Printf

type backend = 
  | NS3
  | XEN

let string_to_backend = function
  | "ns3-direct" -> NS3
  | "xen-direct" -> XEN
  | a -> failwith (sprintf "unknown output %s" a)

type params = (string option * string) list

type scenario = {
  mutable name : string;
  mutable duration : int;
  mutable backend :backend;
  mutable log_server : (string * int) option;
  mutable nodes : (string, string * params) Hashtbl.t;
  mutable links : (string * string * int * int * bool) list;
  mutable modules : string list;
}

let init_scenario () = 
  {duration=60; backend=NS3; nodes=(Hashtbl.create 64); links=[];
    name="";modules=[]; log_server=None; }

let set_scenario_log_server sc value =  sc.log_server <- value
let get_scenario_log_server sc = sc.log_server

let set_scenario_duration sc d = sc.duration <- d
let get_scenario_duration sc = sc.duration
let set_scenario_backend sc b = sc.backend <- (string_to_backend b)
let get_scenario_backend sc = sc.backend
let set_scenario_name sc n = sc.name <- n
let get_scenario_name sc = sc.name
let add_scenario_module sc name = sc.modules <- name :: sc.modules
let iter_scenario_module sc f = List.iter f sc.modules 

let add_scenario_node sc name main_method params = 
  Hashtbl.replace sc.nodes name (main_method, params)

let add_scenario_link sc src dst (delay, rate, pcap) =
  if ((Hashtbl.mem sc.nodes src) && (Hashtbl.mem sc.nodes dst)) then 
    let _ = sc.links <- (src, dst, delay, rate, pcap) :: sc.links in 
      printf "read %s -> %s (%d, %d, %B)\n%!" src dst delay rate pcap
  else
    printf "Node %s or %s not define, ignoring links...\n" src dst
 
let iter_scenario_nodes sc fn = 
  Hashtbl.iter (
    fun host (main, params) -> fn host main params
  ) sc.nodes
let get_scenario_node_count sc = Hashtbl.length sc.nodes 

let iter_scenario_links sc fn = 
  List.iter (
    fun (src, dst, delay, rate, pcap) -> 
      fn src dst delay rate pcap
  ) sc.links

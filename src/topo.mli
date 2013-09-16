type backend = 
  | NS3
  | XEN

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

val init_scenario : unit -> scenario 

val set_scenario_duration : scenario -> int -> unit
val get_scenario_duration : scenario -> int

val set_scenario_backend : scenario -> string -> unit
val get_scenario_backend : scenario -> backend

val set_scenario_name : scenario -> string -> unit 
val get_scenario_name : scenario -> string

val add_scenario_node : scenario -> string -> string -> params -> unit
val add_scenario_link : scenario -> string -> string -> 
  (int * int * bool) -> unit
val get_scenario_node_count : scenario -> int

val set_scenario_log_server : scenario -> (string * int) option -> unit
val get_scenario_log_server : scenario ->  (string * int) option

val iter_scenario_nodes : scenario -> 
  (string -> string -> params -> unit) -> unit
val iter_scenario_links : scenario -> 
  (string -> string -> int -> int -> bool -> unit) -> 
    unit

val get_scenario_nodes : scenario -> (string, string * params) Hashtbl.t
val get_scenario_links : scenario -> (string * string * int * int * bool) list

val add_scenario_module : scenario -> string -> unit
val iter_scenario_module : scenario -> (string -> unit) -> unit



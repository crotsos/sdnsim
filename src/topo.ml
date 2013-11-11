(*
 * Copyright (c) 2012 Charalampos Rotsos <cr409@cl.cam.ac.uk>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

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
  mutable slowdown : float;
  mutable backend :backend;
  mutable log_server : (string * int) option;
  mutable nodes : (string, string * params) Hashtbl.t;
  mutable links : (string * string * int * int * bool) list;
  mutable modules : string list;
}

let init_scenario () = 
  {duration=60; backend=NS3; nodes=(Hashtbl.create 64); links=[];
    name="";modules=[]; log_server=None; slowdown=1.0;}

let set_scenario_log_server sc value =  sc.log_server <- value
let get_scenario_log_server sc = sc.log_server

let set_scenario_duration sc d = sc.duration <- d
let get_scenario_duration sc = sc.duration
let set_scenario_slowdown sc d = sc.slowdown <- d
let get_scenario_slowdown sc = sc.slowdown
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


let get_scenario_nodes sc = sc.nodes
let get_scenario_links sc = sc.links

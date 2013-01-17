(*
 * Copyright (c) 2012 Charalmpos Rotsos <cr409@cl.cam.ac.uk>
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

open Lwt
open OS
open Net
open Printf

module OP = Openflow.Ofpacket
module OC = Openflow.Ofcontroller
module OE = OC.Event

let port = 55555
let use_dhcp = false

let pp = Printf.printf

(*********************************************************
 * Host configuration code and traffic generation code 
 *********************************************************)

let rec echo dst chan = 
  let _ = printf "[host1] server connected....\n%!" in 
  try_lwt
    lwt _ =
      while_lwt true do
        lwt buf = Channel.read_some chan in
(*         let _ = Printf.printf "%f: read %d\n%!" 
         (Clock.time ())
         (Cstruct.len buf) in *)
          return () 
      done
    in
      return ()
  with Nettypes.Closed -> return (pp "closed!\n")

let rec echo_client chan = 
  let _ = printf "[host2] client connected....\n%!" in 
  try_lwt
    let data = String.create 1460 in 
    let rec send_data () = 
        let _ = Channel.write_string chan data 0 (String.length data) in
(*        let _ = Printf.printf "%f: Writing new buffer....\n%!" (Clock.time
 *        ())
        in *)
        lwt _ = Channel.flush chan in
          send_data ()
    in
      send_data ()
  with 
    | Nettypes.Closed -> return (pp "closed!\n")
    | ex ->  return (Printf.printf "Eroor:%s\n%!" (Printexc.to_string ex))

let rec echo_udp dst buf = 
  return (Printf.printf "%f: read %d\n%!" 
    (Clock.time ())
    (Cstruct.len buf))
  
let rec echo_client_udp mgr dst =
  try_lwt
    let data = Cstruct.sub (Io_page.to_cstruct (Io_page.get ())) 0 1460 in
    let rec send_data () = 
        lwt _ = Datagram.UDPv4.send mgr dst data in
        lwt () = Time.sleep 1.0 in
          Printf.printf "%f: Writing new buffer....\n%!" (Clock.time ()); 
          send_data ()
    in
      send_data ()
  with 
    | Nettypes.Closed -> return (pp "closed!\n")
    | ex ->  return (Printf.printf "Error:%s\n%!" (Printexc.to_string ex))

let ip node_id = 
  Nettypes.(
    (ipv4_addr_of_tuple (10l,0l,1l,(Int32.of_int node_id)),
    ipv4_addr_of_tuple (255l,255l,255l,0l),
    [ ipv4_addr_of_tuple (10l,0l,1l,1l) ]
    )) 
      
(* Code to run on the end node *)
let host_inner host_id () =
  lwt _ = OS.Time.sleep 2.0 in 
  let config_host host_id =
    try_lwt 
      Manager.create (fun mgr interface id ->
        match host_id with
        | 1 ->
          lwt _ = Manager.configure interface (`IPv4 (ip host_id)) in
          let _ = printf "[host%d] server setting up ip 10.0.1.%d\n%!" 
                    host_id host_id  in  
(*             Datagram.UDPv4.recv mgr (None, port) echo_udp *)
          Net.Channel.listen mgr (`TCPv4 ((None, port), echo ))
        | 2 -> 
          let dst_ip = Nettypes.ipv4_addr_of_tuple (10l,0l,1l,1l) in  
          let _ = printf "[host%d] client setting up ip 10.0.1.%d\n%!" 
                    host_id host_id  in  
          lwt _ = Manager.configure interface (`IPv4 (ip host_id)) in
          let _ = Printf.printf "[host%d] %f: trying to connect client \n%!"
                    host_id (Clock.time ()) in 
(*             echo_client_udp mgr (dst_ip,port) *)
          Net.Channel.connect mgr (`TCPv4 (None, (dst_ip, port), echo_client ))
        | _ -> return (printf "Invalid node_id %d\n%!" host_id)
        )
    with e ->
      Printf.eprintf "Error: %s" (Printexc.to_string e); 
      return ()
  in  
    config_host host_id

(****************************************************************
 *   OpenFlow controller code 
 ****************************************************************)

(* TODO this the mapping is incorrect. the datapath must be moved to the key
 * of the hashtbl *)
type mac_switch = {
  addr: OP.eaddr; 
  switch: OP.datapath_id;
}

type switch_state = {
(*   mutable mac_cache: (mac_switch, OP.Port.t) Hashtbl.t; *)
  mutable mac_cache: (OP.eaddr, OP.Port.t) Hashtbl.t; 
  mutable dpid: OP.datapath_id list;
  mutable of_ctrl: OC.t list;
  req_count: int ref; 
}

let switch_data = 
  { mac_cache = Hashtbl.create 0; dpid = []; of_ctrl = []; 
  req_count=(ref 0);} 

let datapath_join_cb controller dpid evt =
  let dp = 
    match evt with
      | OE.Datapath_join (c, _) -> c
      | _ -> invalid_arg "bogus datapath_join event match!" 
  in
  switch_data.dpid <- switch_data.dpid @ [dp];
  return (pp "+ datapath:0x%012Lx\n" dp)

let packet_in_cb controller dpid evt =
  incr switch_data.req_count;
  let (in_port, buffer_id, data, dp) = 
    match evt with
      | OE.Packet_in (inp, buf, dat, dp) -> (inp, buf, dat, dp)
      | _ -> invalid_arg "bogus datapath_join event match!"
  in

  (* Parse Ethernet header *)
  let m = OP.Match.raw_packet_to_match in_port data in 

  (* Store src mac address and incoming port *)
  let ix = m.OP.Match.dl_src in
  let _ = Hashtbl.replace switch_data.mac_cache ix in_port in
 
  (* check if I know the output port in order to define what type of message
   * we need to send *)
 let ix = m.OP.Match.dl_dst in
  if ( (ix = "\xff\xff\xff\xff\xff\xff")
       || (not (Hashtbl.mem switch_data.mac_cache ix)) ) 
  then (
    let bs =
      (OP.Packet_out.create ~buffer_id:buffer_id
        ~actions:[ OP.(Flow.Output(Port.All, 2000))]
        ~data:data ~in_port:in_port () ) in
    let h = OP.Header.create OP.Header.PACKET_OUT 0 in
    OC.send_data controller dpid (OP.Packet_out (h, bs))
  ) else (
    let out_port = (Hashtbl.find switch_data.mac_cache ix) in
    let flags = OP.Flow_mod.({send_flow_rem=true; emerg=false; overlap=false;}) in 
    lwt _ =
      if (buffer_id = -1l) then
        (* Need to send also the packet in cache the packet is not cached *)
        let bs =
                OP.Packet_out.create
                   ~buffer_id:buffer_id
                   ~actions:[ OP.(Flow.Output(out_port, 2000))]
                   ~data:data ~in_port:in_port ()  in
        let h = OP.Header.create OP.Header.PACKET_OUT 0 in
          OC.send_data controller dpid (OP.Packet_out (h, bs))
      else
        return ()
    in
    let pkt =
            (OP.Flow_mod.create m 0_L OP.Flow_mod.ADD ~hard_timeout:0
                 ~idle_timeout:0 ~buffer_id:(Int32.to_int buffer_id)  ~flags
                 [OP.Flow.Output(out_port, 2000)] ()) in
        let h = OP.Header.create OP.Header.FLOW_MOD 0 in
          OC.send_data controller dpid (OP.Flow_mod (h, pkt))
 )    
    
let init st = 
  if (not (List.mem st switch_data.of_ctrl)) then
    switch_data.of_ctrl <- (([st] @ switch_data.of_ctrl));
  OC.register_cb st OE.DATAPATH_JOIN datapath_join_cb;
  OC.register_cb st OE.PACKET_IN packet_in_cb

let controller_inner () = 
  try_lwt 
    Manager.create (fun mgr interface id ->
      let ip = 
        Nettypes.(
          (ipv4_addr_of_tuple (10l,0l,0l,2l),
          ipv4_addr_of_tuple (255l,255l,255l,0l), [])) in  
     lwt _ = Manager.configure interface (`IPv4 ip) in
      let dst = ((Nettypes.ipv4_addr_of_tuple (10l,0l,0l,1l)), 6633) in 
        OC.connect mgr dst init 
      )
  with exn -> 
    return (printf "%f: controller error: %s\n%!" (Clock.time ()) 
            (Printexc.to_string exn))

(****************************************************************
 * OpenFlow Switch configuration 
 *****************************************************************)

let print_time () =
  while_lwt true do
    Time.sleep 1.0 >>
    return (printf "%03.6f: process running..\n%!" (OS.Clock.time ()))
  done

let switch_inner () = 
  let sw = Openflow.Ofswitch.create_switch () in
  try_lwt 
    Manager.create 
    (fun mgr interface id ->
      let _ = pp "[xwitch] adding intf %s\n%!" id in 
       match (id) with 
         | "0" ->
             let ip = 
               Nettypes.(
                 (ipv4_addr_of_tuple (10l,0l,0l,1l),
                  ipv4_addr_of_tuple (255l,255l,255l,0l), [])) in  
               lwt _ = Manager.configure interface (`IPv4 ip) in
               lwt _ = Openflow.Ofswitch.listen sw mgr (None, 6633) in 
                return ()
        | _ ->  Openflow.Ofswitch.add_port mgr sw id
    )
  with e ->
    Printf.eprintf "Error: %s" (Printexc.to_string e); 
    return ()

(* Design the topology *)
let run () =
  let _ = OS.Time.set_duration 10 in 
  (* Define participating nodes *)
  let _ = Topology.add_node "switch" switch_inner in 
  let _ = Topology.add_node "controller" controller_inner in 
  let _ = Topology.add_node "node1" (host_inner 1) in
  let _ = Topology.add_node "node2" (host_inner 2) in

  (* Define topology *)
  let _ = Topology.add_link "controller" "switch" in
  let _ = Topology.add_link ~rate:1002 ~pcap:true "node1" "switch" in
  let _ = Topology.add_link ~rate:1000 "node2" "switch" in
    ()
(*   OS.Topology.add_external_dev "nstap0" "node1" "10.0.1.0" "255.255.255.0" *)

(*
 * Copyright (c) 2011 Charalampos Rotsos <cr409@cl.cam.ac.uk> 
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
open Net.Nettypes
open Printf

module OP = Openflow.Ofpacket
module OC = Openflow.Ofcontroller
module OSK = Openflow.Ofsocket
module OSW = Openflow.Ofswitch
module OF = Openflow.Flowvisor
module OE = OC.Event

let port = 55555
let use_dhcp = false

let pp = Printf.printf

let log_dev id ip = 
  let Some(node_name) = Lwt.get OS.Topology.node_name in
  let msg = Rpc.Dict [
    ("name", (Rpc.String node_name));
    ("dev_id", (Rpc.String id));
    ("ip", (Rpc.String ip));] in
      OS.Console.broadcast "node_dev" (Jsonrpc.to_string msg)
 

(*********************************************************
 * Host configuration code and traffic generation code 
 *********************************************************)
let ip gid nid = 
  (ipv4_addr_of_tuple (10l,0l,(Int32.of_int gid),(Int32.of_int nid)),
    ipv4_addr_of_tuple (255l,255l, 0l,0l),
    [(ipv4_addr_of_tuple (10l, 0l, (Int32.of_int gid), 1l)) ]) 
      
(* Code to run on the end node *)
let host_inner gid hid () =
  try_lwt 
    Manager.create (fun mgr interface id ->
      lwt _ = Time.sleep 2.0 in 
      lwt _ = Manager.configure interface (`IPv4 (ip gid hid)) in
      let _ = log_dev id (sprintf "10.0.%d.%d" gid hid) in 
        match hid with
        | 2 ->
(*            Datagram.UDPv4.recv mgr (None, port) echo_udp *)
(*            Net.Channel.listen mgr (`TCPv4 ((None, port), Client.echo )) *)
            Client.pttcp_server mgr port 5 
        | 3 ->
            let dst_gid = (gid + 1) mod 2 in 
            let rem_dst_ip = 
              ipv4_addr_of_tuple (10l,0l,(Int32.of_int dst_gid),2l) in  
            let loc_dst_ip = 
              ipv4_addr_of_tuple (10l,0l,(Int32.of_int gid),2l) in  
(*            echo_client_udp mgr (dst_ip,port) *)
(*            Net.Channel.connect mgr 
                  (`TCPv4 (None, (loc_dst_ip, port), Client.echo_client )) *)
(*            Client.pttcp_client mgr rem_dst_ip port 5 10000000l<&> *)
            Client.pttcp_client mgr loc_dst_ip port 5 10000000l
        | _ -> return (printf "Invalid node_id %d\n%!" hid)
        )
    with e -> return (Printf.eprintf "Error: %s" (Printexc.to_string e))

(*
 * controller code 
 * *)
let router_inner () = 
  try_lwt
  let switch_data = Controller.sw_data () in 
    Manager.create (fun mgr interface id ->
      let ip = 
          (ipv4_addr_of_tuple (192l,168l,2l,2l),
          ipv4_addr_of_tuple (255l,255l,255l,0l), []) in  
      lwt _ = Manager.configure interface (`IPv4 ip) in
      let _ = log_dev id "192.168.3.2" in 
        OC.listen mgr (None, 6633) 
        (Controller.init_router "router" switch_data )
      )
  with exn -> 
    return (printf "%f: controller error: %s\n%!" (Clock.time ()) 
            (Printexc.to_string exn))

let controller_inner ctrl_id () = 
  try_lwt
    let switch_data = Controller.sw_data () in 
    Manager.create (fun mgr interface id ->
      let ip = 
          (ipv4_addr_of_tuple (192l,168l,(Int32.of_int ctrl_id),2l),
          ipv4_addr_of_tuple (255l,255l,255l,0l), []) in  
      lwt _ = Manager.configure interface (`IPv4 ip) in
      let name = sprintf "controller%d" ctrl_id in 
      let _ = log_dev id (sprintf "192.168.%d.2" ctrl_id) in 
        OC.listen mgr (None, 6633) 
        (Controller.init ~handle_arp:false name switch_data)
      )
  with exn -> 
    return (printf "%f: controller error: %s\n%!" (Clock.time ()) 
            (Printexc.to_string exn))

(*
 * flow visor node
 * *)
let init_slice mgr st id =
  let dpid = Int64.add 0x10L (Int64.of_string id) in 
  let subnet = Uri_IP.string_to_ipv4 (sprintf "10.0.%s.1" id) in  
  let dst_ip = ((ipv4_addr_of_tuple (192l,168l,(Int32.of_string id),2l)), 6633)  in
  match id with 
  | "0" | "1" -> 
      OF.add_slice mgr st 
      (OP.Match.create_match ~nw_src:(Some subnet) ~nw_src_len:24
        ~nw_dst:(Some subnet) ~nw_dst_len:24 ()) dst_ip dpid
  | "2" ->      
      OF.add_slice mgr st (OP.Match.create_match ()) dst_ip dpid
 

let configure_ctrl_intf mgr sw intf id  = 
  let ip = 
      (ipv4_addr_of_tuple (192l, 168l, (Int32.of_string id), 1l),
       ipv4_addr_of_tuple (255l,255l,255l,0l), []) in  
  lwt _ = Manager.configure intf (`IPv4 ip) in
  let _ = log_dev id (sprintf "192.168.%s.1" id) in 
    init_slice mgr sw id

let configure_sw_intf mgr sw intf id switch_id = 
  let ip = 
    (ipv4_addr_of_tuple (172l,16l,(Int32.of_int !switch_id),1l),
      ipv4_addr_of_tuple (255l,255l,255l,0l), []) in
  let _ = switch_id := !switch_id + 1 in 
  let _ = log_dev id (sprintf "172.16.%d.1" !switch_id) in 
  lwt _ = Manager.configure intf (`IPv4 ip) in
    OF.listen sw mgr (None, 6633)
 
let flowvisor_inner () = 
  try_lwt
    let sw = Openflow.Flowvisor.create_flowvisor () in
    let switch_id = ref 1 in 
    Manager.create 
      (fun mgr intf id -> 
        match id with
        | "0" | "1" | "2" -> configure_ctrl_intf mgr sw intf id
        | _ -> configure_sw_intf mgr sw intf id switch_id)
  with e -> return (Printf.eprintf "Error: %s" (Printexc.to_string e))

(****************************************************************
 * OpenFlow Switch configuration 
 *****************************************************************)
let switch_inner switch_id () = 
  let switch_data = Controller.sw_data () in 
  let sw = OSW.create_switch (Int64.of_int switch_id) in
  let flv = OF.create_flowvisor () in
  let ctrl = OC.init_controller () in 
  let (ctrl_ch1, ctrl_ch2) = OSK.init_local_conn_state () in
  let (sw_ch1, sw_ch2) = OSK.init_local_conn_state () in

  (* start local controller and local flowvisor threads in run mode *)
  let name = sprintf "switch%d" switch_id in 
  let _ = ignore_result (OF.local_listen flv sw_ch1) in 
  let _ = ignore_result 
          (OC.local_connect ctrl ctrl_ch1 (Controller.init ~handle_arp:true name switch_data)) in

  (* forward arp traffic to local controller *)
  let dpid = Int64.add 0x20L (Int64.of_int switch_id) in
  lwt _ = OF.add_local_slice flv 
            (OP.Match.create_match ~dl_type:(Some 0x0806) ()) 
            ctrl_ch2 dpid in   
  
  try_lwt 
    Manager.create 
    (fun mgr intf -> function
      | "0" ->
          let ip = 
            (ipv4_addr_of_tuple (172l,16l,(Int32.of_int switch_id),2l),
            ipv4_addr_of_tuple (255l,255l,255l,0l), []) in  
          let _ = log_dev "0" (sprintf "172.16.%d.2" switch_id) in 
          lwt _ = Manager.configure intf (`IPv4 ip) in
          let dst_ip = ipv4_addr_of_tuple (172l,16l,(Int32.of_int switch_id),1l) in
          lwt _ = OF.add_slice mgr flv (OP.Match.create_match ())
                    (dst_ip, 6633) (Int64.add 0x30L (Int64.of_int switch_id)) in
            OSW.local_connect sw mgr sw_ch2
      | id ->  let _ = log_dev id "" in OSW.add_port mgr sw id
    )
  with e -> return (Printf.eprintf "Error: %s" (Printexc.to_string e))

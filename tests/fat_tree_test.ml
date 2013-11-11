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
open Printf

module OP = Openflow.Ofpacket
module OC = Openflow.Ofcontroller
module OSK = Openflow.Ofsocket
module OSW = Openflow.Ofswitch
module OF = Flv.Flowvisor
module OE = OC.Event

let pp = Printf.printf

let port = 55555
let verbose = false

(*********************************************************
 * Host configuration code and traffic generation code 
 *********************************************************)
let ip pod swid hid = 
  (Ipaddr.V4.make 10l (Int32.of_int pod) (Int32.of_int swid)
  (Int32.of_int hid),
  Ipaddr.V4.make 255l 255l 255l 0l,
  [(Ipaddr.V4.make 10l (Int32.of_int pod) (Int32.of_int swid) 1l) ])
      
(* Code to run on the end node *)
let host_inner ~pod ~swid ~hid () =
  try_lwt 
    Manager.create (fun mgr interface id ->
      lwt _ = Manager.configure interface (`IPv4 (ip pod swid hid)) in
      lwt _ = Time.sleep 1.0 in 
        match hid with
        | 2 ->
(*            Datagram.UDPv4.recv mgr (None, port) echo_udp *)
           Net.Channel.listen mgr (`TCPv4 ((None, port), Client.echo ))   
(*          Client.pttcp_server mgr port 5 *)
        | 3 ->
          lwt _ = Time.sleep 1.0 in 
          let dst_pod = (pod + 1) mod 4 in 
(*          let dst_pod = pod in *)
          let dst_swid = (swid + 1) mod 2 in 
(*            let rem_dst_ip = 
              ipv4_addr_of_tuple (10l,0l,(Int32.of_int dst_gid),2l) in  *)
          let loc_dst_ip = 
            Ipaddr.V4.make 10l (Int32.of_int dst_pod) 
            (Int32.of_int dst_swid) 2l in  
(*            echo_client_udp mgr (dst_ip,port) *)
             Net.Channel.connect mgr  
               (`TCPv4 (None, (loc_dst_ip, port), Client.echo_client ))  
(*            Client.pttcp_client mgr rem_dst_ip port 5 10000000l<&> 
            Client.pttcp_client mgr loc_dst_ip port 5 1000000l *)

        | _ -> return (printf "Invalid node_id %d\n%!" hid)
        )
    with e -> return (Printf.eprintf "Error: %s" (Printexc.to_string e))

(*********************************************************
 * Switch code
 * *)
let switch_inner ~pod ~swid () = 
  let dpid = Int64.of_int ((pod lsl 8) + swid) in 
  let ip = Controller.fat_tree_ip pod swid 1 in 
  let switch_data = Controller.sw_data () in 
  let sw = OSW.create_switch ~verbose dpid in
  let flv = OF.create_flowvisor ~verbose () in 
  let ctrl = OC.init_controller ~verbose () in 
  let (ctrl_ch1, ctrl_ch2) = OSK.init_local_conn_state () in 
  let (sw_ch1, sw_ch2) = OSK.init_local_conn_state () in

  (* start local controller and local flowvisor threads in run mode *)
  let name = sprintf "switch_%d_%d" pod swid in 
  let _ = ignore_result (OF.local_listen flv sw_ch1) in 
  let _ = ignore_result 
          (OC.local_connect ctrl ctrl_ch1
          (Controller.init_fat_tree pod swid name switch_data)) in

  (* forward arp traffic to local controller *)
  let dpid = Int64.add 0x200000L dpid in
  lwt _ = OF.add_local_slice flv (OP.Match.create_match ()) ctrl_ch2 dpid in
  
  try_lwt 
    Manager.create 
    (fun mgr intf id -> 
      match (OS.Netif.string_of_id id ) with 
      | "0" ->
        lwt _ = OSW.add_port mgr sw id in 
          OSW.local_connect sw mgr sw_ch2 
      | _ ->  (* let _ = log_dev id "" in *) OSW.add_port mgr sw id
    )
  with e -> return (Printf.eprintf "Error: %s" (Printexc.to_string e))

(****************************************************************
 * core switch 
 *****************************************************************)

let core_switch_inner ~pod ~i ~j () = 
  let dpid = Int64.of_int ((pod lsl 16) + (i lsl 8) + j) in 
  let switch_data = Controller.sw_data () in 
  let sw = OSW.create_switch ~verbose dpid in
  let flv = OF.create_flowvisor () in
  let ctrl = OC.init_controller ~verbose () in 
  let (ctrl_ch1, ctrl_ch2) = OSK.init_local_conn_state () in 
  let (sw_ch1, sw_ch2) = OSK.init_local_conn_state () in

  (* start local controller and local flowvisor threads in run mode *)
  let name = sprintf "core_%d_%d" i j in
  let _ = ignore_result (OF.local_listen flv sw_ch1) in 
  let _ = ignore_result 
          (OC.local_connect ctrl ctrl_ch1 (Controller.init_fat_tree_core pod i j
          name switch_data)) in

  (* forward arp traffic to local controller *)
  let dpid = Int64.add 0x200000L dpid in
  lwt _ = OF.add_local_slice flv (OP.Match.create_match ()) ctrl_ch2 dpid in   
  try_lwt 
    Manager.create 
    (fun mgr intf id -> 
      match (OS.Netif.string_of_id id) with 
      | "0" ->
          lwt _ = OSW.add_port mgr sw id in 
          OSW.local_connect sw mgr sw_ch2
      | _ ->  OSW.add_port mgr sw id
    )
  with e -> return (Printf.eprintf "Error: %s" (Printexc.to_string e))

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
module OE = OC.Event

let port = 55555
let use_dhcp = false

let pp = Printf.printf

(*********************************************************
 * Host configuration code and traffic generation code 
 *********************************************************)
let ip node_id = 
  Nettypes.(
    (ipv4_addr_of_tuple (10l,0l,0l,(Int32.of_int node_id)),
    ipv4_addr_of_tuple (255l,255l,255l,0l),
    [ ipv4_addr_of_tuple (10l,0l,0l,1l) ]
    )) 
      
(* Code to run on the end node *)
let host_inner host_id () =
  let config_host host_id =
    try_lwt 
      Manager.create (fun mgr interface id ->
        lwt _ = OS.Time.sleep 1. in 
        lwt _ = Manager.configure interface (`IPv4 (ip host_id)) in
        let _ = printf "[host%d] server setting up ip 10.0.0.%d\n%!" 
                    host_id host_id  in  
        match host_id with
        | 1 ->
(*            lwt _ = Datagram.UDPv4.recv mgr (None, port) echo_udp in *)
            lwt _ = 
              Net.Channel.listen mgr (`TCPv4 ((None, port), Client.echo ))
            in 
(*            lwt _ = Client.pttcp_server mgr port 30 in *)
              return ()
        | 2 -> 
            let dst_ip = Nettypes.ipv4_addr_of_tuple (10l,0l,0l,1l) in  
            let _ = Printf.printf "[host%d] %f: trying to connect client \n%!"
                    host_id (Clock.time ()) in 
(*            lwt _ = echo_client_udp mgr (dst_ip,port) in *)
            lwt _ = Net.Channel.connect mgr 
                  (`TCPv4 (None, (dst_ip, port), Client.echo_client )) in 
(*            lwt _ = Client.pttcp_client mgr dst_ip port 30 100000l in *)
              return ()
        | _ -> return (printf "Invalid node_id %d\n%!" host_id)
        )
    with e ->
      Printf.eprintf "Error: %s" (Printexc.to_string e); 
      return ()
  in  
    config_host host_id
(*
 * controller code 
 * *)
let controller_inner () = 
  try_lwt 
    Manager.create (fun mgr interface id ->
      let ip = 
        Nettypes.(
          (ipv4_addr_of_tuple (192l,168l,0l,2l),
          ipv4_addr_of_tuple (255l,255l,255l,0l), [])) in  
     lwt _ = Manager.configure interface (`IPv4 ip) in
     let dst = (None, 6633) in 
        OC.listen mgr dst Controller.init 
      )
  with exn -> 
    return (printf "%f: controller error: %s\n%!" (Clock.time ()) 
            (Printexc.to_string exn))

(*
 * flow visor node
 * *)
let init_slice mgr st =
  let _ = printf "[flowvisor] connecting to controller\n%!" in 
  let dst_ip = ((ipv4_addr_of_tuple (192l,168l,0l,2l)), 6633)  in
  lwt _ = Openflow.Flowvisor.add_slice mgr st
            (OP.Match.create_flow_match (OP.Wildcards.full_wildcard ()) () ) 
            dst_ip 0x10L in 
  return ()

let flowvisor_inner () = 
  let sw = Openflow.Flowvisor.create_flowvisor () in
  try_lwt
    let t, u = Lwt.task () in 
    Manager.create 
      (fun mgr interface id ->
           match id with
             | "0" -> 
                 let ip = 
                   Nettypes.(
                     (ipv4_addr_of_tuple (192l, 168l, 0l, 1l),
                      ipv4_addr_of_tuple (255l,255l,255l,0l), [])) in  
                      lwt _ = Manager.configure interface (`IPv4 ip) in
                 let _ = printf "XXXXXXX found intf 0\n%!" in
                 lwt _ = init_slice mgr sw in 
                  return ()
             | _ -> 
                 let ip = 
                   Nettypes.(
                     (ipv4_addr_of_tuple (10l,0l,(Int32.of_string id),1l),
                      ipv4_addr_of_tuple (255l,255l,255l,0l), [])) in
                 let _ = printf "[flowvisor] setting ip on id %s 10.0.%s.1\n%!" 
                        id id in 
                 lwt _ = Manager.configure interface (`IPv4 ip) in
                 lwt _ = Openflow.Flowvisor.listen sw mgr (None, 6633) in
                   t)
  with e ->
    Printf.eprintf "Error: %s" (Printexc.to_string e); 
    return ()

(****************************************************************
 * OpenFlow Switch configuration 
 *****************************************************************)
let switch_inner switch_id () = 
  let sw = Openflow.Ofswitch.create_switch (Int64.of_int switch_id) in
  try_lwt 
    Manager.create 
    (fun mgr interface id ->
       match (id) with 
         | "0" ->
             let _ = printf "[switch%d] setting device %s ip 10.0.%d.2\n%!" 
                      switch_id id switch_id in 
             let ip = 
               Nettypes.(
                 (ipv4_addr_of_tuple (10l,0l,(Int32.of_int switch_id),2l),
                  ipv4_addr_of_tuple (255l,255l,255l,0l), [])) in  
               lwt _ = Manager.configure interface (`IPv4 ip) in
               let dst_ip = ipv4_addr_of_tuple (10l,0l,(Int32.of_int switch_id),1l) in
               lwt _ = (Openflow.Ofswitch.connect sw mgr (dst_ip, 6633)) in 
                return ()
         | _ -> 
             let Some(node_name) = Lwt.get OS.Topology.node_name in
             let msg = Rpc.Dict [
               ("name", (Rpc.String node_name));
               ("dev_id", (Rpc.String id));
               ("ip", (Rpc.String ""));] in
             let _ = printf "XXXXXXXXX %s\n%!" (Jsonrpc.to_string msg) in 
             let _ = OS.Console.broadcast "node_dev" (Jsonrpc.to_string msg) in 
               Openflow.Ofswitch.add_port mgr sw id
    )
  with e ->
    Printf.eprintf "Error: %s" (Printexc.to_string e); 
    return ()



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
let ip node_id = 
  Nettypes.(
    (ipv4_addr_of_tuple (10l,0l,1l,(Int32.of_int node_id)),
    ipv4_addr_of_tuple (255l,255l,255l,0l),
    [ ipv4_addr_of_tuple (10l,0l,1l,1l) ]
    )) 
      
(* Code to run on the end node *)
let host_inner host_id () =
  lwt _ = OS.Time.sleep 1.0 in 
  let config_host host_id =
    try_lwt 
      Manager.create (fun mgr interface id ->
        match host_id with
        | 1 ->
          lwt _ = Manager.configure interface (`IPv4 (ip host_id)) in
          let _ = printf "[host%d] server setting up ip 10.0.1.%d\n%!" 
                    host_id host_id  in  
(*             Datagram.UDPv4.recv mgr (None, port) echo_udp *)
(*          Net.Channel.listen mgr (`TCPv4 ((None, port), Client.echo )) *)
          Client.pttcp_udp_server mgr port 1
        | 2 -> 
          let dst_ip = Nettypes.ipv4_addr_of_tuple (10l,0l,1l,1l) in  
          let _ = printf "[host%d] client setting up ip 10.0.1.%d\n%!" 
                    host_id host_id  in  
          lwt _ = Manager.configure interface (`IPv4 (ip host_id)) in
          let _ = Printf.printf "[host%d] %f: trying to connect client \n%!"
                    host_id (Clock.time ()) in 
(*             echo_client_udp mgr (dst_ip,port) *)
(* Net.Channel.connect mgr (`TCPv4 (None, (dst_ip, port), Client.echo_client ))
      * *)
          Client.pttcp_udp_client ~debug:true mgr dst_ip port 1 100000l
        | _ -> return (printf "Invalid node_id %d\n%!" host_id)
        )
    with e ->
      Printf.eprintf "Error: %s" (Printexc.to_string e); 
      return ()
  in  
    config_host host_id

let controller_inner () = 
  try_lwt 
    Manager.create (fun mgr interface id ->
      let ip = 
        Nettypes.(
          (ipv4_addr_of_tuple (10l,0l,0l,2l),
          ipv4_addr_of_tuple (255l,255l,255l,0l), [])) in  
     lwt _ = Manager.configure interface (`IPv4 ip) in
      let dst = ((Nettypes.ipv4_addr_of_tuple (10l,0l,0l,1l)), 6633) in 
        OC.connect mgr dst (Controller.init ~handle_arp:true "ctrl"
        (Controller.sw_data ()))
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
  let sw = Openflow.Ofswitch.create_switch 0x1L in
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

(*   OS.Topology.add_external_dev "nstap0" "node1" "10.0.1.0" "255.255.255.0" *)

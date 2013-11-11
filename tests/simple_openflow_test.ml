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
open Pttcp.Pttcp_tcp

let port = 55555
let use_dhcp = false

module OP = Openflow.Ofpacket
module OC = Openflow.Ofcontroller
module OE = OC.Event

module V = Vchan.Make(Xs) 

let pp = Printf.printf

(*********************************************************
 * Host configuration code and traffic generation code 
 *********************************************************)
let ip node_id = 
  Nettypes.(
    (Ipaddr.V4.make 10l 0l 1l (Int32.of_int node_id)),
    (Ipaddr.V4.make 255l 255l 255l 0l),
    [(Ipaddr.V4.make 10l 0l 1l 1l) ]
  ) 

(* Code to run on the end node *)
let host_inner host_id () =
  let config_host host_id =
    try_lwt 
      Manager.create (fun mgr interface id ->
          match host_id with
          | 1 ->
              lwt _ = OS.Time.sleep 2.0 in 
            lwt _ = Manager.configure interface (`IPv4 (ip host_id)) in
            let _ = printf "[host%d] server setting up ip 10.0.1.%d\n%!" 
                host_id host_id  in  
            (* Datagram.UDPv4.recv mgr (None, port) echo_udp *)
            (* Client.pttcp_server mgr port 30 *)
            lwt _ = Client.bursty_server mgr port in  
            return ()  
            (*Net.Channel.listen mgr (`TCPv4 ((None, port), Client.echo ))  *)
            (* return () *)
          | 2 -> 
            let _ = Printf.printf "listening server starting\n%!" in 
            lwt vwrite = V.server ~evtchn_h:(Eventchn.init ()) ~domid:0 
              ~xs_path:"data/vchan-write" ~read_size:4080 ~write_size:4080
              ~persist:true in
            lwt vread = V.server ~evtchn_h:(Eventchn.init ()) ~domid:0 
              ~xs_path:"data/vchan-read" ~read_size:4080 ~write_size:4080
              ~persist:true in  
            let dst_ip = Ipaddr.V4.make 10l 0l 1l 1l in  
            let _ = printf "[host%d] client setting up ip 10.0.1.%d\n%!" 
                host_id host_id  in  
            lwt _ = Manager.configure interface (`IPv4 (ip host_id)) in
(*            lwt _ = OS.Time.sleep 8.0 in *)
            lwt _ = OS.Time.sleep 3.0 in 
            let _ = Printf.printf "[host%d] %f: trying to connect client \n%!"
                host_id (Clock.time ()) in 
            (*             echo_client_udp mgr (dst_ip,port) *)
(*            Net.Channel.connect mgr (`TCPv4 (None, (dst_ip, port),
                                             Client.echo_client )) *)
(*            lwt _ = OS.Time.sleep 5.0 in *)

(*            let size_model = (Constant 1e7) in 
            let delay_model = (Exp (100.0)) in  *)

            let size_model = (Constant 5e6) in 
            let delay_model = (Pareto(50., 2.0)) in 
           
(*            let size_model = (Constant 1e7) in 
            let delay_model = (Exp(100.0)) in   *)
            let st = 
              init_pttcp_state_t (Trace_client([dst_ip], port, 3, size_model,
                                               delay_model) ) true in 
            lwt _ = generate_traffic mgr st <?> OS.Time.sleep 60.0 in
            let _ = Printf.printf "starting to send results back to server...\n%!" in 
            let msg = String.create 2040 in 
            let test = String.create 1 in 
            let rec send_results count = function
              | [] -> 
                lwt () = V.write_from_exactly vwrite msg 0 ((count+1)*51) in
                  return ()
              | s :: tl -> 
                let record = Printf.sprintf "%016.6f:%016.6f:%06d:%09ld;"
                    s.beg_ts s.end_ts s.req_id s.size in
                let _ = String.blit record 0 msg (count * 51) 51 in 
                if (count = 39) then 
                  let _ = printf "sening data\n%s\n%!" msg in 
                  lwt () = V.write_from_exactly vwrite msg 0 2040 in
                  let _ = printf "sent data and waiting for ack\n%!" in 
                  lwt msg = V.read_into_exactly vread test 0 1 in 
                  let _ = printf "ack received %d\n%!" (String.length test) in 
                  send_results  0 tl 
                else 
                  send_results (count + 1) tl
            in
            lwt _ = send_results 0 (stats st) in 
            return (Printf.printf "host finished\n%!") 
(*            Client.pttcp_client ~debug:false mgr dst_ip port 30 100000l *)
          | _ -> return (printf "Invalid node_id %d\n%!" host_id)
        )
    with e ->
      Printf.eprintf "Error: %s" (Printexc.to_string e); 
      return ()
  in  
  config_host host_id


let controller_inner () = 
  try_lwt 
(*    let _ = Printf.printf "listening server starting\n%!" in 
    lwt vwrite = V.server ~evtchn_h:(Eventchn.init ()) ~domid:0 
      ~xs_path:"data/vchan-write" ~read_size:4080 ~write_size:4080
      ~persist:true in
    lwt vread = V.server ~evtchn_h:(Eventchn.init ()) ~domid:0 
      ~xs_path:"data/vchan-read" ~read_size:4080 ~write_size:4080
      ~persist:true in *)
    Manager.create (fun mgr interface id ->
        let ip = 
          Nettypes.(
            (Ipaddr.V4.make  192l 168l 0l 2l,
             Ipaddr.V4.make  255l 255l 255l 0l, [])) in  
        lwt _ = Manager.configure interface (`IPv4 ip) in
        let dst = ((Ipaddr.V4.make 192l 168l 0l 1l), 6633) in
        printf "controller connecting to server\n%!";
(*        lwt _ = OS.Time.sleep 5.0 in  *)
        lwt _ = OS.Time.sleep 1.0 in 
        printf "connecting to server\n%!";
        OC.connect mgr dst ~verbose:false (Controller.init ~handle_arp:true "ctrl"
                              (Controller.sw_data ()))
      )
  with exn -> 
    return (printf "%f: controller error: %s\n%!" (Clock.time ()) 
              (Printexc.to_string exn))


(****************************************************************
 * OpenFlow Switch configuration 
 *****************************************************************)
let switch_inner () =

  let model = Switch_model.Ofswitch_model.({flow_insert=0.; flow_update=0.; pktin_rate=50.;
               pktin_delay=0.002;stats_delay=0.; pktout_delay=0.;}) in
  let sw = Switch_model.Ofswitch_model.create_switch ~verbose:false 0x1L model in
  try_lwt
    let _ = Printf.printf "listening server starting\n%!" in 
(*    lwt vwrite = V.server ~evtchn_h:(Eventchn.init ()) ~domid:0 
      ~xs_path:"data/vchan-write" ~read_size:4080 ~write_size:4080
      ~persist:true in
    lwt vread = V.server ~evtchn_h:(Eventchn.init ()) ~domid:0 
      ~xs_path:"data/vchan-read" ~read_size:4080 ~write_size:4080
      ~persist:true in *)
    Manager.create 
      (fun mgr interface id ->
         let _ = pp "[switch] adding intf %s\n%!" (OS.Netif.string_of_id id) in 
         match (OS.Netif.string_of_id id) with 
         | "0" ->
           let ip = 
             Nettypes.(
               (Ipaddr.V4.make 192l 168l 0l 1l,
                Ipaddr.V4.make 255l 255l 255l 0l, [])) in  
           lwt _ = Manager.configure interface (`IPv4 ip) in
           lwt _ = Switch_model.Ofswitch_model.listen sw mgr (None, 6633) in 
           return ()
         | _ ->  Switch_model.Ofswitch_model.add_port mgr sw id
      )
  with e ->
    Printf.eprintf "Error: %s" (Printexc.to_string e); 
    return () 

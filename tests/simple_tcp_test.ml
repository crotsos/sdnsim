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
open Pttcp
open Pttcp.Pttcp_tcp
open Activations

module V = Vchan.Make(Xs)

let port = 55555

let pp = Printf.printf
let ep = Printf.eprintf
let ip node_id = 
  Nettypes.(
    (Ipaddr.V4.make  10l 0l 1l (Int32.of_int node_id)),
    (Ipaddr.V4.make  255l 255l 255l 0l),
    [ (Ipaddr.V4.make 10l 0l 1l 1l) ]
    ) 
      
let print_time () = 
  while_lwt true do
    OS.Time.sleep 1.0 >>
      return ( printf "%03.6f: running process..\n%!" (OS.Clock.time ()) )
  done

(* Code to run on the end node *)
let host_inner host_id () =
  (* lwt res = V.read vread 100 in 
  let _ = Printf.printf "[sdnsim-vchan] read from vm :%s\n%!" res in
  lwt _ = V.write_from_exactly vwrite msg 0 (String.length msg) in *)

(*  lwt res = V.read vread 100 in  *)
(*  let _ = Printf.printf "[sdnsim-vchan] read from vm :%s\n%!" res in *)
  
  (* let _ = Lwt.ignore_result (print_time ()) in   *)
    try_lwt 
      Manager.create (fun mgr interface id ->
          match host_id with
          | 1 ->
            lwt _ = Manager.configure interface (`IPv4 (ip host_id)) in
            Printf.printf "%f: trying to connect server\n%!" (Clock.time ());
            (*             Datagram.UDPv4.recv mgr (None, port) echo_udp *)
(*            lwt _ = Net.Channel.listen mgr (`TCPv4 ((None, port),
            Client.echo)) in *)
            (*          lwt _ = Client.pttcp_udp_server mgr port 1 in *)
(*            lwt _ = Client.pttcp_server mgr port 30 in *)
            lwt _ = Client.bursty_server mgr port in 
            return (printf "server returned\n%!")
          | 2 ->
            let _ = Printf.printf "listening server starting\n%!" in 
            lwt vwrite = V.server ~evtchn_h:(Eventchn.init ()) ~domid:0 
              ~xs_path:"data/vchan-write" ~read_size:4080 ~write_size:4080
              ~persist:true in
            lwt vread = V.server ~evtchn_h:(Eventchn.init ()) ~domid:0 
              ~xs_path:"data/vchan-read" ~read_size:4080 ~write_size:4080
              ~persist:true in
            let _ = Printf.printf "listening server started\n%!" in
            lwt _ = OS.Time.sleep 1.0 in 
            let dst_ip = Ipaddr.V4.make 10l 0l 1l 1l in  
            lwt _ = Manager.configure interface (`IPv4 (ip host_id)) in
            (*          echo_client_udp mgr (dst_ip,port) *)
            Printf.printf "%f: connecting client ip\n%!" (Clock.time ());

(*             lwt _ = 
              Net.Channel.connect mgr 
                (`TCPv4 (None, (dst_ip, port), Client.echo_client)) in *)
(*            lwt _ = Client.pttcp_client mgr dst_ip port 30  100000000l in *)
            (*          lwt _ = Client.pttcp_udp_client mgr dst_ip port 1 100000000l in *)
            let size_model = (Constant 1e6) in 
            let delay_model = (Pareto(8.0, 2.0)) in  
            let st = 
              init_pttcp_state_t (Trace_client([dst_ip], port, 3, size_model,
                                               delay_model) ) false in 
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
            return (printf "client returned\n%!")
          | _ -> return (printf "Invalid node_id %d\n%!" host_id)
        )
    with e -> return (ep "Error: %s" (Printexc.to_string e))

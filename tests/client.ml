(*2
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
open Net
open Printf
open Pttcp.Pttcp_tcp

let pp = Printf.printf

(*********************************************************
 * Host configuration code and traffic generation code 
 *********************************************************)

let rec echo dst chan = 
  try_lwt
    while_lwt true do
      lwt buf = Channel.read_some chan in
(*        let _ = Printf.printf "%f: read %d\n%!" 
         (OS.Clock.time ()) (Cstruct.len buf) in *)
      return () 
    done
  with Nettypes.Closed -> return (pp "closed!")

let rec echo_client chan = 
  try_lwt
    let data = Cstruct.of_bigarray (OS.Io_page.get 1) in 
    while_lwt true do 
      let _ = Channel.write_buffer chan data in
(*  Printf.printf "%f: Writing new buffer....\n%!" (OS.Clock.time ()); *)
      Channel.flush chan
    done
  with 
    | Nettypes.Closed -> return (pp "closed!")
    | ex ->  return (Printf.printf "Error:%s\n%!" (Printexc.to_string ex))

let rec echo_udp dst buf = 
  return ( (* Printf.printf "%f: read %d\n%!" (Clock.time ()) (Cstruct.len buf)
  *) )
  
let rec echo_client_udp mgr dst =
  try_lwt
    let data = Cstruct.sub OS.Io_page.(to_cstruct (get 1)) 0 1460 in
    let rec send_data () = 
      lwt _ = Datagram.UDPv4.send mgr dst data in
      lwt () = OS.Time.sleep 1.0 in
(* Printf.printf "%f: Writing new buffer....\n%!" (Clock.time ()); *)
      send_data ()
    in
    send_data ()
  with 
    | Nettypes.Closed -> return (pp "closed!")
    | ex ->  return (Printf.printf "Eroor:%s\n%!" (Printexc.to_string ex))

let pttcp_server ?(debug=false) mgr port count =
  let st = init_pttcp_state_t (Srv(count, port)) debug in 
  generate_traffic mgr st
          
let pttcp_client ?(debug=false) mgr ip port conns size =
  let model = Cts_ctl(conns, size, ip, conns, port) in
  let st = init_pttcp_state_t model debug in 
  (generate_traffic mgr st)

let pttcp_udp_server ?(debug=false) mgr port count =
  Pttcp.Pttcp_udp.(generate_traffic mgr (Srv(count, port)) (Constant 0.1) 1400 debug)
          
let pttcp_udp_client ?(debug=false) mgr ip port conns size =
  Pttcp.Pttcp_udp.(let model = Cts_ctl(conns, size, ip, conns, port) in
  (generate_traffic mgr model  (Constant 0.0001) 1460 debug ))

let bursty_server mgr port = 
  let st = init_pttcp_state_t (Trace_server port) false in 
  generate_traffic mgr st
let bursty_client mgr port dhosts seed =
  let size_model = (Constant 1e6) in 
  let delay_model = (Pareto(7.5, 2.0)) in  
  let st = 
    init_pttcp_state_t (Trace_client(dhosts, port, seed, size_model,
  delay_model) ) false in 
  generate_traffic mgr st 


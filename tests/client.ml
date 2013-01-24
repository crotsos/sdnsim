open Lwt
open OS
open OS.Console
open Net
open Printf
open Pttcp.Pttcp_tcp

let pp = Printf.printf

(*********************************************************
 * Host configuration code and traffic generation code 
 *********************************************************)

let rec echo dst chan = 
  try_lwt
    lwt _ =
      while_lwt true do
        lwt buf = Channel.read_some chan in
(*        let _ = Printf.printf "%f: read %d\n%!" 
         (Clock.time ())
         (Cstruct.len buf) in *)
          return () 
      done
    in
      return ()
  with Nettypes.Closed -> return (pp "closed!")

let rec echo_client chan = 
  try_lwt
    let data = String.create 1460 in 
    let rec send_data () = 
        let _ = Channel.write_string chan data 0 (String.length data) in
(*         Printf.printf "%f: Writing new buffer....\n%!" (Clock.time ());   *)
        lwt _ = Channel.flush chan in
        lwt _ = send_data () in 
          return ()
    in
      send_data ()
  with 
    | Nettypes.Closed -> return (pp "closed!")
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
    | Nettypes.Closed -> return (pp "closed!")
    | ex ->  return (Printf.printf "Eroor:%s\n%!" (Printexc.to_string ex))

let pttcp_server ?(debug=false) mgr port count =
  generate_traffic mgr (Srv(count, port)) debug
          
let pttcp_client ?(debug=false) mgr ip port conns size =
  let model = Cts_ctl(conns, size, ip, conns, port) in
  (generate_traffic mgr model debug )
 

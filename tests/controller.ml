open Lwt
open OS
open OS.Console
open Net
open Printf

let pp = Printf.printf

module OP = Openflow.Ofpacket
module OC = Openflow.Ofcontroller
module OE = OC.Event

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
      | OE.Packet_in (inp, _, buf, dat, dp) -> (inp, buf, dat, dp)
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



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
  mutable fib : (int32 * int * int) list;
  mac_cache: (OP.eaddr, OP.Port.t) Hashtbl.t;
  ip_cache : (int32, OP.eaddr) Hashtbl.t;
  mutable dpid: OP.datapath_id list;
  mutable of_ctrl: OC.t list;
  req_count: int ref; 
}

cstruct arp {
  uint8_t dst[6];
  uint8_t src[6];
  uint16_t ethertype;
  uint16_t htype;
  uint16_t ptype;
  uint8_t hlen;
  uint8_t plen;
  uint16_t op;
  uint8_t sha[6];
  uint32_t spa;
  uint8_t tha[6];
  uint32_t tpa
} as big_endian

let get_arp_reply mac dst_ip src_ip = 
  let buf = OS.Io_page.to_cstruct (OS.Io_page.get ()) in 
  let _ = set_arp_dst mac 0 buf in 
  let _ = set_arp_src "\xfe\xff\xff\xff\xff\xff" 0 buf in 
  let _ = set_arp_ethertype buf 0x0806 in  (* ARP *)
  let _ = set_arp_htype buf 1 in
  let _ = set_arp_ptype buf 0x0800 in  (* IPv4 *)
  let _ = set_arp_hlen buf 6 in  (* ethernet mac size *)
  let _ = set_arp_plen buf 4 in  (* ipv4 size *)
  let _ = set_arp_op buf 2 in 
  let _ = set_arp_sha "\xfe\xff\xff\xff\xff\xff" 0 buf in 
  let _ = set_arp_spa buf src_ip in 
  let _ = set_arp_tha mac 0 buf in 
  let _ = set_arp_tpa buf dst_ip in 
  (* Resize buffer to sizeof arp packet *)
    Cstruct.sub buf 0 sizeof_arp

  let create_gratituous_arp dl_src nw_src =
    let bits = OS.Io_page.to_cstruct (OS.Io_page.get ()) in 
    let _ = set_arp_dst "\xff\xff\xff\xff\xff\xff" 0 bits in
    let _ = set_arp_src dl_src 0 bits in 
    let _ = set_arp_ethertype bits 0x0806 in 
    let _ = set_arp_htype bits 1 in 
    let _ = set_arp_ptype bits 0x0800 in 
    let _ = set_arp_hlen bits 6 in 
    let _ = set_arp_plen bits 4 in 
    let _ = set_arp_op bits 1 in 
    let _ = set_arp_sha dl_src 0 bits in 
    let _ = set_arp_spa bits nw_src in 
    let _ = set_arp_tha "\x00\x00\x00\x00\x00\x00" 0 bits in 
    let _ = set_arp_tpa bits nw_src in
      Cstruct.sub bits 0 sizeof_arp

let sw_data () = 
  { fib=[]; mac_cache = Hashtbl.create 0; dpid = []; of_ctrl = []; 
  req_count=(ref 0);ip_cache = Hashtbl.create 0; } 

let datapath_join_cb switch_data ?(ip=None) controller dpid evt =
  let dp = 
    match evt with
      | OE.Datapath_join (c, _) -> c
      | _ -> invalid_arg "bogus datapath_join event match!" 
  in
  let _ = switch_data.dpid <- switch_data.dpid @ [dp] in 
  return (pp "+ datapath:0x%012Lx\n" dp)

let packet_in_cb ?(handle_arp=true) name switch_data controller dpid evt =
  incr switch_data.req_count;
  let (in_port, buffer_id, data, dp) = 
    match evt with
      | OE.Packet_in (inp, _, buf, dat, dp) -> (inp, buf, dat, dp)
      | _ -> invalid_arg "bogus datapath_join event match!"
  in

  (* Parse Ethernet header *)
  let m = OP.Match.raw_packet_to_match in_port data in 
(*  let _ = printf "[openflow] received a packet from ip %s on ctrl %s\n%!" 
          (Uri_IP.ipv4_to_string m.OP.Match.nw_src) name in *)

  (* Store src mac address and incoming port *)
  match (handle_arp, m.OP.Match.dl_type) with
  | (_, 0x88cc) -> return ()
  | (_, 0x0800)
  | (true, 0x0806) -> begin 
    let ix = m.OP.Match.dl_src in
    let _ = Hashtbl.replace switch_data.mac_cache ix in_port in
 
    (* check if I know the output port in order to define what type of message
     * we need to send *)
    let ix = m.OP.Match.dl_dst in
    if ( (ix = "\xff\xff\xff\xff\xff\xff")
        || (not (Hashtbl.mem switch_data.mac_cache ix)) ) then (
          let bs =
            (OP.Packet_out.create ~buffer_id:buffer_id
              ~actions:[ OP.(Flow.Output(m.OP.Match.in_port, 2000))]
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
             OC.send_data controller dpid (OP.Flow_mod (h, pkt)) )    
  end
  | (_, _) -> 
    let ix = m.OP.Match.dl_src in
    let _ = Hashtbl.replace switch_data.mac_cache ix in_port in
      return ()


let test_packet_out controller dpid actions data in_port = 
  let buffer_id = -1l in 
  let bs = OP.Packet_out.create ~buffer_id ~actions ~data ~in_port () in
  let h = OP.Header.create OP.Header.PACKET_OUT 0 in
    OC.send_data controller dpid (OP.Packet_out (h, bs)) 

let router_packet_in_cb name switch_data controller dpid evt =
  incr switch_data.req_count;
  let (in_port, buffer_id, data, dp) = 
    match evt with
      | OE.Packet_in (inp, _, buf, dat, dp) -> (inp, buf, dat, dp)
      | _ -> invalid_arg "bogus datapath_join event match!"
  in

  (* Parse Ethernet header *)
  let m = OP.Match.raw_packet_to_match in_port data in 
(*  let _ = printf "[openflow] received a packet %s - %s %x on ctrl %s\n%!" 
          (Uri_IP.ipv4_to_string m.OP.Match.nw_src) 
          (Uri_IP.ipv4_to_string m.OP.Match.nw_dst) 
          m.OP.Match.dl_type name in *)
  
  let ix = m.OP.Match.dl_src in
  let _ = Hashtbl.replace switch_data.mac_cache ix in_port in

  let _ = 
    if ((m.OP.Match.dl_type = 0x0806) || (m.OP.Match.dl_type = 0x0800)) then
      Hashtbl.replace switch_data.ip_cache
        m.OP.Match.nw_src m.OP.Match.dl_src
  in

  (* Store src mac address and incoming port *)
  match (m.OP.Match.dl_type) with
  | (0x88cc) -> return ()
  | 0x0806 when (Int32.logand m.OP.Match.nw_dst 0xffl = 1l) -> 
      let ix = m.OP.Match.dl_src in
      let data = get_arp_reply ix m.OP.Match.nw_src m.OP.Match.nw_dst in 
      let bs =
        (OP.Packet_out.create ~buffer_id:(-1l)
              ~actions:[ OP.(Flow.Output(OP.Port.All, 2000))]
              ~data ~in_port:(OP.Port.No_port) () ) in
      let h = OP.Header.create OP.Header.PACKET_OUT 0 in
        OC.send_data controller dpid (OP.Packet_out (h, bs))
  | 0x0800  
      when ((Int32.logand m.OP.Match.nw_dst 0xffffff00l) <>
            (Int32.logand m.OP.Match.nw_src 0xffffff00l)) -> begin
     try_lwt
     let ix = Hashtbl.find switch_data.ip_cache m.OP.Match.nw_dst in 
     let out_port = Hashtbl.find switch_data.mac_cache ix in
     let flags=OP.Flow_mod.({send_flow_rem=true;emerg=false;overlap=false;}) in
     let actions = [OP.(Flow.Set_dl_dst(ix));
                    OP.(Flow.Set_dl_src("\xfe\xff\xff\xff\xff\xff"));
                    OP.(Flow.Output(out_port, 2000))] in 
     lwt _ =
       if (buffer_id = -1l) then
            (* Need to send also the packet in cache the packet is not cached *)
         let bs =
           OP.Packet_out.create ~buffer_id ~actions ~data ~in_port ()  in
          let h = OP.Header.create OP.Header.PACKET_OUT 0 in
            OC.send_data controller dpid (OP.Packet_out (h, bs))
        else return () in
    lwt _ =  test_packet_out controller dpid actions data in_port in 
    let pkt =
        OP.Flow_mod.create m 0_L OP.Flow_mod.ADD ~hard_timeout:0
           ~idle_timeout:0 ~buffer_id:(Int32.to_int buffer_id)  ~flags
           actions () in
     let h = OP.Header.create OP.Header.FLOW_MOD 0 in
      OC.send_data controller dpid (OP.Flow_mod (h, pkt)) 
     with Not_found -> return ()
  end
  | (_) -> 
       (* let _ = printf "XXXXXX No idea what this is\n%!" in *)
       return ()
     
let init ?(handle_arp=true) name switch_data st = 
  if (not (List.mem st switch_data.of_ctrl)) then
    switch_data.of_ctrl <- (([st] @ switch_data.of_ctrl));
  OC.register_cb st OE.DATAPATH_JOIN (datapath_join_cb switch_data);
  OC.register_cb st OE.PACKET_IN (packet_in_cb ~handle_arp name switch_data)

let init_router name switch_data st =
  let _ = 
    if (not (List.mem st switch_data.of_ctrl)) then
      switch_data.of_ctrl <- (([st] @ switch_data.of_ctrl))
  in 
  let _ = OC.register_cb st OE.DATAPATH_JOIN (datapath_join_cb switch_data) in
  OC.register_cb st OE.PACKET_IN  (router_packet_in_cb name switch_data)


(******************************************
 *i Fat tree controller 
 * *)
let fib_match st dst_ip =
  let ip_match ip mask dst_ip = 
    let netmask = Int32.shift_left 0xffffffffl (32 - mask) in 
      (Int32.logand netmask dst_ip) = (Int32.logand netmask ip)
  in
  let (m, p) = List.fold_right (
    fun (ip, mask, port) (m, p) -> 
      if ((m <= mask) && (ip_match ip mask dst_ip)) then 
          (mask, port)
        else
          (m,p)
  ) st.fib (0, 0) in 
  if (m = 0) then 
    failwith (sprintf "Invalid IP address %lx" dst_ip ) 
  else 
    (m, p)

let fat_tree_ip pod swid hid = 
  Int32.add 0x0a000000l
    (Int32.add (Int32.of_int (pod lsl 16)) 
      (Int32.add (Int32.of_int (swid lsl 8)) 
        (Int32.of_int hid) ) )


let fat_tree_packet_in_cb pod swid name switch_data controller dpid evt =
  incr switch_data.req_count;
  let (in_port, buffer_id, data, dp) = 
    match evt with
      | OE.Packet_in (inp, _, buf, dat, dp) -> (inp, buf, dat, dp)
      | _ -> invalid_arg "bogus datapath_join event match!"
  in
  let m = OP.Match.raw_packet_to_match in_port data in 
  
  (* Store src mac address and incoming port *)
  let ix = m.OP.Match.dl_src in
  let _ = Hashtbl.replace switch_data.mac_cache ix in_port in

  let _ = 
    if ((m.OP.Match.dl_type = 0x0806) || (m.OP.Match.dl_type = 0x0800)) then
      Hashtbl.replace switch_data.ip_cache m.OP.Match.nw_src m.OP.Match.dl_src
  in
  let ip = fat_tree_ip pod swid 1 in 


  match (m.OP.Match.dl_type) with
  | (0x88cc) -> return ()
  | 0x0806  -> 
    let ix = m.OP.Match.dl_src in
    let _ = Hashtbl.replace switch_data.ip_cache m.OP.Match.nw_src
            m.OP.Match.dl_src in
      if (m.OP.Match.nw_dst = ip) then 
        let data = get_arp_reply ix m.OP.Match.nw_src ip in 
        let bs =
          (OP.Packet_out.create ~buffer_id:(-1l)
          ~actions:[ OP.(Flow.Output(OP.Port.All, 2000))]
          ~data ~in_port:(OP.Port.No_port) () ) in
        let h = OP.Header.create OP.Header.PACKET_OUT 0 in
        lwt _ = OC.send_data controller dpid (OP.Packet_out (h, bs)) in 
          return ()
      else return ()
  | _ -> begin
    let (_, out_port) = fib_match switch_data m.OP.Match.nw_dst in

    let flags=OP.Flow_mod.({send_flow_rem=true;emerg=false;overlap=false;}) in
    let actions = 
      if (Hashtbl.mem switch_data.ip_cache m.OP.Match.nw_dst) then
        let ix = Hashtbl.find switch_data.ip_cache m.OP.Match.nw_dst in
        [
          OP.(Flow.Set_dl_src("\xfe\xff\xff\xff\xff\xff"));
          OP.(Flow.Set_dl_dst(ix));
        OP.(Flow.Output( (OP.Port.Port(out_port)), 2000))]
      else
        [ OP.(Flow.Output( (OP.Port.Port(out_port)), 2000)) ]
    in 
    lwt _ =
      if (buffer_id = -1l) then
        (* Need to send also the packet in cache the packet is not cached *)
        let bs =
          OP.Packet_out.create ~buffer_id ~actions ~data ~in_port ()  in
        let h = OP.Header.create OP.Header.PACKET_OUT 0 in
          OC.send_data controller dpid (OP.Packet_out (h, bs))
      else return () in
    let pkt =
      OP.Flow_mod.create m 0_L OP.Flow_mod.ADD ~hard_timeout:0
      ~idle_timeout:0 ~buffer_id:(Int32.to_int buffer_id)  ~flags
      actions () in
    let h = OP.Header.create OP.Header.FLOW_MOD 0 in
      OC.send_data controller dpid (OP.Flow_mod (h, pkt)) 
  end

let init_fat_tree pod swid name switch_data st =
  let _ = 
    if (not (List.mem st switch_data.of_ctrl)) then
      switch_data.of_ctrl <- (([st] @ switch_data.of_ctrl))
  in
  (* create the fib *)
  let _ = 
    if (swid < 2) then 
      switch_data.fib <- 
        [((fat_tree_ip pod swid 2) , 32, 10);
        ((fat_tree_ip pod swid 3), 32, 11); 
        (0x0a000000l, 8, 12)]
    else 
      switch_data.fib <- 
        [((fat_tree_ip pod 0 0), 24, 10); 
        ((fat_tree_ip pod 1 0), 24, 11); 
        (0x0a000000l, 8, 12)]
  in
  let _ = OC.register_cb st OE.DATAPATH_JOIN (datapath_join_cb switch_data 
        ~ip:(Some(fat_tree_ip pod swid 1))) in
  let _ = OC.register_cb st OE.PACKET_IN  (fat_tree_packet_in_cb pod swid name
  switch_data) in 
    ()
 
let init_fat_tree_core pod i j name switch_data st =
  let _ = 
    if (not (List.mem st switch_data.of_ctrl)) then
      switch_data.of_ctrl <- (([st] @ switch_data.of_ctrl))
  in
  (* create the fib *)
  let _ = 
      switch_data.fib <- 
        [((fat_tree_ip 0 0 0) , 16, 10);
        ((fat_tree_ip 1 0 0), 16, 11); 
        ((fat_tree_ip 2 0 0), 16, 12); 
        ((fat_tree_ip 3 0 0), 16, 13); 
        (* (0x0a000000l, 8, 13) *) ]
  in
  let _ = OC.register_cb st OE.DATAPATH_JOIN (datapath_join_cb switch_data 
        ~ip:(Some(fat_tree_ip 4 i j))) in
  let _ = OC.register_cb st OE.PACKET_IN  (fat_tree_packet_in_cb 4 i name switch_data) in 
    ()

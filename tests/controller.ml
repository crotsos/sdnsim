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
  mutable fib : (int32 * int * int list) list;
  mac_cache: (OP.eaddr, OP.Port.t) Hashtbl.t;
  ip_cache : (int32, OP.eaddr) Hashtbl.t;
  mutable dpid: OP.datapath_id list;
  mutable of_ctrl: OC.t list;
  req_count: int ref; 
  port_load: (OP.Port.t, float) Hashtbl.t; 
  flows : (OP.Match.t, OP.Port.t *int64 * int64) Hashtbl.t;
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

let get_arp_reply src_mac dst_mac dst_ip src_ip = 
  let buf = OS.Io_page.to_cstruct (OS.Io_page.get ()) in 
  let _ = set_arp_dst dst_mac 0 buf in 
  let _ = set_arp_src src_mac 0 buf in 
  let _ = set_arp_ethertype buf 0x0806 in  (* ARP *)
  let _ = set_arp_htype buf 1 in
  let _ = set_arp_ptype buf 0x0800 in  (* IPv4 *)
  let _ = set_arp_hlen buf 6 in  (* ethernet mac size *)
  let _ = set_arp_plen buf 4 in  (* ipv4 size *)
  let _ = set_arp_op buf 2 in 
  let _ = set_arp_sha src_mac 0 buf in 
  let _ = set_arp_spa buf src_ip in 
  let _ = set_arp_tha dst_mac 0 buf in 
  let _ = set_arp_tpa buf dst_ip in 
  (* Resize buffer to sizeof arp packet *)
    Cstruct.sub buf 0 sizeof_arp

let create_gratituous_arp dl_src nw_src =
  get_arp_reply dl_src "\xff\xff\xff\xff\xff\xff" nw_src nw_src  

let sw_data () = 
  { fib=[]; mac_cache = Hashtbl.create 0; dpid = []; of_ctrl = []; 
  req_count=(ref 0);ip_cache = (Hashtbl.create 0); port_load=(Hashtbl.create 0);
  flows=(Hashtbl.create 0);} 

let poll_flow_stats ctrl st dpid ts = 
  let f = OP.Stats.({ty=0;flags=0;}) in 
  let m = OP.Match.create_match () in 
  let req = OP.Stats.Flow_req (f, m, OP.Stats.All, OP.Port.No_port) in  
  let h = OP.Header.(create STATS_REQ 0) in
  while_lwt true do
    lwt _ = OS.Time.sleep ts in 
      OC.send_data ctrl dpid (OP.Stats_req (h, req)) 
  done 

let test_packet_out controller dpid actions buffer_id data in_port = 
  let bs = OP.Packet_out.create ~buffer_id ~actions ~data ~in_port () in
  let h = OP.Header.create OP.Header.PACKET_OUT 0 in
    OC.send_data controller dpid (OP.Packet_out (h, bs)) 

let datapath_join_cb st ?(stats=None) ?(ip=None) controller dpid evt =
  let dp, port = 
    match evt with
      | OE.Datapath_join (c, p) -> c, p
      | _ -> invalid_arg "bogus datapath_join event match!" 
  in
  let _ = printf "datapath 0x%Lx joined...\n%!" dpid in
  let _ = List.iter 
          (fun p -> 
            Hashtbl.replace st.port_load (OP.Port.port_of_int p.OP.Port.port_no) 0.0 ) 
          port in 
  let _ = 
    match stats with
    | Some ts -> ignore_result (poll_flow_stats controller st dpid ts)
    | None -> ()
  in
  let _ = st.dpid <- st.dpid @ [dp] in 
  return (pp "+ datapath:0x%012Lx\n" dp)

let port_status_cb st ctrl dpid = function 
  | OE.Port_status(OP.Port.ADD, p, dpid) ->
    return (Hashtbl.replace st.port_load (OP.Port.port_of_int p.OP.Port.port_no) 0.0)
  | OE.Port_status(OP.Port.DEL, p, dpid) -> 
    return (Hashtbl.remove st.port_load (OP.Port.port_of_int p.OP.Port.port_no) )
  | _ ->  return ()


let packet_in_cb ?(handle_arp=true) name switch_data controller dpid evt =
  incr switch_data.req_count;
  let (in_port, buffer_id, data, dp) = 
    match evt with
      | OE.Packet_in (inp, _, buf, dat, dp) -> (inp, buf, dat, dp)
      | _ -> invalid_arg "bogus datapath_join event match!"
  in

  (* Parse Ethernet header *)
  let m = OP.Match.raw_packet_to_match in_port data in

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
      if ((ix = "\xff\xff\xff\xff\xff\xff") ||
         (not (Hashtbl.mem switch_data.mac_cache ix)) ) then (
(*        let _ = pp "XXXXX got a packet I don't know the output port\n%!" in *)
        let bs =
          (OP.Packet_out.create ~buffer_id
          ~actions:[ OP.(Flow.Output(OP.Port.All, 2000))]
          ~data ~in_port () ) in
          let h = OP.Header.create OP.Header.PACKET_OUT 0 in
            OC.send_data controller dpid (OP.Packet_out (h, bs))
      ) else (
(*        let _ = pp "XXXXX got a packet I know the output port\n%!" in *)
        let out_port = (Hashtbl.find switch_data.mac_cache ix) in
        let flags = OP.Flow_mod.({send_flow_rem=true; emerg=false; overlap=false;}) in
        lwt _ =
          if (buffer_id = -1l) then
            (* Need to send also the packet in cache the packet is not cached *)
            let bs =
              OP.Packet_out.create
              ~buffer_id ~actions:[ OP.(Flow.Output(out_port, 2000))]
              ~data ~in_port ()  in
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
  | (_, _) -> return (Hashtbl.replace switch_data.mac_cache m.OP.Match.dl_src in_port)


let router_packet_in_cb name switch_data controller dpid evt =
  incr switch_data.req_count;
  let (in_port, buffer_id, data, dp) = 
    match evt with
      | OE.Packet_in (inp, _, buf, dat, dp) -> (inp, buf, dat, dp)
      | _ -> invalid_arg "bogus datapath_join event match!"
  in

  (* Parse Ethernet header *)
  let m = OP.Match.raw_packet_to_match in_port data in 
  
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
      let data = get_arp_reply "\xfe\xff\xff\xff\xff\xff" ix 
                    m.OP.Match.nw_src m.OP.Match.nw_dst in 
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
         test_packet_out controller dpid actions buffer_id data in_port
        else return () in
    let pkt =
        OP.Flow_mod.create m 0_L OP.Flow_mod.ADD ~hard_timeout:0
           ~idle_timeout:0 ~buffer_id:(Int32.to_int buffer_id)  ~flags
           actions () in
     let h = OP.Header.create OP.Header.FLOW_MOD 0 in
      OC.send_data controller dpid (OP.Flow_mod (h, pkt)) 
     with Not_found -> return ()
  end
  | (_) -> return ()
     
let init ?(handle_arp=true) name switch_data st = 
  if (not (List.mem st switch_data.of_ctrl)) then
    switch_data.of_ctrl <- (([st] @ switch_data.of_ctrl));
  OC.register_cb st OE.DATAPATH_JOIN (datapath_join_cb switch_data);
  OC.register_cb st OE.PACKET_IN (packet_in_cb ~handle_arp name switch_data)

let init_router name st ctrl =
  let _ = 
    if (not (List.mem ctrl st.of_ctrl)) then
      st.of_ctrl <- (([ctrl] @ st.of_ctrl))
  in 
  let _ = OC.register_cb ctrl OE.DATAPATH_JOIN (datapath_join_cb st) in
    OC.register_cb ctrl OE.PACKET_IN  (router_packet_in_cb name st)


(******************************************
 *i Fat tree controller 
 * *)
let fib_match st dpid dst_ip =
  let ip_match ip mask dst_ip = 
    let netmask = Int32.shift_left 0xffffffffl (32 - mask) in 
      (Int32.logand netmask dst_ip) = (Int32.logand netmask ip)
  in
  let (m, p) = List.fold_right (
    fun (ip, mask, port) (m, p) -> 
      if ((m <= mask) && (ip_match ip mask dst_ip)) then 
          (mask, port )
        else
          (m,p)
  ) st.fib (0, []) in 
  if (m = 0) then 
    failwith (sprintf "Invalid IP address %lx" dst_ip ) 
  else
    (* Find the load on each link and define weights *)
    let sum = List.fold_right (fun p sum -> 
      if (Hashtbl.mem st.port_load (OP.Port.Port(p)) ) then
        sum +. (100.0 -. (Hashtbl.find st.port_load (OP.Port.Port(p)) ) )
      else sum +. 100.0  ) p 0.0 in
    let prob = Random.float sum in 
    let (_, p) = List.fold_right ( 
      fun port (sum, p) -> 
        let count = 
          try 
            let load =  Hashtbl.find st.port_load 
                          (OP.Port.Port(port))  in 
            100.0 -. load
          with Not_found -> 100.0
        in 
        if ((prob >= sum) && (prob <= count +. sum )) then
          let _ = printf "[switch 0x%Lx] found port %d with prob %f [%f - %f]\n%!" 
                    dpid port prob sum (sum +. count) in 
          (sum +. count, OP.Port.Port(port) )
        else 
          let _ = printf "[switch 0x%Lx] didn't found port %d with prob %f [%f - %f]\n%!" 
                    dpid port prob sum (sum +. count) in 
          (sum +. count, p)
    ) p (0.0, OP.Port.Port(List.hd p)) in 
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
        let data = get_arp_reply "\xfe\xff\xff\xff\xff\xff" ix m.OP.Match.nw_src ip in 
        let bs =
          (OP.Packet_out.create ~buffer_id:(-1l)
          ~actions:[ OP.(Flow.Output(OP.Port.All, 2000))]
          ~data ~in_port:(OP.Port.No_port) () ) in
        let h = OP.Header.create OP.Header.PACKET_OUT 0 in
        lwt _ = OC.send_data controller dpid (OP.Packet_out (h, bs)) in 
          return ()
      else return ()
  | _ -> begin
    try_lwt
    let (_, out_port) = fib_match switch_data dpid m.OP.Match.nw_dst in

    let flags=OP.Flow_mod.({send_flow_rem=true;emerg=false;overlap=false;}) in
    let actions = 
      if (Hashtbl.mem switch_data.ip_cache m.OP.Match.nw_dst) then
        let ix = Hashtbl.find switch_data.ip_cache m.OP.Match.nw_dst in
        [OP.(Flow.Set_dl_src("\xfe\xff\xff\xff\xff\xff"));
         OP.(Flow.Set_dl_dst(ix));
         OP.(Flow.Output(out_port, 2000))]
      else
        [ OP.(Flow.Output(out_port, 2000)) ]
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
    with Not_found ->
      return (printf "XXXXXXXX Found the not found error\n%!")

  end

let fat_tree_stats_cb st ctrl dpid = function 
  | OE.Flow_stats_reply(xid, more, stats, dpid) ->
    let ports = Hashtbl.create 16 in 
    let _ = List.iter (fun fl ->
        let (p, count, _) = 
          if (Hashtbl.mem st.flows fl.OP.Flow.of_match) then
            Hashtbl.find st.flows fl.OP.Flow.of_match 
          else 
            let p = List.fold_right (
              fun a p -> 
                match a with 
                | OP.Flow.Output(p, _)  -> p
                | _ -> p ) fl.OP.Flow.action OP.Port.No_port in 
            (p, 0L, 0L)
        in 
        let new_diff = Int64.sub fl.OP.Flow.byte_count count in 
        let _ = Hashtbl.replace st.flows fl.OP.Flow.of_match 
                  (p, fl.OP.Flow.byte_count, new_diff ) in
        let count =
          if (Hashtbl.mem ports p) then Hashtbl.find ports p 
          else 0L in 
          Hashtbl.replace ports p (Int64.add count new_diff)
      ) stats in

    let finished = Hashtbl.fold (fun k _ finished -> 
      if (List.exists (fun fl -> fl.OP.Flow.of_match = k) stats ) then
        finished 
      else 
        k :: finished 
    ) st.flows [] in 
    let _ = List.iter (fun k -> Hashtbl.remove st.flows k) finished in    
    let _ = Hashtbl.iter 
              (fun p c ->
                let load = 
                  if (Hashtbl.mem ports p) then 
                    let load = Hashtbl.find ports p in
                    let load = (Int64.to_float (Int64.div load 131072L )) in
                    let _ = Hashtbl.replace st.port_load p load in 
                      load
                  else
                    let _ = Hashtbl.replace st.port_load p 0.0 in 0.0
                in 
        (*        printf "[switch 0x%Lx] Port %s load %f\n%!" 
                  dpid (OP.Port.string_of_port p) load *)
        ()
    ) st.port_load in
    return ()
  | _ -> return (printf "invalid event on fat_tree_stats_cb\n%!")

let init_fat_tree pod swid name st ctrl =
  let _ = 
    if (not (List.mem ctrl st.of_ctrl)) then
      st.of_ctrl <- (([ctrl] @ st.of_ctrl))
  in
  (* create the fib *)
  let _ = 
    if (swid < 2) then 
      st.fib <- 
        [((fat_tree_ip pod swid 2) , 32, [10] );
        ((fat_tree_ip pod swid 3), 32, [11] ); 
        (0x0a000000l, 8, [12;13] )]
    else 
      st.fib <- 
        [((fat_tree_ip pod 0 0), 24, [10] ); 
        ((fat_tree_ip pod 1 0), 24, [11] ); 
        (0x0a000000l, 8, [12;13] )]
  in
  let _ = OC.register_cb ctrl OE.DATAPATH_JOIN (datapath_join_cb st
          ~stats:(Some(1.0)) ~ip:(Some(fat_tree_ip pod swid 1))) in
  let _ = OC.register_cb ctrl OE.PACKET_IN  (fat_tree_packet_in_cb pod swid name st) in 
  let _ = OC.register_cb ctrl OE.FLOW_STATS_REPLY  (fat_tree_stats_cb st) in 
  let _ = OC.register_cb ctrl OE.PORT_STATUS_CHANGE (port_status_cb st) in 
    ()

let init_fat_tree_core pod i j name st ctrl =
  let _ = 
    if (not (List.mem ctrl st.of_ctrl)) then
      st.of_ctrl <- (([ctrl] @ st.of_ctrl))
  in
  (* create the fib *)
  let _ = 
      st.fib <- 
        [((fat_tree_ip 0 0 0) , 16, [10] );
        ((fat_tree_ip 1 0 0), 16, [11] ); 
        ((fat_tree_ip 2 0 0), 16, [12] ); 
        ((fat_tree_ip 3 0 0), 16, [13] ); 
        (* (0x0a000000l, 8, 13) *) ]
  in
  let _ = OC.register_cb ctrl OE.DATAPATH_JOIN (datapath_join_cb st
            ~stats:(Some(1.0)) ~ip:(Some(fat_tree_ip 4 i j))) in
  let _ = OC.register_cb ctrl OE.PACKET_IN  (fat_tree_packet_in_cb 4 i name st) in 
  let _ = OC.register_cb ctrl OE.FLOW_STATS_REPLY  (fat_tree_stats_cb st) in
    ()

#let BUILD_XEN = true

#if BUILD_XEN
open Lwt
open Lwt_unix
open Printf
open Topo


open Xen_api
open Xen_api_lwt_unix

(* informations required to store for each host *)
type host_type = {
  name_label: string;
  vm_uuid: string;
  mutable vif_uuid : (string * string * int * int) list;
  mutable vif_count: int;
  mutable domid : int;
}
let hosts = ref []

(* (network_ref, net_uuid, (vm_uuid, vif_uuid, vif_ref) list) *)
type link_type = {
  hosts : string * string;
  net_ref: string; 
}
let links = ref []
let net_count = ref 0

let rpc = make  "file:///var/lib/xcp/xapi"

let init_session () =
    Session.login_with_password rpc Xen_pass.user Xen_pass.pass "1.0" 
      
let vm_install session_id name_label =
  try_lwt 
    let memory = 1024000000L in
    let recommendations = "" (* "<restrictions><restriction
    field=\"memory-static-max\" max=\"137438953472\" /><restriction
    field=\"vcpus-max\" max=\"16\"/><restriction property=\"number-of-vbds\"
    max=\"7\" /><restriction property=\"number-of-vifs\" max=\"7\"
    /></restrictions>" *) in 
    lwt vm_uuid = VM.create ~rpc ~session_id ~name_label
    ~name_description:("sdnsim host")
    ~user_version:(1L) ~is_a_template:(false) ~affinity:("") ~memory_target:(0L)
        ~memory_static_max:memory ~memory_dynamic_max:memory
        ~memory_dynamic_min:memory ~memory_static_min:memory  
        ~vCPUs_params:([]) ~vCPUs_max:(1L) ~vCPUs_at_startup:(1L)
        ~actions_after_shutdown:(`destroy) ~actions_after_reboot:(`restart) 
        ~actions_after_crash:(`restart) ~pV_bootloader:("")
        ~pV_kernel:(sprintf "/boot/guest/%s.xen" name_label) 
        ~pV_ramdisk:"" ~pV_args:"" ~pV_bootloader_args:"" ~pV_legacy_args:"" 
        ~hVM_boot_policy:"" ~hVM_boot_params:([])
        ~hVM_shadow_multiplier:(1.0) 
        ~platform:([("nx", "false");("acpi","true");("apic","true");
        ("pae","true"); ("viridian", "true")]) ~pCI_bus:"" 
        ~other_config:([]) ~recommendations ~xenstore_data:([]) ~ha_always_run:false 
        ~ha_restart_priority:"" ~tags:([]) ~blocked_operations:([])
        ~protection_policy:"" ~is_snapshot_from_vmpp:false 
        ~appliance:"" ~start_delay:0L ~shutdown_delay:0L ~order:0L
        ~suspend_SR:"" ~version:(0L) in 
let h = {name_label; vm_uuid; vif_uuid=[]; vif_count=0;domid=0;} in 
    let _ = hosts := !hosts @ [h] in
    return vm_uuid
  with exn -> 
    failwith (sprintf "ERROR: vm_install:%s\n%!" (Printexc.to_string exn))

let build_vm module_name host = 
  lwt _ = Lwt_unix.system "ocamlbuild -clean" in
  let path = "/home/cr409/.opam/4.00.1+mirage-xen/" in 

  (* TODO make he bin patgh dynamic *)
  let _ = Util.command "PATH=%s/bin/:$PATH %s/bin/ocamlbuild topo_%s.nobj.o"
      path path module_name in
  let obj = sprintf "_build/topo_%s.nobj.o" module_name in 
  if Sys.file_exists obj then begin
    let path = Util.read_command "%s/bin/ocamlfind printconf path" path in
    let lib = Util.strip path ^ "/mirage-xen" in
    Util.command "ld -d -nostdlib -m elf_x86_64 -T %s/mirage-x86_64.lds %s/x86_64.o %s %s/libocaml.a %s/libxen.a \
             %s/libxencaml.a %s/libdiet.a %s/libm.a %s/longjmp.o -o _build/topo_%s.xen"  
            lib lib obj lib lib lib lib lib lib module_name;
    lwt _ = Lwt_unix.system 
        (sprintf "mv _build/topo_%s.xen /boot/guest/%s.xen"
           module_name host) in  
    return ()
  end else
    Util.error "xen object file %s not found, cannot continue" obj

let lwt_open_out file = openfile file [O_WRONLY; O_CREAT; O_TRUNC; ] 0o666 
let lwt_output_string fd str = 
  lwt _ = write fd str 0 (String.length str) in 
    return ()
let lwt_close_out fd = close fd

let generate_vms ses module_name nodes =
  lwt _ = 
    Lwt_list.iter_s 
      ( fun (host, main, params) ->  
          lwt out = lwt_open_out (sprintf "topo_%s.ml" module_name) in
          lwt _ = lwt_output_string out "let () = OS.Main.run ( " in  
          let str_param = 
            List.fold_right (
              fun (n, v) r -> 
                match n with
                  | None -> sprintf "%s %s" r v
                  | Some(n) -> sprintf "%s ~%s:%s" r n v
            ) params "" in 
          lwt _ = lwt_output_string out (sprintf "%s.%s %s ())\n" 
                  module_name main str_param) in
          lwt _ = lwt_close_out out in 
          lwt _ = build_vm module_name host in 
          lwt _ = vm_install ses host in
            return ()
     ) (Hashtbl.fold (fun h (m, p) r -> r @ [(h,m,p)] ) nodes []) in
  return ()

let vif_install ses h dst_h net_ref rate = 
  let device = h.vif_count in 
  let _ = h.vif_count <- h.vif_count + 1 in
  lwt vif_uuid = VIF.create ~rpc ~session_id:ses 
      ~vM:h.vm_uuid ~network:net_ref ~mTU:1500L 
      ~mAC:"" ~device:(string_of_int device) 
      ~other_config:["promiscuous", "on"; "mtu", "1500"] 
      ~qos_algorithm_type:"ratelimit" ~qos_algorithm_params:[("kbps","1024")]
      ~locking_mode:(`network_default) ~ipv4_allowed:[]
        ~ipv6_allowed:[] in
  let _ = h.vif_uuid <- (vif_uuid, dst_h, device, rate) :: h.vif_uuid in 
    return ()
 

let generate_links ses link_list = 
  lwt _ = 
    Lwt_list.iter_s 
      (
        fun (node_src, node_dst, delay, rate, _) ->
          try 
            let dst_h = List.find (fun a -> (a.name_label = node_dst)) !hosts in
            let src_h = List.find (fun a -> (a.name_label = node_src)) !hosts in 
            (* generate target network *)
            let net_id = !net_count in 
            let _ = net_count := !net_count + 1 in 
            lwt net_ref = Network.create ~rpc ~session_id:ses 
                            ~name_label:(sprintf "mir%d" net_id)
                            ~name_description:(sprintf "link between %s & %s" node_src node_dst)
                            ~mTU:1500L ~other_config:[] ~tags:[] in
            let _ = links := net_ref :: !links in 
            (* allocate vifs *)
            lwt _ = vif_install ses dst_h node_src net_ref rate in 
            lwt _ = vif_install ses src_h node_dst net_ref rate in 
             return ()
          with Not_found -> 
            let _ = Printf.eprintf "ERROR:generate_links: cannot create link
            between %s - %s\n%!" node_src node_dst in 
            return ()
      )  link_list in 
  return ()

let clean_up_vm session_id = 
  lwt _ = Lwt_list.iter_p (
      fun h ->
        lwt _ = 
          try_lwt 
            VM.hard_shutdown ~rpc ~session_id ~vm:h.vm_uuid 
            with _ -> return ()
        in 
        lwt _ = Lwt_unix.system  (sprintf "rm /boot/guest/%s.xen" h.name_label) in 
        VM.destroy ~rpc ~session_id ~self:(h.vm_uuid)
    ) !hosts in 
  Lwt_list.iter_p (
    fun self -> Network.destroy ~rpc ~session_id ~self) !links 

let generate_scenario sc =
  lwt ses = init_session () in
  let module_name = get_scenario_name sc in 
  let nodes = get_scenario_nodes sc in
  let links = get_scenario_links sc in 
  lwt _ = generate_vms ses module_name nodes in
  lwt _ = generate_links ses links in
    return ()

let clean_scenario sc = 
  lwt session_id = init_session () in
  let module_name = get_scenario_name sc in 
  lwt _ = clean_up_vm session_id in 
  lwt _ = Lwt_unix.system 
      (sprintf "rm _build/topo_%s.ml"
         module_name) in 
    return ()

let get_topology sc =
  let ix = ref 0L in 
  let names = Hashtbl.create 64 in 
  let nodes = ref [] in
  let links = ref [] in  
  let _ = Topo.iter_scenario_nodes  sc 
    (fun name _ _ ->
      Hashtbl.add names name !ix;
      let _ = ix := Int64.add !ix 1L in 
      nodes := !nodes @ [(Rpc.Dict [
        ("name", (Rpc.String name));
        ("flows", (Rpc.Enum []));
        ("dev", (Rpc.Enum []));
        ] )]
    ) in

  let _ = Topo.iter_scenario_links sc (
    fun nodes_a nodes_b _ _ _ -> 
      let ix_a = Hashtbl.find names nodes_a in 
      let ix_b = Hashtbl.find names nodes_b in
      links := !links @ [(Rpc.Dict 
        [("source",(Rpc.Int ix_a));
      ("target",(Rpc.Int ix_b));
      ("ts", (Rpc.Float 0.0 ));
      ("value",(Rpc.Int 1L))])] 
      ) in 
  (names, Rpc.Dict [("nodes",(Rpc.Enum !nodes));
      ("links", (Rpc.Enum !links));])

let send_msg fd time tpy msg = 
  let msg = Jsonrpc.to_string (
    Rpc.Dict [
      ("ts", (Rpc.Float time));
      ("type", (Rpc.String tpy));
      ("data", (Rpc.String msg));]) in
  let len = Cstruct.create 4 in 
  let _ = Cstruct.BE.set_uint32 len 0 (Int32.of_int (String.length msg)) in 
  lwt _ = Lwt_unix.write fd (Cstruct.to_string len) 0 4 in 
  lwt _ = Lwt_unix.write fd msg 0 (String.length msg) in
  return ()

type link_utilisation = {
  source : string;
  target : string;
  mutable byte_count : int64;
  mutable rate: float;
}

let run_logger host port sc vifs =
  let fd = socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM  0 in
  lwt _ = Lwt_unix.connect fd (Lwt_unix.ADDR_INET((Unix.inet_addr_of_string host), port)) in 
  (* let ch = Lwt_io.of_fd mode:Lwt_io.output_channel fd in *)
  let clock = ref 0.0 in 
  let names, topo = get_topology sc in 
  let msg =  Jsonrpc.to_string topo in 
  lwt _ = send_msg fd !clock "topology" msg in
  while_lwt true do
    lwt _ = Lwt_unix.sleep 1.0 in
    clock := !clock +. 1.0;
    lwt links = 
      Lwt_list.fold_right_s (
        fun (vif, data) r -> 
          (* (source, target, byte_count, rate) -> *)
          (* sudo tc -s qdisc show dev vif220.0 | grep bytes  | cut -d \  -f 5 *)
          lwt res = Lwt_process.pread ("/usr/bin/get_bytes",
          [|"/usr/bin/get_bytes"; vif;|]) in
          let res = String.sub res 0 ((String.length res)-1)  in 
          let byte_count = Int64.of_string res in 
          printf "device %s bytes %Ld\n%!" vif (Int64.sub byte_count data.byte_count); 
          let load = Int64.sub byte_count data.byte_count in 
          let utilization = (Int64.to_float (Int64.shift_right load 17)) in
          Hashtbl.replace vifs vif 
          ({source=data.source;target=data.target;byte_count;rate=data.rate;});
              if (utilization >= 0.0) then
                let s_ix = Hashtbl.find names data.source in 
                let d_ix = Hashtbl.find names data.target in 
                return (r @ [(Rpc.Dict [
                     ("source", Rpc.String data.source);
                     ("target", Rpc.String data.target);
                     ("ts", (Rpc.Float !clock ));
                     ("value", (Rpc.Float (utilization /. data.rate)))])])
              else 
                return (r)
      ) (Hashtbl.fold (fun a b r-> (a,b)::r) vifs []) [] in  
      printf "utilisation links %d\n%!" (List.length links);
      let msg =  Jsonrpc.to_string (Rpc.Enum links) in 
      lwt _ = send_msg fd !clock "link_utilization" msg in
     return ()
  done 

let run_emulation session_id duration sc =
  let vifs = Hashtbl.create 16 in 
  lwt _ = Lwt_list.iter_s (
    fun h -> 
    lwt _ = VM.start ~rpc ~session_id ~vm:h.vm_uuid
               ~start_paused:false ~force:false in
    lwt domid = VM.get_domid ~rpc ~session_id ~self:h.vm_uuid in
    let _ = h.domid <- Int64.to_int domid in
    let _ = 
      List.iter ( fun (uuid, dst, id, rate) ->
        Hashtbl.add vifs (sprintf "vif%Ld.%d" domid id) 
        ({source=h.name_label; target=dst; byte_count=0L; rate=(float_of_int rate);})
      ) h.vif_uuid in 
    return ()
    ) !hosts in
  lwt _ = 
    match (get_scenario_log_server sc) with
    | None ->  (Lwt_unix.sleep (float_of_int duration))
    | Some (host, port) -> (run_logger host port sc vifs) <?> (Lwt_unix.sleep (float_of_int duration)) 
  in
    return ()

let run_scenario sc =
  lwt ses = init_session () in
  let duration = get_scenario_duration sc in
  lwt _ = run_emulation ses duration sc in
  return ()

#else

let generate_scenario sc = ()
let run_scenario sc = ()

#endif

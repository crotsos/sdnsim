#let BUILD_XEN = true

#if BUILD_XEN
open Lwt
open Lwt_unix
open Printf
open Stringext
open Topo


open Xen_api
open Xen_api_lwt_unix

(* informations required to store for each host *)
type host_type = {
  name_label: string;
  vm_uuid: string;
  mutable vif_uuid : string list;
  mutable vif_count: int;
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
    let memory = 32000000L in
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
    let h = {name_label; vm_uuid; vif_uuid=[]; vif_count=0;} in 
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

let vif_install ses h net_ref = 
  let device = h.vif_count in 
  let _ = h.vif_count <- h.vif_count + 1 in
  lwt vif_uuid = VIF.create ~rpc ~session_id:ses 
      ~vM:h.vm_uuid ~network:net_ref ~mTU:1500L 
      ~mAC:"" ~device:(string_of_int device) 
      ~other_config:["promiscuous", "on"; "mtu", "1500"] 
      ~qos_algorithm_type:"" ~qos_algorithm_params:[]
      ~locking_mode:(`network_default) ~ipv4_allowed:[]
        ~ipv6_allowed:[] in
  let _ = h.vif_uuid <- vif_uuid :: h.vif_uuid in 
    return ()
 

let generate_links ses link_list = 
  lwt _ = 
    Lwt_list.iter_p 
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
            lwt _ = vif_install ses dst_h net_ref in 
            lwt _ = vif_install ses src_h net_ref in 
             return ()
          with Not_found -> 
            let _ = Printf.eprintf "ERROR:generate_links: cannot create link
            between %s - %s\n%!" node_src node_dst in 
            return ()
      ) link_list in 
  return ()

let run_emulation session_id duration = 
  lwt _ = Lwt_list.iter_p (
    fun h -> VM.start ~rpc ~session_id ~vm:h.vm_uuid
    ~start_paused:false ~force:false ) !hosts in 
    Lwt_unix.sleep (float_of_int duration)

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

let run_scenario sc =
  lwt ses = init_session () in
  let duration = get_scenario_duration sc in 
  lwt _ = run_emulation ses duration in
    return ()

#else

let generate_scenario sc = ()
let run_scenario sc = ()

#endif

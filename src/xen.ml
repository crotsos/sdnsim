open Lwt
open Lwt_unix
open Printf
open Client
open Xmlrpc_client
open Printf
open Stringext

let http = xmlrpc ~version:"1.1" "/"
let host = ref "localhost" 
let vm_template = "Other install media"

(* informations required to store for each host *)
type host_type = {
  name: string;
  vm_uuid: string;
  vm_ref: API.ref_VM; 
  mutable vif_count: int;
}
let hosts = ref []

(* (network_ref, net_uuid, (vm_uuid, vif_uuid, vif_ref) list) *)
type link_type = {
  net_ref: API.ref_network; 
}
let links = ref []
let net_count = ref 0

let rpc_remote xml = 
  XML_protocol.rpc ~transport:(SSL(SSL.make(), !host, 443)) 
    ~http xml

let rpc_unix_domain xml = 
  XML_protocol.rpc ~transport:(Unix "/var/lib/xcp/xapi") ~http xml

let rpc = ref rpc_unix_domain

let init_session uname pwd =
    Client.Session.login_with_password ~rpc:!rpc 
      ~uname ~pwd ~version:Xapi_globs.api_version_string
      
let find_template session_id vm = 
  let vms = Client.VM.get_all !rpc session_id in
  match List.filter 
          (fun self ->
             ((Client.VM.get_name_label !rpc session_id self) = vm)
             && (Client.VM.get_is_a_template !rpc session_id self)
          ) vms with
    | [] -> failwith "Unable_to_find_suitable_vm_template"
    | x :: _ ->
        Printf.printf "Choosing template with name: %s\n" (Client.VM.get_name_label !rpc session_id x); 
        x 


let cli_cmd args = 
  Printf.printf "%s\n" (String.concat " " ("$ "::Xapi_globs.xe_path :: args));
  try
    let output = String.rtrim (fst(Forkhelpers.execute_command_get_output 
                                     "/usr/bin/xe" args)) in
    Printf.printf "'%s'\n%!" output;
    output
  with 
  | Forkhelpers.Spawn_internal_error(log, output, Unix.WEXITED n) ->
      Printf.printf "a: %s\n%!!" log;
      failwith "CLI failed"
  | Forkhelpers.Spawn_internal_error(log, output, _) ->
      Printf.printf "b: %s\n%!!" log;
      failwith "CLI failed"
  | e -> 
      Printf.printf "c: %s\n%!!" (Printexc.to_string e);
      failwith "CLI failed"

let vm_install session_id template name = 
  let uuid_vm = cli_cmd [ "vm-install"; "template-uuid=" ^ template; 
                             "new-name-label=" ^ name ] in
  let vm_ref = Client.VM.get_by_uuid !rpc session_id uuid_vm in
  let _ = Client.VM.set_PV_kernel!rpc session_id vm_ref
            (sprintf "/boot/guests/%s" name) in
  let _ = Client.VM.set_memory_static_min !rpc session_id vm_ref 32000000L in 
  let _ = Client.VM.set_memory_dynamic_min !rpc session_id vm_ref 32000000L in 
  let _ = Client.VM.set_memory_dynamic_max !rpc session_id vm_ref 32000000L in 
  let _ = Client.VM.set_memory_static_max !rpc session_id vm_ref 32000000L in
  let h = {name; vm_uuid=uuid_vm; vm_ref=vm_ref;
           vif_count=0;} in 
  let _ = hosts := !hosts @ [h] in 
    (uuid_vm, vm_ref)

let build_vm module_name host = 
  lwt _ = Lwt_unix.system "ocamlbuild -clean" in 
  lwt _ = Lwt_unix.system 
            (sprintf "mir-build xen/topo_%s.xen" module_name) in 
  lwt _ = Lwt_unix.system 
            (sprintf "mv ./_build/xen/topo_%s.xen /boot/guest/%s.xen"
              module_name host) in  
    return ()

let lwt_open_out file = 
  openfile file [O_WRONLY; O_CREAT; O_TRUNC; ] 0o666 

let lwt_output_string fd str = 
  lwt _ = write fd str 0 (String.length str) in 
    return ()

let lwt_close_out fd = 
  close fd

let generate_vms ses uuid_tmpl module_name nodes =
  lwt _ = 
    Lwt_list.iter_s 
      ( fun (host, main, params) ->  
          lwt out = lwt_open_out (sprintf "topo_%s.ml" module_name) in
          lwt _ = lwt_output_string out "let run () = " in  
          let str_param = 
            List.fold_right (
              fun (n, v) r -> 
                match n with
                  | None -> sprintf "%s %s" r v
                  | Some(n) -> sprintf "%s ~%s:%s" r n v
            ) params "" in 
          lwt _ = lwt_output_string out (sprintf "%s.%s %s ()\n" 
                  module_name main str_param) in
          lwt _ = lwt_close_out out in 
          lwt out = lwt_open_out (sprintf "topo_%s.mir" module_name) in
          lwt _ = lwt_output_string out (sprintf "Topo_%s.run \n"
                                           module_name) in  
          lwt _ = lwt_close_out out in
          lwt _ = build_vm module_name host in 
          let _ = vm_install ses uuid_tmpl host in
            return ()
     ) (Hashtbl.fold (fun h (m, p) r -> r @ [(h,m,p)] ) nodes []) in
  return ()

let vif_install ses h net_ref = 
  let device = h.vif_count in 
  let _ = h.vif_count <- h.vif_count + 1 in
    Client.VIF.create ~rpc:!rpc ~session_id:ses 
      ~vM:h.vm_ref ~network:net_ref ~mTU:1500L 
      ~mAC:"" ~device:(string_of_int device) 
      ~other_config:["promiscuous", "on"; "mtu", "1500"] 
      ~qos_algorithm_type:"" ~qos_algorithm_params:[]
 

let generate_links ses link_list = 
  lwt _ = 
    Lwt_list.iter_p 
      (
        fun (node_src, node_dst, delay, rate, _) ->
          try 
            let dst_h = List.find (fun a -> (a.name = node_dst)) !hosts in
            let src_h = List.find (fun a -> (a.name = node_src)) !hosts in 
            (* generate target network *)
            let net_id = !net_count in 
            let _ = net_count := !net_count + 1 in 
            let net_ref = Client.Network.create !rpc ses 
                            (sprintf "mir%d" net_id)
                            (sprintf "link between %s & %s" node_src node_dst)
                            1500L [] [] in
            let l = {net_ref;} in
            let _ = links := !links @ [l] in 
            (* allocate vifs *)
            let _ = vif_install ses dst_h net_ref in 
            let _ = vif_install ses src_h net_ref in 
             return ()
          with Not_found -> return ()
      ) link_list in 
  return ()

let run_emulation ses duration = 
  let _ = List.iter (
    fun h -> Client.VM.start !rpc ses h.vm_ref false false 
  ) !hosts in 
    Lwt_unix.sleep (float_of_int duration)

let clean_up_vm ses = 
  let _ = List.iter (
    fun h -> 
      let _ = Client.VM.hard_shutdown !rpc ses h.vm_ref in 
      let _ = Client.VM.destroy !rpc ses h.vm_ref in
        ()
  ) !hosts in 
  let _ = List.iter (
    fun l -> 
      let _ = Client.Network.destroy !rpc ses l.net_ref in 
        ()
  ) !links in 
    return ()
  
let run_code module_name duration nodes links =
  let ses = init_session "root" "c074e5d6" in
    try 
      let _ = printf "xapi: connected client.. \n" in
      let uuid_tmpl = Client.VM.get_uuid !rpc ses
                        (find_template ses vm_template) in
      lwt _ = generate_vms ses uuid_tmpl module_name nodes in
      let _ = generate_links ses links in
      lwt _ = run_emulation ses duration in 
      lwt _ = clean_up_vm ses in 
        return ()
    with _ -> 
      lwt _ = clean_up_vm ses in 
        return ()

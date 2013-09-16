
(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2013 Anil Madhavapeddy <anil@recoil.org>
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

(* code copied from the mirari project *)

let info fmt =
  Printf.kprintf (Printf.printf "[sdnsim] %s\n%!") fmt

let strip str =
  let p = ref 0 in
  let l = String.length str in
  let fn = function
    | ' ' | '\t' | '\r' | '\n' -> true
    | _ -> false in
  while !p < l && fn (String.unsafe_get str !p) do
    incr p;
  done;
  let p = !p in
  let l = ref (l - 1) in
  while !l >= p && fn (String.unsafe_get str !l) do
    decr l;
  done;
  String.sub str p (!l - p + 1)

let error fmt =
  Printf.kprintf (fun str ->
    Printf.eprintf "[sdnsim] ERROR: %s\n%!" str;
    exit 1;
  ) fmt

let command ?switch fmt =
  Printf.kprintf (fun str ->
    let cmd = match switch with
      | None -> str
      | Some cmp -> Printf.sprintf "opam config exec \"%s\" --switch=%s" str cmp in
    info "+ Executing: %s" cmd;
    match Sys.command cmd with
    | 0 -> ()
    | i -> error "The command %S exited with code %d." cmd i
  ) fmt

let read_command fmt =
  let open Unix in
  Printf.ksprintf (fun cmd ->
      let () = info "+ Executing: %s" cmd in
      let ic, oc, ec = open_process_full cmd (environment ()) in
      let buf1 = Buffer.create 64 and buf2 = Buffer.create 64 in
      (try while true do Buffer.add_channel buf1 ic 1 done with End_of_file -> ());
      (try while true do Buffer.add_channel buf2 ec 1 done with End_of_file -> ());
      match close_process_full (ic,oc,ec) with
      | WEXITED 0   -> Buffer.contents buf1
      | WSIGNALED n -> error "process killed by signal %d" n
      | WSTOPPED n  -> error "process stopped by signal %d" n
      | WEXITED r   -> error "command terminated with exit code %d\nstderr: %s" r (Buffer.contents buf2)
    ) fmt



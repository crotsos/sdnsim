open Printf
open Xml

#let XML_LIGHT_2 = true

#if XML_LIGHT_2

exception No_attribute of string

(* Augment the xml parsing functions *)
let attrib el name = 
  match el with 
  | PCData _ -> raise (No_attribute name)
  | Element (_, attr, _) -> 
      try
        let _, value = List.find (fun (n, _) -> n = name ) attr in 
        value
      with Not_found -> raise (No_attribute name)

let get_attrib_fail el name = 
  try 
    attrib el name 
  with No_attribute _ -> 
    failwith (sprintf "required attribute %s missing" name)

let get_attrib_default el name default_value = 
  try 
    attrib el name 
  with No_attribute _ ->  default_value

let parse_file file = Xml.parse_file file 
let iter fn el =  
  match el with
  | PCData _ -> ()
  | Element (_, _, ch) -> 
      List.iter fn ch 
let tag      = Xml.tag 
let pcdata   = Xml.pcdata
let children = Xml.children  
let fold fn v el = 
  match el with
  | PCData _ -> v
  | Element (_, _, ch) -> 
      List.fold_left fn v ch 

#else

(* Augment the xml parsing functions *)
let get_attrib_fail el name = 
  try 
    Xml.attrib el name
  with Xml.No_attribute _ -> 
    failwith (sprintf "required attribute %s missing" name)

let get_attrib_default el name default_value = 
  try 
    Xml.attrib el name 
  with Xml.No_attribute _ -> default_value

let parse_file file = Xml.parse_file file 
let iter = Xml.iter
let tag = Xml.tag
let pcdata = Xml.pcdata
let children = Xml.children
let fold = Xml.fold

#endif


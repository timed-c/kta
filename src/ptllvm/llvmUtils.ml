

open LlvmAst
open Ustring.Op

exception Function_not_found of string * sid


let const_int_val width v = VConst(CInt(width, Int64.of_int v))

let get_fun m f = 
  let LLModule(_,lst) = m in
  try List.assoc f lst with Not_found -> 
     raise (Function_not_found ((Ustring.to_utf8 (ustring_of_sid f)), f)) 


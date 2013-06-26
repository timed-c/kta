

open LlvmAst
open Ustring.Op

exception Function_not_found of string * sid
exception Illegal_llvm_code

let const_int_val width v = VConst(CInt(width, Int64.of_int v))

let get_fun m f = 
  let LLModule(_,lst) = m in
  try List.assoc f lst with Not_found -> 
     raise (Function_not_found ((Ustring.to_utf8 (ustring_of_sid f)), f)) 

let type_of_val v = 
  match v with 
  | VId(id,ty) -> ty
  | VConst(CInt(bits,_)) -> TyInt(bits)
  | VConstExpr(ty) -> ty

let const32 v = CInt(32,Int64.of_int v)
    

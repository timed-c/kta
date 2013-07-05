

open LlvmAst
open Ustring.Op

exception Function_not_found of string * sid
exception Illegal_llvm_code

let const_int_val width v = ExpConst(CInt(width, Int64.of_int v))


let get_fun m f = 
  let LLModule(_,lst) = m in
  try List.assoc f lst with Not_found -> 
     raise (Function_not_found ((Ustring.to_utf8 (ustring_of_sid f)), f)) 


let rec type_of_val v = 
  match v with 
  | ExpId(id,ty) -> ty
  | ExpConst(CInt(bits,_)) -> TyInt(bits)
  | ExpConst(CPtr(d)) -> type_of_dataval (!d)
  | ExpConstExpr(ty) -> ty
  

and type_of_dataval v =
  match v with 
  | DArray(a) -> 
    type_of_dataval (!(try a.(0) with _ -> raise Illegal_llvm_code))
  | DConst(c) -> type_of_val (ExpConst(c))


let const32 v = CInt(32,Int64.of_int v)


let sign_ext_int64 v n =
  if (Int64.logand (Int64.shift_right_logical v (n-1)) Int64.one) = Int64.zero then v 
  else Int64.logor (Int64.shift_left (Int64.minus_one) n) v


let mask_int64 v n =
        Int64.logand  v (Int64.sub (Int64.shift_left (Int64.one) n) Int64.one)

let default_constval ty =
  match ty with
  | TyInt(w) -> CInt(w,Int64.zero)
  | _ -> failwith "Cannot create default value of this type."


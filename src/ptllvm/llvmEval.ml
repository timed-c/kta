

open Printf
open LlvmAst
open LlvmUtils
open Ustring.Op
open LlvmPPrint


(*************** Exported types and exceptions ********************)

exception Eval_error_identfier_not_found of string 

type time = int
type btime = llabel -> llabel -> time

(*************** Local types and exceptions ***********************)



(*************** Local functions **********************************)

let env_new = PMap.empty
let env_add id v env = PMap.add id v env
let env_find id env = PMap.find id env
let utf8 sid = Ustring.to_utf8 (ustring_of_sid sid)

let eval_expr env e = 
  match e with 
  | VId(id,ty) -> (
    try env_find id env 
    with Not_found -> 
      let ids = pprint_llid id in
      raise (Eval_error_identfier_not_found (Ustring.to_utf8 ids)))
  | VConst(c) -> c
  | VConstExpr(_) -> CInt(1,Int64.zero)   (* TODO *)

let eval_bop bop v1 v2 =
  match v1,v2 with
  | CInt(w,v1),CInt(_,v2) -> (
    let andval = Int64.of_int ((1 lsl w)-1) in
    Int64.logand andval (
      match bop with     
      | BopAdd -> Int64.add v1 v2
      | BopSub -> Int64.sub v1 v2
      | BopMul -> Int64.mul v1 v2
      | BopUDiv | BopSDiv | BopURem | BopSRem -> failwith "todo"
      | BopShl -> Int64.shift_left v1 (Int64.to_int v2)
      | BopLShr -> Int64.shift_right_logical v1 (Int64.to_int v2)
      | BopAShr -> Int64.shift_right v1 (Int64.to_int v2)
      | BopAnd -> Int64.logand v1 v2
      | BopOr -> Int64.logor v1 v2
      | BopXor -> Int64.logxor v1 v2
      | BopFAdd | BopFSub | BopFMul | BopFDiv | BopFRem 
          -> raise Illegal_llvm_code))
  | _ -> raise Illegal_llvm_code

  
(*
let rec eval_inst instlst time env timeout =
  match instlst with
  | IBinOp(id,bop,ty,v1,v2)::lst -> 
    LocalId(eval_bop bop (eval_expr env v1) (eval_expr env v2))

 of      (* Various binary operations. See llBinOp *)
    llLocId *         (* Assignment id *) 
    llBinOp *         (* Binop *)
    llType *          (* Operand type *)        
    llVal *           (* Op1 *)
    llVal             (* Op2 *)
    
*)


(*
let eval_block instlst src_label block_label env timeout =  
let eval_cfg blocks src_label block_label env timeout =
*)

let eval_function blocks params args env timeout =
  (* Get first block in function *)
  let start = match blocks with b::_ -> b | _ -> raise Illegal_llvm_code in  
  (* Augment environment with arguments *)
  let parargs = try List.combine params args with _ -> raise Illegal_llvm_code in
  let env = List.fold_left (fun e ((ty,id), v) -> env_add id v e) env parargs in
  
  (1, VConst(CInt(1,Int64.zero)))


(**************** Exported functions *******************************)

let eval_fun m bt f args timeout =
  (* Find function to execute *)
  let LLFunc(_,params,blocks) = get_fun m f in
  (* Evaluate function *)
  eval_function blocks params args env_new timeout
  

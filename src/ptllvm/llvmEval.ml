

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

type instTy = 
| InstRet    of llConst option 
| InstBranch of llabel * (llId,llConst) PMap.t


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

let eval_bop bop val1 val2 =
  match val1,val2 with
  | CInt(w,v1),CInt(_,v2) -> (
    let andval = Int64.of_int ((1 lsl w)-1) in
    CInt(w,Int64.logand andval (
      match bop with     
      | BopAdd -> Int64.add v1 v2
      | BopSub -> Int64.sub v1 v2
      | BopMul -> Int64.mul v1 v2
      | BopUDiv | BopSDiv | BopURem | BopSRem -> failwith "TODO bop"
      | BopShl -> Int64.shift_left v1 (Int64.to_int v2)
      | BopLShr -> Int64.shift_right_logical v1 (Int64.to_int v2)
      | BopAShr -> Int64.shift_right v1 (Int64.to_int v2)
      | BopAnd -> Int64.logand v1 v2
      | BopOr -> Int64.logor v1 v2
      | BopXor -> Int64.logxor v1 v2
      | BopFAdd | BopFSub | BopFMul | BopFDiv | BopFRem 
          -> raise Illegal_llvm_code)))
  | _ -> raise Illegal_llvm_code

  
let eval_icmp_pred icmppred val1 val2 =
  match val1,val2 with
  | CInt(w,v1), CInt(_,v2) -> (
    CInt(1, (
      match icmppred with
      | IcmpEq  -> if Int64.compare v1 v2 == 0 then Int64.one else Int64.zero
      | IcmpNe  -> if Int64.compare v1 v2 != 0 then Int64.one else Int64.zero
      | IcmpUgt -> if Int64.compare v1 v2 >  0 then Int64.one else Int64.zero
      | IcmpUge -> if Int64.compare v1 v2 >= 0 then Int64.one else Int64.zero
      | IcmpUlt -> if Int64.compare v1 v2 <  0 then Int64.one else Int64.zero
      | IcmpUle -> if Int64.compare v1 v2 <= 0 then Int64.one else Int64.zero
      | IcmpSgt | IcmpSge | IcmpSlt | IcmpSle ->
          failwith "Signed comparison not yet implemented")))
  | _ -> raise Illegal_llvm_code


(* Evaluate a list of instructions *)
let rec eval_inst instlst env  =
  match instlst with
  | IRet(None)::[] -> InstRet(None)
  | IRet(Some(_,e))::[] -> InstRet(Some(eval_expr env e)) 
  | IBrCond(exp,l1,l2)::[] -> (
    match eval_expr env exp with
    | CInt(1,v) when Int64.to_int v = 1 -> InstBranch(l1,env)
    | CInt(1,v) when Int64.to_int v = 0 -> InstBranch(l2,env)
    | _ -> raise Illegal_llvm_code)
  | IBrUncond(l)::[] -> InstBranch(l,env)
  | IBinOp(id,bop,ty,v1,v2)::lst -> 
    let nval = eval_bop bop (eval_expr env v1) (eval_expr env v2) in
    let env' = env_add (LocalId(id)) nval env in
    eval_inst lst env'  
  | ICmp(id,pred,ty,v1,v2)::lst ->
    let nval = eval_icmp_pred pred (eval_expr env v1) (eval_expr env v2) in
    let env' = env_add (LocalId(id)) nval env in
    eval_inst lst env'  
  | _ -> failwith "Instruction not implemented."

(*
(* Evaluate phi functions *)
let eval_phi phis src_label env =
  let foldfun env (LLPhi(id,ty,
  List.fold_left (fun 
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
  

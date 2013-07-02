

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


(* Evaluate an expression to a constant. Looks up identifiers in the environment *)
let eval_expr env e = 
  match e with 
  | VId(id,ty) -> (
    try env_find id env 
    with Not_found -> 
      let ids = pprint_llid id in
      raise (Eval_error_identfier_not_found (Ustring.to_utf8 ids)))
  | VConst(c) -> c
  | VConstExpr(_) -> CInt(1,Int64.zero)   (* TODO *)


(* Evaluating binary operators *)
let eval_bop bop val1 val2 =
  match val1,val2 with
  | CInt(w,v1),CInt(_,v2) -> (
    let andval = Int64.of_int ((1 lsl w)-1) in
    CInt(w,Int64.logand andval (
      match bop with     
      | BopAdd -> mask_int64 (Int64.add v1 v2) w
      | BopSub ->  mask_int64 (Int64.sub v1 v2) w
      | BopMul ->  mask_int64 (Int64.mul v1 v2) w
      | BopUDiv | BopSDiv | BopURem | BopSRem -> failwith "TODO bop"
      | BopShl ->  mask_int64 (Int64.shift_left v1 (Int64.to_int v2)) w
      | BopLShr ->  mask_int64 (Int64.shift_right_logical v1 (Int64.to_int v2)) w
      | BopAShr ->  mask_int64 (Int64.shift_right v1 (Int64.to_int v2)) w
      | BopAnd ->  mask_int64 (Int64.logand v1 v2) w
      | BopOr ->  mask_int64 (Int64.logor v1 v2) w 
      | BopXor ->  mask_int64 (Int64.logxor v1 v2) w
      | BopFAdd | BopFSub | BopFMul | BopFDiv | BopFRem 
          -> raise Illegal_llvm_code)))
  | _ -> raise Illegal_llvm_code


(* Evaluate the predicate for comparison instruction *)  
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
      | IcmpSgt -> if Int64.compare (sign_ext_int64 v1 w) (sign_ext_int64 v2 w) > 0
                   then Int64.one else Int64.zero
      | IcmpSge -> if Int64.compare (sign_ext_int64 v1 w) (sign_ext_int64 v2 w) >= 0
                   then Int64.one else Int64.zero
      | IcmpSlt -> if Int64.compare (sign_ext_int64 v1 w) (sign_ext_int64 v2 w) < 0
                   then Int64.one else Int64.zero
      | IcmpSle -> if Int64.compare (sign_ext_int64 v1 w) (sign_ext_int64 v2 w) <= 0
                   then Int64.one else Int64.zero)))
  | _ -> failwith "Not yet implemented."


(* Evaluate conversion operations *)
let eval_conv_op convop ty1 val1 ty2 =
  match ty1,val1,ty2 with 
  | TyInt(w1),CInt(w2,v1),TyInt(w3) -> (
    match convop with 
    | CopTrunc -> CInt(w3,mask_int64 v1 w3)
    | CopZExt -> CInt(w3, v1)
    | CopSExt -> failwith "todo"
    | CopFPTrunc -> failwith "todo"
    | CopFPExt -> failwith "todo"
    | CopFPToUI -> failwith "todo"
    | CopFPToSI -> failwith "todo"
    | CopUIToFP -> failwith "todo"
    | CopSIToFP -> failwith "todo"
    | CopPtrToInt -> failwith "todo"
    | CopIntToPtr -> failwith "todo"
    | CopBitCast -> failwith "todo")
  | _ -> failwith "Not yet implemented."
  

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
  | IConvOp(id,convop,ty1,v,ty2)::lst -> 
    let nval = eval_conv_op convop ty1 (eval_expr env v) ty2 in
    let env' = env_add (LocalId(id)) nval env in
    eval_inst lst env'
  | ICmp(id,pred,ty,v1,v2)::lst ->
    let nval = eval_icmp_pred pred (eval_expr env v1) (eval_expr env v2) in
    let env' = env_add (LocalId(id)) nval env in
    eval_inst lst env'  
  | _ -> failwith "Instruction not implemented."


(* Evaluate phi functions *)
let eval_phi phis src_label env_orig =
  let foldfun env (LLPhi(id,ty,lvpairs)) =
    let e = try List.assoc src_label lvpairs with _ -> raise Illegal_llvm_code in
    env_add (LocalId(id)) (eval_expr env_orig e) env
  in
    List.fold_left foldfun env_orig phis


(* Evaluate the graph if basic blocks in a function *)
let rec eval_blocks blocks src_label block_label env = 
  let LLBlock(phis,insts) = 
    try List.assoc block_label blocks with _ -> raise Illegal_llvm_code in
  (* Evaluate phi functions, if not the source node *)
  let env = match src_label with  | None -> env
    | Some l -> eval_phi phis l env in
  (* Evaluate instructions in block *)
  match eval_inst insts env with
  | InstRet(None) -> None
  | InstRet(Some v) -> Some v 
  | InstBranch(next_l,env') -> eval_blocks blocks (Some block_label) next_l env'
 

(* Evaluate a function *)
let eval_function blocks params args env  =
  (* Augment environment with arguments *)
  let parargs = try List.combine params args with _ -> raise Illegal_llvm_code in
  let env = List.fold_left (fun e ((ty,id), v) -> env_add (LocalId(id)) v e) env parargs in 
  (* Get first block in function and evaluate blocks*)
  let (start_l,b) = match blocks with b::_ -> b | _ -> raise Illegal_llvm_code in  
  eval_blocks blocks None start_l env   


(**************** Exported functions *******************************)

let eval_fun m bt f args timeout =
  (* Find function to execute *)
  let LLFunc(_,params,blocks) = get_fun m f in
  (* Evaluate function *)
  (0, eval_function blocks params args env_new)
  




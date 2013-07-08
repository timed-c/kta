

open Printf
open LlvmAst
open LlvmUtils
open Ustring.Op
open LlvmPPrint


(*************** Exported types and exceptions ********************)

exception Eval_error_identfier_not_found of string 

type llVal =
| VConst of       (* Constant value *)
    llConst         (* Constant. See llvmAst.mli *)
| VPtr of         (* Pointer. Modeled using an index and a mutable array *)
    int *           (* Index to where in the array the pointer points *)
    llVal array     (* Array of constants. *)

type time = int
type btime = llabel -> llabel -> time

(*************** Local types and exceptions ***********************)

type instTy = 
| InstRet    of llVal option 
| InstBranch of llabel * (llId,llVal) PMap.t


(*************** Local functions **********************************)

let _debug = ref false

let env_new = PMap.empty
let env_add id v env = PMap.add id v env
let env_find id env = PMap.find id env
let utf8 sid = Ustring.to_utf8 (ustring_of_sid sid)


let pprint_val v =
  match v with
  | VConst(c) -> pprint_const c
  | VPtr(i,a) -> us"VPtr(" ^. ustring_of_int i ^. us")"


(* Evaluate an expression to a constant. Looks up identifiers in the environment *)
let eval_expr env e = 
  match e with 
  | ExpId(id,ty) -> (
    try env_find id env 
    with Not_found -> 
      let ids = pprint_llid id in
      raise (Eval_error_identfier_not_found (Ustring.to_utf8 ids)))
  | ExpConst(c) -> VConst(c)
  | ExpConstExpr(_) -> VConst(CInt(1,Int64.zero))   (* TODO *)


(* Evaluating binary operators *)
let eval_bop bop val1 val2 =
  match val1,val2 with
  | VConst(CInt(w,v1)),VConst(CInt(_,v2)) -> (
    let andval = Int64.of_int ((1 lsl w)-1) in
    VConst(CInt(w,Int64.logand andval (
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
          -> raise Illegal_llvm_code))))
  | _ -> raise Illegal_llvm_code


(* Evaluate the predicate for comparison instruction *)  
let eval_icmp_pred icmppred val1 val2 =
  match val1,val2 with
  | VConst(CInt(w,v1)), VConst(CInt(_,v2)) -> (
    VConst(CInt(1, (
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
                   then Int64.one else Int64.zero))))
  | _ -> raise Illegal_llvm_code


(* Evaluate conversion operations *)
let eval_conv_op convop ty1 val1 ty2 =
  match ty1,val1,ty2 with 
  | TyInt(w1),VConst(CInt(w2,v1)),TyInt(w3) -> (VConst(
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
    | CopBitCast -> failwith "todo"))
  | _ -> failwith "Not yet implemented."
  

(* Evaluate a list of instructions *)
let rec eval_inst m instlst env  =
  match instlst with
  (** IRet **)
  | IRet(None)::[] -> InstRet(None)
  | IRet(Some(_,e))::[] -> InstRet(Some(eval_expr env e)) 
  (** IBrCond **)
  | IBrCond(exp,l1,l2)::[] -> (
    match eval_expr env exp with
    | VConst(CInt(1,v)) when Int64.to_int v = 1 -> InstBranch(l1,env)
    | VConst(CInt(1,v)) when Int64.to_int v = 0 -> InstBranch(l2,env)
    | _ -> raise Illegal_llvm_code)
  (** IBrUncond **)
  | IBrUncond(l)::[] -> InstBranch(l,env)
  (** IBinOp **)
  | IBinOp(id,bop,ty,v1,v2)::lst -> 
    let nval = eval_bop bop (eval_expr env v1) (eval_expr env v2) in
    let env' = env_add (LocalId(id)) nval env in
    if !_debug then
      uprint_endline (us"DEBUG: %" ^. ustring_of_sid id ^. us"=" ^. 
      pprint_val nval ^. us" " ^. pprint_binop bop);
    eval_inst m lst env'        
  (** IAlloc **)
  | IAlloca(id,ty,_)::lst ->            
    let rec mklst ty rep acc = 
      if rep = 0 then acc 
      else let acc = mklst ty (rep-1) acc in      
           match ty with 
           | TyInt(w) -> VConst(CInt(w,Int64.zero))::acc
           | TyPointer(ty2) -> 
               (VPtr(0, Array.make 0 (VConst(CInt(0,Int64.zero)))))::acc
           | TyArray(n,elemty) -> mklst elemty n acc
           | _ -> failwith "Type not yet supported"
    in
    if !_debug then 
      uprint_endline (us"DEBUG: %" ^. ustring_of_sid id ^. us" alloca length=" ^. 
      ustring_of_int (List.length (mklst ty 1 [])) ^. us" ty=" ^. pprint_type ty);
    let nval = VPtr(0,Array.of_list (List.rev (mklst ty 1 []))) in
    let env' = env_add (LocalId(id)) nval env in
    eval_inst m lst env'        
  | ILoad(id,ty,e)::lst ->
    let  nval = (match (eval_expr env e) with
                | VPtr(idx,arr) -> ( 
                  let v = Array.get arr idx in
                  if !_debug then 
                    uprint_endline (us"DEBUG: %" ^. ustring_of_sid id ^. us"=" ^. 
                    pprint_val v ^. us" load idx=" ^. ustring_of_int idx); v)
                | _ -> raise Illegal_llvm_code) in
    let env' = env_add (LocalId(id)) nval env in
    eval_inst m lst env'        
  (** IStore **)
  | IStore(e1,ty,e2)::lst ->              
    let v1 = eval_expr env e1 in
    (match eval_expr env e2 with 
     | VPtr(idx,arr) -> 
       if !_debug then 
         uprint_endline (us"DEBUG: store idx=" ^. ustring_of_int idx ^. 
                         us" val=" ^. pprint_val v1);
       Array.set arr idx v1; 
     | _ -> raise Illegal_llvm_code);
    eval_inst m lst env          
  (** GetElementPtr **)
  | IGetElementPtr(id,ty,ptr,indices)::lst -> 
    let rec elems_in_type ty =
      match ty with
      | TyInt(w) -> 1
      | TyPointer(ty2) -> elems_in_type ty2
      | TyArray(n,elem_ty) -> n * (elems_in_type elem_ty)
      | _ -> failwith "Type is not supported in getelementptr"
    in      
    let rec adv_index idx ty indices =
      match ty,indices with
      | TyPointer(ty2),VConst(CInt(_,n))::lst -> adv_index (idx+Int64.to_int n) ty2 lst
      | TyArray(elems,elem_ty),VConst(CInt(_,n))::lst ->
          adv_index (idx + (Int64.to_int n) * (elems_in_type elem_ty)) elem_ty lst
      | _,[] -> idx
      | _,_ -> raise Illegal_llvm_code
    in 
    let nval = (
      match eval_expr env ptr with
      | VPtr(idx,arr) ->         
        let idx' = adv_index idx ty (List.map (eval_expr env) indices) in
        if !_debug then 
          uprint_endline (us"DEBUG: %" ^. ustring_of_sid id ^. us"=" ^.
          ustring_of_int idx' ^. us" getelementptr" ^. us" ty=" ^. pprint_type ty);
        VPtr(idx', arr)
      | _ -> raise Illegal_llvm_code) in
    let env' = env_add (LocalId(id)) nval env in
    eval_inst m lst env'        
  (** IConvOp **)
  | IConvOp(id,convop,ty1,v,ty2)::lst -> 
    let nval = eval_conv_op convop ty1 (eval_expr env v) ty2 in
    let env' = env_add (LocalId(id)) nval env in
    eval_inst m lst env'
  (** ICmp **)
  | ICmp(id,pred,ty,v1,v2)::lst ->
    let nval = eval_icmp_pred pred (eval_expr env v1) (eval_expr env v2) in
    let env' = env_add (LocalId(id)) nval env in
    eval_inst m lst env'  
  (** ICall **)
  | ICall(id_opt,tail,ty,f,args)::lst -> 
    let args' = List.map (eval_expr env) args in
    let LLFunc(_,params,blocks) = get_fun m f in (
    match eval_function m blocks params args' env,id_opt with
    | None,None -> eval_inst m lst env
    | Some nval,Some id -> let env' = env_add (LocalId(id)) nval env in
                   eval_inst m lst env'
    | _,_ -> raise Illegal_llvm_code)
  | _ -> failwith "Instruction not implemented."


(* Evaluate phi functions *)
and eval_phi phis src_label env_orig =
  let foldfun env (LLPhi(id,ty,lvpairs)) =
    let e = try List.assoc src_label lvpairs with _ -> raise Illegal_llvm_code in
    env_add (LocalId(id)) (eval_expr env_orig e) env
  in
    List.fold_left foldfun env_orig phis


(* Evaluate the graph if basic blocks in a function. Returns an option value. *)
and eval_blocks m blocks src_label block_label env = 
  let LLBlock(phis,insts) = 
    try List.assoc block_label blocks with _ -> raise Illegal_llvm_code in
  (* Evaluate phi functions, if not the source node *)
  let env = match src_label with  | None -> env
    | Some l -> eval_phi phis l env in
  (* Evaluate instructions in block *)
  match eval_inst m insts env with
  | InstRet(None) -> None
  | InstRet(Some v) -> Some v 
  | InstBranch(next_l,env') -> eval_blocks m blocks (Some block_label) next_l env'
 

(* Evaluate a function *)
and eval_function m blocks params args env  =
  (* Augment environment with arguments *)
  let parargs = try List.combine params args with _ -> raise Illegal_llvm_code in
  let env = List.fold_left (fun e ((ty,id), v) -> env_add (LocalId(id)) v e) env parargs in 
  (* Get first block in function and evaluate blocks*)
  let (start_l,b) = match blocks with b::_ -> b | _ -> raise Illegal_llvm_code in  
  eval_blocks m blocks None start_l env   


(**************** Exported functions *******************************)

let v32 v = VConst(CInt(32,Int64.of_int v))

let eval_fun m bt f args timeout =
  (* Find function to execute *)
  let LLFunc(_,params,blocks) = get_fun m f in
  (* Evaluate function *)
  (0, eval_function m blocks params args env_new)
  
let enable_debug() = _debug := true



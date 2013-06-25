

open Ustring.Op
open LlvmAst
open Printf

let string_of_label id = us"%" ^. ustring_of_sid id
let string_of_local_id id = us"%" ^. ustring_of_sid id
let string_of_global_id id = us"@" ^. ustring_of_sid id
let pure_string_of_label id = ustring_of_sid id
let string_of_id id = 
  match id with 
  | GlobalId(id) -> us"@" ^. ustring_of_sid id
  | LocalId(id) -> us"%" ^. ustring_of_sid id


let rec pprint_type ty =
  match ty with
  | TyVoid -> us"void"
  | TyInt(i) -> us"i" ^. ustring_of_int i
  | TyFP(FPTyHalf) -> us"half"
  | TyFP(FPTyFloat) -> us"float"
  | TyFP(FPTyDouble) -> us"double"
  | TyFP(FPTyx86_fp80) -> us"x86_fp80"
  | TyFP(FPTyfp128) -> us"fp128"
  | TyFP(FPTyppc_fp128) -> us"ppc_fp128"
  | TyFun(ret_ty,param_tys) -> 
      pprint_type ret_ty ^. us" (" ^.
      Ustring.concat (us",") (List.map pprint_type param_tys) ^. us")"
  | TyPointer(ty2) -> pprint_type ty2 ^. us"*"

let pprint_const c =
  match c with
  | CInt(n,i) -> us(Int64.to_string i)

let pprint_val v = 
  match v with
  | VId(id) -> string_of_id id
  | VConst(c) -> pprint_const c
  | VConstExpr -> us"<constexpr>"  (* TODO *)

let pprint_binop bop = us (
  match bop with 
  | BopAdd -> "add"
  | BopFAdd -> "fadd"
  | BopSub -> "sub"
  | BopFSub -> "fsub"
  | BopMul -> "mul"
  | BopFMul -> "fmul"
  | BopUDiv -> "udiv"
  | BopSDiv -> "sdiv"
  | BopFDiv -> "fdiv"
  | BopURem -> "urem"
  | BopSRem -> "srem"
  | BopFRem -> "frem"
  | BopShl -> "shl"
  | BopLShr -> "lshr"
  | BopAShr -> "ashr"
  | BopAnd -> "and"
  | BopOr -> "or"
  | BopXor -> "xor")

let pprint_conv_op cop = us (
  match cop with
  | CopTrunc -> "trunc"
  | CopZExt -> "zext"
  | CopSExt -> "sext"    
  | CopFPTrunc -> "fptrunc"  
  | CopFPExt -> "fpext"    
  | CopFPToUI -> "fptoui"   
  | CopFPToSI -> "fptosi"   
  | CopUIToFP -> "uitofp"   
  | CopSIToFP -> "sitofp"   
  | CopPtrToInt -> "ptrtoint" 
  | CopIntToPtr -> "inttoptr"
  | CopBitCast -> "bitcast")      

let pprint_icmp_pred_op pred = us (
  match pred with
  | IcmpEq  -> "eq"
  | IcmpNe  -> "ne"
  | IcmpUgt -> "ugt"    
  | IcmpUge -> "uge"
  | IcmpUlt -> "ult"
  | IcmpUle -> "ule"
  | IcmpSgt -> "sgt"
  | IcmpSge -> "sge"
  | IcmpSlt -> "slt"
  | IcmpSle -> "sle")

let pp_fold_inst s inst = 
  let istr = 
    match inst with
   (* -- Terminator instructions -- *)
    | IRet(None) -> us"ret void"
    | IRet(Some(ty,v)) -> us "ret " ^. pprint_type ty ^. us" " ^. pprint_val v
    | IBrCond(c,tl,fl) -> us"br i1 " ^. pprint_val c ^.
           us", label " ^. string_of_label tl ^. 
           us", label " ^. string_of_label fl 
    | IBrUncond(l) -> us"br label " ^. string_of_label l
    | ISwitch -> us"switch (todo)"        
    | IIndirectBr -> us"indirectbr (todo)"    
    | IInvoke -> us"invoke (todo)"      
    | IResume -> us"resume (todo)"        
    | IUnreachable -> us"unreachable (todo)"  
   (* -- Binary operations -- *)
    | IBinOp(id,bop,ty,op1,op2) ->
        (string_of_local_id id) ^. us" = " ^. pprint_binop bop ^. us" " ^. 
          pprint_type ty ^. us" " ^. pprint_val op1 ^. us", " ^. pprint_val op2
   (* -- Vector operations -- *)
    | IExtractElement -> us"IExtractElement (todo)"
    | IInsertElement -> us"IInsertElement (todo)" 
    | IShuffleVector -> us"IShuffleVector (todo)" 
   (* -- Aggregate Operations -- *)
    | IExtractValue -> us"IExtractValue (todo)"   
    | IInsertValue -> us"IInsertValue (todo)"    
   (* -- Memory Access and Addressing Operations -- *)
    | IAlloca -> us"IAlloca (todo)"
    | ILoad -> us"ILoad (todo)"
    | IStore -> us"IStore (todo)" 
    | IFence -> us"IFence (todo)"   
    | ICmpXchg -> us"ICmpXchg (todo)"
    | IAtomicRMW -> us"IAtomicRMW (todo)"  
    | IGetElementPtr -> us"IGetElementPtr (todo)" 
   (* -- Conversion operations -- *)
    | IConvOp(id, cop, ty1, op1, ty2) -> 
      (string_of_local_id id) ^. us" = " ^. pprint_conv_op cop ^. us" " ^.
      pprint_type ty1 ^. us" " ^. pprint_val op1 ^. us" " ^. pprint_type ty2      
   (* -- Miscellaneous instructions -- *)
    | ICmp(id,pred,ty,op1,op2) -> 
        (string_of_local_id id) ^. us" = icmp " ^. pprint_icmp_pred_op pred ^. us" " 
         ^. pprint_type ty ^. us" " ^. pprint_val op1 ^. us", " ^. pprint_val op2
    | IFCmp -> us"IFCmp (todo)"
    | ISelect -> us"ISelect (todo)"         
    | ICall(id, tail, ret_ty, name, args) -> (
        let arglst = List.map (fun (ty,id) -> pprint_type ty ^. us" " ^. 
          pprint_val id) args in
        (match id with None -> us"" | Some(ids) -> 
        (string_of_local_id ids) ^. us" = ") ^.
        us (if tail then "tail " else "") ^. us"call " ^. pprint_type ret_ty ^. 
        us" " ^. string_of_global_id name ^. 
        us"(" ^. Ustring.concat (us", ") arglst ^. us")")
    | IVAArg -> us"IVAArg (todo)" 
    | ILandingPad -> us"ILandingPad (todo)" 
   (* -- PRET Timing Instructions *)
    | IPretGT(id) -> (string_of_local_id id) ^. us" = gt"
    | IPretDU(op) -> us"du " ^. pprint_val op
    | IPretMT(op) -> us"mt " ^. pprint_val op
    | IPretFD -> us"fd"       
   (* -- Other not documented instructions *)
    | IInvalid -> us"IInvalid (todo)"
    | IInvalid2 -> us"IInvalid2 (todo)"
    | IUserOp1 -> us"IUserOp1 (todo)"
    | IUserOp2 -> us"IUserOp2 (todo)"
    | IUnwind -> us"IUnwind (todo)"
  in
    s ^. us"  " ^. istr ^. us"\n"
    
let pp_fold_phi s (LLPhi(id,ty,inlst)) = 
  let lst = List.map (fun (v,l) -> us"[ " ^. pprint_val v ^. us", " ^. 
                      string_of_label l ^. us" ]") inlst in
  let clst = Ustring.concat (us", ") lst in
  s ^. us"  " ^. string_of_local_id id ^. us" = phi " 
  ^. pprint_type ty ^. us" " ^. clst ^. us"\n" 

let pp_fold_block s (label,LLBlock(phis,insts)) = 
    s ^. (pure_string_of_label label) ^. us":\n" ^.
    (List.fold_left pp_fold_phi (us"") phis) ^.
    (List.fold_left pp_fold_inst (us"") insts) 
  
let pp_fold_func s (id,(LLFunc(ty,ps,bbs))) =
  let param_lst = List.map (fun (ty,id) -> 
        pprint_type ty ^. us" " ^. string_of_local_id id) ps in
  let decl = List.length bbs = 0 in
  s ^. (if decl then us"declare " else us"define ") ^.
  pprint_type ty ^. us" " ^. string_of_global_id id ^.
  us"(" ^. Ustring.concat (us", ") param_lst ^. us") " ^.
  (if decl then us"" 
   else 
    us"{\n" ^.
    (List.fold_left pp_fold_block (us"") bbs) ^.
    us"}\n"
  ) ^. us"\n"

  
let pprint_module (LLModule(globs,funcs)) =
    List.fold_left pp_fold_func (us"") funcs


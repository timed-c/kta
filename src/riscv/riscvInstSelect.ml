
open Ustring.Op
open LlvmTree
open LlvmAst

exception Illegal_instruction of string

let i64_has_12bits v = 
  Int64.compare v (Int64.of_int 2048) < 0 &&
  Int64.compare v (Int64.of_int (-2049)) > 0
 
let i64_mask12 x = (Int64.to_int x) land 0b111111111111

(* Create an identifier that has no machine register assigned to it (-1) *)
let mkid id = (id,-1)

let not_imp s = failwith ("Instruction selection for " ^ s ^ " is not implemented.")

(*
  | BopAdd  ->                    | BopFAdd -> 
  | BopSub  ->                    | BopFSub -> 
  | BopMul  ->                    | BopFMul -> 
  | BopUDiv -> RiscvISA.OpDIVU    | BopSDiv -> 
  | BopFDiv ->                    | BopURem -> RiscvISA.OpREMU
  | BopSRem ->                    | BopFRem ->
  | BopShl  ->                    | BopLShr -> RiscvISA.OpSRL
  | BopAShr ->                    | BopAnd  -> RiscvISA.OpAND
  | BopOr   -> RiscvISA.OpOR      | BopXor  -> RiscvISA.OpXOR

TODO:
  - Implement uint module.
  - Add tests the above instructons
  - General impl for icmp using extra jump
  - Impl. all conversion operations
  - Add general add constant using LUI
  - Rewrite to new standard for jalr
  - Add translation function to i32.
*)


(* Binary operation translation for register-register operations *)
let binop2exp op = match op with
  | BopAdd  -> RiscvISA.OpADD     | BopFAdd -> not_imp "BopFadd"
  | BopSub  -> RiscvISA.OpSUB     | BopFSub -> not_imp "BopFSub"
  | BopMul  -> RiscvISA.OpMUL     | BopFMul -> not_imp "BopFMul"
  | BopUDiv -> RiscvISA.OpDIVU    | BopSDiv -> RiscvISA.OpDIV
  | BopFDiv -> not_imp "BopFDiv"  | BopURem -> RiscvISA.OpREMU
  | BopSRem -> RiscvISA.OpREM     | BopFRem -> not_imp "BopFRem"
  | BopShl  -> RiscvISA.OpSLL     | BopLShr -> RiscvISA.OpSRL
  | BopAShr -> RiscvISA.OpSRA     | BopAnd  -> RiscvISA.OpAND
  | BopOr   -> RiscvISA.OpOR      | BopXor  -> RiscvISA.OpXOR




(* Comparison operations. If the second element of the returned tuple 
   is true, the operands of the comparison should be revered. *)
let cmp_pred_op op = match op with
  IcmpEq  -> (RiscvISA.OpBEQ,false)  | IcmpNe  -> (RiscvISA.OpBNE,false)  |
  IcmpUgt -> (RiscvISA.OpBLTU,true)  | IcmpUge -> (RiscvISA.OpBGEU,false) |
  IcmpUlt -> (RiscvISA.OpBLTU,false) | IcmpUle -> (RiscvISA.OpBGEU,true)  |
  IcmpSgt -> (RiscvISA.OpBLT,true)   | IcmpSge -> (RiscvISA.OpBGE,false)  |
  IcmpSlt -> (RiscvISA.OpBLT,false)  | IcmpSle -> (RiscvISA.OpBGE,true)

let noid = usid""

let make_tmp tmpno = (usid ("tmp#" ^ string_of_int tmpno), tmpno+1)  

(** This match algorithm assumes that all integer operations are 32-bits or lower *)
let rec match_tree tree acc_inst tmpno =
  match tree with

  (* Binary Register-Immediate Instructions *)
  | TExp(IBinOp(id,BopAdd,TyInt(32),_,_), [e1;TConst(CInt(_,x))]) |
    TExp(IBinOp(id,BopAdd,TyInt(32),_,_), [TConst(CInt(_,x));e1])
    when i64_has_12bits x ->
      let (cid,acc_inst,tmpno) = match_tree e1 acc_inst tmpno in
      (id,(RiscvISA.SICompImm(RiscvISA.OpADDI,mkid id,mkid cid, i64_mask12 x))::acc_inst,tmpno)

  (* Binary Register-Register Instructions *)
  | TExp(IBinOp(id,(BopAdd  as op),TyInt(32),_,_), [e1;e2]) |
    TExp(IBinOp(id,(BopSub  as op),TyInt(32),_,_), [e1;e2]) |
    TExp(IBinOp(id,(BopMul  as op),TyInt(32),_,_), [e1;e2]) |
    TExp(IBinOp(id,(BopUDiv as op),TyInt(32),_,_), [e1;e2]) |
    TExp(IBinOp(id,(BopSDiv as op),TyInt(32),_,_), [e1;e2]) |
    TExp(IBinOp(id,(BopURem as op),TyInt(32),_,_), [e1;e2]) |
    TExp(IBinOp(id,(BopSRem as op),TyInt(32),_,_), [e1;e2]) |
    TExp(IBinOp(id,(BopShl  as op),TyInt(32),_,_), [e1;e2]) |
    TExp(IBinOp(id,(BopLShr as op),TyInt(32),_,_), [e1;e2]) |
    TExp(IBinOp(id,(BopAShr as op),TyInt(32),_,_), [e1;e2]) |
    TExp(IBinOp(id,(BopAnd  as op),TyInt(32),_,_), [e1;e2]) |
    TExp(IBinOp(id,(BopOr   as op),TyInt(32),_,_), [e1;e2]) |
    TExp(IBinOp(id,(BopXor  as op),TyInt(32),_,_), [e1;e2]) |
    TExp(IBinOp(id,(BopAnd  as op),TyInt(1),_,_),  [e1;e2]) |
    TExp(IBinOp(id,(BopOr   as op),TyInt(1),_,_),  [e1;e2]) |
    TExp(IBinOp(id,(BopXor  as op),TyInt(1),_,_),  [e1;e2])  ->
      let (cid2,acc_inst,tmpno) = match_tree e2 acc_inst tmpno in
      let (cid1,acc_inst,tmpno) = match_tree e1 acc_inst tmpno in
      (id,(RiscvISA.SICompReg(binop2exp op,mkid id,mkid cid1,mkid cid2)::acc_inst),tmpno)
  | TExp(IBinOp(id,op,_,_,_), [e1;e2]) -> 
      not_imp ("IBinOp '" ^ (Ustring.to_utf8 (LlvmPPrint.llbinop op)) ^ "'")
  | TExp(IBinOp(_,_,_,_,_),_) -> raise (Illegal_instruction "IBinOp")

  (* Conditional branches *)
  | TExp(IBrCond(_,l1,l2),[TExp(ICmp(_,op,TyInt(32),_,_),[e1;e2])]) ->
      let (cid2,acc_inst,tmpno) = match_tree e2 acc_inst tmpno in
      let (cid1,acc_inst,tmpno) = match_tree e1 acc_inst tmpno in
      let (cmpop,rev) = cmp_pred_op op in
      let (cid1,cid2) = if rev then (cid2,cid1) else (cid1,cid2) in
      let cond_br = RiscvISA.SICondJmp(cmpop,mkid cid1,mkid cid2,l1) in
      let uncond_br = RiscvISA.SIUncondJmp(RiscvISA.OpJ,l2) in  
      (noid,uncond_br::cond_br::acc_inst,tmpno)
  | TExp(IBrCond(_,l1,l2),[e1;e2]) -> not_imp "IBrCond 2 expr"
  | TExp(IBrCond(_,_,_),_) -> raise (Illegal_instruction "IBrCond")
  | TExp(IRet(_),[]) -> 
      (noid,(RiscvISA.SIIndJmp(RiscvISA.JALR((noid,0),mkid noid,mkid noid)))::acc_inst,tmpno)
  | TExp(IRet(_),[e1]) -> 
      let (cid1,acc_inst,tmpno) = match_tree e1 acc_inst tmpno in
      (noid,(RiscvISA.SIIndJmp(RiscvISA.JALR((noid,0),mkid noid,mkid cid1)))::acc_inst,tmpno)
  | TExp(IRet(_),_) -> raise (Illegal_instruction "IRet")
  | TExp(IBrUncond(_),_) -> not_imp "IBrUncond"
  | TExp(ISwitch(_,_,_),_) -> not_imp "ISwitch"
  | TExp(IIndirectBr,_) -> not_imp "IIndirectBr"
  | TExp(IInvoke,_) -> not_imp "IInvoke"
  | TExp(IResume,_) -> not_imp "IResume"
  | TExp(IUnreachable,_) -> not_imp "IUnreachable"
  | TExp(IExtractElement,_) -> not_imp "IExtractElement"
  | TExp(IInsertElement,_) -> not_imp "IInsertElement"
  | TExp(IShuffleVector,_) -> not_imp "IShuffleVector"
  | TExp(IExtractValue,_) -> not_imp "IExtractValue"
  | TExp(IInsertValue,_) -> not_imp "IInsertValue"
  | TExp(IAlloca(_,_,_),_) -> not_imp "IAlloca"
  | TExp(ILoad(_,_,_),_) -> not_imp "ILoad"
  | TExp(IStore(_,_,_),_) -> not_imp "IStore"
  | TExp(IFence,_) -> not_imp "IFence"
  | TExp(ICmpXchg,_) -> not_imp "ICmpXchg"
  | TExp(IAtomicRMW,_) -> not_imp "IAtomicRMW"
  | TExp(IGetElementPtr(_,_,_,_),_) -> not_imp "IGetElementPtr"
  | TExp(IConvOp(id,CopZExt,TyInt(1),_,TyInt(32)),[e1]) -> 
      let (cid1,acc_inst,tmpno) = match_tree e1 acc_inst tmpno in
      (id,(RiscvISA.SICompImm(RiscvISA.OpADDI,mkid id,mkid cid1, 0))::acc_inst,tmpno)
  | TExp(IConvOp(_,_,_,_,_),_) -> not_imp "IConvOp"

  (* Integer compare instruction (icmp) *)
  | TExp(ICmp(id,IcmpNe,ty,_,_),[e1;TConst(CInt(_,x))]) when Int64.compare Int64.zero x = 0 -> 
      let (cid1,acc_inst,tmpno) = match_tree e1 acc_inst tmpno in
      let (tmp,tmpno) = make_tmp tmpno in
      let i1 = RiscvISA.SICompImm(RiscvISA.OpSLTIU,mkid tmp,mkid cid1, 1) in
      let i2 = RiscvISA.SICompImm(RiscvISA.OpXORI,mkid id,mkid tmp,1) in
      (id,i2::i1::acc_inst,tmpno)
  | TExp(ICmp(id,IcmpEq,ty,_,_),[e1;e2])  -> not_imp "IcmpEq"
  | TExp(ICmp(id,IcmpNe,ty,_,_),[e1;e2])  -> not_imp "IcmpNe" 
  | TExp(ICmp(id,IcmpUgt,ty,_,_),[e1;e2]) -> not_imp "IcmpUgt"
  | TExp(ICmp(id,IcmpUge,ty,_,_),[e1;e2]) -> not_imp "IcmpUge"
  | TExp(ICmp(id,IcmpUlt,ty,_,_),[e1;e2]) -> not_imp "IcmpUlt"
  | TExp(ICmp(id,IcmpUle,ty,_,_),[e1;e2]) -> not_imp "IcmpUle"
  | TExp(ICmp(id,IcmpSgt,ty,_,_),[e1;e2]) -> not_imp "IcmpSgt"
  | TExp(ICmp(id,IcmpSge,ty,_,_),[e1;e2]) -> not_imp "IcmpSge"
  | TExp(ICmp(id,IcmpSlt,ty,_,_),[e1;e2]) -> not_imp "IcmpSlt"
  | TExp(ICmp(id,IcmpSle,ty,_,_),[e1;e2]) -> not_imp "IcmpSle"        
  | TExp(ICmp(_,_,_,_,_),_) -> raise (Illegal_instruction "ICmp")

  | TExp(IFCmp,_) -> not_imp "IFCmp"
  | TExp(ISelect,_) -> not_imp "ISelect"
  | TExp(ICall(_,_,_,_,_),_) -> not_imp "ICall"
  | TExp(IVAArg,_) -> not_imp "IVAArg"
  | TExp(ILandingPad,_) -> not_imp "ILandingPad"
  | TExp(IPretGT(_),_) -> not_imp "IPretGT"
  | TExp(IPretDU(_),_) -> not_imp "IPretDU"
  | TExp(IPretMT(_),_) -> not_imp "IPretMT"
  | TExp(IPretFD,_) -> not_imp "IPretFD"
  | TExp(IInvalid,_) -> not_imp "IInvalid"
  | TExp(IInvalid2,_) -> not_imp "IInvalid2"
  | TExp(IUserOp1,_) -> not_imp "IUserOp1"
  | TExp(IUserOp2,_) -> not_imp "IUserOp2"
  | TExp(IUnwind,_) -> not_imp "IUnwind"
  | TId(LocalId(id)) -> (id,acc_inst,tmpno)
  | TId(GlobalId(id)) -> not_imp "GlobalId"
  | TConst(CInt(_,x)) when i64_has_12bits x -> 
    let (tmp,tmpno) = make_tmp tmpno in
    (tmp,(RiscvISA.SICompImm(RiscvISA.OpADDI, mkid tmp, (noid,0),i64_mask12 x))::acc_inst,tmpno)
  | TConst(_) -> not_imp "large TConst"


let maximal_munch forest tmpno =
  let rev_insts = List.fold_left (fun acc_inst tree ->
    let (_,insts,_) = match_tree tree acc_inst tmpno in 
    insts
  ) [] forest in
  List.rev rev_insts



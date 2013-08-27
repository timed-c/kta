
open Printf
open Ustring.Op
open LlvmTree
open LlvmAst

exception Illegal_instruction of string
exception Code_not_32_bit of string

let i64_has_12bits v = 
  Int64.compare v (Int64.of_int 2048) < 0 &&
  Int64.compare v (Int64.of_int (-2049)) > 0
 
let i64_mask12 x = (Int64.to_int x) land 0b111111111111

(* Create an identifier that has no machine register assigned to it (-1) *)
let mkid id = (id,-1)

let not_imp s = failwith ("Instruction selection for " ^ s ^ " is not implemented.")

let noid = usid""

(* Help function for generating temporary variable names. These generated names
   do not change SSA form. *)
let make_tmp tmpno = (usid ("tmp#" ^ string_of_int tmpno), tmpno+1)  



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

(* Help function for icmp. Generates code for equality. *)
let mk_equal id cid1 cid2 tmpno acc_inst = 
  let (tmp1,tmpno) = make_tmp tmpno in 
  let i1 = RiscvISA.SICompReg(RiscvISA.OpSUB,mkid tmp1,mkid cid1, mkid cid2) in
  let i2 = RiscvISA.SICompImm(RiscvISA.OpSLTIU,mkid id,mkid tmp1, 1) in
  (id,i2::i1::acc_inst,tmpno)

(* Help function for icmp. Generates code for not equal. *)
let mk_not_equal id cid1 cid2 tmpno acc_inst =
  let (tmp1,tmpno) = make_tmp tmpno in 
  let (tmp2,tmpno) = make_tmp tmpno in 
  let i1 = RiscvISA.SICompReg(RiscvISA.OpSUB,mkid tmp1,mkid cid1, mkid cid2) in
  let i2 = RiscvISA.SICompImm(RiscvISA.OpSLTIU,mkid tmp2,mkid tmp1, 1) in
  let i3 = RiscvISA.SICompImm(RiscvISA.OpXORI,mkid id,mkid tmp2, 1) in
  (id,i3::i2::i1::acc_inst,tmpno)

(* Help function for icmp. Generates code for less than. *)
let mk_less_than op id cid1 cid2 tmpno acc_inst =
  let i1 = RiscvISA.SICompReg(op,mkid id,mkid cid1, mkid cid2) in
  (id,i1::acc_inst,tmpno)

(* Help function for icmp. Generates code for not less than. Used for generating
   code of less than and equal, greater than and equal etc. *)
let mk_not_less_than op id cid1 cid2 tmpno acc_inst =
  let (tmp,tmpno) = make_tmp tmpno in 
  let i1 = RiscvISA.SICompReg(op,mkid tmp,mkid cid1, mkid cid2) in
  let i2 = RiscvISA.SICompImm(RiscvISA.OpXORI,mkid id,mkid tmp, 1) in
  (id,i2::i1::acc_inst,tmpno)



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
  | TExp(IBinOp(id,op,TyInt(32),_,_), [e1;e2]) |
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

  (* Return *)
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
  | TExp(ICmp(id,IcmpEq,ty,_,_),[e1;TConst(CInt(_,x))]) when Int64.compare Int64.zero x = 0 -> 
      let (cid1,acc_inst,tmpno) = match_tree e1 acc_inst tmpno in
      let i1 = RiscvISA.SICompImm(RiscvISA.OpSLTIU,mkid id,mkid cid1, 1) in
      (id,i1::acc_inst,tmpno)
  | TExp(ICmp(id,IcmpNe,ty,_,_),[e1;TConst(CInt(_,x))]) when Int64.compare Int64.zero x = 0 -> 
      let (cid1,acc_inst,tmpno) = match_tree e1 acc_inst tmpno in
      let (tmp,tmpno) = make_tmp tmpno in
      let i1 = RiscvISA.SICompImm(RiscvISA.OpSLTIU,mkid tmp,mkid cid1, 1) in
      let i2 = RiscvISA.SICompImm(RiscvISA.OpXORI,mkid id,mkid tmp,1) in
      (id,i2::i1::acc_inst,tmpno)
  | TExp(ICmp(id,op,ty,_,_),[e1;e2])  ->
      let (cid2,acc_inst,tmpno) = match_tree e2 acc_inst tmpno in
      let (cid1,acc_inst,tmpno) = match_tree e1 acc_inst tmpno in (
      match op with  
      | IcmpEq  -> mk_equal id cid1 cid2 tmpno acc_inst
      | IcmpNe  -> mk_not_equal id cid1 cid2 tmpno acc_inst
      | IcmpUgt -> mk_less_than (RiscvISA.OpSLTU) id cid2 cid1 tmpno acc_inst
      | IcmpUge -> mk_not_less_than (RiscvISA.OpSLTU) id cid1 cid2 tmpno acc_inst
      | IcmpUlt -> mk_less_than (RiscvISA.OpSLTU) id cid1 cid2 tmpno acc_inst
      | IcmpUle -> mk_not_less_than (RiscvISA.OpSLTU) id cid2 cid1 tmpno acc_inst
      | IcmpSgt -> mk_less_than (RiscvISA.OpSLT) id cid2 cid1 tmpno acc_inst
      | IcmpSge -> mk_not_less_than (RiscvISA.OpSLT) id cid1 cid2 tmpno acc_inst
      | IcmpSlt -> mk_less_than (RiscvISA.OpSLT) id cid1 cid2 tmpno acc_inst
      | IcmpSle -> mk_not_less_than (RiscvISA.OpSLT) id cid2 cid1 tmpno acc_inst
      )
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

  (* Immediate (constant) *)
  | TConst(CInt(_,x)) when i64_has_12bits x -> 
    let (tmp,tmpno) = make_tmp tmpno in
    (tmp,(RiscvISA.SICompImm(RiscvISA.OpADDI, mkid tmp, (noid,0),i64_mask12 x))::acc_inst,tmpno)
  | TConst(CInt(32,x)) -> 
    let (tmp1,tmpno) = make_tmp tmpno in
    let (tmp2,tmpno) = make_tmp tmpno in
    let low = i64_mask12 x in 
    let high = Int64.to_int (Int64.shift_right_logical x 12) in
    let high' = if low >= 2048 then high + 1 else high in
    let i1 = RiscvISA.SICompImm(RiscvISA.OpLUI, mkid tmp1, (noid,0), high') in
    let i2 = RiscvISA.SICompImm(RiscvISA.OpADDI,mkid tmp2, mkid tmp1, low) in
    (tmp2, i2::i1::acc_inst,tmpno)
  | TConst(CInt(_,_)) -> raise (Code_not_32_bit "TConst")


let maximal_munch forest tmpno =
  let rev_insts = List.fold_left (fun acc_inst tree ->
    let (_,insts,_) = match_tree tree acc_inst tmpno in 
    insts
  ) [] forest in
  List.rev rev_insts




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

(* Binary operation translation for register-register operations *)
let binop2exp op = match op with
  BopAdd -> RiscvISA.OpADD | BopMul -> RiscvISA.OpMUL | _ -> not_imp "BOP"

(* Comparison operations. If the second element of the returned tuple 
   is true, the operands of the comparison should be revered. *)
let cmp_pred_op op = match op with
  IcmpEq  -> (RiscvISA.OpBEQ,false)  | IcmpNe  -> (RiscvISA.OpBNE,false)  |
  IcmpUgt -> (RiscvISA.OpBLTU,true)  | IcmpUge -> (RiscvISA.OpBGEU,false) |
  IcmpUlt -> (RiscvISA.OpBLTU,false) | IcmpUle -> (RiscvISA.OpBGEU,true)  |
  IcmpSgt -> (RiscvISA.OpBLT,true)   | IcmpSge -> (RiscvISA.OpBGE,false)  |
  IcmpSlt -> (RiscvISA.OpBLT,false)  | IcmpSle -> (RiscvISA.OpBGE,true)

let noid = usid""


(** This match algorithm assumes that all integer operations are 32-bits or lower *)
let rec match_tree tree acc_inst =
  match tree with
  (* 32-bit Binary Register-Immediate Instructions *)
  | TExp(IBinOp(id,BopAdd,TyInt(32),_,_), [e1;TConst(CInt(_,x))]) |
    TExp(IBinOp(id,BopAdd,TyInt(32),_,_), [TConst(CInt(_,x));e1])
    when i64_has_12bits x ->
      let (cid,acc_inst) = match_tree e1 acc_inst in
      (id,(RiscvISA.SICompImm(RiscvISA.OpADDI,mkid id,mkid cid, i64_mask12 x))::acc_inst)
  (* 32-bit Binary Register-Register Instructions *)
  | TExp(IBinOp(id,(BopAdd as op),TyInt(32),_,_), [e1;e2]) |
    TExp(IBinOp(id,(BopMul as op),TyInt(32),_,_), [e1;e2]) ->
      let (cid2,acc_inst) = match_tree e2 acc_inst in
      let (cid1,acc_inst) = match_tree e1 acc_inst in
      (id,(RiscvISA.SICompReg(binop2exp op,mkid id,mkid cid1,mkid cid2)::acc_inst))
  | TExp(IBinOp(id,_,_,_,_), [e1;e2]) -> not_imp "IBinOp 2 expr"
  | TExp(IBinOp(_,_,_,_,_),_) -> raise (Illegal_instruction "IBinOp")
  (* Conditional branches *)
  | TExp(IBrCond(_,l1,l2),[TExp(ICmp(_,op,TyInt(32),_,_),[e1;e2])]) ->
      let (cid2,acc_inst) = match_tree e2 acc_inst in
      let (cid1,acc_inst) = match_tree e1 acc_inst in
      let (cmpop,rev) = cmp_pred_op op in
      let (cid1,cid2) = if rev then (cid2,cid1) else (cid1,cid2) in
      let cond_br = RiscvISA.SICondJmp(cmpop,mkid cid1,mkid cid2,l1) in
      let uncond_br = RiscvISA.SIUncondJmp(RiscvISA.OpJ,l2) in  
      (noid,uncond_br::cond_br::acc_inst)
  | TExp(IBrCond(_,l1,l2),[e1;e2]) -> not_imp "IBrCond 2 expr"
  | TExp(IBrCond(_,_,_),_) -> raise (Illegal_instruction "IBrCond")
  | TExp(IRet(_),_) -> not_imp "IRet"
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
  | TExp(IConvOp(_,_,_,_,_),_) -> not_imp "IConvOp"
  | TExp(ICmp(_,_,_,_,_),_) -> not_imp "ICmp"
  | TExp(IFCmp,_) -> not_imp "IFCmp"
  | TExp(ISelect,_) -> not_imp "ISelect"
  | TExp(ICall(_,_,_,_,_),_) -> not_imp "ICall"
  | TExp(IVAArg,_) -> not_imp "IVAArg"
  | TExp(ILandingPad,_) -> not_imp "ILandingPad"
  | TExp(IPretGT(_),_) -> not_imp "IPretGT"
  | TExp(IPretDU(_),_) -> not_imp "IPretDU"
  | TExp(IPretMT(_),_) -> not_imp "IPretMT"
  | TExp(IPretMT(_),_) -> not_imp "IPretMT"
  | TExp(IPretFD(_),_) -> not_imp "IPretFD"
  | TExp(IInvalid,_) -> not_imp "IInvalid"
  | TExp(IInvalid2,_) -> not_imp "IInvalid2"
  | TExp(IUserOp1,_) -> not_imp "IUserOp1"
  | TExp(IUserOp2,_) -> not_imp "IUserOp2"
  | TExp(IUnwind,_) -> not_imp "IUnwind"
  | TId(LocalId(id)) -> (id,acc_inst)
  | TId(GlobalId(id)) -> not_imp "GlobalId"
  | TConst(c) -> not_imp "Const" 



let maximal_munch forest =
  let rev_insts = List.fold_left (fun acc_inst tree ->
    snd  (match_tree tree acc_inst) 
  ) [] forest in
  List.rev rev_insts



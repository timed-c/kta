

open LlvmAst
open Ustring.Op
open Utils

exception Function_not_found of string * sid
exception Illegal_llvm_code of string

let const_int_val width v = ExpConst(CInt(width, Int64.of_int v))


let get_fun m f = 
  let LLModule(_,lst) = m in
  try List.assoc f lst with Not_found -> 
     raise (Function_not_found ((Ustring.to_utf8 (ustring_of_sid f)), f)) 


let rec type_of_exp e = 
  match e with 
  | ExpId(id,ty) -> ty
  | ExpConst(CInt(bits,_)) -> TyInt(bits)
  | ExpConstExpr(ty) -> ty
  

let sign_ext_int64 v n =
  if (Int64.logand (Int64.shift_right_logical v (n-1)) Int64.one) = Int64.zero then v 
  else Int64.logor (Int64.shift_left (Int64.minus_one) n) v


let mask_int64 v n =
        Int64.logand  v (Int64.sub (Int64.shift_left (Int64.one) n) Int64.one)


let defining_id_in_instruction inst =
  match inst with
  | IRet(_) | IBrCond(_,_,_) | IBrUncond(_) | ISwitch(_,_,_) |
    IIndirectBr | IInvoke | IResume | IUnreachable -> None
  | IBinOp(id,_,_,_,_) -> Some(id)
  | IExtractElement | IInsertElement | IShuffleVector |
    IExtractValue | IInsertValue  -> None
  | IAlloca(id,_,_) | ILoad(id,_,_) -> Some(id)
  | IStore(_,_,_) | IFence | ICmpXchg | IAtomicRMW -> None
  | IGetElementPtr(id,_,_,_) | IConvOp(id,_,_,_,_) | ICmp(id,_,_,_,_) -> Some(id)
  | IFCmp | ISelect -> None
  | ICall(op_id,_,_,_,_) -> op_id
  | IVAArg | ILandingPad -> None
  | IPretGT(id) -> Some(id)
  | IPretDU(_) | IPretMT(_) | IPretFD -> None
  | IInvalid | IInvalid2 | IUserOp1 | IUserOp2 | IUnwind -> None


let defining_ids_in_phi_list phis =
  List.fold_left (fun a phi ->
    let LLPhi(id,_,_) = phi in id::a
  ) [] phis 


let defining_ids_in_inst_list insts =
  List.fold_left (fun a inst ->
    match defining_id_in_instruction inst with 
    | None -> a
    | Some(id) -> id::a
  ) [] insts


let rec ids_of_explst elst =     
  match elst with
  | [] -> []
  | ExpId(id,_)::ls -> (id::ids_of_explst ls)
  | _::ls -> ids_of_explst ls  

let using_ids_in_instruction inst = 
  match inst with
  | IRet(None) -> []
  | IRet(Some(_,e)) -> ids_of_explst [e]
  | IBrCond(e,_,_) -> ids_of_explst [e]
  | IBrUncond(_) -> []
  | ISwitch(e1,_,elst) -> 
    ids_of_explst (e1::(fst (List.split elst)))
  | IIndirectBr | IInvoke | IResume | IUnreachable -> []
  | IBinOp(_,_,_,e1,e2) -> ids_of_explst [e1;e2]
  | IExtractElement | IInsertElement | IShuffleVector | 
      IExtractValue | IInsertValue | IAlloca(_,_,_) -> []
  | ILoad(_,_,e) -> ids_of_explst [e]
  | IStore(e1,_,e2) -> ids_of_explst [e1;e2]
  | IFence | ICmpXchg | IAtomicRMW -> []    
  | IGetElementPtr(_,_,e1,elst) -> ids_of_explst (e1::elst)
  | IConvOp(_,_,_,e,_) -> ids_of_explst [e]
  | ICmp(_,_,_,e1,e2) -> ids_of_explst [e1;e2]
  | IFCmp | ISelect -> []
  | ICall(_,_,_,gid,elst) -> (GlobalId(gid))::(ids_of_explst elst)
  | IVAArg | ILandingPad | IPretGT(_) -> []
  | IPretDU(e) | IPretMT(e) -> ids_of_explst [e]
  | IPretFD | IInvalid | IInvalid2 | IUserOp1 | IUserOp2 | IUnwind -> []


let using_ids_in_phi phi =
  let LLPhi(_,_,elst) = phi in  
  ids_of_explst (snd (List.split elst))

  
let using_ids_in_phi_list phis = 
  List.fold_left (fun a phi -> (using_ids_in_phi phi)@a) [] phis 


let using_ids_in_inst_list insts = 
  List.fold_left (fun a inst -> (using_ids_in_instruction inst)@a) [] insts 
  

let local_ids lst = 
  List.fold_left (fun a id -> 
    match id with LocalId(id) -> id::a | _ -> a
  ) [] lst


let global_ids lst = 
  List.fold_left (fun a id -> 
    match id with GlobalId(id) -> id::a | _ -> a
  ) [] lst

  
let used_in_another_block func =
  let LLFunc(_,_,cfg) = func in
  List.fold_left (fun lset (_,b) -> 
    let LLBlock(phis,insts) = b in 
    let lset = List.fold_left (fun lset id -> SidSet.add id lset) 
      lset (local_ids (using_ids_in_phi_list phis)) in
    let def_ids = defining_ids_in_inst_list insts in
    let use_ids = local_ids (using_ids_in_inst_list insts) in
    List.fold_left (fun lset used_id ->
      if List.mem used_id def_ids then lset
      else SidSet.add used_id lset
    ) lset use_ids
  ) 
  (SidSet.empty) cfg











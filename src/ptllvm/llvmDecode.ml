

open Printf
open LlvmAst
open Ustring.Op



(* --------------------------------------------------------------------------*)
(*                      *** Labels and Identifiers ***                       *)
(* --------------------------------------------------------------------------*)

let mkGlobalId s = GlobalId(usid s)
let mkLocalId s = LocalId(usid s)


(* Mapping of numbered variables *)
let (var_map : (Llvm.llvalue,int) Hashtbl.t) = Hashtbl.create 1024 
let var_no = ref 0

(* Generate assignment id. If unnamed temporaries, a new name is generated *)
let mk_assign_id inst =
  let id_str = Llvm.value_name inst in
  if id_str = "" then ( 
    (* Generate names for unnamed temporary variables *)
    Hashtbl.add var_map inst !var_no ;
    var_no := !var_no + 1;
    usid (string_of_int (!var_no - 1)))
  else usid id_str 

(* Reset assignment count for unnamed temp names. 
   Done each time a new function is reached. *)
let reset_assign_id() =
  var_no := 0;
  Hashtbl.clear var_map  


(* TODO: Add the rest of the types *)
let rec toAstTy ty = 
  match Llvm.classify_type ty with
  | Llvm.TypeKind.Void -> TyVoid
  | Llvm.TypeKind.Half -> TyFP(FPTyHalf)
  | Llvm.TypeKind.Float -> TyFP(FPTyFloat)
  | Llvm.TypeKind.Double -> TyFP(FPTyDouble)
  | Llvm.TypeKind.X86fp80 -> TyFP(FPTyx86_fp80)
  | Llvm.TypeKind.Fp128 -> TyFP(FPTyfp128)
  | Llvm.TypeKind.Ppc_fp128 -> TyFP(FPTyppc_fp128)
  | Llvm.TypeKind.Label -> failwith "todo label"
  | Llvm.TypeKind.Integer -> TyInt(Llvm.integer_bitwidth ty)
  | Llvm.TypeKind.Function -> 
    let retty = toAstTy (Llvm.return_type ty) in
    let paramty = 
      Array.fold_right (fun t lst -> (toAstTy t)::lst) (Llvm.param_types ty) [] in 
    TyFun(retty,paramty)
  | Llvm.TypeKind.Struct -> failwith "todo struct"
  | Llvm.TypeKind.Array -> failwith "todo array"
  | Llvm.TypeKind.Pointer -> TyPointer(toAstTy (Llvm.element_type ty))
  | Llvm.TypeKind.Vector -> failwith "todo vector"
  | Llvm.TypeKind.Metadata -> failwith "todo metadata"

(* Convert from the API representation of value to a first class value *)
let toAstVal v = 
  match Llvm.classify_value v with 
  | Llvm.ValueKind.Argument -> VId(mkLocalId (Llvm.value_name v))
  | Llvm.ValueKind.ConstantExpr -> VConstExpr
  | Llvm.ValueKind.ConstantInt ->  
    let bitwidth = Llvm.integer_bitwidth (Llvm.type_of v) in
    let int64 = 
      match Llvm.int64_of_const v with 
      | Some(i) -> i | None -> failwith "Integers larger than 64-bits are not supported." 
    in
      VConst(CInt(bitwidth, int64))
  | Llvm.ValueKind.Instruction(op) -> 
    let name = 
      if Llvm.value_name v = "" then
        string_of_int (try Hashtbl.find var_map v with Not_found -> 99999)
      else Llvm.value_name v                 
    in
      VId(mkLocalId name)
  | _ -> failwith "todo: Value not yet supported"


let toAstIcmpPred pred = 
  match pred with
  | Some Llvm.Icmp.Eq  -> IcmpEq
  | Some Llvm.Icmp.Ne  -> IcmpNe
  | Some Llvm.Icmp.Ugt -> IcmpUgt
  | Some Llvm.Icmp.Uge -> IcmpUge
  | Some Llvm.Icmp.Ult -> IcmpUlt
  | Some Llvm.Icmp.Ule -> IcmpUle
  | Some Llvm.Icmp.Sgt -> IcmpSgt
  | Some Llvm.Icmp.Sge -> IcmpSge
  | Some Llvm.Icmp.Slt -> IcmpSlt
  | Some Llvm.Icmp.Sle -> IcmpSle
  | None -> failwith "Icmp operation without predicate operation."

(* Returns the return type of function type *)
let ret_of_funtype ty = 
  match ty with
  | TyVoid -> ty
  | TyPointer(TyFun(tyret,_)) -> tyret
  | _ -> failwith "Not a return type."

  

(* Help function when folding the list of instructions in a basic block *)
let foldinst (insts,phis) inst =
  let mkbop bop =
    let ty = toAstTy (Llvm.type_of inst) in
    let id = mk_assign_id inst in
    let op1 = toAstVal (Llvm.operand inst 0) in
    let op2 = toAstVal (Llvm.operand inst 1) in
    (IBinOp(id, bop, ty, op1, op2)::insts, phis)
  in
  let mk_conv_op cop =
    let id = mk_assign_id inst in    
    let op1 = toAstVal (Llvm.operand inst 0) in
    let ty1 = toAstTy (Llvm.type_of (Llvm.operand inst 0)) in
    let ty2 = toAstTy (Llvm.type_of inst) in    
    (IConvOp(id, cop, ty1, op1, ty2)::insts, phis)
  in
  match Llvm.instr_opcode inst with
   (* -- Terminator instructions *)
  | Llvm.Opcode.Ret -> 
    if Llvm.num_operands inst = 0 then
      (IRet(None)::insts,phis)
    else
      let i = Llvm.operand inst 0 in
      (IRet(Some(toAstTy (Llvm.type_of i),toAstVal i))::insts,phis)           
  | Llvm.Opcode.Br -> 
    let newi = 
      match Llvm.num_operands inst with
      | 1 -> IBrUncond (usid (Llvm.value_name (Llvm.operand inst 0)))
      | 3 -> IBrCond(toAstVal (Llvm.operand inst 0),
                     usid (Llvm.value_name (Llvm.operand inst 2)),
                     usid (Llvm.value_name (Llvm.operand inst 1)))
      | _ -> failwith "Illegal branch arguments."
    in
      (newi::insts,phis)
  | Llvm.Opcode.Switch -> (ISwitch::insts,phis)       
  | Llvm.Opcode.IndirectBr -> (IIndirectBr::insts,phis)
  | Llvm.Opcode.Invoke -> (IInvoke::insts,phis)
  | Llvm.Opcode.Resume -> (IResume::insts,phis)
  | Llvm.Opcode.Unreachable -> (IUnreachable::insts,phis)
   (* -- Binary operations -- *)
  | Llvm.Opcode.Add -> mkbop BopAdd  
  | Llvm.Opcode.FAdd -> mkbop BopFAdd 
  | Llvm.Opcode.Sub -> mkbop BopSub  
  | Llvm.Opcode.FSub -> mkbop BopFSub 
  | Llvm.Opcode.Mul -> mkbop BopMul  
  | Llvm.Opcode.FMul -> mkbop BopFMul 
  | Llvm.Opcode.UDiv -> mkbop BopUDiv 
  | Llvm.Opcode.SDiv -> mkbop BopSDiv 
  | Llvm.Opcode.FDiv -> mkbop BopFDiv 
  | Llvm.Opcode.URem -> mkbop BopURem 
  | Llvm.Opcode.SRem -> mkbop BopSRem 
  | Llvm.Opcode.FRem -> mkbop BopFRem 
  | Llvm.Opcode.Shl -> mkbop BopShl  
  | Llvm.Opcode.LShr -> mkbop BopLShr  
  | Llvm.Opcode.AShr -> mkbop BopAShr 
  | Llvm.Opcode.And -> mkbop BopAnd  
  | Llvm.Opcode.Or -> mkbop BopOr   
  | Llvm.Opcode.Xor -> mkbop BopXor  
   (* -- Memory Access and Addressing Operations -- *)
  | Llvm.Opcode.Alloca -> failwith "Alloca (todo)"
  | Llvm.Opcode.Load -> failwith "Load (todo)"
  | Llvm.Opcode.Store -> failwith "Store (todo)"
  | Llvm.Opcode.GetElementPtr -> failwith "GetElementPtr (todo)"
   (* -- Conversion operations -- *)
  | Llvm.Opcode.Trunc -> mk_conv_op CopTrunc
  | Llvm.Opcode.ZExt -> mk_conv_op CopZExt
  | Llvm.Opcode.SExt -> mk_conv_op CopSExt
  | Llvm.Opcode.FPToUI -> mk_conv_op CopFPToUI
  | Llvm.Opcode.FPToSI -> mk_conv_op CopFPToSI
  | Llvm.Opcode.UIToFP -> mk_conv_op CopUIToFP
  | Llvm.Opcode.SIToFP -> mk_conv_op CopSIToFP
  | Llvm.Opcode.FPTrunc -> mk_conv_op CopFPTrunc
  | Llvm.Opcode.FPExt -> mk_conv_op CopFPExt
  | Llvm.Opcode.PtrToInt -> mk_conv_op CopPtrToInt
  | Llvm.Opcode.IntToPtr -> mk_conv_op CopIntToPtr
  | Llvm.Opcode.BitCast -> mk_conv_op CopBitCast
   (* -- Miscellaneous instructions -- *)
  | Llvm.Opcode.ICmp -> 
    let id = usid (Llvm.value_name inst) in
    let pred = toAstIcmpPred (Llvm.icmp_predicate inst) in
    let ty = toAstTy (Llvm.type_of (Llvm.operand inst 0)) in
    let op1 = toAstVal (Llvm.operand inst 0) in
    let op2 = toAstVal (Llvm.operand inst 1) in
    (ICmp(id, pred, ty, op1, op2)::insts, phis)
  | Llvm.Opcode.FCmp -> failwith "FCmp (todo)"
   (* -- Miscellaneous instructions -- *)
  | Llvm.Opcode.PHI -> 
      let id = usid (Llvm.value_name inst) in
      let ty = toAstTy (Llvm.type_of  inst) in
      let inlst = List.map (fun (v,l) -> 
        let label = usid (Llvm.value_name (Llvm.value_of_block l)) in
        (toAstVal v, label)) (Llvm.incoming inst) 
      in
      (insts,LLPhi(id,ty,inlst)::phis)
  | Llvm.Opcode.Call ->
    let id = Llvm.value_name inst in
    let idop = if id = "" then None 
      else Some (usid (Llvm.value_name inst)) in
    let tail = Llvm.is_tail_call inst in
    let ops = Llvm.num_operands inst in
    let funop = Llvm.operand inst (ops - 1) in
    let ret_ty = ret_of_funtype (toAstTy (Llvm.type_of funop)) in
    ((match Llvm.value_name funop with
     | "llvm.pret_gt" -> IPretGT(usid id)
     | "llvm.pret_du" -> IPretDU(toAstVal (Llvm.operand inst 0))
     | "llvm.pret_mt" -> IPretMT(toAstVal (Llvm.operand inst 0))
     | "llvm.pret_fd" -> IPretFD
     | _ -> (       
       let name = usid (Llvm.value_name funop) in
       let rec build_args k = 
         if k = ops - 1 then [] else
           let op = Llvm.operand inst k in
           (toAstTy (Llvm.type_of op), toAstVal op)::build_args (k+1)
       in 
       ICall(idop, tail, ret_ty, name, build_args 0))
     )::insts, phis)
  | Llvm.Opcode.Select -> failwith "Select (todo)"
  | Llvm.Opcode.UserOp1 -> failwith "UserOp1 (todo)"
  | Llvm.Opcode.UserOp2 -> failwith "UserOp2 (todo)"
  | Llvm.Opcode.VAArg -> failwith "VAArg (todo)"
  | Llvm.Opcode.ExtractElement -> failwith "ExtractElement (todo)"
  | Llvm.Opcode.InsertElement -> failwith "InsertElement (todo)"
  | Llvm.Opcode.ShuffleVector -> failwith "ShuffleVector (todo)"
  | Llvm.Opcode.ExtractValue -> failwith "ExtractValue (todo)"
  | Llvm.Opcode.InsertValue -> failwith "InsertValue (todo)"
  | Llvm.Opcode.Fence -> failwith "Fence (todo)"
  | Llvm.Opcode.AtomicCmpXchg -> failwith "AtomicCmpXchg (todo)"
  | Llvm.Opcode.AtomicRMW -> failwith "AtomicRMW (todo)"
  | Llvm.Opcode.LandingPad -> failwith "LandingPad (todo)"
  | Llvm.Opcode.Unwind -> failwith "Unwind (todo)"
  | Llvm.Opcode.Invalid -> failwith "Invalid (todo)"
  | Llvm.Opcode.Invalid2 -> failwith "Invalid2 (todo)"


(* Help function when folding the list of basic blocks *)
let foldblock lst bb  = 
  let label = usid (Llvm.value_name (Llvm.value_of_block bb)) in
  let (insts,phis) = Llvm.fold_left_instrs foldinst ([],[]) bb in
  (label,LLBlock(List.rev phis,List.rev insts,None))::lst
  
(* Help functions when folding the function lists of a module *)
let foldfunc llval (LLModule(globs,funcs)) =
  reset_assign_id();
  let id = usid (Llvm.value_name llval) in
  let ty = toAstTy (Llvm.type_of llval) in
  let params = [] in
  let blocks = 
    if Llvm.is_declaration llval then []
    else Llvm.fold_left_blocks foldblock [] llval in  
  let newfunc = (id, LLFunc(ty,params,List.rev blocks)) in
  LLModule(globs,newfunc::funcs)


(** Creates an AST of a LLVM module. *)
let make_module_ast llvmModule =
  (* Start with an empty module *)
  let emptyModule = LLModule([],[]) in

  (* Add both function declarations and definitions *)
  let funcModule = Llvm.fold_right_functions foldfunc llvmModule emptyModule in
  
  (* Return the final AST module *)
  funcModule


(** Read a bitcode file and translate it to an llvm ast. *)
let bcfile2ast filename =    
    let ctx = Llvm.create_context() in
    let buf = Llvm.MemoryBuffer.of_file filename in
    let m = Llvm_bitreader.parse_bitcode ctx buf in
    
    let ast = make_module_ast m in    
    let _ = Llvm.MemoryBuffer.dispose buf in
    ast
    

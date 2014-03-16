

open Printf
open LlvmAst
open Ustring.Op
open LlvmPPrint


exception Llvm_api_error

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
  | Llvm.TypeKind.Struct -> 
     TyStruct(List.map toAstTy (Array.to_list (Llvm.struct_element_types ty)))
  | Llvm.TypeKind.Array -> TyArray(Llvm.array_length ty, 
                                   toAstTy (Llvm.element_type ty))
  | Llvm.TypeKind.Pointer -> TyPointer(toAstTy (Llvm.element_type ty))
  | Llvm.TypeKind.Vector -> failwith "todo vector"
  | Llvm.TypeKind.Metadata -> failwith "todo metadata"

(* Convert from the API representation of value to a first class value *)
let rec toAstVal v = 
  let fail s = failwith ("todo: Value kind " ^ s ^ " is not yet supported.") in
  let vkind = Llvm.classify_value v in
  match vkind with 
  | Llvm.ValueKind.NullValue -> fail "NullValue"
  | Llvm.ValueKind.Argument -> 
    ExpId(mkLocalId (Llvm.value_name v), 
                                   toAstTy (Llvm.type_of v))
  | Llvm.ValueKind.BasicBlock -> fail "BasicBlock"
  | Llvm.ValueKind.InlineAsm -> fail "InlineAsm"
  | Llvm.ValueKind.MDNode -> fail "MDNode"
  | Llvm.ValueKind.MDString -> fail "MDString"
  | Llvm.ValueKind.BlockAddress -> fail "BlockAddress"
  | Llvm.ValueKind.ConstantAggregateZero -> fail "ConstantAggregateZero"
  | Llvm.ValueKind.ConstantArray -> fail "ConstArray"
  | Llvm.ValueKind.ConstantExpr -> ExpConstExpr(toAstTy(Llvm.type_of v))
  | Llvm.ValueKind.ConstantFP -> fail "ConstantFP"
  | Llvm.ValueKind.ConstantInt -> 
    let bitwidth = Llvm.integer_bitwidth (Llvm.type_of v) in
    let intv = 
      match Llvm.int64_of_const v with 
      | Some(i) -> i 
      | None -> failwith "Integers larger than 64-bits are not supported." 
    in
      ExpConst(CInt(bitwidth, intv))
  | Llvm.ValueKind.ConstantPointerNull -> fail "ConstantPointerNull"
  | Llvm.ValueKind.ConstantStruct -> fail "ConstantStruct"
  | Llvm.ValueKind.ConstantVector -> fail "ConstantVector"
  | Llvm.ValueKind.Function -> fail "Function"
  | Llvm.ValueKind.GlobalAlias -> fail "GlobalAlias"
  | Llvm.ValueKind.GlobalVariable ->
     ExpId(mkGlobalId (Llvm.value_name v), toAstTy (Llvm.type_of v))
  | Llvm.ValueKind.UndefValue -> fail "UndefValue"
  | Llvm.ValueKind.Instruction(op) -> 
    let name = 
      if Llvm.value_name v = "" then
        string_of_int (try Hashtbl.find var_map v with Not_found ->
          failwith "unnamed variable not found.")
      else Llvm.value_name v                 
    in
      ExpId(mkLocalId name, toAstTy (Llvm.type_of v))


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
  | Llvm.Opcode.Switch -> 
    let comp = toAstVal (Llvm.operand inst 0) in
    let ldefault = usid (Llvm.value_name (Llvm.value_of_block 
                         (Llvm.switch_default_dest inst))) in
    let ty = Llvm.type_of (Llvm.operand inst 0) in
    let val32 n = Llvm.const_int (Llvm.i32_type (Llvm.type_context ty)) n in
    let arr = Llvm.const_gep (Llvm.operand inst 2) [|val32 0|] in
    let arr2 = Llvm.const_extractvalue arr [|0|] in
    let cases = [(toAstVal arr, 
                  usid (Llvm.value_name (Llvm.operand inst 3)))] in  
    (ISwitch(comp,ldefault,cases)::insts,phis)       
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
   (* -- Stack Memory Access and Addressing Operations -- *)
  | Llvm.Opcode.Alloca -> 
      let id = mk_assign_id inst in 
      let ty = Llvm.type_of inst in 
      let align = 0 in (* TODO: add alignment *) (
      match toAstTy ty with
      | TyPointer(ty2) -> 
        (IAlloca(id,ty2,align)::insts,phis)
      | _ -> raise Llvm_api_error)
  | Llvm.Opcode.Load -> 
      let id = mk_assign_id inst in 
      let ty = toAstTy (Llvm.type_of inst) in 
      let ptr = toAstVal (Llvm.operand inst 0) in      
      (ILoad(id,ty,ptr)::insts,phis)
  | Llvm.Opcode.Store -> 
      let ty = toAstTy (Llvm.type_of (Llvm.operand inst 0)) in 
      let v = toAstVal (Llvm.operand inst 0) in          
      let ptr = toAstVal (Llvm.operand inst 1) in          
      (IStore(v,ty,ptr)::insts,phis)
  | Llvm.Opcode.GetElementPtr -> 
      let rec make_indices n max =
        if n = max then [] 
        else (toAstVal (Llvm.operand inst n))::(make_indices (n+1) max) in
      let id = mk_assign_id inst in 
      let ty = toAstTy (Llvm.type_of (Llvm.operand inst 0)) in 
      let ptr = toAstVal (Llvm.operand inst 0) in          
      let indices = make_indices 1 (Llvm.num_operands inst) in
      (IGetElementPtr(id,ty,ptr,indices)::insts,phis)
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
      let ty = toAstTy (Llvm.type_of inst) in
      let inlst = List.map (fun (v,l) -> 
        let label = usid (Llvm.value_name (Llvm.value_of_block l)) in
        (label,toAstVal v)) (Llvm.incoming inst) 
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
           (toAstVal op)::build_args (k+1)
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
let foldblock lst bb = 
  let label = usid (Llvm.value_name (Llvm.value_of_block bb)) in
  let (insts,phis) = Llvm.fold_left_instrs foldinst ([],[]) bb in
  (label,LLBlock(List.rev phis,List.rev insts))::lst
  
(* Help function for folding function parameters *)
let fold_param lst p = 
  let ty = toAstTy (Llvm.type_of p) in
  let id = usid (Llvm.value_name p) in
  (ty,id)::lst

(* Help functions when folding the function lists of a module *)
let foldfunc llval funcs =
  reset_assign_id();
  let id = usid (Llvm.value_name llval) in
  let ty = ret_of_funtype (toAstTy (Llvm.type_of llval)) in
  let params = List.rev (Llvm.fold_left_params fold_param [] llval) in
  let blocks = 
    if Llvm.is_declaration llval then []
    else Llvm.fold_left_blocks foldblock [] llval in  
  let newfunc = (id, LLFunc(ty,params,List.rev blocks)) in
  newfunc::funcs

let opnum op =
match op with
|	Llvm.Opcode.Invalid -> 1
|	Llvm.Opcode.Ret -> 2
|	Llvm.Opcode.Br -> 3
|	Llvm.Opcode.Switch -> 4
|	Llvm.Opcode.IndirectBr -> 5
|	Llvm.Opcode.Invoke -> 6
|	Llvm.Opcode.Invalid2  -> 7
|	Llvm.Opcode.Unreachable -> 8
|	Llvm.Opcode.Add -> 9
|	Llvm.Opcode.FAdd -> 10
|	Llvm.Opcode.Sub -> 11
|	Llvm.Opcode.FSub -> 12
|	Llvm.Opcode.Mul -> 13
|	Llvm.Opcode.FMul -> 14
|	Llvm.Opcode.UDiv  -> 15
|	Llvm.Opcode.SDiv  -> 16
|	Llvm.Opcode.FDiv -> 17
|	Llvm.Opcode.URem -> 18
|	Llvm.Opcode.SRem -> 19
|	Llvm.Opcode.FRem -> 20
|	Llvm.Opcode.Shl -> 21
|	Llvm.Opcode.LShr -> 22
|	Llvm.Opcode.AShr -> 23
|	Llvm.Opcode.And -> 24
|	Llvm.Opcode.Or  -> 25
|	Llvm.Opcode.Xor -> 26
|	Llvm.Opcode.Alloca -> 27
|	Llvm.Opcode.Load -> 28
|	Llvm.Opcode.Store -> 29
|	Llvm.Opcode.GetElementPtr  -> 30
|	Llvm.Opcode.Trunc -> 31
|	Llvm.Opcode.ZExt -> 32
|	Llvm.Opcode.SExt -> 33
|	Llvm.Opcode.FPToUI -> 34
|	Llvm.Opcode.FPToSI -> 35
|	Llvm.Opcode.UIToFP -> 36
|	Llvm.Opcode.SIToFP -> 37
|	Llvm.Opcode.FPTrunc -> 38
|	Llvm.Opcode.FPExt -> 39
|	Llvm.Opcode.PtrToInt -> 40
|	Llvm.Opcode.IntToPtr -> 41
|	Llvm.Opcode.BitCast -> 42
|	Llvm.Opcode.ICmp  -> 43
|	Llvm.Opcode.FCmp -> 44
|	Llvm.Opcode.PHI -> 45
|	Llvm.Opcode.Call -> 46
|	Llvm.Opcode.Select -> 47
|	Llvm.Opcode.UserOp1 -> 48
|	Llvm.Opcode.UserOp2 -> 49
|	Llvm.Opcode.VAArg -> 50
|	Llvm.Opcode.ExtractElement -> 51
|	Llvm.Opcode.InsertElement -> 52
|	Llvm.Opcode.ShuffleVector -> 53
|	Llvm.Opcode.ExtractValue -> 54
|	Llvm.Opcode.InsertValue -> 55
|	Llvm.Opcode.Fence -> 56
|	Llvm.Opcode.AtomicCmpXchg -> 57
|	Llvm.Opcode.AtomicRMW -> 58
|	Llvm.Opcode.Resume -> 59
|	Llvm.Opcode.LandingPad -> 60
|	Llvm.Opcode.Unwind -> 61

let foldglobals (llvmMod,globals) llval =
  let id = usid (Llvm.value_name llval) in 
  (* Llvm.dump_value llval;*)
  let v = toAstVal ( llval) in
(*  uprint_endline (us"** name = " ^. ustring_of_sid (id) ^. us" val = " ^.
                  pprint_exp v ^. us" constant = " ^. 
                  (if Llvm.is_global_constant llval then us"true" else us"false")); *)
  (llvmMod,globals)


(** Creates an AST of a LLVM module. *)
let make_module_ast llvmModule =
  (* Get globals *)
  let (_,globals) = Llvm.fold_left_globals foldglobals (llvmModule,[]) llvmModule in

  (* Add both function declarations and definitions *)
  let funcModule = Llvm.fold_right_functions foldfunc llvmModule [] in
  
  (* Return the final AST module *)
  LLModule(globals,funcModule)


(** Read a bitcode file and translate it to an llvm ast. *)
let bcfile2ast filename =    
    let ctx = Llvm.create_context() in
    let buf = Llvm.MemoryBuffer.of_file filename in
    let m = Llvm_bitreader.parse_bitcode ctx buf in
    
    let ast = make_module_ast m in    
    let _ = Llvm.MemoryBuffer.dispose buf in
    ast
    

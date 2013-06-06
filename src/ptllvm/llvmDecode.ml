

open Printf
open LlvmAst
open Ustring.Op


(* --------------------------------------------------------------------------*)
(*                      *** Labels and Identifiers ***                       *)
(* --------------------------------------------------------------------------*)

let mkGlobalId s = GlobalId(s)
let mkLocalId s = LocalId(s)


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
  let failmsg v = failwith (v ^ " is not a first class value") in 
  match Llvm.classify_value v with 
  | Llvm.ValueKind.NullValue -> failwith "todo NullValue"
  | Llvm.ValueKind.Argument -> failmsg "Argument"
  | Llvm.ValueKind.BasicBlock -> failmsg "BasicBlock"
  | Llvm.ValueKind.InlineAsm -> failmsg "InlineAsm"
  | Llvm.ValueKind.MDNode -> failmsg "MDNode"
  | Llvm.ValueKind.MDString -> failmsg "MDString"
  | Llvm.ValueKind.BlockAddress -> failmsg "BlockAddress"
  | Llvm.ValueKind.ConstantAggregateZero -> failwith "todo ConstAggregateZero"
  | Llvm.ValueKind.ConstantArray -> failwith "todo ConstArray"
  | Llvm.ValueKind.ConstantExpr -> failwith "todo ConstExpr"
  | Llvm.ValueKind.ConstantFP -> failwith "todo ConstFP"
  | Llvm.ValueKind.ConstantInt -> 
    let bitwidth = Llvm.integer_bitwidth (Llvm.type_of v) in
    let int64 = 
      match Llvm.int64_of_const v with 
      | Some(i) -> i | None -> failwith "Integers larger than 64-bits are not supported." 
    in
      VConst(CInt(bitwidth, int64))
  | Llvm.ValueKind.ConstantPointerNull -> failwith "todo ConstPointerNull"
  | Llvm.ValueKind.ConstantStruct -> failwith "todo ConstStruct"
  | Llvm.ValueKind.ConstantVector -> failwith "todo ConstVector"
  | Llvm.ValueKind.Function  -> failmsg "Function "
  | Llvm.ValueKind.GlobalAlias  -> failmsg "GlobalAlias"
  | Llvm.ValueKind.GlobalVariable -> failwith "todo GlobalVariable"
  | Llvm.ValueKind.UndefValue -> failmsg "UndefValue"
  | Llvm.ValueKind.Instruction(op) -> 
      VId(mkLocalId (Llvm.value_name v))


(* Help function when folding the list of instructions in a basic block *)
let foldinst inst (insts,phis) =
  let mkbop bop =
    let id = mkLocalId (Llvm.value_name inst) in
    let ty = toAstTy (Llvm.type_of inst) in
    let op1 = toAstVal (Llvm.operand inst 0) in
    let op2 = toAstVal (Llvm.operand inst 1) in
    (IBinOp(id, bop, ty, op1, op2)::insts, phis)
  in
  match Llvm.instr_opcode inst with
   (* -- Terminator instructions *)
  | Llvm.Opcode.Ret -> (IRet::insts,phis)           
  | Llvm.Opcode.Br -> 
    let newi = 
      match Llvm.num_operands inst with
      | 1 -> IBrUncond(Llvm.value_name (Llvm.operand inst 0))
      | 3 -> IBrCond(toAstVal (Llvm.operand inst 0),
                     Llvm.value_name (Llvm.operand inst 1),
                     Llvm.value_name (Llvm.operand inst 2))
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
  | Llvm.Opcode.AShr -> mkbop BopAShr 
  | Llvm.Opcode.And -> mkbop BopAnd  
  | Llvm.Opcode.Or -> mkbop BopOr   
  | Llvm.Opcode.Xor -> mkbop BopXor  

   (* -- Miscellaneous instructions -- *)
  | Llvm.Opcode.PHI -> 
      let id = mkLocalId (Llvm.value_name inst) in
      let ty = toAstTy (Llvm.type_of inst) in
      let inlst = List.map (fun (v,l) -> 
        let label = Llvm.value_name (Llvm.value_of_block l) in
        (toAstVal v, label)) (Llvm.incoming inst) 
      in
      (insts,LLPhi(id,ty,inlst)::phis)
  | _ -> (IInvalid::insts,phis)  (* TODO: Make complete *)

(* Help function when folding the list of basic blocks *)
let foldblock bb lst = 
  let label = Llvm.value_name (Llvm.value_of_block bb) in
  let (insts,phis) = Llvm.fold_right_instrs foldinst bb ([],[]) in
  LLBlock(label,phis,insts)::lst
  
(* Help functions when folding the function lists of a module *)
let foldfunc llval (LLModule(globs,funcs)) =
  let id = mkGlobalId (Llvm.value_name llval) in
  let ty = toAstTy (Llvm.type_of llval) in
  let params = [] in
  let blocks = 
    if Llvm.is_declaration llval then []
    else Llvm.fold_right_blocks foldblock llval []  in  
  let newfunc = LLFunc(id,ty,params,blocks) in
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
    

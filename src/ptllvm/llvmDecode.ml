

open Printf
open LlvmAst

(* --------------------------------------------------------------------------*)
(*                      *** Labels and Identifiers ***                       *)
(* --------------------------------------------------------------------------*)

let mkGlobalId s = GlobalId(s)
let mkLocalId s = LocalId(s)
let mkLabel s = LLabel(s)
let string_of_label (LLabel(s)) = "%" ^ s
let pure_string_of_label (LLabel(s)) = s
let string_of_id id = 
  match id with 
  | GlobalId(s) -> "@" ^ s
  | LocalId(s) -> "%" ^ s



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
      | 1 -> IBrUncond(mkLabel (Llvm.value_name (Llvm.operand inst 0)))
      | 3 -> IBrCond(toAstVal (Llvm.operand inst 0),
                     mkLabel (Llvm.value_name (Llvm.operand inst 1)),
                     mkLabel (Llvm.value_name (Llvm.operand inst 2)))
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
        let label = mkLabel (Llvm.value_name (Llvm.value_of_block l)) in
        (toAstVal v, label)) (Llvm.incoming inst) 
      in
      (insts,LLPhi(id,ty,inlst)::phis)
  | _ -> (IInvalid::insts,phis)  (* TODO: Make complete *)

(* Help function when folding the list of basic blocks *)
let foldblock bb lst = 
  let label = mkLabel (Llvm.value_name (Llvm.value_of_block bb)) in
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
let makeAST llvmModule =
  (* Start with an empty module *)
  let emptyModule = LLModule([],[]) in

  (* Add both function declarations and definitions *)
  let funcModule = Llvm.fold_right_functions foldfunc llvmModule emptyModule in
  
  (* Return the final AST module *)
  funcModule


(* --------------------------------------------------------------------------*)
(*                           *** PRETTY PRINTER ***                          *)
(* --------------------------------------------------------------------------*)


let rec ppType ty =
  match ty with
  | TyVoid -> "void"
  | TyInt(i) -> "i" ^ string_of_int i
  | TyFP(FPTyHalf) -> "half"
  | TyFP(FPTyFloat) -> "float"
  | TyFP(FPTyDouble) -> "double"
  | TyFP(FPTyx86_fp80) -> "x86_fp80"
  | TyFP(FPTyfp128) -> "fp128"
  | TyFP(FPTyppc_fp128) -> "ppc_fp128"
  | TyFun(ret_ty,param_tys) -> 
      ppType ret_ty ^ " (" ^
      String.concat "," (List.map ppType param_tys) ^ ")"
  | TyPointer(ty2) -> ppType ty2 ^ "*"
 

let ppConst c =
  match c with
  | CInt(n,i) -> ppType (TyInt(n)) ^ " " ^ Int64.to_string i

let ppVal v = 
  match v with
  | VId(id) -> string_of_id id
  | VConst(c) -> ppConst c

let ppBinop bop =
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
  | BopXor -> "xor" 

let ppFoldInst s inst = 
  let istr = 
    match inst with
   (* -- Terminator instructions -- *)
    | IRet -> "ret (todo)"          
    | IBrCond(c,tl,fl) -> "br " ^ ppVal c ^ 
           ", label " ^ string_of_label tl ^ 
           ", label " ^ string_of_label fl 
    | IBrUncond(l) -> "br label " ^ string_of_label l
    | ISwitch -> "switch (todo)"        
    | IIndirectBr -> "indirectbr (todo)"    
    | IInvoke -> "invoke (todo)"      
    | IResume -> "resume (todo)"        
    | IUnreachable -> "unreachable (todo)"  
   (* -- Binary operations -- *)
    | IBinOp(id,bop,ty,op1,op2) ->
        (string_of_id id) ^ " = " ^ ppBinop bop ^ " " ^ ppType ty ^
          " " ^ ppVal op1 ^ ", " ^ ppVal op2
    | IInvalid -> "invalid **"
    | _ -> "unknown instruction (todo)"
  in
    s ^ "  " ^ istr ^ "\n"
    
let ppFoldPhi s (LLPhi(id,ty,inlst)) = 
  let lst = List.map (fun (v,l) -> "[" ^ ppVal v ^ ", " ^ 
                      string_of_label l ^ "]") inlst in
  let clst = String.concat ", " lst in
  s ^ "  " ^ string_of_id id ^ " = phi " ^ ppType ty ^ " " ^ clst ^ "\n" 

let ppFoldBlock s (LLBlock(label,phis,insts)) = 
    s ^ (pure_string_of_label label) ^ ":\n" ^
    (List.fold_left ppFoldPhi "" phis) ^
    (List.fold_left ppFoldInst "" insts) ^ "\n"
  
let ppFoldFunc s (LLFunc(id,ty,ps,bbs)) =
  let decl = List.length bbs = 0 in
  s ^ (if decl then "declare " else "define ") ^
  ppType ty ^ " " ^ string_of_id id ^
  (if decl then "" 
   else 
    "{\n" ^
    (List.fold_left ppFoldBlock "" bbs) ^
    "}\n"
  ) ^ "\n"

  
let pprintMod (LLModule(globs,funcs)) =
    List.fold_left ppFoldFunc "" funcs 

let ppInst inst = ppFoldInst "" inst
let ppPhi phi = ppFoldPhi "" phi
let ppBlock block = ppFoldBlock "" block
let ppFunc f = ppFoldFunc "" f


let pprint_lltype lltype =
    match Llvm.classify_type lltype with
    | Llvm.TypeKind.Void -> "Void"
    | Llvm.TypeKind.Half -> "Half"
    | Llvm.TypeKind.Float -> "Float"
    | Llvm.TypeKind.Double -> "Double"
    | Llvm.TypeKind.X86fp80 -> "X86fp80"
    | Llvm.TypeKind.Fp128 -> "Fp128"
    | Llvm.TypeKind.Ppc_fp128 -> "Ppc_fp128"
    | Llvm.TypeKind.Label -> "Label"
    | Llvm.TypeKind.Integer -> "Integer"
    | Llvm.TypeKind.Function -> "Function"
    | Llvm.TypeKind.Struct -> "Struct"
    | Llvm.TypeKind.Array -> "Array"
    | Llvm.TypeKind.Pointer -> "Pointer"
    | Llvm.TypeKind.Vector -> "Vector" 
    | Llvm.TypeKind.Metadata -> "Metadata"

let pprint_llval llval =
   printf "%s : %s\n" (Llvm.value_name llval) (pprint_lltype (Llvm.type_of llval))

let pprint_mod m = 
    let _ = print_endline "-- Globals --" in
    let _ = Llvm.iter_globals pprint_llval m in
    let _ = print_endline "-- Functions --" in
    Llvm.iter_functions pprint_llval m 

(** Read a bitcode file and translate it to an llvm ast. *)
let bcfile2ast filename =    
    let ctx = Llvm.create_context() in
    let buf = Llvm.MemoryBuffer.of_file filename in
    let m = Llvm_bitreader.parse_bitcode ctx buf in
    
    (* let _ = pprint_mod m in *)
    let ast = makeAST m in
    let _ = print_endline (pprintMod ast) in
    
    let _ = Llvm.MemoryBuffer.dispose buf in
    0
    

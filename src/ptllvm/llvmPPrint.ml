

open Ustring.Op
open LlvmAst
open Printf

let string_of_label (LLabel(s)) = "%" ^ s
let pure_string_of_label (LLabel(s)) = s
let string_of_id id = 
  match id with 
  | GlobalId(s) -> "@" ^ s
  | LocalId(s) -> "%" ^ s


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

  
let pprint_module (LLModule(globs,funcs)) =
    us(List.fold_left ppFoldFunc "" funcs) 

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

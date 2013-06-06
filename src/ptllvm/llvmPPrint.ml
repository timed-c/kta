

open Ustring.Op
open LlvmAst
open Printf

let string_of_label (LLabel(s)) = us"%" ^. us(s)
let pure_string_of_label (LLabel(s)) = us(s)
let string_of_id id = 
  match id with 
  | GlobalId(s) -> us"@" ^. us(s)
  | LocalId(s) -> us"%" ^. us(s)


let rec ppType ty =
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
      ppType ret_ty ^. us" (" ^.
      Ustring.concat (us",") (List.map ppType param_tys) ^. us")"
  | TyPointer(ty2) -> ppType ty2 ^. us"*"
 

let ppConst c =
  match c with
  | CInt(n,i) -> ppType (TyInt(n)) ^. us" " ^. us(Int64.to_string i)

let ppVal v = 
  match v with
  | VId(id) -> string_of_id id
  | VConst(c) -> ppConst c

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

let ppFoldInst s inst = 
  let istr = 
    match inst with
   (* -- Terminator instructions -- *)
    | IRet -> us"ret (todo)"          
    | IBrCond(c,tl,fl) -> us"br " ^. ppVal c ^.
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
        (string_of_id id) ^. us" = " ^. pprint_binop bop ^. us" " ^. ppType ty ^.
          us" " ^. ppVal op1 ^. us", " ^. ppVal op2
    | IInvalid -> us"invalid **"
    | _ -> us"unknown instruction (todo)"
  in
    s ^. us"  " ^. istr ^. us"\n"
    
let ppFoldPhi s (LLPhi(id,ty,inlst)) = 
  let lst = List.map (fun (v,l) -> us"[" ^. ppVal v ^. us", " ^. 
                      string_of_label l ^. us"]") inlst in
  let clst = Ustring.concat (us", ") lst in
  s ^. us"  " ^. string_of_id id ^. us" = phi " 
  ^. ppType ty ^. us" " ^. clst ^. us"\n" 

let ppFoldBlock s (LLBlock(label,phis,insts)) = 
    s ^. (pure_string_of_label label) ^. us":\n" ^.
    (List.fold_left ppFoldPhi (us"") phis) ^.
    (List.fold_left ppFoldInst (us"") insts) ^. us"\n"
  
let ppFoldFunc s (LLFunc(id,ty,ps,bbs)) =
  let decl = List.length bbs = 0 in
  s ^. (if decl then us"declare " else us"define ") ^.
  ppType ty ^. us" " ^. string_of_id id ^.
  (if decl then us"" 
   else 
    us"{\n" ^.
    (List.fold_left ppFoldBlock (us"") bbs) ^.
    us"}\n"
  ) ^. us"\n"

  
let pprint_module (LLModule(globs,funcs)) =
    List.fold_left ppFoldFunc (us"") funcs

(*
let ppInst inst = ppFoldInst "" inst
let ppPhi phi = ppFoldPhi "" phi
let ppBlock block = ppFoldBlock "" block
let ppFunc f = ppFoldFunc "" f


let pprint_lltype lltype = us(
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
    | Llvm.TypeKind.Metadata -> "Metadata")

let pprint_llval llval =
   printf "%s : %s\n" (Llvm.value_name llval) (pprint_lltype (Llvm.type_of llval))


let pprint_mod m = 
    let _ = print_endline "-- Globals --" in
    let _ = Llvm.iter_globals pprint_llval m in
    let _ = print_endline "-- Functions --" in
    Llvm.iter_functions pprint_llval m 
*)

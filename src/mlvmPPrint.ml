

open Ustring.Op
open MlvmAst
open Printf
open MlvmUtils

let string_of_label id = us"%" ^. ustring_of_sid id
let string_of_local_id id = us"%" ^. ustring_of_sid id
let string_of_global_id id = us"@" ^. ustring_of_sid id
let pure_string_of_label id = ustring_of_sid id

let llid id = 
  match id with 
  | GlobalId(id) -> us"@" ^. ustring_of_sid id
  | LocalId(id) -> us"%" ^. ustring_of_sid id


let rec lltype ty =
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
      lltype ret_ty ^. us" (" ^.
      Ustring.concat (us",") (List.map lltype param_tys) ^. us")"
  | TyPointer(ty2) -> lltype ty2 ^. us"*"
  | TyArray(elems,ty) -> us"[" ^. ustring_of_int elems ^. us" x " ^.
                         lltype ty ^. us"]"
  | TyStruct(tys) -> 
      us"{" ^. Ustring.concat (us", ") (List.map lltype tys) ^. us"}"

let llconst c =
  match c with
  | CInt(n,i) -> us(Int64.to_string i)

let llexp e = 
  match e with
  | ExpId(id,_) -> llid id
  | ExpConst(c) -> llconst c
  | ExpConstExpr(_) -> us"<constexpr>" 

let llbinop bop = us (
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

let lconv_op cop = us (
  match cop with
  | CopTrunc -> "trunc"
  | CopZExt -> "zext"
  | CopSExt -> "sext"    
  | CopFPTrunc -> "fptrunc"  
  | CopFPExt -> "fpext"    
  | CopFPToUI -> "fptoui"   
  | CopFPToSI -> "fptosi"   
  | CopUIToFP -> "uitofp"   
  | CopSIToFP -> "sitofp"   
  | CopPtrToInt -> "ptrtoint" 
  | CopIntToPtr -> "inttoptr"
  | CopBitCast -> "bitcast")      

let llicmp_pred_op pred = us (
  match pred with
  | IcmpEq  -> "eq"
  | IcmpNe  -> "ne"
  | IcmpUgt -> "ugt"    
  | IcmpUge -> "uge"
  | IcmpUlt -> "ult"
  | IcmpUle -> "ule"
  | IcmpSgt -> "sgt"
  | IcmpSge -> "sge"
  | IcmpSlt -> "slt"
  | IcmpSle -> "sle")



let llinst i = 
  match i with
    (* -- Terminator instructions -- *)
    | IRet(None) -> us"ret void"
    | IRet(Some(ty,v)) -> us "ret " ^. lltype ty ^. us" " ^. llexp v
    | IBrCond(c,tl,fl) -> us"br i1 " ^. llexp c ^.
           us", label " ^. string_of_label tl ^. 
           us", label " ^. string_of_label fl 
    | IBrUncond(l) -> us"br label " ^. string_of_label l
    | ISwitch(comp,ldefault,cases) -> 
        us"switch " ^. lltype (type_of_exp comp) ^. us" " ^.
        llexp comp ^. us", label " ^. string_of_label ldefault ^. us" [\n" ^.
        (List.fold_left (fun a (e,l) -> us"    " ^. 
                        lltype (type_of_exp e) ^.
                        us" " ^. llexp e ^. us", label " ^. 
                        string_of_label l ^. us"\n") (us"") cases) ^. us"  ]"
    | IIndirectBr -> us"indirectbr (todo)"    
    | IInvoke -> us"invoke (todo)"      
    | IResume -> us"resume (todo)"        
    | IUnreachable -> us"unreachable (todo)"  
   (* -- Binary operations -- *)
    | IBinOp(id,bop,ty,op1,op2) ->
        (string_of_local_id id) ^. us" = " ^. llbinop bop ^. us" " ^. 
          lltype ty ^. us" " ^. llexp op1 ^. us", " ^. llexp op2
   (* -- Vector operations -- *)
    | IExtractElement -> us"IExtractElement (todo)"
    | IInsertElement -> us"IInsertElement (todo)" 
    | IShuffleVector -> us"IShuffleVector (todo)" 
   (* -- Aggregate Operations -- *)
    | IExtractValue -> us"IExtractValue (todo)"   
    | IInsertValue -> us"IInsertValue (todo)"    
   (* -- Memory Access and Addressing Operations -- *)
    | IAlloca(id,ty,align) -> 
        (string_of_local_id id) ^. us" = alloca " ^. lltype ty ^.
        (if align != 0 then us", align " ^. ustring_of_int align else us"")
    | ILoad(id,ty,ptr) -> 
        (string_of_local_id id) ^. us" = load " ^. lltype (TyPointer ty) ^. 
          us" " ^. llexp ptr
    | IStore(v,ty,ptr) -> 
        us"store " ^. lltype ty ^. us" " ^. llexp v ^. us", " ^.
          lltype (TyPointer ty) ^. us" " ^. llexp ptr
    | IFence -> us"IFence (todo)"   
    | ICmpXchg -> us"ICmpXchg (todo)"
    | IAtomicRMW -> us"IAtomicRMW (todo)"  
    | IGetElementPtr(id,ty,ptr,indices) -> 
      (string_of_local_id id) ^. us" = getelementptr " ^. lltype ty ^.
      us" " ^. llexp ptr ^. 
        (List.fold_left (fun a e -> a ^. us", " ^. lltype (type_of_exp e) ^.
                        us" " ^. llexp e) (us"") indices) 
   (* -- Conversion operations -- *)
    | IConvOp(id, cop, ty1, op1, ty2) -> 
      (string_of_local_id id) ^. us" = " ^. lconv_op cop ^. us" " ^.
      lltype ty1 ^. us" " ^. llexp op1 ^. us" " ^. lltype ty2      
   (* -- Miscellaneous instructions -- *)
    | ICmp(id,pred,ty,op1,op2) -> 
        (string_of_local_id id) ^. us" = icmp " ^. llicmp_pred_op pred ^. us" " 
         ^. lltype ty ^. us" " ^. llexp op1 ^. us", " ^. llexp op2
    | IFCmp -> us"IFCmp (todo)"
    | ISelect -> us"ISelect (todo)"         
    | ICall(id, tail, ret_ty, name, args) -> (
        let arglst = List.map (fun id -> lltype (type_of_exp id) ^. us" " ^. 
          llexp id) args in
        (match id with None -> us"" | Some(ids) -> 
        (string_of_local_id ids) ^. us" = ") ^.
        us (if tail then "tail " else "") ^. us"call " ^. lltype ret_ty ^. 
        us" " ^. string_of_global_id name ^. 
        us"(" ^. Ustring.concat (us", ") arglst ^. us")")
    | IVAArg -> us"IVAArg (todo)" 
    | ILandingPad -> us"ILandingPad (todo)" 
   (* -- PRET Timing Instructions *)
    | IPretGT(id) -> (string_of_local_id id) ^. us" = gt"
    | IPretDU(op) -> us"du " ^. llexp op
    | IPretMT(op) -> us"mt " ^. llexp op
    | IPretFD -> us"fd"       
   (* -- Other not documented instructions *)
    | IInvalid -> us"IInvalid (todo)"
    | IInvalid2 -> us"IInvalid2 (todo)"
    | IUserOp1 -> us"IUserOp1 (todo)"
    | IUserOp2 -> us"IUserOp2 (todo)"
    | IUnwind -> us"IUnwind (todo)"

let pp_fold_inst s i = 
    s ^. us"  " ^. llinst i ^. us"\n"
    
let pp_fold_phi s (LLPhi(id,ty,inlst)) = 
  let lst = List.map (fun (l,v) -> us"[ " ^. llexp v ^. us", " ^. 
                      string_of_label l ^. us" ]") inlst in
  let clst = Ustring.concat (us", ") lst in
  s ^. us"  " ^. string_of_local_id id ^. us" = phi " 
  ^. lltype ty ^. us" " ^. clst ^. us"\n" 


let llblock (LLBlock(phis,insts)) =    
    (List.fold_left pp_fold_phi (us"") phis) ^.
    (List.fold_left pp_fold_inst (us"") insts) 


let pp_fold_block s (label,bbs) = 
    s ^. (pure_string_of_label label) ^. us":\n" ^.llblock bbs
  

let llnamed_func name (LLFunc(ty,ps,bbs)) =
  let param_lst = List.map (fun (ty,id) -> 
        lltype ty ^. us" " ^. string_of_local_id id) ps in
  let decl = List.length bbs = 0 in
  (if decl then us"declare " else us"define ") ^.
  lltype ty ^. us" " ^. name ^.
  us"(" ^. Ustring.concat (us", ") param_lst ^. us") " ^.
  (if decl then us"" 
   else 
    us"{\n" ^.
    (List.fold_left pp_fold_block (us"") bbs) ^.
    us"}\n"
  ) ^. us"\n"

let llfunc bbs = llnamed_func (us"function") bbs
    
let pp_fold_func s (id,func) =
  s ^. llnamed_func (string_of_global_id id) func
  
let llmodule (LLModule(globs,funcs)) =
    List.fold_left pp_fold_func (us"") funcs


let lltree t = 
  let rec pp lev t =
    (Ustring.make (lev*4) (uc (' '))) ^. (
      match t with 
      | MlvmTree.TExp(i,t2) -> us"TExp(" ^. llinst i ^. us")\n" ^. 
        List.fold_left (fun a t ->
          a ^. pp (lev+1) t 
        ) (us"") t2  
      | MlvmTree.TId(id) -> us"TId(" ^. llid id ^. us")\n"
      | MlvmTree.TConst(c) -> us"TConst(" ^. llconst c ^. us")\n")     
  in pp 0 t
    

let llforest f = 
  List.fold_left (fun a t -> a ^. lltree t) (us"") f
    
 


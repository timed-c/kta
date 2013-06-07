

open Printf

open Ustring.Op
(* --------------------------------------------------------------------------*)
(*                      *** Labels and Identifiers ***                       *)
(* --------------------------------------------------------------------------*)

type llabel = sid
type llId = 
|  GlobalId of sid
|  LocalId of sid


(* --------------------------------------------------------------------------*)
(*                           *** Abstract Syntax  ***                        *)
(* --------------------------------------------------------------------------*)


type  llGlobal = bool

type llParam = bool


type fpType =
| FPTyHalf
| FPTyFloat
| FPTyDouble
| FPTyx86_fp80
| FPTyfp128
| FPTyppc_fp128

type llType = 
| TyVoid
| TyInt of int             (* Integer with n number of bits *)
| TyFP  of fpType          (* Floating point types *)
| TyFun of                 (* Function type *) 
    llType *                    (* Return type *)
    llType list                 (* List of parameter types *)
| TyPointer of llType      (* Pointer type of element type *)

type llConst =
| CInt of                  (* Integer constant *)
  int *                       (* Bit width of the integer constant *)
  Int64.t                     (* Integer value. We support up to 64 bits values *)

type llVal =
| VId    of llId           (* Identifier to a value *)
| VConst of llConst        (* Constant value *)

(* Opcodes for binary operator instructions *)
type llBinOp = 
| BopAdd  (* Add two integer values or two vectors of integer values *)
| BopFAdd (* Add two floating point values or two vectors of floating point values *)
| BopSub  (* Subtract two integer values or two vectors of integer values *)
| BopFSub (* Subtract two floating point values or two vectors of floating point values *)
| BopMul  (* Multiply two integer values or two vectors of integer values *)
| BopFMul (* Multiply two floating point values or two vectors of floating point values *)
| BopUDiv (* Unsigned division of two integer values or two vectors of integer values *)
| BopSDiv (* Signed division of two integer values or two vectors of integer values *)
| BopFDiv (* Division of two floating point values or two vectors of floating point values *)
| BopURem (* Unsigned reminder of two integer values or two vectors of integer values *)
| BopSRem (* Signed reminder of two integer values or two vectors of integer values *)
| BopFRem (* Reminder of two floating point values or two vectors of floating point values *)
| BopShl  (* Bit shift left on two integer values or two vectors of integer values *)
| BopLShr (* Logical bit shift right on two integer values or two vectors of integer values *)
| BopAShr (* Arithmetic bit shift right on two integer values or two vectors of integer values *)
| BopAnd  (* Bitwise logical 'and' of two integer values or two vectors of integer values *)
| BopOr   (* Bitwise logical inclusive 'or' of two integer values or two vectors of integer values *)
| BopXor  (* Bitwise logical exclusive 'or' of two integer values or two vectors of integer values *)


(* Opcodes for unary conversion operations *)
type llConvOp = 
| CopTrunc    (* Truncate to the given type (integers or integer vectors) *)
| CopZExt     (* Zero extend to the given type (integers or integer vectors) *)
| CopSExt     (* Sign extend to the given type  (integers or integer vectors) *)
| CopFPTrunc  (* Truncates a floating point value *)
| CopFPExt    (* Extends floating point value*)
| CopFPToUI   (* Floating point to unsigned integer *)
| CopFPToSI   (* Floating point to signed integer *)
| CopUIToFP   (* Unsigned integer to floating point *)
| CopSIToFP   (* Signed integer to floating point *)
| CopPtrToInt (* Converts a pointer to an integer *)
| CopIntToPtr (* Converts an integer to a pointer *)
| CopBitCast  (* A no-op cast to a specific type *)


(* LLVM instructions *)
type llInst = 
   (* -- Terminator instructions *)
| IRet           (* Return from a function call *)
| IBrCond of     (* Conditional branch *)
    llVal *           (* Conditional value. Has type 'i1' *)
    llabel *          (* Label to true branch *)
    llabel            (* Label to false branch *)
| IBrUncond of   (* Unconditional branch *)
    llabel            (* Label to destination *)
| ISwitch        (* Switch instruction - generalization of IBr *)
| IIndirectBr    (* Indirect branch to a local address *)
| IInvoke        (* A call that is used in languages with exceptions *)
| IResume        (* Resumes a existing exception *)
| IUnreachable   (* No defined semantics. Informs that the code is unreachable *)
   (* -- Binary operations -- TODO add 'nuw' and 'nsw' *)
| IBinOp of       (* Various binary operations. See llBinOp *)
    llId *             (* Assignment id *) 
    llBinOp *          (* Binop *)
    llType *           (* Operand type *)        
    llVal *            (* Op1 *)
    llVal              (* Op2 *)
   (* -- Vector operations -- *)
| IExtractElement (* Extracts an element at a specific index *)
| IInsertElement  (* Insert an element at a specific index *)
| IShuffleVector  (* Permutation of vector elements *)
   (* -- Aggregate Operations -- *)
| IExtractValue   (* Extract member element *)
| IInsertValue    (* Insert element *)
   (* -- Memory Access and Addressing Operations -- *)
| IAlloca         (* Allocate memory on the stack. Automatically released when
                        a function returns. *)
| ILoad           (* Load data from memory *) 
| IStore          (* Store data to memory *)
| IFence          (* Synchronization using a fence *)
| ICmpXchg        (* Atomic modification of memory *)
| IAtomicRMW      (* Atomic modification of memory *)
| IGetElementPtr  (* Get the address of a sub-element of an aggregate data structure *)
   (* -- Conversion operations -- *)
| IConvOp of 
    llConvOp      (* Unary conversion operations *) 
   (* -- Miscellaneous instructions -- *)
| ICmp            (* Compares integers, integer vectors, or pointer values *)
| IFCmp           (* Compares floating point values *)
| ISelect         (* Conditionally select without branching *)
| ICall           (* Function call *)
| IVAArg          (* Accessing the variable argument list *)
| ILandingPad     (* The catch part of the LLVM exception mechanism *)
   (* -- Other not documented instructions *)
| IInvalid
| IInvalid2
| IUserOp1
| IUserOp2
| IUnwind


type llPhi = LLPhi of
     llId   *              (* Left hand side identifier *)
     llType *              (* Type of incoming values *)
     (llVal * llabel) list (* List of incoming value/label pair *)

type llBlock = LLBlock of 
     llabel *              (* Basic block label *)
     llPhi list *          (* List of phi functions *) 
     llInst list           (* List of instructions *)

type llFunc = LLFunc of   
     llId *                (* Function name *)
     llType *              (* Return type *)
     llParam list *        (* List of parameters *)
     llBlock list          (* List of basic blocks *)

type llModule = LLModule of
     llGlobal list *       (* List of global variables *)
     llFunc   list         (* List of function definitions and declarations *)






open Printf
open LlvmAst
open LlvmUtils
open Ustring.Op


(*************** Exported types and exceptions ********************)

type time = int
type btime = llabel -> llabel -> time

(*************** Local types and exceptions ***********************)



(*************** Local functions **********************************)

let env_new = PMap.empty
let env_add id v env = PMap.add id v env
let env_find id env = PMap.find id env


let eval_function blocks args env timeout =
  (* Get first block in function *)
  let start = match blocks with b::_ -> b | _ -> raise Illegal_llvm_code in  
  (* Augment environment with arguments *)
  (*  let env = List.fold_left  *)
  (1, VConst(CInt(1,Int64.zero)))


(**************** Exported functions *******************************)

let eval_fun m bt f args timeout =
  (* Find function to execute *)
  let LLFunc(_,_,blocks) = get_fun m f in
  (* Evaluate function *)
  eval_function blocks args env_new timeout
  



open Printf
open LlvmAst
open Ustring.Op


(*************** Exported types and exceptions ********************)

type time = int
type btime = llabel -> llabel -> time


(*************** Local types and exceptions ***********************)




(*************** Local functions **********************************)




(**************** Exported functions *******************************)


let eval_fun m bt f args timeout =
  (0, VConst(CInt(1,Int64.zero)))

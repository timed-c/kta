

open Ustring.Op

type row = int
type col = int
type filename = ustring
type asminfo = 
  | Info of filename * row * col * row * col  
  | NoInfo

(*
type ast = 
| AsmLabel of info * sid * ast
| AsmInst  of info * RiscvISA.inst * sid option * ast
*)

type 'a tokendata = {i:asminfo; v:'a}



let sprint ast = us""

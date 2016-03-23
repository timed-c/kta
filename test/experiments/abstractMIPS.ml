

open Aint32Interval

(* ------------------------ REGISTERS ------------------------------*)

type registers = |R0 |R1 |R2 |R3 |R4 |R5 |R6 |R7
                 |R8 |R9 |R10|R11|R12|R13|R14|R15
                 |R16|R17|R18|R19|R20|R21|R22|R23
                 |R24|R25|R26|R27|R28|R29|R30|R31
    

let zero = R0
let at = R1
let v0 = R2
let v1 = R3
let a0 = R4
let a1 = R5
let a2 = R6
let a3 = R7
let t0 = R8
let t1 = R9
let t2 = R10
let t3 = R11
let t4 = R12
let t5 = R13
let t6 = R14
let t7 = R15
let s0 = R16
let s1 = R17
let s2 = R18
let s3 = R19
let s4 = R20
let s5 = R21
let s6 = R22
let s7 = R23
let t8 = R24
let t9 = R25
let k0 = R26
let k1 = R27
let gp = R28
let sp = R29
let fp = R30
let ra = R31


type astate = {
                 reg8  : aint32; reg16 : aint32; reg24 : aint32;
  reg1 : aint32; reg9  : aint32; reg17 : aint32; reg25 : aint32;
  reg2 : aint32; reg10 : aint32; reg18 : aint32; reg26 : aint32;
  reg3 : aint32; reg11 : aint32; reg19 : aint32; reg27 : aint32;
  reg4 : aint32; reg12 : aint32; reg20 : aint32; reg28 : aint32;
  reg5 : aint32; reg13 : aint32; reg21 : aint32; reg29 : aint32;
  reg6 : aint32; reg14 : aint32; reg22 : aint32; reg30 : aint32;
  reg7 : aint32; reg15 : aint32; reg23 : aint32; reg31 : aint32;

  pc : int;
}


(* Returns the abstract value for a specific register *)
let reg state r =
  match r with
  | R0  -> aint32_const(0)  | R16 -> state.reg16 
  | R1  -> state.reg1       | R17 -> state.reg17 
  | R2  -> state.reg2       | R18 -> state.reg18 
  | R3  -> state.reg3       | R19 -> state.reg19 
  | R4  -> state.reg4       | R20 -> state.reg20 
  | R5  -> state.reg5       | R21 -> state.reg21 
  | R6  -> state.reg6       | R22 -> state.reg22 
  | R7  -> state.reg7       | R23 -> state.reg23 
  | R8  -> state.reg8       | R24 -> state.reg24 
  | R9  -> state.reg9       | R25 -> state.reg25 
  | R10 -> state.reg10      | R26 -> state.reg26 
  | R11 -> state.reg11      | R27 -> state.reg27 
  | R12 -> state.reg12      | R28 -> state.reg28 
  | R13 -> state.reg13      | R29 -> state.reg29 
  | R14 -> state.reg14      | R30 -> state.reg30 
  | R15 -> state.reg15      | R31 -> state.reg31 

let setreg state r v =
  match r with
  | R0   -> state                   | R16 -> {state with reg16 = v}
  | R1   -> {state with reg1 = v}   | R17 -> {state with reg17 = v}
  | R2   -> {state with reg2 = v}   | R18 -> {state with reg18 = v}
  | R3   -> {state with reg3 = v}   | R19 -> {state with reg19 = v}
  | R4   -> {state with reg4 = v}   | R20 -> {state with reg20 = v}
  | R5   -> {state with reg5 = v}   | R21 -> {state with reg21 = v}
  | R6   -> {state with reg6 = v}   | R22 -> {state with reg22 = v}
  | R7   -> {state with reg7 = v}   | R23 -> {state with reg23 = v}
  | R8   -> {state with reg8 = v}   | R24 -> {state with reg24 = v}
  | R9   -> {state with reg9 = v}   | R25 -> {state with reg25 = v}
  | R10  -> {state with reg10 = v}  | R26 -> {state with reg26 = v}
  | R11  -> {state with reg11 = v}  | R27 -> {state with reg27 = v}
  | R12  -> {state with reg12 = v}  | R28 -> {state with reg28 = v}
  | R13  -> {state with reg13 = v}  | R29 -> {state with reg29 = v}
  | R14  -> {state with reg14 = v}  | R30 -> {state with reg30 = v}
  | R15  -> {state with reg15 = v}  | R31 -> {state with reg31 = v}

  



  
(* ----------------------  BASIC BLOCKS   ------------------------*)
  
type pstate = int
type blockid = int
let exit_ = -1

type block_info =
{
   func   : pstate -> pstate;
   nextid : blockid;
   addr   : int; 
}
  
  
(* ------------------------ INSTRUCTIONS -------------------------*)

let add rd rs rt state =
    state

let addi rt rs imm state  =
    state

let bne rs rt label state =
    state

let jr rs state  =
    state

let next state =
    state












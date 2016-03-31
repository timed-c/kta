

open Ustring.Op
open Printf
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

let pprint_reg r =
  us(match r with
  | R0 -> "zero" | R8  -> "t0" | R16 -> "s0" | R24 -> "t8"
  | R1 -> "at"   | R9  -> "t1" | R17 -> "s1" | R25 -> "t9"
  | R2 -> "v0"   | R10 -> "t2" | R18 -> "s2" | R26 -> "k0"
  | R3 -> "v1"   | R11 -> "t3" | R19 -> "s3" | R27 -> "k1"
  | R4 -> "a0"   | R12 -> "t4" | R20 -> "s4" | R28 -> "gp"
  | R5 -> "a1"   | R13 -> "t5" | R21 -> "s5" | R29 -> "sp"
  | R6 -> "a2"   | R14 -> "t6" | R22 -> "s6" | R30 -> "fp"
  | R7 -> "a3"   | R15 -> "t7" | R23 -> "s7" | R31 -> "ra")
      
let int2reg x =
  match x with
  | 0 -> R0 | 8  -> R8  | 16 -> R16 | 24 -> R24
  | 1 -> R1 | 9  -> R9  | 17 -> R17 | 25 -> R25
  | 2 -> R2 | 10 -> R10 | 18 -> R18 | 26 -> R26
  | 3 -> R3 | 11 -> R11 | 19 -> R19 | 27 -> R27
  | 4 -> R4 | 12 -> R12 | 20 -> R20 | 28 -> R28
  | 5 -> R5 | 13 -> R13 | 21 -> R21 | 29 -> R29
  | 6 -> R6 | 14 -> R14 | 22 -> R22 | 30 -> R30
  | 7 -> R7 | 15 -> R15 | 23 -> R23 | 31 -> R31
  | _ -> failwith "Unknown register."


    
(** Abstract program state. Contains a concrete value
    for the program counter and abstract values for 
    registers and memory *)
type pstate = {
  pc : int;
                 reg8  : aint32; reg16 : aint32; reg24 : aint32;
  reg1 : aint32; reg9  : aint32; reg17 : aint32; reg25 : aint32;
  reg2 : aint32; reg10 : aint32; reg18 : aint32; reg26 : aint32;
  reg3 : aint32; reg11 : aint32; reg19 : aint32; reg27 : aint32;
  reg4 : aint32; reg12 : aint32; reg20 : aint32; reg28 : aint32;
  reg5 : aint32; reg13 : aint32; reg21 : aint32; reg29 : aint32;
  reg6 : aint32; reg14 : aint32; reg22 : aint32; reg30 : aint32;
  reg7 : aint32; reg15 : aint32; reg23 : aint32; reg31 : aint32;
}

(** Creates an initial any state of the program state 
    ps = The program counter value *)
let init pc =
  {
    pc = pc;
                         reg16 = aint32_any;
    reg1  = aint32_any;  reg17 = aint32_any;
    reg2  = aint32_any;  reg18 = aint32_any;
    reg3  = aint32_any;  reg19 = aint32_any;
    reg4  = aint32_any;  reg20 = aint32_any;
    reg5  = aint32_any;  reg21 = aint32_any;
    reg6  = aint32_any;  reg22 = aint32_any;
    reg7  = aint32_any;  reg23 = aint32_any;
    reg8  = aint32_any;  reg24 = aint32_any;
    reg9  = aint32_any;  reg25 = aint32_any;
    reg10 = aint32_any;  reg26 = aint32_any;
    reg11 = aint32_any;  reg27 = aint32_any;
    reg12 = aint32_any;  reg28 = aint32_any;
    reg13 = aint32_any;  reg29 = aint32_any;
    reg14 = aint32_any;  reg30 = aint32_any;
    reg15 = aint32_any;  reg31 = aint32_any;
}

     

(** Returns the abstract value for a specific register
   r = register symbol, ps = program state
   returns the abstract value for the register r *)
let reg r ps =
  match r with
  | R0  -> aint32_const(0) | R16 -> ps.reg16 
  | R1  -> ps.reg1         | R17 -> ps.reg17 
  | R2  -> ps.reg2         | R18 -> ps.reg18 
  | R3  -> ps.reg3         | R19 -> ps.reg19 
  | R4  -> ps.reg4         | R20 -> ps.reg20 
  | R5  -> ps.reg5         | R21 -> ps.reg21 
  | R6  -> ps.reg6         | R22 -> ps.reg22 
  | R7  -> ps.reg7         | R23 -> ps.reg23 
  | R8  -> ps.reg8         | R24 -> ps.reg24 
  | R9  -> ps.reg9         | R25 -> ps.reg25 
  | R10 -> ps.reg10        | R26 -> ps.reg26 
  | R11 -> ps.reg11        | R27 -> ps.reg27 
  | R12 -> ps.reg12        | R28 -> ps.reg28 
  | R13 -> ps.reg13        | R29 -> ps.reg29 
  | R14 -> ps.reg14        | R30 -> ps.reg30 
  | R15 -> ps.reg15        | R31 -> ps.reg31 

(** Sets the value of a register.
    r = register symbol, v = abstract value to be set
    ps = program state
    returns the new abstract program state. *)
let setreg r v ps =
  match r with
  | R0   -> ps                   | R16 -> {ps with reg16 = v}
  | R1   -> {ps with reg1 = v}   | R17 -> {ps with reg17 = v}
  | R2   -> {ps with reg2 = v}   | R18 -> {ps with reg18 = v}
  | R3   -> {ps with reg3 = v}   | R19 -> {ps with reg19 = v}
  | R4   -> {ps with reg4 = v}   | R20 -> {ps with reg20 = v}
  | R5   -> {ps with reg5 = v}   | R21 -> {ps with reg21 = v}
  | R6   -> {ps with reg6 = v}   | R22 -> {ps with reg22 = v}
  | R7   -> {ps with reg7 = v}   | R23 -> {ps with reg23 = v}
  | R8   -> {ps with reg8 = v}   | R24 -> {ps with reg24 = v}
  | R9   -> {ps with reg9 = v}   | R25 -> {ps with reg25 = v}
  | R10  -> {ps with reg10 = v}  | R26 -> {ps with reg26 = v}
  | R11  -> {ps with reg11 = v}  | R27 -> {ps with reg27 = v}
  | R12  -> {ps with reg12 = v}  | R28 -> {ps with reg28 = v}
  | R13  -> {ps with reg13 = v}  | R29 -> {ps with reg29 = v}
  | R14  -> {ps with reg14 = v}  | R30 -> {ps with reg30 = v}
  | R15  -> {ps with reg15 = v}  | R31 -> {ps with reg31 = v}

    
(** Pretty prints the entire program state 
    ps = program state
    noregs = number of registers to print *)
let pprint_pstate ps noregs =
  let rec pregs x s =
    if x < noregs then
      let r = int2reg x in
      let n = pprint_reg r ^. us" = " ^. (aint32_pprint (reg r ps)) in      
      let n' = Ustring.spaces_after n 20 ^. us(if x mod 4 = 0 then "\n" else "") in
      pregs (x+1) (s ^. n')        
    else s
  in      
    us(sprintf "PC = 0x%08x\n" ps.pc) ^.
    pregs 1 (us"") 
   

(* ---------------  BASIC BLOCKS AND PRIORITY QUEUE -----------------*)
  
type blockid = int
type distance = int
type listsize = int  
let exit_ = -1

type pqueue = (distance * blockid * listsize * pstate list) list 

  
type block_info =
{
   func   : gstate -> pstate -> pstate;
   nextid : blockid;
   dist   : distance;
   addr   : int; 
}

(* Main state *)
type mstate = {
  prio    : pqueue;           (* Overall priority queue *)
  cblock  : blockid;          (* Current basic block *)
  bitable : block_info array; (* Basic block info table *)
}
  
  
let rec enqueue dist blockid ps queue =
  match queue with
    (* Distance larger? Go to next *)
  | (d,bid,size,pstates)::qs when d > dist ->
      (d,bid,size,pstates)::(enqueue dist blockid ps qs)
    (* Same dist?  *)
  | (d,bid,size,pstates)::qs when dist = d ->
     (* Same block id? *)                          
     if bid = blockid then
       (* Yes, enqueue *)                          
       (d,bid,size+1, ps::pstates)::qs       
     else
       (* No. Go to next *)
       (d,bid,size,pstates)::(enqueue dist blockid ps qs)
    (* Block not found. Enqueue *)
  | qs -> (dist,blockid,1,[ps])::qs


(** Picks the block with highest priority.
    Returns the block id,
    the size of the program state list, and the program state list *)
let dequeue queue =
  match queue with
  | [] -> failwith "Should not happen"
  | (dist,blockid,lsize,ps::pss)::rest ->
    let queue' = (dist,blockid,lsize-1,pss)::rest in
      (blockid,ps,queue')
    

let emptyqueue = []


  
(* ------------------------ CONTINUATION  -------------------------*)

(* Continue and execute the next basic block in turn *)
let continue ms =
  let (blockid,ps,queue') = dequeue ms.prio in
  let ms' = {ms with cblock = blockid, prio = queue'} in
  let bi = ms.bitable.(blockid) in
  bi.func ms' ps

(* Enqueue a new program state using a block id. Returns a machine state *)    
let enqueue_block blockid ps ms in
  let bi = ms.bitable(blockid) in
  let prio' = enqueue bi.dist blockid ps ms.prio in
  {ms with prio = prio'}
    
(* ------------------------ INSTRUCTIONS -------------------------*)

let add rd rs rt ps =
    setreg rd (aint32_add (reg rs ps) (reg rt ps)) ps

let addi rt rs imm ps  =
    setreg rt (aint32_add (reg rs ps) (aint32_const imm)) ps

let bne rs rt label ms ps =
    ps 

let jr rs ms ps  =
    ps

(* Go to next basic block *)
let next ms ps =
  (* Get the block info for the current basic block *)
  let bi = ms.bitable(ms.cblock) in
  (* Enqueue the current program state with the next basic block *)
  let ms' = enqueue_block bi.nextid ps ms in
  (* Continue and process next block *)
  continue ms'


(* -------------------- PSEUDO INSTRUCTIONS -------------------------*)

(* load immediate interval *)
let lii rd l h ps =
  setreg rd (aint32_interval l h) ps


    

(* ------------------- MAIN ANALYSIS FUNCTIONS ----------------------*)
    
(** Main function for analyzing a function *)
let analyze startblock blocks =
  (* Get the block info of the first basic block *)  
  let bi = blocks.(startblock) in

  (* Create a new program state with the start address *)
  let ps = init bi.addr in

  (* Add the start block to the priority queue *)
  let pqueue = enqueue bi.dist startblock ps emptyqueue in

  
  ()
  

















  
  

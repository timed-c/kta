
(*

  Execute the main experiment using the following example
  >> ocamlbuild experiment.native -lib Str -- v0=[1,100]
*)

open Ustring.Op
open Printf
open Aint32Interval
open Scanf
open Str


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

let str2reg str =
  match str with
  | "zero" -> Some R0 | "t0" -> Some R8  | "s0" -> Some R16 | "t8" -> Some R24
  | "at"   -> Some R1 | "t1" -> Some R9  | "s1" -> Some R17 | "t9" -> Some R25
  | "v0"   -> Some R2 | "t2" -> Some R10 | "s2" -> Some R18 | "k0" -> Some R26
  | "v1"   -> Some R3 | "t3" -> Some R11 | "s3" -> Some R19 | "k1" -> Some R27
  | "a0"   -> Some R4 | "t4" -> Some R12 | "s4" -> Some R20 | "gp" -> Some R28
  | "a1"   -> Some R5 | "t5" -> Some R13 | "s5" -> Some R21 | "sp" -> Some R29
  | "a2"   -> Some R6 | "t6" -> Some R14 | "s6" -> Some R22 | "fp" -> Some R30
  | "a3"   -> Some R7 | "t7" -> Some R15 | "s7" -> Some R23 | "ra" -> Some R31
  | _ -> None
    
    
(** Abstract program state. Contains a concrete value
    for the program counter and abstract values for 
    registers and memory *)
type progstate = {
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
let init_pstate =
  {
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
      let n' = Ustring.spaces_after n 18 ^. us(if x mod 4 = 0 then "\n" else "") in
      pregs (x+1) (s ^. n')        
    else s
  in      
    pregs 1 (us"") 
   
(** Join two program states, assuming they have the same program counter value *)
let join_pstates ps1 ps2 =
  {reg1  = aint32_join ps1.reg1  ps2.reg1;  reg17 = aint32_join ps1.reg17 ps2.reg17;
   reg2  = aint32_join ps1.reg2  ps2.reg2;  reg18 = aint32_join ps1.reg18 ps2.reg18;
   reg3  = aint32_join ps1.reg3  ps2.reg3;  reg19 = aint32_join ps1.reg19 ps2.reg19;
   reg4  = aint32_join ps1.reg4  ps2.reg4;  reg20 = aint32_join ps1.reg20 ps2.reg20;
   reg5  = aint32_join ps1.reg5  ps2.reg5;  reg21 = aint32_join ps1.reg21 ps2.reg21;
   reg6  = aint32_join ps1.reg6  ps2.reg6;  reg22 = aint32_join ps1.reg22 ps2.reg22;
   reg7  = aint32_join ps1.reg7  ps2.reg7;  reg23 = aint32_join ps1.reg23 ps2.reg23;
   reg8  = aint32_join ps1.reg8  ps2.reg8;  reg24 = aint32_join ps1.reg24 ps2.reg24;
   reg9  = aint32_join ps1.reg9  ps2.reg9;  reg25 = aint32_join ps1.reg25 ps2.reg25;
   reg10 = aint32_join ps1.reg10 ps2.reg10; reg26 = aint32_join ps1.reg26 ps2.reg26;
   reg11 = aint32_join ps1.reg11 ps2.reg11; reg27 = aint32_join ps1.reg27 ps2.reg27;
   reg12 = aint32_join ps1.reg12 ps2.reg12; reg28 = aint32_join ps1.reg28 ps2.reg28;
   reg13 = aint32_join ps1.reg13 ps2.reg13; reg29 = aint32_join ps1.reg29 ps2.reg29;
   reg14 = aint32_join ps1.reg14 ps2.reg14; reg30 = aint32_join ps1.reg30 ps2.reg30;
   reg15 = aint32_join ps1.reg15 ps2.reg15; reg31 = aint32_join ps1.reg31 ps2.reg31;
   reg16 = aint32_join ps1.reg16 ps2.reg16}
     
(* ---------------  INPUT ARGUMENT HANDLING -----------------*)

(** Returns a new program state that is updated with input from
    the command line using args (a list of arguments) *)
let rec pstate_input ps args =
  let make_error a = raise (Failure ("Unknown argument: " ^ a)) in
  (* No more args? *)
  match args with
  | [] -> ps 
  | a::next_args -> (    
    (* Is a register definition? *)
    match split (regexp "=") a with
    | [reg;value] ->
         (match str2reg reg with
         | Some(regval) ->
             (* Is concrete value? *)
            (try (let aval = aint32_const (int_of_string value) in
                  pstate_input (setreg regval aval ps) next_args)
             with _ -> (
               (* Is abstract interval value? *)
               (match (try bscanf (Scanning.from_string value) "[%d,%d]" (fun x y -> Some(x,y))
                         with _ -> None)
               with
               | Some(l,h) ->
                   let aval = aint32_interval l h in
                   pstate_input (setreg regval aval ps) next_args
               | None -> make_error a)))
          | None -> make_error a)
    | _ -> make_error a)
 
  
    
(* ---------------  BASIC BLOCKS AND PRIORITY QUEUE -----------------*)
  
type blockid = int     (* The block ID type *)
type distance = int    (* The type for describing distances of blocks *)
type listsize = int    (* Size of the list *)

(** Basic block ID na_ means "not applicable". *)
let  na_ = -1

(** Priority queue type *)
type pqueue = (distance * blockid * listsize * progstate list) list 

(** The basic block info entry type is one element in the
    basic block table. This table provides all information about
    how basic blocks are related, which distances they have etc. *)  
type bblock_info =
{
   func   : mstate -> mstate;  (* The function that represents the basic block *)
   nextid : blockid;           (* The identifier that shows the next basic block *)
   dist   : distance;          (* The distance to the exit, that is the number of edges *)
   addr   : int;               (* Address to the first instruction in the basic block *)
}

(** Main state of the analysis *)
and mstate = {
  cblock  : blockid;           (* Current basic block *)
  pc      : int;               (* Current program counter *)
  pstate  : progstate;         (* Current program state *)
  prio    : pqueue;            (* Overall priority queue *)
  bbtable : bblock_info array; (* Basic block info table *)
}

  

(* Enqueue a basic block *)  
let rec enqueue dist blockid ps queue =
  match queue with
    (* Distance larger? Go to next *)
  | (d,bid,size,pss)::qs when d > dist ->
      (d,bid,size,pss)::(enqueue dist blockid ps qs)
    (* Same dist?  *)
  | (d,bid,size,pss)::qs when dist = d ->
     (* Same block id? *)                          
     if bid = blockid then
       (* Yes, enqueue *)                          
       (d,bid,size+1, ps::pss)::qs       
     else
       (* No. Go to next *)
       (d,bid,size,pss)::(enqueue dist blockid ps qs)
    (* Block not found. Enqueue *)
  | qs -> (dist,blockid,1,[ps])::qs


(** Picks the block with highest priority.
    Returns the block id,
    the size of the program state list, and the program state list *)
let dequeue queue =
  match queue with
  (* Have we finished (block id equal to 0)? *)
  | (_,0,_,ps::pss)::rest ->
    (* Join all final program states *)
    let ps' = List.fold_left join_pstates ps pss in
     (0,ps',rest) 
  (* Dequeue the top program state *)  
  | (dist,blockid,lsize,ps::pss)::rest ->      
      let queue' = if lsize = 1 then rest 
                   else (dist,blockid,lsize-1,pss)::rest in 
      (blockid,ps,queue')
  (* This should never happen. It should end with a terminating 
     block id zero block. *)    
  | _ -> failwith "Should not happen"
    
(** Returns the empty queue *)
let emptyqueue = []
 

  
  
(* ------------------------ CONTINUATION  -------------------------*)

(* Continue and execute the next basic block in turn *)
let continue ms =
  let (blockid,ps,queue') = dequeue ms.prio in
  let ms' = {ms with cblock = blockid; prio = queue'} in
  let bi = ms.bbtable.(blockid) in
  bi.func ms' 

(* Enqueue a new program state using a block id. Returns a main state *)    
let enqueue_block blockid ps ms =
  let bi = ms.bbtable.(blockid) in
  let prio' = enqueue bi.dist blockid ps ms.prio in
  {ms with prio = prio'}

let to_mstate ms ps =
  {ms with pstate = ps}
    
(* ------------------------ INSTRUCTIONS -------------------------*)

let add rd rs rt ms =
    let ps = ms.pstate in
    setreg rd (aint32_add (reg rs ps) (reg rt ps)) ps |> to_mstate ms

let addi rt rs imm ms  =
    let ps = ms.pstate in
    setreg rt (aint32_add (reg rs ps) (aint32_const imm)) ps |> to_mstate ms

        
let branch_equality equal rs rt label ms =
    let ps = ms.pstate in    
    let bi = ms.bbtable.(ms.cblock) in
    let (tb,fb) = aint32_test_equal (reg rs ps) (reg rt ps) in
    let (tbranch,fbranch) = if equal then (tb,fb) else (fb,tb) in
    let enq blabel ms (rs,rt) = enqueue_block blabel ps ms in
    let ms = List.fold_left (enq label) ms tbranch in
    let ms = List.fold_left (enq bi.nextid) ms fbranch in
    continue ms

let beq rs rt label ms =
    branch_equality true rt rs label ms
      
let bne rs rt label ms =
    branch_equality false rs rt label ms
  
      
(* Go to next basic block *)
let next ms =
  (* Get the block info for the current basic block *)
  let bi = ms.bbtable.(ms.cblock) in
  (* Enqueue the current program state with the next basic block *)
  let ms' = enqueue_block bi.nextid ms.pstate ms in
  (* Continue and process next block *)
  continue ms'


(* -------------------- PSEUDO INSTRUCTIONS -------------------------*)

(* load immediate interval *)
let lii rd l h ms =
  setreg rd (aint32_interval l h) ms.pstate |> to_mstate ms


     

(* ------------------- MAIN ANALYSIS FUNCTIONS ----------------------*)
    
(** Main function for analyzing an assembly function *)
let analyze startblock bblocks args =

  (* Get the block info of the first basic block *)  
  let bi = bblocks.(startblock) in

  (* Update the program states with abstract inputs from program arguments *)
  let ps =
    try pstate_input init_pstate args
    with Failure s -> (printf "Error: %s\n" s; exit 1) in

 (* uprint_endline (pprint_pstate ps 16);
    exit 0; *)
  
  (* Add the start block to the priority queue *)
  let pqueue = enqueue bi.dist startblock ps emptyqueue in

  (* Create the main state *)
  let mstate = {
    pc = bi.addr;
    cblock= startblock;   (* N/A, since the process has not yet started *)    
    pstate = ps;          (* New program state *)
    prio = pqueue;        (* Starts with a queue with just one element, the entry *)
    bbtable = bblocks;    (* Stores a reference to the basic block info table *)
  } in

  (* Continue the process and execute the next basic block from the queue *)
  continue mstate 
  

(** Print main state info *)
let print_mstate ms =
  uprint_endline (pprint_pstate ms.pstate 32)
    
  

















  
  

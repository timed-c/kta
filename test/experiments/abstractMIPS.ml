
(*

  Execute the main experiment using the following example
  >> ocamlbuild experiment.native -lib Str -- v0=[1,100]
*)

open Ustring.Op
open Printf
open Aint32Interval
open Aregsimple
open Scanf
open Str


let count = ref 0

(* ------------------------ TYPES ------------------------------*)
  
  

type blockid = int     (* The block ID type *)
type distance = int    (* The type for describing distances of blocks *)

  
(** Abstract program state. Contains a concrete value
    for the program counter and abstract values for 
    registers and memory *)
type progstate = {
  reg  : aregister;
  bcet : int;
  wcet : int;
}

  
(** Basic block ID na_ means "not applicable". *)
let  na_ = -1

(** Priority queue type *)
type pqueue = {
  curr_batch   : progstate list;                       (* Current batch of states *)
  curr_blockid : blockid;                              (* Current block id *)
  curr_queue : (distance * blockid * progstate list) list;  (* Queue of next states *)
}
  
(** The basic block info entry type is one element in the
    basic block table. This table provides all information about
    how basic blocks are related, which distances they have etc. *)  
type bblock_info =
{
   func    : mstate -> mstate;  (* The function that represents the basic block *)
   nextid  : blockid;           (* The identifier that shows the next basic block *)
   dist    : distance;          (* The distance to the exit, that is the number of edges *)
   addr    : int;               (* Address to the first instruction in the basic block *)
   joinvar : registers list     (* List of registers that should be handled especially for join *) 
}

(** Main state of the analysis *)
and mstate = {
  cblock  : blockid;           (* Current basic block *)
  pc      : int;               (* Current program counter *)
  pstate  : progstate;         (* Current program state *)
  prio    : pqueue;            (* Overall priority queue *)
  bbtable : bblock_info array; (* Basic block info table *)
}

  
(* ------------------------ REGISTERS ------------------------------*)

    

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
    
(** Creates an initial any state of the program state 
    ps = The program counter value *)
let init_pstate =
  {
    bcet  = 0;
    wcet = 0;
    reg = areg_init;
}

(** Join two program states, assuming they have the same program counter value *)
let join_pstates ps1 ps2 =
{
   bcet  = min ps1.bcet ps2.bcet;
   wcet  = max ps1.wcet ps2.wcet;
   reg   = areg_join [ps1.reg;ps2.reg]
}
    
     
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
                  pstate_input {ps with reg = (setreg regval aval ps.reg)} next_args)
             with _ -> (
               (* Is abstract interval value? *)
               (match (try bscanf (Scanning.from_string value) "[%d,%d]" (fun x y -> Some(x,y))
                         with _ -> None)
               with
               | Some(l,h) ->
                   let aval = aint32_interval l h in
                   pstate_input {ps with reg = (setreg regval aval ps.reg)} next_args
               | None -> make_error a)))
          | None -> make_error a)
    | _ -> make_error a)
 

(* ----------------------- DEBUG FUNCTIONS  -------------------------*)

(** Pretty prints the entire program state 
    ps = program state
    noregs = number of registers to print *)
let pprint_pstate noregs ps =
  let rec pregs x s =
    if x < noregs then
      let r = int2reg x in
      let (_,v) = getreg r ps.reg in
      let n = pprint_reg r ^. us" = " ^. (aint32_pprint v) in      
      let n' = Ustring.spaces_after n 18 ^. us(if x mod 4 = 0 then "\n" else "") in
      pregs (x+1) (s ^. n')        
    else s
  in      
    pregs 1 (us"") 

(* Debug print a program queue element *)
let print_pqueue_elem noregs elem =
  let (dist,id,progs) = elem in
  printf "ID: %d dist: %d\n" id dist;
  List.iter (fun ps ->
    uprint_endline (pprint_pstate noregs ps);
    printf "------\n";
  ) progs
  

(* Debug print the program queue *)
let print_pqueue noregs pqueue =
  List.iter (print_pqueue_elem noregs) pqueue 
      
    
    
(* ---------------  BASIC BLOCKS AND PRIORITY QUEUE -----------------*)
  
           
      
  
(* Enqueue a basic block *)  
let enqueue dist blockid joinvars ps prioqueue =
  count := !count + 1;
  let rec work queue = 
    match queue with
    (* Distance larger? Go to next *)
    | (d,bid,pss)::qs when d > dist ->
      (d,bid,pss)::(work qs)
    (* Same dist?  *)
    | (d,bid,pss)::qs when dist = d ->
     (* Same block id? *)                          
      if bid = blockid then(
       (*  printf "%d\n" (List.length pss);    *)
       (* Yes, enqueue and check for join variables *)       
        (d,bid, ps::pss)::qs       )
     else
       (* No. Go to next *)
       (d,bid,pss)::(work qs)
    (* Block not found. Enqueue *)
    | qs -> (dist,blockid,[ps])::qs
  in
    {prioqueue with curr_queue = work prioqueue.curr_queue}


module JMap = Map.Make(
  struct
    type t = (int * int) list
    let compare = compare
  end)      
  

let strategic_joins ms pss = 
  let bi = ms.bbtable.(ms.cblock) in   
  let jmap = List.fold_left
    (fun map ps ->
      let key = List.map (fun v -> getreg v ps.reg |> snd) bi.joinvar in
      let newval = try join_pstates ps (JMap.find key map)
                   with Not_found -> ps in
      JMap.add key newval map
    )JMap.empty pss in
  List.map (fun (k,v) -> v) (JMap.bindings jmap)
  

  
(** Picks the block with highest priority.
    Returns the block id,
    the size of the program state list, and the program state list *)
let dequeue ms =
  let prioqueue = ms.prio in
  (* Process the batch program states first *)
  match prioqueue.curr_batch with
  (* Found a batch state? *)
  | ps::pss -> (prioqueue.curr_blockid, ps, {prioqueue with curr_batch = pss})
  (* No more batch states. Find the next in the priority queue *)
  | [] -> (
    match prioqueue.curr_queue with
    (* Have we finished (block id equal to 0)? *)
    | (_,0,ps::pss)::rest ->
      (* Join all final program states *)(
      printf "Number of states before final merge: %d\n" (List.length (ps::pss));
      let ps' = List.fold_left join_pstates ps pss in
      let prioqueue' = {prioqueue with curr_queue = rest} in  
      (0,ps',prioqueue'))
    (* Dequeue the top program state *)  
    | (dist,blockid,pss)::rest ->
      let pss' = strategic_joins ms pss in
      let prioqueue' = {
        curr_blockid = blockid;
        curr_batch = List.tl pss';
        curr_queue = rest;
      }
      in
        (blockid,List.hd pss',prioqueue')
    (* This should never happen. It should end with a terminating 
       block id zero block. *)    
    | _ -> failwith "Should not happen"
  )


    
(** Returns the empty queue *)
let emptyqueue = {
  curr_blockid = 0;
  curr_batch = [];
  curr_queue = [];
}
 

  
  
(* ------------------------ CONTINUATION  -------------------------*)

(* Continue and execute the next basic block in turn *)
let continue ms =
  let (blockid,ps,queue') = dequeue ms in
  let ms' = {ms with cblock = blockid;
                     pc = ms.bbtable.(blockid).addr;
                     pstate = ps;
                     prio = queue'} in
  let bi = ms.bbtable.(blockid) in
  bi.func ms' 

(* Enqueue a new program state using a block id. Returns a main state *)    
let enqueue_block blockid ps ms =
  let bi = ms.bbtable.(blockid) in
  let prio' = enqueue bi.dist blockid bi.joinvar ps ms.prio in
  {ms with prio = prio'}

let to_mstate ms ps =
  {ms with pstate = ps}

let tick n ps =  
  {ps with bcet = ps.bcet + n; wcet = ps.wcet + n} 

let update r ps =
  {ps with reg = r}
    
(* ------------------------ INSTRUCTIONS -------------------------*)

let add rd rs rt ms =
  let ps = ms.pstate in
  let r = ps.reg in
  let (r,v_rs) = getreg rs r in
  let (r,v_rt) = getreg rt r in
  let r = setreg rd (aint32_add v_rs v_rt) r in
  ps |> update r |> tick 1 |> to_mstate ms

let addi rt rs imm ms  =
    let ps = ms.pstate in
    let r = ps.reg in
    let (r,v_rs) = getreg rs r in
    let r = setreg rt (aint32_add v_rs (aint32_const imm)) r in             
    ps |> update r |> tick 1 |> to_mstate ms 

        
let branch_equality equal rs rt label ms =
    let ps = tick 1 ms.pstate in    
    let r = ps.reg in
    let (r,v_rs) = getreg rs r in
    let (r,v_rt) = getreg rt r in
    let bi = ms.bbtable.(ms.cblock) in
    let (tb,fb) = aint32_test_equal v_rs v_rt in
    let (tbranch,fbranch) = if equal then (tb,fb) else (fb,tb) in
    let enq blabel ms (rsval,rtval) =
      let ps = {ps with reg = setreg rs rsval (setreg rt rtval r)} in
      enqueue_block blabel ps ms in
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
  let ps = ms.pstate in
  let r = ps.reg in
  let r = setreg rd (aint32_interval l h) r in
  ps |> update r |> to_mstate ms
  
     

(* ------------------- MAIN ANALYSIS FUNCTIONS ----------------------*)
    
(** Main function for analyzing an assembly function *)
let analyze startblock bblocks args =

  (* Get the block info of the first basic block *)  
  let bi = bblocks.(startblock) in

  (* Update the program states with abstract inputs from program arguments *)
  let ps =
    try pstate_input init_pstate args
    with Failure s -> (printf "Error: %s\n" s; exit 1) in
  
  (* Add the start block to the priority queue *)
  let pqueue = enqueue bi.dist startblock bi.joinvar ps emptyqueue in

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
  printf "Count: %d\n" !count;
  printf "BCET:  %d cycles\n" ms.pstate.bcet;
  printf "WCET:  %d cycles\n" ms.pstate.wcet;
  uprint_endline (pprint_pstate 32 ms.pstate)
    
  

















  
  

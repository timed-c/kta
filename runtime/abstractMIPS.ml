
(*
  Execute the main experiment using the following example
  >> ocamlbuild experiment.native -lib Str -- a0=[1,100]
*)


(* open Aint32congruence *)
open Aint32relint
open Aregsimple
open Amemhierarchy
open Apipeline
       
open Ustring.Op
open Printf
open Scanf
open Config
open Str

       

(* ------------------------ TYPES ------------------------------*)  

type blockid = int     (* The block ID type *)
type distance = int    (* The type for describing distances of blocks *)


                   
(** Abstract program state. Contains a concrete value
    for the program counter and abstract values for 
    registers and memory *)
type progstate = {
  reg   : aregister;
  hmem   : amemhierarchy;
  pipeline : apipeline;
  bcet  : int;
  wcet  : int;
  input_set : spliter list;
}


type branchprogstate =
  | Nobranch of progstate
  | Branch of (blockid *
                  (branchprogstate option *
                     branchprogstate option))

  | Sbranch of (registers *                   (* destination register *)
                  branchprogstate option *    (* original programstate *)
                    branchprogstate option *  (* special case when slt used in branch *)
                      branchprogstate option) (* special case when slt used in branch *)

(** Basic block ID na_ means "not applicable". *)
let  na_ = -1

(** Priority queue type *)
type pqueue = (distance * blockid * branchprogstate list) list

type specialbranch =
  (blockid *         
   registers * registers *
  (aint32 * aint32) option *
  (aint32 * aint32) option)

type mem_info =
  {
    address   : int;
    size      : int;
    sect_name : string;
    data      : int list; (*(int * int * int * int) list;*)
  }
(** The basic block info entry type is one element in the
    basic block table. This table provides all information about
    how basic blocks are related, which distances they have etc. *)  
type bblock_info =
{
  func    : mstate -> mstate;  (* The function that represents the basic block *)
  name    : string;            (* String used for debugging *)
  nextid  : blockid;           (* The identifier that shows the next basic block *)
  dist    : distance;          (* The distance to the exit (the number of edges) *)
  addr    : int;               (* Address to the first instruction in the basic block *)
  caller  : bool;              (* true if the node is a function call node *)
}

(** Main state of the analysis *)
and mstate = {
  cblock   : blockid;                  (* Current basic block *)
  pc       : int;                      (* Current program counter *)
  pstate   : branchprogstate;                (* Current program state *)
  batch    : branchprogstate list;     (* Current batch of program states *)
  bbtable  : bblock_info array;        (* Basic block info table *)
  prio     : pqueue;                   (* Overall priority queue *)
  returnid : blockid;                  (* Block id when returning from a call *)
  cstack   : (blockid * pqueue) list;  (* Call stack *)
  (*  sbranch  : specialbranch option;     (* special branch. Used between slt and beq *)*)
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

(* Used for instructions mflo and mfhi *)  
let internal_lo = R32
let internal_hi = R33
  
let reg2str r =
  match r with
  | R0 -> "zero" | R8  -> "t0" | R16 -> "s0" | R24 -> "t8"
  | R1 -> "at"   | R9  -> "t1" | R17 -> "s1" | R25 -> "t9"
  | R2 -> "v0"   | R10 -> "t2" | R18 -> "s2" | R26 -> "k0"
  | R3 -> "v1"   | R11 -> "t3" | R19 -> "s3" | R27 -> "k1"
  | R4 -> "a0"   | R12 -> "t4" | R20 -> "s4" | R28 -> "gp"
  | R5 -> "a1"   | R13 -> "t5" | R21 -> "s5" | R29 -> "sp"
  | R6 -> "a2"   | R14 -> "t6" | R22 -> "s6" | R30 -> "fp"
  | R7 -> "a3"   | R15 -> "t7" | R23 -> "s7" | R31 -> "ra"
  | R32 -> "internal_lo"       | R33 -> "internal_hi"

let reg2ustr r = reg2str r |> us
    
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
  | 32 -> R32 | 33 -> R33
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
  | "internal_lo" -> Some R32            | "internal_hi" -> Some R33
  | _ -> None
    
(** Creates an initial any state of the program state 
    ps = The program counter value *)
let init_pstate =
  {
    reg = areg_init;
    hmem = hmem_init;
    pipeline = pipeline_init;
    bcet = 0;
    wcet = 0;
    input_set = [];
    (* [AbstractR;AbstractR;AbstractR;AbstractR; *)
    (*  AbstractR;AbstractR;AbstractR;AbstractR; *)
    (*  AbstractR;AbstractR;AbstractR;AbstractR; *)
    (*  AbstractR;AbstractR;AbstractR;AbstractR; *)
    (*  AbstractR;AbstractR;AbstractR;AbstractR; *)
    (*  AbstractR;AbstractR;AbstractR;AbstractR; *)
    (*  AbstractR;AbstractR;AbstractR;AbstractR; *)
    (*  AbstractR;AbstractR;AbstractR;AbstractR; *)
    (*  ConcreteM;]; *)
  }

(** Join two program states, assuming they have the same program counter value *)
let rec join_pstates ps1 ps2 =
  match ps1, ps2 with
  | Nobranch ps1, Nobranch ps2
    | Sbranch (_,Some (Nobranch ps1),_,_), Nobranch ps2
    | Nobranch ps1, Sbranch (_,Some (Nobranch ps2),_,_)
    | Sbranch (_,Some (Nobranch ps1),_,_), Sbranch (_,Some (Nobranch ps2),_,_) ->
     Nobranch
       {
         reg   = areg_join [ps1.reg;ps2.reg];
         hmem = if ps1.wcet > ps2.wcet
                then hmem_join ps1.hmem ps2.hmem
                else hmem_join ps2.hmem ps1.hmem;
         pipeline = pipeline_join ps1.pipeline ps2.pipeline;
         bcet  = min ps1.bcet ps2.bcet;
         wcet  = max ps1.wcet ps2.wcet;
         input_set = ps1.input_set; (*TODO(Romy):????*)
       }
  | Sbranch _ , _ | _, Sbranch _ ->
     failwith (sprintf "ERROR: Should not happen - join_pstates sbranch")
  | Branch(l,_), _ | _, Branch(l,_)->
     failwith (sprintf "ERROR: Should not happen - join_pstates branch to label %d" l)

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
            (try (let aval = aint32_const_uninit (int_of_string value) in
                  pstate_input {ps with reg = (setreg regval aval ps.reg)} next_args)
             with _ -> (
               (* Is abstract interval value? *)
               (match (try bscanf (Scanning.from_string value) "[%d,%d]" (fun x y -> Some(x,y))
                         with _ -> None)
               with
               | Some(l,h) ->
                   let aval = aint32_interval_uninit l h in
                   pstate_input {ps with reg = (setreg regval aval ps.reg)} next_args
               | None -> make_error a)))
          | None -> make_error a)
    | _ -> make_error a)

(* ---------------  INPUT MEMORY HANDLING -----------------*)
                 
(** Returns a new memory containing all the memory content *)

let rec memory_init bigendian mem amem =
  let rec init_section d addr amem =
    match d with
    | [] -> amem
    | d0::d1::d2::d3::ds ->
       let data =
         match bigendian with
         (*d0|d1|d2|d3*)
         | true -> (d0 lsl 24) lor ((d1 lsl 16) lor ((d2 lsl 8) lor d3)) 
         (*d3|d2|d1|d0*)
         | false -> (d3 lsl 24) lor ((d2 lsl 16) lor ((d1 lsl 8) lor d0)) 
       in
       (* TODO(Romy): 32 or 8x4 ?*)
       let _,amem = set_memval_word addr (aint32_const data) amem in
       init_section ds (addr+4) amem
    | d0::ds ->
       let _,amem = set_memval_byte addr (aint32_const d0) amem in
       init_section ds (addr+1) amem
  in
  match mem with
  | [] -> amem
  | m::mems ->
     match m.sect_name with
     | ".bss" ->
        let mmem = hmem_init_zero BSS m.address m.size amem in
        amem |> hmem_update_mem mmem |> memory_init bigendian mems
     | ".sbss" ->
        let mmem = hmem_init_zero SBSS m.address m.size amem in
        amem |> hmem_update_mem mmem |> memory_init bigendian mems
     | _ -> init_section m.data m.address amem |>
         memory_init bigendian mems
            
(* ----------------------- DEBUG FUNCTIONS  -------------------------*)
let should_not_happen no = failwith (sprintf "ERROR: Should not happen ID = %d" no)

    
(** Pretty prints the entire program state 
    ps = program state
    noregs = number of registers to print *)
let pprint_pstate noregs ps =
  let rec pregs x s =
    if x < noregs then
      let r = int2reg x in
      let (_,sl,v) = getreg r ps.input_set ps.reg in
      let n = reg2ustr r ^. us" = " ^. (aint32_pprint false v) in      
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
      
let prn_inst_main linebreak ms str =
  if !dbg_inst then
    (printf "%10s | pc=0x%x | %s" ms.bbtable.(ms.cblock).name ms.pc (Ustring.to_utf8 str);
     if linebreak then printf "\n" else ())
  else ()
    

let prn_inst = prn_inst_main true
    
let prn_inst_no_linebreak = prn_inst_main false
    
let preg rt r =
     aint32_pprint !dbg_debug_intervals (let _,_,r = getreg rt [] r in r)
                   
let pprint_true_false_choice t f =
  let prn v =
    match v with
    | None -> us"none"
    | Some(v1,v2) -> us"(" ^. (aint32_pprint false v1) ^. us"," ^.
                     (aint32_pprint false v2) ^. us")"
  in
    us"T:" ^. prn t ^. us" F:" ^. prn f 
  
        
(* ---------------  BASIC BLOCKS AND PRIORITY QUEUE -----------------*)           
    
(* Enqueue a basic block *)  
let enqueue blockid ps ms =
  let bi = ms.bbtable.(blockid) in
  let dist = bi.dist in
  let rec work queue = 
    (match queue with
    (* Distance larger? Go to next *)
    | (d,bid,pss)::qs when d > dist ->
      (d,bid,pss)::(work qs)
    (* Same dist?  *)
    | (d,bid,pss)::qs when dist = d ->
      (* Same block id *)                          
      if bid = blockid  then(
          (* Yes, enqueue *)       
        (d,bid,ps::pss)::qs)
      else
        (* No. Go to next *)
        (d,bid,pss)::(work qs)
    (* Block not found. Enqueue *)
    | qs ->
        (dist,blockid,[ps])::qs
    )
  in
  {ms with prio = work ms.prio}
  


let max_batch_size pss =
  let rec no_sbranches pss =
    match pss with
    | [] -> true
    | ps::pss ->
       match ps with
       | Nobranch _ -> no_sbranches pss
       | Sbranch _ | Branch _ -> false
  in
  if (List.length pss > !config_max_batch_size && no_sbranches pss) then
    [List.fold_left join_pstates (List.hd pss) (List.tl pss)]
  else
    pss
  
(** Picks the block with highest priority.
    Returns the block id,
    the size of the program state list, and the program state list *)
let dequeue ms =
  (* Process the batch program states first *)
  match ms.batch with
  (* Found a batch state? *)
  | ps::pss -> {ms with pstate=ps; batch = pss}
  (* No more batch states. Find the next in the priority queue *)
  | [] -> 
    (match ms.prio with
    (* Have we finished a call? Dist = 0? *)
    | (0,blockid,ps::pss)::rest ->
       (* Is it the final node? *)
       if blockid=0 then 
         (* Join all final program states *)
         let ps' = List.fold_left join_pstates ps pss in
         {ms with cblock = 0; pstate=ps'; prio=rest} 
       else
         (* No, just pop from the call stack *)
         (match ms.cstack with
          | (retid,prio')::cstackrest ->  
             {ms with cblock=blockid; pstate=ps; batch=max_batch_size pss;
                      prio=prio'; returnid = retid; cstack=cstackrest;}
          | [] -> should_not_happen 1)
        
    (* Dequeue the top program state *)  
    | (dist,blockid,ps::pss)::rest ->
       (* Is this a calling node? *)
       let bs = ms.bbtable.(blockid) in
       if bs.caller then
         (* Yes, add to the call stack *)        
         {ms with
           cblock=blockid; pstate=ps; batch=max_batch_size pss; prio=[];
           cstack = (ms.bbtable.(blockid).nextid,rest)::ms.cstack}
       else(
         (* No, just get the last batch *)
         {ms with cblock=blockid; pstate=ps; batch=max_batch_size pss; prio=rest;})
    (* This should never happen. It should end with a terminating 
           block id zero block. *)    
    | _ ->  should_not_happen 2)
  
  
(* ------------------------ CONTINUATION  -------------------------*)

(* Continue and execute the next basic block in turn *)
let continue ms =
  (* Debug output for the mstate *)
  if !dbg && !dbg_mstate_sizes then (
    printf "-----------------\n";
    printf "blockid = %d, pc = %d, batch size = %d, prio queue size = %d\n%!"
           ms.cblock ms.pc (List.length ms.batch) (List.length ms.prio);
    printf "cstack size %d\n%!" (List.length ms.cstack))
  else ();
  let ms = dequeue ms in
  let ms = {ms with pc = ms.bbtable.(ms.cblock).addr} in
  let bi = ms.bbtable.(ms.cblock) in
  bi.func ms 

let inc_pc ms =
  {ms with pc = ms.pc+4}
    
let to_mstate ms ps =
  {ms with pstate = ps}
    
exception MaxCyclesException

let tick n ps =
  let wcet = ps.wcet + n in
  let bcet = ps.bcet + n in
  if !dbg_ticks then
    printf "WCET %d\nBCET %d\n%!" wcet bcet;
  if wcet > !config_max_cycles then raise MaxCyclesException
  else 
    {ps with bcet = bcet; wcet = wcet} 

let update r ps =
  {ps with reg = r}

let updatesl sl ps =
  {ps with input_set = sl}

let updatehmem m ps =
  {ps with hmem = m}

let updatemem r m ps =
  {ps with reg = r; hmem = m}

let updatepipl p ps =
  {ps with pipeline = p}
    
let nobranch ps =
  Nobranch ps
           
let rec proc_branches proc_ps ps =
  let proc_option_ps ps =
    match ps with
    | None -> None
    | Some ps -> Some (proc_branches proc_ps ps)
  in
  match ps with
  | Nobranch ps -> proc_ps ps 
  | Branch (l, (ps1, ps2)) -> Branch (l, (proc_option_ps ps1,
                                          proc_option_ps ps2))
  | Sbranch (rd, psold, ps1, ps2) -> Sbranch (rd,
                                              proc_option_ps psold,
                                              proc_option_ps ps1,
                                              proc_option_ps ps2)

let rec proc_slt proc_ps ps =
  let proc_option_ps ps =
    match ps with
    | None -> None
    | Some ps -> Some (proc_branches proc_ps ps)
  in
  match ps with
  | Nobranch ps -> proc_ps ps 
  | Branch (l, (ps1, ps2)) -> Branch (l, (proc_option_ps ps1,
                                          proc_option_ps ps2))
  | Sbranch (rd, psold, ps1, ps2) ->
     match psold with
     | Some ps -> proc_branches proc_ps ps
     | None -> should_not_happen 14
     

let instruction_fetch pc ps =
  let ticks,hmem = get_instruction pc ps.hmem in
  let ps = updatehmem hmem ps in
  (ticks, ps)

(* ------------------------ INSTRUCTIONS -------------------------*)

  
let r_instruction str de binop rd rs rt ms =
  (* let ticks = 1 in *)
  let proc_ps ps =
    let df,ps = instruction_fetch ms.pc ps in
    let r = ps.reg in
    let (r,sl,v_rs) = getreg rs ps.input_set r in
    let (r,sl,v_rt) = getreg rt sl r in
    let r' = setreg rd (binop v_rs v_rt) r in
    let ticks,pip = pipeline_update (ND (Some rd, None, (Some rt),(Some rs))) ps.pipeline (df,1,de,1,1) in
    if !dbg then
      prn_inst_no_linebreak ms str;
    if !dbg && !dbg_inst then
      uprint_endline
        (us" " ^. 
           (reg2ustr rd) ^. us"=" ^. (preg rd r') ^. us" " ^.
             (reg2ustr rs) ^. us"=" ^. (preg rs r) ^. us" " ^.
               (reg2ustr rt) ^. us"=" ^. (preg rt r) )
    else ();
    ps |> update r'|> updatesl sl |> updatepipl pip |> tick ticks |> nobranch
  in
  let ps = proc_branches proc_ps ms.pstate in
  ps |> to_mstate ms |> inc_pc

      
let add =
  r_instruction (us"add") 1 aint32_add

let addu =
  r_instruction (us"addu") 1 aint32_add

let sub =
  r_instruction (us"sub") 1 aint32_sub

let subu =
  r_instruction (us"subu") 1 aint32_sub

let mul =
  r_instruction (us"mul") 5 aint32_mul
            
let and_ = 
  r_instruction (us"and") 1 aint32_and

let or_ = 
  r_instruction (us"or") 1 aint32_or

let nor = 
  r_instruction (us"nor") 1 aint32_nor

let xor = 
  r_instruction (us"xor") 1 aint32_xor

let sllv =
  r_instruction (us"sllv") 1 aint32_sllv

let srlv =
  r_instruction (us"srlv") 1 aint32_srlv


(* Shift Word Right Arithmetic Variable
   Copies the sign bit.
*)
let srav =
  r_instruction (us"srav") 1 aint32_srav

                
(* TODO: handle 64-bit. Right now, we only use 32-bit multiplication. *)    
let mult rs rt ms =
  let de = 5 in
  (* let ticks = 1 in *)
  let proc_ps ps = 
    let df,ps = instruction_fetch ms.pc ps in
    let r = ps.reg in
    let (r,sl,v_rs) = getreg rs ps.input_set r in
    let (r,sl,v_rt) = getreg rt sl r in
    let (v_rhi,v_rlo) = aint64_mult v_rs v_rt in
    let r = setreg internal_lo v_rlo r in
    let r = setreg internal_hi v_rhi r in
    let ticks,pip = pipeline_update (ND (Some internal_lo, Some internal_hi, Some rs,Some rt)) ps.pipeline (df,1,de,1,1) in
    if !dbg then prn_inst ms ((us "mult ")
                              ^.
                                (reg2ustr internal_lo) ^. us"=" ^. (preg internal_lo r) ^.
                                  (reg2ustr internal_hi) ^. us"=" ^. (preg internal_hi r) ^.
                                    (reg2ustr rs) ^. us"=" ^. (preg rs r) ^.
                                      (reg2ustr rt) ^. us"=" ^. (preg rt r));
    ps |>  update r |> updatesl sl |>  updatepipl pip |> tick ticks |> nobranch
  in
  let ps =  proc_branches proc_ps ms.pstate in
  (*if !dbg then prn_inst ms (us"mult ");*)
  ps |> to_mstate ms |> inc_pc 

let multu = mult
(* Multiply ADD - no support for internal_hi *)
let madd rs rt ms = 
  let proc_ps ps =
    let df,ps = instruction_fetch ms.pc ps in
    let r = ps.reg in
    let (r,sl,v_rs) = getreg rs ps.input_set r in
    let (r,sl,v_rt) = getreg rt sl r in
    let (r,sl,v_ilo) = getreg internal_lo sl r in
    (* No support for internal_hi *)
    (*let v_ihi = getreg internal_hi r in*)
    let v_rlo = aint32_mul v_rs v_rt in
    let r = setreg internal_lo (aint32_add v_rlo v_ilo) r in
    let r = setreg internal_hi (aint32_any) r in
    let ticks,pip = pipeline_update (ND (Some internal_lo, Some internal_hi, Some rs,Some rt)) ps.pipeline (df,1,1,1,1) in
    ps |> update r |> updatesl sl |> updatepipl pip |> tick ticks |> nobranch
  in
  let ps = proc_branches proc_ps ms.pstate in
  ps |> to_mstate ms |> inc_pc

let div rs rt ms =
  let de = 16 in
  (* let ticks = 1 in *)
  let proc_ps ps =
    let df,ps = instruction_fetch ms.pc ps in
    let r = ps.reg in
    let (r,sl,v_rs) = getreg rs ps.input_set r in
    let (r,sl,v_rt) = getreg rt sl r in
    let r = setreg internal_lo (aint32_div v_rs v_rt) r in
    let r = setreg internal_hi (aint32_mod v_rs v_rt) r in
    let ticks,pip = pipeline_update (ND (Some internal_lo, Some internal_hi,Some rs,Some rt)) ps.pipeline (df,1,de,1,1) in
    if !dbg then prn_inst ms ((us "div ") ^.
                                (reg2ustr internal_lo) ^. us"=" ^. (preg internal_lo r) ^.
                                  (reg2ustr internal_hi) ^. us"=" ^. (preg internal_hi r) ^.
                                    (reg2ustr rs) ^. us"=" ^. (preg rs r) ^.
                                      (reg2ustr rt) ^. us"=" ^. (preg rt r));
    ps |>  update r |> updatesl sl |>
        updatepipl pip |> tick ticks |> nobranch
  in
  let ps = proc_branches proc_ps ms.pstate in
  (*if !dbg then prn_inst ms (us"div ");*)
  ps |> to_mstate ms |> inc_pc

(* TODO(Romy): Unsigned implementation *)
let divu rs rt ms = div rs rt ms

(* Trap if equal - Not implemented 
   Just increases the ticks *)
let teq rs rt ms =
  (* let ticks = 1 in *)
  let ps = proc_branches
             (fun ps ->
               let df,ps = instruction_fetch ms.pc ps in
               let ticks,pip = pipeline_update (ND (None, None,Some rs,Some rt)) ps.pipeline (df,1,1,1,1) in
               ps |>
                 updatepipl pip |>
                 tick ticks |> nobranch) ms.pstate in
  ps |> to_mstate ms |> inc_pc

let process_ps rd rt aint32_f de dbg_f ps ms =
  let proc_ps ps =
    let df,ps = instruction_fetch ms.pc ps in
    let ticks,pip = pipeline_update (ND (Some rd, None, Some rt,None)) ps.pipeline (df,1,de,1,1) in
    let r = ps.reg in
    let (r,sl,v_rs) = getreg rt ps.input_set r in
    let r' = (setreg rd (aint32_f v_rs) r) in
    if !dbg then
      dbg_f rd rt r r' ms;
    ps |> update r' |> updatesl sl |>
        updatepipl pip |> tick ticks |> nobranch
  in
  proc_branches proc_ps ps



let mflo rd ms =
  let dbg_f rd rt r rnew ms =
    prn_inst ms (us "mflo");
  in
  let de = 2 in (*Execution time for pipeline*)
  let ps = ms.pstate in
  let ps = process_ps rd internal_lo (fun v-> v) de dbg_f ps ms in
  ps |> to_mstate ms |> inc_pc
  
  
let mfhi rd ms =
  let dbg_f rd rt r rnew ms =
    prn_inst ms (us "mfhi");
  in
  let de = 2 in (*Execution time for pipeline*)
  let ps = ms.pstate in
  let ps = process_ps rd internal_hi (fun v-> v) de dbg_f ps ms in
  ps |> to_mstate ms |> inc_pc

let mtlo rs ms =
  let dbg_f rd rt r rnew ms =
    prn_inst ms (us "mtlo");
  in
  let de = 1 in
  let ps = ms.pstate in
  let ps = process_ps internal_lo rs (fun v-> v) de dbg_f ps ms in
  ps |> to_mstate ms |> inc_pc
  
  
let mthi rs ms =
  let dbg_f rs rt r rnew ms =
    prn_inst ms (us "mthi");
  in
  let de = 1 in
  let ps = ms.pstate in
  let ps = process_ps internal_hi rs (fun v-> v) de dbg_f ps ms in
  ps |> to_mstate ms |> inc_pc

                  

let dbg_imm_f instr imm imm_str rd rt r rnew ms =
  prn_inst ms (us instr ^. us" " ^. 
                 (reg2ustr rd) ^. us"=" ^. (preg rd rnew) ^. us" " ^.
                   (reg2ustr rt) ^. us"=" ^. (preg rt r) ^. us" " ^.
                     us imm_str ^. us(sprintf "=%d" imm))

           
let addi rt rs imm ms =
  (* Execution time *)
  let de = 1 in
  let ps = ms.pstate in
  let imval = aint32_const imm in
  let dbg_f = dbg_imm_f "addi" imm "imm" in
  let ps = process_ps rt rs (fun v -> aint32_add v imval) de dbg_f ps ms in
  ps |> to_mstate ms |> inc_pc

      
let addiu = addi

let andi rt rs imm ms  =
  let de = 1 in
  let ps = ms.pstate in
  let imval = aint32_const imm in
  let dbg_f = dbg_imm_f "andi" imm "imm" in
  let ps = process_ps rt rs (fun v -> aint32_and v imval) de dbg_f ps ms in
  ps |> to_mstate ms |> inc_pc

let ori rt rs imm ms  =
  let de = 1 in
  let ps = ms.pstate in
  let imval = aint32_const imm in
  let dbg_f = dbg_imm_f "ori" imm "imm" in
  let ps = process_ps rt rs (fun v -> aint32_or v imval) de dbg_f ps ms in
  ps |> to_mstate ms |> inc_pc

let xori rt rs imm ms  =
  let de = 1 in
  let ps = ms.pstate in
  let imval = aint32_const imm in
  let dbg_f = dbg_imm_f "xori" imm "imm" in
  let ps = process_ps rt rs (fun v -> aint32_xor v imval) de dbg_f ps ms in
  ps |> to_mstate ms |> inc_pc


let sll rd rt shamt ms =
  let de = 1 in
  let ps = ms.pstate in
  let mval = aint32_const (1 lsl shamt) in
  let dbg_f = dbg_imm_f "sll" shamt "shamt" in
  let ps = process_ps rd rt (fun v -> aint32_mul v mval) de dbg_f ps ms in
  ps |> to_mstate ms |> inc_pc

let srl rd rt shamt ms =
  let de = 1 in
  let ps = ms.pstate in
  let mval = aint32_const (1 lsl shamt) in
  let dbg_f = dbg_imm_f "srl" shamt "shamt" in
  let ps = process_ps rd rt (fun v -> aint32_div v mval) de dbg_f ps ms in
  ps |> to_mstate ms |> inc_pc

      
let sra rd rt shamt ms =
  let de = 1 in
  let ps = ms.pstate in
  let mval = aint32_const (1 lsl shamt) in
  let dbg_f = dbg_imm_f "sra" shamt "shamt" in
  let ps = process_ps rd rt (fun v -> aint32_div v mval) de dbg_f ps ms in 
  ps |> to_mstate ms |> inc_pc

(* EXT - not implemented. *)
let ext rt rs pos size ms =
  let proc_ps ps =
    let df,ps = instruction_fetch ms.pc ps in
    let ticks,pip = pipeline_update (ND (None,None,Some rs,Some rt)) ps.pipeline (df,1,1,1,1) in
    {ps with reg = setreg rt aint32_any ps.reg}
    |> updatepipl pip
    |> tick ticks
    |> nobranch
  in
  let ps = proc_branches proc_ps ms.pstate in
  ps |> to_mstate ms |> inc_pc

(* INS - not implemented. *)
let ins rs rt pos size ms =
  let proc_ps ps =
    let df,ps = instruction_fetch ms.pc ps in
    let ticks,pip = pipeline_update (ND (None,None,Some rs,Some rt)) ps.pipeline (df,1,1,1,1) in
    {ps with reg = setreg rt aint32_any ps.reg}
    |> updatepipl pip
    |> tick ticks
    |> nobranch
  in
  let ps = proc_branches proc_ps ms.pstate in
  ps |> to_mstate ms |> inc_pc

(* INS - not implemented. *)
let clz rd rs ms =
  let proc_ps ps =
    let df,ps = instruction_fetch ms.pc ps in
    let r = ps.reg in
    let (r,sl,v_rs) = getreg rs ps.input_set r in
    let r = setreg rd (aint32_clz v_rs) r in
    let ticks,pip = pipeline_update (ND (Some rd, None, Some rs,None)) ps.pipeline (df,1,1,1,1) in
    if !dbg then 
      prn_inst ms (us "clz" ^. us" " ^. 
                 (reg2ustr rd) ^. us"=" ^. (preg rd r) ^. us" " ^.
                   (reg2ustr rs) ^. us"=" ^. (preg rs r) ^. us" ");

    {ps with reg = r} |> updatesl sl |>
        updatepipl pip |> tick ticks |> nobranch
  in
  proc_branches proc_ps ms.pstate |> to_mstate ms |> inc_pc

(* MOVZ - not implemented. *)                                               
let movz rd rs rt ms =
  let proc_ps ps =
    let df,ps = instruction_fetch ms.pc ps in
    let ticks,pip = pipeline_update (ND (Some rd, None, Some rs,Some rt)) ps.pipeline (df,1,1,1,1) in
    {ps with reg = setreg rd aint32_any ps.reg}
    |> updatepipl pip
    |> tick ticks
    |> nobranch
  in
  proc_branches proc_ps ms.pstate |> to_mstate ms |> inc_pc

(* MOVZ - not implemented. *) 
let movn rd rs rt ms =
  let proc_ps ps =
    let df,ps = instruction_fetch ms.pc ps in
    let ticks,pip = pipeline_update (ND (Some rd, None, Some rs,Some rt)) ps.pipeline (df,1,1,1,1) in
    {ps with reg = setreg rd aint32_any ps.reg}
    |> updatepipl pip
    |> tick ticks
    |> nobranch
  in
  proc_branches proc_ps ms.pstate |> to_mstate ms |> inc_pc
                                                       
(* LWL - not implemented. *) 
let lwl rt imm rs ms =
  let de = 2 in
  let proc_ps ps =
    let df,ps = instruction_fetch ms.pc ps in
    let dm,m,v = ld_any ps.hmem in
    let r = setreg rt v ps.reg in
    let ticks,pip = pipeline_update (Mem (Some rt, Some rs, None)) ps.pipeline (df,1,de,dm,1) in
    ps |> updatemem r m 
    |> updatepipl pip
    |> tick ticks
    |> nobranch
  in
  proc_branches proc_ps ms.pstate |> to_mstate ms |> inc_pc

(* LWL - not implemented. *) 
let lwr rt imm rs ms =
  (* let ticks = 1 in *)
  let de = 2 in
  let proc_ps ps =
    let df,ps = instruction_fetch ms.pc ps in
    let dm,m,v = ld_any ps.hmem in
    let ticks,pip = pipeline_update (Mem (Some rt, Some rs, None)) ps.pipeline (df,1,de,dm,1) in
    let r = setreg rt v ps.reg in
    ps |> updatemem r m 
    |> updatepipl pip 
    |> tick ticks
    |> nobranch
  in
  proc_branches proc_ps ms.pstate |> to_mstate ms |> inc_pc

(* used by next and branch_equality *)
let enq tlabel flabel pst psf ms =
  (*TODO(Romy):to_mstate: maybe not necessary*)
  match pst, psf with
  | None, Some(psf) ->
     continue (psf |> to_mstate ms |> enqueue flabel psf)
  | Some(pst), None ->
     continue (pst |> to_mstate ms |> enqueue tlabel pst)
  | Some(pst), Some(psf) ->
     let ms = pst |> to_mstate ms |> enqueue tlabel pst in
     let ms = psf |> to_mstate ms |> enqueue flabel psf in
     continue ms
  | None, None ->
     should_not_happen 4

      
let branch_main str equal dslot op rs rt label ms =
  let proc_ps ps =
    let df,ps = instruction_fetch ms.pc ps in
    (* assume delay slot *)
    let ticks,pip = pipeline_update (Br (Some rs, Some rt)) ps.pipeline (df,1,1,1,1) in
    ps |> updatepipl pip |> tick ticks |> nobranch
  in
  let proc_ps_option = function | None -> None
                                | Some ps -> Some
                                               (proc_branches proc_ps ps)
  in
  match ms.pstate with
  | Sbranch (sbrd, psold, pst, psf) when sbrd = rs && rt = zero -> (
    (* Special branch handling when beq or bne is checking with $0 and  
       there is another instruction such as slt that has written 
       information in ms.sbranch *)      
    let bi = ms.bbtable.(ms.cblock) in
    let pst, psf = if equal then (psf, pst) else (pst, psf) in
                            
    let pst = proc_ps_option pst in
    let psf = proc_ps_option psf in
    if !dbg then
      (
       (match pst with
        | None -> prn_inst ms (str ^. us" None");
        | Some (Nobranch ps) ->
           let r = ps.reg in
           prn_inst ms (str ^. us" T: " ^. (reg2ustr rs)
                        ^. us"=" ^. (preg rs r) ^. us" " ^.
                          (reg2ustr rt) ^. us"=" ^. (preg rt r) ^. us" " ^.
                            us(ms.bbtable.(label).name) ^. us" "
                            ^. us(ms.bbtable.(bi.nextid).name) ^.
                                us" " ^. us" (sbranch)")
        | _  -> (*TODO(Romy): Replace with error *)
           printf "Error: should not happen branch_main\n%!";);
        (match psf with
        | None -> prn_inst ms (str ^. us" None");
        | Some (Nobranch ps) ->
           let r = ps.reg in
           prn_inst ms (str ^. us " F: " ^.
                          (reg2ustr rs) ^. us"=" ^. (preg rs r) ^. us" " ^.
                            (reg2ustr rt) ^. us"=" ^. (preg rt r) ^. us" " ^.
                              us(ms.bbtable.(label).name) ^. us" " ^. us(ms.bbtable.(bi.nextid).name) ^.
                                us" " ^. us" (sbranch)")
        | _  -> (*TODO(Romy): Replace with error *)
           printf "Error: should not happen branch_main\n%!";);

      );   
    if dslot then
      {ms with pstate = Branch (label, (pst, psf))}  |> inc_pc
    else
      enq label bi.nextid pst psf (ms |> inc_pc)
  )
  | Nobranch ps | Sbranch (_, Some (Nobranch ps), _, _) -> (
    (* Ordinary branch equality check *)
    (*let ps = tick 1 ps in    *)
    let r = ps.reg in
    let (r,sl,v_rs) = getreg rs ps.input_set r in
    let (r,sl,v_rt) = getreg rt sl r in
    let ps = update r ps |> updatesl sl in
    let ms = Nobranch ps |> to_mstate ms in
    let bi = ms.bbtable.(ms.cblock) in
    let (tb,fb) = op v_rs v_rt in
    let (tbranch,fbranch) = if equal then (tb,fb) else (fb,tb) in
    let update_pstate ps rs rt branch =
      match branch with
      | Some(sval,tval) -> Some (Nobranch
                                   { ps with reg = setreg rs sval
                                                          (setreg rt tval ps.reg)})
      | None -> None
    in
    let pst,psf = update_pstate ps rs rt tbranch,
                  update_pstate ps rs rt fbranch in
    let pst = proc_ps_option pst in
    let psf = proc_ps_option psf in

    if !dbg then (
        let r = ps.reg in
        prn_inst ms (str ^. us " " ^.
        (reg2ustr rs) ^. us"=" ^. (preg rs r) ^. us" " ^.
        (reg2ustr rt) ^. us"=" ^. (preg rt r) ^. us" " ^.
        us(ms.bbtable.(label).name) ^. us" " ^. us(ms.bbtable.(bi.nextid).name) ^.
          us" " ^. pprint_true_false_choice tbranch fbranch ^. us" (sbranch)"));
    if dslot then
      {ms with pstate = Branch (label, (pst, psf))} |> inc_pc
    else
      enq label bi.nextid pst psf (ms |> inc_pc)
  )
  | Branch _ | Sbranch _ -> failwith "Nested Branch."

(* Instruction: beq
   From official MIPS32 manual: 
   "Branch on Equal
   To compare GPRs then do a PC-relative conditional branch." *)
let beq =
  branch_main (us"beq")
                   true false aint32_test_equal

(* Same as above, but with branch delay slots enabled *)    
let beqds =
  branch_main (us"beqds")
                   true true aint32_test_equal

(* Instruction: bne
   From official MIPS32 manual: 
   "Branch on Not Equal
   To compare GPRs then do a PC-relative conditional branch" *)
let bne =
  branch_main (us"bne")
                   false false aint32_test_equal

    
(* Same as above, but with branch delay slots enabled *)    
let bneds =
  branch_main (us"bneds")
                   false true aint32_test_equal

(* Instruction: beql
   From official MIPS32 manual: 
   "Branch on Equal Likely. 
   To compare GPRs then do a PC-relative conditional branch; 
   execute the delay slot only if the branch is taken. " 
   NOTE: the generated code need to insert a "likely" node
   to make this correct. *)
let beqlds =
  branch_main (us"beqlds")
                   true true aint32_test_equal

    
(* Instruction: bnel
   From official MIPS32 manual: 
   "Branch on Not Equal Likely. 
   To compare GPRs then do a PC-relative conditional branch; 
   execute the delay slot only if the branch is taken." 
   NOTE: the generated code need to insert a "likely" node
   to make this correct. *)
let bnelds =
  branch_main (us"bnelds")
                   false true aint32_test_equal

    
(* Instruction: blez
   From official MIPS32 manual: 
   "Branch on Less Than or Equal to Zero
   To test a GPR then do a PC-relative conditional branch." *)
let blez rs label ms =
  branch_main (us"blez")
                   true false aint32_test_less_than_equal rs zero label ms

    
(* Same as above, but with branch delay slots enabled *)    
let blezds rs label ms =
  branch_main (us"blezds")
                   true true aint32_test_less_than_equal rs zero label ms


(* Instruction: blezl *)
let blezlds rs label ms =
  branch_main (us"blezlds")
                   true true aint32_test_less_than_equal rs zero label ms


                   
(* Instruction: bltz
   From official MIPS32 manual: 
   "Branch on Less Than Zero
   To test a GPR then do a PC-relative conditional branch." *)

let bltz rs label ms =
  branch_main (us"bltz")
                   true false aint32_test_less_than rs zero label ms

    
(* Same as above, but with branch delay slots enabled *)    
let bltzds rs label ms =
  branch_main (us"bltzds")
                   true true aint32_test_less_than rs zero label ms

(* Instruction: bltzl *)
let bltzlds rs label ms =
  branch_main (us"bltzlds")
                   true true aint32_test_less_than rs zero label ms

(* Instruction: bgez
   From official MIPS32 manual: 
   "Branch on Greater Than or Equal to Zero
   To test a GPR then do a PC-relative conditional branch." *)
let bgez rs label ms =
  branch_main (us"bgez")
                   true false aint32_test_greater_than_equal rs zero label ms

    
(* Same as above, but with branch delay slots enabled *)    
let bgezds rs label ms =
  branch_main (us"bgezds")
                   true true aint32_test_greater_than_equal rs zero label ms

(* Instruction: bgezl *)
let bgezlds rs label ms =
  branch_main (us"bgezlds")
                   true true aint32_test_greater_than_equal rs zero label ms

(* Instruction: bgtz
   From official MIPS32 manual: 
   "Branch on Greater Than to Zero
   To test a GPR then do a PC-relative conditional branch." *)
let bgtz rs label ms =
  branch_main (us"bgtz")
                   true false aint32_test_greater_than rs zero label ms
    
(* Same as above, but with branch delay slots enabled *)    
let bgtzds rs label ms =
  branch_main (us"bgtzds")
                   true true aint32_test_greater_than rs zero label ms

(* Instruction: bgtzl *)
let bgtzlds rs label ms =
  branch_main (us"bgtzlds")
                   true true aint32_test_greater_than rs zero label ms


let lui rt imm ms =
  let ps = ms.pstate in
  let immi = (aint32_const (imm lsl 16)) in
  let proc_ps ps =
    let df,ps = instruction_fetch ms.pc ps in
    let ticks,pip = pipeline_update (ND (Some rt, None,None,None)) ps.pipeline (df,1,1,1,1) in
    let r = ps.reg in
    let r = setreg rt immi r in
    if !dbg then prn_inst ms
                          (us"lui " ^.
                             (reg2ustr rt) ^.
                               us"=" ^. (preg rt r));
    ps |> update r |> updatepipl pip |> tick ticks |> nobranch
  in
  let ps = proc_branches proc_ps ps in
  ps |> to_mstate ms |> inc_pc

  
      
(* Count cycles for the return. The actual jump is performed
   by the pseudo instruction 'ret'. The reason is that
   all functions should only have one final basic block node *)    
let jr rs ms =
  if !dbg then prn_inst ms (us"jr " ^. (reg2ustr rs));
  if rs = ra then (ms |> inc_pc)
(*    let nextid = ms.bbtable.(ms.cblock).nextid in
      continue (enqueue nextid (tick 2 ms.pstate) ms) *)
  else failwith "Not yet implemented."


let jrds rs ms =
  let proc_ps ps = 
    let df,ps = instruction_fetch ms.pc ps in
    let ticks,pip = pipeline_update (Br (Some rs, None)) ps.pipeline (df,1,1,1,1) in
    ps |> updatepipl pip |> tick ticks |> nobranch
  in
  if !dbg then prn_inst ms (us"jrds " ^. (reg2ustr rs));
  if rs = ra then
    let ps = proc_branches proc_ps ms.pstate in
    ps |> to_mstate ms |> inc_pc
  (*    let nextid = ms.bbtable.(ms.cblock).nextid in
      continue (enqueue nextid (tick 2 ms.pstate) ms) *)
  else failwith "Not yet implemented."

let jal label ms =
  if !dbg then prn_inst ms (us"jal " ^. us(ms.bbtable.(label).name));
  let proc_ps ps = 
    let df,ps = instruction_fetch ms.pc ps in
    let ticks,pip = pipeline_update (Br (None, None)) ps.pipeline (df,1,1,1,1) in
    ps |> updatepipl pip |> tick ticks |> nobranch
  in
  let ps = proc_branches proc_ps ms.pstate in
  continue (enqueue label ps (ps |> to_mstate ms |> inc_pc))
           
let jalds label ms =
  if !dbg then prn_inst ms (us"jalds " ^. us(ms.bbtable.(label).name));
  let proc_ps ps = 
    let df,ps = instruction_fetch ms.pc ps in
    let ticks,pip = pipeline_update (Br (None, None)) ps.pipeline (df,1,1,1,1) in
    ps |> updatepipl pip |> tick ticks 
  in
  let branch ps =
    Branch (label, (Some (Nobranch ps), None))
  in
  match ms.pstate with
  | Nobranch ps ->
     let ps = proc_branches (fun ps -> proc_ps ps |> branch) (Nobranch ps) in
     ps |> to_mstate ms |> inc_pc
  | Sbranch(_,_,Some ps1,_) | Sbranch(_,_,_,Some ps1) -> (*??*)
     proc_branches
       (fun ps -> proc_ps ps |> branch)
       ps1 |> to_mstate ms |> inc_pc
  | Sbranch _ | Branch _ ->
     should_not_happen 5
                        
                       
let jds label ms =
  let proc_ps ps = 
    let df,ps = instruction_fetch ms.pc ps in
    let ticks,pip = pipeline_update (Br (None, None)) ps.pipeline (df,1,1,1,1) in
    ps |> updatepipl pip |> tick ticks 
  in
  let branch ps =
    Branch (label, (Some (Nobranch ps), None))
  in
  if !dbg then prn_inst ms (us"jds " ^. us(ms.bbtable.(label).name));
  match ms.pstate with
  | Nobranch ps ->
     let ps = proc_branches
                (fun ps -> proc_ps ps |> branch)
                (Nobranch ps) in
     ps |> to_mstate ms |> inc_pc
  | Sbranch(_,_,Some ps1,_) | Sbranch(_,_,_,Some ps1) ->
     proc_branches
       (fun ps -> proc_ps ps |> branch)
       ps1 |> to_mstate ms |> inc_pc
  | Sbranch _ | Branch _ -> 
     should_not_happen 6
  
let sw rt imm rs ms =
  let de = 2 in
  (* let ticks = 1 in *)
  let proc_ps ps = 
    let df,ps = instruction_fetch ms.pc ps in
    let r = ps.reg in
    let (r,sl,v_rt) = getreg rt ps.input_set r in
    let (r,sl,v_rs) = getreg rs sl r in
    let con_v_rs = aint32_to_int32 v_rs in
    let dm,m =
      match con_v_rs with
      (* updated the whole memory to Any - empty mem_init *)
      | None -> st_any ps.hmem
      | Some (con_v_rs) ->
         set_memval_word (imm + con_v_rs) v_rt ps.hmem
    in
    let ticks,pip = pipeline_update (ND (None, None, Some rs, Some rt)) ps.pipeline (df,1,de,dm,1) in
    if !dbg then(
      prn_inst ms (us"sw " ^. 
                     (reg2ustr rt) ^. us"=" ^. (preg rt r) ^.
                       us(sprintf " imm=%d(" imm) ^.
                         (reg2ustr rs) ^. us"=" ^. (preg rs r) ^. us")")) else ();
    ps |> updatemem r m |> updatesl sl |>
        updatepipl pip |> tick ticks |> nobranch
  in
  let ps = proc_branches proc_ps ms.pstate in
  ps |> to_mstate ms |> inc_pc


let sb rt imm rs ms =
  let de = 2 in
  let proc_ps ps =
    let df,ps = instruction_fetch ms.pc ps in
    let r = ps.reg in
    let (r,sl,v_rt) = getreg rt ps.input_set r in
    let (r,sl,v_rs) = getreg rs sl r in
    let con_v_rs = aint32_to_int32 v_rs in
    let dm,m =
      match con_v_rs with
        (* updated the whole memory to Any - overapproximation *)
      | None -> st_any ps.hmem
      | Some (con_v_rs) ->
         set_memval_byte (imm + con_v_rs) v_rt ps.hmem
    in
    let ticks,pip = pipeline_update  (ND (None, None, Some rs, Some rt)) ps.pipeline (df,1,de,dm,1) in
    if !dbg then(
      prn_inst ms (us"sb " ^. 
                     (reg2ustr rt) ^. us"=" ^. (preg rt r) ^.
                       us(sprintf " imm=%d(" imm) ^.
                         (reg2ustr rs) ^. us"=" ^. (preg rs r) ^. us")")) else ();
    ps |> updatemem r m |> updatesl sl |>
        updatepipl pip |> tick ticks |> nobranch
  in
  let ps = proc_branches proc_ps ms.pstate in
  ps |> to_mstate ms |> inc_pc 

let sh rt imm rs ms =
  (* let ticks = 1 in *)
  let de = 2 in
  let proc_ps ps = 
    let df,ps = instruction_fetch ms.pc ps in
    let r = ps.reg in
    let (r,sl,v_rt) = getreg rt ps.input_set r in
    let (r,sl,v_rs) = getreg rs sl r in
    let con_v_rs = aint32_to_int32 v_rs in
    let dm,m =
      match con_v_rs with
        (* updated the whole memory to Any - overapproximation *)
      | None -> st_any ps.hmem
      | Some (con_v_rs) ->
         set_memval_hword (imm + con_v_rs) v_rt ps.hmem
    in
    let ticks,pip = pipeline_update (ND (None, None, Some rs, Some rt)) ps.pipeline (df,1,de,dm,1) in
    if !dbg then(
      prn_inst ms (us"sh " ^. 
                     (reg2ustr rt) ^. us"=" ^. (preg rt r) ^.
                       us(sprintf " imm=%d(" imm) ^.
                         (reg2ustr rs) ^. us"=" ^. (preg rs r) ^. us")")) else ();
    ps |> updatemem r m |> updatesl sl |>
        updatepipl pip |> tick ticks |> nobranch
  in
  let ps = proc_branches proc_ps ms.pstate in
  ps |> to_mstate ms |> inc_pc

                                             
let lw rt imm rs ms =
  (* let ticks = 1 in *)
  let de = 2 in
  let ps = ms.pstate in
  let proc_ps ps =
    let df,ps = instruction_fetch ms.pc ps in
    let r = ps.reg in
    let (r,sl,v_rt) = getreg rt ps.input_set r in
    let (r,sl,v_rs) = getreg rs sl r in
    let con_v_rs = aint32_to_int32 v_rs in
    let (dm,sl,m,v) =
      match con_v_rs with
      | None ->
         let dm,m,v = ld_any ps.hmem in
         (dm,sl,m,v)
      | Some (con_v_rs) ->
         get_memval_word (imm + con_v_rs) sl ps.hmem
    in
    let r' = setreg rt v r in
    let ticks,pip = pipeline_update (Mem (Some rt, Some rs, None)) ps.pipeline (df,1,de,dm,1) in
    if !dbg then
      (prn_inst ms (us"lw " ^. 
                      (reg2ustr rt) ^. us"=" ^. (preg rt r') ^.
                        us(sprintf " imm=%d(" imm) ^.
                          (reg2ustr rs) ^. us"=" ^. (preg rs r) ^. us")")) else ();
    ps |> updatemem r' m |> updatesl sl |>
        updatepipl pip |> tick ticks |> nobranch
  in
  let ps = proc_branches proc_ps ps in
  ps |> to_mstate ms |> inc_pc

let lb rt imm rs ms =
  let de = 2 in
  (* let ticks = 1 in *)
  let proc_ps ps =
    let df,ps = instruction_fetch ms.pc ps in
    let r = ps.reg in
    let (r,sl,v_rt) = getreg rt ps.input_set r in
    let (r,sl,v_rs) = getreg rs sl r in
    let con_v_rs = aint32_to_int32 v_rs in
    let (dm,sl,m,v) =
      match con_v_rs with
      | None ->
         let dm,m,v = ld_any ps.hmem in
         (dm,sl,m,v)
      | Some (con_v_rs) ->
         get_memval_byte (imm + con_v_rs) sl ps.hmem
    in
    let r' = setreg rt v r in
    let ticks,pip = pipeline_update (Mem (Some rt, Some rs, None))  ps.pipeline (df,1,de,dm,1) in
    if !dbg then(
      prn_inst ms (us"lb " ^. 
                     (reg2ustr rt) ^. us"=" ^. (preg rt r') ^.
                       us(sprintf " imm=%d(" imm) ^.
                         (reg2ustr rs) ^. us"=" ^. (preg rs r) ^. us")")) else ();
    ps |> updatemem r' m |> updatesl sl |>
        updatepipl pip |> tick ticks |> nobranch
  in
  let ps = proc_branches proc_ps ms.pstate in
  ps |> to_mstate ms |> inc_pc
                                              
(*TODO(Romy): Not implemented*)
let lbu rt imm rs ms = lb rt imm rs ms


let lh rt imm rs ms = 
  let de = 2 in
  let proc_ps ps =
    let df,ps = instruction_fetch ms.pc ps in
    let r = ps.reg in
    let (r,sl,v_rt) = getreg rt ps.input_set r in
    let (r,sl,v_rs) = getreg rs sl r in
    let con_v_rs = aint32_to_int32 v_rs in
    let (dm,sl,m,v) =
      match con_v_rs with
      | None -> 
         let dm,m,v = ld_any ps.hmem in
         (dm,sl,m,v)
      | Some (con_v_rs) ->
         get_memval_hword (imm + con_v_rs) sl ps.hmem
    in
    let r' = setreg rt v r in
    let ticks,pip = pipeline_update (Mem (Some rt, Some rs, None)) ps.pipeline (df,1,de,dm,1) in
    if !dbg then(
      prn_inst ms (us"lh " ^. 
                     (reg2ustr rt) ^. us"=" ^. (preg rt r') ^.
                       us(sprintf " imm=%d(" imm) ^.
                         (reg2ustr rs) ^. us"=" ^. (preg rs r) ^. us")"))
    else ();
    ps |> updatemem r' m |> updatesl sl |>
         updatepipl pip |> tick ticks |> nobranch
  in
  let ps = proc_branches proc_ps ms.pstate in
  ps |> to_mstate ms |> inc_pc
                                              
(*TODO(Romy): Not implemented*)
let lhu rt imm rs ms = lh rt imm rs ms

let slt_main signed r v_rs v_rt rd rs rt sl ps ms =
  let proc_ps st rd_v ps =
    let df,ps = instruction_fetch ms.pc ps in
    let ticks,pip = pipeline_update (ND (Some rd, None, Some rs, Some rt))
                                    ps.pipeline (df,1,1,1,1) in
    match st with
    | Some(sval,tval) -> Some
                           (Nobranch
                              ({ps with reg =
                                          setreg rd rd_v
                                                 (setreg rs sval
                                                         (setreg rt tval r))}
                               |> updatesl sl |> updatepipl pip
                               |> tick ticks))
    | None -> None
  in
  let proc_ps_old rd_v ps =
    let df,ps = instruction_fetch ms.pc ps in
    let ticks,pip = pipeline_update (ND (Some rd, None, Some rs, Some rt))
                                    ps.pipeline (df,1,1,1,1) in
    Some (Nobranch ({ps with reg = setreg rd rd_v r}
                    |> updatesl sl |> updatepipl pip
                    |> tick ticks))
  in
  let (t,f) = (if signed then aint32_test_less_than else
               aint32_test_less_than_unsigned) v_rs v_rt in
  let rd_v = match t,f with
    | _,None -> aint32_const 1
    | None,_ -> aint32_const 0
    | _,_ -> aint32_interval 0 1
  in
  let r' = setreg rd rd_v r in
  let ps = Sbranch(rd,
                   proc_ps_old rd_v ps,
                   proc_ps t (aint32_const 1) ps,
                   proc_ps f (aint32_const 0) ps)
  in  
  if !dbg then
    prn_inst ms ((if signed then us"slt " else us"sltu ") ^.
                   (reg2ustr rd) ^. us"=" ^. (preg rd r') ^. us" " ^.
                     (reg2ustr rs) ^. us"=" ^. (preg rs r) ^. us" " ^.
                       (reg2ustr rt) ^. us"=" ^. (preg rt r) ^.
                         us" " ^. pprint_true_false_choice t f);
   
  ps



let slt rd rs rt ms =
  let ps = ms.pstate in
  let proc_ps ps =
    let (r,sl,v_rs) = getreg rs ps.input_set ps.reg in
    let (r,sl,v_rt) = getreg rt sl r in
    slt_main true r v_rs v_rt rd rs rt sl ps ms 
  in    
  proc_slt proc_ps ps |> to_mstate ms |> inc_pc
    
let sltu rd rs rt ms = 
  let ps = ms.pstate in
  let proc_ps ps =
    let (r,sl,v_rs) = getreg rs ps.input_set ps.reg in
    let (r,sl,v_rt) = getreg rt sl r in
    slt_main false r v_rs v_rt rd rs rt sl ps ms 
  in    
  proc_slt proc_ps ps |> to_mstate ms |> inc_pc

let slti rd rs imm ms =
  let ps = ms.pstate in
  let proc_ps ps =
    let (r,sl,v_rs) = getreg rs ps.input_set ps.reg in
    slt_main true r v_rs (aint32_const imm) rd rs zero sl ps ms
  in
  proc_slt proc_ps ps |> to_mstate ms |> inc_pc
                                      
(* TODO: right now, stliu is just a copy of slti. It must 
   be update to also include correct handling of unsigned integers! *)    
let sltiu rd rs imm ms =
  let ps = ms.pstate in
  let proc_ps ps =
    let (r,sl,v_rs) = getreg rs ps.input_set ps.reg in
    slt_main true r v_rs (aint32_const imm) rd rs zero sl ps ms
  in
  proc_slt proc_ps ps |> to_mstate ms |> inc_pc
  

      
(* -------------------- PSEUDO INSTRUCTIONS -------------------------*)
      
        
(* Go to next basic block. Special handling if branch delay slot. *)
let next ms =
  (* Get the block info for the current basic block *)
  let bi = ms.bbtable.(ms.cblock) in
  (match ms.pstate with
   | Nobranch ps ->
      if !dbg then prn_inst ms (us(sprintf "next id=%d" bi.nextid));
      (* Ordinary branch equality check *)
      (* Enqueue the current program state with the next basic block *)
      let ms' = enqueue bi.nextid ms.pstate ms in
      (* Continue and process next block *)
      continue ms'
   | Branch (label,(pst,psf)) ->
      enq label bi.nextid pst psf ms
   | Sbranch sb ->
      let ms' = enqueue bi.nextid (Sbranch sb) ms in
      continue ms'
  )  
    

(* Return from a function. Pseudo-instruction that does not take time *)    
let ret ms =
  if !dbg then prn_inst ms (us(sprintf "ret returnid=%d" ms.returnid));
  continue (enqueue ms.returnid ms.pstate ms)

(* load immediate interval *)
let lii rd l h ms =
  if !dbg then prn_inst ms (us"lii");
  let proc_ps ps =
    let r = ps.reg in
    let r = setreg rd (aint32_interval l h) r in
    ps |> update r |> nobranch
  in
  let ps = proc_branches proc_ps ms.pstate in
  ps |> to_mstate ms
  

(* ------------------- MAIN ANALYSIS FUNCTIONS ----------------------*)


      
(** Main function for analyzing an assembly function *)
let analyze_main startblock bblocks gp_addr args init_mem task_amem n bound inputlist =
  (* Get the block info of the first basic block *)  
  let bi = bblocks.(startblock) in

  (* Update the program states with abstract inputs from program arguments *)
  set_max_cycles bound;    
  let ps = 
    try pstate_input init_pstate args
    with Failure s -> (printf "Error: %s\n" s; exit 1) in 
  let ps = updatesl inputlist ps in

  (* Initiate the stack pointer *)
  let stack_addr = 0x80000000 - 8 - (n*0x01000000) in
  let frame_ptr = 0x04000000 - 8 - (n*0x01000000) in
  let reg = setreg sp (aint32_const stack_addr) ps.reg in
  let reg = setreg gp (aint32_const gp_addr) reg in
  let reg = setreg fp (aint32_const frame_ptr) reg in
  (* let hmem = disable_dcache ps.hmem in *)
  let nocache = !nocache in
  set_nocache true;
  let hmem = memory_init false init_mem ps.hmem in
  set_nocache nocache;
  (* let hmem = enable_dcache hmem in *)

  (* TODO(Romy): Overhead *)
  let hmem,ps =
    if not !record_mtags then
      let tmap,oh =
        match task_amem with
        | [] | _::[] -> None,0
        | t::ts -> get_tmaps t ts
      in
      hmem |> hmem_update_amap tmap, ps |> tick oh
    else
      hmem,ps
  in
  
  let ps = {ps with reg = reg; hmem=hmem} in
  
  (* Create the main state *)
  let mstate = {
    cblock = startblock;   (* N/A, since the process has not yet started *)    
    pc = bi.addr;
    pstate = Nobranch ps;          (* New program state *)
    batch = [];
    bbtable = bblocks;    (* Stores a reference to the basic block info table *)
    prio = [];            (* Starts with an empty priority queue *)
    returnid = 0;         (* Should finish by returning to the final block *)
    cstack = [(0,[])];    (* The final state *)
  } in
  
  let mstate = enqueue startblock (Nobranch ps) mstate in

  (* Continue the process and execute the next basic block from the queue *)
  continue mstate 

(* Input:
   integer bound, input list
   Output: execution time (as an integer). *)

let mywcetfunc 
  st bbl gp_addr args mem tasks n bound inputlist =
    let get_wcet ms =
      let ps = ms.pstate in
      match ps with
      | Nobranch ps -> ps.wcet
      | _ -> should_not_happen 12
    in
    set_max_cycles bound;    
    try
      analyze_main st bbl gp_addr args mem tasks n bound inputlist |> get_wcet
    with
    | MaxCyclesException -> bound


    
(* -------------- BORDER ------------ *)

    
module IntMap = Map.Make(struct type t = int let compare = compare end)

(* Split an abstract input into two parts *)  
let abstractSplit u =
  match List.rev u with
  | Abstract::rest -> (List.rev (Abstract::AbstractR::rest), List.rev (Abstract::AbstractL::rest))
  | _ -> failwith "This split should not happen"

    
(* Generate concrete input values from an abstract input *)
let generateConcrete u =
  let rec work u accL accR =
    match u with
    | Abstract::us -> work us (ConcreteL::accL) (ConcreteR::accR)
    | AbstractL::us -> work us (ConcreteL::accL) (ConcreteML::accR)
    | AbstractR::us -> work us (ConcreteMR::accL) (ConcreteR::accR)
    | [] -> (List.rev accL, List.rev accR)
    | _ -> failwith "Generate concrete should not happen"
  in work u [] []

  
(* Implements the sound abstract search, according to the paper *)  
let sound_abstract_search bound wcetfunc  =
  printf "Sound abstract search\n%!";
  let rec search inputsetU w =
    match inputsetU with
    (* Pick an interval and abstractly execute it *)
    | u::us -> let t_a = wcetfunc bound u in
               (* Check if the interval has a sound WCET *)
               if t_a = bound then
                 (* Not sound. Check if out of bound *)
                 let (i1,i2) = generateConcrete u in
                 let t_c1 = wcetfunc bound i1 in
                 let t_c2 = wcetfunc bound i1 in
                 if t_c1 = bound || t_c2 = bound then
                   (* Out of bound *)
                   None
                 else
                   (* Split the interval *)
                   let (u1,u2) = abstractSplit u in
                   search (u1::u2::us) w
               else
                 let w = IntMap.add t_a u w in
                 search us w
    | [] -> Some(w)                   
  in
   search [[Abstract]] (IntMap.empty) 

     
(* Implements the optimal abstract search, according to the paper *)
let optimal_abstract_search w bound wcetfunc =
  let rec search w =
    (* Pick the input set with the highest WCET *)
    
    (* printf "size %d\n%!" (IntMap.cardinal w); *)
    let (t_o,u) = IntMap.max_binding w in
    (* print_spliter u; *)
    let w = IntMap.remove t_o w in
    (* printf "Rsize %d\n%!" (IntMap.cardinal w); *)
    (* Check if it is the optimal interval *)
    let (i1,i2) = generateConcrete u in
    (* printf "print spliter concrete\n%!"; *)
    (* print_spliter i1; *)
    (* print_spliter i2; *)
    let t_c1 = wcetfunc bound i1 in
    let t_c2 = wcetfunc bound i2 in
    (* printf "tc1tc2 %d %d\n%!" t_c1 t_c2; *)
    if t_c1 = t_o || t_c2 = t_o then
      (* Optimal value *)
      t_o
    else      
      (* Split the interval *)
      let (u1,u2) = abstractSplit u in
      (* printf "print spliter\n%!"; *)
      (* print_spliter u1; *)
      (* print_spliter u2; *)
      let t_a1 = wcetfunc bound u1 in
      let t_a2 = wcetfunc bound u2 in
      let w = IntMap.add t_a1 u1 w in
      let w = IntMap.add t_a2 u2 w in
      (* printf "tc1tc2 %d %d\n%!" t_a2 t_a1; *)
      (* printf "Size %d\n%!" (IntMap.cardinal w); *)
      search w
  in
    search w
  
(* Performs abstract search, but first trying to find a sound bound, and then, 
   if successful, find the optimal bound *)          
let abstract_search bound wcetfunc =
  printf "abstract_serach\n%!";
  match sound_abstract_search bound wcetfunc with
  | None ->
    printf "Found counter example. For certain input, the execution\n%!";
    printf "time exceeds the execution bound of %d cycles.\n%!" bound
  | Some(w) ->
    let (t_s,u) = IntMap.max_binding w in
    printf "Found a sound WCET bound of %d cycles.\n%!" t_s;
    printf "Trying to find the optimal WCET\n%!";
    let t_o = optimal_abstract_search w bound wcetfunc in
    printf "Found an optimal WCET bound of %d cycles.\n%!" t_o    

let _ = if !dbg && !dbg_trace then Printexc.record_backtrace true else ()

    
(** Print main state info *)
let print_mstate str ms =
  let print_pstate ps =
    if recording() then (*!record_mtags then *)
      Ustring.write_file
        (sprintf "memmap_%s_%s.ml" str (string_of_int (!record_ntask)))
        (us (print_amem str ps.hmem))
      else  (
      printf "Counter: %d\n" !counter;
      printf "BCET:  %d cycles\n" ps.bcet;
      printf "WCET:  %d cycles\n" ps.wcet;
      if !dbg_stats then 
        print_hmem_stats ps.hmem;
      uprint_endline (pprint_pstate 32 ps))
  in
  let ps = ms.pstate in
  match ps with
  | Nobranch ps -> print_pstate ps
  | Sbranch (_,psold,ps1,ps2) -> should_not_happen 15
  (*  (match psold with
     | Some (Nobranch ps) -> print_pstate ps
     | _ -> should_not_happen 15)*)
  | Branch _ -> should_not_happen 12
               
type options_t =
  | OpDebug
  | OpArgs
  | OpConfigBatchSize
  | OpConfigMaxCycles
  | OpRecord
  | OpOptimize
      
open Ustring.Op
       
let options =
  [ (OpDebug, Uargs.No, us"-debug", us"",
     us"Enable debug.");
    (OpOptimize, Uargs.No, us"-optimization", us"",
     us"Enable search based optimization.");
    (OpRecord, Uargs.Int, us"-record", us"",
     us"Record memory accesses.");
    (OpConfigBatchSize, Uargs.Int, us"-bsconfig", us"",
     us"Configure maximum batch size.");
    (OpConfigMaxCycles, Uargs.Int, us"-max_cycles", us"",
     us"Configure maximum cycles.");
    (OpArgs, Uargs.StrList, us"-args", us" <args>",
     us"Accepts Initial Intervals for Registers a0, a1, a2, and a3.");  ]


  
let analyze startblock bblocks gp_addr mem defaultargs tasks n =
  let args = (Array.to_list Sys.argv |> List.tl) in
  let (ops, args) = Uargs.parse args options in
  let debug = Uargs.has_op OpDebug ops in
  enable_debug debug;

  let optimization = Uargs.has_op OpOptimize ops in

  if Uargs.has_op OpRecord ops then
    set_record true (Uargs.int_op OpRecord ops)
  else 
    set_record false 0;
  
  if Uargs.has_op OpConfigMaxCycles ops then
    (set_max_cycles (Uargs.int_op OpConfigMaxCycles ops));
  if Uargs.has_op OpConfigBatchSize ops then
    (set_max_batch_size (Uargs.int_op OpConfigBatchSize ops));
  let args =
    if Uargs.has_op OpArgs ops then
      Uargs.strlist_op OpArgs ops |> List.map Ustring.to_utf8
    else
      []
  in
  let args = if args = [] then defaultargs else args in
  if !record_mtags then
    (printf "RECORDING\n%!";
    analyze_main startblock bblocks gp_addr args mem tasks n (!config_max_cycles) [Abstract] |> print_mstate bblocks.(startblock).name
    )else if optimization then
        (printf "OPT\n%!";
         abstract_search (!config_max_cycles)
           (mywcetfunc startblock bblocks gp_addr args mem tasks n ))  
      else
        (printf "NORMAL\n%!";try
           analyze_main startblock bblocks gp_addr args mem tasks n (!config_max_cycles) [Abstract] |> print_mstate bblocks.(startblock).name;
         with
         | MaxCyclesException -> printf "Analysis not finished. A path reached the maximum cycles allowed: %d\n%!" (!config_max_cycles)
        );  

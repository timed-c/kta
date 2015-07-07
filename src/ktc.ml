
open TaFile
open TaFileTypes
open Printf
open Ustring.Op
open MipsAst
open MipsEval 



(* ****************** DUMMY CODE **************************** *)

let get_req_tpp req = 
  match req with ReqWCP(t1,t2)  | ReqBCP(t1,t2) | ReqLWCET(t1,t2) | ReqLBCET(t1,t2) |
                 ReqFWCET(t1,t2) | ReqFBCET(t1,t2) -> (t1,t2)

(* Compare two tpp values. Compatible with the standard 'compare' interface. *)
let tpp_compare t1 t2 = 
  match t1,t2 with
  | TppEntry,TppEntry -> 0  
  | TppEntry,_ -> -1
  | _,TppEntry -> 1
  | TppExit,TppExit -> 0
  | TppExit,_ -> 1
  | _,TppExit -> -1
  | TppNode(v1),TppNode(v2) -> compare v1 v2



  
(* Function [list_remove_duplicates f l] takes a list [l], sorts it using comparsion
   function [f] and removes any duplicates that exists in the list. The returned
   list is in reversed sorted order without any duplicates *)
let list_remove_duplicates compare_func list = 
  let rec work list acc = 
    match list with
    | x1::x2::ls -> if compare_func x1 x2 = 0 then work (x2::ls) acc 
                    else work (x2::ls) (x1::acc)
    | [x1] -> x1::acc
    | [] -> acc
  in
    work (List.fast_sort compare_func list) []
    

(* Takes a timing analysis function request and returns a list of timing analysis
   responses *)
let dummy_timing_analysis func_ta_req = 
  (* Get all programming point numbers *)
  let tpps = TppEntry::TppExit::(List.fold_left (fun acc (_,tareq) -> 
    let (t1,t2) = get_req_tpp tareq in t1::t2::acc 
  ) [] (func_ta_req.ta_req))
  in

  (* Remove all duplicates *)
  let tpps_distinct = List.rev (list_remove_duplicates tpp_compare tpps) in  

  (* Sort and create absolute time for each tpp as an associate list *)
  let timediff = 100 in
  let tpp_times = snd (List.fold_left (fun (time,acc) tpp ->
    (time + timediff, (tpp,time)::acc)    
  ) (timediff,[]) tpps_distinct)
  in
  
  (* Generate the response *)
  List.rev (List.fold_left (fun acc (_,tareq) -> 
    let find_path t1 t2 = (
      let cv = tpp_compare t1 t2 in
      if cv = 0 then TppPath([t1]) else if cv > 0 then TppPathInfinity else 
          TppPath(List.filter (fun x -> 
            (match x with | TppNode(13) -> false | _ -> ( 
            tpp_compare x t1 >= 0 && tpp_compare x t2 <= 0))) tpps_distinct))
    in
    let get_ta_time t1 t2 wrong_order =
      match t1,t2 with 
      | TppNode(13),_ | _,TppNode(13) -> TimeUnknown 
      | _ ->  (
      let cv = tpp_compare t1 t2 in
      if cv = 0 then TimeCycles(0) else if cv > 0 then wrong_order else 
      TimeCycles(List.assoc t2 tpp_times - List.assoc t1 tpp_times))
    in                                                               
    match tareq with
    | ReqWCP(t1,t2)  -> ResWCP(find_path t1 t2)::acc
    | ReqBCP(t1,t2) ->  ResBCP(find_path t1 t2)::acc
    | ReqLWCET(t1,t2) -> ResLWCET(get_ta_time t1 t2 TimeInfinity)::acc
    | ReqLBCET(t1,t2) -> ResLBCET(get_ta_time t1 t2 TimeInfinity)::acc
    | ReqFWCET(t1,t2) -> ResFWCET(get_ta_time t1 t2 (TimeCycles(0)))::acc
    | ReqFBCET(t1,t2) -> ResFBCET(get_ta_time t1 t2 (TimeCycles(0)))::acc
  ) [] func_ta_req.ta_req)

    
              
(* Error levels *)
type error_level = Error | Warning
  


(** Format and print an error message to the standard output *)
let print_error filename line column level message =
  uprint_endline (us filename ^. us":" ^. ustring_of_int line ^. 
      us":" ^. ustring_of_int column ^. us" " ^.
      us (match level with Error -> "error: " | Warning -> "warning: ") ^.
      us message)



(** Function [run_timing_analysis names w s f] performs timing analysis
    according to the specification in the list of files [names] using
    timing analysis function [f]. If flag [w] is true, the result is
    written to files with the same name as in [names], appended with the
    suffix [".out"]. If the simple output flag [s] is true, then the generated
    output format is simple. The function returns the concatenated string of 
    all output. *)
let run_timing_analysis filenames write_files simple_output func_timing_analysis =
  (* Parse all files *)
  let file_ta_requests = List.map parse_ta_file filenames in

  (* Iterate over all file requests. *)
  List.fold_left (fun acc file_ta_req -> 
    (* Perform timing analysis on all functions *)
    let ta_reps = List.map (fun x -> (x,func_timing_analysis x)) 
      (file_ta_req.func_ta_reqs) in
    (* Generate pretty printed output for one function *)
    let output = Ustring.concat (us"") (List.map (fun (req,res) ->
      (if simple_output then pprint_simple_ta_res else pprint_full_ta_res req) res)
      (ta_reps)) in
    (* If chosen, write to file *)
    if write_files then Ustring.write_file (file_ta_req.ta_filename ^ ".out") output;
    (* Accumulate all output *)
    acc ^. output 
  ) (us"") file_ta_requests 

(* ****************** END DUMMY CODE **************************** *)


(* Simple test function that pretty prints the assembly code of 
   mips assembly code that is generated from compiling a C file. *)
let mips_print filename = 
  let insts = MipsUtils.decode (Utils.read_binfile filename) in
  print_endline "";
  uprint_endline (MipsUtils.pprint_inst_list insts) 
    
let mips_compile filename opt = 
  let tmpname = "__tmp__" in
  MipsSys.pic32_compile [filename] false opt tmpname;
  let prog =  MipsUtils.add_branch_symbols (MipsSys.get_program tmpname) in
  Sys.remove tmpname;
  print_endline "";
  uprint_endline (MipsUtils.pprint_asm prog prog.text_sec.addr prog.text_sec.size true)

let mips_sections filename opt = 
  let tmpname = "__tmp__" in
  MipsSys.pic32_compile [filename] false opt tmpname;
  let secs  = MipsSys.section_info tmpname in
  Sys.remove tmpname;
  List.iter (fun x -> let (k,(s,a)) = x in printf "%s %d,0x%x\n" k s a) secs
  
let mips_symbols filename opt = 
  let tmpname = "__tmp__" in
  MipsSys.pic32_compile [filename] false opt tmpname;
  let symtbl  = MipsSys.symbol_table tmpname in
  Sys.remove tmpname;
  List.iter (fun x -> let (k,a) = x in printf "%s -> 0x%x\n" k a) symtbl
  
let print_res state =
  printf "Result v1 = %d\n" (Int32.to_int state.registers.(2))

let stack_ptr = 0x80000000 - 8 
let stack_size = 1024*256 
let stack_addr = stack_ptr - stack_size + 8

let mips_eval filename func args opt = 
  let tmpname = "__tmp__" in
  MipsSys.pic32_compile [filename] false opt tmpname;
  let prog = MipsSys.assign_program_stack (MipsSys.get_program tmpname) 
             stack_ptr stack_size stack_addr in
  let initstate = MipsEval.init prog func args  in
  let (state,(count,terminate)) = MipsEval.eval prog initstate MipsEval.cycle_count (0,None)  in
  let res = Int32.to_int state.registers.(2) in
  uprint_endline (MipsEval.pprint_state state);
  printf "Result v0 = %d (0x%x)\n" res res;
  printf "Cycle count = %d\n\n"  count;
  (try
    let (b,ptr,maxval) = MipsEval.getmemptr state prog res 1 in
    uprint_endline (MipsUtils.pprint_bytes b ptr (min maxval 32) res false)
  with _ -> ()); 
  (match terminate with
  | None -> ()
  | Some(s) -> printf "Error: %s\n" s);
  Sys.remove tmpname

let mips_debug filename func args opt = 
  let tmpname = "__tmp__" in
  MipsSys.pic32_compile [filename] false opt tmpname;
  let prog = MipsSys.assign_program_stack (MipsUtils.add_branch_symbols (
             MipsSys.get_program tmpname)) stack_ptr stack_size stack_addr in
  let initstate = MipsEval.init prog func args  in
  uprint_endline (MipsEval.pprint_state initstate);  
  let (state,(str,_)) = MipsEval.eval prog initstate MipsEval.debug_print 
                      (us"", initstate.registers) in
  uprint_endline str;
  printf "Result v0 = %d\n\n" (Int32.to_int state.registers.(2));
  uprint_endline (MipsEval.pprint_state state);
  Sys.remove tmpname



let mips_verify filename file_ta_req = 
  let tmpname = "__tmp__" in
  MipsSys.pic32_compile [filename] false true tmpname;
  let prog = MipsSys.assign_program_stack (MipsSys.get_program tmpname) 
    stack_ptr stack_size stack_addr in

  let ta_req = List.hd (file_ta_req.func_ta_reqs) in
  let fname = (Ustring.to_utf8 ta_req.funcname) in
  let args = [] in
  let initstate = MipsAbstract.init prog fname args in
  let dist = MipsAbstract.distance prog fname args in
  let timeout = 1000 in
  let (ok,wcet,state) = MipsAbstract.eval prog initstate dist timeout in
  
  (if ok then
    if wcet <= timeout then printf "Verification OK. WCET = %d\n" wcet
    else printf "Verification could not terminated within the specified timeout.\n" 
  else printf "Verification failed due to timeout.\n");  
      
  Sys.remove tmpname


    
let print_help() =     
    printf "KTC - KTH Timed Compiler. Copyright (C) 2015 David Broman.\n"


let main =
  (* Do mips pretty printing test *)
  let len = Array.length Sys.argv in
  if len <= 1 then (print_help(); exit(0))
    else
  if Sys.argv.(1) = "-mips" then 
    mips_print (Sys.argv.(2))
  else if Sys.argv.(1) = "-compile" then 
      mips_compile (Sys.argv.(2)) true
  else if Sys.argv.(1) = "-compile-no-opt" then 
      mips_compile (Sys.argv.(2)) false
  else if Sys.argv.(1) = "-sections" then 
      mips_sections (Sys.argv.(2)) true
  else if Sys.argv.(1) = "-symbols" then 
      mips_symbols (Sys.argv.(2)) true
  else if Sys.argv.(1) = "-eval" || Sys.argv.(1) = "-debug"  then 
    let filename = if len >= 3 then (Sys.argv.(2)) else failwith "No filename" in
    let funcname = if len >= 4 then (Sys.argv.(3)) else "main" in
    let args = 
      if len <= 4 then [] 
      else
        let int32arg x = Int32.of_int (int_of_string x) in
        let lst = Array.to_list (Array.sub Sys.argv 4 
                                   ((Array.length Sys.argv)-4)) in        
        List.map int32arg lst   
    in     
    match Sys.argv.(1) with
    | "-eval" ->  mips_eval filename funcname args true
    | "-debug" ->  mips_debug filename funcname args true
    | _ -> failwith "Cannot happen"

  else if len = 4 && Sys.argv.(1) = "-verify" then
    let filename = Sys.argv.(2) in
    let tafile = Sys.argv.(3) in
    let tareq = TaFile.parse_ta_file tafile in
    mips_verify filename tareq
      
  else if len >= 3 && Sys.argv.(1) = "-test_tafile" then (
    (* Test and parse the timing analysis file *)
    if true then (
      let filenames =  Array.to_list (Sys.argv) |> List.tl |> List.tl  in
    try let _ = run_timing_analysis filenames true true dummy_timing_analysis in ()
    with 
    | TA_file_syntax_error(filename,line) -> 
      (print_error filename line 0 Error "Syntax error in timing analysis file.";
      exit 1)
    | Sys_error(filename) ->
      (print_error filename 0 0 Error ("Error reading file '" ^ filename ^ "'");
       exit 1)))
  else
    print_help()
      
   

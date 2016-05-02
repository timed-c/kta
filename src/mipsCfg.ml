
open Ustring.Op
open MipsAst
open Printf


let arrow_padding = 40  (* Characters for the printed arrows |> *)
let inden_inst = us"  " (* Characters indent for the instruction *)

let idno = ref 1  

(* Used for checking visited nodes *)
module IntSet = Set.Make(
  struct
    let compare = Pervasives.compare
    type t = int
  end)    

  
(** Add a new symbol with address to the program *)  
let add_addrsym_prog prog addr sym =
  {prog with sym2addr = Sym2Addr.add sym addr prog.sym2addr;
             addr2sym = Addr2Sym.add addr sym prog.addr2sym}

(* Creates new symbol if address to symbol already exists. 
   Returns a tuple: the new program with the added symbol and the actual symbol *)
let rec new_unique_sym prog addr =
  let sym = "ex" ^ string_of_int !idno in
  if not (MipsAst.Sym2Addr.mem sym prog.sym2addr) then (
    idno := !idno + 1;
    (add_addrsym_prog prog addr sym, sym))
  else
    new_unique_sym prog addr
    
  
let getinstidx prog addr =
  (addr - prog.text_sec.addr)/4

(* Checks if there is a symbol for the address. If not, create a new
   symbol. Returns a tuple (prog',sym) where prog' is the updated program *)
let mksym_from_addr prog addr =
  try (prog,MipsAst.Addr2Sym.find addr prog.addr2sym)
  with _ -> new_unique_sym prog addr 

(* Returns the address for a specific symbol. Assumes that the symbol exists *)    
let s2a prog sym = MipsAst.Sym2Addr.find sym prog.sym2addr
  
  
    
(* Returns the tuple (prog',codelist,exittype) where prog' the updated program,
   codelist the code list for the basic block, and exittype the actual [exittype]. *)
let rec getblockcode prog addr acc_code =
  let inst = prog.code.(getinstidx prog addr) in
  match inst with
  | MipsBEQ(_,_,_,s)  | MipsBGEZ(_,_,s) | MipsBGTZ(_,_,s) |
    MipsBLEZ(_,_,s) | MipsBLTZ(_,_,s) | MipsBNE(_,_,_,s) 
    ->
    let inst_ds = prog.code.(getinstidx prog (addr+4)) in
    let (prog,next_addr_sym) = mksym_from_addr prog (addr+8) in    
    let codelist = List.rev (inst_ds::inst::acc_code) in
    (prog,codelist,ExitTypeBranch(s,next_addr_sym))
  | MipsBEQL(_,_,_,s) | MipsBNEL(_,_,_,s) ->
    let inst_ds = prog.code.(getinstidx prog (addr+4)) in
    let (prog,next_addr_sym) = mksym_from_addr prog (addr+8) in    
    let codelist = List.rev (inst_ds::inst::acc_code) in
    (prog,codelist,ExitTypeBrLikely(s,next_addr_sym))
  | MipsJALR(rs) -> failwith "JALR not implemented yet"
  | MipsJR(rs) ->
    let codelist = List.rev (inst::acc_code) in
      (* TODO: This is just temp code. It assumes that the mapping to ra is correct. 
         Cannot handle jump tables, will fail *)
     if rs = reg_ra then 
       (prog,codelist,ExitTypeReturn)
     else failwith "Indirect jumps (used in switch statements) are not supported."
  | MipsJ(_,s) ->
    let codelist = List.rev (inst::acc_code) in
    (prog,codelist,ExitTypeJump(s))
  | MipsJAL(_,s) ->
    let (prog,next_addr_sym) = mksym_from_addr prog (addr+4) in     
    let codelist = List.rev (inst::acc_code) in
    (prog,codelist,ExitTypeCall(s,next_addr_sym))
  | _ ->
    try let sym = MipsAst.Addr2Sym.find (addr+4) prog.addr2sym in
        let codelist = List.rev (inst::acc_code) in
        (prog,codelist,ExitTypeNext(sym))
    with Not_found -> getblockcode prog (addr+4) (inst::acc_code) 

(* returns a tuple (prog',block) *)   
let getbblock prog addr =
  let (prog,codelist,exittype) = getblockcode prog addr [] in
  let block = {
    block_addr = addr;
    block_code = codelist;
    block_exit = exittype;
    block_dist = 0;
  } in
  (prog,block)

    
(* Traverse a function and generates the CFG *)
let make_cfg addr prog =
  (* The first pass - traverse and generate graph *)
  let rec traverse addr prog graph visited funccalls = 
    (* Check if already visited *)
    if IntSet.mem addr visited then (prog,graph,visited,funccalls)
    else
      (* Get one basic block and add it to the graph *)
      let (prog,block) = getbblock prog addr in
      (* Add to graph, new symbol if necessary *)
      let (prog,sym) = mksym_from_addr prog addr in
      let graph = BlockMap.add sym block graph in
      (* Mark as visited *)
      let visited = IntSet.add addr visited in

      (* Go to next node *)
      match block.block_exit with
      | ExitTypeNext(nid) ->
        traverse (s2a prog nid) prog graph visited funccalls
      | ExitTypeBranch(tid,fid) ->
        let (prog,graph,visited,funccalls) =
          traverse (s2a prog fid) prog graph visited funccalls in
        traverse (s2a prog tid) prog graph visited funccalls
      | ExitTypeBrLikely(tid,fid) ->
        let (prog,graph,visited,funccalls) =
          traverse (s2a prog fid) prog graph visited funccalls in
        traverse (s2a prog tid) prog graph visited funccalls
      | ExitTypeJump(id) ->
        traverse (s2a prog id) prog graph visited funccalls
      | ExitTypeCall(callid,nid) ->
        traverse (s2a prog nid) prog graph visited (callid::funccalls)
      | ExitTypeReturn ->
        (prog,graph,visited,funccalls)
  in
  (* Second pass. Generate extra nodes for all likely finish nodes, e.g., bnel *)
  let rec expand_likely prog graph =
    let (likely,graph) = BlockMap.partition
      (fun _ block -> match block.block_exit with| ExitTypeBrLikely(_,_) -> true | _ -> false) graph in
    BlockMap.fold (fun name block (prog,graph) -> (
      let nextsym = match block.block_exit with
                    ExitTypeBrLikely(_,next) -> next | _ -> failwith "Should not" in      
      let new_addr = block.block_addr + (List.length block.block_code - 1) * 4 in
      let (prog,new_sym) = mksym_from_addr prog new_addr in
      let (inst,truebranch,new_block_code) =
        match List.rev block.block_code with
        | inst::MipsBEQL(rs,rt,imm,s)::rest -> (inst,s,List.rev (MipsBEQL(rs,rt,imm,new_sym)::rest))
        | inst::MipsBNEL(rs,rt,imm,s)::rest -> (inst,s,List.rev (MipsBNEL(rs,rt,imm,new_sym)::rest))
        | _ -> failwith "should not happen"
      in
      let block1 = {block with block_code = new_block_code; block_exit = ExitTypeNext(nextsym)} in
      let block2 = {block_addr = new_addr; block_code=[inst];
                    block_exit = ExitTypeNext(truebranch); block_dist=0} in
      let graph = BlockMap.add name block1 graph in
      let graph = BlockMap.add new_sym block2 graph in
      (prog,graph)
    )) likely (prog,graph)
  in
  (* Create entry node symbol *)
  let (prog,entry_node) = mksym_from_addr prog addr in
  (* Traverse the graph *)
  let (prog,graph,_,funccalls) = traverse addr prog BlockMap.empty IntSet.empty [] in
  (* Expand likely nodes *)
  let (prog,graph) = expand_likely prog graph in
  (* Create exit node *)
  let exit_node = "no_exit_yet" in
  (* Construct and return the control flow graph *)
  let cfg = {
      entry_node = entry_node; 
      exit_node  = exit_node;
      cfg_graph = graph;
  } in
  (prog,cfg,funccalls)
  




let make_cfgmap program = MipsAst.CfgMap.empty


(* Pretty print one basic block *)
let pprint_bblock name block =
  let add_arrow str = (Ustring.spaces_after str arrow_padding) ^. us"|>\n" in
  let head = us"let " ^. (us name) ^. us" ms = ms" |> add_arrow in
  let rec instlist a instlst =
    match instlst with
    | i::is ->
      instlist (a ^. add_arrow(inden_inst ^. MipsUtils.pprint_inst_ocaml i)) is
    | [] -> a
  in
  let final = (match block.block_exit with
    | ExitTypeNext(_)  | ExitTypeBranch(_,_)  | ExitTypeJump(_) |
      ExitTypeCall(_,_) -> inden_inst ^.  us"next" ^. us"\n"
    | ExitTypeBrLikely(_,_) -> inden_inst
    | ExitTypeReturn -> us"")
  in
    head ^. instlist (us"") block.block_code ^. final


(* Pretty print a whole control flow graph *)      
let pprint_ocaml_cps_from_cfg nice_output cfg prog =
  let cfglst = BlockMap.bindings cfg.cfg_graph in
  let cfglst = if nice_output then cfglst 
                  |> List.map (fun (s,b) -> (s2a prog s,(s,b)))
                  |> List.fast_sort (fun (k1,_) (k2,_) -> compare k1 k2) 
                  |> List.split |> snd
               else cfglst
  in        
  List.fold_left (fun a (name,block) ->
    a ^. pprint_bblock name block ^. us"\n"
  ) (us"") cfglst
    

 
  
let pprint_ocaml_cps_from_funcmap funcmap = us"NOP"

  
  
let test prog fname =
  let (prog,cfg1,_) = make_cfg (Sym2Addr.find fname prog.sym2addr) prog in
  uprint_endline (pprint_ocaml_cps_from_cfg true cfg1 prog);    
  printf "Yes, length of code: %d\n" (Array.length prog.code);
    








  

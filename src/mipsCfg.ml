
open Ustring.Op
open MipsAst
open Printf


let arrow_padding = 40  (* Characters for the printed arrows |> *)
let inden_inst = us"  " (* Characters indent for the instruction *)

let idno = ref 1  

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
  | MipsBEQL(_,_,_,s) | MipsBNEL(_,_,_,s) -> failwith "Not yet implemented"     
  | MipsJALR(rs) -> failwith "JALR not implemented yet"
  | MipsJR(rs) ->
     let codelist = List.rev (inst::acc_code) in
     (prog,codelist,ExitTypeReturn)
  | MipsJ(_,s) ->
    let codelist = List.rev (inst::acc_code) in
    (prog,codelist,ExitTypeJump(s))
  | MipsJAL(_,s) ->
    let (prog,next_addr_sym) = mksym_from_addr prog (addr+4) in     
    let codelist = List.rev (inst::acc_code) in
    (prog,codelist,ExitTypeCall(s,next_addr_sym))
  | _ -> getblockcode prog (addr+4) (inst::acc_code) 

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


let rec get_cfg_graph prog addr graph =
  let (prog,block) = getbblock prog addr in
  let (prog,sym) = mksym_from_addr prog addr in
  let graph = BlockMap.add sym block graph in
  (prog,graph)
 
      
let make_cfg addr prog =
  let (prog,entry_node) = mksym_from_addr prog addr in
  let exit_node = "no_exit_yet" in
  let (prog,graph) = get_cfg_graph prog addr BlockMap.empty in
  (* Construct and return the control flow graph *)
  let cfg = {
      entry_node = entry_node; 
      exit_node  = exit_node;
      cfg_graph = graph;
  } in
  (prog,cfg)



let make_cfgmap program = MipsAst.CfgMap.empty



let pprint_bblock name block =
  let add_arrow str = (Ustring.spaces_after str arrow_padding) ^. us"|>\n" in
  let head = us"let " ^. (us name) ^. us" ms = ms" |> add_arrow in
  let rec instlist a instlst =
    match instlst with
    | i::is ->
       instlist (a ^. add_arrow (inden_inst ^. MipsUtils.pprint_inst_ocaml i)) is
    | [] -> a
  in
  let final = (match block.block_exit with
    | ExitTypeNext(_)  | ExitTypeBranch(_,_)  | ExitTypeJump(_) |
      ExitTypeCall(_,_) -> inden_inst ^.  us"next"
    | ExitTypeReturn -> us"")
  in
    head ^. instlist (us"") block.block_code ^. final

    
let pprint_ocaml_cps_from_cfg cfg =
  List.fold_left (fun a (name,block) ->
    a ^. pprint_bblock name block ^. us"\n"
  ) (us"") (BlockMap.bindings cfg.cfg_graph)
    

 
  
let pprint_ocaml_cps_from_funcmap funcmap = us"NOP"

  
  
let test prog fname =
  let (prog,cfg1) = make_cfg (Sym2Addr.find fname prog.sym2addr) prog in
  uprint_endline (pprint_ocaml_cps_from_cfg cfg1);    
  printf "Yes, length of code: %d\n" (Array.length prog.code);
    








  

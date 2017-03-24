
open Ustring.Op
open MipsAst
open Printf
open MipsEval

let comp_name = "mipsel-mcb32-elf"
let objcopy = comp_name ^ "-objcopy"
let objdump = comp_name ^ "-objdump"
let nm = comp_name ^ "-nm"
let gcc = comp_name ^ "-gcc"
let section_guid = "20b8de13-4db6-4ac8-89ff-0bb1ac7aadc8"

let empty_sid = usid ""
let tpp_magic = us"MAGIC070704090916_TPP_"
let magic_len = Ustring.length tpp_magic

let enable_verbose = ref false
let kta_wcet_runtime_path = "KTA_WCET_RUNTIME_PATH"
(* ---------------------------------------------------------------------*)
let verbose enable = 
  enable_verbose := enable
  
(* ---------------------------------------------------------------------*)
let verbose_enabled() = !enable_verbose
  

(* ---------------------------------------------------------------------*)
let get_section filename section =
  try 
    let command = (objcopy ^ " " ^ filename ^ " --dump-section " 
                   ^ section ^ "=" ^ section_guid) in
    if !enable_verbose then print_endline (command ^ "\n");
    let (code,stdout,stderr) = 
      USys.shellcmd command in
    if code != 0 then Bytes.empty 
    else
      let data = Utils.read_binfile section_guid in
      Sys.remove section_guid;
      data
  with
    _ -> Bytes.empty
  

(* ---------------------------------------------------------------------*)
let pic32_compile filenames only_compile optimization outputname =
  let cflags = " -ffreestanding  -mips32 -mips2  -msoft-float -Wa,-msoft-float " in
  (*"-I/opt/mcb32tools/include/ -L/opt/mcb32tools/lib/ -lm " in*)
(* NOTE:  -march=mips32r2  and -mips32 -mips2  are not the same. *)
  let command = (gcc ^ cflags ^ (String.concat " " filenames) ^ " " ^
                   (if only_compile then "-c " else "") ^ 
                     (sprintf "-O%d " optimization) ^ 
                       "-o " ^ outputname) in 
  if !enable_verbose then print_endline (command ^ "\n");
  let (code,stdout,stderr) = USys.shellcmd command in
  if code != 0 then raise (Sys_error (stderr ^ " " ^ stdout)) else ()
  

    
(* ---------------------------------------------------------------------*)
let section_info filename = 

  (* Print verbose *)
  let command = (objdump ^ " -h " ^ filename) in
  if !enable_verbose then print_endline (command ^ "\n");

  let (code,stdout,stderr) = 
    USys.shellcmd command in
  if code != 0 then raise (Sys_error (stderr ^ " " ^ stdout));
  let lines = List.map Ustring.trim (Ustring.split (us stdout) (us"\n")) in
  let splits = List.map (fun x -> (
                            List.filter (fun y -> Ustring.length y != 0) 
                              (Ustring.split x (us" ")))) lines in
  let filtered = List.filter 
    (fun list -> match list with 
              | l::ls -> (try let _ = int_of_string (Ustring.to_utf8 l) in true 
                          with _ -> false)
              | _ -> false) splits in                            
  List.map (fun line ->
            match line with
            | _::sec::size::addr::_ -> 
                 (Ustring.to_utf8 sec, 
                  (int_of_string ("0x" ^ (Ustring.to_utf8 size)),
                   int_of_string ("0x" ^ (Ustring.to_utf8 addr))))
            | _ -> raise (Sys_error 
                            ("Error reading section data: " ^ stdout)))
            filtered



(* ---------------------------------------------------------------------*)
let symbol_table filename = 

  let command = nm ^ " " ^ filename in
  if !enable_verbose then print_endline (command ^ "\n");
  let (code,stdout,stderr) = USys.shellcmd command in
  if code != 0 then raise (Sys_error (stderr ^ " " ^ stdout));
  let lines = List.map Ustring.trim (Ustring.split (us stdout) (us"\n")) in
  List.fold_left 
    (fun acc line ->
      let sp = List.filter (fun y -> Ustring.length y != 0) 
                            (Ustring.split line (us" ")) in
      match sp with
      | addr::_::sym::_ -> (
          try
            let addrno = int_of_string ("0x" ^ (Ustring.to_utf8 addr)) in
            (Ustring.to_utf8 sym,addrno)::acc
          with _ -> acc)
      | _ -> acc
    ) [] lines
  
  
(* ---------------------------------------------------------------------*)
let get_program filename =
  let build_in_names = ["_start"; "_gp"; "_ftext"; "_fdata";
			"_fbss"; "_end"; "_edata"; "__bss_start"] in
  let l_symbols = List.rev (symbol_table filename) in
  let l_sections = section_info filename in
  let l_text = try Some(List.assoc ".text" l_sections) with _ -> None in
  let l_data = try Some(List.assoc ".data" l_sections) with _ -> None in
  let l_sdata = try Some(List.assoc ".sdata" l_sections) with _ -> None in
  let l_bss = try Some(List.assoc ".bss" l_sections) with _ -> None in
  let l_sbss = try Some(List.assoc ".sbss" l_sections) with _ -> None in
  let l_rodata = try Some(List.assoc ".rodata" l_sections) with _ -> None in
  let l_textcode = get_section filename ".text" in 
{ 
  filename = filename;
  symbols = l_symbols;
  sym2addr = List.fold_left (fun m (s,a) -> Sym2Addr.add s a m) 
                Sym2Addr.empty l_symbols;
  addr2sym = List.fold_left (fun m (s,a) -> Addr2Sym.add a s m) 
                Addr2Sym.empty (List.filter (fun (sym,a) -> not (List.exists (fun x -> x = sym) build_in_names)) l_symbols);
  sections = l_sections;
  text_sec = {d = l_textcode;
              addr = (match l_text with Some(_,a) -> a | None -> 0);
              size = (match l_text with Some(s,_) -> s | None -> 0); 
             };
  data_sec = {d = get_section filename ".data";
              addr = (match l_data with Some(_,a) -> a | None -> 0);
              size = (match l_data with Some(s,_) -> s | None -> 0);
             };
  sdata_sec = {d = get_section filename ".sdata";
              addr = (match l_sdata with Some(_,a) -> a | None -> 0);
              size = (match l_sdata with Some(s,_) -> s | None -> 0);
             };
  bss_sec = {d = Bytes.empty;
             addr =  (match l_bss  with Some(_,a) -> a | None -> 0);
             size =  (match l_bss  with Some(s,_) -> s | None -> 0);
            };
  sbss_sec = {d = Bytes.empty;
             addr =  (match l_sbss  with Some(_,a) -> a | None -> 0);
             size =  (match l_sbss  with Some(s,_) -> s | None -> 0);
            };
  rodata_sec = {d = get_section filename ".rodata";
              addr = (match l_rodata with Some(_,a) -> a | None -> 0);
              size = (match l_rodata with Some(s,_) -> s | None -> 0);
             };
  stack_sec = {d = Bytes.empty;
             addr = 0; 
             size = 0;
            };
  gp = (try List.assoc "_gp" l_symbols with _ -> 0);
  sp = 0;
  code = Array.of_list (MipsUtils.decode l_textcode);
}



(* ---------------------------------------------------------------------*)
let assign_program_stack prog ptr size addr = 
  {prog with stack_sec = {d = Bytes.empty; addr = addr; size = size};
             sp = ptr}



    
(* ---------------------------------------------------------------------*)
let cycle_count_with_tpp tppmap func_assumptions 
                         inst pc prog state is_a_delay_slot 
                         terminate ((wc_count,bc_count,lst),_) =
  try
    let tpp_sid_list = Array.get tppmap ((pc - prog.text_sec.addr) / 4) in 
    (* There can be more than one tpp for the same address *)
    let lst' =       
      if tpp_sid_list <> [] then 
        List.fold_left (fun lst tpp_sid -> (tpp_sid,(wc_count,bc_count))::lst) 
                       lst tpp_sid_list
      else 
        lst 
    in
    (* Compute extra time for function assumptions *)
    let (wc_g,bc_g) = 
      (match inst with
       | MipsJAL(_,s) -> (try func_assumptions s with _ -> (0,0))
       | _ -> (0,0))
    in
      (((wc_count+1+wc_g,bc_count+1+bc_g,lst'),terminate), false)      
  with
    _ -> failwith "Internal error in function cycle_count_with_tpp() in mipsSys.ml"
      



(* ---------------------------------------------------------------------*)
let get_eval_func ?(bigendian=false) prog statevarlist =

  (* Create the tpp map *)
  let tppmap = Array.make (Array.length prog.code) [] in
  List.iter (fun (s,a) ->
    let u = us s in
    (* Check if a tpp symbol *)
    if Ustring.starts_with tpp_magic u then
      (* Remove the magic string from the tpp string *)
      let tpp = Ustring.sub u magic_len (Ustring.length u - magic_len)
          |> sid_of_ustring in
      (* Calculate the address in words in the code section *)
      let address = (a - prog.text_sec.addr) / 4 in
      Array.set tppmap address (tpp::(Array.get tppmap address));
    ) prog.symbols;         
  (* Change the order of symbols, making them the same as original code *)
  let tppmap' = Array.map List.rev tppmap in


  
  (* Create the timed eval function *)
  let timed_eval_func funcname args meminitmap func_wcet func_bcet func_assumptions = 
        
    (* Initialize the state *)
    let state = MipsEval.init prog funcname args in
    
    (* Set memory init map *)
    List.iter (fun (addr,v) ->          
      let (mem,i,_) = MipsEval.getmemptr state prog addr 4 in
      MipsUtils.set_32_bits bigendian mem i v; 
    ) meminitmap;

    (* Evaluate/execute the function *)
    let (state,((wc_count,bc_count,tpp_path),terminate)) =
      MipsEval.eval ~bigendian:bigendian prog state 
        (cycle_count_with_tpp tppmap' func_assumptions) ((0,0,[]),None)  in 

    (* Get the state variables *)
    let statevarmap = List.map (fun x -> 
      let a = MipsEval.getaddr prog x in
      let v = MipsEval.getval bigendian state prog a in
      (a,v)
    ) statevarlist in

    (* TODO: Right now, there is no timeout. This means that
       a program with infinit loop will go on forever *)
    (ExhaustiveTA.TppTimedPath(wc_count,bc_count,List.rev tpp_path), statevarmap)
  in

  
  (* Return the timed eval function *)
  timed_eval_func
  

  
  
(* ---------------------------------------------------------------------*)
let get_init_state_vals ?(bigendian=false) prog initfunc statelist =

  (* Initialize the state *)
  let state = MipsEval.init prog initfunc [] in

  (* Run init function, if it exists, to generate the new state *)
  let state =
    if initfunc <> "" then(
      MipsEval.eval ~bigendian:bigendian prog state 
        (fun _ _ _ _ _ _ _ -> (0,false)) 0 |> fst)
    else state
  in

  (* Extract the state values *)
  List.map (fun x ->    
      let a = MipsEval.getaddr prog x in
      let v = MipsEval.getval bigendian state prog a in
      (a,v)
  ) statelist


(* ---------------------------------------------------------------------*)
let wcet_compile fname debug bsconfig program_code args =
  let remove_file fname = if Sys.file_exists fname then
                            Sys.remove fname
                          else () in  
  let bsconfigflag =
    match bsconfig with
    | None -> ""
    | Some i -> sprintf " -bsconfig %d " i
  in
  let flags = (if debug then " -debug " else "")
              ^ bsconfigflag in
  let args = List.fold_left (fun x -> (^) (x ^ " ")) " -args " args in
  let ocamlargs = " -lib str -- " ^ flags ^ args in
  let ocamlflnm = "temp_1214325_" ^ fname in

  let runtime_path =
    try
      Sys.getenv(kta_wcet_runtime_path) ^ "/"
    with Not_found -> "runtime/"
  in
  let files = [".ml"; ".native"] |> List.map (fun x -> runtime_path ^ ocamlflnm ^ x) in

  Ustring.write_file (List.hd files) program_code;
  try      
    if Sys.is_directory runtime_path then	
      (let command = ("sh -c \"cd " ^ runtime_path ^ "; ocamlbuild " ^ ocamlflnm ^ ".native" ^ ocamlargs ^ "\"") in
       if !enable_verbose then print_endline (command ^ "\n");
       let (code, stdout, stderr) = USys.shellcmd command in

       files |> List.iter remove_file;

       if code != 0 then raise (Sys_error (stderr ^ " " ^ stdout))
       else stdout)
    else
      (eprintf "Runtime PATH: %s is not a directory.\n\tSet enviroment variable KTA_WCET_RUNTIME_PATH\n" runtime_path;
       files |> List.iter remove_file;
       "")
  with Sys_error e ->
    e |> eprintf "Error %s\n";
    files |> List.iter remove_file;
    ""

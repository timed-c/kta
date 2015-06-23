
open Ustring.Op
open MipsAst

let comp_name = "mipsel-pic32-elf"
let objcopy = comp_name ^ "-objcopy"
let objdump = comp_name ^ "-objdump"
let nm = comp_name ^ "-nm"
let gcc = comp_name ^ "-gcc"
let section_guid = "20b8de13-4db6-4ac8-89ff-0bb1ac7aadc8"


(* ---------------------------------------------------------------------*)
let get_section filename section =
  try 
    let (code,stdout,stderr) = 
      USys.shellcmd (objcopy ^ " " ^ filename ^ " --dump-section " 
                   ^ section ^ "=" ^ section_guid) in
    if code != 0 then Bytes.empty 
    else
      let data = Utils.read_binfile section_guid in
      Sys.remove section_guid;
      data
  with
    _ -> Bytes.empty
  

(* ---------------------------------------------------------------------*)
let pic32_compile filenames only_compile optimization outputname =
(*  let cflags = " -ffreestanding -march=mips32r2 -msoft-float -Wa,-msoft-float " in *)
   let cflags = " -ffreestanding  -mips32 -mips2  -msoft-float -Wa,-msoft-float " in 
(* NOTE:  -march=mips32r2  and -mips32 -mips2  are not the same. *)
  let (code,stdout,stderr) = 
    USys.shellcmd (gcc ^ cflags ^ (String.concat " " filenames) ^ " " ^
                   (if only_compile then "-c " else "") ^ 
                   (if optimization then "-O3 " else "-O0 ") ^ 
                   "-o " ^ outputname) in
  if code != 0 then raise (Sys_error (stderr ^ " " ^ stdout)) else ()
  

    
(* ---------------------------------------------------------------------*)
let section_info filename = 
  let (code,stdout,stderr) = 
    USys.shellcmd (objdump ^ " -h " ^ filename) in
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
  let (code,stdout,stderr) = USys.shellcmd (nm ^ " " ^ filename) in
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
  let l_symbols = List.rev (symbol_table filename) in
  let l_sections = section_info filename in
  let l_text = try Some(List.assoc ".text" l_sections) with _ -> None in
  let l_data = try Some(List.assoc ".data" l_sections) with _ -> None in
  let l_sdata = try Some(List.assoc ".sdata" l_sections) with _ -> None in
  let l_bss = try Some(List.assoc ".bss" l_sections) with _ -> None in
  let l_sbss = try Some(List.assoc ".sbss" l_sections) with _ -> None in
  let l_textcode = get_section filename ".text" in 
{ 
  filename = filename;
  symbols = l_symbols;
  sym2addr = List.fold_left (fun m (s,a) -> Sym2Addr.add s a m) 
                Sym2Addr.empty l_symbols;
  addr2sym = List.fold_left (fun m (s,a) -> Addr2Sym.add a s m) 
                Addr2Sym.empty l_symbols;
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
  gp = (try List.assoc "_gp" l_symbols with _ -> 0);
  code = Array.of_list (MipsUtils.decode l_textcode);
}







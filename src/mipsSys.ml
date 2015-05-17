
open Ustring.Op

let comp_name = "mipsel-pic32-elf"
let objcopy = comp_name ^ "-objcopy"
let objdump = comp_name ^ "-objdump"
let nm = comp_name ^ "-nm"
let gcc = comp_name ^ "-gcc"
let section_guid = "20b8de13-4db6-4ac8-89ff-0bb1ac7aadc8"


let get_section filename section =
  let (code,stdout,stderr) = 
    USys.shellcmd (objcopy ^ " " ^ filename ^ " --dump-section " 
                   ^ section ^ "=" ^ section_guid) in
  if code != 0 then raise (Sys_error (stderr ^ " " ^ stdout));
  let data = Utils.read_binfile section_guid in
  Sys.remove section_guid;
  data
  

let pic32_compile filenames only_compile optimization outputname =
  let cflags = " -ffreestanding -march=mips32r2 -msoft-float -Wa,-msoft-float " in
  let (code,stdout,stderr) = 
    USys.shellcmd (gcc ^ cflags ^ (String.concat " " filenames) ^ " " ^
                   (if only_compile then "-c " else "") ^ 
                   (if optimization then "-O3 " else "-O0 ") ^ 
                   "-o " ^ outputname) in
  if code != 0 then raise (Sys_error (stderr ^ " " ^ stdout)) else ()
  
    
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
  
  



let comp_name = "mipsel-pic32-elf"
let objcopy = comp_name ^ "-objcopy"
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
                   (if optimization then "-O0 " else "-O3 ") ^ 
                   "-o " ^ outputname) in
  if code != 0 then raise (Sys_error (stderr ^ " " ^ stdout));
  
    

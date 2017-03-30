
open MipsCfg
open MipsSys
open MipsUtils
open Ustring.Op
open Printf
open Str
open Utest
       
let tmpfile = "temp-file-090916-070704.tmp"

let compile_file filename fname args optimize debug bsconfig =
  (try
     MipsSys.pic32_compile [filename] false optimize tmpfile
   with Sys_error e ->
     e |> eprintf "Error %s");
  let prog = MipsSys.get_program tmpfile |>  MipsUtils.add_branch_symbols in
  let (prog,cfgmap) = MipsCfg.make_cfgmap fname prog in
  let program_code = MipsCfg.pprint_ocaml_cps_from_cfgmap true fname cfgmap prog in
  let stdout = MipsSys.wcet_compile fname false None bsconfig program_code args in
  try
    let regex = Str.regexp "BCET:[^0-9]*\\([0-9]+\\)\\(.\\|\n\\)*WCET:[^0-9]*\\([0-9]+\\)" in
    let _ = Str.search_forward regex stdout 0 in
    let bcet, wcet = int_of_string (Str.matched_group 1 stdout), int_of_string (Str.matched_group 3 stdout) in
    (bcet,wcet)                                                 
  with Not_found ->
       if debug then printf "%s\n%!" stdout;
       (-1,-1)
(*    (eprintf "Error: BCET/WCET Not_Found\n";
     raise Not_found)
 *)
         
let run_test test_file =
  Utest.init "WCET";
  let reg_separator = Str.regexp "|" in
  let arg_separator = Str.regexp ";" in
  let ic = open_in test_file in 
  let _ = input_line ic in
  try
    while true; do
      let line_list = Str.split reg_separator (input_line ic) in
      match line_list with
      | fname::func::args::opt::bsconfig::exp_bcet::exp_wcet::[] ->
         let argslist = Str.split arg_separator (args) in
         let bsconfig =
           match int_of_string (String.trim bsconfig) with
           | (-1) -> None
           |   n  -> Some n
         in
         let opt = int_of_string (String.trim opt) in
         let exp_wcet = int_of_string (String.trim exp_wcet) in
         let exp_bcet = int_of_string (String.trim exp_bcet) in
         let bcet,wcet = compile_file fname func argslist opt true bsconfig in      
         Utest.test_int (sprintf "%s, func=%s, input=[%s], %s=%d" fname func args "BCET" exp_bcet) bcet exp_bcet;
         Utest.test_int (sprintf "%s, func=%s, input=[%s], %s=%d" fname func args "WCET" exp_wcet) wcet exp_wcet;
      | _ -> printf "Wrong format in %s\n%!" test_file;
    done;
  with 
  | End_of_file -> close_in ic;


  Utest.result()

type compOpTypes =
  (* General compiler options used by all commands *)
| OpCSVFile

let bench_options = 
  [(OpCSVFile, Uargs.Str,  us"-csvfile",  us"",
    us"Select .csv file for selecting the tests.");
  ]

let main =
  try
    match Array.to_list Sys.argv with
    | args ->
       (
         let (ops,args) = Uargs.parse args bench_options in
         if Uargs.has_op OpCSVFile ops then
           let test_file = Uargs.str_op OpCSVFile ops |> Ustring.to_utf8 in
           run_test test_file
         else
           printf "No input file\n%!"
       )
  with Uargs.Error s -> uprint_endline s
 

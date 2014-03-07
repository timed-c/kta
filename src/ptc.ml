

open Printf
open Ustring.Op



type  arg_error_type = 
| ArgErrorUnkownFileExtension of ustring

exception Argument_error

type files_record = {
  c : ustring list;
  ll: ustring list;
}



let fold_list lst f = 
  List.rev (List.fold_left f [] lst)

type filename = ustring
type lineno = int
type argno = int
type varid = sid

(* Timing Program Point *)
type tpp = 
|TppEnty
|TppExit 
|TppNode of int



type value = VInt of int * int (* Upper and lower bounds on integers *)

type ta_req = 
| ReqWCP of tpp * tpp
| ReqBCP of tpp * tpp
| ReqLWCET of tpp * tpp
| ReqLBCET of tpp * tpp
| ReqFWCET of tpp * tpp
| ReqFBCET of tpp * tpp

type ta_res = 
| ReqWCP of tpp list
| ReqBCP of tpp list
| ReqLWCET of int
| ReqLBCET of int
| ReqFWCET of int
| ReqFBCET of int



(* Structure to represent a timing analysis request for a specific function *)
type func_ta_req = {
  funcname : ustring;               (* Name of the function that should be analyzed *)
  args : (argno * value) list;      (* Argument assumptions. argno = 0 is first argument *)
  gvars : (sid * value) list;       (* Global variable assumptions *)
  fwcet : (sid * int) list;         (* Assumed WCET in clock cycles for functions *)
  fbcet : (sid * int) list;         (* Assumed BCET in clock cycles for functions *)  
  reqta : (lineno * ta_req) list;  (* Requested timing analysis values *)
}

(* Timing analysis requests within a file *)
type file_ta_req = {
  filename : string;                 (* Name of the file *)
  lines : ustring list;              (* The text of the files represented as a list of lines *)
  ta_requests : func_ta_req list;    (* List of ta requests *)
}






(* Parse text lines *)
let parse_ta_strings lines = 
  (* Removes empty lines and lines with comments. Adds line numbers *)
  let nolines = List.rev (snd (List.fold_left (fun (no,acc) line ->
    let tline = Ustring.trim line in
    if Ustring.length tline = 0 || Ustring.sub tline 0 1 = us"#"
    then (no+1, acc) 
    else (no+1,((no,tline)::acc))) (1,[]) lines)) in
  
  (* Split lines into token lists *)
  let tokenlst = List.map (fun (no,line) ->
    let tokens = Ustring.split line (us" \t") in
    let ftokens = List.filter (fun t -> Ustring.length t <> 0) tokens in
    (no,ftokens)) nolines in
  
  (* Translate all strings to utf8 strings, which we can do pattern matching on *)
  let utf8tokens = List.map (fun (n,ls) -> (n,List.map Ustring.to_utf8 ls)) tokenlst in 
  
  (* Extract timing analysis request data *)
  let rec extract tokens fname args gvars fwcet fbcet reqta acc =
    match tokens with
    | (line_no,["function";name])::ts -> (
         match fname with
         | Some(prename) -> 
           (* We are done with this function. Process next *)
           let new_functa =  {funcname = us prename; args; gvars; fwcet; fbcet; reqta} in
           extract ts (Some(name)) [] [] [] [] [] (new_functa::acc)
         | None -> extract ts (Some(name)) [] [] [] [] [] acc)
    | _ -> acc
  in 
  (* Test code *)
  let tlines = List.map (fun (no,toks) -> 
    let l = Ustring.concat (us",") toks in
    ustring_of_int no ^. us": " ^. l) tokenlst
  in
  uprint_endline (Ustring.concat (us"\n") tlines);
  extract utf8tokens None [] [] [] [] [] [] 






(** Raises exception Sys_error if the file cannot be found. *)
let parse_ta_file filename =
  (* Read file and split into list of lines *)
  let lines = Ustring.split (Ustring.read_file filename) (us"\n") in

  (* Parse requested string and return the request structure for a file *)
  let tareqs = parse_ta_strings lines in
  {filename; lines; ta_requests = tareqs} 


  
  

let read_ta_files filenames =
  let ta = parse_ta_file (List.hd filenames) in
  
  printf "---------------------\n";
  printf "Filename: %s\n" ta.filename
  


let main =
  read_ta_files (List.tl (Array.to_list (Sys.argv)))






  
(** The [files_record] type contains all files that are given on the command line
    to ptc. Note that all file names are given without their extension. *)
  

(*let compile_with_clang files =
  Sys.command ("clang -S -O1 -m32 -emit-llvm " ^ List.hd files ^ ".c -o " ^ List.hd files ^ ".ll")
*)

                         


(*
  let funcs = List.fold_left (fun (facc,lineno) line ->
    
  ) ([], 
*)
  
(*
  let rec plines linelst line_no acc = 
      match linelst with
      | l::ls when Ustring.length l = 0 -> work ls (line_no+1) acc
      | l::ls when Ustring.
  in List.rev (plines lines 1 [])
*)
(*
  { name = filename;
    linelist = lines;
    funclist = funcs;}
*)  
  

(** [make_ta_file fconf res] returns the content of the file as a
    ustring.  The inputs are the file config record [fconf] and the
    [res], the result of the timing analysis. *)
(*
let make_ta_file fileconf ta_res = 
  (* Create a list of requested timing analysis numbers (from different functions). *)
  let req_ta_list = List.fold_left (fun acc func ->
    List.rev_append func.req_ta acc
  ) [] fileconf.funclist in
  (* Concatenate all text lines. If there is an analysis value, append the result to 
     the end of the text line. *)
  List.fold_left (fun (accstr,lineno) linestr ->
    let valstr = (
      try 
        match List.assoc lineno req_ta_list with
        (*| ReqPath -> us" = " ^. Ustring.concat (us" ") (List.map ustring_of_int ta_res.path) *)
        | ta -> us" = " ^. ustring_of_int (List.assoc ta ta_res.ta_vals)
      with 
        Not_found -> us"")
    in 
       (accstr ^. (Ustring.trim_right linestr) ^.valstr ^. us"\n",lineno+1)
  ) (us"",1) fileconf.linelist
*) 
   
   




(*
  type ta_conf =
  | TaFile of ustring * ta_conf
  | TaFunc of ustring * ta_def list * ta_req list
*)

(* Generate a list of valid lines (tuples with line no and string), i.e.,
   empty lines and lines with comments are removed. *)
(*let gen_lines parse_str =
  let rec work linelst line_no acc = 
      match linelst with
      | l::ls when Ustring.length l = 0 -> work ls (line_no+1) acc
      | l::ls when Ustring.
  in List.rev (work (Map.list (Ustring.trim) (Ustring.split str "\n")) 1 [])
*)






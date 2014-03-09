

open Printf
open Ustring.Op


type  arg_error_type = 
| ArgErrorUnkownFileExtension of ustring

exception Argument_error

(* Exception for syntax errors in timing analysis files. The arguments are
   filename and line number for the error. *)
exception TA_file_syntax_error of string * int


(* Structure representing the sorted list of different file arguments 
   that were supplied when executing ptc. *)
type filename_args = {
  c : ustring list;
  ll: ustring list;
}


type filename = ustring
type lineno = int            (* Line number *)
type argno = int             (* Argument number. Used for assumptions of arguments. *)

(* Representing execution time *)
type time = 
| TimeCycles of int     (* Time in clock cycles *)
| TimeInfinity    (* The time is proven to be infinite *)
| TimeUnknown     (* A safe bound can not be computed. This does not, however,
                     mean that such a bound does not exist *)
 
(* Timing Program Point *)
type tpp = 
|TppEntry   
|TppExit 
|TppNode of int

(* Abstract value *)
type value = 
| VInt of int * int (* Upper and lower bounds on integers *)

(* Different forms of timing requests *)
type ta_req = 
| ReqWCP of tpp * tpp   (* Worst-case path request *)
| ReqBCP of tpp * tpp   (* Best-case path request *)
| ReqLWCET of tpp * tpp (* Local worst-case execution time request *)
| ReqLBCET of tpp * tpp (* Local best-case execution time request *)
| ReqFWCET of tpp * tpp (* Fractional worst-case execution time request *)
| ReqFBCET of tpp * tpp (* Fractional best-case execution time request *)

(* Different possible responses to a path request *)
type tpp_path =
| TppPath of tpp list   (* Path represented as a list of timing program points *)
| TppPathInfinity       (* The path is proven to be infinite *)
| TppPathUnknown        (* The path is unknown. Could not be computed *)

(* Different form of timing analysis responses *)
type ta_res = 
| ResWCP of tpp_path  
| ResBCP of tpp_path
| ResLWCET of time
| ResLBCET of time
| ResFWCET of time
| ResFBCET of time

(* Structure to represent a timing analysis request for a specific function *)
type func_ta_req = {
  funcname : ustring;               (* Name of the function that should be analyzed *)
  args : (argno * value) list;      (* Argument assumptions. argno = 0 is first argument *)
  gvars : (sid * value) list;       (* Global variable assumptions *)
  fwcet : (sid * int) list;         (* Assumed WCET in clock cycles for functions *)
  fbcet : (sid * int) list;         (* Assumed BCET in clock cycles for functions *)  
  ta_req : (lineno * ta_req) list;  (* Requested timing analysis values *)
}

(* Timing analysis requests within a file *)
type file_ta_req = {
  filename : string;                 (* Name of the file *)
  lines : ustring list;              (* The text of the files represented as a list of lines *)
  func_ta_reqs : func_ta_req list;    (* List of ta requests *)
}

(** Pretty print an abstract value *)
let pprint_abstract_value v =
  match v with
  | VInt(i1,i2) when i1 = i2 -> ustring_of_int i1
  | VInt(i1,i2) -> ustring_of_int i1 ^. us".." ^. ustring_of_int i2


(** Pretty print the timing request of a function *)
let pprint_func_ta_req func_ta_req =
  (* Function *)
  us"function " ^. func_ta_req.funcname  ^. us"\n" ^.

  (* Arguments *)
  (if List.length func_ta_req.args = 0 then us"" else 
    Ustring.concat (us"\n") 
      (List.map (fun (argno,v) -> us"arg " ^. ustring_of_int argno ^. us" " ^.
                 pprint_abstract_value v) func_ta_req.args) ^. us"\n") 

  

(** Pretty print the content of a timing analysis file *)
let pprint_file_ta_req file_ta_req = 
  us"Filename: " ^. us(file_ta_req.filename) ^. us"\n" ^.
  us"Number of function requests: " ^. 
     ustring_of_int (List.length (file_ta_req.func_ta_reqs)) ^. us"\n" ^.
  Ustring.concat (us"----\n") (List.map pprint_func_ta_req (file_ta_req.func_ta_reqs)) ^.
  us"\n"

(* Returns the pair of timing program points from a timing request *)
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
  let tpps = List.fold_left (fun acc (_,tareq) -> 
    let (t1,t2) = get_req_tpp tareq in t1::t2::acc 
  ) [] (func_ta_req.ta_req)
  in
  
  (* Remove all duplicates *)
  let tpps_distinct = list_remove_duplicates tpp_compare tpps in

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
            tpp_compare x t1 >= 0 && tpp_compare x t2 <= 0) tpps_distinct))
    in
    let get_ta_time t1 t2 wrong_order =
      let cv = tpp_compare t1 t2 in
      if cv = 0 then TimeCycles(0) else if cv > 0 then wrong_order else 
      TimeCycles(List.assoc t2 tpp_times - List.assoc t1 tpp_times)
    in                                                               
    match tareq with
    | ReqWCP(t1,t2)  -> ResWCP(find_path t1 t2)::acc
    | ReqBCP(t1,t2) ->  ResBCP(find_path t1 t2)::acc
    | ReqLWCET(t1,t2) -> ResLWCET(get_ta_time t1 t2 TimeInfinity)::acc
    | ReqLBCET(t1,t2) -> ResLBCET(get_ta_time t1 t2 TimeInfinity)::acc
    | ReqFWCET(t1,t2) -> ResFWCET(get_ta_time t1 t2 (TimeCycles(0)))::acc
    | ReqFBCET(t1,t2) -> ResFBCET(get_ta_time t1 t2 (TimeCycles(0)))::acc
  ) [] func_ta_req.ta_req)



                      
(* Parse a value. Raise exception TA_file_syntax_error if the kind of value is
   unknown. Right now, we only supports integer values (no intervals) *)
let parse_abstract_value filename line_no value = 
  let v = try int_of_string value 
    with _ -> raise (TA_file_syntax_error(filename,line_no)) in
  VInt(v,v)
    

  
  
(* Parses a positive integer and raises TA_file_syntax_erro if something is wrong. *)
let parse_positive_int filename line_no value  = 
  let v = try int_of_string value 
     with _ -> raise (TA_file_syntax_error(filename,line_no)) in
  if v < 0 then raise (TA_file_syntax_error(filename,line_no))
  else v




(* Parse text lines. Raises TA_file_syntax_error if there are any 
   errors in the ta file *)
let parse_ta_strings filename lines = 
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
  let rec extract tokens fname args gvars fwcet fbcet ta_req acc =
    match tokens with
    | (_,["function";name])::ts -> (
        match fname with
        | Some(prename) ->
        (* We are done with this function. Process next *)
          let func =  {funcname = us prename; args = List.rev args; 
                       gvars = List.rev gvars; fwcet = List.rev fwcet; 
                       fbcet = List.rev fbcet; ta_req = List.rev ta_req} in
          extract ts (Some(name)) [] [] [] [] [] (func::acc)
        | None -> 
             (* We have detected a new function (the first one) *)
          extract ts (Some(name)) [] [] [] [] [] acc)
    | (lineno,["arg";pos;value])::ts -> (
        let pos' = parse_positive_int filename lineno pos in
        let value' = parse_abstract_value filename lineno value in 
        extract ts fname ((pos',value')::args) gvars fwcet fbcet ta_req acc)
    | [] -> (
        match fname with
        | Some(prename) -> 
          let func = {funcname = us prename; args = List.rev args; 
                       gvars = List.rev gvars; fwcet = List.rev fwcet; 
                       fbcet = List.rev fbcet; ta_req = List.rev ta_req} in
          func::acc
        | None -> acc)
    | _::ts -> extract ts fname args gvars fwcet fbcet ta_req acc
  in 
  (* Test code *)
  let tlines = List.map (fun (no,toks) -> 
    let l = Ustring.concat (us",") toks in
    ustring_of_int no ^. us": " ^. l) tokenlst
  in
  uprint_endline (Ustring.concat (us"\n") tlines);
  List.rev (extract utf8tokens None [] [] [] [] [] [])





(** Raises exception Sys_error if the file cannot be found. *)
let parse_ta_file filename =
  (* Read file and split into list of lines *)
  let lines = Ustring.split (Ustring.read_file filename) (us"\n") in

  (* Parse requested string and return the request structure for a file *)
  let func_ta_reqs = parse_ta_strings filename lines in
  {filename; lines; func_ta_reqs} 



type error_level = Error | Warning
  

(** Format and print an error message to the standard output *)
let print_error filename line column level message =
  uprint_endline (us filename ^. us":" ^. ustring_of_int line ^. 
      us":" ^. ustring_of_int column ^. us" " ^.
      us (match level with Error -> "error: " | Warning -> "warning: ") ^.
      us message)




let main =
  let filenames =  (List.tl (Array.to_list (Sys.argv))) in
  let file_ta_req = 
    try List.map parse_ta_file filenames
    with 
    | TA_file_syntax_error(filename,line) -> 
        print_error filename line 0 Error "Syntax error in timing analysis file.";
        exit 1
  in
  uprint_endline (Ustring.concat (us"") 
    (List.map (fun x -> us"=============\n" ^. pprint_file_ta_req x) file_ta_req))

  







  
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






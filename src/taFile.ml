

open Printf
open Ustring.Op
open TaFileTypes
open AInt32


(* ---------------------------------------------------------------------*)
(** Pretty print a timing program point *)
let pprint_tpp tpp = ustring_of_sid tpp



(* ---------------------------------------------------------------------*)
(** Pretty print a path between timing program points *)
let pprint_path path =
  match path with  
  | TppPath(tpps) -> Ustring.concat (us",") (List.map pprint_tpp tpps)
  | TppPathUnknown -> us"unknown"

   


(* ---------------------------------------------------------------------*)
(** Pretty print a time value *)
let pprint_time_value v = 
  match v with
  | TimeCycles(i) -> ustring_of_int i
  | TimeUnknown -> us"unknown"



(* ---------------------------------------------------------------------*)
(** Pretty print an abstract value *)
let pprint_abstract_value v =
  match v with
  | VInt(i1,i2) when i1 = i2 -> ustring_of_int i1
  | VInt(i1,i2) -> ustring_of_int i1 ^. us".." ^. ustring_of_int i2


(* ---------------------------------------------------------------------*)
let pprint_req_name req = 
  match req with 
  | ReqWCP(_,_) -> us"WCP"
  | ReqBCP(_,_) -> us"BCP"
  | ReqLWCET(_,_) -> us"LWCET"
  | ReqLBCET(_,_) -> us"LBCET"
  | ReqFWCET(_,_) -> us"FWCET"
  | ReqFBCET(_,_) -> us"FBCET"


(* ---------------------------------------------------------------------*)
(* Returns the pair of timing program points from a timing request *)
let get_req_tpp req = 
  match req with ReqWCP(t1,t2)  | ReqBCP(t1,t2) | ReqLWCET(t1,t2) | ReqLBCET(t1,t2) |
                 ReqFWCET(t1,t2) | ReqFBCET(t1,t2) -> (t1,t2)



(* ---------------------------------------------------------------------*)
(** Pretty print the timing request of a function *)
let pprint_func_ta_req func_ta_req =
  (* Function *)
  us"Function " ^. func_ta_req.funcname  ^. us"\n" ^.

  (* Arguments *)
  (if List.length func_ta_req.args = 0 then us"" else 
    Ustring.concat (us"\n") 
      (List.map (fun (argno,v) -> us"arg " ^. ustring_of_int argno ^. us" " ^.
                 pprint_abstract_value v) func_ta_req.args) ^. us"\n") ^.

  (* Global variables *)
  (if List.length func_ta_req.gvars = 0 then us"" else 
    Ustring.concat (us"\n") 
      (List.map (fun (x,v) -> us"GlobalVar " ^. ustring_of_sid x ^. us" " ^.
                 pprint_abstract_value v) func_ta_req.gvars) ^. us"\n") ^. 

  (* Function WCET assumptions *)
  (if List.length func_ta_req.fwcet = 0 then us"" else 
    Ustring.concat (us"\n") 
      (List.map (fun (x,v) -> us"FunctionWCET " ^. ustring_of_sid x ^. us" " ^.
                 pprint_time_value v) func_ta_req.fwcet) ^. us"\n")  ^.

  (* Function BCET assumptions *)
  (if List.length func_ta_req.fbcet = 0 then us"" else 
    Ustring.concat (us"\n") 
      (List.map (fun (x,v) -> us"FunctionBCET " ^. ustring_of_sid x ^. us" " ^.
                 pprint_time_value v) func_ta_req.fbcet) ^. us"\n") ^.
  
  (* Timing requests *)
  (if List.length func_ta_req.ta_req = 0 then us"" else 
    Ustring.concat (us"\n") 
      (List.map (fun (_,req) -> 
                 let (tpp1,tpp2) = get_req_tpp req in
                 pprint_req_name req ^. us" " ^.
                 pprint_tpp tpp1 ^. us" " ^. pprint_tpp tpp2) 
                 func_ta_req.ta_req) ^. us"\n") 




(* ---------------------------------------------------------------------*)
(* see .mli *)
let pprint_simple_ta_res ta_res =
  List.fold_left (fun acc res ->
    (match res with 
     | ResWCP(path) -> acc ^. pprint_path path  
     | ResBCP(path) -> acc ^. pprint_path path
     | ResLWCET(t) | ResLBCET(t) | ResFWCET(t) | ResFBCET(t) 
       -> acc ^. pprint_time_value t) ^. us"\n"
  ) (us"") ta_res



  
(* ---------------------------------------------------------------------*)
(* see .mli *)
let pprint_full_ta_res func_ta_req ta_res = us"Full!"



  
(* ---------------------------------------------------------------------*)
(** Pretty print the content of a timing analysis file *)
let pprint_file_ta_req file_ta_req = 
  us"Filename: " ^. us(file_ta_req.ta_filename) ^. us"\n" ^.
  us"Number of function requests: " ^. 
     ustring_of_int (List.length (file_ta_req.func_ta_reqs)) ^. us"\n" ^.
  Ustring.concat (us"----\n") (List.map pprint_func_ta_req (file_ta_req.func_ta_reqs)) ^.
  us"\n"




  
(* ---------------------------------------------------------------------*)
(* Parses a positive integer and raises TA_file_syntax_erro if something is wrong. *)
let parse_positive_int filename line_no value  = 
  let v = try int_of_string value 
     with _ -> raise (TA_file_syntax_error(filename,line_no)) in
  if v < 0 then raise (TA_file_syntax_error(filename,line_no))
  else v



    
(* ---------------------------------------------------------------------*)
(** Parse a timing program point *)
let parse_tpp filename line_no str = usid str



    
(* ---------------------------------------------------------------------*)
(* Parse an literal to to a time value *)
let parse_time_literal filename line_no str =
  let v = try int_of_string str
    with _ -> raise (TA_file_syntax_error(filename,line_no)) in
  TimeCycles(v)


                      
(* ---------------------------------------------------------------------*)
(* Parse a value. Raise exception TA_file_syntax_error if the kind of value is
   unknown. Right now, we only supports integer values (no intervals) *)
let parse_abstract_value filename line_no value =
  (* Is concrete integer *)
  try let v = int_of_string value in VInt(v,v)

  (* Is integer interval? *)
  with _ -> 
  (try
     match Str.split (Str.regexp "\\.\\.") value with
     | [l;u] ->
         VInt(int_of_string l, int_of_string u)
   | _ -> raise (TA_file_syntax_error(filename,line_no))
  with _ ->
    raise (TA_file_syntax_error(filename,line_no)))
  

   

(* ---------------------------------------------------------------------*)
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
    | (_,["Function";name])::ts -> (
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
    | (lineno,["Arg";pos;value])::ts -> (
        let pos' = parse_positive_int filename lineno pos in
        let value' = parse_abstract_value filename lineno value in 
        extract ts fname ((pos',value')::args) gvars fwcet fbcet ta_req acc)
    | (lineno,["GlobalVar";var;value])::ts -> (
        let value' = parse_abstract_value filename lineno value in 
        extract ts fname args (((usid var),value')::gvars) fwcet fbcet ta_req acc)
    | (lineno,["FunctionWCET";var;time])::ts -> (
        let tval = parse_time_literal filename lineno time in 
        extract ts fname args gvars (((usid var),tval)::fwcet) fbcet ta_req acc)        
    | (lineno,["FunctionBCET";var;time])::ts -> (
        let tval = parse_time_literal filename lineno time in 
        extract ts fname args gvars fwcet (((usid var),tval)::fbcet) ta_req acc)        
    | (lineno,["WCP";tpp1;tpp2])::ts -> (
        let mktpp = parse_tpp filename lineno in
        let newreq = (lineno,ReqWCP(mktpp tpp1,mktpp tpp2)) in
        extract ts fname args gvars fwcet fbcet (newreq::ta_req) acc)
    | (lineno,["BCP";tpp1;tpp2])::ts -> (
        let mktpp = parse_tpp filename lineno in
        let newreq = (lineno,ReqBCP(mktpp tpp1,mktpp tpp2)) in
        extract ts fname args gvars fwcet fbcet (newreq::ta_req) acc)
    | (lineno,["LWCET";tpp1;tpp2])::ts -> (
        let mktpp = parse_tpp filename lineno in
        let newreq = (lineno,ReqLWCET(mktpp tpp1,mktpp tpp2)) in
        extract ts fname args gvars fwcet fbcet (newreq::ta_req) acc)
    | (lineno,["LBCET";tpp1;tpp2])::ts -> (
        let mktpp = parse_tpp filename lineno in
        let newreq = (lineno,ReqLBCET(mktpp tpp1,mktpp tpp2)) in
        extract ts fname args gvars fwcet fbcet (newreq::ta_req) acc)
    | (lineno,["FWCET";tpp1;tpp2])::ts -> (
        let mktpp = parse_tpp filename lineno in
        let newreq = (lineno,ReqFWCET(mktpp tpp1,mktpp tpp2)) in
        extract ts fname args gvars fwcet fbcet (newreq::ta_req) acc)
    | (lineno,["FBCET";tpp1;tpp2])::ts -> (
        let mktpp = parse_tpp filename lineno in
        let newreq = (lineno,ReqFBCET(mktpp tpp1,mktpp tpp2)) in
        extract ts fname args gvars fwcet fbcet (newreq::ta_req) acc)
    | (lineno,_)::ts -> raise (TA_file_syntax_error(filename,lineno))
    | [] -> (
        match fname with
        | Some(prename) -> 
          let func = {funcname = us prename; args = List.rev args; 
                       gvars = List.rev gvars; fwcet = List.rev fwcet; 
                       fbcet = List.rev fbcet; ta_req = List.rev ta_req} in
          func::acc
        | None -> acc)
  in 
  List.rev (extract utf8tokens None [] [] [] [] [] [])


(* ---------------------------------------------------------------------*)
let val_to_aint32 value =
  match value with
  | VInt(l,u) -> AInt32.make_intervals [(l,u)]


(* ---------------------------------------------------------------------*)
let to_args_list lst =
  let rec insert lst k no arg =
    match lst with
    | a::al -> if k = no then arg::al else a::(insert al (k+1) no arg)
    | [] -> if k = no then [arg] else abstractval::(insert [] (k+1) no arg)        
  in
  List.fold_left (fun a (no,v) -> insert a 0 no (val_to_aint32 v)) [] lst

  

(* ---------------------------------------------------------------------*)
let parse_ta_file ta_filename =
  (* Read file and split into list of lines *)
  let lines = 
      Ustring.split (Ustring.read_file ta_filename) (us"\n") 
  in
    
  (* Parse requested string and return the request structure for a file *)
  let func_ta_reqs = parse_ta_strings ta_filename lines in
  {ta_filename; lines; func_ta_reqs} 

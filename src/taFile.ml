


open Ustring.Op
open TaFileTypes


  
(* ---------------------------------------------------------------------*)
(* Parses a positive integer and raises TA_file_syntax_erro if something is wrong. *)
let parse_positive_int filename line_no value  = 
  let v = try int_of_string value 
     with _ -> raise (TA_file_syntax_error(filename,line_no)) in
  if v < 0 then raise (TA_file_syntax_error(filename,line_no))
  else v



    
(* ---------------------------------------------------------------------*)
(** Parse a timing program point *)
let parse_tpp filename line_no str =
  match str with 
  | "entry" -> TppEntry
  | "exit" -> TppExit
  | _ -> TppNode(parse_positive_int filename line_no str)


    
(* ---------------------------------------------------------------------*)
(* Parse an literal to to a time value *)
let parse_time_literal filename line_no str =
  if str = "infinity" then TimeInfinity else
  let v = try int_of_string str
    with _ -> raise (TA_file_syntax_error(filename,line_no)) in
  TimeCycles(v)


                      
(* ---------------------------------------------------------------------*)
(* Parse a value. Raise exception TA_file_syntax_error if the kind of value is
   unknown. Right now, we only supports integer values (no intervals) *)
let parse_abstract_value filename line_no value = 
  let v = try int_of_string value 
    with _ -> raise (TA_file_syntax_error(filename,line_no)) in
  VInt(v,v)



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
    | (lineno,["globalvar";var;value])::ts -> (
        let value' = parse_abstract_value filename lineno value in 
        extract ts fname args (((usid var),value')::gvars) fwcet fbcet ta_req acc)
    | (lineno,["funcWCET";var;time])::ts -> (
        let tval = parse_time_literal filename lineno time in 
        extract ts fname args gvars (((usid var),tval)::fwcet) fbcet ta_req acc)        
    | (lineno,["funcBCET";var;time])::ts -> (
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
let parse_ta_file ta_filename =
  (* Read file and split into list of lines *)
  let lines = 
      Ustring.split (Ustring.read_file ta_filename) (us"\n") 
  in
    
  (* Parse requested string and return the request structure for a file *)
  let func_ta_reqs = parse_ta_strings ta_filename lines in
  {ta_filename; lines; func_ta_reqs} 

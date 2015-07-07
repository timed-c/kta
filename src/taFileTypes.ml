
open Ustring.Op

type lineno = int                (* Line number *)
type argno = int                 (* Argument number. Used for assumptions of arguments. *)

(* Representing execution time *)
type time = 
| TimeCycles of int              (* Time in clock cycles *)
| TimeInfinity                   (* The time is proven to be infinite *)
| TimeUnknown                    (* A safe bound can not be computed. This does not, 
                                   however, mean that such a bound does not exist *)
 
(* Timing Program Point *)
type tpp = 
|TppEntry   
|TppExit 
|TppNode of int

(* Abstract value *)
type value = 
| VInt of int * int                (* Upper and lower bounds on integers *)

(* Different forms of timing requests *)
type ta_req = 
| ReqWCP of tpp * tpp              (* Worst-case path request *)
| ReqBCP of tpp * tpp              (* Best-case path request *)
| ReqLWCET of tpp * tpp            (* Local worst-case execution time request *)
| ReqLBCET of tpp * tpp            (* Local best-case execution time request *)
| ReqFWCET of tpp * tpp            (* Fractional worst-case execution time request *)
| ReqFBCET of tpp * tpp            (* Fractional best-case execution time request *)

(* Different possible responses to a path request *)
type tpp_path =
| TppPath of tpp list              (* Path represented as a list of timing program points *)
| TppPathInfinity                  (* The path is proven to be infinite *)
| TppPathUnknown                   (* The path is unknown. Could not be computed *)

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
  fwcet : (sid * time) list;        (* Assumed WCET in clock cycles for functions *)
  fbcet : (sid * time) list;        (* Assumed BCET in clock cycles for functions *)  
  ta_req : (lineno * ta_req) list;  (* Requested timing analysis values *)
}


(* Timing analysis requests within a file *)
type file_ta_req = {
  ta_filename : string;              (* Name of the file *)
  lines : ustring list;              (* The text of the files represented as a list of lines *)
  func_ta_reqs : func_ta_req list;   (* List of ta requests *)
}


(* Exception for syntax errors in timing analysis files. The arguments are
   filename and line number for the error. *)
exception TA_file_syntax_error of string * int






    

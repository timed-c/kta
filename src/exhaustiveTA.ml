(* 
Copyright (c) 2015, David Broman
All rights reserved.

Redistribution and use in source and binary forms, with or without 
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright 
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright notice, 
      this list of conditions and the following disclaimer in the 
      documentation and/or other materials provided with the distribution.
    * Neither the name of the Linköping University nor the names of its 
      contributors may be used to endorse or promote products derived from 
      this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE 
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL 
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON 
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS 
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

open Ustring.Op
open TaFileTypes
open Printf

type clock_cycles = int
type total_clock_cycles = int

type tpp_timed_path =
| TppTimedPath of total_clock_cycles * ((sid * clock_cycles) list)  (* Timed path *)
| TppTimedPathUnknown                            (* The path is unknown. Could not be computed *)

type assumed_func_timing = (sid * time) list    
  
type timed_eval_func = string -> int32 list -> (int * int32) list ->
                (sid * time) list -> (sid * time) list -> tpp_timed_path
(** [timed_eval_func funcname args meminitmap func_wcet func_bcet]. The [meminitmap] is
    an assoicative list, where the keys are addresses and the values are the
    memory values at these positions. *)

  
type timing_info = {
   wcpath : tpp_timed_path;   (* Overall worst-case path *)
   bcpath : tpp_timed_path;   (* Overall best-case path *)
   tcount : int;
} 
(** Internal timing info structure that is used by the analyze() function
    when computing timing responses. *)
  
let tpp_entry = usid "entry"
let tpp_exit = usid "exit"

(* ---------------------------------------------------------------------*)
let timed_eval_func funcname args mem_init_map func_wcet func_bcet =
  TppTimedPathUnknown




(* ---------------------------------------------------------------------*)
(* Internal. Computes the fractional time from a timed path *)
let frac_time_from_path tpp1 tpp2 c prepath = 
  let path = (tpp_entry,0)::(List.append prepath [(tpp_exit,c)]) in
  try 
    let start_time = List.assoc tpp1 path in
    let end_time = List.assoc tpp2 path in
    if end_time > start_time then TimeCycles(end_time - start_time)      
    else TimeCycles(0)
  with 
    Not_found -> TimeCycles(0)

 
     
(* ---------------------------------------------------------------------*)
let analyze evalfunc func_ta_req symtbl = 
  (* Extract out global variable assumptions *)  
  let (first,addrint) = 
    List.split (List.map (fun (id,VInt(l,u)) -> (l,(id,l,u))) func_ta_req.gvars) in

  (* Get the actual function name *)
  let name = Ustring.to_utf8 func_ta_req.funcname in

  (* Exhaustively explore all possible input combinations *)
  let rec explore lst cur memmap tinfo =
    match lst,cur with
    | (id,l,u)::lres, c::cres -> ( 
        let tinfo' = explore lres cres ((symtbl id,Int32.of_int c)::memmap) tinfo in
        if c = u then tinfo'
        else explore lst ((c+1)::cres) memmap tinfo')
    | [],[] -> (
        (* Perform the analysis by executing one configuration.
           For MIPS, this function is defined in mipsSys.ml *)
        match evalfunc name [] memmap [] [] with
        (*** Return a new update tinfo record in the case of a valid evaluation  *)
        | TppTimedPath(cycles,timedpath) as newpath ->           
          
           (* (*Print out information about all program points  *)
           printf "-----------\n";
           List.iter (fun (s,c) -> 
              uprint_endline ((ustring_of_sid s) ^. us": " ^. ustring_of_int c)) 
              timedpath;
           printf "final: %d\n" cycles;
           *)

           {
           (* wcpath field *)
            wcpath = (
              match tinfo.wcpath with
              | TppTimedPath(curr_cycles,curr_timedpath) ->
                if cycles > curr_cycles then newpath else tinfo.wcpath
              | TppTimedPathUnknown -> TppTimedPathUnknown);             
               
           (* bcpath field *)
             bcpath = (
               match tinfo.bcpath with
               | TppTimedPath(curr_cycles,curr_timedpath) ->
                 if cycles < curr_cycles then newpath else tinfo.bcpath
               | TppTimedPathUnknown -> TppTimedPathUnknown);             
             
           (* tocunt field: Update the test counter. *)
             tcount = tinfo.tcount + 1;
           } 
                    
        (*** Return a new update tinfo record in the case of a invalid evaluation  *)
        | TppTimedPathUnknown  -> 
           {
           (* wcpath field *)
            wcpath = TppTimedPathUnknown;
               
           (* bcpath field *)
             bcpath = TppTimedPathUnknown;
             
           (* tocunt field: Update the test counter. Should be removed. *)
             tcount = tinfo.tcount + 1;
           } 
    )
    | _,_ -> failwith "should not happen."
  in
    
  (* Init the timing info that is passed around for calculating the response *)
  let init_timing_info = {
    wcpath = TppTimedPath(0,[]);
    bcpath = TppTimedPath(max_int,[]);    
    tcount = 0;
  } 
  in

  (* Start to explore all paths, calling the function above *)
  let tinfo = explore addrint first [] init_timing_info in
  
  (* Inform about explored paths.  *)
  printf "\nExhaustive search explored %d program paths.\n" tinfo.tcount;

  (* Generate the ta responses by iterating over ta requests *)
  List.fold_left (fun accresp (_,ta_req) ->
    match ta_req with
    | ReqWCP(tpp1,tpp2) -> failwith "Not yet implemented"
    | ReqBCP(tpp1,tpp2) -> failwith "Not yet implemented"
    | ReqLWCET(tpp1,tpp2) -> failwith "Not yet implemented"
    | ReqLBCET(tpp1,tpp2) -> failwith "Not yet implemented"

      (* Compute fractional WCET *)
    | ReqFWCET(tpp1,tpp2) -> (
        match tinfo.wcpath with
        | TppTimedPath(c,prepath) -> 
             ResFWCET(frac_time_from_path tpp1 tpp2 c prepath)::accresp
        | TppTimedPathUnknown -> ResFWCET(TimeUnknown)::accresp)

      (* Compute fractional BCET *)
    | ReqFBCET(tpp1,tpp2) -> (
        match tinfo.bcpath with
        | TppTimedPath(c,prepath) -> 
             ResFBCET(frac_time_from_path tpp1 tpp2 c prepath)::accresp
        | TppTimedPathUnknown -> ResFBCET(TimeUnknown)::accresp)

  ) [] func_ta_req.ta_req |> List.rev
 
  














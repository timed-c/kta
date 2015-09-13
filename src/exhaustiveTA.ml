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

type wc_clock_cycles = int
type bc_clock_cycles = int

type tpp_timed_path =
| TppTimedPath of wc_clock_cycles * 
                  bc_clock_cycles *
                  ((sid * (wc_clock_cycles * bc_clock_cycles)) list)
| TppTimedPathUnknown                          

type assumed_func_timing = (sid * time) list    
  
type timed_eval_func = string -> int32 list -> (int * int32) list ->
                (sid * time) list -> (sid * time) list -> (string -> (int * int))
                -> tpp_timed_path
(** [timed_eval_func funcname args meminitmap func_wcet func_bcet]. The [meminitmap] is
    an assoicative list, where the keys are addresses and the values are the
    memory values at these positions. *)

  
type timing_info = {
   wcpath : tpp_timed_path;           (* Overall worst-case path *)
   bcpath : tpp_timed_path;           (* Overall best-case path *)
   lwcet  : ((sid * sid) * int) list; (* Local WCET list *)  
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
let frac_time_from_path tpp1 tpp2 wc_c bc_c prepath = 
  let path = (tpp_entry,(0,0))::(List.append prepath [(tpp_exit),(wc_c,bc_c)]) in
  try 
    let (wc_start_time,bc_start_time) = List.assoc tpp1 path in
    let (wc_end_time,bc_end_time) = List.assoc tpp2 path in
    ((if wc_end_time > wc_start_time then TimeCycles(wc_end_time - wc_start_time)      
     else TimeCycles(0)),
     (if bc_end_time > bc_start_time then TimeCycles(bc_end_time - bc_start_time)      
     else TimeCycles(0)))  
  with 
    Not_found -> (TimeCycles(0),TimeCycles(0))

 
     
(* ---------------------------------------------------------------------*)
let analyze evalfunc func_ta_req symtbl = 
  (* Extract out global variable assumptions *)  
  let (first,addrint) = 
    List.split (List.map (fun (id,VInt(l,u)) -> (l,(id,l,u))) func_ta_req.gvars) in

  (* Get the actual function name *)
  let name = Ustring.to_utf8 func_ta_req.funcname in

  (* Create a map from global assumptions to timing tuple (wcet,bcet) *)
  let fmap = List.map (fun (sid,wcet) ->
               (ustring_of_sid sid |> Ustring.to_utf8,(wcet,0))) func_ta_req.fwcet in
  let func_assumptions s = List.assoc s fmap  in

  (* TODO: implement support for function assumptons for BCET. 
           See above. *)
  if (List.length func_ta_req.fbcet) <> 0 
  then failwith "Assumptions for Function BCET is not yet supported.";

  (* Create lwcet init list *)
  let lwcet_list = List.fold_left (fun a (_,ta_req) ->
     match ta_req with
     | ReqLWCET(tpp1,tpp2) -> ((tpp1,tpp2),0)::a
     | _ -> a
  ) [] func_ta_req.ta_req |> List.rev in


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
        match evalfunc name [] memmap [] [] func_assumptions with
        (*** Return a new update tinfo record in the case of a valid evaluation  *)
        | TppTimedPath(wc_cycles,bc_cycles,timedpath) as newpath ->           
          
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
              | TppTimedPath(curr_wc_cycles,_,curr_timedpath) ->
                if wc_cycles > curr_wc_cycles then newpath else tinfo.wcpath
              | TppTimedPathUnknown -> TppTimedPathUnknown);             
               
           (* bcpath field *)
             bcpath = (
               match tinfo.bcpath with
               | TppTimedPath(_,curr_bc_cycles,curr_timedpath) ->
                 if bc_cycles < curr_bc_cycles then newpath else tinfo.bcpath
               | TppTimedPathUnknown -> TppTimedPathUnknown);             

           (* lwcet field *)
             lwcet = (
               let path = (tpp_entry,(0,0))::(List.append timedpath 
                                             [(tpp_exit),(wc_cycles,bc_cycles)]) in
               List.map (fun ((tp1,tp2),c) ->
                 try
                   let (wc_start,_) = List.assoc tp1 path in
                   let (wc_end,_) = List.assoc tp2 path in
                   let wc_diff = wc_end - wc_start in
                   if wc_diff >= 0 && wc_diff > c then ((tp1,tp2),wc_diff)
                   else ((tp1,tp2),c)
                 with Not_found -> ((tp1,tp2),c))
                tinfo.lwcet
             );
             
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

           (* lwcet field *)
             lwcet = []; 
             
           (* tocunt field: Update the test counter. Should be removed. *)
             tcount = tinfo.tcount + 1;
           } 
    )
    | _,_ -> failwith "should not happen."
  in
    
  (* Init the timing info that is passed around for calculating the response *)
  let init_timing_info = {
    wcpath = TppTimedPath(0,max_int,[]);
    bcpath = TppTimedPath(0,max_int,[]);    
    lwcet = lwcet_list;
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
    | ReqLWCET(tpp1,tpp2) ->       
        if List.length tinfo.lwcet = 0 then 
          (* Time is unknown because the list is empty. *)
          ResLWCET(TimeUnknown)::accresp 
        else (  
          (* See if we find the pair of tpps *)
          let cycles = try List.assoc (tpp1,tpp2) tinfo.lwcet 
                       with Not_found -> 0 in
          ResLWCET(TimeCycles(cycles))::accresp
        )
      
    | ReqLBCET(tpp1,tpp2) -> failwith "Not yet implemented"

      (* Compute fractional WCET *)
    | ReqFWCET(tpp1,tpp2) -> (
        match tinfo.wcpath with
        | TppTimedPath(wc_c,bc_c,prepath) -> 
             let (wc_time,_) = frac_time_from_path tpp1 tpp2 wc_c bc_c prepath in
             ResFWCET(wc_time)::accresp
        | TppTimedPathUnknown -> ResFWCET(TimeUnknown)::accresp)

      (* Compute fractional BCET *)
    | ReqFBCET(tpp1,tpp2) -> (
        match tinfo.bcpath with
        | TppTimedPath(wc_c,bc_c,prepath) -> 
             let (_,bc_time) = frac_time_from_path tpp1 tpp2 wc_c bc_c prepath in
             ResFBCET(bc_time)::accresp
        | TppTimedPathUnknown -> ResFBCET(TimeUnknown)::accresp)

  ) [] func_ta_req.ta_req |> List.rev
 
  














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

type clock_cycles = int

type tpp_timed_path =
| TppTimedPath of (clock_cycles * tpp) list      (* Timed path *)
| TppTimedPathUnknown                            (* The path is unknown. Could not be computed *)

type assumed_func_timing = (sid * time) list    
  
type timed_eval_func = string -> int32 list -> (int * int32) list ->
                (sid * time) list -> (sid * time) list -> tpp_timed_path
(** [timed_eval_func funcname args meminitmap func_wcet func_bcet]. The [meminitmap] is
    an assoicative list, where the keys are addresses and the values are the
    memory values at these positions. *)
  

    

(* ---------------------------------------------------------------------*)
let timed_eval_func funcname args mem_init_map func_wcet func_bcet =
  TppTimedPathUnknown

    
(* ---------------------------------------------------------------------*)
let analyze evalfunc func_ta_req = 
  
  let name = Ustring.to_utf8 func_ta_req.funcname in
  match evalfunc name [] [] [] [] with
  | TppTimedPath timedpath -> []
  | TppTimedPathUnknown  -> []


(*
type timed_eval_func = string -> int32 list -> (int * int32) list ->
                (sid * time) list -> (sid * time) list -> tpp_timed_path
(** [timed_eval_func funcname args meminitmap func_wcet func_bcet]. The [meminitmap] *)

val analyze : timed_eval_func -> TaFileTypes.func_ta_req -> TaFileTypes.ta_res list
(** [analyze evalfunc func_ta_req] *)

type tpp_timed_path =
| TppTimedPath of (clock_cycles * tpp) list      (* Timed path *)
| TppTimedPathUnknown                            (* The path is unknown. Could not be computed *)
*)



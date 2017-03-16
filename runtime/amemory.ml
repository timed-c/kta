open Aint32congruence  
  
open Printf
open Config


(* If true, the memory will not set anything and all memory requests will
   return Any *)
let nomem = false
  
module Mem = Map.Make(
  struct type t = int
  let compare = compare
end)

type amemory = {
  memory : aint32 Mem.t;
  mjoins : amemory list;
}

let mem_init = {
  memory = Mem.empty;
  mjoins = [];
}
  
(** Returns mem', the updated abstract memory *)
let set_memval addr v mem =
  if nomem then mem
  else {mem with memory = Mem.add addr v mem.memory}

    
(** Returns a tuple, (mem',val), where mem' is the updated
    abstract memory *)
let get_memval addr mem =
  if nomem then (mem, aint32_any)
  else
    let rec getmem_internal addr mem =      
      try Mem.find addr mem.memory
      with Not_found ->
           match mem.mjoins with
           | [] -> aint32_any
           | m::rest ->
              List.fold_left
                (fun v1 mem ->
                  aint32_join v1 (getmem_internal addr mem))
                (getmem_internal addr m) rest
    in
    let v = getmem_internal addr mem in
    (set_memval addr v mem, v)

      
let get_memval_byte addr mem byte =
  let m,v = get_memval addr mem in
  (m, aint32_mem_byte byte false v)

let get_memval_hword addr mem hword =
  let m,v = get_memval addr mem in
  (m, aint32_any)

let set_memval_byte addr v mem byte =
  let m,oldv = get_memval addr mem in
  let newv = aint32_mem_update_byte byte v oldv in
  set_memval addr newv mem

let set_memval_hword addr v mem hword =
  (*let m,oldv = get_memval addr mem in*)
  let newv = aint32_any in
  set_memval addr newv mem

let mem_join memlist =
  (*  {mem_init with mjoins = memlist}*)
  match memlist with
  | [] ->   {mem_init with mjoins = []}
  | m::ms -> 
     List.fold_left
       (fun m1 m2 ->
         { memory =
             Mem.merge
               (fun addr v1 v2 ->
                 match v1, v2 with
                 | None, None -> None
                 | None, Some vi | Some vi, None -> Some vi
                 | Some v1, Some v2 ->
                    Some (aint32_join v1 v2)) m1.memory m2.memory;
           mjoins = [];
       }) m ms                      
  
let mem_to_any mem =
  mem_init


let mem_to_val aval mem =
  { mem with
    memory = Mem.mapi (fun addr v1 -> aval) mem.memory }
    

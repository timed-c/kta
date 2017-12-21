(* open Aint32congruence   *)
open Aint32congruence
open Printf
open Config
open Amemtype
open Cpumodel
       
(* If true, the memory will not set anything and all memory requests will
   return Any *)
let nomem = false
  
module Mem = Map.Make(
  struct type t = int
  let compare = compare
end)


type section_t = {
    addr : int;
    size : int;
  }
  
type amemory = {
    memory : amem_t Mem.t;
    mjoins : amemory list;
    bss : section_t;
    sbss : section_t;
    access_time: int;
  }

let mem_init = {
    memory = Mem.empty;
    mjoins = [];
    bss = { addr = 0; size = 0 };
    sbss = { addr = 0; size = 0 };
    access_time = !mem_access_time;
}


                    
(** Returns mem', the updated abstract memory *)
let set_memval addr v mem =
  if nomem then mem
  else {mem with memory = Mem.add addr v mem.memory}

    
(** Returns a tuple, (mem',val), where mem' is the updated
    abstract memory *)
let get_memval addr slist mem =
  if nomem then (mem,slist, AAny)
  else
    let rec getmem_internal addr slist mem =      
      try Mem.find addr mem.memory,slist
      with Not_found ->
           match mem.mjoins with
           | [] ->
              let bss_addr, bss_size = mem.bss.addr, mem.bss.size in
              let sbss_addr, sbss_size = mem.sbss.addr, mem.sbss.size in
              if (addr >= bss_addr && addr < bss_addr + bss_size) ||
                   (addr >= sbss_addr && addr < sbss_addr + sbss_size)
              then
                AInt32 (aint32_const 0),slist
              else
                let spl_any,slist' = Any true,slist in (* split_aint32 (aint32_any_set true) slist in *)
                AInt32 spl_any,slist'
           | m::rest ->
              List.fold_left
                (fun (v1,sl) mem ->
                  let v, sl = getmem_internal addr sl mem in
                  let v = amem_join false v1 v in
                 v,sl)
                (getmem_internal addr slist m) rest
    in
    let v,sl = getmem_internal addr slist mem in
    (*    (set_memval addr v mem, v)*)
    (mem, sl, v)
      

let mem_join memlist =
  (*  {mem_init with mjoins = memlist}*)
  match memlist with
  | [] ->   {mem_init with mjoins = []}
  | m::ms -> 
     List.fold_left
       (fun m1 m2 ->
         { m1 with
           memory =
             Mem.merge
               (fun addr v1 v2 ->
                 match v1, v2 with
                 | None, None -> None
                 | None, Some vi | Some vi, None -> Some vi
                 | Some v1, Some v2 ->
                    Some (amem_join false v1 v2)) m1.memory m2.memory;
           mjoins = [];
       }) m ms                      
  
let mem_to_any mem =
  mem_init


let mem_to_val aval mem =
  { mem with
    memory = Mem.mapi (fun addr v1 -> aval) mem.memory }
    


open Aint32relint
open Printf

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
  {mem with memory = Mem.add addr v mem.memory}

    
(** Returns a tuple, (val,mem'), where mem' is the updated
    abstract memory *)
let get_memval addr mem = 
  let rec getmem_internal addr mem =      
    try Mem.find addr mem.memory
    with Not_found ->
      (match mem.mjoins with
      | [] -> aint32_any
      | m::rest ->
        List.fold_left
          (fun v1 mem -> aint32_join v1 (getmem_internal addr mem))
          (getmem_internal addr m) rest)
  in
  let v = getmem_internal addr mem in
  (set_memval addr v mem, v)

    
let mem_join memlist =
  {mem_init with mjoins = memlist}
  




  

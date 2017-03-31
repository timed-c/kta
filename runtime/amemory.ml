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

type amem_t =
  | AInt32 of aint32
  | AInt16 of aint32 * aint32
  | AInt8 of aint32 * aint32 * aint32 * aint32
  | AAny

type bss_t = {
    addr : int;
    size : int;
  }
  
type amemory = {
  memory : amem_t Mem.t;
  mjoins : amemory list;
  bss : bss_t;
  }

let mem_init = {
  memory = Mem.empty;
  mjoins = [];
  bss = { addr = 0; size = 0 };
}

let amem_join bigendian v1 v2 =
  let convert_to_aint32 bigendian v = 
    (match v with
     | AInt32 v -> v
     | AInt16 (v0,v2) ->
        if bigendian then aint16_merge v0 v2
        else aint16_merge v2 v0
     | AInt8 (v0,v1,v2,v3) ->
        if bigendian then
          aint16_merge (aint8_merge v0 v1) (aint8_merge v2 v3)
        else
          aint16_merge (aint8_merge v3 v2) (aint8_merge v1 v0)
     | AAny -> aint32_any)
  in
  match v1,v2 with
  | AInt32 v1, AInt32 v2 -> AInt32 (aint32_join v1 v2)
  | AInt16 (v11,v12), AInt16 (v21,v22) -> AInt16 (aint32_join v11 v21, aint32_join v21 v22)
  | AInt8 (v11,v12,v13,v14), AInt8(v21,v22,v23,v24) -> AInt8 (aint32_join v11 v21,aint32_join v12 v22,aint32_join v13 v23,aint32_join v14 v24)
  | v1,v2 ->
     let v1 = convert_to_aint32 bigendian v1 in
     let v2 = convert_to_aint32 bigendian v2 in
     AInt32 (aint32_join v1 v2) 
                    
(** Returns mem', the updated abstract memory *)
let set_memval addr v mem =
  if nomem then mem
  else {mem with memory = Mem.add addr v mem.memory}

(* TODO(Romy): Limits for 8bit and 16bit *)
let getval_aint32 bigendian v =
  match v with
  | AInt32 v -> v
  | AInt16 (v1,v2) ->
     if bigendian then aint16_merge v1 v2
     else aint16_merge v1 v2
  | AInt8 (v0,v1,v2,v3) ->
     if bigendian then
       aint16_merge (aint8_merge v0 v1) (aint8_merge v2 v3)
     else
       aint16_merge (aint8_merge v3 v2) (aint8_merge v1 v0)
  | AAny -> aint32_any

let getval_aint16 bigendian v =
  let v0,v2 =
    match v with
    | AInt32 v -> aint32_split bigendian v
    | AInt16 (v0,v2) -> (v0,v2)
    | AInt8 (v0,v1,v2,v3) ->
       if bigendian then
         ((aint8_merge v0 v1),(aint8_merge v2 v3))
       else
         ((aint8_merge v1 v0),(aint8_merge v3 v2))
    | AAny -> (aint32_any,aint32_any)
  in (v0,v2)
    
let getval_aint8 bigendian v =
    match v with
    | AInt32 v ->
       let v0,v2 = aint32_split bigendian v in
       let v0,v1 = aint16_split bigendian v0 in
       let v2,v3 = aint16_split bigendian v2 in
       (v0,v1,v2,v3)
    | AInt16 (v0,v2) ->
       let v0,v1 = aint16_split bigendian v0 in
       let v2,v3 = aint16_split bigendian v2 in
       (v0,v1,v2,v3)
    | AInt8 (v0,v1,v2,v3) -> (v0,v1,v2,v3)
    | AAny -> (aint32_any,aint32_any,aint32_any,aint32_any)

    
(** Returns a tuple, (mem',val), where mem' is the updated
    abstract memory *)
let get_memval addr mem =
  if nomem then (mem, AAny)
  else
    let rec getmem_internal addr mem =      
      try Mem.find addr mem.memory 
      with Not_found ->
           match mem.mjoins with
           | [] ->
              let bss_addr = mem.bss.addr in
              let bss_size = mem.bss.size in
              if addr >= bss_addr && addr < bss_addr + bss_size then
                AInt32 (aint32_const 0)
              else AAny
           | m::rest ->
              List.fold_left
                (fun v1 mem ->
                  amem_join false v1 (getmem_internal addr mem))
                (getmem_internal addr m) rest
    in
    let v = getmem_internal addr mem in
    (set_memval addr v mem, v)

let set_memval_word addr v mem =
  set_memval addr (AInt32 v) mem

let set_memval_hword addr v mem =
  let addr0, hword = (addr lsr 2) lsl 2, addr land 0x3 in
  let m,oldv = get_memval addr0 mem in
  let v0,v2 = getval_aint16 false oldv in
  let newv =
    match hword with
    | 0 -> AInt16 (v,v2) | 2 -> AInt16(v0,v)
    | _ -> failwith (sprintf "Error set_memval_hword hword=%d" hword)
  in
  set_memval addr0 newv mem


let set_memval_byte addr v mem =
  let addr0, byte = (addr lsr 2) lsl 2, addr land 0x3 in 
  let m,oldv = get_memval addr0 mem in
  let v0,v1,v2,v3 = getval_aint8 false oldv in
  let newv =
    match byte with
    | 0 -> AInt8 (v,v1,v2,v3)
    | 1 -> AInt8 (v0,v,v2,v3)
    | 2 -> AInt8 (v0,v1,v,v3)
    | 3 -> AInt8 (v0,v1,v2,v)
    | _ -> failwith (sprintf "Error set_memval_byte byte=%d" byte)            in
  set_memval addr0 newv mem

let get_memval_word addr mem =
  let m,v = get_memval addr mem in
  (m,v |> getval_aint32 false)
      
let get_memval_hword addr mem =
  let addr0, hword = (addr lsr 2) lsl 2, addr land 0x3 in
  let m,v = get_memval addr0 mem in
  let v0,v2 = getval_aint16 false v in
  let v =
    match hword with
    | 0 -> v0  | 2 -> v2
    | _ -> failwith (sprintf "Error get_memval_hword hword=%d" hword)
  in (m, v)

let get_memval_byte addr mem =
  let addr0, byte = (addr lsr 2) lsl 2, addr land 0x3 in 
  let m,v = get_memval addr0 mem in
  let v0,v1,v2,v3 = getval_aint8 false v in
  let v =
      match byte with
      | 0 -> v0  | 1 -> v1  | 2 -> v2  | 3 -> v3
      | _ -> failwith (sprintf "Error get_memval_byte byte=%d" byte)
  in (m, v)


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
    

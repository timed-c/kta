open Amemtype
open Amemory
open Acache
open Printf
open Aint32congruence  
open Config

type cache_t =
  | Uni of acache
  | Sep of acache * acache

                    
type amemhierarchy = {
    mem : amemory;
    cache : cache_t;
    amap : tagmap_t option;
  }

let dcache mem =
  match mem.cache with | Uni cache | Sep (_,cache) -> cache

let icache mem =
  match mem.cache with | Uni cache | Sep (cache,_) -> cache

let disable_dcache mem =
  let cache = 
    match mem.cache with
    | Uni cache -> Uni (disable_cache cache) 
    | Sep (icache,dcache) -> Sep (icache, disable_cache dcache)
  in
  {mem with cache = cache}

let enable_dcache mem =
  let cache = 
    match mem.cache with
    | Uni cache -> Uni (enable_cache cache) 
    | Sep (icache,dcache) -> Sep (icache, enable_cache dcache)
  in
  {mem with cache = cache}

(* Update hmem_init to initialize *)
let hmem_init =
  let cache = 
    if !unified_cache then
      Uni (cache_init())
    else
      Sep (icache_init(),cache_init())
  in
  {
    mem = mem_init;
    cache = cache;
    amap = tag_init;
  }

let hmem_update_dcache cache hmem =
  match hmem.cache with
  | Uni c -> {hmem with cache = Uni cache}
  | Sep (ic,dc) -> {hmem with cache = Sep(ic,cache)}

let hmem_update_icache cache hmem =
  match hmem.cache with
  | Uni c -> {hmem with cache = Uni cache}
  | Sep (ic,dc) -> {hmem with cache = Sep(cache,dc)} 

let hmem_update_mem mem hmem =
  {hmem with mem = mem}

let hmem_update_amap amap hmem =
  {hmem with amap = amap}
    
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



let write_mem addr aval cache mem amap =
  let ticks,cache,amap = write_cache addr cache amap in
  let mem = set_memval addr aval mem in
  (ticks,cache,mem,None,amap)
     
     

let read_mem addr cache mem amap =
  let ticks,cache,amap = read_cache addr cache amap in
  let mem,v = get_memval addr mem in
  (ticks,cache,mem,v,amap)

let set_memval_word addr v mem =
  let ticks,c,m,_,amap = write_mem addr (AInt32 v) (dcache mem) mem.mem mem.amap in
  (ticks,mem |> hmem_update_dcache c |> hmem_update_mem m |> hmem_update_amap amap)

let set_memval_hword addr v mem =
  let addr0, hword = (addr lsr 2) lsl 2, addr land 0x3 in
  let ticks,c,m,oldv,_ = read_mem addr0 (dcache mem) mem.mem None in 
  let v0,v2 = getval_aint16 false oldv in
  let v = check_aint16 v in
  let newv =
    match hword with
    | 0 -> AInt16 (v,v2) | 2 -> AInt16(v0,v)
    | _ -> failwith (sprintf "Error set_memval_hword hword=%d" hword)
  in
  let _,c,m,_,amap = write_mem addr0 newv (dcache mem) mem.mem mem.amap in
  (ticks,mem |> hmem_update_dcache c |> hmem_update_mem m |> hmem_update_amap amap)


let set_memval_byte addr v mem =
  let addr0, byte = (addr lsr 2) lsl 2, addr land 0x3 in 
  let ticks,c,m,oldv,_ = read_mem addr0 (dcache mem) mem.mem None in 
  let v0,v1,v2,v3 = getval_aint8 false oldv in
  let v = check_aint8 v in
  let newv =
    match byte with
    | 0 -> AInt8 (v,v1,v2,v3)
    | 1 -> AInt8 (v0,v,v2,v3)
    | 2 -> AInt8 (v0,v1,v,v3)
    | 3 -> AInt8 (v0,v1,v2,v)
    | _ -> failwith (sprintf "Error set_memval_byte byte=%d" byte)            in
  let ticks,c,m,_,amap = write_mem addr0 newv (dcache mem) mem.mem mem.amap in
  (ticks,mem |> hmem_update_dcache c |> hmem_update_mem m |> hmem_update_amap amap)

let get_memval_word addr mem =
  let ticks,c,m,v,amap = read_mem addr (dcache mem) mem.mem mem.amap in 
  (ticks,
   mem |> hmem_update_dcache c |> hmem_update_mem m |> hmem_update_amap amap,
   v |> getval_aint32 false)
      
let get_memval_hword addr mem =
  let addr0, hword = (addr lsr 2) lsl 2, addr land 0x3 in
  let ticks,c,m,v,amap = read_mem addr0 (dcache mem) mem.mem mem.amap in 
  let v0,v2 = getval_aint16 false v in
  let v =
    match hword with
    | 0 -> v0  | 2 -> v2
    | _ -> failwith (sprintf "Error get_memval_hword hword=%d" hword)
  in (ticks,
      mem |> hmem_update_dcache c |> hmem_update_mem m |> hmem_update_amap amap,
      v)

let get_memval_byte addr mem =
  let addr0, byte = (addr lsr 2) lsl 2, addr land 0x3 in
  let ticks,c,m,v,amap = read_mem addr0 (dcache mem) mem.mem mem.amap in 
  let v0,v1,v2,v3 = getval_aint8 false v in
  let v =
      match byte with
      | 0 -> v0  | 1 -> v1  | 2 -> v2  | 3 -> v3
      | _ -> failwith (sprintf "Error get_memval_byte byte=%d" byte)
  in (ticks,
      mem |> hmem_update_dcache c |> hmem_update_mem m |> hmem_update_amap amap,
      v)

let hmem_join m1 m2 =
  let cache = 
    match m1.cache, m2.cache with
    | Uni c1, Uni c2 -> Uni (cache_join c1 c2)
    | Sep (ic1,dc1), Sep (ic2,dc2) -> Sep (cache_join ic1 ic2, cache_join dc1 dc2) 
    | _ -> failwith ("impossible")
  in
  {
    mem = mem_join [m1.mem;m2.mem];
    cache = cache;
    amap = access_merge m1.amap m2.amap;
  }
  

(********* INSTRUCTION CACHE **********)
let get_instruction addr hmem =
  let ticks,icache,amap = read_cache addr (icache hmem) (hmem.amap) in
  (ticks,hmem |> hmem_update_icache icache |> hmem_update_amap amap)

(* LD and ST -> ANY - Case of a interval access *) 
let ld_any hmem =
  let ticks = miss_cache (dcache hmem) in 
  (ticks, hmem, Any)

let st_any hmem =
  let ticks = miss_cache (dcache hmem) in
  let cache = 
    match hmem.cache with
    | Uni c ->   Uni (cache_to_any c) 
    | Sep (ic,dc) -> Sep (ic, cache_to_any dc)
  in
  let hmem = {hmem with mem = mem_to_any hmem.mem;
                        cache = cache;
             } in
  (ticks, hmem)


(********************************************)
let print_hmem_stats hmem =
  match hmem.cache with
  | Uni c -> print_cache_stats c "Unified Cache";
  | Sep (ic,dc) ->
     print_cache_stats ic "Instruction Cache";
     print_cache_stats dc "Data Cache"
    

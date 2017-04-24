open Printf
open Ustring.Op
open Amemtype
open Config

module Cache =
  Map.Make(
      struct type t = int 
             let compare = compare
      end)

module Set =
  Map.Make(
      struct type t = int 
             let compare = compare
      end)

type tag = int
                                  
type dirty = bool
               
type assoc_t = int
                 
(* 
   Set with (x <= associativity) number lines 
   Accessed by tag
   Contains LRU * dirty
*)
type aset_t = (int * dirty) Set.t

type info_t = {
    num : int;
    (* log2 : int; *)
  }

type cache_stats_t =
  {
    misses : float;
    hits : float;
  }


(************************* Record accesses *** *********************)
module AccMap =
  Map.Make(
      struct type t = int 
             let compare = compare
      end)
          
type memaccess_t = bool * bool * int
                                   
type tagmap_t = memaccess_t AccMap.t


let tag_init =
  if !record_mtags then Some AccMap.empty else None
         
let access_merge tm1 tm2 = 
  match tm1, tm2 with
  | None, None -> None
  | Some tm1, Some tm2 -> Some (AccMap.merge
                                  (fun t m1 m2 ->
                                    match m1,m2 with
                                    | Some (r1,w1,bs1), Some (r2,w2,bs2) ->
                                       (* bs1 should be larger than bs2 *)
                                       Some (r1 || r2, w1 || w2, max bs1 bs2)
                                    | Some acc, _ | _, Some acc -> Some acc
                                    | None, None -> None
                                  ) tm1 tm2
                               )
  | _, _ -> failwith "Error in tag_merge."


(*************************** ACACHE ****************************)
type acache = {
    cache : aset_t Cache.t; 
    assoc : int;
    size : int;
    word_size : int;
    block_size : int;
    write_allocate : bool;
    write_back : bool;

    setinfo : info_t;
    wordinfo : info_t;
    byteinfo : info_t;

    disabled : bool;

    hit_time : int; (* * int;*)
    miss_penalty : int; (* * int;*)

    cache_stats : cache_stats_t;
  }

type address = {
    tag : int;
    set : int;
    word : int;
    byte : int;
  }
            
type cache_responce =
  | Hit  | Miss of address * aset_t
  | Nocache

let print_cache_stats cache str =
  let hits, misses = cache.cache_stats.hits,cache.cache_stats.misses in
  printf "%s HITS:  %f\n%!" str hits;
  printf "%s MISSES:  %f\n%!" str misses;
  printf "%s HIT_RATE:  %f%%\n%!" str ((100. *. hits)/. (hits +. misses));
  printf "%s MISS_RATE:  %f%%\n%!" str ((100. *. misses)/. (hits +. misses))


(*
let rec shift_log2 pow log =
  match pow with
  | 0 | 1 -> log
  | _ -> shift_log2 (pow lsr 1) (log+1)
*)

let cache_init ?(assoc=(!associativity))
               ?(blksize=(!block_size))
               ?(csize=(!cache_size))
               ?(wdsize=(!word_size))
               ?(wralloc=(!write_allocate))
               ?(wrback=(!write_back))
               ?(hitp=(!hit_time))
               ?(missp=(!miss_penalty))
               ()
  = {
    cache = Cache.empty;
    assoc = assoc;
    block_size = blksize;
    size = csize;
    word_size = wdsize;
    
    setinfo = {
        num = csize/(assoc*blksize);
        (*log2 = shift_log2 (csize/(assoc*blksize)) 0;*)
      };
    wordinfo = {
        num = blksize/wdsize;
        (*log2 = shift_log2 (blksize/wdsize) 0;*)
      };
    byteinfo = {
        num = wdsize;
        (*log2 = shift_log2 (wdsize lsr 3) 0;*)
      };

    write_allocate = wralloc;
    write_back = wrback;
    disabled = false;
    hit_time = hitp;
    miss_penalty = missp;
    cache_stats = { misses = 0.; hits = 0.};
  }


let icache_init () = cache_init
                       ~assoc:(!iassociativity)
                       ~blksize:(!iblock_size)
                       ~csize:(!icache_size)
                       ~wdsize:(!iword_size)
                       ~wralloc:(!iwrite_allocate)
                       ~wrback:(!iwrite_back)
                       ~hitp:(!ihit_time)
                       ~missp:(!imiss_penalty)
                       ()

let cache_hits_inc cache =
  let stats = cache.cache_stats in
  let stats = {stats with hits = stats.hits +. 1.} in
  {cache with cache_stats = stats} 
  (* {cache with cache_stats = { cache.cache_stats with hits = cache.cache_stats.hits +. 1. }} *)

let cache_misses_inc cache =
  {cache with cache_stats = { cache.cache_stats with misses = cache.cache_stats.misses +. 1. }}
    
let new_cache_set () = Set.empty
  
let update_set num set cache =
  { cache with cache = Cache.add num set cache.cache }
    
let split_address addr cache =
  let byte = addr mod cache.byteinfo.num in
  let word = (addr / cache.byteinfo.num) mod cache.wordinfo.num in
  let set = ((addr / cache.byteinfo.num) / cache.wordinfo.num) mod cache.setinfo.num in
  let tag = ((addr / cache.byteinfo.num) / cache.wordinfo.num) / cache.setinfo.num in
  if !dbg_cache then
    printf "SPLIT ADDRESS: tag 0x%x set 0x%x word 0x%x byte 0x%x\n%!"
           tag set word byte;
  (*
  let byte = addr land (cache.byteinfo.num-1) in
  let word = (addr lsr cache.byteinfo.log2) land (cache.wordinfo.num-1) in
  let set = (addr lsr (cache.wordinfo.log2 + cache.byteinfo.log2)) land (cache.setinfo.num-1) in
  let tag = (addr lsr (cache.wordinfo.log2 + cache.setinfo.log2 + cache.byteinfo.log2)) in
  *)
  { tag = tag; set = set; word = word; byte = byte;}

let merge_address address cache =
  (address.byte land (cache.byteinfo.num-1))
  lor (address.word * cache.byteinfo.num)
  lor (address.set * cache.byteinfo.num * cache.wordinfo.num)
  lor (address.tag * cache.byteinfo.num * cache.wordinfo.num * cache.setinfo.num)
  
  (* (address.byte land (cache.byteinfo.num-1)) *)
  (* lor (address.word lsl cache.byteinfo.log2) *)
  (* lor (address.set lsl (cache.wordinfo.log2 + cache.byteinfo.log2)) *)
  (* lor (address.tag lsl (cache.wordinfo.log2 + cache.setinfo.log2 + cache.byteinfo.log2)) *)

let get_set address cache =
  (address.set * cache.byteinfo.num * cache.wordinfo.num)
  lor (address.tag * cache.byteinfo.num * cache.wordinfo.num * cache.setinfo.num)

(****************** Insert Function **********************)
        
let insert_line line set cache =
  let (tag,dirty) = line in
  let (lru,rest) = Set.partition
                     (fun t (pos,d) ->
                       (* TODO(Romy): should be = *)
                       pos >= cache.assoc-1) set in
  let set = Set.mapi
               (fun t (p,d) ->
                 (p+1,d)) rest in
  (lru, Set.add tag (0,dirty) set)
               

let exists_dirty set =
  Set.exists (fun t (p,d) -> d) set

(****************** Update Function **********************)

let update_cache dirty address set =
  try
    let pos,d = Set.find address.tag set in
    (*let set = Set.remove address.tag set in*)
    let set = Set.mapi
                 (fun t (p,d) ->
                   if t = address.tag then (0,dirty) 
                   else if p<pos then (p+1,d)
                   else (p,d)
                 ) set in
    (Hit,set)
  with Not_found ->
    (Miss (address,set), set)
         
let access_cache write addr cache amap =
  if !dbg_cache then
    (match amap with
     | None -> printf "None\n%!";
     | Some amap ->
        printf "TAGMAP\n";
        AccMap.iter (fun tag (_,_,v) -> printf "0x%x: %d\n%!" tag v) amap
    );
  if nocache || cache.disabled then (Nocache,cache,amap)
  else
    let address = split_address addr cache in
    let amap = match amap with
      | None -> None (* Disabled amap - No recording *)
      | Some amap ->
         let set_address = get_set address cache in
         let (r,w,bsize) =
           try
             AccMap.find address.set amap
           with
           | Not_found ->
              (false,false,cache.block_size)
         in
         let (r,w,bsize) = (r || (not write), w || write, bsize) in
         Some (AccMap.add set_address
                          (r,w,bsize) amap)
    in
    try
      let set = Cache.find address.set cache.cache in
      let (w,set) = update_cache write address set in
      let cache = update_set address.set set cache in
      (w,cache,amap)
    with
    | Not_found ->
       let set = new_cache_set () in
       let cache = update_set address.set set cache in
       (Miss(address,set),cache,amap)
         
let read_line_from_mem tag dirty = (tag,dirty)
                                
let read_cache addr cache amap =
  let (resp,cache,amap) = access_cache false addr cache amap in
  let ticks,cache =
    match resp,cache with
    | Miss (address,set), cache ->
       let cache = cache_misses_inc cache in
       if !dbg_cache then
         (* TODO(Romy): fix debugging msg *)(
         printf "MISS: read_cache addr:0x%x\n%!" addr;
         print_cache_stats cache "read miss");
       let line = read_line_from_mem address.tag false in
       let blru,set = insert_line line set cache in
       let cache = update_set address.set set cache in
       if cache.write_back && exists_dirty blru then
         (* write back to mem --- penalty *)
         (cache.hit_time + cache.miss_penalty,cache)
       else
         (* write_back - nodirty || write through *)
         (cache.hit_time + cache.miss_penalty,cache)
    | Hit, cache ->
       let cache = cache_hits_inc cache in
       if !dbg_cache then
         (* TODO(Romy): fix debugging msg *)
         printf "HIT: read_cache addr:0x%x\n%!" addr;
       (cache.hit_time,cache)
    | Nocache, _ -> (1,cache)
  in ticks,cache,amap

               
let write_cache addr cache amap =
  let (resp,cache,amap) = access_cache true addr cache amap in
  let (ticks,cache) = 
    match resp with
    | Miss (address,set) ->
       let cache = cache_misses_inc cache in
       if !dbg_cache then
         (* TODO(Romy): fix debugging msg *)
         printf "MISS: write_cache addr:0x%x\n%!" addr;
       if cache.write_allocate then (
         let line = read_line_from_mem address.tag true in
         let blru,set = insert_line line set cache in
         let cache = update_set address.set set cache in

         if cache.write_back then
           if exists_dirty blru then 
             (* write_allocate && write_back -> MISS *)
             (* update main_mem?? *)
             (cache.hit_time + cache.miss_penalty,cache)
           else
             (cache.hit_time + cache.miss_penalty,cache)
         else
           (* write_allocate && write_through -> MISS *)
           (cache.hit_time + cache.miss_penalty,cache)
       )
       else
         (* write_no_allocate -> MISS *)
         (cache.hit_time + cache.miss_penalty,cache)
    | Hit ->
       if !dbg_cache then
         (* TODO(Romy): fix debugging msg *)
         printf "HIT: write_cache addr:0x%x\n%!" addr;
       let cache = cache_hits_inc cache in
       if cache.write_back then
         (* write_back -> HIT *)
         (cache.hit_time,cache)
       else
         (* write_through -> HIT - has to be written to the main mem *)
         (cache.hit_time + cache.miss_penalty,cache)        
    | Nocache -> (1,cache)
  in (ticks,cache,amap)

(******** Cache to any (similar to mem_to_any) *********)

let cache_to_any cache =
  {cache with cache = Cache.empty}

(****************** Disable/Enable Cache ***************)

let disable_cache c =
  { c with disabled = true }

let enable_cache c =
  { c with disabled = false }

(****************** Join Function **********************)
(*** c1 is the longest path ****)
let cache_join c1 c2 =
  let rec join_sets cache1 cache2 cache num =
    match num with
    | -1 -> cache
    | _ ->
       let cache =
         (try
            let set1 = Cache.find num cache1.cache in
            let set2 = Cache.find num cache2.cache in
            let set = Set.merge
                         (fun t l1 l2 ->
                           match l1,l2 with
                           | None,_ | _,None -> None
                           | Some (pos1,d1), Some (pos2,d2) ->
                              Some (max pos1 pos2, d1 || d2)
                         ) set1 set2
            in
            update_set num set cache
          with Not_found -> cache
         )
       in
       join_sets cache1 cache2 cache (num-1)
  in
  join_sets c1 c2 ({c1 with cache = Cache.empty}) (c1.setinfo.num-1)

             
(******************Print Functions**********************)

let prn_cache str addr =
  if !dbg_cache then
    printf "%10s | %s addr: 0x%x\n%!" "CACHE" str addr
  else ()

let print_cache cache =
  let rec get_sets cache num =
    match num with
    | -1 -> ()
    | _ ->
       (try
          let set = Cache.find num cache.cache in
          printf "sets of sets found for set %d and has length %d. Assoc = %d\n%!" num (Set.cardinal set) cache.assoc
        with Not_found ->
          printf "set not found %d\n%!" num; );
       get_sets cache (num-1)
  in
  get_sets cache (cache.setinfo.num-1)


(************************* MISS *********************)
let miss_cache cache =
  if nocache || cache.disabled then 1 else cache.hit_time + cache.miss_penalty
                                             

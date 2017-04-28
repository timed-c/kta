let counter = ref 0
let count() = counter := !counter + 1
                                      
let config_max_batch_size = ref 4

let config_max_cycles = ref 1000000
                            
let set_max_batch_size size =
  config_max_batch_size := size

let set_max_cycles cycles =
  config_max_cycles := cycles


(* Debugging variables *)
let dbg = ref false
(* Note that you should compile with exper.d.byte *)
let dbg_trace = ref true 
let dbg_inst = ref true
let dbg_mstate_sizes = ref true
let dbg_debug_intervals = ref true

(* TODO(Romy): fix the dbg messages in cache/pipeline/ticks/stats *)
let dbg_pipeline = ref false
let dbg_cache = ref false
let dbg_ticks = ref false
let dbg_stats = ref false 
                    
let enable_debug enable =
  dbg := enable;
  dbg_trace := enable;
  dbg_inst := enable;
  dbg_mstate_sizes := enable;
  dbg_debug_intervals := enable

(****** Cache configuration test-parameters *****)

(* disable cache *)
let nocache = ref false

let levels = ref 1
                       
(* Data/Unified Cache parameters *)
(* Number of sets *)
let associativity = ref 2
(* Absolute block size of cache *)
let block_size = ref 16
(* Absolute size of the cache *)
let cache_size = ref 1024
(* Absolute word size of the cache *)
let word_size = ref 4
(* Type of cache *)
let write_allocate = ref true
let write_back = ref true

let hit_time = ref 1 (*(1,2)*)
(* let miss_penalty = ref 5 (\*(8,66)*\) *)

let shared = ref true
                 
(****** TAG RECORD ******)
let record_mtags = ref true

let set_record v =
  record_mtags := false

(****** PIPELINE ******)
let disable_pipeline = ref false

  

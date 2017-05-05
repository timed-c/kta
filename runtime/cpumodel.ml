
type acache_info_t = {
  assoc : int;
  size : int;
  word_size : int;
  block_size : int;
  write_allocate : bool;
  write_back : bool;
  hit_time : int; (* * int;*)
    (* miss_penalty : int; (\* * int;*\) *)
  shared : bool;
}

type l1cache_type =
  | U of acache_info_t
  | S of acache_info_t * acache_info_t 

let cache_model =
  [S (
       (*Icache*)
       { assoc = 1; size = 2048;
         word_size = 4; block_size = 16;
         write_allocate = true; write_back = true;
         hit_time = 1;
         shared = false; 
       },
       (*Dcache*)
       { assoc = 4; size = 2048;
         word_size = 4; block_size = 16;
         write_allocate = true; write_back = true;
         hit_time = 1;
         shared = false;});
   (*L2*)
   U ({ assoc = 4; size = 8192;
        word_size = 4; block_size = 16;
        write_allocate = true; write_back = true;
        hit_time = 5; 
        shared = false;
     });
   (*L3*)
   U ({ assoc = 8; size = 32768;
        word_size = 4; block_size = 16;
        write_allocate = true; write_back = true;
        hit_time = 50;
        shared = false;
      })
  ]

(****** MEMORY ******)

let mem_access_time = ref 100

(****** COHERENCE PENALTY ******)

(* let coherence_penalty = ref 130 *)
let inv_penalty = ref 15
let wb_penalty = ref 10
let cache_penalty = ref 40

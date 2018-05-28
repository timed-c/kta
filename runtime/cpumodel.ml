
type acache_info_t = {
  assoc : int;
  size : int;
  word_size : int;
  block_size : int;
  write_allocate : bool;
  write_back : bool;
  hit_time : int; (* * int;*)
  shared : bool;
}

type l1cache_type =
  | U of acache_info_t
  | S of acache_info_t * acache_info_t 

let cache_model =
  [ (*L1*)
     S (
       (*Icache*)
       { assoc = 1; size = 4096;
         word_size = 4; block_size = 16;
         write_allocate = true; write_back = true;
         hit_time = 1;
         shared = false; 
       },
       (*Dcache*)
       { assoc = 1; size = 4096; (*1 lsl 15 *)
         word_size = 4; block_size = 16;
         write_allocate = true; write_back = true;
         hit_time = 1;
         shared = false;});
   (*L2*)
   (*U ({ assoc = 8; size = 1 lsl 19;
        word_size = 4; block_size = 32;
        write_allocate = true; write_back = true;
        hit_time = 10-2; 
        shared = true;
     });
   *)
   (*L3*)
   (* U ({ assoc = 8; size = 32768; *)
   (*      word_size = 4; block_size = 16; *)
   (*      write_allocate = true; write_back = true; *)
   (*      hit_time = 50; *)
   (*      shared = false; *)
   (*    }) *)
  ]



(****** CPU MODEL ******)
(* Execution stage of MIPS instructions *)
type instructions_t = {
  br : int;
  div : int;
  mul : int;
  mult : int;
  madd : int;
  arithm : int;
  movlh : int;
}



let cpu_model = ref {
  br = 1;
  div = 1;
  mul = 1;
  mult = 1;
  madd = 1;
  arithm = 1;
  movlh = 1;
}

let set_cpu_single_cycle () =
	cpu_model := {
		br = 1;
		div = 1;
		mul = 1;
		mult = 1;
		madd = 1;
		arithm = 1;
		movlh = 1;
	}

(****** COHERENCE PENALTY ******)
(* let coherence_penalty = ref 130 *)
let inv_penalty = ref 15
let wb_penalty = ref 10
let cache_penalty = ref 40

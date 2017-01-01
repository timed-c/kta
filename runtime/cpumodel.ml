open Acache

type l1cache_type =
  | U of acache_info_t
  | S of acache_info_t * acache_info_t 

let cache_model =
  [S (
       (*Icache*)
       { assoc = 1; size = 1024;
         word_size = 4; block_size = 16;
         write_allocate = true; write_back = true;
         hit_time = 1;
         shared = false; 
       },
       (*Dcache*)
       { assoc = 2; size = 1024;
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
       hit_time = 100;
       shared = false;
      })
  ]

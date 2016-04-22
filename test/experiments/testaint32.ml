



open Ustring
open Printf
open Aint32relint


let main =
  let a = aint32_interval 7 100 in
  let b = aint32_interval 50 200 in
  let c = aint32_add a b in
  aint32_print c;  
  printf "\n";
  aint32_print (aint32_const 88);
  printf "\n";
    


  

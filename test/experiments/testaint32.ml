



open Ustring
open Printf
open Aint32relint


let print_op op =
  match op with
  | None -> printf "**\n"  
  | Some(v1,v2) ->
    aint32_print_debug v1; printf "    "; aint32_print_debug v2; printf "\n"

let print_rval (op1,op2) =
  printf "TRUE:  "; print_op op1;
  printf "FALSE: "; print_op op2;
  printf "\n"
      
let main =
  let a = aint32_interval 7 100 in
  let b = aint32_interval 50 200 in
  let c = aint32_add a b in 
  aint32_print_debug c; printf "\n";
  let c = aint32_join a b in 
  aint32_print_debug c; printf "\n";
  let c = aint32_add c a in 
  aint32_print_debug c; printf "\n";

  let d = aint32_join c a in 
  aint32_print_debug c; printf "\n";

  let d = aint32_join c d in 
  aint32_print_debug d; printf "\n";

  printf "------------\n";
  let a = aint32_interval 7 100 in
  let b = aint32_interval 100 200 in
  let (vt1,vf1) = aint32_test_equal a b in
  print_rval (vt1,vf1);
  let (Some(c1,c2)) = vf1 in
  let k = aint32_interval 7 100 in
  let c2' = aint32_add c2 k in
  aint32_print_debug c2'; printf "\n";

  let (vt1,vf1) = aint32_test_equal c2' a in
  print_rval (vt1,vf1);

  let a = aint32_interval 0 1 in
  let b = aint32_interval 0 1 in
  let (vt1,vf1) = aint32_test_equal a b in
  print_rval (vt1,vf1);
  
  printf "\n";
    


  

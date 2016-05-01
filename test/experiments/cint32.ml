
open Printf
open Ustring.Op

type aint32 = int

let lowval = -2147483648
let highval = 2147483647
let aint32_any = 0

exception Not_concrete_val of string  
  
  
let interval_merge _ _  = raise (Not_concrete_val "interval_merge")
  

let interval_merge_list lst = raise (Not_concrete_val "interval_merge_list")

    
let aint32_to_int32 v = v
    
let aint32_pprint debug v = ustring_of_int v

let aint32_print_debug v = printf "%d\n" v
  
let aint32_add v1 v2 = v1 + v2

let aint32_mul v1 v2 = v1 * v2
    
let aint32_const v = v

let aint32_interval l h =
  if l = h then l else raise (Not_concrete_val "aint32_interval")
  
let aint32_join v1 v2 = raise (Not_concrete_val "aint32_join")
             
let aint32_compare x y = compare x y
    
let aint32_test_less_than v1 v2 =
  if v1 < v2 then
    (Some(v1,v2),None)
  else
    (None,Some(v1,v2))

let aint32_test_less_than_equal v1 v2 =
  if v1 <= v2 then
    (Some(v1,v2),None)
  else
    (None,Some(v1,v2))

let aint32_test_equal v1 v2 =
  if v1 = v2 then
    (Some(v1,v2),None)
  else
    (None,Some(v1,v2))


      

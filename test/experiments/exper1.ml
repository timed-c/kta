
open Printf
open AbstractMIPS 
open Ustring.Op

open Scanf

(* -- Basic Block Identifiers -- *)

let final_  = 0
let exper_  = 1
  
  
(* -- Program Code -- *)
    
let rec final ms = ms
    
and exper ms = ms        |>   
    addi  t0 zero 10     |>
    add   v0 t0 a0       |>
    next  


(* -- Basic Block Info -- *)
    
let bblocks =
[|
  {func=final;  nextid=na_;    dist=0; addr=0x0};
  {func=exper;  nextid=final_; dist=1; addr=0x00400000};
|]

  
(* -- Start of Analysis -- *)

(* Returns a option tuple Some(low,high) if the input string
   has format "[low,high]" where low and high are decimal numbers.
   Returns none if there are any errors *)
let get_str str =
  try bscanf (Scanning.from_string str) "[%d,%d]" (fun x y -> Some(x,y))
  with _ -> None

  
let main =
  let s = Sys.argv.(1) in
 (* (try bscanf Scanning.stdin "[%d,%d]" (fun x y -> printf "[%d,%d]\n" x y)
    with _ -> printf "Scan failure\n"); *)
  printf "%s\n" s;
  (match get_str s with
   | Some(x,y) -> printf "[%d,%d]\n" x y
   | None -> ());

  analyze exper_ bblocks (Array.to_list Sys.argv |> List.tl)

      

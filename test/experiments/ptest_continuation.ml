(*

Performance test to check if a continuation is much slower than a loop.
Also, we check if it is more expensive to use records instead of using currying

Conclusion: Continuations are at least as fast as just using loops and match constructs.

It quite much slower to do parameter passing using records instead of using currying.

Execute the program using the following line
 >>ocamlbuild ptest_module_func.native -lib unix --


*)

open Printf


let times = 5000000000

(* -- f_func -- *)
  
let rec f1 a b c d e f g =
  if a < times then 
    f2 (a+1) (b+10) (c+20) (d+30) (e+40) (f+50) (g+60) 
  else
    a + b + c + g
      
and f2 a b c d e f g =
  if a < times/2 then
    f3 (a+1) (b+a/1000) (c+d) (d+e) (e+f) (f+g) (g+a+b) 
  else
    f1 (a+1) (b+77) (c*2+20) (d*2+30) (e*2+40) (f*2 +50) (g*10+60)   

and f3 a b c d e f g =
    f1 (a+1) (b-2) (c*20) (d*30) (e*40) (f*50) (g*60)   


let f_func() =
  f1 0 1 2 3 4 5 6


(* -- g_func -- *)

type myrec = {
  a : int;
  b : int;
  c : int;
  d : int;
  e : int;
  f : int;
  g : int;
}


let rec g1 t =
  if t.a < times then 
    g2 {a=t.a+1; b=t.b+10; c=t.c+20; d=t.d+30; e=t.e+40; f=t.f+50; g=t.g+60} 
  else
    t.a + t.b + t.c + t.g
      
and g2 t =
  if t.a < times/2 then
    g3 {a=t.a+1; b=t.b+t.a/1000; c=t.c+t.d; d=t.d+t.e; e=t.e+t.f; f=t.f+t.g; g=t.g+t.a+t.b} 
  else
    g1 {a=t.a+1; b=t.b+77; c=t.c*2+20; d=t.d*2+30; e=t.e*2+40; f=t.f*2 +50; g=t.g*10+60}   

and g3 t =
    g1 {a=t.a+1; b=t.b-2; c=t.c*20; d=t.d*30; e=t.e*40; f=t.f*50; g=t.g*60}   


let g_func() =
  g1 {a=0;b=1;c=2;d=3;e=4;f=5;g=6}



(* -- l_func -- *)

    
type funct = F1 | F2 | F3

    
let l_func() =
  let rec loop func a b c d e f g =
    match func with
    | F1 ->    
      if a < times then 
        loop F2 (a+1) (b+10) (c+20) (d+30) (e+40) (f+50) (g+60) 
      else
        a + b + c + g
    | F2 ->
      if a < times/2 then
        loop F3 (a+1) (b+a/1000) (c+d) (d+e) (e+f) (f+g) (g+a+b) 
      else
        loop F1 (a+1) (b+77) (c*2+20) (d*2+30) (e*2+40) (f*2 +50) (g*10+60)   
    | F3 -> 
      loop F1 (a+1) (b-2) (c*20) (d*30) (e*40) (f*50) (g*60)     
  in
    loop F1 0 1 2 3 4 5 6

    



let timetest str f1 =
    let t1 = Unix.gettimeofday() in
    let v = f1() in
    let t2 = Unix.gettimeofday() in
    printf "%s %10f   v = %d\n" str (t2 -. t1) v 
    

let main =
    timetest "continuation curried   " f_func; 
    timetest "continuation record    " g_func; 
    timetest "loop                   " l_func;


    

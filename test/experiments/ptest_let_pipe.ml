(*

Performance test to check if a series of let constructs are faster then using pipe with a record state


It quite much slower to do parameter passing using records instead of using currying.

Conclusion: It is not much slower to use records and pipes. No real reason not using it.

Execute the program using the following line
 >>ocamlbuild ptest_module_func.native -lib unix --


*)

open Printf


let times = 500000000


let let_op1 x y a b c = (a+1+x,a+b+x,b+c+y)
let let_op2 x y a b c = (a+1+y,c+10+y,b+7+x)
 
let lets a b c =
  let (a,b,c) = let_op1 7 10 a b c in
  let (a,b,c) = let_op2 2 8 a b c in
  let (a,b,c) = let_op2 70 9 a b c in
  let (a,b,c) = let_op1 20 2 a b c in
  let (a,b,c) = let_op2 100 9 a b c in
  (a,b,c)

let lets_loop () = 
  let rec loop count a b c =
    if count < times then
      let (a,b,c) = lets a b c in
      loop (count+1) a b c
    else
      a + b
  in
    loop 0 1 2 3

      
type myrec = {
  a : int;
  b : int;
  c : int;
}
    
let pipe_op1 x y t = {a=t.a+1+x; b=t.a+t.b+x; c=t.b+t.c+y}
let pipe_op2 x y t = {a=t.a+1+y; b=t.c+10+y; c=t.b+7+x}
 
let pipes t = t |>    
  pipe_op1 7 10  |>
  pipe_op2 2 8   |>
  pipe_op2 70 9  |>
  pipe_op1 20 2  |>
  pipe_op2 100 9 

let pipes_loop () = 
  let rec loop count t =
    if count < times then
      let t = pipes t in
      loop (count+1) t
    else
      t.a + t.b
  in
    loop 0 {a=1;b=2;c=3}


let timetest str f1 =
    let t1 = Unix.gettimeofday() in
    let v = f1() in
    let t2 = Unix.gettimeofday() in
    printf "%s %10f   v = %d\n" str (t2 -. t1) v 
    

let main =
    timetest "let counting   " lets_loop; 
    timetest "pipe counting  " pipes_loop; 
    timetest "let counting   " lets_loop; 
    timetest "pipe counting  " pipes_loop; 



    

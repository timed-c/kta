(*

Performance test to check if it is faster or slower to access tuples using
abstractions.

Conclusion. The abstraction costs a little bit (10%) but not much. 

Execute the program using the following line
 >>ocamlbuild ptest_rec_accesss.native -lib unix --

Executation time: ~ 20sec

*)

open Printf

type myrec = {
  a : int;
  b : int;
  c : int;
  d : int;
  e : int;
  f : int;
  g : int;
}

let newrec v =
{
a = v;
b = v;
c = v;
d = v;
e = v;
f = v;
g = v;
}

let times = 1000000000

let addv = 87

type inst = A | B | C | D | E | F | G

let getV t inst =
    match inst with
    | A -> t.a
    | B -> t.b
    | C -> t.c
    | D -> t.d
    | E -> t.e
    | F -> t.f
    | G -> t.g

let setV t inst v =
    match inst with
    | A -> {t with a = v}
    | B -> {t with b = v}
    | C -> {t with c = v}
    | D -> {t with d = v}
    | E -> {t with e = v}
    | F -> {t with f = v}
    | G -> {t with g = v}


let adder t inst v =
    setV t inst (getV t inst + v)
    

let record_abs() = 
    let rec loop count t =
        if count == 0 then t
        else
           let t' = adder t A 7 in
           let t'' = adder t' D (getV t' A) in
           loop (count-1) t''
    in
      (loop times (newrec 0)).d



let record_op() = 
    let rec loop count t =
        if count == 0 then t
        else
           let t' = {t with a = t.a + 7} in
           let t'' = {t' with d = t'.a + t'.d} in           
           loop (count-1) t''
    in
      (loop times (newrec 0)).d


let timetest str f1 =
    let t1 = Unix.gettimeofday() in
    let v = f1() in
    let t2 = Unix.gettimeofday() in
    printf "%s %10f   v = %d\n" str (t2 -. t1) v 
    

let main =
    timetest "Record Abstracted " record_abs; 
    timetest "Record Optimized  " record_op
    

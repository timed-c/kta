(*

Performance test to check if it is faster to use tuples than immutable records.
It seems like immutable records are at least as fast, if not faster.

Execute the program using the following line
 >>ocamlbuild ptest_rec_tuple.native -lib unix --

Executation time: ~2.5seconds

*)

open Printf



let times = 1000000000


let tuple() =
    let rec loop count t =
        if count == 0 then t
        else
           let (a,b,c) = t in 
           loop (count-1) (a+10,b,c+100)
    in
      let (a,_,_) = loop times (7,7,7) in
      a

type myrec = {
  a : int;
  b : int;
  c : int;
}

let record() = 
    let rec loop count t =
        if count == 0 then t
        else
           loop (count-1) {t with a = t.a + 10; c = t.c+100 }
    in
      (loop times {a=7;b=7;c=7}).a


let timetest str f1 =
    let t1 = Unix.gettimeofday() in
    let v = f1() in
    let t2 = Unix.gettimeofday() in
    printf "%s %10f   v = %d\n" str (t2 -. t1) v 
    



let main =
    timetest "Tuple " tuple; 
    timetest "Record " record 
    
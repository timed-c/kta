(*

Performance test to check if it is more expensive to call functions if the are located in another 
module compared to if they are local.

Conclusion: It is not possible to notice any performance difference if the
functions are local or shared in a module. It is  

Execute the program using the following line
 >>ocamlbuild ptest_module_func.native -lib unix --


*)

open Printf

open Ptest_lib_module_func
  
type _myrec = {
  _a : int;
  _b : int;
  _c : int;
  _d : int;
  _e : int;
  _f : int;
  _g : int;
}

let _newrec v =
{
_a = v;
_b = v;
_c = v;
_d = v;
_e = v;
_f = v;
_g = v;
}

let times = 100000000

let addv = 87

type _inst = A_ | B_ | C_ | D_ | E_ | F_ | G_

let _getV t inst =
    match inst with
    | A_ -> t._a
    | B_ -> t._b
    | C_ -> t._c
    | D_ -> t._d
    | E_ -> t._e
    | F_ -> t._f
    | G_ -> t._g

let _setV t inst v =
    match inst with
    | A_ -> {t with _a = v}
    | B_ -> {t with _b = v}
    | C_ -> {t with _c = v}
    | D_ -> {t with _d = v}
    | E_ -> {t with _e = v}
    | F_ -> {t with _f = v}
    | G_ -> {t with _g = v}


let _adder t inst v =
    _setV t inst (_getV t inst + v)
    

let record_abs() = 
    let rec loop count t =
        if count == 0 then t
        else
           let t' = adder t A 7 in
           let t'' = adder t' D (getV t' A) in
           loop (count-1) t''
    in
      (loop times (newrec 0)).d

let record_abs_local() = 
    let rec loop count t =
        if count == 0 then t
        else
           let t' = _adder t A_ 7 in
           let t'' = _adder t' D_ (_getV t' A_) in
           loop (count-1) t''
    in
      (loop times (_newrec 0))._d
        

let record_abs_func() = 
    let rec loop count t =
        if count == 0 then t
        else
           let t' = adder t __a 7 in
           let t'' = adder t' __d (getV t' __a) in
           loop (count-1) t''
    in
      (loop times (newrec 0)).d



let timetest str f1 =
    let t1 = Unix.gettimeofday() in
    let v = f1() in
    let t2 = Unix.gettimeofday() in
    printf "%s %10f   v = %d\n" str (t2 -. t1) v 
    

let main =
    timetest "Using module    " record_abs; 
    timetest "Local functions " record_abs_local;
    timetest "Model with func " record_abs_func;
    timetest "Using module    " record_abs; 
    timetest "Local functions " record_abs_local;
    timetest "Model with func " record_abs_func;

    

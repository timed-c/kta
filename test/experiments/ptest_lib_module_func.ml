(*

This is a support module to  ptest_module_func.ml

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
    


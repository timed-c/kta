
type high = int
type low = int

type aval = high * low


let aint32_add (h1,l1) (h2,l2) =
    (h1+h2,l1+l2)


let aint32_const v =
    (v,v)

let aint32_interval h l =
    (h,l)




type lower = int
type upper = int
type aint32 = (lower * upper) list


let int32max = 0x7fffffff
  
let int32min = (0x7fffffff + 1) * -1

let abstractval = [(int32min,int32max)]


let make v = ([v,v])

let make_intervals ival = ival




open LlvmAst



val const_int_val : int -> int -> llVal 
(** Expression [const_int_val w v] creates a constant integer value with bit
    width [w] and value [v]. *)

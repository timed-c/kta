

open LlvmAst


let const_int_val width v = VConst(CInt(width, Int64.of_int v))

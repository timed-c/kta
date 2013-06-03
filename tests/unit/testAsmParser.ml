




open Ustring.Op
open Utils
open Utest


let str_parse_print_asm str = 
  AsmSyntax.sprint (AsmParser.main (AsmLexer.main) (Ustring.lexing_from_ustring str))


let main = 
  init "Generic Asm parsing module.";


  test_ustr "An add instruction"  
            "add     x1,x2,x3" (str_parse_print_asm "add x1,x2,x3")

  result()

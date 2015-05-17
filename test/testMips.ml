

open Ustring.Op
open Utest
open Printf
open Utils


let main = 

  Utest.init "MIPS";


  (* Test decoding of basic ASM instructions *)
  let tmpname = "__tmp__" in
  MipsSys.pic32_compile ["test/mips_tests/asmtest.c"] false false tmpname;
  let insts = MipsUtils.decode (MipsSys.get_section tmpname ".text") in
  Sys.remove tmpname;

  let expected = Ustring.read_file "test/mips_tests/asmtest.expected_asm" in
  let result = MipsUtils.pprint_inst_list insts in
  Utest.test_ustr "Test decoding of basic MIPS ISA instructions."
    result expected;


  Utest.result()


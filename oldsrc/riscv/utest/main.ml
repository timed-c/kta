

open Printf

let main = 
  TestBinDecodePrint.main;
  TestBinEncodeDecode.main;
  Utest.summary()




type machinestate = 
{
  registers : int32 array;
  data : bytes;
}


let eval prog func = 
{
  registers = Array.make 32 Int32.zero;
  data = Bytes.empty;
}


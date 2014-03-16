
extern int k;

int main(int args, char **argv)
{
  return args * 50 - (args/10) * k;
}

/*
return args / 50 + 10;
        UNKNOWN(2,1,3,4)
        UNKNOWN(2,5,4,0,2)


return args * 50 + 10;
        UNKNOWN(2,1,3,2,2)
        UNKNOWN(2,5,4,0,2)

return args * 50 - (args/10);
--
%mul = mul nsw i32 %args, 50
%div = sdiv i32 %args, 10
%sub = sub nsw i32 %mul, %div
        UNKNOWN(2,1,3,2,2)
        UNKNOWN(2,1,4,4)
        UNKNOWN(2,5,6,1,2)


  return args * 50 - (args/10) * k;
  --
  %mul = mul nsw i32 %args, 50
  %div = sdiv i32 %args, 10
  %0 = load i32* @k, align 4, !tbaa !0
  %mul1 = mul nsw i32 %0, %div
  %sub = sub nsw i32 %mul, %mul1
        UNKNOWN(2,2,4,2,2)
        UNKNOWN(2,2,5,4)
        UNKNOWN(20,0,3,0)
        UNKNOWN(2,8,7,2,2)
        UNKNOWN(2,6,9,1,2)
*/

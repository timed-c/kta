; ModuleID = 'loop1.c'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.9.0"

@.str = private unnamed_addr constant [12 x i8] c"Result: %d\0A\00", align 1

; Function Attrs: nounwind readnone ssp uwtable
define i32 @foo(i32 %max, i32 %a, i32 %b, i32 %k) #0 {

b1:
  %i1 = add i32 0, 0 
  %r1 = add i32 0, 0
  br label %b2
b2:      
  %i2 = phi i32 [%i1, %b1], [%i7, %b7]  
  %r2 = phi i32 [%r1, %b1], [%r7, %b7]
  %c2 = icmp slt i32 %i2, %max
  br i1 %c2, label %b3, label %b8

b3:
  %c3 = icmp slt i32 %i2, %a
  %j3 = add i32 0, %i2
  br i1 %c3, label %b4, label %b6

b4: 
  %j4 = phi i32 [%j3, %b3], [%j5, %b5]
  %r4 = phi i32 [%r2, %b3], [%r5, %b5]
  %c4 = icmp slt i32 %j4, 10
  br i1 %c4, label %b6, label %b5

b5:
  %r5 = add i32 %r4, %j4 
  %j5 = add i32 %j4, -1
  br label %b4

b6:
  %r6 = phi i32 [%r2, %b3], [%r4, %b4]
  %c6 = icmp slt i32 %i2, %b
  br i1 %c6, label %b7, label %b8

b7:
  %r7 = add i32 %r6, %i2
  %i7 = add i32 %i2, %k
  br label %b2

b8:
  %r8 = phi i32 [%r2, %b2], [%r6, %b6]
  ret i32 %r8


}

; Function Attrs: nounwind ssp uwtable
define i32 @main() #1 {
  %1 = tail call i32 @foo(i32 1000, i32 100, i32 900, i32 3)
  %2 = tail call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([12 x i8]* @.str, i64 0, i64 0), i32 %1) #3
  ret i32 0
}

; Function Attrs: nounwind
declare i32 @printf(i8* nocapture readonly, ...) #2

attributes #0 = { nounwind readnone ssp uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind ssp uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { nounwind "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { nounwind }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"Apple LLVM version 5.1 (clang-503.0.40) (based on LLVM 3.4svn)"}

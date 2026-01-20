; LLVM IR sample
@.str = private unnamed_addr constant [9 x i8] c"fib=%d\0A\00"

define i32 @fib(i32 %n) {
entry:
  %cmp = icmp slt i32 %n, 2
  br i1 %cmp, label %ret, label %rec

ret:
  ret i32 %n

rec:
  %n1 = sub i32 %n, 1
  %a = call i32 @fib(i32 %n1)
  %n2 = sub i32 %n, 2
  %b = call i32 @fib(i32 %n2)
  %sum = add i32 %a, %b
  ret i32 %sum
}

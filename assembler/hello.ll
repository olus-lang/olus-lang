

define i32 @f(i32 %x) {
entry:
  %add = add nsw i32 %x, 1
  ret i32 %add
}


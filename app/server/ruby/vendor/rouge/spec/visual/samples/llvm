; copied from http://llvm.org/docs/LangRef.html#module-structure

; Declare the string constant as a global constant.
@.str = private unnamed_addr constant [13 x i8] c"hello world\0A\00"

; External declaration of the puts function
declare i32 @puts(i8* nocapture) nounwind

; Definition of main function
define i32 @main() {   ; i32()*
  ; Convert [13 x i8]* to i8  *...
  %cast210 = getelementptr [13 x i8]* @.str, i64 0, i64 0

  ; Call puts function to write out the string to stdout.
  call i32 @puts(i8* %cast210)
  ret i32 0
}

; Named metadata
!1 = metadata !{i32 42}
!foo = !{!1, null}


; copied from http://llvm.org/docs/LangRef.html#poison-values

entry:
  %poison = sub nuw i32 0, 1           ; Results in a poison value.
  %still_poison = and i32 %poison, 0   ; 0, but also poison.
  %poison_yet_again = getelementptr i32* @h, i32 %still_poison
  store i32 0, i32* %poison_yet_again  ; memory at @h[0] is poisoned

  store i32 %poison, i32* @g           ; Poison value stored to memory.
  %poison2 = load i32* @g              ; Poison value loaded back from memory.

  store volatile i32 %poison, i32* @g  ; External observation; undefined behavior.

  %narrowaddr = bitcast i32* @g to i16*
  %wideaddr = bitcast i32* @g to i64*
  %poison3 = load i16* %narrowaddr     ; Returns a poison value.
  %poison4 = load i64* %wideaddr       ; Returns a poison value.

  %cmp = icmp slt i32 %poison, 0       ; Returns a poison value.
  br i1 %cmp, label %true, label %end  ; Branch to either destination.

true:
  store volatile i32 0, i32* @g        ; This is control-dependent on %cmp, so
                                       ; it has undefined behavior.
  br label %end

end:
  %p = phi i32 [ 0, %entry ], [ 1, %true ]
                                       ; Both edges into this PHI are
                                       ; control-dependent on %cmp, so this
                                       ; always results in a poison value.

  store volatile i32 0, i32* @g        ; This would depend on the store in %true
                                       ; if %cmp is true, or the store in %entry
                                       ; otherwise, so this is undefined behavior.

  br i1 %cmp, label %second_true, label %second_end
                                       ; The same branch again, but this time the
                                       ; true block doesn't have side effects.

second_true:
  ; No side effects!
  ret void

second_end:
  store volatile i32 0, i32* @g        ; This time, the instruction always depends
                                       ; on the store in %end. Also, it is
                                       ; control-equivalent to %end, so this is
                                       ; well-defined (ignoring earlier undefined
                                       ; behavior in this example).

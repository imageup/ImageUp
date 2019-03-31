; ModuleID = 'ImageUp'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.1 = private unnamed_addr constant [4 x i8] c"%g\0A\00"
@fmt_str = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@system_string = private unnamed_addr constant [13 x i8] c"hello world!\00"

declare i32 @printf(i8*, ...)

declare i8* @prints(i8*, ...)

define i32 @main() {
entry:
  %printf = call i8* (i8*, ...) @prints(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt_str, i32 0, i32 0), i8* getelementptr inbounds ([13 x i8], [13 x i8]* @system_string, i32 0, i32 0))
  ret i32 0
}

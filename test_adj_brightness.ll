; ModuleID = 'ImageUp'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.1 = private unnamed_addr constant [4 x i8] c"%g\0A\00"
@fmt.2 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@path_name = private unnamed_addr constant [19 x i8] c"./images/face1.jpg\00"
@path_name.3 = private unnamed_addr constant [13 x i8] c"test.out.jpg\00"
@fmt.4 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.5 = private unnamed_addr constant [4 x i8] c"%g\0A\00"
@fmt.6 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@fmt.7 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.8 = private unnamed_addr constant [4 x i8] c"%g\0A\00"
@fmt.9 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@fmt.10 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.11 = private unnamed_addr constant [4 x i8] c"%g\0A\00"
@fmt.12 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@fmt.13 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.14 = private unnamed_addr constant [4 x i8] c"%g\0A\00"
@fmt.15 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@fmt.16 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.17 = private unnamed_addr constant [4 x i8] c"%g\0A\00"
@fmt.18 = private unnamed_addr constant [4 x i8] c"%s\0A\00"

declare i32 @printf(i8*, ...)

define i32 @main() {
entry:
  %a = alloca [3000000 x double]*
  %output = alloca [3000000 x double]*
  %0 = call [3000000 x double]* @read_c(i8* getelementptr inbounds ([19 x i8], [19 x i8]* @path_name, i32 0, i32 0))
  store [3000000 x double]* %0, [3000000 x double]** %a
  %a1 = load [3000000 x double]*, [3000000 x double]** %a
  %adjust_brightness_result = call [3000000 x double]* @adjust_brightness([3000000 x double]* %a1, double 2.000000e+01)
  store [3000000 x double]* %adjust_brightness_result, [3000000 x double]** %output
  %image = load [3000000 x double]*, [3000000 x double]** %output
  call void @save_c(i8* getelementptr inbounds ([13 x i8], [13 x i8]* @path_name.3, i32 0, i32 0), [3000000 x double]* %image)
  ret i32 0
}

define [3000000 x double]* @beautify([3000000 x double]* %input, double %sm_ratio, double %br_ratio, double %con_ratio, double %sa_ratio) {
entry:
  %input1 = alloca [3000000 x double]*
  store [3000000 x double]* %input, [3000000 x double]** %input1
  %sm_ratio2 = alloca double
  store double %sm_ratio, double* %sm_ratio2
  %br_ratio3 = alloca double
  store double %br_ratio, double* %br_ratio3
  %con_ratio4 = alloca double
  store double %con_ratio, double* %con_ratio4
  %sa_ratio5 = alloca double
  store double %sa_ratio, double* %sa_ratio5
  %sm_ratio6 = load double, double* %sm_ratio2
  %image = load [3000000 x double]*, [3000000 x double]** %input1
  %0 = call [3000000 x double]* @smooth_c([3000000 x double]* %image, double %sm_ratio6)
  store [3000000 x double]* %0, [3000000 x double]** %input1
  %br_ratio7 = load double, double* %br_ratio3
  %input8 = load [3000000 x double]*, [3000000 x double]** %input1
  %adjust_brightness_result = call [3000000 x double]* @adjust_brightness([3000000 x double]* %input8, double %br_ratio7)
  store [3000000 x double]* %adjust_brightness_result, [3000000 x double]** %input1
  %con_ratio9 = load double, double* %con_ratio4
  %input10 = load [3000000 x double]*, [3000000 x double]** %input1
  %adjust_contrast_result = call [3000000 x double]* @adjust_contrast([3000000 x double]* %input10, double %con_ratio9)
  store [3000000 x double]* %adjust_contrast_result, [3000000 x double]** %input1
  %sa_ratio11 = load double, double* %sa_ratio5
  %image12 = load [3000000 x double]*, [3000000 x double]** %input1
  %1 = call [3000000 x double]* @saturation_c([3000000 x double]* %image12, double %sa_ratio11)
  store [3000000 x double]* %1, [3000000 x double]** %input1
  %input13 = load [3000000 x double]*, [3000000 x double]** %input1
  ret [3000000 x double]* %input13
}

define [3000000 x double]* @to_gray([3000000 x double]* %input) {
entry:
  %input1 = alloca [3000000 x double]*
  store [3000000 x double]* %input, [3000000 x double]** %input1
  %gray = alloca double
  %i = alloca i32
  %j = alloca i32
  %shape = alloca [3 x double]*
  %tmp_pixel = alloca [3 x double]*
  %output = alloca [3000000 x double]*
  %image = load [3000000 x double]*, [3000000 x double]** %input1
  %0 = call [2 x double]* @size_c([3000000 x double]* %image)
  %row_size = getelementptr [2 x double], [2 x double]* %0, i32 0, i32 0
  %number = load double, double* %row_size
  %col_size = getelementptr [2 x double], [2 x double]* %0, i32 0, i32 1
  %number2 = load double, double* %col_size
  %malloccall = tail call i8* @malloc(i32 trunc (i64 mul nuw (i64 ptrtoint (double* getelementptr (double, double* null, i32 1) to i64), i64 3) to i32))
  %tuple0 = bitcast i8* %malloccall to [3 x double]*
  %tuple1 = getelementptr [3 x double], [3 x double]* %tuple0, i32 0, i32 0
  store double %number, double* %tuple1
  %tuple2 = getelementptr [3 x double], [3 x double]* %tuple0, i32 0, i32 1
  store double %number2, double* %tuple2
  %tuple3 = getelementptr [3 x double], [3 x double]* %tuple0, i32 0, i32 2
  store double 0.000000e+00, double* %tuple3
  store [3 x double]* %tuple0, [3 x double]** %shape
  %image3 = load [3000000 x double]*, [3000000 x double]** %input1
  %1 = call [3000000 x double]* @copy_c([3000000 x double]* %image3)
  store [3000000 x double]* %1, [3000000 x double]** %output
  store i32 0, i32* %i
  br label %while

while:                                            ; preds = %merge, %entry
  %i50 = load i32, i32* %i
  %tuple851 = load [3 x double]*, [3 x double]** %shape
  %shape52 = getelementptr [3 x double], [3 x double]* %tuple851, i32 0, i32 0
  %tuple953 = load double, double* %shape52
  %cast_tmp54 = fptosi double %tuple953 to i32
  %tmp55 = icmp slt i32 %i50, %cast_tmp54
  br i1 %tmp55, label %while_body, label %merge56

while_body:                                       ; preds = %while
  store i32 0, i32* %j
  br label %while4

while4:                                           ; preds = %while_body5, %while_body
  %j42 = load i32, i32* %j
  %tuple843 = load [3 x double]*, [3 x double]** %shape
  %shape44 = getelementptr [3 x double], [3 x double]* %tuple843, i32 0, i32 1
  %tuple945 = load double, double* %shape44
  %cast_tmp46 = fptosi double %tuple945 to i32
  %tmp47 = icmp slt i32 %j42, %cast_tmp46
  br i1 %tmp47, label %while_body5, label %merge

while_body5:                                      ; preds = %while4
  %image6 = load [3000000 x double]*, [3000000 x double]** %input1
  %i7 = load i32, i32* %i
  %cast_tmp = sitofp i32 %i7 to double
  %j8 = load i32, i32* %j
  %cast_tmp9 = sitofp i32 %j8 to double
  %malloccall10 = tail call i8* @malloc(i32 trunc (i64 mul nuw (i64 ptrtoint (double* getelementptr (double, double* null, i32 1) to i64), i64 3) to i32))
  %tuple011 = bitcast i8* %malloccall10 to [3 x double]*
  %tuple112 = getelementptr [3 x double], [3 x double]* %tuple011, i32 0, i32 0
  store double %cast_tmp, double* %tuple112
  %tuple213 = getelementptr [3 x double], [3 x double]* %tuple011, i32 0, i32 1
  store double %cast_tmp9, double* %tuple213
  %tuple314 = getelementptr [3 x double], [3 x double]* %tuple011, i32 0, i32 2
  store double 0.000000e+00, double* %tuple314
  %2 = call [3 x double]* @get_pixel_c([3000000 x double]* %image6, [3 x double]* %tuple011)
  store [3 x double]* %2, [3 x double]** %tmp_pixel
  %tuple8 = load [3 x double]*, [3 x double]** %tmp_pixel
  %tmp_pixel15 = getelementptr [3 x double], [3 x double]* %tuple8, i32 0, i32 0
  %tuple9 = load double, double* %tmp_pixel15
  %tmp = fmul double %tuple9, 3.000000e-01
  %tuple816 = load [3 x double]*, [3 x double]** %tmp_pixel
  %tmp_pixel17 = getelementptr [3 x double], [3 x double]* %tuple816, i32 0, i32 1
  %tuple918 = load double, double* %tmp_pixel17
  %tmp19 = fmul double %tuple918, 5.900000e-01
  %tmp20 = fadd double %tmp, %tmp19
  %tuple821 = load [3 x double]*, [3 x double]** %tmp_pixel
  %tmp_pixel22 = getelementptr [3 x double], [3 x double]* %tuple821, i32 0, i32 2
  %tuple923 = load double, double* %tmp_pixel22
  %tmp24 = fmul double %tuple923, 1.100000e-01
  %tmp25 = fadd double %tmp20, %tmp24
  store double %tmp25, double* %gray
  %image26 = load [3000000 x double]*, [3000000 x double]** %output
  %i27 = load i32, i32* %i
  %cast_tmp28 = sitofp i32 %i27 to double
  %j29 = load i32, i32* %j
  %cast_tmp30 = sitofp i32 %j29 to double
  %malloccall31 = tail call i8* @malloc(i32 trunc (i64 mul nuw (i64 ptrtoint (double* getelementptr (double, double* null, i32 1) to i64), i64 3) to i32))
  %tuple032 = bitcast i8* %malloccall31 to [3 x double]*
  %tuple133 = getelementptr [3 x double], [3 x double]* %tuple032, i32 0, i32 0
  store double %cast_tmp28, double* %tuple133
  %tuple234 = getelementptr [3 x double], [3 x double]* %tuple032, i32 0, i32 1
  store double %cast_tmp30, double* %tuple234
  %tuple335 = getelementptr [3 x double], [3 x double]* %tuple032, i32 0, i32 2
  store double 0.000000e+00, double* %tuple335
  %gray36 = load double, double* %gray
  %gray37 = load double, double* %gray
  %gray38 = load double, double* %gray
  %malloccall39 = tail call i8* @malloc(i32 trunc (i64 mul nuw (i64 ptrtoint (double* getelementptr (double, double* null, i32 1) to i64), i64 3) to i32))
  %tuple4 = bitcast i8* %malloccall39 to [3 x double]*
  %tuple5 = getelementptr [3 x double], [3 x double]* %tuple4, i32 0, i32 0
  store double %gray36, double* %tuple5
  %tuple6 = getelementptr [3 x double], [3 x double]* %tuple4, i32 0, i32 1
  store double %gray37, double* %tuple6
  %tuple7 = getelementptr [3 x double], [3 x double]* %tuple4, i32 0, i32 2
  store double %gray38, double* %tuple7
  call void @write_pixel_c([3000000 x double]* %image26, [3 x double]* %tuple032, [3 x double]* %tuple4)
  %j40 = load i32, i32* %j
  %tmp41 = add i32 %j40, 1
  store i32 %tmp41, i32* %j
  br label %while4

merge:                                            ; preds = %while4
  %i48 = load i32, i32* %i
  %tmp49 = add i32 %i48, 1
  store i32 %tmp49, i32* %i
  br label %while

merge56:                                          ; preds = %while
  %output57 = load [3000000 x double]*, [3000000 x double]** %output
  ret [3000000 x double]* %output57
}

define [3000000 x double]* @adjust_contrast([3000000 x double]* %input, double %ratio) {
entry:
  %input1 = alloca [3000000 x double]*
  store [3000000 x double]* %input, [3000000 x double]** %input1
  %ratio2 = alloca double
  store double %ratio, double* %ratio2
  %ratio3 = load double, double* %ratio2
  %input4 = load [3000000 x double]*, [3000000 x double]** %input1
  %adjust_image_result = call [3000000 x double]* @adjust_image([3000000 x double]* %input4, double %ratio3, double 0.000000e+00)
  ret [3000000 x double]* %adjust_image_result
}

define [3000000 x double]* @adjust_brightness([3000000 x double]* %input, double %ratio) {
entry:
  %input1 = alloca [3000000 x double]*
  store [3000000 x double]* %input, [3000000 x double]** %input1
  %ratio2 = alloca double
  store double %ratio, double* %ratio2
  %ratio3 = load double, double* %ratio2
  %input4 = load [3000000 x double]*, [3000000 x double]** %input1
  %adjust_image_result = call [3000000 x double]* @adjust_image([3000000 x double]* %input4, double 1.000000e+00, double %ratio3)
  ret [3000000 x double]* %adjust_image_result
}

define [3000000 x double]* @adjust_image([3000000 x double]* %input, double %alpha, double %beta) {
entry:
  %input1 = alloca [3000000 x double]*
  store [3000000 x double]* %input, [3000000 x double]** %input1
  %alpha2 = alloca double
  store double %alpha, double* %alpha2
  %beta3 = alloca double
  store double %beta, double* %beta3
  %r = alloca double
  %g = alloca double
  %b = alloca double
  %i = alloca i32
  %j = alloca i32
  %shape = alloca [3 x double]*
  %tmp_pixel = alloca [3 x double]*
  %output = alloca [3000000 x double]*
  %image = load [3000000 x double]*, [3000000 x double]** %input1
  %0 = call [2 x double]* @size_c([3000000 x double]* %image)
  %row_size = getelementptr [2 x double], [2 x double]* %0, i32 0, i32 0
  %number = load double, double* %row_size
  %col_size = getelementptr [2 x double], [2 x double]* %0, i32 0, i32 1
  %number4 = load double, double* %col_size
  %malloccall = tail call i8* @malloc(i32 trunc (i64 mul nuw (i64 ptrtoint (double* getelementptr (double, double* null, i32 1) to i64), i64 3) to i32))
  %tuple0 = bitcast i8* %malloccall to [3 x double]*
  %tuple1 = getelementptr [3 x double], [3 x double]* %tuple0, i32 0, i32 0
  store double %number, double* %tuple1
  %tuple2 = getelementptr [3 x double], [3 x double]* %tuple0, i32 0, i32 1
  store double %number4, double* %tuple2
  %tuple3 = getelementptr [3 x double], [3 x double]* %tuple0, i32 0, i32 2
  store double 0.000000e+00, double* %tuple3
  store [3 x double]* %tuple0, [3 x double]** %shape
  %image5 = load [3000000 x double]*, [3000000 x double]** %input1
  %1 = call [3000000 x double]* @copy_c([3000000 x double]* %image5)
  store [3000000 x double]* %1, [3000000 x double]** %output
  store i32 0, i32* %i
  br label %while

while:                                            ; preds = %merge84, %entry
  %i87 = load i32, i32* %i
  %tuple888 = load [3 x double]*, [3 x double]** %shape
  %shape89 = getelementptr [3 x double], [3 x double]* %tuple888, i32 0, i32 0
  %tuple990 = load double, double* %shape89
  %cast_tmp91 = fptosi double %tuple990 to i32
  %tmp92 = icmp slt i32 %i87, %cast_tmp91
  br i1 %tmp92, label %while_body, label %merge93

while_body:                                       ; preds = %while
  store i32 0, i32* %j
  br label %while6

while6:                                           ; preds = %merge59, %while_body
  %j78 = load i32, i32* %j
  %tuple879 = load [3 x double]*, [3 x double]** %shape
  %shape80 = getelementptr [3 x double], [3 x double]* %tuple879, i32 0, i32 1
  %tuple981 = load double, double* %shape80
  %cast_tmp82 = fptosi double %tuple981 to i32
  %tmp83 = icmp slt i32 %j78, %cast_tmp82
  br i1 %tmp83, label %while_body7, label %merge84

while_body7:                                      ; preds = %while6
  %image8 = load [3000000 x double]*, [3000000 x double]** %input1
  %i9 = load i32, i32* %i
  %cast_tmp = sitofp i32 %i9 to double
  %j10 = load i32, i32* %j
  %cast_tmp11 = sitofp i32 %j10 to double
  %malloccall12 = tail call i8* @malloc(i32 trunc (i64 mul nuw (i64 ptrtoint (double* getelementptr (double, double* null, i32 1) to i64), i64 3) to i32))
  %tuple013 = bitcast i8* %malloccall12 to [3 x double]*
  %tuple114 = getelementptr [3 x double], [3 x double]* %tuple013, i32 0, i32 0
  store double %cast_tmp, double* %tuple114
  %tuple215 = getelementptr [3 x double], [3 x double]* %tuple013, i32 0, i32 1
  store double %cast_tmp11, double* %tuple215
  %tuple316 = getelementptr [3 x double], [3 x double]* %tuple013, i32 0, i32 2
  store double 0.000000e+00, double* %tuple316
  %2 = call [3 x double]* @get_pixel_c([3000000 x double]* %image8, [3 x double]* %tuple013)
  store [3 x double]* %2, [3 x double]** %tmp_pixel
  %alpha17 = load double, double* %alpha2
  %tuple8 = load [3 x double]*, [3 x double]** %tmp_pixel
  %tmp_pixel18 = getelementptr [3 x double], [3 x double]* %tuple8, i32 0, i32 2
  %tuple9 = load double, double* %tmp_pixel18
  %tmp = fmul double %alpha17, %tuple9
  %beta19 = load double, double* %beta3
  %tmp20 = fadd double %tmp, %beta19
  store double %tmp20, double* %r
  %r21 = load double, double* %r
  %tmp22 = fcmp ogt double %r21, 2.550000e+02
  br i1 %tmp22, label %then, label %else

merge:                                            ; preds = %else, %then
  %r23 = load double, double* %r
  %tmp24 = fcmp olt double %r23, 0.000000e+00
  br i1 %tmp24, label %then26, label %else27

then:                                             ; preds = %while_body7
  store double 2.550000e+02, double* %r
  br label %merge

else:                                             ; preds = %while_body7
  br label %merge

merge25:                                          ; preds = %else27, %then26
  %alpha28 = load double, double* %alpha2
  %tuple829 = load [3 x double]*, [3 x double]** %tmp_pixel
  %tmp_pixel30 = getelementptr [3 x double], [3 x double]* %tuple829, i32 0, i32 1
  %tuple931 = load double, double* %tmp_pixel30
  %tmp32 = fmul double %alpha28, %tuple931
  %beta33 = load double, double* %beta3
  %tmp34 = fadd double %tmp32, %beta33
  store double %tmp34, double* %g
  %g35 = load double, double* %g
  %tmp36 = fcmp ogt double %g35, 2.550000e+02
  br i1 %tmp36, label %then38, label %else39

then26:                                           ; preds = %merge
  store double 0.000000e+00, double* %r
  br label %merge25

else27:                                           ; preds = %merge
  br label %merge25

merge37:                                          ; preds = %else39, %then38
  %g40 = load double, double* %g
  %tmp41 = fcmp olt double %g40, 0.000000e+00
  br i1 %tmp41, label %then43, label %else44

then38:                                           ; preds = %merge25
  store double 2.550000e+02, double* %g
  br label %merge37

else39:                                           ; preds = %merge25
  br label %merge37

merge42:                                          ; preds = %else44, %then43
  %alpha45 = load double, double* %alpha2
  %tuple846 = load [3 x double]*, [3 x double]** %tmp_pixel
  %tmp_pixel47 = getelementptr [3 x double], [3 x double]* %tuple846, i32 0, i32 0
  %tuple948 = load double, double* %tmp_pixel47
  %tmp49 = fmul double %alpha45, %tuple948
  %beta50 = load double, double* %beta3
  %tmp51 = fadd double %tmp49, %beta50
  store double %tmp51, double* %b
  %b52 = load double, double* %b
  %tmp53 = fcmp ogt double %b52, 2.550000e+02
  br i1 %tmp53, label %then55, label %else56

then43:                                           ; preds = %merge37
  store double 0.000000e+00, double* %g
  br label %merge42

else44:                                           ; preds = %merge37
  br label %merge42

merge54:                                          ; preds = %else56, %then55
  %b57 = load double, double* %b
  %tmp58 = fcmp olt double %b57, 0.000000e+00
  br i1 %tmp58, label %then60, label %else61

then55:                                           ; preds = %merge42
  store double 2.550000e+02, double* %b
  br label %merge54

else56:                                           ; preds = %merge42
  br label %merge54

merge59:                                          ; preds = %else61, %then60
  %image62 = load [3000000 x double]*, [3000000 x double]** %output
  %i63 = load i32, i32* %i
  %cast_tmp64 = sitofp i32 %i63 to double
  %j65 = load i32, i32* %j
  %cast_tmp66 = sitofp i32 %j65 to double
  %malloccall67 = tail call i8* @malloc(i32 trunc (i64 mul nuw (i64 ptrtoint (double* getelementptr (double, double* null, i32 1) to i64), i64 3) to i32))
  %tuple068 = bitcast i8* %malloccall67 to [3 x double]*
  %tuple169 = getelementptr [3 x double], [3 x double]* %tuple068, i32 0, i32 0
  store double %cast_tmp64, double* %tuple169
  %tuple270 = getelementptr [3 x double], [3 x double]* %tuple068, i32 0, i32 1
  store double %cast_tmp66, double* %tuple270
  %tuple371 = getelementptr [3 x double], [3 x double]* %tuple068, i32 0, i32 2
  store double 0.000000e+00, double* %tuple371
  %r72 = load double, double* %r
  %g73 = load double, double* %g
  %b74 = load double, double* %b
  %malloccall75 = tail call i8* @malloc(i32 trunc (i64 mul nuw (i64 ptrtoint (double* getelementptr (double, double* null, i32 1) to i64), i64 3) to i32))
  %tuple4 = bitcast i8* %malloccall75 to [3 x double]*
  %tuple5 = getelementptr [3 x double], [3 x double]* %tuple4, i32 0, i32 0
  store double %r72, double* %tuple5
  %tuple6 = getelementptr [3 x double], [3 x double]* %tuple4, i32 0, i32 1
  store double %g73, double* %tuple6
  %tuple7 = getelementptr [3 x double], [3 x double]* %tuple4, i32 0, i32 2
  store double %b74, double* %tuple7
  call void @write_pixel_c([3000000 x double]* %image62, [3 x double]* %tuple068, [3 x double]* %tuple4)
  %j76 = load i32, i32* %j
  %tmp77 = add i32 %j76, 1
  store i32 %tmp77, i32* %j
  br label %while6

then60:                                           ; preds = %merge54
  store double 0.000000e+00, double* %b
  br label %merge59

else61:                                           ; preds = %merge54
  br label %merge59

merge84:                                          ; preds = %while6
  %i85 = load i32, i32* %i
  %tmp86 = add i32 %i85, 1
  store i32 %tmp86, i32* %i
  br label %while

merge93:                                          ; preds = %while
  %output94 = load [3000000 x double]*, [3000000 x double]** %output
  ret [3000000 x double]* %output94
}

declare [3000000 x double]* @read_c(i8*)

declare void @save_c(i8*, [3000000 x double]*)

declare [3000000 x double]* @smooth_c([3000000 x double]*, double)

declare [3000000 x double]* @saturation_c([3000000 x double]*, double)

declare [2 x double]* @size_c([3000000 x double]*)

declare noalias i8* @malloc(i32)

declare [3000000 x double]* @copy_c([3000000 x double]*)

declare [3 x double]* @get_pixel_c([3000000 x double]*, [3 x double]*)

declare void @write_pixel_c([3000000 x double]*, [3 x double]*, [3 x double]*)

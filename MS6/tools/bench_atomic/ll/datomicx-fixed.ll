target datalayout = "e-i64:64-v16:16-v32:32-n16:32:64"
target triple = "nvptx64-nvidia-cuda"

define void @atomicAddDoubleWithOffset(double* %address0, i32 %index, double %val) #0 {
  %1 = sext i32 %index to i64
  %2 = getelementptr inbounds double, double* %address0, i64 %1
  %3 = bitcast double* %2 to i64*
  %4 = load i64, i64* %3, align 8
  br label %5

  %old.0 = phi i64 [ %4, %0 ], [ %10, %5 ]
  %6 = bitcast i64 %old.0 to double
  ; %7 = fsub double %val, %6
  %7 = fadd double %6, %val
  %8 = bitcast double %7 to i64
  %9 = cmpxchg i64* %3, i64 %old.0, i64 %8 seq_cst seq_cst
  %10 = extractvalue { i64, i1 } %9, 0
  %11 = icmp eq i64 %old.0, %10
  br i1 %11, label %12, label %5

  ret void
}

attributes #0 = { alwaysinline inlinehint norecurse nounwind }

target datalayout = "e-i64:64-v16:16-v32:32-n16:32:64"
target triple = "nvptx64-nvidia-cuda"

define void @atomicAddDouble(double* %address, double %val) #0 {
  %1 = bitcast double* %address to i64*
  %2 = load i64, i64* %1, align 8
  br label %3

  %old.0 = phi i64 [ %2, %0 ], [ %8, %3 ]
  %4 = bitcast i64 %old.0 to double
  %5 = fadd double %4, %val
  %6 = bitcast double %5 to i64
  %7 = cmpxchg i64* %1, i64 %old.0, i64 %6 seq_cst seq_cst
  %8 = extractvalue { i64, i1 } %7, 0
  %9 = icmp eq i64 %old.0, %8
  br i1 %9, label %10, label %3

  ret void
}

attributes #0 = { alwaysinline inlinehint norecurse nounwind }

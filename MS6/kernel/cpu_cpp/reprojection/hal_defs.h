#pragma once
#include "vecalg.h"

using __double = Expr;
using __int = Expr;
using __bool = Expr;

using Matrix2x2 = MatrixSq<__double, 2>;
using Matrix3x3 = MatrixSq<__double, 3>;

using Matrix3x3a = Matrix3x3a_<__double>;
using Matrix2x2a = Matrix2x2a_<__double>;

using VectorF3 = arr<__double, 3>;
using VectorF = arr<__double, 2>;
using VectorI = arr<__int, 2>;

inline Matrix3x3a mkToWorld(__double refx, __double refy){
  return rotZ(refx) * rotY(refy);
}

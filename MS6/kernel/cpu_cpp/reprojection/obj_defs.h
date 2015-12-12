#pragma once
#include "vecalg.h"

using Matrix2x2 = MatrixSq<double, 2>;
using Matrix3x3 = MatrixSq<double, 3>;

using Matrix3x3a = Matrix3x3a_<double>;
using Matrix2x2a = Matrix2x2a_<double>;

using VectorF3 = arr<double, 3>;
using VectorF = arr<double, 2>;
using VectorI = arr<int, 2>;

inline Matrix3x3a mkToWorld(double refx, double refy){
  return rotZ(refx) * rotY(refy);
}

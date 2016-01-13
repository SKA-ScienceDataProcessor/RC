
#ifndef HALIDE_UTILS_H
#define HALIDE_UTILS_H

// Linear algebra primitives, expressed as wrapper on Halide tuples
#define _USE_MATH_DEFINES
#include <math.h>
#include <Halide.h>
using namespace Halide;

// Pi. We do not use a constant, as Halide cannot express
// double-precision constants.
inline Expr pi() { return 4 * atan(cast<double>(1)); }

// Complex numbers
enum ComplxFields { _REAL = 0, _IMAG,  _CPLX_FIELDS };
struct Complex {
  Expr real, imag;
  Complex(Expr r, Expr i) : real(r), imag(i) {}
  Complex(FuncRefVar f) : real(Tuple(f)[0]), imag(Tuple(f)[1]) {}
  Complex(FuncRefExpr f) : real(Tuple(f)[0]), imag(Tuple(f)[1]) {}
  Complex operator+(const Complex &other) const {
    return{ real + other.real, imag + other.imag };
  }
  Complex operator*(const Complex &other) const {
    return{
        real * other.real - imag * other.imag
      , real * other.imag + imag * other.real
      };
  }
  Expr unpack(Expr c) { return select(c == _REAL, real, imag); }
  operator Tuple() { return Tuple(real, imag); }
};

inline Complex polar(Expr r, Expr phi) {
  return Complex(r * cos(phi), r * sin(phi));
}

// 3 vector
enum Vector3Field { _A1,_A2,_A3, _VECTOR_FIELDS };
struct Vector3 {
  Expr fields[_VECTOR_FIELDS];
  Vector3(Expr a1, Expr a2, Expr a3)
      : fields{ a1, a2, a3 }
    { }
  Vector3(FuncRefVar f)
      : fields{ Tuple(f)[_A1], Tuple(f)[_A2], Tuple(f)[_A3] }
    { }
  Vector3(FuncRefExpr f)
      : fields{ Tuple(f)[_A1], Tuple(f)[_A2], Tuple(f)[_A3] }
    { }
  Expr operator [](int i) const { return fields[i]; }
  operator Tuple() {
    return Tuple(fields[_A1], fields[_A2], fields[_A3]);
  }
  Expr unpack(Expr c) {
    return select(c == _A1, fields[_A1],
                  c == _A2, fields[_A2],
                            fields[_A3]);
  }
  Vector3 operator -(const Vector3 &v) const {
    return { fields[_A1] - v[_A1], fields[_A2] - v[_A2], fields[_A3] - v[_A3] };
  }
  Expr operator *(const Vector3 &v) const {
    return fields[_A1] * v[_A1] + fields[_A2] * v[_A2] + fields[_A3] * v[_A3];
  }
};

// 3x3 matrix
enum MatrixField { _A11,_A12,_A13, _A21,_A22,_A23, _A31,_A32,_A33, _MATRIX_FIELDS };
struct Matrix {
  Expr fields[_MATRIX_FIELDS];
  Matrix(Expr a11, Expr a12, Expr a13, Expr a21, Expr a22, Expr a23, Expr a31, Expr a32, Expr a33)
      : fields{ a11, a12, a13, a21, a22, a23, a31, a32, a33 }
    { }
  Matrix(Tuple f)
      : fields{ Tuple(f)[_A11], Tuple(f)[_A12], Tuple(f)[_A13]
              , Tuple(f)[_A21], Tuple(f)[_A22], Tuple(f)[_A23]
              , Tuple(f)[_A31], Tuple(f)[_A32], Tuple(f)[_A33]
              }
    { }
  Matrix(FuncRefVar f)
      : fields{ Tuple(f)[_A11], Tuple(f)[_A12], Tuple(f)[_A13]
              , Tuple(f)[_A21], Tuple(f)[_A22], Tuple(f)[_A23]
              , Tuple(f)[_A31], Tuple(f)[_A32], Tuple(f)[_A33]
              }
    { }
  Expr operator [](int i) const { return fields[i]; }
  operator Tuple() {
    return Tuple(fields[_A11], fields[_A12], fields[_A13],
                 fields[_A21], fields[_A22], fields[_A23],
                 fields[_A31], fields[_A32], fields[_A33]);
  }
  Matrix transpose() const {
    return Matrix(fields[_A11], fields[_A21], fields[_A31],
                  fields[_A12], fields[_A22], fields[_A32],
                  fields[_A13], fields[_A23], fields[_A33]);
  }
  Matrix operator*(const Matrix &m) const {
    return { fields[_A11]*m[_A11]+fields[_A12]*m[_A21]+fields[_A13]*m[_A31]
           , fields[_A11]*m[_A12]+fields[_A12]*m[_A22]+fields[_A13]*m[_A32]
           , fields[_A11]*m[_A13]+fields[_A12]*m[_A23]+fields[_A13]*m[_A33]

           , fields[_A21]*m[_A11]+fields[_A22]*m[_A21]+fields[_A23]*m[_A31]
           , fields[_A21]*m[_A12]+fields[_A22]*m[_A22]+fields[_A23]*m[_A32]
           , fields[_A21]*m[_A13]+fields[_A22]*m[_A23]+fields[_A23]*m[_A33]

           , fields[_A31]*m[_A11]+fields[_A32]*m[_A21]+fields[_A33]*m[_A31]
           , fields[_A31]*m[_A12]+fields[_A32]*m[_A22]+fields[_A33]*m[_A32]
           , fields[_A31]*m[_A13]+fields[_A32]*m[_A23]+fields[_A33]*m[_A33]
           };
  }
  Vector3 operator*(const Vector3 &v) const {
    return { fields[_A11]*v[_A1]+fields[_A12]*v[_A2]+fields[_A13]*v[_A3]
           , fields[_A21]*v[_A1]+fields[_A22]*v[_A2]+fields[_A23]*v[_A3]
           , fields[_A31]*v[_A1]+fields[_A32]*v[_A2]+fields[_A33]*v[_A3]
           };
  }
  // Routines treating this as a 2x2 matrix. Third row and column are
  // always set to the identity matrix.
  Matrix inverse2x2() const {
    Expr det = (fields[_A11] * fields[_A22]) - (fields[_A12] * fields[_A21]);
    return Matrix( fields[_A22]/det, -fields[_A12] / det, cast<double>(0),
                  -fields[_A21]/det,  fields[_A11] / det, cast<double>(0),
                   cast<double>(0),   cast<double>(0),    cast<double>(1));
  }
};

inline Matrix selectMtx(Expr condition, Matrix true_value, Matrix false_value) {
  std::vector<Expr> out;
  for (int i = _A11; i <= _A33; i++) {
    out.push_back(select(condition, true_value[i], false_value[i]));
  }
  return Matrix(Tuple(out));
}

// Standard rotation matrixes (adapted from Chris Skipper's code, assuming radians)
inline Matrix rotateX(Expr phi) {
  Expr s = sin(phi), c = cos(phi);
  return Matrix(1,0,0, 0,c,-s, 0,s,c);
}
inline Matrix rotateY(Expr phi) {
  Expr s = sin(phi), c = cos(phi);
  return Matrix(c,0,-s, 0,1,0, s,0,c);
}
inline Matrix rotateZ(Expr phi) {
  Expr s = sin(phi), c = cos(phi);
  return Matrix(c,-s,0, s,c,0, 0,0,1);
}

// Convert between UVW and world coordinates (again, see Chris
// Skipper's code for reference)
inline Matrix xyz2uvw(Expr lon, Expr lat) {
  return rotateX(-(pi()/2)+lat)*rotateZ(-cast<double>(pi()/2)-lon);
}
inline Matrix uvw2xyz(Expr lon, Expr lat) {
  return rotateZ((pi()/2)+lon)*rotateX(cast<double>(pi()/2)-lat);
}

#endif // HALIDE_UTILS_H

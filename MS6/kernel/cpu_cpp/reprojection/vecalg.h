#pragma once

#include <array>
#include <cmath>

template <typename T, size_t n> using arr = std::array<T, n>;
template <typename T, typename T1, typename T2> using bop = T (&) (T1, T2);
template <typename T, size_t n> using MatrixSq = T[n][n];

// Not too generic
template <size_t i> struct Iter {
  template <typename T, size_t n, size_t stride>
  static T dot(const T * l, const T * r){
    return l[n-i] * r[(n-i)*stride] + Iter<i-1>::template dot<T,n,stride>(l, r);
  }
  template <typename T, size_t n, size_t stride>
  static void m2v(T * res, const MatrixSq<T, n> & m, const T * v){
    res[(n-i)*stride] = Iter<n>::template dot<T, n,stride>(m[n-i], v);
    Iter<i-1>::template m2v<T, n, stride>(res, m, v);
  }
  template <typename T, size_t n>
  static void m2m(T * res, const MatrixSq<T, n> & m, const T * v){
  	m2v<T,n,n>(res+(n-i), m, v+(n-i));
    Iter<i-1>::m2m(res, m, v);
  }
  template <typename T, typename T1, typename T2, size_t n, bop<T, T1, T2> op>
  static void zip(T * res, const T1 * l, const T2 * r){
    res[n-i] = op(l[n-i], r[n-i]);
    Iter<i-1>::template zip<T, T1, T2, n, op>(res, l, r);
  }
};

template <> struct Iter<1> {
  template <typename T, size_t n, size_t stride>
  static T dot(const T * l, const T * r){
    return l[n-1] * r[(n-1)*stride];
  }
  template <typename T, size_t n, size_t stride>
  static void m2v(T * res, const MatrixSq<T, n> & m, const T * v){
    res[(n-1)*stride] = Iter<n>::template dot<T, n, stride>(m[n-1], v);
  }
  template <typename T, size_t n>
  static void m2m(T * res, const MatrixSq<T, n> & m, const T * v){
  	m2v<T,n,n>(res+(n-1), m, v+(n-1));
  }
  template <typename T, typename T1, typename T2, size_t n, bop<T, T1, T2> op>
  static void zip(T * res, const T1 * l, const T2 * r){
    res[n-1] = op(l[n-1], r[n-1]);
  }
};

template <typename T, size_t n>
inline void mxv(arr<T, n> & res, const MatrixSq<T, n> & m, const arr<T, n> & v){
  Iter<n>::template m2v<T,n,1>(res.data(), m, v.data());
}

#define __MKZIP(nm, op)                                                              \
template <typename T, typename T1, typename T2> T __##nm(T1 l, T2 r){return l op r;} \
template <typename T, typename T1, typename T2, size_t n>                            \
inline void nm(arr<T, n> & res, const arr<T1, n> & l, const arr<T2, n> & r){         \
  Iter<n>::template zip<T, T1, T2, n, __##nm>(res.data(), l.data(), r.data());       \
}

__MKZIP(add, +)
__MKZIP(sub, -)

using VectorF3 = arr<double, 3>;
using VectorF = arr<double, 2>;
using VectorI = arr<int, 2>;

using Matrix2x2 = MatrixSq<double, 2>;
using Matrix3x3 = MatrixSq<double, 3>;

enum Coordinates {x, y, z};

template <typename T>
inline
VectorF operator -(const VectorF & l, const arr<T,2> & r){
  VectorF ret;
  sub(ret, l, r);
  return ret;
}

template <typename T>
inline
VectorF operator +(const VectorF & l, const arr<T,2> & r){
  VectorF ret;
  add(ret, l, r);
  return ret;
}

template <size_t n>
inline
arr<double, n> operator *(const MatrixSq<double,n> & m, const arr<double,n> & v){
  arr<double, n> ret;
  mxv(ret, m, v);
  return ret;
}

template <size_t n> using MatrixA = arr<double, n*n>;

template <size_t n>
inline
MatrixA<n> mmul(const MatrixA<n> & m1, const MatrixA<n> & m2){
  MatrixA<n> ret;
  Iter<n>::m2m(ret.data(), reinterpret_cast<const MatrixSq<double, n>&>(m1), m2.data());
  return ret;
}

using Matrix3x3a = MatrixA<3>;

inline
Matrix3x3a operator *(const Matrix3x3a & m1, const Matrix3x3a & m2){ return mmul<3>(m1, m2); }

using Matrix2x2a = MatrixA<2>;

inline
Matrix2x2a operator *(const Matrix2x2a & m1, const Matrix2x2a & m2){ return mmul<2>(m1, m2); }

#ifdef __TEST
void test_add(VectorF3 & res, const VectorF3 & l, const VectorF3 & r){
  return add(res, l, r);
}

void test_sub(VectorF3 & res, const VectorF3 & l, const VectorF3 & r){
  return sub(res, l, r);
}

void test_mul(VectorF3 & res, const Matrix3x3 & m, const VectorF3 & v){
  return mxv(res, m, v);
}
#endif

#ifdef _MSC_VER
inline void sincos (double x, double *p_sin, double *p_cos){
  *p_sin = sin(x);
  *p_cos = cos(x);
}
#endif

inline Matrix3x3a rotX(double p){
  double s, c;
  sincos(p, &s, &c);
  return {
      1, 0, 0
    , 0, c,-s
    , 0, s, c
    };
}

inline Matrix3x3a rotY(double p){
  double s, c;
  sincos(p, &s, &c);
  return {
      c, 0,-s
    , 0, 1, 0
    , s, 0, c
    };
}

inline Matrix3x3a rotZ(double p){
  double s, c;
  sincos(p, &s, &c);
  return {
      c,-s, 0
    , s, c, 0
    , 0, 0, 1
    };
}

inline Matrix3x3a mkToWorld(double refx, double refy){
  return rotZ(refx) * rotY(refy);
}

template <typename T, size_t n>
inline void transpose(MatrixSq<T, n> & t, const MatrixSq<T, n> & o){
  for(size_t r = 0; r<n; r++) {
    t[r][r] = o[r][r];
    for(size_t c = r+1; c < n; c++) {
      t[r][c] = o[c][r];
      t[c][r] = o[r][c];
    }
  }
}

inline Matrix3x3a transpose(const Matrix3x3a & o){
  Matrix3x3a t;
  transpose(reinterpret_cast<Matrix3x3&>(t), reinterpret_cast<const Matrix3x3&>(o));
  return t;
}

inline void inverse(Matrix2x2 & i, const Matrix2x2 & m)
{
  double det = m[0][0]*m[1][1]-m[0][1]*m[1][0];
  i[0][0] =  m[1][1]/det;
  i[0][1] = -m[0][1]/det;
  i[1][0] = -m[1][0]/det;
  i[1][1] =  m[0][0]/det;
}

inline Matrix2x2a inverse(const Matrix2x2a & m)
{
  double det = m[0]*m[3]-m[1]*m[2];
  return {m[3]/det, -m[1]/det, -m[2]/det, m[0]/det};
}

#ifdef __TEST
#include <iostream>

int main(){
  auto r = rotX(0);
  for(auto i : r * r) std::cout << i << " ";

  Matrix2x2a d2({2, 0, 0, 2});
  std::cout << std::endl;
  for(auto i : d2 * inverse(d2)) std::cout << i << " ";
  std::cout << std::endl;
  for(auto i : inverse(d2) * d2) std::cout << i << " ";

  std::cout << std::endl;
  Matrix3x3a o = {
  	0, 1, 2
  , 3, 4, 5
  , 6, 7, 8
  };
  for(auto i : transpose(o)) std::cout << i << " ";
}
#endif

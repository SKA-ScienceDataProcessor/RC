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

enum Coordinates {x, y, z};

template <typename T1, typename T2, size_t n>
inline
arr<T1, n> operator -(const arr<T1, n> & l, const arr<T2, n> & r){
  arr<T1, n> ret;
  sub(ret, l, r);
  return ret;
}

template <typename T1, typename T2, size_t n>
inline
arr<T1, n> operator +(const arr<T1, n> & l, const arr<T2, n> & r){
  arr<T1, n> ret;
  add(ret, l, r);
  return ret;
}

template <typename T, size_t n>
inline
arr<T, n> operator *(const MatrixSq<T,n> & m, const arr<T,n> & v){
  arr<T, n> ret;
  mxv(ret, m, v);
  return ret;
}

template <typename T, size_t n> using MatrixA = arr<T, n*n>;

template <typename T, size_t n>
inline
MatrixA<T,n> mmul(const MatrixA<T,n> & m1, const MatrixA<T,n> & m2){
  MatrixA<T,n> ret;
  Iter<n>::m2m(ret.data(), reinterpret_cast<const MatrixSq<T, n>&>(m1), m2.data());
  return ret;
}

#define __MUL_SPEC_N(n) \
  template <typename T> \
  inline MatrixA<T,n> operator *(const MatrixA<T,n> & m1, const MatrixA<T,n> & m2){ return mmul<T, n>(m1, m2); }

__MUL_SPEC_N(3)
__MUL_SPEC_N(2)


template <typename T> using Matrix3x3a_ = MatrixA<T, 3>;
template <typename T> using Matrix2x2a_ = MatrixA<T, 2>;

template <typename T> 
inline void sincos (T x, T *p_sin, T *p_cos){
  *p_sin = sin(x);
  *p_cos = cos(x);
}

template <typename T>
inline Matrix3x3a_<T> rotX(T p){
  T s, c;
  sincos(p, &s, &c);
  return {
      1, 0, 0
    , 0, c,-s
    , 0, s, c
    };
}

template <typename T>
inline Matrix3x3a_<T> rotY(T p){
  T s, c;
  sincos(p, &s, &c);
  return {
      c, 0,-s
    , 0, 1, 0
    , s, 0, c
    };
}

template <typename T>
inline Matrix3x3a_<T> rotZ(T p){
  T s, c;
  sincos(p, &s, &c);
  return {
      c,-s, 0
    , s, c, 0
    , 0, 0, 1
    };
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

template <typename T>
inline Matrix3x3a_<T> transpose(const Matrix3x3a_<T> & o){
  Matrix3x3a_<T> t;
  transpose(reinterpret_cast<MatrixSq<T,3>&>(t), reinterpret_cast<const MatrixSq<T,3>&>(o));
  return t;
}

template <typename T>
inline void inverse(MatrixSq<T,2> & i, const MatrixSq<T,2> & m)
{
  T det = m[0][0]*m[1][1]-m[0][1]*m[1][0];
  i[0][0] =  m[1][1]/det;
  i[0][1] = -m[0][1]/det;
  i[1][0] = -m[1][0]/det;
  i[1][1] =  m[0][0]/det;
}

template <typename T>
inline Matrix2x2a_<T> inverse(const Matrix2x2a_<T> & m)
{
  T det = m[0]*m[3]-m[1]*m[2];
  return {m[3]/det, -m[1]/det, -m[2]/det, m[0]/det};
}

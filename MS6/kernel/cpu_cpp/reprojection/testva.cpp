#include <iostream>

#include "obj_defs.h"

void test_add(VectorF3 & res, const VectorF3 & l, const VectorF3 & r){
  return add(res, l, r);
}

void test_sub(VectorF3 & res, const VectorF3 & l, const VectorF3 & r){
  return sub(res, l, r);
}

void test_mul(VectorF3 & res, const Matrix3x3 & m, const VectorF3 & v){
  return mxv(res, m, v);
}

template <typename T>
inline Matrix2x2a_<T> inverse(const Matrix2x2a_<T> & m)
{
  T det = m[0]*m[3]-m[1]*m[2];
  return {m[3]/det, -m[1]/det, -m[2]/det, m[0]/det};
}

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

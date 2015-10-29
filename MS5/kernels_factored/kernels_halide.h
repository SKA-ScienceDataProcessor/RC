#include "Halide.h"
using namespace Halide;

const int
    WIDTH = 8192
  , HEIGHT = 8192
  , GCF_SIZE = 16
  , OVER = 8
  ;

// infer_arguments looks broken or incomplete
//   thus we add them explicitly
typedef std::pair<std::vector<Argument>, Func> FullFunc;
FullFunc genInit(), genFFTO0(), genScatter(bool reorderRDom);

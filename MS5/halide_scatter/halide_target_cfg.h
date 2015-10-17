#ifndef __HALIDE_TARGET_CFG_H
#define __HALIDE_TARGET_CFG_H

#ifdef _WIN32
#define __HALIDE_HOST Windows
#else
#define __HALIDE_HOST Linux
#endif

#ifdef __AVX__
#define	__HALIDE_ADD_AVX__ , Target::AVX
#else
#define	__HALIDE_ADD_AVX__
#endif

#endif

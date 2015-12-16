/*
  Adapted from Chris Skipper's code.
  We use the code from 'crocodile' repo.
  More complex version from Confluence segfaults.

  Difference from bare 'reprojection' variant:
  we fuse  worldToPixel . pixelToWorld explicitly into the single 'p2p'
  and precalculate cvt=in.toPixel*out.fromPixel matrix explicitly.

  Translated to Halide.
 */
#define _USE_MATH_DEFINES 1
#include <cmath>
#include <utility>

#ifdef _TEST
#include <vector>
#include "bmp_utils.h"
#endif

#include "Halide.h"
using namespace Halide;
#include "hal_defs.h"

#include "mkHalideBuf.h"

using namespace std;

const __double rScale = cast<double>(Expr(180.0 * M_1_PI));
const __double __M_PI = cast<double>(Expr(M_PI));
const __double __M_PI_2 = cast<double>(Expr(M_PI_2));
#define __CAST(t,a) cast<t>(a)

#define DE(a) __CAST(double,Expr(a))
#define Z DE(0.0f)

struct Converter {
  VectorI out_crPix, in_crPix;
  Matrix2x2 cd, in_cd, inv_cd;
  Matrix3x3 cvt;

  Converter(
      const VectorI & outcrPix
    , const VectorI & incrPix
    , const VectorF & outcrVal
    , const VectorF & incrVal
    , const Matrix2x2 & outcd
    , const Matrix2x2 & incd
  	) : out_crPix(outcrPix), in_crPix(incrPix) {
  	  auto scale = [](__double v){return v * rScale;};
  	  transform(&outcd[0][0], &outcd[2][0], &cd[0][0], scale);
  	  transform(&incd[0][0], &incd[2][0], &in_cd[0][0], scale);
      inverse(inv_cd, in_cd);
      Matrix3x3a r = transpose(mkToWorld(incrVal[x], incrVal[y])) * mkToWorld(outcrVal[x], outcrVal[y]);
      copy(r.begin(), r.end(), &cvt[0][0]);
    }
};

// define a maximum number of interpolation points for each output pixel. this is the maximum along each axis, so the actual
// number of interpolation points is n*n per pixel.
const int MAX_INTERPOLATION_POINTS = 10;

inline __double enorm(__double x, __double y){
  return sqrt(pow(x, 2) + pow(y, 2));
}

/* NOTE1: Ignore it ATM
__double pixelAreaRatio(const Converter & c){
  __double
      inPixelWidth   = enorm(c.in_cd[x][x], c.in_cd[y][x])
    , outPixelWidth  = enorm(c.   cd[x][x], c.   cd[y][x])
    , inPixelHeight  = enorm(c.in_cd[x][y], c.in_cd[y][y])
    , outPixelHeight = enorm(c.   cd[x][y], c.   cd[y][y])
    , inPixelArea = inPixelWidth * inPixelHeight
    , outPixelArea = outPixelWidth * outPixelHeight
    ;
  return outPixelArea / inPixelArea;
} */

inline
std::pair<VectorF, __bool> p2p(const VectorF & pPixelPosition, const Converter & c )
{
  // apply coordinate system CD transformation matrix.
  VectorF intermediatePosition = c.cd * (pPixelPosition - c.out_crPix);

  // skew the image using the declination. this step reflects the fact that lines of RA get closer together
  // as the image gets nearer to +/- 90 deg declination. this transformation effectively converts from angular
  // distance in the ra direction to actual ra coordinates.
  VectorF worldOffset;
  worldOffset[x] = intermediatePosition[x] / cos( intermediatePosition[y] );
  worldOffset[y] = intermediatePosition[y];

  // check for wrap around, and set the flag.
  __bool wrapAround = worldOffset[x] < -__M_PI || worldOffset[x] > __M_PI || worldOffset[y] < -__M_PI_2 || worldOffset[y] > __M_PI_2;

  // get x, y and z cartesian coordinates.
  VectorF3 cartesianOffset;
  __double xs, xc, ys, yc;
  sincos(worldOffset[x], &xs, &xc);
  sincos(worldOffset[y], &ys, &yc);

  cartesianOffset[x] = xc * yc;
  cartesianOffset[y] = xs * yc;
  cartesianOffset[z] = ys;

  // the world offset coordinates are relative to the reference pixel, which is currently at ra 0, dec 0. we need to
  // rotate the offset coordinates by the reference pixel's true ra and dec so that they are relative to ra 0, dec 0.
  // unfortunately, the dec rotation has to be done in cartesian coordinates, which are rather messy to convert back
  // to spherical.
  VectorF3 cartesianOffset2 = c.cvt * cartesianOffset;

  // we now need to convert back into polar coordinates.
  VectorF intermediatePosition2 = { atan2( cartesianOffset2[y], cartesianOffset2[x] ), asin( cartesianOffset2[z] ) };

  // skew the image using the declination. this step reflects the fact that lines of RA get closer together
  // as the image gets nearer to +/- 90 deg declination.
  intermediatePosition2[x] = intermediatePosition2[x] * cos( intermediatePosition2[y] );

  // add reference pixel coordinates.
  return make_pair(c.inv_cd * intermediatePosition2 + c.in_crPix, wrapAround);
}

//
//  REPROJECTION AND REGRIDDING FUNCTIONS
//

//
//  interpolateValue()
//
//  CJS:  08/07/15
//
//  use 'pPosition' to do bilinear interpolation between 4 data points.
//
__double interpolateValue( VectorF pPosition, __double pBLValue, __double pBRValue, __double pTLValue, __double pTRValue )
{

  // subtract the integer part of the position. we don't need this here.
  VectorI integerPart = { __CAST(int,floor( pPosition[x] )), __CAST(int,floor( pPosition[y] )) };
  VectorF fraction = { pPosition[x] - (__double)integerPart[x], pPosition[y] - (__double)integerPart[y] };

  // interpolate top and bottom in the x-direction.
  __double valueTop = ((pTRValue - pTLValue) * fraction[x]) + pTLValue;
  __double valueBottom = ((pBRValue - pBLValue) * fraction[x]) + pBLValue;

  // interpolate in y-direction.
  return ((valueTop - valueBottom) * fraction[y]) + valueBottom;

} // interpolateValue

//
//  reprojection()
//
//  CJS: 07/07/2015
//
//  Performs regridding and reprojection between the input and output images.
//  We need to handle the case where the output image pixels are much larger than the input image pixels (we need
//  to sum over many pixels), and also when the output image pixels are much smaller than the input image pixels
//  (we need to interpolate between input image pixels).
//
//  This routine works by comparing the size of the input and output image pixels, and choosing a number of
//  interpolation points for each output pixel. For example, overlaying the input and output images in world
//  coordinates may give:
//
//    +--------+--------+--------+--------+
//    |        |        |        |        | +---+
//    |        |#   #   #   #   #|  #     | |   | = input image pixels
//    |        |        |                 | +---+
//    +--------+#-------+--------+--#-----+
//    |        |     X====X====X |        | +===+
//    |        |#    I  |      I |  #     | I   I = output image pixel
//    |        |     X  | X    X |        | +===+
//    +--------+#----I--+------I-+--#-----+
//    |        |     X====X====X |        | # # #   region centred on the output image pixel, that extends on
//    |        |#       |        |  #     | #   # = all four sides to the surrounding output image pixels. this
//    |        |        |        |        | # # #   is the region we sum over.
//    +--------+#---#---#---#---#+--#-----+
//    |        |        |        |        |   X = interpolation point. the centre point has weight 1, the ones
//    |        |        |        |        |         along side it have weight 0.5, and the
//    |        |        |        |        |         ones on the diagonals have weight 0.25.
//    +--------+--------+--------+--------+
//
//  The program uses bilinear interpolation to calculate the value of the input grid at each interpolation point. These
//  values are then summed using a weighting that depends upon the position of the interpolation point relative to the output
//  pixel (the centre of the output pixel has weight 1, and this drops to 0 as we near the adjacent output pixels). If the
//  output pixel is small compared to the input pixels then we use a small number of interpolation points (one would do the
//  job, but we use a minimum of 3x3). If the output pixel is large compared to the input pixels then we use many
//  interpolation points (enough to ensure that at least one interpolation point is found within each fully-enclosed input
//  pixel).
//
void reprojection(
    const Converter & c
  , const ImageParam _inputImage
  , Func & _outputImage
)
{
  const VectorI _inSize{_inputImage.extent(0),_inputImage.extent(1)};
  enum pos {
      POS_BL
    , POS_BR
    , POS_TL
    , POS_TR
    , NUM_POS
  };

  Var i("i");
    Var j("j");

      // we need to find the coordinates of the 4 output pixels that diagonally surround our
      // pixel, so that we can choose some interpolation points within this region of the input image.
      VectorF outPixelCoordinate[NUM_POS];
      VectorF inPixelCoordinate[NUM_POS];

      outPixelCoordinate[POS_BL][x] = i - 1; outPixelCoordinate[POS_BL][y] = j - 1;
      outPixelCoordinate[POS_BR][x] = i + 1; outPixelCoordinate[POS_BR][y] = j - 1;
      outPixelCoordinate[POS_TL][x] = i - 1; outPixelCoordinate[POS_TL][y] = j + 1;
      outPixelCoordinate[POS_TR][x] = i + 1; outPixelCoordinate[POS_TR][y] = j + 1;

      // convert each of these four pixels to the input coordinate system.
      __bool wrapAround = cast<bool>(false);
      for ( int p = 0; p < NUM_POS; p++ ) {
        auto res = p2p( outPixelCoordinate[p], c );
        inPixelCoordinate[p] = res.first;
        wrapAround = wrapAround || res.second;
      }

      Func pixelValue("pixelValue");
        // the input pixel coordinates will map out same shape in the input image, which will not necessarily
        // be square. we need to find the limits in both x and y, so that we can choose an appropriate
        // number of interpolation points within the region.
        VectorF minc = inPixelCoordinate[POS_BL], maxc = inPixelCoordinate[POS_BL];
        for ( int p = 1; p < NUM_POS; p++ )
        {

          minc[x] = min( minc[x], inPixelCoordinate[p][x] );
          maxc[x] = max( maxc[x], inPixelCoordinate[p][x] );
          minc[y] = min( minc[y], inPixelCoordinate[p][y] );
          maxc[y] = max( maxc[y], inPixelCoordinate[p][y] );
        }

        // find the size of the input image region.
        __int regionSize = max(
           __CAST(int, floor( maxc[x] )) - __CAST(int, floor( minc[x] ))
         , __CAST(int, floor( maxc[y] )) - __CAST(int, floor( minc[y] ))
         );

        // the input image pixels could be much larger than our region, or they could be much smaller.
        // we use the region size to define a number of interpolation points, which form a NxN grid
        // around our region of the input image. Each input pixel should have at least one interpolation
        // point.
        __int interpolationPoints = ((regionSize / 2) + 1) * NUM_POS;

        interpolationPoints = select( interpolationPoints < 2, 2
                                    , interpolationPoints > MAX_INTERPOLATION_POINTS, MAX_INTERPOLATION_POINTS
                                    , interpolationPoints
                                    );
        
        // loop through all the interpolation points. we don't bother with the first or last
        // interpolation points, since they have weight 0.
          RDom ipoints(
              0, MAX_INTERPOLATION_POINTS // interpolationPoints
            , 0, MAX_INTERPOLATION_POINTS // interpolationPoints
            );
          RVar k = ipoints[x], l = ipoints[y];
          // We can't have variable RDom, thus this check:
          // auto valid_pt = [k,l,interpolationPoints](){return k < interpolationPoints && l < interpolationPoints;};
          // simple #define if better
          #define valid_pt() k < interpolationPoints && l < interpolationPoints

          Func totalValue("totalValue");
          totalValue(i, j) = __CAST(double, 0.0f);
          Func totalWeight("totalWeight");
          totalWeight(i, j) = __CAST(double, 0.0f);

            // calculate the position of this interpolation point as a fraction
            // of the output pixel size (the centre of the pixel will be at <0.5, 0.5>).
            VectorF fraction = {
            	  __CAST(double,k) / __CAST(double,interpolationPoints)
            	, __CAST(double,l) / __CAST(double,interpolationPoints)
                };

            // calculate the weight of this interpolation point. this is based upon its
            // position - the centre of the region has weight 1, and the edges weight 0.
            VectorF weight = {
            	   select(fraction[x] <= __CAST(double, 0.5f), fraction[x] * 2, 2 - (fraction[x] * 2))
            	 , select(fraction[y] <= __CAST(double, 0.5f), fraction[y] * 2, 2 - (fraction[y] * 2))
                 };

            __double interpolationWeight = (weight[x] * weight[y]);

            // get the position of the interpolation point in the output image.
            VectorF outPixelInterpolationPoint = {
                __CAST(double, i - 1) + (fraction[x] * 2)
              , __CAST(double, j - 1) + (fraction[y] * 2)
              };

            // convert the world coordinates back into input pixel coordinates.
            VectorF inPixelInterpolationPoint = p2p( outPixelInterpolationPoint, c ).first;

            // calculate the four pixel coordinates surrounding this interpolation point.
            VectorI pixel[NUM_POS];
            pixel[POS_BL][x] = __CAST(int, floor( inPixelInterpolationPoint[x] ));
            pixel[POS_BL][y] = __CAST(int, floor( inPixelInterpolationPoint[y] ));
            pixel[POS_BR][x] = pixel[POS_BL][x] + 1; pixel[POS_BR][y] = pixel[POS_BL][y];
            pixel[POS_TL][x] = pixel[POS_BL][x]; pixel[POS_TL][y] = pixel[POS_BL][y] + 1;
            pixel[POS_TR][x] = pixel[POS_BL][x] + 1; pixel[POS_TR][y] = pixel[POS_BL][y] + 1;

            // ensure all pixels are within the extent of the input image.
            __bool withinRange = cast<bool>(true);
            for ( int p = 0; p < NUM_POS; p++ )
            {
              withinRange = withinRange && (pixel[p][x] >= 0) && (pixel[p][x] < _inSize[x]);
              withinRange = withinRange && (pixel[p][y] >= 0) && (pixel[p][y] < _inSize[y]);
              pixel[p][x] = clamp(pixel[p][x], 0, _inSize[x]-1);
              pixel[p][y] = clamp(pixel[p][y], 0, _inSize[y]-1);
            }
            __double value = interpolateValue(  inPixelInterpolationPoint,
                      _inputImage( pixel[POS_BL][x], pixel[POS_BL][y]),
                      _inputImage( pixel[POS_BR][x], pixel[POS_BR][y]),
                      _inputImage( pixel[POS_TL][x], pixel[POS_TL][y]),
                      _inputImage( pixel[POS_TR][x], pixel[POS_TR][y]) );

            __double valinc = value * interpolationWeight;
            totalValue(i,j) = select(valid_pt() && withinRange, totalValue(i,j) + valinc, totalValue(i,j));
            totalWeight(i,j) = select(valid_pt() && withinRange, totalWeight(i,j) + interpolationWeight, totalWeight(i,j));

      pixelValue(i,j) = select(totalWeight(i,j) != __CAST(double, 0.0f), totalValue(i,j)/totalWeight(i,j), __CAST(double, 0.0f));

      // update output pixel value.
      _outputImage(i, j) = select(wrapAround, __CAST(double, 0.0f), pixelValue(i,j));

} // reprojection

#ifdef _TEST
int main(){
  int xsize, ysize;
  unsigned char * imgbuf;
  double inFluxScale;
#ifdef _WIN32
  if (!loadBitmap("G:\\sources2\\__BR\\RC\\crocodile\\reprojection-pseudocode\\lovell.bmp", &xsize, &ysize, &imgbuf, &inFluxScale)){
#else
  if (!loadBitmap("/home/awson/data/Work/crocodile/reprojection-pseudocode/lovell.bmp", &xsize, &ysize, &imgbuf, &inFluxScale)){
#endif
    printf("Can't load the input!");
    return -1;
  }

  Converter c(
      {{Expr(500), Expr(500)}}
    , {{Expr(500), Expr(500)}}
  	, {{Z, Z}}
  	, {{Z, Z}}
    , { {DE(-0.00002), DE(-0.00002)}
   	  , {DE(-0.00002), DE( 0.00002)}
      }
    , { {DE(-0.000027778), Z}
  	  , {Z,  DE(0.000027778)}
      }
    );

  vector<double> inp(xsize * ysize);
  vector<double> outp(xsize * ysize);

  unsigned char * it = imgbuf;
  for (double & v : inp) v = double(*it++);

  buffer_t
      in_buffer  = mkHalideBuf<double>(xsize, ysize)
    , out_buffer = mkHalideBuf<double>(xsize, ysize)
    ;
  in_buffer.host = tohost(inp.data());
  out_buffer.host = tohost(outp.data());
  Buffer
      inbuf(type_of<double>(), &in_buffer, "inbuf")
    , outbuf(type_of<double>(), &out_buffer, "outbuf")
    ;

  ImageParam img(type_of<double>(), 2, "img");
  img.set(inbuf);

  Func out("out");
  printf("Create reprojection function ...\n");
  reprojection(
      c
    , img
    , out
  );

  Target target(get_jit_target_from_environment()
#ifdef __USE_GPU
                  .with_feature(Target::CUDA)
#ifdef __ON_WILKES
                  .with_feature(Target::CUDACapability35)
#endif
#endif
                  );
  printf("Compile reprojection function ...\n");
  out.compile_jit(target);
  printf("Compute ...\n");
  out.realize(outbuf);
  printf("Done!\n");

  // Dirty! Reuse imgbuf.
  it = imgbuf;
  for (double v : outp) *it++ = (unsigned char)(std::min(255.0, v));

  saveBitmap("image.bmp", xsize, ysize, imgbuf, inFluxScale /* NOTE1: * pixelAreaRatio(c) */);
  free(imgbuf);
}
#endif

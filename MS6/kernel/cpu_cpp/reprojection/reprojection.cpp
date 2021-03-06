/*
  Adapted from Chris Skipper's code.
  We use the code from 'crocodile' repo.
  More complex version from Confluence segfaults.
 */

#define _USE_MATH_DEFINES 1
#include <cstdio>
#include <algorithm>
#include <cmath>

#include "obj_defs.h"
#ifdef _TEST
#include <vector>
#include "bmp_utils.h"
#endif

using namespace std;

const double rScale = 180.0 * M_1_PI;

struct CoordinateSystem
{
  // the reference pixel.
  VectorF crVAL;
  VectorI crPIX;

  // the linear conversion between pixel coordinates and RA and DEC offsets.
  Matrix2x2 cd;
  Matrix2x2 inv_cd;

  // the rotation matrices to convert between coordinates near the origin (RA 0, DEC 0) to the required RA and DEC.
  Matrix3x3 toWorld;
  Matrix3x3 toPixel;

  void prepare(){
    Matrix3x3a toWorldArr = mkToWorld(crVAL[x], crVAL[y]);
    copy(toWorldArr.begin(), toWorldArr.end(), &toWorld[0][0]);
    transpose<double,3>(toPixel, toWorld);
    auto scale = [](double & v){v *= rScale;};
    for_each(&cd[0][0], &cd[2][0], scale);
    inverse(inv_cd, cd);
  }
};

// define a maximum number of interpolation points for each output pixel. this is the maximum along each axis, so the actual
// number of interpolation points is n*n per pixel.
const int MAX_INTERPOLATION_POINTS = 10;

inline double enorm(double x, double y){
  return sqrt(pow(x, 2) + pow(y, 2));
}

double pixelAreaRatio(
    const CoordinateSystem & _inCoordinateSystem
  , const CoordinateSystem & _outCoordinateSystem
  ){
  double
      inPixelWidth   = enorm( _inCoordinateSystem.cd[x][x],  _inCoordinateSystem.cd[y][x])
    , outPixelWidth  = enorm(_outCoordinateSystem.cd[x][x], _outCoordinateSystem.cd[y][x])
    , inPixelHeight  = enorm( _inCoordinateSystem.cd[x][y],  _inCoordinateSystem.cd[y][y])
    , outPixelHeight = enorm(_outCoordinateSystem.cd[x][y], _outCoordinateSystem.cd[y][y])
    , inPixelArea = inPixelWidth * inPixelHeight
    , outPixelArea = outPixelWidth * outPixelHeight
    ;
  return outPixelArea / inPixelArea;
}

//
//  COORDINATE SYSTEM TRANSFORMATION FUNCTIONS
//

//
//  pixelToWorld()
//
//  CJS:  08/07/15
//
//  Convert pixel coordinates into world coordinates using the supplied coordinate system. CASA uses WCSLIB to do this,
//  which does a complete spherical transformation of the coordinates. However, here we compromise and just use the
//  FITS matrix transformation (CD) to do a linear conversion from pixel to intermediate coordinates, and then multiply
//  the first coordinate by cos(dec) in order to convert from an angular size to degrees of RA (i.e. implement sinusoidal
//  projection).
//
//  The rotation matrix attached to the coordinate system will convert the coordinates from the origin (chosen to be
//  RA 0, DEC 0) to the relevant RA and DEC position of the reference pixel. Epoch conversion will be done if required.
//
//  Note that the routine returns the new position in cartesian coordinates (using directional cosines). This is because
//  the world to pixel routine needs cartesian coordinates.
//
//  The returned 'wrap around' flag warns if this pixel is outside the range -180 to 180 in RA, or -90 to 90 in DEC.
//
inline
VectorF3 pixelToWorld( VectorF pPixelPosition, CoordinateSystem pCoordinateSystem, bool * pWrapAround )
{

  // subtract reference pixel from position.
  VectorF pixelOffset = pPixelPosition - pCoordinateSystem.crPIX;

  // apply coordinate system CD transformation matrix.
  VectorF intermediatePosition = pCoordinateSystem.cd * pixelOffset;

  // skew the image using the declination. this step reflects the fact that lines of RA get closer together
  // as the image gets nearer to +/- 90 deg declination. this transformation effectively converts from angular
  // distance in the ra direction to actual ra coordinates.
  VectorF worldOffset;
  worldOffset[x] = intermediatePosition[x] / cos( intermediatePosition[y] );
  worldOffset[y] = intermediatePosition[y];

  // check for wrap around, and set the flag.
  if (worldOffset[x] < -M_PI || worldOffset[x] > M_PI || worldOffset[y] < -M_PI_2 || worldOffset[y] > M_PI_2)
    *pWrapAround = true;

  // get x, y and z cartesian coordinates.
  VectorF3 cartesianOffset;
  double xs, xc, ys, yc;
  sincos(worldOffset[x], &xs, &xc);
  sincos(worldOffset[y], &ys, &yc);

  cartesianOffset[x] = xc * yc;
  cartesianOffset[y] = xs * yc;
  cartesianOffset[z] = ys;

  // the world offset coordinates are relative to the reference pixel, which is currently at ra 0, dec 0. we need to
  // rotate the offset coordinates by the reference pixel's true ra and dec so that they are relative to ra 0, dec 0.
  // unfortunately, the dec rotation has to be done in cartesian coordinates, which are rather messy to convert back
  // to spherical.
  cartesianOffset = pCoordinateSystem.toWorld * cartesianOffset;

  // return the world position.
  return cartesianOffset;

} // pixelToWorld

//
//  worldToPixel()
//
//  CJS:  08/07/15
//
//  Convert world coordinates into pixel coordinates using the supplied coordinate system. Now we must use
//  the inverse transformation matrix, which was calculated earlier from CD.
//
inline
VectorF worldToPixel( VectorF3 pWorldPosition, CoordinateSystem pCoordinateSystem )
{

  // rotate the vector to bring it from its world position near RA and DEC to the origin, which we choose
  // to be RA 0, DEC 0.
  VectorF3 cartesianOffset = pCoordinateSystem.toPixel * pWorldPosition;

  // we now need to convert back into polar coordinates.
  VectorF intermediatePosition = { atan2( cartesianOffset[y], cartesianOffset[x] ), asin( cartesianOffset[z] ) };

  // skew the image using the declination. this step reflects the fact that lines of RA get closer together
  // as the image gets nearer to +/- 90 deg declination.
  intermediatePosition[x] = intermediatePosition[x] * cos( intermediatePosition[y] );

  // apply coordinate system inverse-CD transformation matrix.
  VectorF pixelOffset = pCoordinateSystem.inv_cd * intermediatePosition;

  // add reference pixel coordinates.
  VectorF pixelPosition = pixelOffset + pCoordinateSystem.crPIX;

  // return the world position.
  return pixelPosition;

} // worldToPixel


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
double interpolateValue( VectorF pPosition, double pBLValue, double pBRValue, double pTLValue, double pTRValue )
{

  // subtract the integer part of the position. we don't need this here.
  VectorI integerPart = { (int) floor( pPosition[x] ), (int) floor( pPosition[y] ) };
  VectorF fraction = { pPosition[x] - (double)integerPart[x], pPosition[y] - (double)integerPart[y] };

  // interpolate top and bottom in the x-direction.
  double valueTop = ((pTRValue - pTLValue) * fraction[x]) + pTLValue;
  double valueBottom = ((pBRValue - pBLValue) * fraction[x]) + pBLValue;

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
    const CoordinateSystem & _inCoordinateSystem
  , const CoordinateSystem & _outCoordinateSystem
  , const VectorI & _inSize
  , const VectorI & _outSize
  , const double * _inputImage
  ,       double * _outputImage
)
{
  enum pos {
      POS_BL
    , POS_BR
    , POS_TL
    , POS_TR
    , NUM_POS
  };

  // loop through all the output image pixels.
  for ( int i = 0; i < _outSize[x]; i++ )
  {
#ifdef _TEST
    if (i % 50 == 0) {printf("%d%%\r", i*100/_outSize[x]); fflush(stdout);}
#endif
    for ( int j = 0; j < _outSize[y]; j++ )
    {
      double pixelValue = 0;

      // we need to find the coordinates of the 4 output pixels that diagonally surround our
      // pixel, so that we can choose some interpolation points within this region of the input image.
      VectorF outPixelCoordinate[NUM_POS];
      VectorF3 worldCoordinate[NUM_POS];
      VectorF inPixelCoordinate[NUM_POS];

      outPixelCoordinate[POS_BL][x] = i - 1; outPixelCoordinate[POS_BL][y] = j - 1;
      outPixelCoordinate[POS_BR][x] = i + 1; outPixelCoordinate[POS_BR][y] = j - 1;
      outPixelCoordinate[POS_TL][x] = i - 1; outPixelCoordinate[POS_TL][y] = j + 1;
      outPixelCoordinate[POS_TR][x] = i + 1; outPixelCoordinate[POS_TR][y] = j + 1;

      // convert each of these four pixels to the input coordinate system.
      bool wrapAround = false;
      for ( int k = 0; k < NUM_POS; k++ )
      {

        // convert these pixel coordinates to world coordinates (cartesian).
        worldCoordinate[k] = pixelToWorld( outPixelCoordinate[k], _outCoordinateSystem, &wrapAround );

        // convert the world coordinates back into input pixel coordinates.
        inPixelCoordinate[k] = worldToPixel( worldCoordinate[k], _inCoordinateSystem );

      }

      // if we have wrapped around, then leaves this pixel black. Otherwise, carry on.
      if (wrapAround == false)
      {

        // the input pixel coordinates will map out same shape in the input image, which will not necessarily
        // be square. we need to find the limits in both x and y, so that we can choose an appropriate
        // number of interpolation points within the region.
        VectorF minc = inPixelCoordinate[POS_BL], maxc = inPixelCoordinate[POS_BL];
        for ( int k = 1; k < NUM_POS; k++ )
        {

          minc[x] = min( minc[x], inPixelCoordinate[k][x] );
          maxc[x] = max( maxc[x], inPixelCoordinate[k][x] );
          minc[y] = min( minc[y], inPixelCoordinate[k][y] );
          maxc[y] = max( maxc[y], inPixelCoordinate[k][y] );
        }

        // find the size of the input image region.
        int regionSize = max( (int) floor( maxc[x] ) - (int) floor( minc[x] ),
              (int) floor( maxc[y] ) - (int) floor( minc[y] ) );

        // the input image pixels could be much larger than our region, or they could be much smaller.
        // we use the region size to define a number of interpolation points, which form a NxN grid
        // around our region of the input image. Each input pixel should have at least one interpolation
        // point.
        int interpolationPoints = (((int)(regionSize / 2)) + 1) * NUM_POS;
        if (interpolationPoints < 2)
          interpolationPoints = 2;
        if (interpolationPoints > MAX_INTERPOLATION_POINTS)
          interpolationPoints = MAX_INTERPOLATION_POINTS;

        // keep track of the total value and total weight, so we can normalise the sum over
        // the interpolation points.
        double totalValue = 0;
        double totalWeight = 0;

        // loop through all the interpolation points. we don't bother with the first or last
        // interpolation points, since they have weight 0.
        for ( int k = 1; k < interpolationPoints; k++ )
          for ( int l = 1; l < interpolationPoints; l++ )
          {

            // calculate the position of this interpolation point as a fraction
            // of the output pixel size (the centre of the pixel will be at <0.5, 0.5>).
            VectorF fraction = { (double)k / (double)interpolationPoints,
                 (double)l / (double)interpolationPoints };

            // calculate the weight of this interpolation point. this is based upon its
            // position - the centre of the region has weight 1, and the edges weight 0.
            VectorF weight = { (fraction[x] <= 0.5) ? fraction[x] * 2 : 2 - (fraction[x] * 2),
                 (fraction[y] <= 0.5) ? fraction[y] * 2 : 2 - (fraction[y] * 2) };

            double interpolationWeight = (weight[x] * weight[y]);

            // get the position of the interpolation point in the output image.
            VectorF outPixelInterpolationPoint = { (double)(i - 1) + (fraction[x] * 2),
                     (double)(j - 1) + (fraction[y] * 2) };

            // convert these pixel coordinates to world coordinates (cartesian). we ignore the wrap around
            // since we've already checked that this is OK whilst calculating the interpolation points.
            VectorF3 worldInterpolationPoint = pixelToWorld( outPixelInterpolationPoint,
                        _outCoordinateSystem, &wrapAround );

            // convert the world coordinates back into input pixel coordinates.
            VectorF inPixelInterpolationPoint = worldToPixel( worldInterpolationPoint,
                          _inCoordinateSystem );

            // calculate the four pixel coordinates surrounding this interpolation point.
            VectorI pixel[NUM_POS];
            pixel[POS_BL][x] = (int) floor( inPixelInterpolationPoint[x] );
            pixel[POS_BL][y] = (int) floor( inPixelInterpolationPoint[y] );
            pixel[POS_BR][x] = pixel[POS_BL][x] + 1; pixel[POS_BR][y] = pixel[POS_BL][y];
            pixel[POS_TL][x] = pixel[POS_BL][x]; pixel[POS_TL][y] = pixel[POS_BL][y] + 1;
            pixel[POS_TR][x] = pixel[POS_BL][x] + 1; pixel[POS_TR][y] = pixel[POS_BL][y] + 1;

            // ensure all pixels are within the extent of the input image.
            bool withinRange = true;
            for ( int i = 0; i < NUM_POS; i++ )
            {
              withinRange = withinRange && (pixel[i][x] >= 0) && (pixel[i][x] < _inSize[x]);
              withinRange = withinRange && (pixel[i][y] >= 0) && (pixel[i][y] < _inSize[y]);
            }

            // calculate memory location of this pixel within the input image.
            int location[NUM_POS];
            for ( int i = 0; i < NUM_POS; i++ )
              location[i] = (pixel[i][y] * _inSize[x]) + pixel[i][x];

            // are these pixels all within the input image size?
            if (withinRange == true)
            {

              // get an bilinearly interpolated value from the input pixel image.
              double value = interpolateValue(  inPixelInterpolationPoint,
                        _inputImage[ location[POS_BL] ],
                        _inputImage[ location[POS_BR] ],
                        _inputImage[ location[POS_TL] ],
                        _inputImage[ location[POS_TR] ] );

              // update the summed value and the summed weight.
              totalValue = totalValue + (value * interpolationWeight);
              totalWeight = totalWeight + interpolationWeight;

            }

          }

        // calculate output pixel value.
        if (totalWeight != 0)
          pixelValue = totalValue / totalWeight;

      }

      // update output pixel value.
      _outputImage[ (j * _outSize[x]) + i ] = pixelValue;

    }

  }
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
  CoordinateSystem inCS = {
  	{0, 0}
  , {500, 500}
  , { {-0.000027778, 0}
  	, { 0, 0.000027778}
    }
  };
  inCS.prepare();
  CoordinateSystem outCS = {
  	{0, 0}
  , {500, 500}
  , { {-0.00002, -0.00002}
  	, {-0.00002,  0.00002}
    }
  };
  outCS.prepare();

  vector<double> inp(xsize * ysize);
  vector<double> outp(xsize * ysize);

  unsigned char * it = imgbuf;
  for (double & v : inp) v = double(*it++);

  printf("Start repro ...\n");
  reprojection(
      inCS
    , outCS
    , {xsize, ysize}
    , {xsize, ysize}
    , inp.data()
    , outp.data()
  );
  printf("Done repro!\n");

  // Dirty! Reuse imgbuf.
  it = imgbuf;
  for (double v : outp) *it++ = (unsigned char)(min(255.0, v));

  saveBitmap("image.bmp", xsize, ysize, imgbuf, inFluxScale * pixelAreaRatio(inCS, outCS));
  free(imgbuf);
}
#endif

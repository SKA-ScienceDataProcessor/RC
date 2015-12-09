/*
  Adapted from Chris Skipper's code.
 */

#include <cstdio>
#include <cstdlib>
#include <cstring>

#include "bmp_utils.h"

// bitmap file header positions.
const int BIT_CONST = 0x00;
const int MAP_CONST = 0x01;
const int IMAGE_SIZE = 0x02;
const int RESERVED = 0x06;
const int FILE_HEADER_SIZE = 0x0A;
const int BITMAP_INFO_HEADER = 0x0E;
const int IMAGE_WIDTH = 0x12;
const int IMAGE_HEIGHT = 0x16;
const int COLOUR_PLANES = 0x1A;
const int BIT_COUNT = 0x1C;
const int COMPRESSION_TYPE = 0x1E;
const int COLOURS_USED = 0x2E;
const int SIGNIFICANT_COLOURS = 0x32;

bool loadBitmap( const char * pFilename, int * xp, int * yp, unsigned char ** pImageData, double * p_inFluxScale)
{
	
	bool ok = true;
	unsigned char * fileInfo, * fileHeader;
	
	// open the bitmap file.
	FILE * inputFile = fopen( pFilename, "rb" );
	if (inputFile == NULL)
	{
		printf("Could not open file \"%s\".\n", pFilename);
		ok = false;
	}
	else
	{
		
		// reserve memory for the start of the file header, and read it from the file. we only
		// read the first 18 bytes, because these contain information about how large the header is. once we
		// know this we can read the rest of the header.
		fileInfo = (unsigned char *) malloc( 18 );
		size_t num_read = fread( fileInfo, sizeof( unsigned char ), 18, inputFile );
				
		// ensure we've read the correct number of bytes.
		if (num_read != 18)
		{
			printf( "Error: read only %lu values from the file header.\n", num_read );
			ok = false;
		}

		// make sure this is a bitmap file by checking that the first two bytes are ASCII codes 'B' and 'M'.
		if (ok == true)
			if ((fileInfo[BIT_CONST] != 'B') || (fileInfo[MAP_CONST] != 'M'))
			{
				printf( "Error: this is not a bitmap file.\n" );
				ok = false;
			}
			
		// get the size of the file header (i.e. a pointer to the start of the actual image).
		int fileHeaderSize = 0;
		if (ok == true)
			memcpy( &fileHeaderSize, &fileInfo[FILE_HEADER_SIZE], 4 );
			
		// get the size of the bitmap info header (the bitmap info header is followed by the colour table,
		// so we need to know the offset in order to read the colours).
		int bitmapInfoHeaderSize = 0;
		if (ok == true)
			memcpy( &bitmapInfoHeaderSize, &fileInfo[BITMAP_INFO_HEADER], 4 );
		
		// need to add 14 because the bitmap info header size does not include the first 14 bytes of the file (which
		// technically are part of the file header but not the bitmap header; we lump everything in together so that
		// all of our offsets are from the start of the file - less confusing this way).
		bitmapInfoHeaderSize = bitmapInfoHeaderSize + 14;
			
		// get the rest of the file header now we know how big it is. we already have the first 18 bytes,
		// which should be copied to the start of the new memory area.
		if (ok == true)
		{
			fileHeader = (unsigned char *) malloc( fileHeaderSize );
			memcpy( fileHeader, fileInfo, 18 );
			num_read = fread( &fileHeader[18], sizeof( unsigned char ), fileHeaderSize - 18, inputFile );
			if (num_read != (fileHeaderSize - 18))
			{
				printf( "Error: read only %lu values from the file header.\n", num_read + 18 );
				ok = false;
			}
		}
		
		// get the input image flux scale. this value may be stored in the reserved part of the bitmap file header
		// (0x06 -> 0x09), and will not be supplied if the input image has been saved using something like GIMP or
		// Photoshop. if it is zero, then we assume a scale of 1 Jy/PIXEL. this value gets re-scaled along with our
		// image, and is then written back to the output file.
		if (ok == true)
			memcpy( p_inFluxScale, &fileHeader[RESERVED], 4 );
		
		if (*p_inFluxScale == 0)
			*p_inFluxScale = 1;
			
		// ensure we have an 8-bit image.
		if (ok == true)
		{
			short bitCount;
			memcpy( &bitCount, &fileHeader[BIT_COUNT], 2 );
			if (bitCount != 8)
			{
				printf( "Error: expecting an 8-bit greyscale image. This one is %hi bit.\n", bitCount );
				ok = false;
			}
		}
			
		// ensure the image in not compressed.
		if (ok == true)
		{
			int compressionMethod;
			memcpy( &compressionMethod, &fileHeader[COMPRESSION_TYPE], 4 );
			if (compressionMethod != 0)
			{
				printf( "Error: can only handle uncompressed bitmaps." );
				ok = false;
			}
		}
			
		if (ok == true)
		{
			
			// get the width and height of the image.
			memcpy( xp, &fileHeader[IMAGE_WIDTH], 4 );
			memcpy( yp, &fileHeader[IMAGE_HEIGHT], 4 );
		
			// ensure width and height are greater than zero.
			if (*xp <= 0 || *yp <= 0)
			{
				printf( "Error: invalid image size (%i x %i).\n", *xp, *yp );
				ok = false;
			}
			
		}
		
		if (ok == true)
		{
			
			// ensure the number of colours used is 256.
			int coloursUsed = 0;
			memcpy( &coloursUsed, &fileHeader[COLOURS_USED], 4 );
			if (coloursUsed != 256)
			{
				printf( "ERROR: Can only handle 256 colours in pallette.\n" );
				ok = false;
			}
			
		}
		
		// get the number of significant colours used. this value can (theoretically) be less than COLOURS_USED
		// if an image is only using (e.g.) 37 shades rather than all 256. in practice, this is never implemented, and
		// this value will either be 0 (= all colours) or will match COLOURS_USED. however, only SIGNIFICANT_COLOURS are
		// written to the pallette, so we have to handle this parameter just in case.
		int significantColours = 0;
		if (ok == true)
			memcpy( &significantColours, &fileHeader[SIGNIFICANT_COLOURS], 4 );
		
		// if significant colours = 0, then they are ALL significant so set to 256.
		if (significantColours == 0)
			significantColours = 256;
			
		unsigned int colour[256];
		if (ok == true)
		{
				
			// load colour table from bmp.
			for ( unsigned int i = 0; i < significantColours; ++i )
			{
				
				memcpy( &colour[i], &fileHeader[bitmapInfoHeaderSize + (i * 4)], 4 );
				
				// convert pallette colour to greyscale, using 0.2990, 0.5870, 0.1140 RGB weighting. add 0.5
				// to round to nearest integer (since C only rounds down).
				unsigned char red = colour[i] >> 16;
				unsigned char green = (colour[i] >> 8) - (red << 8);
				unsigned char blue = colour[i] - (red << 16) - (green << 8);
				colour[i] = (unsigned int) ((((double)red * 0.2990) + ((double)green * 0.5870) +
								((double)blue * 0.1140)) + 0.5);
				
			}
				
			// reserve some memory for the image, and read it from the file.
			*pImageData = (unsigned char *) malloc( *xp * *yp );
			num_read = fread( *pImageData, sizeof( unsigned char ), *xp * *yp, inputFile );
				
			// ensure we've read the correct number of bytes.
			if (num_read != *xp * *yp)
			{
				printf( "Error: read only %lu values from the image.\n", num_read );
				ok = false;
			}
				
		}
			
		if (ok == true)
		{
				
			// update image values using the values from the colour table.
			unsigned char * imageData = *pImageData;
			for ( int i = 0; i < *xp * *yp; i++ )
				imageData[i] = (unsigned char)colour[imageData[i]];
				
		}
		
		// close file.
		fclose( inputFile );
	
	}
	
	// tidy up memory.
	if ( fileInfo != NULL )
		free( (void *) fileInfo );
	if ( fileHeader != NULL )
		free( (void *) fileHeader );
	
	// return success flag.
	return ok;
	
} // loadBitmap

//
//	saveBitmap()
//
//	CJS:	08/07/2015
//
//	write the output bitmap to file.
//

bool saveBitmap( const char * pFilename, int px, int py, unsigned char * image, double _outFluxScale)
{
	
	const int HEADER_SIZE = 1078;
	
	// allocate and build the header.
	unsigned char * fileHeader = (unsigned char *) malloc( HEADER_SIZE );
	memset( fileHeader, 0, HEADER_SIZE );

	// file header.
	fileHeader[BIT_CONST] = 'B'; fileHeader[MAP_CONST] = 'M';					// bfType
	int size = (px * py) + HEADER_SIZE; memcpy( &fileHeader[IMAGE_SIZE], &size, 4 );	// bfSize
	int offBits = HEADER_SIZE; memcpy( &fileHeader[FILE_HEADER_SIZE], &offBits, 4 );		// bfOffBits
	
	// we write our flux scale (in Jy/PIXEL) to the reserved part of the bitmap. this
	// space would not normally be filled.
	memcpy(	&fileHeader[RESERVED], &_outFluxScale, 4 );						// bfReserved1

	// image header.
	size = 40; memcpy( &fileHeader[BITMAP_INFO_HEADER], &size, 4 );					// biSize
	memcpy( &fileHeader[IMAGE_WIDTH], &px, 4 );						// biWidth
	memcpy( &fileHeader[IMAGE_HEIGHT], &py, 4 );						// biHeight
	short planes = 1; memcpy( &fileHeader[COLOUR_PLANES], &planes, 2 );				// biPlanes
	short bitCount = 8; memcpy( &fileHeader[BIT_COUNT], &bitCount, 2 );				// biBitCount
	int coloursUsed = 256; memcpy( &fileHeader[COLOURS_USED], &coloursUsed, 4 );			// biClrUsed

	// colour table.
	for (unsigned int i = 0; i < 256; ++i)
	{
		unsigned int colour = (i << 16) + (i << 8) + i;
		memcpy( &fileHeader[54 + (i * 4)], &colour, 4 );
	}
	
	bool ok = true;

	// open file.
	FILE * outputFile = fopen( pFilename, "wb" );
	if (outputFile == NULL)
	{
		printf( "Could not open file \"%s\".\n", pFilename );
		ok = false;
	}
	else
	{

		// write the file header.
		size_t num_written = fwrite( fileHeader, 1, 1078, outputFile );
		if (num_written != 1078)
		{
			printf( "Error: cannot write to file.\n" );
			ok = false;
		}
		
		// write the data.
		if (ok == true)
		{
			
			size_t num_written = fwrite( image, 1, px * py, outputFile );
			if (num_written != (px * py))
			{
				printf( "Error: cannot write to file.\n" );
				ok = false;
			}
			
		}

		// close file.
		fclose( outputFile );
		
	}

	// cleanup memory.
	free( (void *) fileHeader );
	
	// return success flag.
	return ok;
	
} // saveBitmap

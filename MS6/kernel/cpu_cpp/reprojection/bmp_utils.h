#pragma once

bool loadBitmap( const char * pFilename, int * xp, int * yp, unsigned char ** pImageData, double * p_inFluxScale);
bool saveBitmap( const char * pFilename, int px, int py, unsigned char * image, double _outFluxScale);

// (C) 2012  John Romein/ASTRON

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

#define MODE_SIMPLE	 0
#define MODE_OVERSAMPLE	 1
#define MODE_INTERPOLATE 2

#if !defined MODE
#define MODE		MODE_OVERSAMPLE
#endif

#undef DEGRIDDING
#define USE_REAL_UVW
#define USE_SYMMETRY
#undef MAP_OBJECTS
#define ENABLE_PROFILING

#if defined __CUDA__ || 0
#define USE_TEXTURE
#endif

#if !defined GRID_U
#define GRID_U		2048
#endif

#if !defined GRID_V
#define GRID_V		2048
#endif

#define POLARIZATIONS	4

#if !defined SUPPORT_U
#define SUPPORT_U	16
#endif
#if !defined X
#define X SUPPORT_U
#endif

#if !defined SUPPORT_V
#define SUPPORT_V	16
#endif

#if !defined W_PLANES
#define W_PLANES	32
#endif

#define OVERSAMPLE_U	8
#define OVERSAMPLE_V	8

#define CELL_SIZE_U	(1.08*13107.2 / GRID_U)
#define CELL_SIZE_V	(1.08*13107.2 / GRID_V)
#define CELL_SIZE_W	(8192.0 / W_PLANES)

#ifndef NR_STATIONS
#define NR_STATIONS	44
#endif

#define BASELINES	(NR_STATIONS * (NR_STATIONS - 1) / 2)

#define MAX_BASELINE_LENGTH	22000

#if !defined CHANNELS
#define CHANNELS	16
#endif

#if !defined TIMESTEPS
#define TIMESTEPS	20
#endif

#if !defined BLOCKS
#define BLOCKS		108
#endif

#define	STREAMS		2

#define SPEED_OF_LIGHT	299792458.

#if !defined ORDER
#define ORDER			ORDER_W_OV_OU_V_U
#endif

#define ORDER_W_OV_OU_V_U	0
#define ORDER_W_V_OV_U_OU	1

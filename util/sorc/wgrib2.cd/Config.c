#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"

#if defined USE_NETCDF3 || defined USE_NETCDF4
#include <netcdf.h>
#endif

/*
 * Config.c  just prints out the configuration
 *
 * 3/2009 public domain Wesley Ebisuzaki
 */

/*
 * HEADER:100:config:misc:0:shows the configuration
 */
int f_config(ARG0) {

    char *filename;
    FILE *input;

    inv_out[0] = 0;
    strcat(inv_out, "wgrib2 " WGRIB2_VERSION "\n    " BUILD_COMMENTS "\n\n");

    inv_out += strlen(inv_out);
    sprintf(inv_out,"Compiled on %s %s\n\n",__TIME__,__DATE__);

#if defined USE_NETCDF3 || defined USE_NETCDF4
    strcat(inv_out, "Netcdf package: ");
    strcat(inv_out,  nc_inq_libvers());
    strcat(inv_out, " is installed\n");
#else
    strcat(inv_out, "Netcdf package is not installed\n");
#endif


#ifdef USE_MYSQL
    strcat(inv_out, "mysql package is installed\n");
#else
    strcat(inv_out, "mysql package is not installed\n");
#endif


#ifdef USE_REGEX
    strcat(inv_out, "regex package is installed\n");
#else
    strcat(inv_out, "regex package is not installed\n");
#endif


#ifdef USE_TIGGE
    strcat(inv_out, "tigge package is installed\n");
#else
    strcat(inv_out, "tigge package is not installed\n");
#endif

#ifdef USE_IPOLATES
    strcat(inv_out, "interpolation package is installed\n");
#else
    strcat(inv_out, "interpolation package is not installed\n");
#endif

    strcat(inv_out, "gptpc interface: experimental v0.1\n");

#ifdef USE_UDF
    strcat(inv_out, "UDF package is installed\n");
#else
    strcat(inv_out, "UDF package is not installed\n");
#endif

#ifdef N_ARGLIST
    inv_out += strlen(inv_out);
    sprintf(inv_out, "maximum number of arguments on command line: %d\n",
	N_ARGLIST);
#else
    inv_out += strlen(inv_out);
    sprintf(inv_out, "maximum number of arguments on command line: limited by shell/OS\n");
#endif

#ifdef USE_REGEX
    inv_out += strlen(inv_out);
    sprintf(inv_out, "maximum number of -match,-not,-if, and -not_if arguments: %d\n", MATCH_MAX);
#endif

    inv_out += strlen(inv_out);
    sprintf(inv_out, "stdout buffer length: %d\n", INV_BUFFER);

#if (DEFAULT_G2CLIB == 0)
    strcat(inv_out, "default decoding: WMO standard\n");
#endif
#if (DEFAULT_G2CLIB == 1)
    strcat(inv_out, "default decoding: g2clib emulation\n");
#endif
#if (DEFAULT_G2CLIB == 2)
    strcat(inv_out, "default decoding: g2clib\n");
#endif

#if (DEFAULT_GCTPC == 0)
    strcat(inv_out, "default geolocation: spherical\n");
#else
    strcat(inv_out, "default geolocation: gctpc\n");
#endif


#ifdef USE_G2CLIB
    strcat(inv_out, "g2clib decoders are installed\n");
#else
    strcat(inv_out, "g2clib decoders are not installed\n");
#endif

#ifndef USE_PNG
    strcat(inv_out, "png compression not supported\n");
#endif
#ifndef USE_JASPER
    strcat(inv_out, "jpeg2000 compression not supported\n");
#endif

    filename = getenv("GRIB2TABLE");
    if (filename == NULL) filename = getenv("grib2table");
    if (filename == NULL) filename = "grib2table";
    input = fopen(filename,"r");
    inv_out += strlen(inv_out);
    sprintf(inv_out, "user gribtable: %s\n", input == NULL ? "(none)" : filename);
    if (input) fclose(input);


#ifdef CC
    strcat(inv_out,"C compiler: " CC "\n");
#endif

#ifdef FORTRAN
    strcat(inv_out,"Fortran compiler: " FORTRAN "\n");
#endif

#ifdef USE_OPENMP
    strcat(inv_out,"OpenMP: control number of threads with environment variable OMP_NUM_THREADS\n");
#else
    strcat(inv_out,"OpenMP: not used\n");
#endif
    inv_out += strlen(inv_out);

    sprintf(inv_out, "INT_MAX:   %d\n", INT_MAX);
    inv_out += strlen(inv_out);
    sprintf(inv_out, "ULONG_MAX: %lu\n", ULONG_MAX);

    return 1;
}

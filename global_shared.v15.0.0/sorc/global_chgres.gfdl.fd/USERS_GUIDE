          HOW TO CONVERT A SURFACE RESTART FILE

1.0 INTRODUCTION

NEAREST NEIGHBOR INTERPOLATION IS PERFORMED SO THAT LAND/NONLAND
POINTS ON THE INPUT GRID ARE MAPPED TO LAND/NONLAND POINTS
ON THE TARGET GRID.  IF THE INPUT FILE CONTAINS LANDICE
AND THE OUTPUT GRID IS TO HAVE LANDICE, THEN NONLAND IS
MAPPED TO NONLAND, LANDICE IS MAPPED TO LANDICE, ICE FREE
LAND IS MAPPED TO ICE FREE LAND. OPTIONALLY, THE CLIMO FIELDS
SUCH AS ALBEDO, ROUGHNESS, ETC, MAY DETERMINED ON THE OUTPUT
GRID FROM SFCCYCLE (WHICH IS CALLED FROM THE SURFACE
CHGRES MODULE).  THE LATTER IS RECOMMENDED WHEN CONVERTING
FROM A LOW TO HIGH RESOLUTION GRID.  A NEW LAND-SEA MASK IS
OPTIONALLY READ IN. IF IT IS MISSING, THE NEW LAND-SEA MASK IS
INTERPOLATED FROM THE OLD MASK.  SKIN AND SOIL TEMPERATURE OVER
LAND ARE ADJUSTED FOR DIFFERENCES BETWEEN THE INPUT AND OUTPUT
OROGRAPHY. LIQ SOIL MOISTURE IS CALCULATED ACCORDING TO THE
ADJUSTED TEMP. OUTPUT OROGRAPHY MAY BE READ IN FROM FILE OR INTERPOLATED
FROM INPUT OROGRAPHY.  NOTE: OLDER VERSIONS OF THE SURFACE
RESTART FILE (BEFORE IVS 200501) DO NOT HAVE OROGRAPHY RECORDS.
IN CASES WHERE THE INPUT SURFACE FILE IS PRE 200501,
THE PROGRAM WILL GET THE OROGRAPHY FROM THE SIGMA FILE.
THEREFORE, YOU MUST SET THE OPTIONS TO CONVERT A SIGMA FILE
AS WELL AS A SURFACE FILE.  WHEN CHANGING A PRE 200501 FILE,
THE PROGRAM WILL INTERPOLATE ONLY THOSE LAND FIELDS NEEDED
TO RUN THE OLD OSU LAND MODEL AND OLD SEA ICE PHYSICS.
WHEN CHANGING A 200501 FILE, THE PROGRAM WILL INTERPOLATE/CALC
THOSE ADDITIONAL FIELDS NEEDED BY THE NOAH LSM (MAX SNOW ALB,
LIQ. SOIL MOIST, SNOW DEPTH, PRECIP, PRECIP TYPE, SLOPE TYPE,
MAX/MIN GREENNESS) AND THE NEW SEA ICE MODEL (ICE DEPTH AND
FRACTION).  WHEN CHANGING A PRE 200501 FILE TO A 200501 FILE,
THE PROGRAM WILL AUTOMATICALLY INITIALIZE THE ABOVE
MENTIONED FIELDS USING EITHER GUESS VALUES OR VALUES
CALCULATED FROM SFCCYCLE.  THE PROGRAM WILL ALSO CONVERT FROM TWO
TO FOUR SOIL LAYERS AND VICE VERSA.  THE PROGRAM WILL RUN
ON THE FULL OR REDUCED GRID DEPENDING ON THE LONSPERLAT
RECORD OF THE INPUT FILE OR WHETHER THE USER SPECIFIES
AN EXTERNAL LONSPERLAT FILE.  THE PROGRAM WILL INITIALIZE
ALL LAND STATES FOR THE LANDICE PHYSICS IF DESIRED.  THE PROGRAM
WILL SCALE TOTAL SOIL MOISTURE FOR ANY DIFFERENCES IN SOIL
TYPE BETWEEN THE INPUT AND OUTPUT GRIDS.  CONTACT G. GAYNO
WITH QUESTIONS.

2.0 HOW TO RUN CHGRES

THE PROGRAM IS CONTROLLED BY SETTING SEVERAL ENVIRONMENT VARIABLES
IN THE DRIVER SCRIPT.

LSOIL - NUMBER OF SOIL LAYERS ON OUTPUT GRID.  WHEN NOT SET, THE
        DEFAULT IS SAME AS INPUT GRID.  OTHERWISE, MAY BE SET TO
        2 OR 4 LAYERS.

IVSSFC - THE VERSION NUMBER OF THE SURFACE RESTART FILE

LANDICE_OPT - THE LANDICE PHYSICS OPTIONS:
              1-NO LANDICE ON INPUT GRID -> INITIALIZE LANDICE ON OUTPUT GRID
              2-LANDICE ON INPUT GRID -> LANDICE ON OUTPUT GRID
              3-NO LANDICE ON INPUT GRID -> NO LANDICE ON OUTPUT GRID
              4-LANDICE ON INPUT GRID -> REMOVE LANDICE FROM OUTPUT GRID
              5-INITIALIZE LANDICE OUTPUT GRID REGARDLESS OF WHETHER
                INPUT GRID HAS LANDICE OR NOT.

CLIMO_FIELDS_OPT - OPTION FOR DETERMINING CLIMATOLOGICAL FIELDS ON
                   OUTPUT GRID.
                   1-INTERPOLATE ALL FROM INPUT GRID
                   2-INTERPOLATE VEG, SOIL, SLOPE TYPE
                     FROM INPUT GRID.  OTHERS FROM
                     SFCCYCLE PROGRAM.
                   3-ALL FROM SFCCYCLE PROGRAM.

#--------------------------------------------------------------------
# Example #1: convert a t382 file with 4 soil layers and noah lsm 
# physics and NO landice, to a 254 file with 2 soil layers and 
# osu lsm physics (and no landice).
#--------------------------------------------------------------------

export CLIMO_FIELDS_OPT=3
export LANDICE_OPT=3
export IVSSFC=200004
export LSOIL=2

./global_chgres.sh  NULL \
                   ./t382.sfc.input.file \
                    NULL \
                   ./t254.sfc.output.file  \
                    254 0 768 384

#--------------------------------------------------------------------
# Example #2: convert a t382 file with 4 soil layers and noah lsm 
# physics AND landice fields, to a 254 file with 2 soil layers and 
# osu lsm physics (and no landice).
#--------------------------------------------------------------------

export CLIMO_FIELDS_OPT=3
export LANDICE_OPT=4
export IVSSFC=200004
export LSOIL=2

./global_chgres.sh NULL \
                   ./t382.sfc.input.file \
                   NULL \
                   ./t254.sfc.output.file \
                   254 0 768 384

#--------------------------------------------------------------------
# Example #3: convert a t254 file with 2 soil layers and osu lsm 
# physics, to a t382 file with 4 soil layers and noah lsm physics and
# NO landice initialization. 
# note: the old style surface files do not have terrain, so you 
# must get this field from a sigma file.
#--------------------------------------------------------------------

export CLIMO_FIELDS_OPT=3
export LANDICE_OPT=3
export IVSSFC=200501
export LSOIL=4

./global_chgres.sh ./t254.sig.input.file  \
                   ./t254.sfc.input.file  \
                   ./t382.sig.output.file \
                   ./t382.sfc.output.file  382 0 1152 576

#--------------------------------------------------------------------
# Example #4: convert a t254 file with 2 soil layers and osu lsm 
# physics, to a t382 file with 4 soil layers and noah lsm physics
# and landice initialization. 
# note: the old style surface files do not have terrain, so you 
# must get this field from a sigma file.
#--------------------------------------------------------------------

export CLIMO_FIELDS_OPT=3
export LANDICE_OPT=1
export IVSSFC=200501
export LSOIL=4

./global_chgres.sh ./t254.sig.input.file  \
                   ./t254.sfc.input.file  \
                   ./t382.sig.output.file \
                   ./t382.sfc.output.file  382 0 1152 576


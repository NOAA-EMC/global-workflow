NEMS: Old to New Structure How To  {#HowToOldToNew}
==================================
 
_Purpose:_ This document will walk through the steps of how to go from
the old NEMS App folder and svn structure, using the original
NEMSAppBuilder and NEMSCompsetRun to the
\ref structure "new NEMS App folder and svn structure",
the updated NEMSAppBuilder and the regression test run system.  As an
example, we will be taking the WW3TestBed, an app in the the old
structure which is a two component system of GSM before it had a cap
and WW3, and creating an app with the new app structure, build and run
systems with WW3 and GSM, including upgrading GSM to its trunk.

 _Assumptions:_  
This tutorial assumes you have an app working with the original/old
appBuilder and NEMSCompsetRun.  A separate tutorial for adding a new
component into the new system will be provided.
 
_Starting Point:_ 
We are starting out with the WW3TestBed App. To get a better idea of the starting point: 
 
    $ svn co -r 83803 https://svnemc.ncep.noaa.gov/projects/nems/apps/WW3TestBed/trunk WW3TestBedr83803
    $ cd WW3TestBedr83803
    $ svn propget svn:externals 
    # for snapshot revisions strictly version all constituent components
    #NEMS                 -r 72098 https://svnemc.ncep.noaa.gov/projects/nems/branches/NUOPC/development
    #NEMS/src/atmos/GSM   -r 71778 https://svnemc.ncep.noaa.gov/projects/gsm/branches/NUOPC/gsm67963branch
    #NEMS/src/atmos/nmm   -r 71873 https://svnemc.ncep.noaa.gov/projects/nems/external_comps/NMM
    #WW3                  -r 72006 https://svnemc.ncep.noaa.gov/projects/ww3/branches/esmf2/model
    # for development switch to head revisions
    #NEMS                 https://svnemc.ncep.noaa.gov/projects/nems/branches/NUOPC/development
    #NEMS/src/atmos/GSM   https://svnemc.ncep.noaa.gov/projects/gsm/branches/NUOPC/gsm67963branch
    #NEMS/src/atmos/nmm   https://svnemc.ncep.noaa.gov/projects/nems/external_comps/NMM
    #WW3                  https://svnemc.ncep.noaa.gov/projects/ww3/branches/esmf2/model
    # Test bed for example of running NEMS in WW3
    NEMS                -r 83802 https://svnemc.ncep.noaa.gov/projects/nems/branches/UGCS-Seasonal/twoWayWW3_from_r80562
    NEMS/src/atmos/gsm  -r 80525 https://svnemc.ncep.noaa.gov/projects/gsm/trunk
    NEMS/src/atmos/nmm  -r 80429 https://svnemc.ncep.noaa.gov/projects/nmmb/trunk
    NEMS/src/chem       -r 80344 https://svnemc.ncep.noaa.gov/projects/nceplibs/chem/trunk
    WW3                 -r 82617 https://svnemc.ncep.noaa.gov/projects/ww3/branches/esmf2/model
    $ ls 
    NEMS  WW3  ww3TestBed.appBuilder  ww3TestBed.compsetRun

Note, here we have 4 externals, NEMS, GSM, NMM, CHEM, and WW3 and all compsets are in the NEMS/compset directory. 
 
To build and run: 

    ./NEMS/NEMSAppBuilder
    ./NEMS/NEMSCompsetRun -compset NEMS/compsets/cfsr%20150401_1day_leapfrog_gsm%slg%T126_ww3%t188

This runs a two way coupled GSM<->WW3 system on a T126 grid, similar
to what is documented in \ref milestone_DREV84205

Stages of this tutorial:

* \subpage HowToOldToNewFolder

* \subpage HowToOldToNewAppBuilder

* \subpage HowToOldToNewCompBranch

* \subpage HowToOldToNewCompsets

* \subpage HowToOldToNewBuildRun 

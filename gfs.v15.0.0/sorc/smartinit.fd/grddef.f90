MODULE grddef
!=======================================================================
!  Create TYPE to store grid and run definitions
!=======================================================================
   TYPE GINFO
     INTEGER IMAX,JMAX,KMAX,FHR,CYC,DATE,HOUR,ITOT,OGRD,INHRFRQ,IFHRSTR
     LOGICAL LCYCON,LHR12,LNEST,LHIRESW
     CHARACTER*4 REGION, CORE
   END TYPE ginfo
END MODULE grddef



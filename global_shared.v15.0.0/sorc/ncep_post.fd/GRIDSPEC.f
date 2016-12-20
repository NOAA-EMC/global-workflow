      module GRIDSPEC_mod
!
!      COMMON /GRIDSPEC/
!     & DXVAL,DYVAL,CENLAT,CENLON,TRUELAT1,TRUELAT2,LATSTART,LONSTART
!     &,MAPTYPE,LATLAST,LONLAST,STANDLON
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none 
!
       integer DXVAL,DYVAL,CENLAT,CENLON,TRUELAT1,TRUELAT2
       integer LATSTART,LONSTART,LATLAST,LONLAST
       integer latnw,lonnw,latse,lonse
       integer MAPTYPE,STANDLON
       integer latstartv,cenlatv,lonstartv,cenlonv,latlastv,lonlastv
       real    PSMAPF
       character(len=1) gridtype
!
     end module GRIDSPEC_mod

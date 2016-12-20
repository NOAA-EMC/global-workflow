!   01-10-22  H CHUANG - MODIFIED TO PROCESS HYBRID MODEL OUTPUT
!   02-04-17  BALDWIN  - MODIFIED TO INCLUDE ALL 3D ARRAYS
!   11-10-18  SARAH LU - MODIFIED TO INCLUDE GOCART AEROSOLS
      module vrbls4d
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       implicit none
!
      real, allocatable :: DUST(:,:,:,:)        ! dust
      real, allocatable :: SALT(:,:,:,:)        ! sea salt
      real, allocatable :: SOOT(:,:,:,:)        ! black carbon
      real, allocatable :: WASO(:,:,:,:)        ! organic carbon
      real, allocatable :: SUSO(:,:,:,:)        ! sulfate
!
      end module vrbls4d

#include "./ESMFVersionDefine.h"

!-----------------------------------------------------------------------
!
      MODULE module_EARTH_INTERNAL_STATE
!
!-----------------------------------------------------------------------
!***  Contents of the ESMF internal state of the EARTH component.
!-----------------------------------------------------------------------
!
      USE ESMF
!
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      PRIVATE
!
      PUBLIC :: EARTH_INTERNAL_STATE,         &
                WRAP_EARTH_INTERNAL_STATE
!
!-----------------------------------------------------------------------
!
      TYPE EARTH_INTERNAL_STATE
!
        real(ESMF_KIND_R8)  :: medAtmCouplingIntervalSec
        real(ESMF_KIND_R8)  :: medOcnCouplingIntervalSec
!
      END TYPE EARTH_INTERNAL_STATE
!
!-----------------------------------------------------------------------
!
      TYPE WRAP_EARTH_INTERNAL_STATE
!
        TYPE(EARTH_INTERNAL_STATE),POINTER :: EARTH_INT_STATE
!
      END TYPE WRAP_EARTH_INTERNAL_STATE
!
!-----------------------------------------------------------------------
!
      END MODULE module_EARTH_INTERNAL_STATE
!
!-----------------------------------------------------------------------

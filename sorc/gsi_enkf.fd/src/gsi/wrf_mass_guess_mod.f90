!-------------------------------------------------------------------------
!    NOAA/GSD, GSI                                                       !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE: wrf_mass_guess_mod
!
! !INTERFACE:

module wrf_mass_guess_mod
!
! !DESCRIPTION: allocate/deallocate guess grids for clouds
!
! !USES:

use kinds, only: i_kind,r_kind
implicit none

! !REVISION HISTORY:
!    2011-04-29  Todling
!    2011-08-01  Lueken  - add module protext, replace F90 with f90 (no machine logic)

!EOP

private

public :: ges_xlon,ges_xlat,soil_temp_cld,isli_cld,ges_tten
public :: create_cld_grids,destroy_cld_grids

real(r_kind),allocatable,dimension(:,:,:):: ges_xlon      ! longitude
real(r_kind),allocatable,dimension(:,:,:):: ges_xlat      ! latitude
real(r_kind),allocatable,dimension(:,:,:):: soil_temp_cld ! soil temperature
real(r_kind),allocatable,dimension(:,:,:):: isli_cld      !
real(r_kind),allocatable,dimension(:,:,:,:):: ges_tten    ! radar temperature tendency

CONTAINS

!-------------------------------------------------------------------------
!    NOAA/GSD, GSI                                                       !
!-------------------------------------------------------------------------
!BOP
!
! IROUTINE:  create_cld_grids --- Alloc grid for guess of cloud
!
! INTERFACE:
!
  subroutine create_cld_grids

! USES:

    use kinds, only: i_kind,r_kind
    use constants,only: zero
    use gridmod, only: lat2,lon2,nsig
    use guess_grids, only: nfldsig
    implicit none

! INPUT PARAMETERS:

! OUTPUT PARAMETERS:

! DESCRIPTION: allocate grids to hold guess cloud fields
!
! REVISION HISTORY:
!   2007-08-16  Ming Hu, original code
!   2008-09-26  Ming Hu, add ges_tten
!   2011-04-29  Todling, move here from guess_grids
!
!EOP
!-------------------------------------------------------------------------

    integer(i_kind) i,j,k,n,istatus

!   Allocate and zero guess grids
    allocate (&
       ges_xlon(lat2,lon2,nfldsig),ges_xlat(lat2,lon2,nfldsig),&
       soil_temp_cld(lat2,lon2,nfldsig),isli_cld(lat2,lon2,nfldsig),&
       ges_tten(lat2,lon2,nsig,nfldsig), &
       stat=istatus)
    if (istatus/=0) write(6,*)'CREATE_CLD_GRIDS:  allocate error1, istatus=',&
       istatus,lat2,lon2,nsig,nfldsig

!  Default for cloud 
    do n=1,nfldsig
       do k=1,nsig
          do j=1,lon2
             do i=1,lat2
                ges_tten(i,j,k,n)=-20.0_r_kind
             end do
          end do
       end do
       ges_tten(:,:,nsig,n)=-10.0_r_kind
       do j=1,lon2
          do i=1,lat2
             ges_xlon(i,j,n)=zero
             ges_xlat(i,j,n)=zero
             soil_temp_cld(i,j,n)=zero
             isli_cld(i,j,n)=zero
          end do
       end do
    enddo

    return
  end subroutine create_cld_grids

!-------------------------------------------------------------------------
!    NOAA/GSD, GSI                                                       !
!-------------------------------------------------------------------------
!BOP
!
! IROUTINE:  destroy_cld_grids --- Dealloc grid for guess of cloud
!
! INTERFACE:
!
  subroutine destroy_cld_grids

! USES:
    implicit none
! INPUT PARAMETERS:
! OUTPUT PARAMETERS:

! DESCRIPTION: Dealloc grids that hold guess cloud fields
!
! REVISION HISTORY:
!   2007-08-16  Ming Hu, original code
!   2008-09-26  Ming Hu, add ges_tten
!   2011-04-29  Todling, move here from guess_grids
!
! REMARKS:
!   language: f90
!   machine:  ijet
!
! !AUTHOR:
!   Ming Hu          org: w/gsd     date: 2007-08-16
!
!EOP
!-------------------------------------------------------------------------

    integer(i_kind) istatus

    deallocate (ges_xlon,ges_xlat,&
                soil_temp_cld,isli_cld,ges_tten,stat=istatus)
    if (istatus/=0) &
       write(6,*)'DESTROY_CLD_GRIDS:  deallocate error1, istatus=',&
       istatus

    return
  end subroutine destroy_cld_grids

end module wrf_mass_guess_mod

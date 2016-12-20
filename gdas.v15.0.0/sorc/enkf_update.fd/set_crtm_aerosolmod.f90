module set_crtm_aerosolmod
!$$$ module documentation block
!           .      .    .                                       .
! module:   set_crtm_aerosolmod
!  prgmmr: todling          org: gmao                date: 2011-06-01
!
! abstract: module providing interface to set-crtm-aerosol procedures
!
! program history log:
!   2011-06-01  todling
!   2011-09-20  hclin   - separate na and na_crtm for p25 handling
!
! subroutines included:
!   sub Set_CRTM_Aerosol_
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

private

public Set_CRTM_Aerosol

interface Set_CRTM_Aerosol
  subroutine Set_CRTM_Aerosol_ ( km, na, na_crtm, aero_name, aero_conc, rh, aerosol)
  use kinds, only: i_kind,r_kind
  use constants, only: tiny_r_kind
  use mpimod, only: mype
  use CRTM_Aerosol_Define, only: CRTM_Aerosol_type
  use mpeu_util, only: getindex
  use crtm_module, only: SULFATE_AEROSOL,BLACK_CARBON_AEROSOL,ORGANIC_CARBON_AEROSOL,&
      DUST_AEROSOL,SEASALT_SSAM_AEROSOL,SEASALT_SSCM1_AEROSOL,SEASALT_SSCM2_AEROSOL,SEASALT_SSCM3_AEROSOL
  implicit none
  integer(i_kind) , intent(in)    :: km                ! number of levels
  integer(i_kind) , intent(in)    :: na                ! number of aerosols
  integer(i_kind) , intent(in)    :: na_crtm           ! number of aerosols seen by CRTM
  character(len=*), intent(in)    :: aero_name(na)     ! [na]    GOCART aerosol names: du0001, etc.
  real(r_kind),     intent(inout) :: aero_conc(km,na)  ! [km,na] aerosol concentration (Kg/m2)
  real(r_kind),     intent(in)    :: rh(km)            ! [km]    relative humdity [0,1]
  type(CRTM_Aerosol_type), intent(inout) :: aerosol(na_crtm)! [na]   CRTM Aerosol object
  end subroutine Set_CRTM_Aerosol_
end interface

end module set_crtm_aerosolmod

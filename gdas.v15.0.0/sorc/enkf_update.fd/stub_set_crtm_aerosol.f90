subroutine Set_CRTM_Aerosol_ ( km, na, na_crtm, aero_name, aero_conc, rh, aerosol)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    Set_CRTM_Aerosol_
!   prgmmr: hclin          org: ncar/mmm                date: 2011-09-20
!
! abstract: Set the CRTM Aerosol object given GOCART aerosol properties.
!
!
! program history log:
! 2011-02-23  da Silva - Initial version, FORTRAN-77 interface for GSI.
! 2011-08-01  Lueken   - Replaced F90 with f90 (no machine logic)
! 2011-09-20  HCLin    - Coded based on the WRFCHEM implementation of GOCART.
! 2013-11-17 Todling   - Brought HCLin implementation into stub - it live
!                        outside GSI, but to not break DTC usage it's placed
!                        here temporarily. 
!
!   input argument list:
!     km        : number of CRTM levels
!     na        : number of aerosols
!     na_crtm   : number of aerosols seen by CRTM
!     aero_name : GOCART aerosol names
!     aero_conc : aerosol concentration (Kg/m2)
!     rh        : relative humdity [0,1]
!     aerosol   : CRTM Aerosol object
!
!   output argument list:
!     aero_conc : aerosol concentration (Kg/m2)
!     aerosol   : CRTM Aerosol object
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

! USES:

  use kinds, only: i_kind,r_kind
  use constants, only: tiny_r_kind
  use mpimod, only: mype
  use CRTM_Aerosol_Define, only: CRTM_Aerosol_type
  use mpeu_util, only: getindex
  use crtm_module, only: SULFATE_AEROSOL,BLACK_CARBON_AEROSOL,ORGANIC_CARBON_AEROSOL,&
      DUST_AEROSOL,SEASALT_SSAM_AEROSOL,SEASALT_SSCM1_AEROSOL,SEASALT_SSCM2_AEROSOL,SEASALT_SSCM3_AEROSOL

  implicit none

! !ARGUMENTS:

  integer(i_kind) , intent(in)    :: km                ! number of levels
  integer(i_kind) , intent(in)    :: na                ! number of aerosols
  integer(i_kind) , intent(in)    :: na_crtm           ! number of aerosols seen by CRTM
  character(len=*), intent(in)    :: aero_name(na)     ! [na]    GOCART aerosol names
  real(r_kind),     intent(inout) :: aero_conc(km,na)  ! [km,na] aerosol concentration (Kg/m2)
  real(r_kind),     intent(in)    :: rh(km)            ! [km]    relative humdity [0,1]

  type(CRTM_Aerosol_type), intent(inout) :: aerosol(na_crtm)! [na]   CRTM Aerosol object

  integer(i_kind) :: i, k
  integer(i_kind) :: indx_p25, indx_dust1, indx_dust2, indx_dust3, indx_dust4, indx_dust5
  integer(i_kind) :: indx_bc1, indx_oc1

  indx_bc1=-1; indx_oc1=-1; indx_dust1=-1; indx_dust2=-1
  indx_dust3=-1; indx_dust4=-1; indx_dust5=-1; indx_p25=-1

  indx_p25   = getindex(aero_name,'p25')
  indx_dust1 = getindex(aero_name,'dust1')
  indx_dust2 = getindex(aero_name,'dust2')
  indx_dust3 = getindex(aero_name,'dust3')
  indx_dust4 = getindex(aero_name,'dust4')
  indx_dust5 = getindex(aero_name,'dust5')
  indx_bc1   = getindex(aero_name,'bc1')
  indx_oc1   = getindex(aero_name,'oc1')

  do i = 1, na

     if ( trim(aero_name(i)) == 'p25' ) cycle

     ! assign aerosol type
     select case ( trim(aero_name(i)) )
        case ('sulf')
           aerosol(i)%type  = SULFATE_AEROSOL
        case ('bc1','bc2')
           aerosol(i)%type  = BLACK_CARBON_AEROSOL
        case ('oc1','oc2')
           aerosol(i)%type  = ORGANIC_CARBON_AEROSOL
        case ('dust1','dust2','dust3','dust4','dust5')
           aerosol(i)%type  = DUST_AEROSOL
        case ('seas1')
           aerosol(i)%type  = SEASALT_SSAM_AEROSOL
        case ('seas2')
           aerosol(i)%type  = SEASALT_SSCM1_AEROSOL
        case ('seas3')
           aerosol(i)%type  = SEASALT_SSCM2_AEROSOL
        case ('seas4')
           aerosol(i)%type  = SEASALT_SSCM3_AEROSOL
     end select

     if ( indx_p25 > 0 ) then
        ! partition p25 to dust1 and dust2
        if ( i == indx_dust1 ) then
           aero_conc(:,i) = aero_conc(:,i)+ 0.78_r_kind*aero_conc(:,indx_p25)
        endif
        if ( i == indx_dust2 ) then
           aero_conc(:,i) = aero_conc(:,i)+ 0.22_r_kind*aero_conc(:,indx_p25)
        endif
     endif

     ! crtm aerosol structure
     do k = 1, km
        aerosol(i)%concentration(k) = max(tiny_r_kind, aero_conc(k,i))
        ! calculate effective radius
        aerosol(i)%effective_radius(k) &
           = GOCART_Aerosol_size(i, aerosol(i)%type, rh(k))
        ! 5 dust bins
        aerosol(indx_dust1)%effective_radius(k) = 0.55_r_kind
        aerosol(indx_dust2)%effective_radius(k) = 1.4_r_kind
        aerosol(indx_dust3)%effective_radius(k) = 2.4_r_kind
        aerosol(indx_dust4)%effective_radius(k) = 4.5_r_kind
        aerosol(indx_dust5)%effective_radius(k) = 8.0_r_kind
     enddo

  enddo  ! na

  contains

  function GOCART_Aerosol_size( kk, itype,  & ! Input
                                       eh ) & ! Input in 0-1
                           result( R_eff  )   ! in micrometer
  use crtm_aerosolcoeff, only: AeroC
  implicit none
!
!   modified from a function provided by Quanhua Liu
!
  integer(i_kind) ,intent(in) :: kk, itype
  real(r_kind)    ,intent(in) :: eh

  integer(i_kind) :: j1,j2,k
  real(r_kind)    :: h1
  real(r_kind)    :: R_eff

  if ( itype==DUST_AEROSOL ) then
     return
  else if ( itype==BLACK_CARBON_AEROSOL .and. kk==indx_bc1 ) then
     R_eff = AeroC%Reff(1,itype )
     return
  else if ( itype==ORGANIC_CARBON_AEROSOL .and. kk==indx_oc1 ) then
     R_eff = AeroC%Reff(1,itype )
     return
  endif

  j2 = 0
  if ( eh < AeroC%RH(1) ) then
     j1 = 1
  else if ( eh > AeroC%RH(AeroC%n_RH) ) then
     j1 = AeroC%n_RH
  else
     do k = 1, AeroC%n_RH-1
        if ( eh <= AeroC%RH(k+1) .and. eh > AeroC%RH(k) ) then
           j1 = k
           j2 = k+1
           h1 = (eh-AeroC%RH(k))/(AeroC%RH(k+1)-AeroC%RH(k))
           exit
        endif
     enddo
  endif

  if ( j2 == 0 ) then
     R_eff = AeroC%Reff(j1,itype )
  else
     R_eff = (1.0_r_kind-h1)*AeroC%Reff(j1,itype ) + h1*AeroC%Reff(j2,itype )
  endif

  return
  end function GOCART_Aerosol_size

end subroutine Set_CRTM_Aerosol_

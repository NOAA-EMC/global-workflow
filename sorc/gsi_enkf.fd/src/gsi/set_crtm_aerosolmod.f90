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
!   2019-03-21  martin - replaced blank subroutine here with that previously
!                        in stub_set_crtm_aerosol.f90;
!                        also moved eff rad for dust to size function
!   2022-05-24 h.wang  - add interface for RRFS-CMAQ regional model
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

public Set_CRTM_Aerosol,set_crtm_aerosol_fv3_cmaq_regional

contains

    subroutine set_crtm_aerosol_fv3_cmaq_regional ( km, na, na_crtm, aero_name, aero_conc, rh, aerosol)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    set_crtm_aerosol_fv3_cmaq_regional
!   prgmmr: H.Wang          org: NOAA/ESRL/GSL          date: 2021-04-29
!   
!   Updated based on set_crtm_aerosol
!
! abstract: Set CMAQ aerosol for CRTM Aerosol object 
!
!
!   input argument list:
!     km        : number of CRTM levels
!     na        : number of aerosols
!     na_crtm   : number of aerosols seen by CRTM
!     aero_name : GOCART aerosol names
!     aero_conc : aerosol concentration (Kg/m2)
!     rh        : relative humidity [0,1]
!     aerosol   : CRTM Aerosol object
!
!   output argument list:
!     aero_conc : aerosol concentration (Kg/m2)
!     aerosol   : CRTM Aerosol object

    use kinds, only: i_kind,r_kind
    use constants, only: tiny_r_kind
    use CRTM_Aerosol_Define, only: CRTM_Aerosol_type
    use mpeu_util, only: getindex

    use chemmod, only: naero_cmaq_fv3,aeronames_cmaq_fv3,imodes_cmaq_fv3
    use chemmod, only: crtm_aerosol_model
    use chemmod, only: raod_radius_mean_scale !,raod_radius_std_scale
    use crtm_module, only: SULFATE_AEROSOL,BLACK_CARBON_AEROSOL,ORGANIC_CARBON_AEROSOL,&
        DUST_AEROSOL,SEASALT_SSAM_AEROSOL,SEASALT_SSCM1_AEROSOL,SEASALT_SSCM2_AEROSOL,SEASALT_SSCM3_AEROSOL

    implicit none

!Aerosol types  (CRTM CMAQ LUTs):
!1. Dust
!2. Soot
!3. Water soluble
!4. Sulfate
!5. Sea salt
!6. Water
!7. Insoluble
!8. dust-like

    INTEGER(i_kind),  PARAMETER ::        INVALID_AEROSOL = 0
    INTEGER(i_kind),  PARAMETER ::      DUST_CMAQ_AEROSOL = 1
    INTEGER(i_kind),  PARAMETER ::           SOOT_AEROSOL = 2
    INTEGER(i_kind),  PARAMETER ::  WATER_SOLUBLE_AEROSOL = 3
    INTEGER(i_kind),  PARAMETER ::   SULFATE_CMAQ_AEROSOL = 4
    INTEGER(i_kind),  PARAMETER ::        SEASALT_AEROSOL = 5
    INTEGER(i_kind),  PARAMETER ::          WATER_AEROSOL = 6
    INTEGER(i_kind),  PARAMETER ::      INSOLUBLE_AEROSOL = 7
    INTEGER(i_kind),  PARAMETER ::      DUST_LIKE_AEROSOL = 8


    integer(i_kind) , intent(in)    :: km                ! number of levels
    integer(i_kind) , intent(in)    :: na                ! number of aerosols
    integer(i_kind) , intent(in)    :: na_crtm           ! number of aerosols seen by CRTM
    character(len=*), intent(in)    :: aero_name(na)     ! [na]    GOCART aerosol names
    real(r_kind),     intent(inout) :: aero_conc(km,na)  ! [km,na] aerosol concentration (Kg/m2)
    real(r_kind),     intent(in)    :: rh(km)            ! [km]    relative humidity [0,1]
    type(CRTM_Aerosol_type), intent(inout) :: aerosol(na_crtm)!   CRTM Aerosol object

    Real(r_kind),    Parameter :: def_diam( 3 )   = (/ 15.0E-3_r_kind, 80.0E-3_r_kind, 600.0E-3_r_kind /) !um for CRTM 
    Real(r_kind),    Parameter :: def_sigma_g( 3 ) = (/ 1.70_r_kind, 2.0_r_kind,2.2_r_kind /)
 
    integer(i_kind) :: i, k, irh

    integer(i_kind) :: indx_dust1, indx_dust2, indx_dust3, indx_dust4,indx_dust5
    integer(i_kind) :: indx_bc1, indx_oc1

    indx_bc1=-1; indx_oc1=-1; indx_dust1=-1; indx_dust2=-1
    indx_dust3=-1; indx_dust4=-1; indx_dust5=-1;

!!! Attention !!! 
!!! Please use this interface as a start point to modify.
!!!  Users need to adapt the aerosol mapping parts for their own application! 
    
    do i = 1, na_crtm
       if (crtm_aerosol_model == "CMAQ")then
         select case ( trim(aero_name(i)) )
           case ('aalj','acaj','afej','akj','amgj','amnj','asij','atij')
             aerosol(i)%type  = DUST_CMAQ_AEROSOL
           case ('asoil','acors')
             aerosol(i)%type  = DUST_LIKE_AEROSOL
           case ('aeci','aecj')
             aerosol(i)%type  = SOOT_AEROSOL
           case ('alvoo1i','alvoo2i','anh4i','anh4j','ano3i','ano3j','aso4k','asvoo1i','asvoo2i')
             aerosol(i)%type  = WATER_SOLUBLE_AEROSOL
           case ('aso4i','aso4j','acli','anh4k','ano3k')
             aerosol(i)%type  = SULFATE_CMAQ_AEROSOL 
           case ('aclj','aclk','anai','anaj','aseacat')
             aerosol(i)%type  = SEASALT_AEROSOL
           case ('aivpo1j','alvpo1i','alvpo1j','aothri','aothrj','asvpo1i','asvpo1j','asvpo2i','asvpo2j', &
                 'asvpo3j','atol1j','axyl1j','axyl2j','axyl3j')
             aerosol(i)%type  = INSOLUBLE_AEROSOL 
           case default
             aerosol(i)%type  = INSOLUBLE_AEROSOL ! INVALID_AEROSOL 
         end select
       end if

       if (crtm_aerosol_model == "GOCART" .or. crtm_aerosol_model == "CRTM")then
       !!! assign cmaq aerosol to crtm_gocart 
       !!! GOCCART is renamed to CRTM in an udpated CRTM2.4 repo.
!Tang, Y., Pagowski, M., Chai, T., Pan, L., Lee, P., Baker, B., Kumar, R., Delle
!Monache, L., Tong, D., and Kim, H.-C.: A case study of aerosol data
!assimilation with the Community Multi-scale Air Quality Model over the
!contiguous United States using 3D-Var and optimal interpolation methods,
!Geosci. Model Dev., 10, 4743–4758,
!https://doi.org/10.5194/gmd-10-4743-2017,2017.
         select case ( trim(aero_name(i)) )
           case ('aalj','acaj','afej','akj','amgj','amnj','asij','asoil','atij','acors')
             aerosol(i)%type  = DUST_AEROSOL
           case ('aeci','aecj')
             aerosol(i)%type  = BLACK_CARBON_AEROSOL 
           case ('aso4i','aso4j','acli','anh4k','ano3k','anh4i','anh4j','ano3i','ano3j','aso4k')
             aerosol(i)%type  = SULFATE_AEROSOL
           case ('aclj','anai','anaj','aclk','aseacat')
             aerosol(i)%type  = SEASALT_SSAM_AEROSOL
           case ('alvoo1i','alvoo2i','asvoo1i','asvoo2i','aivpo1j','alvpo1i','alvpo1j', &
                'aothri','aothrj','asvpo1i','asvpo1j','asvpo2i','asvpo2j','asvpo3j',&
                'atol1j','atol2j','atol3j','axyl1j','axyl2j','axyl3j', &
                'abnz1j','abnz2j','abnz3j','aiso1j','aiso2j','aiso3j','atrp1j','atrp2j','asqtj', &
                'aalk1j','aalk2j','apah1j','apah2j','apah3j','aorgcj','aolgbj','aolgaj',&
                'alvoo1j','alvoo2j','asvoo1j','asvoo2j','asvoo3j','apcsoj'  )
             aerosol(i)%type  = ORGANIC_CARBON_AEROSOL 
           case default
             aerosol(i)%type  = INVALID_AEROSOL
         end select
       end if


       if (crtm_aerosol_model == "GOCART-GEOS5")then
! Aerosol_Type_Name =
!1  "Dust",
!2  "Sea salt",
!3  Organic carbon hydrophobic
!4  Organic carbon hydrophilic
!5  Black carbon hydrophobic
!6  Black carbon hydrophilic
!7  "Sulfate",
!8  "Nitrate" ;
         select case ( trim(aero_name(i)) )
           case ('aalj','acaj','afej','akj','amgj','amnj','asij','asoil','atij','acors')
             aerosol(i)%type  = 1
           case ('aclj','anai','anaj','aclk','aseacat')
             aerosol(i)%type  = 2
           case ('aeci','aecj')
             aerosol(i)%type  = 6
           case ('aso4i','aso4j','acli','aso4k')
             aerosol(i)%type  = 7
           case ('anh4i','anh4j','anh4k','ano3i','ano3j','ano3k')
             aerosol(i)%type  = 8
           case ('alvoo1i','alvoo2i','asvoo1i','asvoo2i','aivpo1j','alvpo1i','alvpo1j', &
                'aothri','aothrj','asvpo1i','asvpo1j','asvpo2i','asvpo2j','asvpo3j', &
                'atol1j','atol2j','atol3j','axyl1j','axyl2j','axyl3j', &
                'abnz1j','abnz2j','abnz3j', 'aiso1j','aiso2j','aiso3j','atrp1j','atrp2j','asqtj', & 
                'aalk1j','aalk2j','apah1j','apah2j','apah3j','aorgcj','aolgbj','aolgaj', & 
                'alvoo1j','alvoo2j','asvoo1j','asvoo2j','asvoo3j','apcsoj'  )
             aerosol(i)%type  = 4 
           case default
             aerosol(i)%type  = INVALID_AEROSOL  
         end select
       end if ! GOCART-GEOS5


       ! crtm aerosol structure
       do k = 1, km
          irh = int( 100.0_r_kind * rh(k)  ) ! truncate relative humidity to nearest
          irh = max( 1, min( 99, irh ) ) ! set bounds

          aerosol(i)%concentration(k) = max(tiny_r_kind, aero_conc(k,i))
          aerosol(i)%effective_radius(k) = raod_radius_mean_scale*0.5_r_kind*def_diam(imodes_cmaq_fv3(i)) & 
                                          *exp(2.5_r_kind*(log(def_sigma_g(imodes_cmaq_fv3(i))))**2) 
! for crtm2.4.1 with cmaq LUTs
!          aerosol(i)%effective_variance(k) = raod_radius_std_scale*def_sigma_g(imodes_cmaq_fv3(i)) 

       enddo
       
    enddo  ! na
end    subroutine set_crtm_aerosol_fv3_cmaq_regional

  subroutine Set_CRTM_Aerosol ( km, na, na_crtm, aero_name, aero_conc, rh, aerosol)
  
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    Set_CRTM_Aerosol
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
! 2019-03-21 Martin    - Moved aerosol eff radius for dust to function GOCART_Aerosol_size
!
!   input argument list:
!     km        : number of CRTM levels
!     na        : number of aerosols
!     na_crtm   : number of aerosols seen by CRTM
!     aero_name : GOCART aerosol names
!     aero_conc : aerosol concentration (Kg/m2)
!     rh        : relative humidity [0,1]
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
    real(r_kind),     intent(in)    :: rh(km)            ! [km]    relative humidity [0,1]
  
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
       if (kk==indx_dust1) then
            R_eff = 0.55_r_kind
       else if (kk==indx_dust2) then
            R_eff = 1.4_r_kind
       else if (kk==indx_dust3) then
            R_eff = 2.4_r_kind
       else if (kk==indx_dust4) then
            R_eff = 4.5_r_kind
       else if (kk==indx_dust5) then
            R_eff = 8.0_r_kind
       end if
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
  
  end subroutine Set_CRTM_Aerosol

end module set_crtm_aerosolmod

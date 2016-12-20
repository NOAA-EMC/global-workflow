!========================================================================
! = = = = = = = = = = = = module DerivedFields  = = = = = = = = = = = =
!========================================================================
module DerivedFields

  private
  public derive_fields, mixing_ratio
  public PRECIPS

  ! Precipitation types
  type :: precipitations_t
    integer :: NONE = 0
    integer :: RAIN = 1
    integer :: SNOW = 2
    integer :: OTHER = 3
    integer :: CONVECTION = 4
  end type precipitations_t
  type(precipitations_t), parameter :: PRECIPS = precipitations_t()  

contains

!-----------------------------------------------------------------------+
  subroutine derive_fields(t, rh, pres, hgt, cwat, nz, &
                           topoK, xacp,xcp, cin, cape, &
                           ept, wbt, twp, pc, kx, lx, tott, prcpType)
    IMPLICIT NONE
! 3-D derived data:
!     ept - equivalent potential temperature
!     wbt - wet bulb temperature
!     twp - total water path
!
! 2-D derived data: (indice for convective icing severity)
!     kx   - k index
!     lx   - lifted index
!     tott - total totals
!
! 2-D derived data:
!     pc   - precipitation condensate
!     prcpType - surface precipitation type

    integer, intent(in) :: nz, topoK
    real, intent(in) :: t(nz), rh(nz), pres(nz), hgt(nz), cwat(nz)
    real, intent(in) :: xacp, xcp, cin, cape
    real, intent(out) :: ept(nz), wbt(nz), twp(nz)
    real, intent(out) :: pc, kx, lx, tott
    integer, intent(out) :: prcpType

    real, allocatable :: td(:), tlc(:), wvm(:)
    integer :: k

    allocate(td(nz))
    allocate(tlc(nz))
    allocate(wvm(nz))

    td(:)  = getDewPointTemp(t(:), rh(:))
    tlc(:) = get_tLCL(t(:), td(:))
    wvm(:) = mixing_ratio(td(:), pres(:))

    ept(:) = getThetae(pres(:), t(:), wvm(:), tlc(:))
    wbt(:) = getThetaw(t(:), td(:), pres(:))

    call calc_totalWaterPath(hgt, pres, t, cwat, nz, twp)

    pc = getPrecipCond(cwat, nz, topoK)

    ! indice for convective icing severity
    call calc_indice(t, td, pres, wvm, nz, topoK, kx, lx, tott)

    prcpType=getPrecipType(pres,hgt,t,rh,wbt,nz,xacp,xcp,cin,cape,lx)

    deallocate(td)
    deallocate(tlc)
    deallocate(wvm)

    return
  end subroutine derive_fields

!-----------------------------------------------------------------------+
! dew point temperature in K 
! t in K
  elemental real function getDewPointTemp(t, rh)
    IMPLICIT NONE
    real, intent(in) :: t, rh

    real vapr, rm

    ! actual water vapor presure in pascal
    ! 611.2 Pa is the saturated vapor presure at 0°C
    ! Pa = (rh/100) * Ps
    vapr = (max(1.0e-6,rh) / 100.0) * getVaporPres(t)
    rm   = log(vapr/611.2)

    getDewPointTemp = 243.5 * rm / (17.67 - rm) + 273.15

    return 
  end function getDewPointTemp

!-----------------------------------------------------------------------+
! Equivalent potential temperature in K
! pres in Pa
! t in K
! wvm in kg/kg
  elemental real function getThetae(pres, t, wvm, tAtLCL)
    IMPLICIT NONE
    real, intent(in) :: pres, t, wvm, tatlcl

    real rmix, e, thtam

    rmix = max(0.0,wvm)  ! in g/kg
    ! potential temperature
    e      = (2.0/7.0) * ( 1.0 - 0.28 * rmix * 0.001)
    thtam  = t * ((100000.0/pres)**e) 
    getThetae = thtam * exp( (3.376/tAtLCL - 0.00254) *  &
               (rmix * (1. + (.81 * rmix * 0.001)) ))

    return
  end function getThetae

!-----------------------------------------------------------------------+
! saturation vapor presure in Pa
! t in K
  elemental real function getVaporPres(t)
    IMPLICIT NONE
    real, intent(in) :: t

    real tc

    ! 611.2 Pa is the saturated vapor presure at 0°C
    tc = t-273.15
    getVaporPres = 611.2 * exp( 17.67*tc/(tc+243.5))

    return
  end function getVaporPres

!-----------------------------------------------------------------------+
! lifted condensation level temperature in K
! t and td in K
  elemental real function  get_tLCL(t, td)
    IMPLICIT NONE
    real, intent(in) :: t, td

    get_tLCL = 1. / (1./(td - 56.) + log(t/td)/800.0) + 56.0

    return
  end function get_tLCL

!-----------------------------------------------------------------------+
! mixing ratio in g/kg = water vapr/dry air
! td in K
! pres in Pa
  elemental real function  mixing_ratio(td, pres)
    IMPLICIT NONE
    real, intent(in) :: td, pres

    real corr, e

    corr = 1.001 + ( (pres/100.0 - 100) /900.) * 0.0034
    e = getVaporPres(td) * corr
    mixing_ratio = 0.62197 * (e/(pres-e)) * 1000.0

    return
  end function mixing_ratio

!-----------------------------------------------------------------------+
! wet bulb temperature in K
! t and td in K
! pres in Pa
  elemental real function getThetaw(t, td, pres)
    IMPLICIT NONE
    real, intent(in) :: t, td, pres

    real :: Vl, Cp, dt, top, bottom, twet, twetNew
    real :: rmixr, smixr
    integer :: i

    Vl = 2.5e6
    Cp = 1004.0

    dt = 9.9e9
    top = t
    bottom = td

    do i = 1, 100

       twet = (top + bottom) / 2.0
       rmixr = mixing_ratio(td, pres) / 1000.0
       smixr = mixing_ratio(twet, pres) / 1000.0

       twetNew = t - (Vl/Cp) * (smixr-rmixr)
       if(twetNew < twet) then
          top = twet
       else 
          bottom = twet
       end if

       dt = abs(twet - twetNew)
       if (dt <= 0.1) exit

    end do

    ! assign a value anyway, don't need  (dt < 1.)
    getThetaw  = twet

    return
  end function getThetaw

!-----------------------------------------------------------------------+
! Precipitation Condensate in g/kg
! cwat kg/kg
  real function getPrecipCond(cwat, nz, topoK)
    IMPLICIT NONE
    real, intent(in) :: cwat(nz)
    integer, intent(in) :: nz, topoK

    integer :: k

    getPrecipCond = 0.0

    do  k = topoK, topoK-2, -1 ! three levels
       ! convert from kg/kg to g/kg
       getPrecipCond = cwat(k) * 1000.0 + getPrecipCond
    end do

    return
  end function getPrecipCond

!-----------------------------------------------------------------------+
! These 2-D indice are used for convective icing severity
  subroutine calc_indice(t, td, pres, wvm, nz, topoK, &
                         kIndex, liftedIndex, totalTotals)
    IMPLICIT NONE
    integer, intent(in) :: nz, topoK
    real, intent(in)    :: t(nz), td(nz), pres(nz), wvm(nz)
    real, intent(out)   :: kIndex, liftedIndex, totalTotals

    integer :: k
    real :: t500hPa, t700hPa, t850hPa, dpt700hPa, dpt850hPa

    real :: surfaceTemp,surfaceDewPtTemp,surfacePressure,surfaceMIXR
    real :: tempAtLCL, theta, pressAtLCL, thetaEAtLCL, tempFromThetaE, tem

! write(0,*)' nz=',nz,' pres=',pres(:)
    t500hPa   = t(nz)
    t700hPa   = t(nz)
    dpt700hPa = td(nz)
    t850hPa   = t(nz)
    dpt850hPa = td(nz)
!
    do k = nz, 2, -1

       ! use linear interpolation

!      write(0,*)'k=',k,' pres=',pres(k)
       if ((pres(k)- 50000.0 >= 0.) .and. (pres(k-1)- 50000.0 < 0.) ) then
          if (abs(pres(k)- 50000.0) <= 0.1) then
             t500hPa = t(k)
          elseif (abs(pres(k-1)- 50000.0) <= 0.1) then
             t500hPa = t(k-1)
          else
             t500hPa = t(k) - ((pres(k)-50000.0)/(pres(k)-pres(k-1))) * (t(k)-t(k-1))
          end if
          exit ! from surface to space, time to break the loop
       end if

       if ((pres(k)- 70000.0 >= 0.) .and. (pres(k-1)- 70000.0 < 0.) ) then
          if (abs(pres(k)- 70000.0) <= 0.1) then
             t700hPa   = t(k)
             dpt700hPa = td(k)
          elseif (abs(pres(k-1)- 70000.0) <= 0.1) then
             t700hPa   = t(k-1)
             dpt700hPa = td(k-1)
          else
             tem = (pres(k)-70000.0)/(pres(k)-pres(k-1))
             t700hPa   = t(k)  - tem * (t(k)-t(k-1))
             dpt700hPa = td(k) - tem * (td(k)-td(k-1))
          end if
       endif

!      write(0,*)'k=',k,' pres=',pres(k),pres(k-1)
       if ((pres(k)- 85000.0 >= 0.) .and. (pres(k-1)- 85000.0 < 0.) ) then
          if (abs(pres(k)- 85000.0) <= 0.1) then
             t850hPa   = t(k)
             dpt850hPa = td(k)
          elseif (abs(pres(k-1)- 85000.0) <= 0.1) then
             t850hPa   = t(k-1)
             dpt850hPa = td(k-1)
          else
             tem = (pres(k)-85000.0)/(pres(k)-pres(k-1))
             t850hPa   = t(k)  - tem * (t(k)-t(k-1))
             dpt850hPa = td(k) - tem * (td(k)-td(k-1))
          end if
       endif

    end do

    ! kindex in C
    kIndex = (t850hPa-t500hPa) + (dpt850hPa-273.15) - (t700hPa-dpt700hPa)

    ! total total index
    totalTotals = t850hPa + dpt850hPa - t500hPa * 2.0

    ! lifted index
    ! use topoK instead of 1 - Y Mao
    surfaceTemp      = t(topoK)
    surfaceDewPtTemp = td(topoK)
    surfacePressure  = pres(topoK)
    surfaceMIXR      = wvm(topoK)

    tempAtLCL      = get_tLCL(surfaceTemp, surfaceDewPtTemp)
    theta          = surfaceTemp * (100000.0/surfacePressure)**0.286
    pressAtLCL     = 100000.0 * (tempAtLCL / theta)**3.4965
    thetaEAtLCL    = getThetae(pressAtLCL,tempAtLCL,surfaceMIXR,tempAtLCL)

    tempFromThetaE = getTemperature(thetaEAtLCL, 50000.0)

    liftedIndex    = t500hPa - tempFromThetaE

    return
  end subroutine calc_indice

!-----------------------------------------------------------------------+
  real function getTemperature(thetaE, pres)
    IMPLICIT NONE
    real, intent(in) :: thetaE, pres

    real :: guess, w1, w2, thetaE1, thetaE2, cor
    integer :: i

    guess = (thetaE - 0.5 * (max(thetaE - 270.0, 0.0))**1.05) * &
            (pres/100000.0)**0.2

    do i = 1, 100
       w1 = calcRSubS(pres, guess)
       w2 = calcRSubS(pres, guess + 1.0)
       thetaE1 = getThetae(pres, guess, w1, guess)
       thetaE2 = getThetae(pres, guess + 1.0, w2, guess + 1.0)
       cor = (thetaE - thetaE1)/(thetaE2 - thetaE1)
       guess = guess + cor

       if (abs(cor) < 0.01) then
          exit
       end if
    end do

    getTemperature = guess

    return
  end function getTemperature

!-----------------------------------------------------------------------+
  elemental real function calcRSubS(pres, temp)
    IMPLICIT NONE
    real, intent(in) :: pres, temp

    real :: eSubs

    eSubS = calcESubS(temp)
    calcRSubS = (0.622*eSubS)/(pres - eSubS)

    return
  end function calcRSubS

!-----------------------------------------------------------------------+
  elemental  real function calcESubS(temp)
    IMPLICIT NONE
    real, intent(in) :: temp

    real :: x
    real, parameter :: coeffs(9) = &
           (/ 610.5851, 44.40316, 1.430341, 0.02641412,  2.995057e-4,&
           2.031998e-6, 6.936113e-9, 2.564861e-12, -3.704404e-14 /)


    x= max(-80.0, temp - 273.15)

    calcESubS = coeffs(1) + x*(coeffs(2) + x*(coeffs(3) + x*(coeffs(4) + &
                x*(coeffs(5) + x*(coeffs(6) + x*(coeffs(7) + &
                x*(coeffs(8) + x*coeffs(9))))))))
    return
  end function calcESubS

!-----------------------------------------------------------------------+
  subroutine calc_totalWaterPath(hgt, pres, t, cwat, nz, twp)
    IMPLICIT NONE
    integer, intent(in) :: nz
    real, intent(in) :: hgt(nz), pres(nz), t(nz), cwat(nz)
    real, intent(out):: twp(nz)

    real :: condensateIntegrated
    integer :: topIdx, baseIdx
    integer :: k, m   

    lp100: do k = 1, nz

       twp(k) = 0.0
       topIdx = nz
       baseIdx = nz

       ! get the layer top and base for the total condensate layer
       lp200:  do m = k, 2, -1 
          if (cwat(k) * 1000. > 0.001) then
             topIdx = m
          else
             exit
          end if
       end do lp200

       lp300: do m = k, nz-1, -1
          if (cwat(k) * 1000. > 0.001) then
             baseIdx = m
          else
             exit
          end if
       end do lp300

       ! get the integrated condensate from the rep to the layer top
       condensateIntegrated = 0.0
       lp400: do m = topIdx, baseIdx
          if (t(m) <= 273.15) then
             condensateIntegrated = condensateIntegrated + &
                  ( ((cwat(m)*pres(m)) / (287.05 * t(m)) ) * &
                  (hgt(m-1) - hgt(m) + 0.001))
          end if
       end do lp400

       twp(k) = condensateIntegrated

    end do lp100

    return
  end subroutine calc_totalWaterPath

!-----------------------------------------------------------------------+
  integer function getPrecipType(pres, hgt, t, rh, wbt, nz, &
       xacp, xcp, cin, cape, lx)
    IMPLICIT NONE
    integer, intent(in) :: nz
    real, intent(in) :: pres(nz), hgt(nz), t(nz), rh(nz), wbt(nz)
    real, intent(in) :: xacp, xcp, cin, cape, lx

    integer, allocatable :: wxType(:)

    integer :: idxMelt, idxRefz, iceidx
    real :: coldtemp, tColdArea, wetBuldArea

    integer :: k, m

    allocate(wxType(nz))

    idxMelt = nz ! melting
    idxRefz = nz ! refreezing

    ! Check for convection first
    if_conv: if ( xacp >= 1.0 .and. cape >= 1000.0 .and. cin > -100. &
         .and. lx < 0.0) then
       getPrecipType = PRECIPS% CONVECTION
    else

       ! find the height(index) where the snow melts, wetBulbTemp > 0C
       lp100: do k = 1, nz
          if( wbt(k) > 273.15) then
             idxMelt = k
             ! now look for a refreezing layer below warmnose
             do m = idxMelt, nz
                if (t(m) < 273.15) then
                   idxRefz = m
                   exit
                end if
             end do
             exit
          end if
       end do lp100

       ! find the level that is below 150hPa or
       ! the min and max wet bulb temperature
       lp200: do k = nz, 1, -1

          wxType(k) = 0

          if_pres_wbt: if ( pres(k) >= 15000. .or. &
               (wbt(k) >= 200. .and. wbt(k) <= 1000.)) then
             iceIdx = k
             coldTemp = t(k)

             do m = k, 1, -1
                ! look for level below cloud_top_pressure and
                !     cloud_top_altitude
                ! look for ctt
                if( (pres(m)>=30000. .or. (hgt(m)>hgt(nz)+700.))&
                     .and. (rh(m) > 90.) .and. (t(m) < coldTemp)) then
                   coldTemp = t(m)
                   iceIdx = m
                end if
             end do

             ! found a cloud -- identify the precipitaiton type
             if_iceIdx: if (iceIdx /= k) then

                ! Sum the pos. area between wetBulbTemp and
                !     the 0 deg isotherm below coldTemp
                tColdArea = 0.0
                do m = k, iceIdx, -1
                   if (wbt(m) >= 273.15) then
                      tColdArea = tColdArea +  (wbt(m)-273.15) * &
                                  (hgt(m-1) - hgt(m))
                   end if
                end do

                ! sum the total area between the wet bulb T and the 0 C isotherm 
                ! in the lowest precip_type_altitude
                wetBuldArea = 0.0
                do m = iceIdx, nz
                   if ( (hgt(m) <= hgt(nz)+1500) .and. &
                        (m > idxRefz)) then
                      wetBuldArea = wetBuldArea + (wbt(m)-273.15) * &
                                    (hgt(m-1) - hgt(m))
                   end if
                end do

                ! get the probable Precip type
                if (coldTemp > 265.15) then
                   wxType(k) = PRECIPS% OTHER
                else if (tColdArea < 350.) then
                   if(t(k) <= 273.15) then
                      wxType(k) = PRECIPS% SNOW
                   else 
                      wxType(k) = PRECIPS% RAIN
                   end if
                else if (wetBuldArea <= -250.) then
                   wxType(k) = PRECIPS% OTHER
                else if(t(k) <= 273.15) then
                   wxType(k) = PRECIPS% OTHER
                else 
                   wxType(k) = PRECIPS% RAIN
                end if

             else
                wxType(k) = PRECIPS% NONE
             end if if_iceIdx
          end if if_pres_wbt

       end do lp200

       getPrecipType = PRECIPS% NONE
       if(xcp >= 0.045) then
          do k = nz, nz-1, -1
             if (wxType(k) > getPrecipType) then
                getPrecipType = wxType(k)
             end if
          end do
       end if
    end if if_conv

    deallocate(wxType)

    return
  end function getPrecipType

end module DerivedFields

!========================================================================
! = = = = = = = = = = = = = module  CloudLayers = = = = = = = = = = = = =
!========================================================================
module CloudLayers

  use DerivedFields, only : mixing_ratio

  private
  public calc_CloudLayers
  public clouds_t

  integer, parameter :: MaxLayers = 30 
  type :: clouds_t
     ! 2-D
     integer :: nLayers
     integer :: wmnIdx   ! warm nose index
     real    :: avv      ! average vertical velocity
     ! 3-D, on model levels of nz
     real, allocatable :: layerQ(:)
     ! 3-D, of cloud layers
     integer :: topIdx(MaxLayers)
     integer :: baseIdx(MaxLayers)
     real :: ctt(MaxLayers)
  end type clouds_t

contains

!-----------------------------------------------------------------------+
  subroutine calc_CloudLayers(rh,t,pres,ept,vv, nz, topoK, xlat, xlon,&
       region, clouds)
    IMPLICIT NONE
    integer, intent(in) :: nz, topoK
    real,    intent(in) :: rh(nz),t(nz),pres(nz), ept(nz), vv(nz)
    real,    intent(in) :: xlat,xlon
    integer,          intent(out) :: region
    type(clouds_t), intent(inout) :: clouds ! layerQ has been allocated

    real :: t_rh
    integer :: in_cld,cur_base, num_lyr

    integer :: k, kk, n, kstart

    ! get the global region and set the RH thresholds           
    if (abs(xlat).lt.23.5) then
       t_rh = 80.0
       region = 1
    elseif ( (abs(xlat).ge.23.5).and.(abs(xlat).lt.66)) then
       t_rh = 75.0
       region =2 
    else
       t_rh = 70.0
       region =2 
    endif

    ! loop from the top (start at 2 so we can have n+1) )
    ! bottom is nz and top is 1

    num_lyr  = 0
    in_cld   = 0
    cur_base = 1 

    ! identify the very top layer if rh starts at high value
    if((rh(1) .ge. t_rh) .and. (rh(2) .ge. t_rh))then
       num_lyr = num_lyr + 1 
       in_cld = 1
       ! find the cloud top and ctt
       clouds%topIdx(num_lyr) = 1
       clouds%ctt(num_lyr) = t(1)

       ! find the cloud base
       do kk=2,topoK-1
          if  ((rh(kk-1).ge.t_rh).and.(rh(kk).lt.t_rh)) then  
             if ((rh(kk+1).lt.t_rh).and.(in_cld.eq.1)) then 
                clouds%baseIdx(num_lyr) = kk
                cur_base = kk
                in_cld = 0
             endif
          endif
       end do
    end if
    kstart = cur_base + 1

    do k = kstart, topoK-1 
       if (cur_base.le.k) then
          if ((rh(k-1).lt.t_rh).and.(in_cld.eq.0)) then
             if ((rh(k).ge.t_rh).and.(rh(k+1).ge.t_rh)) then
                num_lyr = num_lyr + 1 
                in_cld = 1
                ! find the cloud top and ctt
                clouds%topIdx(num_lyr) = k 
                clouds%ctt(num_lyr) = t(k)

                ! find the cloud base
                do kk=clouds%topIdx(num_lyr),topoK-1
                   if  ((rh(kk-1).ge.t_rh).and.(rh(kk).lt.t_rh)) then  
                      if ((rh(kk+1).lt.t_rh).and.(in_cld.eq.1)) then 
                         clouds%baseIdx(num_lyr) = kk
                         cur_base = kk
                         in_cld = 0
                      endif
                   endif
                end do

             endif
          endif
       endif
    end do

    ! if the loop exits still in cloud make the cloud base the surface
    if (in_cld.eq.1) then 
       clouds%baseIdx(num_lyr) = topoK
    endif

    clouds%nLayers = num_lyr

    clouds%wmnIdx = getWarmnoseIdx(t, nz)

    ! only for the lowest cloud layer
    ! only used for severity scenario where precip is below warm nose
    if(num_lyr > 0) then
       clouds%avv = getAverageVertVel(t, vv, nz, &
            clouds%topIdx(num_lyr), clouds%baseIdx(num_lyr))
    else
       clouds%avv = 0
    end if

    ! for severity
    call calc_layerQ(t, rh, pres, ept, nz, clouds)

    return
  end subroutine calc_CloudLayers


!-----------------------------------------------------------------------+
! From top to the surface, find the warmnoseIndex if existing
!      |
!      |T<=FREEZING
!  ----|------------------warmnoseIndex
!  |             |
!  | T > FREEZING|
!  --------------
!      |T<=FREEZING
!  ____|________
!  /////////////      surface
!-----------------------------------------------------------------------+
  integer function getWarmnoseIdx(t, nz)
    IMPLICIT NONE
    integer, intent(in) :: nz
    real,    intent(in) :: t(nz)

    logical :: aboveFreezing
    integer :: tmpIndex

    integer :: k

    aboveFreezing = .false.
    tmpIndex = 0 !It doesn't matter of the initialized value

    getWarmnoseIdx = -1

    !  search from top of model down to the surface  
    do k = 1, nz

       if(t(k) <= 273.15) then
          if (aboveFreezing) then ! above 0 then below 0
             getWarmnoseIdx = tmpIndex
             ! once warmnoseIndex is set, we can't get back 
             ! here. let's break out of loop
             exit
          end if !aboveFreezing
          aboveFreezing = .false.
       else 
          if(.not. aboveFreezing) tmpIndex = k
          aboveFreezing = .true.
       end if

    end do

    return
  end function getWarmnoseIdx

!-----------------------------------------------------------------------+
! Calculate the average VV in the dendritic layer (-13 to -16) if it's in 
! the lowest cloud (clouds%nLayers). If the -13 to -16 layer falls
! between 2 levels then use the average VV from the levels on either side
!
  real function getAverageVertVel(t,vv,nz, topIdx_lowest,baseIdx_lowest)
    IMPLICIT NONE
    integer, intent(in) :: nz
    real,    intent(in) :: t(nz), vv(nz)
    integer,    intent(in) :: topIdx_lowest,baseIdx_lowest

    integer :: numVertVel, k, start_base

    real :: sumVertVel

    sumVertVel = 0.0
    numVertVel = 0

    ! The following loop starts at least nz-1 as the lowest model level
    if(baseIdx_lowest == nz) then
       start_base = nz - 1
    else
       start_base = baseIdx_lowest
    end if

    do k = start_base, topIdx_lowest, -1
       if ( t(k) <= 260.15 .and. t(k) >= 257.15) then
          sumVertVel = sumVertVel + vv(k)
          numVertVel = numVertVel + 1
       else if (t(k) < 257.15) then
          sumVertVel = sumVertVel + vv(k) + vv(k+1)
          numVertVel = 2
          exit
       end if
    end do
      
    if (numVertVel == 0) then
       getAverageVertVel = 0.0
    else 
       getAverageVertVel = sumVertVel / numVertVel
    end if
      
    return
  end function getAverageVertVel

!-----------------------------------------------------------------------+
  subroutine calc_layerQ(t, rh, pres, ept, nz, clouds)
    IMPLICIT NONE
    integer, intent(in) :: nz
    real,    intent(in) :: t(nz), rh(nz), pres(nz), ept(nz)
    type(clouds_t), intent(inout) :: clouds


    real :: mean_rh, te_map, rh_map, adj_Q
    real :: totalQ, testThetaE, Q_top, Q_base, Q_layer, delta_TE, sum_rh 
    integer :: base_k, test_k, num_layers

    integer :: k, m, n, kk

    clouds%layerQ(:) = 0.0

    ! loop through each layer
    lp_nlayers: do n = 1, clouds%nLayers
       lp_k_inlayer: do k = clouds%topIdx(n), clouds%baseIdx(n)-1

          totalQ = 0.0
          testThetaE = ept(k)

          ! get base layer k value (first more than 4K colder than at k)
          base_k = clouds%baseIdx(n)
          do m = k, nz-1
             if((testThetaE-ept(m)>4.) .and. (clouds%baseIdx(n)<=m))then 
                base_k = m - 1
                exit
             end if
          end do

          ! calculate delta thetaE and deltaQ and deltaRH for the layer
          lp_kk: do kk = k, base_k-1
             Q_top  = mixing_ratio(t(kk), pres(kk))
             Q_base = mixing_ratio(t(kk+1), pres(kk+1))
             Q_layer = Q_base - Q_top
          
             ! convert Q from g/kg to g/m**3
             Q_layer = Q_layer * (pres(kk)/(287.05 * t(kk)))

             if (Q_layer < 0.0) Q_layer = 0.0

             delta_TE = testThetaE - ept(kk+1)
             test_k = kk + 1

             ! get the mean RH in the layer
             num_layers = 0
             sum_rh = 0.0
             do m = k, test_k
                num_layers = num_layers + 1
                sum_rh = sum_rh + rh(m)
             end do
    
             if (num_layers == 0) then
                mean_rh = 0.0
             else
                mean_rh = sum_rh / num_layers
             end if
	
             te_map = dq_delta_TE_map(delta_TE)
             rh_map = dq_rh_map(mean_rh)
             adj_Q = Q_layer * te_map * rh_map
     
             totalQ = totalQ + adj_Q
          end do lp_kk

          clouds%layerQ(k) = totalQ
       end do lp_k_inlayer
    end do lp_nlayers

    return
  end subroutine calc_layerQ

!-----------------------------------------------------------------------+
  ! 70.0 0.0, 100.0 1.0
  real function dq_rh_map(rh)
    IMPLICIT NONE
    real, intent(in) :: rh

    if(rh <= 70.) then
       dq_rh_map = 0.
    else if( rh >= 100.) then
       dq_rh_map = 1.0
    else
       dq_rh_map = (rh-70.)/30.
    end if

    return
  end function dq_rh_map

!-----------------------------------------------------------------------+
  ! 0.0 1.0, 4.0 0.0
  real function dq_delta_TE_map(delTE)
    IMPLICIT NONE
    real, intent(in) :: delTE

    if (delTE <= 0.0) then
       dq_delta_TE_map = 1.0
    else if (delTE <= 4.0) then
       dq_delta_TE_map = (4. - delTE) / 4.0
    else
       dq_delta_TE_map = 0.0
     end if

    return
  end function dq_delta_TE_map

end module cloudlayers


!========================================================================
! = = = = = = = = = = = = module IcingPotential = = = = = = = = = = = = =
!========================================================================
module IcingPotential

  use DerivedFields, only : PRECIPS
  use CloudLayers, only : clouds_t

  private
  public icing_pot

contains

!-----------------------------------------------------------------------+
  subroutine icing_pot(hgt, rh, t, cwat, vv, nz, clouds, ice_pot)
    IMPLICIT NONE
    integer, intent(in) :: nz
    real, intent(in) :: hgt(nz), rh(nz), t(nz), cwat(nz), vv(nz)
    type(clouds_t), intent(in) :: clouds
    real, intent(out) :: ice_pot(nz)

    real :: ctt, slw, slw_fac, vv_fac
    integer :: k, n

    ice_pot(:) = 0.0

    ! run the icing potential algorithm
    lp_n: do n = 1, clouds%nLayers

       ! apply the cloud top temperature to the cloud layer
       ctt = clouds%ctt(n)

       lp_k: do k = clouds%topIdx(n), clouds%baseIdx(n)

          ! convert the cwater to slw if the CTT > -14C
          if ( ctt>=259.15 .and. ctt<=273.15 ) then
             slw = cwat(k) * 1000.0
          else
             slw = 0.0
          end if

          ice_pot(k) = tmap(t(k)) * rh_map(rh(k)) * ctt_map(ctt)

          ! add the VV map
          if (vv_map(vv(k)).ge.0.0) then
             vv_fac = (1.0-ice_pot(k)) * (0.25*vv_map(vv(k))) 
          else
             vv_fac = ice_pot(k) * (0.25*vv_map(vv(k)))  
          endif

          ! add the slw
          slw_fac = (1.0 - ice_pot(k))*(0.4*slw_map(slw))  

          ! calculate the final icing potential
          if (ice_pot(k).gt.0.001) then 
             ice_pot(k) = ice_pot(k) + vv_fac + slw_fac
          endif

          ! make sure the values don't exceed 1.0
          if (ice_pot(k).gt.1.0) then
             ice_pot(k) = 1.0
          endif

!        print *,ice_pot(k),t(k),tmap(t(k)),rh(k),rh_map(rh(k))
!     *          , ice_ctt(k),ctt_map(ice_ctt(k)),vv(k),vv_map(vv(k))
!     *          , cwat(k), slw(k), slw_map(slw(k))  

       end do lp_k
    end do lp_n

    return
  end subroutine icing_pot

!-----------------------------------------------------------------------+
  real function tmap(temp)
    IMPLICIT NONE
    real, intent(in) :: temp

    if((temp.ge.248.15).and.(temp.le.263.15)) then 
       tmap=((temp-248.15)/15.0)**1.5
    elseif((temp.gt.263.15).and.(temp.lt.268.15)) then 
       tmap=1.0
    elseif((temp.gt.268.15).and.(temp.lt.273.15)) then 
       tmap=((273.15 - temp)/5.0)**0.75
    else
       tmap=0.0
    endif

    return
  end function tmap
 

!-----------------------------------------------------------------------+
  real function rh_map(rh)
    IMPLICIT NONE
    real, intent(in) :: rh

    if (rh.gt.95.0) then
       rh_map=1.0
    elseif ( (rh.le.95.0).and.(rh.ge.50.0) ) then 
       rh_map=((20./9.) * ((rh/100.0)-0.5))**3.0
    else
       rh_map=0.0
    endif

    return
  end function rh_map


!-----------------------------------------------------------------------+
  real function ctt_map(ctt)
    IMPLICIT NONE
    real, intent(in) :: ctt

    if((ctt.ge.261.0).and.(ctt.lt.280.)) then 
       ctt_map = 1.0
    elseif((ctt.gt.223.0).and.(ctt.lt.261.0 )) then 
       ctt_map = 0.2 + 0.8*(((ctt - 223.0)/38.0)**2.0)
    elseif(ctt.lt.223.0) then 
       ctt_map = 0.2
    else
       ctt_map = 0.0
    endif

    return
  end function ctt_map


!-----------------------------------------------------------------------+

  real function vv_map(vv)
    IMPLICIT NONE
    real, intent(in) :: vv

    if (vv.gt.0.0) then 
       vv_map = 0.0
    elseif (vv.lt.-0.5) then 
       vv_map = 1.0
    else
       vv_map = -1.0 * (vv/0.5)
    endif

    return
  end function vv_map


!-----------------------------------------------------------------------+

  real function slw_map(slw)
    IMPLICIT NONE
    real, intent(in) :: slw

    if(slw.gt.0.2) then 
       slw_map = 1.0
    elseif (slw.le.0.001) then 
       slw_map = 0.0
    else
       slw_map = slw/0.2
    endif

    return
  end function slw_map

end module IcingPotential


!========================================================================
! = = = = = = = = = = = = module SeverityMaps = = = = = = = = = = = = =
!========================================================================
module SeverityMaps
  public

  type :: scenarios_t
     integer :: NO_PRECIPITAION = 0
     integer :: PRECIPITAION_BELOW_WARMNOSE = 1
     integer :: PRECIPITAION_ABOVE_WARMNOSE = 2
     integer :: ALL_SNOW = 3
     integer :: COLD_RAIN = 4
     integer :: WARM_PRECIPITAION = 5
     integer :: FREEZING_PRECIPITAION  = 6
     integer :: CONVECTION = 7
  end type scenarios_t
  type(scenarios_t), parameter :: SCENARIOS = scenarios_t()

contains

!-----------------------------------------------------------------------+
! scenario dependant
!-----------------------------------------------------------------------+

  real function twp_map(v, scenario)
    real, intent(in) :: v
    integer, intent(in) :: scenario
    twp_map = 0.0
    select case (scenario) 
    case(SCENARIOS%PRECIPITAION_ABOVE_WARMNOSE, SCENARIOS%ALL_SNOW, &
         SCENARIOS%COLD_RAIN, SCENARIOS%FREEZING_PRECIPITAION)
       ! 0.0 0.0, 1000.0 1.0
       if(v <= 0.0) then
          twp_map = 0.0
       else if( v < 1000.) then
          twp_map = v / 1000.0
       else
          twp_map = 1.
       end if           
    case( SCENARIOS%WARM_PRECIPITAION)
       ! 0.0 0.0, 500.0 1.0
       if(v <= 0.0) then
          twp_map = 0.0
       else if( v < 500.) then
          twp_map = v / 500.0
       else
          twp_map = 1.
       end if           
    end select
    return
  end function twp_map

  ! Only precip below warmnose has a different temperature map
  real function t_map(v, scenario)
    real, intent(in) :: v
    integer, intent(in) :: scenario
    t_map = 0.
    select case (scenario)
    case(SCENARIOS%PRECIPITAION_BELOW_WARMNOSE)
       ! 269.15 1.0, 273.15 0.0
       if(v <= 269.15) then
          t_map = 1.
       elseif( v <= 273.15) then
          t_map = (273.15 - v ) / 4.0
       else
          t_map = 0.
       end if
    case default
       ! 248.15 1.0,    248.65 0.8039, 249.15 0.7226, 251.15 0.5196,
       ! 253.15 0.3798, 255.15 0.2662, 257.15 0.1679, 259.15 0.0801,
       ! 261.15 0.0,    268.15 0.0,    273.15 1.0
       if(v <= 248.15) then
          t_map = 1.0
       elseif( v <= 248.65) then
          t_map = (49.162215 - 0.1961*v) * 2.
       elseif( v <= 249.15) then
          t_map = (20.617195 - 0.0813*v) * 2.
       elseif(v <= 251.15) then
          t_map = (52.02265 - 0.203*v) / 2.0
       elseif(v <= 253.15) then
          t_map = (36.14997 - 0.1398*v) / 2.0
       elseif(v <= 255.15) then
          t_map = (29.51744 - 0.1136*v) / 2.0
       elseif(v <= 257.15) then
          t_map = (25.613645-0.0983*v) / 2.0
       elseif(v <= 259.15) then
          t_map = (22.91357 - 0.0878*v) / 2.0
       elseif( v <= 261.15) then
          t_map = (20.918115 - 0.0801*v) / 2.
       else
          t_map = 0.0
       end if
    end select
  end function t_map


  ! Condensates near the surface take place of radar reflectivity in CIP
  real function prcpCondensate_map(v, scenario)
    real, intent(in) :: v
    integer, intent(in) :: scenario
    prcpCondensate_map = 0.0
    select case (scenario)
    case(SCENARIOS%NO_PRECIPITAION)
       ! do nothing
    case(SCENARIOS%PRECIPITAION_BELOW_WARMNOSE, SCENARIOS%COLD_RAIN, &
         SCENARIOS%PRECIPITAION_ABOVE_WARMNOSE)
       ! 0.05 0.0, 0.2 1.0
       if(v <= 0.05) then
          prcpCondensate_map = 0.
       elseif(v <= 0.2) then
          prcpCondensate_map = (v - 0.05) / 0.15
       else
          prcpCondensate_map = 1.
       end if
    case(SCENARIOS%ALL_SNOW)
       ! 0.05 0.0, 0.25 1.0
       if(v <= 0.05) then
          prcpCondensate_map = 0.
       elseif(v <= 0.25) then
          prcpCondensate_map = (v - 0.05) / 0.2
       else
          prcpCondensate_map = 1.0
       end if
    case(SCENARIOS%WARM_PRECIPITAION, SCENARIOS%FREEZING_PRECIPITAION)
       ! 0.05 0.0, 0.15 0.5
       if(v <= 0.05) then
          prcpCondensate_map = 0.
       elseif(v <= 0.15) then
          prcpCondensate_map = (.5*v - 0.025) / 0.1
       else
          prcpCondensate_map = 0.5
       end if
    end select
    return
  end function prcpCondensate_map


  real function deltaZ_map(v, scenario)
    real, intent(in) :: v
    integer, intent(in) :: scenario
    deltaZ_map  = 0.0
    select case (scenario) 
    case (SCENARIOS%NO_PRECIPITAION, SCENARIOS%WARM_PRECIPITAION)
       ! 0.0 0.0, 1828.8 1.0
       if(v <= 0.0) then
          deltaZ_map = 0.0
       else if(v <= 1828.8) then
          deltaZ_map = v /1828.8
       else
          deltaZ_map = 1.
       end if
    case(SCENARIOS%PRECIPITAION_BELOW_WARMNOSE)
       ! 0.0 0.0, 30.49999 0.0, 30.5 1.0
       if(v < 30.5) then
          deltaZ_map = 0.0
       else 
          deltaZ_map = 1.
       end if
    case(SCENARIOS%ALL_SNOW)
       ! 914.4 0.0, 2743.2 1.0
       if(v <= 914.4) then
          deltaZ_map = 0.0
       else if(v <= 2743.2) then
          deltaZ_map = (v - 914.4) / 1828.8
       else
          deltaZ_map = 1.
       end if
    case(SCENARIOS%COLD_RAIN, SCENARIOS%FREEZING_PRECIPITAION)
       ! 1524.0 0.0, 4267.2 1.0
       if(v <= 1524.) then
          deltaZ_map = 0.0
       else if(v <= 4267.2) then
          deltaZ_map = (v - 1524.) / 2743.2
       else
          deltaZ_map = 1.
       end if
    end select
    return
  end function deltaZ_map

!-----------------------------------------------------------------------+
! scenario independant.
!-----------------------------------------------------------------------+

  ! 223.15 0.8, 233.15 0.7446, 243.15 0.5784, 253.15 0.3014
  ! 261.15 0.0, 280.15 0.0, 280.151 1.0
  real function ctt_map(v)
    real, intent(in) :: v
    if(v <= 223.15) then
       ctt_map = 0.8
    elseif(v <= 233.15) then
       ctt_map = (20.36251 - 0.0554*v) / 10.
    elseif(v <= 243.15) then
       ctt_map = (46.19553 - 0.1662*v) / 10.
    elseif(v <= 253.15) then
       ctt_map = (73.13655 - 0.277*v) / 10.
    elseif(v <= 261.15) then
       ctt_map = (78.71061 - 0.3014*v) / 8.
    else
       ctt_map = 0.
    end if
  end function ctt_map

  ! -0.5 1.0, 0.0 0.0
  real function vv_map(v)
    real, intent(in) :: v
    if(v <= -0.5) then
       vv_map = 1.
    else if(v <= 0.) then
       vv_map = -2. * v
    else
       vv_map = 0.
    end if
  end function vv_map


  ! cloud top distance
  ! 609.6 1.0, 3048.0 0.0
  real function cldTopDist_map(v)
    if( v <= 609.6) then
       cldTopDist_map = 1.0
    elseif( v <= 3048.) then
       cldTopDist_map = (3048. - v) / 2438.4
    else
       cldTopDist_map = 0.
    end if
  end function cldTopDist_map


  ! cloud base distance
  ! 304.8 1.0, 1524.0 0.0
  real function cldBaseDist_map(v)
    if( v <= 304.8) then
       cldBaseDist_map = 1.0
    elseif( v <= 1524.) then
       cldBaseDist_map = (1524. - v) / 1219.2
    else
       cldBaseDist_map = 0.
    end if
  end function cldBaseDist_map

  ! 0.0 0.0, 1.0 1.0
  real function deltaQ_map(v)
    if( v <= 0.) then
       deltaQ_map = 0
    elseif( v <= 1.0) then
       deltaQ_map = v
    else
       deltaQ_map = 1.
    end if
  end function deltaQ_map


  ! Do not identify liquid/ice condensate for GFS
  real function moisture_map(rh, cwat, pres, t)
    IMPLICIT NONE
    real, intent(in) :: rh, cwat, pres, t
    real :: cwat_m3, rhFactor, cwatFactor
    ! Convert cwat from kg/kg to g/m^3
    cwat_m3 = 1000. * (cwat * pres) / (287.05 * t)
    rhFactor = 0.5 * rh_map(rh)
    cwatFactor = cwat_map(cwat_m3)
    moisture_map = rhFactor + (1.0 - rhFactor) * cwatFactor
    return
  end function moisture_map

  ! only called by moisture_map
  ! 70.0 0.0, 100.0 1.0
  real function rh_map(v)
    real, intent(in) :: v
    if(v <= 70.) then
       rh_map = 0.
    else if(v <= 100) then
       rh_map = (v - 70.) / 30.
    else
       rh_map = 1.
    end if
  end function rh_map

  ! only called by moisture_map
  ! 0.00399 0.0, 0.004 0.0, 0.2 1.0
  real function cwat_map(v)
    real, intent(in) :: v
    if(v <= 0.004) then
       cwat_map = 0.
    elseif( v <= 0.2) then
       cwat_map = (v - 0.004) / 0.196
    else
       cwat_map = 1.
    end if
  end function cwat_map


!-----------------------------------------------------------------------+
! convective
!-----------------------------------------------------------------------+

  ! 243.150 0.0, 265.15 1.0, 269.15 1.0, 270.15 0.87
  ! 271.15 0.71, 272.15 0.50, 273.15 0.0
  real function convect_t_map(v)
    real, intent(in) :: v
    if(v <= 243.15) then
       convect_t_map = 0.
    else if(v <= 265.15) then
       convect_t_map = (v -243.15)/22.
    else if(v <= 269.15) then
       convect_t_map = 1.
    else if(v <= 270.15) then
        convect_t_map = -0.13*v + 35.9895
     else if(v <= 271.15) then
        convect_t_map = -0.16*v + 44.094
     else if(v <= 272.15) then
        convect_t_map = -0.21*v + 57.6515
     else if(v <= 273.15) then
        convect_t_map = -0.50*v + 136.575
     else
        convect_t_map = 0.
     end if
   end function convect_t_map


   ! 1.0 0.0, 3.0 1.0
   real function convect_qpf_map(v)
     real, intent(in) :: v
     if(v <= 1.0) then
        convect_qpf_map = 0
     else if(v <= 3.0) then
        convect_qpf_map = (v - 1.) / 2.
     else
        convect_qpf_map = 1. 
     end if
   end function convect_qpf_map

   ! 1000.0 0.0, 2500.0 1.0
   real function convect_cape_map(v)
     real, intent(in) :: v

     if (v <= 1000.0) then
        convect_cape_map = 0.0
     else if(v <= 2500.) then
        convect_cape_map = (v - 1000.0) / 1400.
     else
        convect_cape_map = 1.0
     end if
   end function convect_cape_map


   ! -10.0 1.0, 0.0 0.0
   real function convect_liftedIdx_map(v)
     real, intent(in) :: v
     if(v <= -10.) then
        convect_liftedIdx_map = 1.0
     else if(v <= 0.) then
        convect_liftedIdx_map = -0.1 * v
     else
        convect_liftedIdx_map = 0.
     end if
   end function convect_liftedIdx_map


   ! 20.0 0.0, 40.0 1.0
   real function convect_kIdx_map(v)
     real, intent(in) :: v
     if(v <= 20.0) then
        convect_kIdx_map = 0
     else if(v <= 40.0) then
        convect_kIdx_map = (v - 20.) / 20.
     else
        convect_kIdx_map = 1.
     end if
   end function convect_kIdx_map

   ! 20.0 0.0, 55.0 1.0
   real function convect_totals_map(v)
     real, intent(in) :: v
     if(v <= 20.0) then
        convect_totals_map = 0
     else if( v <= 55.0)then
        convect_totals_map = (v - 20) / 35.
     else
        convect_totals_map = 1.0
     end if
   end function convect_totals_map

end module SeverityMaps

!========================================================================
! = = = = = = = = = = = = module IcingSeverity = = = = = = = = = = = = =
!========================================================================
module IcingSeverity
  use DerivedFields, only : PRECIPS
  use CloudLayers, only : clouds_t
  use SeverityMaps

  private
  public icing_sev

  real, parameter :: COLD_PRCP_T_THLD = 261.15

  ! module members: variables of cloud information
  ! original
  real :: ctt, layerQ, avv
  integer :: nLayers, topIdx, baseIdx, wmnIdx
  ! derived
  real :: deltaZ, cloudTopDist
  ! whether it's the lowest cloud layer
  logical :: lowestCloud

  ! module member: derived variable
  real :: moistInt

contains

!-----------------------------------------------------------------------+
  subroutine icing_sev(hgt, rh, t, pres, cwat, vv, twp, ice_pot, nz, &
       xacp, cape, lx, kx, tott, pc, prcpType, clouds, &
       iseverity)
    IMPLICIT NONE
    integer, intent(in) :: nz
    real, intent(in) :: hgt(nz), rh(nz), t(nz), pres(nz)
    real, intent(in) :: cwat(nz), vv(nz), twp(nz), ice_pot(nz)
    real, intent(in) :: xacp, cape, lx, kx, tott, pc
    integer, intent(in) :: prcpType
    type(clouds_t), intent(in) :: clouds
    real, intent(out) :: iseverity(nz) ! category severity

    real :: severity
    integer :: k, n

    iseverity(:) = 0.0

    lowestCloud = .false.

    ! run the icing severity algorithm
    lp_n: do n = 1, clouds%nLayers

       ! extract cloud information 
       wmnIdx = clouds%wmnIdx
       avv    = clouds%avv ! only for scenario below warmnose
       if (n == clouds%nLayers) lowestCloud = .true.
       ! apply the cloud top temperature to the cloud layer
       ctt = clouds%ctt(n)
       topIdx = clouds%topIdx(n)
       baseIdx = clouds%baseIdx(n)

       lp_k: do k = topIdx, baseIdx
          if (ice_pot(k) < 0.001) cycle

          severity = 0.0

          ! clouds information
          layerQ = clouds%layerQ(k)
          ! derived
          deltaZ = hgt(k) - hgt(baseIdx)
          cloudTopDist = hgt(topIdx) - hgt(k)

          ! derived variable
          moistInt = moisture_map(rh(k), cwat(k), pres(k), t(k))

          if(isConvection(prcpType)) then
             call convectionScenario &
                  (t(k), xacp, cape, lx, kx, tott, severity)
          elseif(isClassicPrcpBlwWmn(prcpType, k)) then
             call classicPrcpBlwWmnScenario &
                  (t(k), avv, pc, ice_pot(k), severity)
          elseif(isClassicPrcpAbvWmn(prcpType, k)) then
             call classicPrcpAbvWmnScenario &
                  (twp(k), vv(k), t(k), pc, ice_pot(k),severity)
          elseif(isNoPrecip(prcpType)) then
             call noPrcpScenario &
                  (vv(k), t(k), pc, ice_pot(k), severity)
          elseif(isSnow(prcpType)) then
             call snowScenario &
                  (twp(k), vv(k), t(k), pc, ice_pot(k), severity)
          elseif(isColdRain(prcpType)) then
             call coldRainScenario &
                  (twp(k), vv(k), t(k), pc, ice_pot(k), severity)
          elseif(isWarmPrecip(prcpType)) then
             call warmPrecipScenario &
                  (twp(k), vv(k), t(k), pc, ice_pot(k), severity)
          elseif(isFreezingPrecip(prcpType)) then
             call freezingPrecipScenario &
                  (twp(k), vv(k), t(k), pc, ice_pot(k), severity)
          end if

          ! severity category
          ! 0 = none (0, 0.08)
          ! 1 = trace [0.08, 0.21]
          ! 2 = light (0.21, 0.37]
          ! 3 = moderate (0.37, 0.67]
          ! 4 = heavy (0.67, 1]
          ! (0.0 0, 0.25 1, 0.425 2, 0.75 3, 1 4)
          ! (0.08 0, 0.21 1, 0.37 2, 0.67 3, 1 4) ! updated June 2015

          ! however the official categories are: 
          ! 0 none
          ! 1 light
          ! 2 moderate
          ! 3 severe (no value)
          ! 4 trace
          ! 5 heavy
          !http://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_table4-207.shtml

          ! move category defination out of GFIP3.f to MDL2P.f - July 2015

          !if (severity < 0.08) then
          !   iseverity(k) = 0.0
          !elseif (severity <= 0.21) then
          !   iseverity(k) = 4.
          !else if(severity <= 0.37) then
          !   iseverity(k) = 1.0
          !else if(severity <= 0.67) then
          !   iseverity(k) = 2.0
          !else
          !   iseverity(k) = 5.0
          !endif

          ! make sure the values don't exceed 1.0
          iseverity(k)=min(1., severity)
          iseverity(k)=max(0., severity)

       end do lp_k
    end do lp_n

    return
  end subroutine icing_sev

!-----------------------------------------------------------------------+
  logical function isConvection(prcpType)
    IMPLICIT NONE
    integer, intent(in) :: prcpType
 
    isConvection = prcpType == PRECIPS%CONVECTION

    return 
  end function isConvection

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - +
  subroutine convectionScenario(t, xacp, cape, lx, kx, tott, severity)
    IMPLICIT NONE
    real, intent(in) :: t, xacp, cape, lx, kx, tott
    real, intent(out) :: severity

    integer :: scenario

    real :: tInt, prcpInt, capeInt, lxInt, kxInt,tottInt

    real :: weights(6) = (/ 4.0, 3.0, 5.0, 5.0, 2.0, 2.0 /)
    real :: sumWeight
    sumWeight = sum(weights)

    tInt    = convect_t_map(t) ** 0.5
    !Quantitative Precipitation Forecast
    prcpInt = convect_qpf_map(xacp)
    capeInt = convect_cape_map(cape)
    lxInt   = convect_liftedIdx_map(lx)
    kxInt   = convect_kIdx_map(kx)
    tottInt = convect_totals_map(tott)

    scenario = SCENARIOS%CONVECTION

    severity = (weights(1) * tInt    + weights(2) * prcpInt  + &
                weights(3) * capeInt + weights(4) * lxInt + &
                weights(5) * kxInt   + weights(6) * tottInt) / &
               sumWeight
    return
  end subroutine convectionScenario
  

!-----------------------------------------------------------------------+
  logical function isClassicPrcpBlwWmn(prcpType, k)
    IMPLICIT NONE
    integer, intent(in) :: prcpType, k
    ! wmnIdx, topIdx, ctt: module members (cloud info)

    if ((prcpType /= PRECIPS%NONE .and. prcpType /= PRECIPS%SNOW) .and.&
        (wmnIdx > 0 .and. k <= wmnIdx .and. topIdx > wmnIdx) .and. &
        (ctt < COLD_PRCP_T_THLD)) then
       isClassicPrcpBlwWmn = .true.
    else 
       isClassicPrcpBlwWmn = .false.
    end if

    return 
  end function isClassicPrcpBlwWmn
   
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - +
  subroutine classicPrcpBlwWmnScenario(t, vv, pc, ice_pot, severity)
    IMPLICIT NONE
    real, intent(in) :: t, pc, vv, ice_pot
    real, intent(out) :: severity
    ! deltaZ, ctt: module members (cloud info)
    ! moistInt: module member

    real :: tInt, pcInt, dzInt
    real :: cttInt, vvInt 

    integer :: scenario

    real :: weights(7) = (/ 3.0, 4.0, 3.0, 3.0, 3.5, 2.5, 2.0 /)
    real :: sumWeight
    sumWeight = sum(weights)

    scenario = SCENARIOS%PRECIPITAION_BELOW_WARMNOSE
 
    tInt   = t_map(t, scenario)
    pcInt  = prcpCondensate_map(pc, scenario)
    dzInt  = deltaZ_map(deltaZ, scenario)
    cttInt = ctt_map(ctt)
    vvInt  = vv_map(vv)

    severity = (weights(1) * tInt     + weights(2) * pcInt + &
                weights(3) * dzInt    + weights(4) * cttInt + &
                weights(5) * moistInt + weights(6) * vvInt + &
                weights(7) * ice_pot) / sumWeight

    return
  end subroutine classicPrcpBlwWmnScenario


!-----------------------------------------------------------------------+
! classical precipitation but not snow  
  logical function isClassicPrcpAbvWmn(prcpType, k)
    IMPLICIT NONE
    integer, intent(in) :: prcpType, k
    ! wmnIdx,topIdx, ctt: module members (cloud info)

    if((prcpType /= PRECIPS%NONE .and. prcpType /= PRECIPS%SNOW) .and.& 
        (wmnIdx > 0 .and. k > wmnIdx .and. topIdx > wmnIdx) .and. &
        (ctt < COLD_PRCP_T_THLD)) then
       isClassicPrcpAbvWmn = .true.
    else 
       isClassicPrcpAbvWmn = .false.
    end if

    return
  end function isClassicPrcpAbvWmn


! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - +
  subroutine classicPrcpAbvWmnScenario(twp, vv, t, pc, ice_pot, severity)
    IMPLICIT NONE
    real, intent(in) :: twp, vv, t, pc, ice_pot
    real, intent(out) :: severity
    ! moistInt: module member

    real :: twpInt, vvInt
    real :: tempAdj, cttAdj, pcAdj

    integer :: scenario

    real :: weights(4) = (/ 3.0, 3.5, 4.0, 2.0 /)
    real :: sumWeight
    sumWeight = sum(weights)

    scenario = SCENARIOS%PRECIPITAION_ABOVE_WARMNOSE
  
    twpInt = twp_map(twp, scenario)
    vvInt  = vv_map(vv)
 
    tempAdj = 0.0
    cttAdj  = 0.0
    pcAdj   = 0.0
    ! ctt, cloudTopDist, lowestCloud, deltaZ: module member (cloud info)
    call cal_DampingFactors(scenario, t, pc, tempAdj, cttAdj, pcAdj)

    severity = (weights(1) * twpInt   + weights(2) * vvInt + &
                weights(3) * moistInt + weights(4) * ice_pot) / &
               (sumWeight + 6.5*tempAdj + 3.0*cttAdj + 3.0*pcAdj)

    return
  end subroutine classicPrcpAbvWmnScenario



!-----------------------------------------------------------------------+
  logical function isNoPrecip(prcpType)
    IMPLICIT NONE
    integer, intent(in) :: prcpType

    isNoPrecip = prcpType == PRECIPS%NONE

    return
  end function isNoPrecip


! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - +

  subroutine noPrcpScenario(vv, t, pc, ice_pot, severity)
    IMPLICIT NONE
    real, intent(in) :: vv, t, pc, ice_pot
    real, intent(out) :: severity
    ! layerQ, deltaZ: module members (cloud info)
    ! moistInt: module member

    real :: dqInt, dzInt, vvInt
    real :: tempAdj, cttAdj, pcAdj

    integer :: scenario

    real :: weights(5) = (/ 3.0, 3.5, 4.0, 4.0, 3.0 /)
    real :: sumWeight
    sumWeight = sum(weights)

    scenario = SCENARIOS%NO_PRECIPITAION

    dqInt = deltaQ_map(layerQ)
    dzInt = deltaZ_map(deltaZ, scenario)
    vvInt = vv_map(vv)

    tempAdj = 0.0
    cttAdj  = 0.0
    pcAdj   = 0.0
    ! ctt, cloudTopDist, lowestCloud, deltaZ: module member (cloud info)
    call cal_DampingFactors(scenario, t, pc, tempAdj, cttAdj, pcAdj)
  
    severity = (weights(1) * dqInt + weights(2) * dzInt + &
                weights(3) * vvInt + weights(4) * moistInt + &
                weights(5) * ice_pot) / &
               (sumWeight + 9.0*tempAdj + 4.5*cttAdj)

    return
  end subroutine noPrcpScenario


!-----------------------------------------------------------------------+
  
  logical function isSnow(prcpType)
    IMPLICIT NONE
    integer, intent(in) :: prcpType

    isSnow = prcpType == PRECIPS%SNOW

    return
  end function isSnow

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - +

  subroutine snowScenario(twp, vv, t, pc, ice_pot, severity)
    IMPLICIT NONE
    real, intent(in) :: twp, vv, t, pc, ice_pot
    real, intent(out) :: severity
    ! deltaZ: module member (cloud info)
    ! moistInt: module member

    real :: twpInt, dzInt, vvInt
    real :: tempAdj, cttAdj, pcAdj

    integer :: scenario

    real :: weights(5) = (/ 3.0, 3.5, 3.5, 4.0, 3.0 /)
    real :: sumWeight
    sumWeight = sum(weights)

    scenario = SCENARIOS%ALL_SNOW

    twpInt = twp_map(twp, scenario)
    dzInt  = deltaZ_map(deltaZ, scenario)
    vvInt  = vv_map(vv)

    tempAdj = 0.0
    cttAdj = 0.0
    pcAdj = 0.0
    ! ctt, cloudTopDist, lowestCloud, deltaZ: module member (cloud info)
    call cal_DampingFactors(scenario, t, pc, tempAdj, cttAdj, pcAdj)

    severity = (weights(1) * twpInt + weights(2) * dzInt + &
                weights(3) * vvInt +  weights(4) * moistInt + &
                weights(5) * ice_pot) / &
               (sumWeight + 9.0*tempAdj + 4.0*cttAdj + 4.0*pcAdj)

    return
  end subroutine snowScenario


!-----------------------------------------------------------------------+
  logical function isColdRain(prcpType)
     IMPLICIT NONE
     integer, intent(in) :: prcpType
     ! ctt: module members (cloud info)

     isColdRain = prcpType == PRECIPS%RAIN .and. ctt < COLD_PRCP_T_THLD

    return
  end function isColdRain

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - +

  subroutine coldRainScenario(twp, vv, t, pc, ice_pot, severity)
    IMPLICIT NONE
    real, intent(in) :: twp, vv, t, pc, ice_pot
    real, intent(out) :: severity
    ! deltaZ: module member (cloud info)
    ! moistInt: module member

    real :: twpInt, dzInt, vvInt
    real :: tempAdj, cttAdj, pcAdj

    integer :: scenario

    real :: weights(5) = (/ 3.0, 3.5, 3.5, 4.0, 2.0 /)
    real :: sumWeight
    sumWeight = sum(weights)

    scenario = SCENARIOS%COLD_RAIN
  
    twpInt = twp_map(twp, scenario)
    dzInt  = deltaZ_map(deltaZ, scenario)
    vvInt  = vv_map(vv)

    tempAdj = 0.0
    cttAdj  = 0.0
    pcAdj   = 0.0
    ! ctt, cloudTopDist, lowestCloud, deltaZ: module member (cloud info)
    call cal_DampingFactors(scenario, t, pc, tempAdj, cttAdj, pcAdj)

    severity = (weights(1) * twpInt + weights(2) * dzInt + &
                weights(3) * vvInt +  weights(4) * moistInt + &
                weights(5) * ice_pot) / &
               (sumWeight + 8.0*tempAdj + 4.0*cttAdj + 4.0*pcAdj)

    return
  end subroutine coldRainScenario


!-----------------------------------------------------------------------+
!  The elseif clause is a result of sloppy thinking on the
!  part of the scientitsts. They were trying to create/use
!  two different tests for the same 'scenario.' There is
!  implicit definition of a 'classic precipitation' scenario.
  logical function isWarmPrecip(prcpType)
    IMPLICIT NONE
    integer, intent(in) :: prcpType
    ! ctt, wmnIdx, topIdx: module members (cloud info)

    if((prcpType == PRECIPS%RAIN .or. prcpType == PRECIPS%OTHER) .and. &
       (ctt >= COLD_PRCP_T_THLD)) then
       isWarmPrecip = .true.
    elseif((prcpType /=  PRECIPS%NONE .and. prcpType /= PRECIPS%SNOW).and.&
           (wmnIdx > 0 .and. topIdx <= wmnIdx) .and. &
           (ctt >= COLD_PRCP_T_THLD)) then
       isWarmPrecip = .true.
    else
       isWarmPrecip = .false.
    end if

    return
  end function isWarmPrecip


! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - +
  subroutine warmPrecipScenario(twp, vv, t, pc, ice_pot, severity)
    IMPLICIT NONE
    real, intent(in) :: twp, vv, t, pc, ice_pot
    real, intent(out) :: severity
    ! deltaZ, layerQ: module member (cloud info)
    ! moistInt: module member

    real :: twpInt, dzInt, vvInt, dqInt
    real :: tempAdj, cttAdj, pcAdj

    integer :: scenario

    real :: weights(6) = (/ 3.0, 3.0, 3.0, 3.5, 4.0, 2.0 /)
    real :: sumWeight
    sumWeight = sum(weights)

    scenario = SCENARIOS%WARM_PRECIPITAION
  
    twpInt = twp_map(twp, scenario)
    dzInt = deltaZ_map(deltaZ, scenario)
    vvInt = vv_map(vv)
    dqInt = deltaQ_map(layerQ)

    tempAdj = 0.0
    cttAdj = 0.0
    pcAdj = 0.0
    ! ctt, cloudTopDist, lowestCloud, deltaZ: module member (cloud info)
    call cal_DampingFactors(scenario, t, pc, tempAdj, cttAdj, pcAdj)

    severity = (weights(1) * twpInt +  weights(2) * dzInt + &
                weights(3) * vvInt  +  weights(4) * moistInt + &
                weights(5) * ice_pot + weights(6) * dqInt) / &
               (sumWeight + 9.0*tempAdj + 4.5*cttAdj + 4.5*pcAdj)

    return
  end subroutine warmPrecipScenario


!-----------------------------------------------------------------------+
  logical function isFreezingPrecip(prcpType)
    IMPLICIT NONE
    integer, intent(in) :: prcpType
    ! ctt: module member (cloud info)

    if (prcpType == PRECIPS%OTHER .and. ctt < COLD_PRCP_T_THLD) then
       isFreezingPrecip = .true.
    else 
       isFreezingPrecip = .false.
    end if

    return
  end function isFreezingPrecip


! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - +

  subroutine freezingPrecipScenario(twp, vv, t, pc, ice_pot, severity)
    IMPLICIT NONE
    real, intent(in) :: twp, vv, t, pc, ice_pot
    real, intent(out) :: severity
    ! deltaZ: module member (cloud info)
    ! moistInt: module member

    real :: twpInt, dzInt, vvInt
    real :: tempAdj, cttAdj, pcAdj

    integer :: scenario

    real :: weights(5) = (/ 3.0, 3.5, 3.5, 4.0, 2.0 /)
    real :: sumWeight
    sumWeight = sum(weights)

    scenario = SCENARIOS%FREEZING_PRECIPITAION

    twpInt = twp_map(twp, scenario)
    dzInt = deltaZ_map(deltaZ, scenario)
    vvInt = vv_map(vv)
  
    tempAdj = 0.0
    cttAdj  = 0.0
    pcAdj   = 0.0
    ! ctt, cloudTopDist, lowestCloud, deltaZ: module member (cloud info)
    call cal_DampingFactors(scenario, t, pc, tempAdj, cttAdj, pcAdj)

    severity = (weights(1) * twpInt +  weights(2) * dzInt + &
                weights(3) * vvInt +   weights(4) * moistInt + &
                weights(5) * ice_pot) / &
               (sumWeight + 8.0*tempAdj + 4.0*cttAdj + 4.0*pcAdj)

    return
  end subroutine freezingPrecipScenario


!-----------------------------------------------------------------------+
!  The only scenario NOT applicable to: below the warm nose
!
!  Temperature damping - can damp by a max of temp_adjust_factor
!
!  CTT damping - can damp by a max of ctt_adjust_factor
!  The CTT is multiplied by a map based on the height below cloud top
!  For all clouds and it's interest increases the closer to cloud 
!  top we are.
!
!  Condensate damping - can damp by a max of cond_adjust_factor
!  Only in the lowest cloud layer and interest increases the closer to
!  cloud base we are. Interest will be the max below cloud base as well. 
!  Maps differ for different scenarios
!
  subroutine cal_DampingFactors(scenario, t, pc, tempAdj,cttAdj,condAdj)
    IMPLICIT NONE
    integer, intent(in) :: scenario
    real, intent(in) :: t, pc
    real, intent(out) :: tempAdj, cttAdj, condAdj
    ! ctt, cloudTopDist, lowestCloud, deltaZ: module member (cloud info)

    tempAdj = t_map(t, scenario)

    cttAdj = ctt_map(ctt) *  cldTopDist_map(cloudTopDist)
  
    if (lowestCloud) then
       condAdj = cldBaseDist_map(deltaZ)
       ! For no precipitation, condAdj is 0.0
       condAdj = condAdj * prcpCondensate_map(pc, scenario)
    end if

    return
  end subroutine cal_DampingFactors


end module IcingSeverity

!========================================================================
! = = = = = = = = = = = = = Icing Algorithm = = = = = = = = = = = = =
!========================================================================
subroutine icing_algo(i,j,pres,temp,rh,hgt,cwat,vv,nz,xlat,xlon, &
                      xalt,xcprate,xacprate,cape,cin,fhour, ice_pot, ice_sev)
  use DerivedFields,  only : derive_fields
  use CloudLayers,    only : calc_CloudLayers, clouds_t
  use IcingPotential, only : icing_pot
  use IcingSeverity,  only : icing_sev

  IMPLICIT NONE

  integer, external :: getTopoK
!---------------------------------------------------------------------
! The GFIP algorithm computes the probability of icing within a model
!    column given the follow input data
!
! var     : precipitation accumulation hour (derive from fhour)
!
! 2-D data: convective precip rate (xacprate),
!           total precip rate (xcprate)
!           the topography height (xalt)
!           the latitude and longitude (xlat and xlon)
!           the number of vertical levels (nz) = 47 in my GFS file
!           ===== for severity only ======
!           Convective Avail. Pot. Energy, pds7=65280 255-0 mb above 
!                gnd (cape)
!           Convective inhibition, pds7=65280 255-0 mb above gnd (cin)
! 3-D data
!            pressure(nz) in PA
!            temperature(nz) in K
!            rh(nz) in %
!            hgt(nz) in GPM)
!            cwat(nz) in kg/x kg
!            vv(nz) in Pa/sec
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------+
! This subroutine calculates the icing potential for a GFS column
! First the topography is mapped to the model's vertical coordinate
!      in (topoK) 
! Then derive related fields and cloud layers,
!      up to 12 layers are possible 
! The icing are computed in (icing_pot, ice_sev):
!      The icing  potential output should range from 0 - 1. 
!      The icing severity is in 4 categories: 1 2 3 4.
!
!-----------------------------------------------------------------------+
  integer, intent(in) ::  i,j, nz
  real, intent(in) :: pres(nz),temp(nz),rh(nz),hgt(nz),cwat(nz),vv(nz)
  real, intent(in) :: xlat, xlon, xalt ! locations
  real, intent(in) :: xcprate, xacprate        ! precipitation rates
  real, intent(in) :: cape, cin
  real, intent(in) :: fhour
  real, intent(out) :: ice_pot(nz), ice_sev(nz)

  real :: xcp, xacp
  integer  :: topoK, region, prcpType
  real, allocatable  :: ept(:), wbt(:), twp(:)
  real :: pc, kx, lx, tott
  type(clouds_t) :: clouds

  integer :: k
  real, parameter :: DTQ2 = 160.

  allocate(ept(nz))
  allocate(wbt(nz))
  allocate(twp(nz))

  allocate(clouds%layerQ(nz))

! if(mod(i,300)==0 .and. mod(j,200)==0)then
!    print*,'sample input to FIP ',i,j,nz,xlat,xlon,xalt,xcprate, xacprate
!    do k=1,nz
!       print*,'k,P,T,RH,H,CWM,VV',k,pres(k),temp(k),rh(k),hgt(k),cwat(k),vv(k)
!    end do
! end if

  topoK = getTopoK(hgt,xalt,nz)

  ! hourly accumulated precipitation
  xcp  = xcprate  * 1000. / DTQ2 * 3600.
  xacp = xacprate * 1000. / DTQ2 * 3600.

  call derive_fields(temp, rh, pres, hgt, cwat, nz, &
       topoK, xacp, xcp, cin, cape,&
       ept, wbt, twp, pc, kx, lx, tott, prcpType)

  call calc_CloudLayers(rh, temp, pres, ept, vv, nz,topoK, xlat,xlon,&
       region, clouds)

  call icing_pot(hgt, rh, temp, cwat, vv, nz, clouds, ice_pot)

  call icing_sev(hgt, rh, temp, pres, cwat, vv, twp, ice_pot, nz, &
       xacp, cape, lx, kx, tott, pc, prcpType, clouds, &
       ice_sev)
! if(mod(i,300)==0 .and. mod(j,200)==0)then
!    print*,'FIP: cin,cape,pc, kx, lx, tott,pcpTyp',cin,cape,pc, kx, lx, tott,prcpType
!    do k=1,nz
!       print*,'ept,wbt,twp',ept(k), wbt(k), twp(k), ice_pot(k), ice_sev(k)
!       print*,'clouds nLayers wmnIdx avv',clouds%nLayers,clouds%wmnIdx,clouds%avv
!    end do
! end if

  deallocate(ept)
  deallocate(wbt)
  deallocate(twp)
  deallocate(clouds%layerQ)

  return
end subroutine icing_algo

!-----------------------------------------------------------------------+
integer function getTopoK(hgt, alt, nz)
  IMPLICIT NONE
  real, intent(in) :: hgt(nz)
  real, intent(in) :: alt
  integer,  intent(in) :: nz

  integer :: k

  if(hgt(nz) >= alt) then
     getTopoK = nz
  else
     do k=nz,2,-1
        if ((hgt(k-1) > alt) .and. (hgt(k) <= alt)) then
           getTopoK = k-1
           exit
        endif
     end do
  end if

  return
end function getTopoK

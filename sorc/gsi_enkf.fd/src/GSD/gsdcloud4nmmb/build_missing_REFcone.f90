SUBROUTINE build_missing_REFcone(mype,nlon,nlat,nsig,krad_bot_in,ref_mos_3d,h_bk,pblh)
!
!  radar observation
!
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  build_missing_REFcone  build missing reflectivity area
!          below cone down to assumed cloud base 
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2008-11-26
!
! ABSTRACT: 
!  This subroutine sets reflectivity values at missing reflectivity volumes
!    below the radar "data cone" down to an assumed cloud base
!  As of March 2010, this code code not yet use the local PBL base
!    as used in the RUC cloud/hydrometeor analysis since summer 2009.
!
! PROGRAM HISTORY LOG:
!    2009-01-20  Hu  Add NCO document block
!    2011-04-08  Hu  Clean the reflectivity below PBL height or level 7
!
!
!   input argument list:
!     mype        - processor ID
!     nlon        - no. of lons on subdomain (buffer points on ends)
!     nlat        - no. of lats on subdomain (buffer points on ends)
!     nsig        - no. of levels
!     krad_bot    - radar bottom level
!     ref_mos_3d  - 3D radar reflectivity
!     h_bk        - 3D background height
!     pblh        - PBL height in grid
!
!   output argument list:
!     ref_mos_3d  - 3D radar reflectivity
!
! USAGE:
!   INPUT FILES: 
!
!   OUTPUT FILES:
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 
!   MACHINE:  Linux cluster (WJET)
!
!$$$
!
!_____________________________________________________________________
!

  use kinds, only: r_kind,i_kind,r_single
  implicit none

  INTEGER(i_kind), intent(in)   :: mype
  INTEGER(i_kind), intent(in)   :: nlon,nlat,nsig
  real(r_single),  intent(in)   :: h_bk(nlon,nlat,nsig)           ! 3D height
  real(r_kind),    intent(inout):: ref_mos_3d(nlon,nlat,nsig)     ! reflectivity in grid
  real(r_single),  intent(in)   :: pblh(nlon,nlat)                ! PBL height
  real(r_single),  intent(in)   :: krad_bot_in                                   
!
  integer(i_kind) :: krad_bot,ifmissing
!
  integer(i_kind) :: maxlvl
  parameter (maxlvl=31)
  real(r_kind) :: newlvlAll(maxlvl)   ! vertical levels of reflectivity statistic profile(km)
  DATA newlvlAll/0.2, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 2.25, 2.5, 2.75, &
                3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, &
                9, 10, 11, 12, 13, 14, 15, 16/

  real(r_kind) :: refprofile_winter(maxlvl,6)   ! statistic reflectivity profile used to
                                                ! retrieve vertical ref based on lightning
!  max reflectivity 20-35 dbz
  DATA refprofile_winter(:,1) /      &
         0.999,0.938,0.957,0.975,0.983,0.990,0.995,0.999,1.000,1.000, &
         0.994,0.985,0.957,0.926,0.892,0.854,0.819,0.791,0.770,0.747, &
         0.729,0.711,0.705,0.685,0.646,0.631,0.649,0.711,0.828,0.931, &
         0.949/
!  max reflectivity 25-30 dbz
  DATA refprofile_winter(:,2) /      &
         0.965,0.937,0.954,0.970,0.984,0.991,0.996,1.000,0.997,0.988, &
         0.973,0.954,0.908,0.856,0.808,0.761,0.718,0.684,0.659,0.631, &
         0.607,0.586,0.570,0.550,0.523,0.512,0.531,0.601,0.711,0.813, &
         0.870/
!  max reflectivity 30-35 dbz
  DATA refprofile_winter(:,3) /      &
         0.966,0.958,0.977,0.989,0.998,1.000,0.997,0.992,0.981,0.962, &
         0.933,0.898,0.826,0.752,0.687,0.626,0.578,0.547,0.522,0.526, &
         0.519,0.501,0.482,0.464,0.437,0.430,0.454,0.539,0.662,0.742, &
         0.793/
!  max reflectivity 35-40 dbz
  DATA refprofile_winter(:,4) /      &
         0.947,0.953,0.980,0.994,1.000,0.996,0.987,0.974,0.956,0.928, &
         0.891,0.848,0.761,0.679,0.613,0.559,0.522,0.491,0.473,0.462, &
         0.451,0.433,0.415,0.403,0.382,0.380,0.406,0.482,0.603,0.707, &
         0.723/
!  max reflectivity 40-45 dbz
  DATA refprofile_winter(:,5) /      &
         0.937,0.955,0.986,1.000,0.997,0.995,0.988,0.978,0.957,0.920, &
         0.871,0.824,0.735,0.654,0.584,0.518,0.465,0.442,0.435,0.412, &
         0.398,0.385,0.376,0.360,0.340,0.350,0.377,0.446,0.551,0.625, &
         0.656/
!  max reflectivity 45-50 dbz
  DATA refprofile_winter(:,6) /      &
         0.900,0.949,0.982,0.995,1.000,0.998,0.983,0.954,0.914,0.874, &
         0.834,0.793,0.721,0.664,0.612,0.565,0.530,0.496,0.460,0.431, &
         0.402,0.383,0.370,0.354,0.335,0.321,0.347,0.342,0.441,0.510, &
         0.548/


  real(r_kind) :: refprofile_summer(maxlvl,6)   ! statistic reflectivity profile used to        
                                                ! retrieve vertical ref based on lightning
!  max reflectivity 20-25 dbz
  DATA refprofile_summer(:,1) /      &
         0.883,0.870,0.879,0.892,0.904,0.912,0.913,0.915,0.924,0.936, &
         0.946,0.959,0.984,0.999,1.000,0.995,0.988,0.978,0.962,0.940, &
         0.916,0.893,0.865,0.839,0.778,0.708,0.666,0.686,0.712,0.771, &
         0.833/
!  max reflectivity 25-30 dbz
  DATA refprofile_summer(:,2) /      &
         0.836,0.874,0.898,0.915,0.927,0.938,0.945,0.951,0.960,0.970, &
         0.980,0.989,1.000,0.995,0.968,0.933,0.901,0.861,0.822,0.783, &
         0.745,0.717,0.683,0.661,0.614,0.564,0.538,0.543,0.578,0.633, &
         0.687/
!  max reflectivity 30-35 dbz
  DATA refprofile_summer(:,3) /      &
         0.870,0.885,0.914,0.931,0.943,0.954,0.967,0.975,0.982,0.989, &
         0.995,1.000,0.998,0.973,0.918,0.850,0.791,0.735,0.690,0.657, &
         0.625,0.596,0.569,0.544,0.510,0.479,0.461,0.460,0.477,0.522, &
         0.570/
!  max reflectivity 35-40 dbz
  DATA refprofile_summer(:,4) /      & 
         0.871,0.895,0.924,0.948,0.961,0.971,0.978,0.983,0.988,0.992, &
         0.997,1.000,0.995,0.966,0.913,0.848,0.781,0.719,0.660,0.611, &
         0.576,0.542,0.523,0.513,0.481,0.448,0.416,0.402,0.417,0.448, &
         0.491/
!  max reflectivity 40-45 dbz
  DATA refprofile_summer(:,5) /      &
         0.875,0.895,0.914,0.936,0.942,0.951,0.964,0.979,0.990,0.998, &
         1.000,0.992,0.961,0.905,0.834,0.772,0.722,0.666,0.618,0.579, &
         0.545,0.518,0.509,0.483,0.419,0.398,0.392,0.403,0.423,0.480, &
         0.440/ 
!  max reflectivity 45-50 dbz
  DATA refprofile_summer(:,6) /      &
         0.926,0.920,0.948,0.975,0.988,0.989,0.995,0.997,1.000,1.000, &
         0.997,0.991,0.970,0.939,0.887,0.833,0.788,0.741,0.694,0.655, &
         0.611,0.571,0.551,0.537,0.507,0.470,0.432,0.410,0.420,0.405, &
         0.410/
         
  INTEGER(i_kind) :: season   ! 1= summer, 2=winter

  REAL(r_kind)    :: heightGSI,upref,downref,wght
  INTEGER(i_kind) :: ilvl,numref
  REAL(r_kind)    :: lowest,highest,tempref(nsig), tempprofile(maxlvl)
  REAL(r_kind)    :: maxref

  INTEGER(i_kind) :: i,j, k2, k, mref

!
!  vertical reflectivity distribution
!
  season=1
  DO k=1,maxlvl
     newlvlAll(k)=newlvlAll(k)*1000.0_r_kind
  ENDDO
!
  DO j=2,nlat-1
    DO i=2,nlon-1
      ifmissing=0
      maxref=-9999.0_r_kind
!mhu      krad_bot= int( max(krad_bot_in,pblh(i,j)) + 0.5_r_single )  ! consider PBL height
! Here, we only use PBL height to build missing corn and clean the reflectivity lower than 
! PBL height. The krad_bot_in will be used when calculate the radar tten but not the hydrometer retrieval.
!  Nov 21, 2011. Ming Hu
      krad_bot= int( pblh(i,j) + 0.5_r_single )  ! consider PBL height
!
!  in our case, -99 is no echo
!
      DO k2=int(nsig/2),krad_bot,-1
        if(ref_mos_3d(i,j,k2+1)>=20._r_kind .and. & 
           ref_mos_3d(i,j,k2)  < -100._r_kind ) ifmissing=k2
        if(ref_mos_3d(i,j,k2)>=maxref) maxref=ref_mos_3d(i,j,k2)
      ENDDO
      IF(ifmissing > 1 ) then
         DO k2=krad_bot,1,-1
           if(ref_mos_3d(i,j,k2) >maxref) maxref=ref_mos_3d(i,j,k2)
         ENDDO
!        if(maxref < 19.0_r_kind) then
!          write(6,*) 'build_missing_REFcone:',ifmissing,i,j,ifmissing
!          write(6,*) (ref_mos_3d(i,j,k2),k2=1,nsig)
!        endif
      endif
      IF(ifmissing > 1 .and. maxref > 19.0_r_kind ) then
         mref =  min(6,(int((maxref - 20.0_r_kind)/5.0_r_kind) + 1 ))
         if(season== 2 ) then
           DO k=1,maxlvl
              tempprofile(k)=refprofile_winter(k,mref)*maxref
           enddo
           lowest=newlvlAll(2)
           highest=7000.0_r_kind
         else if(season== 1 ) then
           DO k=1,maxlvl
              tempprofile(k)=refprofile_summer(k,mref)*maxref
           enddo
           lowest=newlvlAll(3)
           highest=12000.0_r_kind
         endif
! make a ref profile 
         tempref=-9999.9_r_kind
         DO k2=1,nsig
           heightGSI=h_bk(i,j,k2)
           if(heightGSI >= lowest .and. heightGSI < highest) then  ! lower 12km ?
              do k=1,maxlvl-1
                if( heightGSI >=newlvlAll(k) .and. & 
                    heightGSI < newlvlAll(k+1) ) ilvl=k
              enddo
              upref=tempprofile(ilvl+1)
              downref=tempprofile(ilvl)
              wght=(heightGSI-newlvlAll(ilvl))/(newlvlAll(ilvl+1)-newlvlAll(ilvl))
              tempref(k2)=(1-wght)*downref + wght*upref
           endif
         ENDDO
! build missing volumes down to krad_bot level
!   NOTE:  no use of PBL base yet, as done in RUC analysis since summer 2009
         maxref=ref_mos_3d(i,j,ifmissing+1)-tempref(ifmissing+1)
         if(abs(maxref) < 10.0_r_kind ) then
           DO k2=ifmissing,krad_bot,-1
             ref_mos_3d(i,j,k2) = tempref(k2) + maxref
           ENDDO
         else
           DO k2=ifmissing,krad_bot,-1
              ref_mos_3d(i,j,k2) = ref_mos_3d(i,j,ifmissing+1)
           ENDDO
         endif
!
      ENDIF
! clean echo less than PBL height and level 7
      DO k2=1,krad_bot
         ref_mos_3d(i,j,k2) = -99999.0_r_kind
      ENDDO
    ENDDO
  ENDDO

END SUBROUTINE build_missing_REFcone

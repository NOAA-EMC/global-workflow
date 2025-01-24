SUBROUTINE convert_lghtn2ref(mype,nlon,nlat,nsig,ref_mos_3d,lightning,h_bk)
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  convert_lghtn2ref  convert lightning stroke rate to radar reflectivity
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2008-11-17
!
! ABSTRACT: 
!  This subroutine converts lightning stroke rate to radar reflectivity
!
! PROGRAM HISTORY LOG:
!    2009-01-20  Hu  Add NCO document block
!
!
!   input argument list:
!     mype        - processor ID
!     nlon        - no. of lons on subdomain (buffer points on ends)
!     nlat        - no. of lats on subdomain (buffer points on ends)
!     nsig        - no. of levels
!     ref_mos_3d  - 3D reflectivity in analysis grid  
!     lightning   - 2D lightning flash rate in analysis grid
!     h_bk        - 3D height
!
!   output argument list:
!     ref_mos_3d  - 3D reflectivity in analysis grid
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
  use gsd_kinds, only: r_kind,i_kind,r_single
  implicit none

  INTEGER(i_kind),intent(in)   :: mype
  INTEGER(i_kind),intent(in)   :: nlon,nlat,nsig
  real(r_single), intent(in)   :: h_bk(nlon,nlat,nsig)                  ! height
  real(r_single), intent(in)   :: lightning(nlon,nlat)
  real(r_kind),   intent(inout):: ref_mos_3d(nlon,nlat,nsig)            ! reflectivity in grid
!
! local
!
  real(r_kind) :: dbz_lightning(nlon,nlat)
  real(r_kind) :: table_lghtn2ref_winter(30)   ! table content the map from lightning strakes 
                                               ! to maximum reflectivity 
  DATA table_lghtn2ref_winter/       &
     32.81,33.98,34.93,36.26,36.72,37.07,37.93,38.79,39.65,40.10, &
     40.42,41.42,41.90,42.04,42.19,42.45,42.90,43.20,43.50,43.80, &
     44.10,44.66,44.84,45.56,45.64,45.80,45.95,46.11,46.32,46.50/

  real(r_kind) :: table_lghtn2ref_summer(30)   ! table content the map from lightning strakes 
                                               ! to maximum reflectivity 
  DATA table_lghtn2ref_summer/       &
     30.13,31.61,32.78,33.86,34.68,35.34,36.13,36.15,37.02,37.04, &
     37.74,38.00,38.56,38.85,39.10,39.37,39.78,39.98,40.64,41.33, &
     41.50,41.65,41.85,42.08,42.77,43.03,43.26,43.53,43.74,43.73/

  integer(i_kind) :: maxlvl 
  parameter (maxlvl=31)
  real(r_kind) :: newlvlAll(maxlvl)   ! vertical levels of reflectivity statistic profile
  DATA newlvlAll/0.2, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 2.25, 2.5, 2.75, &
                3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, &
                9, 10, 11, 12, 13, 14, 15, 16/

  real(r_kind) :: refprofile_winter(maxlvl,4)   ! statistic reflectivity profile used to  
                                                ! retrieve vertical ref based on lightning
!  max reflectivity 30-35 dbz
  DATA refprofile_winter(:,1) /      &
         0.966,0.958,0.977,0.989,0.998,1.000,0.997,0.992,0.981,0.962, &
         0.933,0.898,0.826,0.752,0.687,0.626,0.578,0.547,0.522,0.526, &
         0.519,0.501,0.482,0.464,0.437,0.430,0.454,0.539,0.662,0.742, &
         0.793/
!  max reflectivity 35-40 dbz
  DATA refprofile_winter(:,2) /      &
         0.947,0.953,0.980,0.994,1.000,0.996,0.987,0.974,0.956,0.928, &
         0.891,0.848,0.761,0.679,0.613,0.559,0.522,0.491,0.473,0.462, &
         0.451,0.433,0.415,0.403,0.382,0.380,0.406,0.482,0.603,0.707, &
         0.723/
!  max reflectivity 40-45 dbz
  DATA refprofile_winter(:,3) /      &
         0.937,0.955,0.986,1.000,0.997,0.995,0.988,0.978,0.957,0.920, &
         0.871,0.824,0.735,0.654,0.584,0.518,0.465,0.442,0.435,0.412, &
         0.398,0.385,0.376,0.360,0.340,0.350,0.377,0.446,0.551,0.625, &
         0.656/
!  max reflectivity 45-50 dbz
  DATA refprofile_winter(:,4) /      &
         0.900,0.949,0.982,0.995,1.000,0.998,0.983,0.954,0.914,0.874, &
         0.834,0.793,0.721,0.664,0.612,0.565,0.530,0.496,0.460,0.431, &
         0.402,0.383,0.370,0.354,0.335,0.321,0.347,0.342,0.441,0.510, &
         0.548/

  real(r_kind) :: refprofile_summer(maxlvl,4)   ! statistic reflectivity profile used to 
                                                ! retrieve vertical ref based on lightning
!  max reflectivity 30-35 dbz
  DATA refprofile_summer(:,1) /      &
         0.870,0.885,0.914,0.931,0.943,0.954,0.967,0.975,0.982,0.989, &
         0.995,1.000,0.998,0.973,0.918,0.850,0.791,0.735,0.690,0.657, &
         0.625,0.596,0.569,0.544,0.510,0.479,0.461,0.460,0.477,0.522, &
         0.570/
!  max reflectivity 35-40 dbz
  DATA refprofile_summer(:,2) /      &
         0.871,0.895,0.924,0.948,0.961,0.971,0.978,0.983,0.988,0.992, &
         0.997,1.000,0.995,0.966,0.913,0.848,0.781,0.719,0.660,0.611, &
         0.576,0.542,0.523,0.513,0.481,0.448,0.416,0.402,0.417,0.448, &
         0.491/
!  max reflectivity 40-45 dbz
  DATA refprofile_summer(:,3) /      &
         0.875,0.895,0.914,0.936,0.942,0.951,0.964,0.979,0.990,0.998, &
         1.000,0.992,0.961,0.905,0.834,0.772,0.722,0.666,0.618,0.579, &
         0.545,0.518,0.509,0.483,0.419,0.398,0.392,0.403,0.423,0.480, &
         0.440/
!  max reflectivity 45-50 dbz
  DATA refprofile_summer(:,4) /      &
         0.926,0.920,0.948,0.975,0.988,0.989,0.995,0.997,1.000,1.000, &
         0.997,0.991,0.970,0.939,0.887,0.833,0.788,0.741,0.694,0.655, &
         0.611,0.571,0.551,0.537,0.507,0.470,0.432,0.410,0.420,0.405, &
         0.410/

  INTEGER(i_kind) :: season   ! 1= summer, 2=winter
  INTEGER(i_kind) :: num_lightning
  INTEGER(i_kind) :: i,j, k2, k, mref
  REAL(r_kind)    :: heightGSI,upref,downref,wght
  INTEGER(i_kind) :: ilvl
  REAL(r_kind)    :: lowest,highest,tempref, tempprofile(maxlvl)


!
! map lightning strokes to maximum reflectiivty 
!
  season=1
  dbz_lightning = -9999.0_r_kind
  DO j=2,nlat-1
    DO i=2,nlon-1
      if(lightning(i,j) > 0.1_r_kind ) then
        num_lightning = max(1,min(30,int(lightning(i,j))))
        if(season== 2 ) then
           dbz_lightning(i,j) = table_lghtn2ref_winter(num_lightning)
        else if(season== 1 ) then
           dbz_lightning(i,j) = table_lghtn2ref_summer(num_lightning)
        endif
      endif
    ENDDO
  ENDDO
!
!  vertical reflectivity distribution
!
  DO k=1,maxlvl
     newlvlAll(k)=newlvlAll(k)*1000.0_r_kind
  ENDDO

!  ref_mos_3d=-9999.0
  DO j=2,nlat-1
    DO i=2,nlon-1
      if( dbz_lightning(i,j) > 30 ) then
         mref =  min(4,(int((dbz_lightning(i,j) - 30.0_r_kind)/5.0_r_kind) + 1 ))
         if(season== 2 ) then
           DO k=1,maxlvl
              tempprofile(k)=refprofile_winter(k,mref)*dbz_lightning(i,j)
           enddo
           lowest=newlvlAll(2)
           highest=7000.0_r_kind
         else if(season== 1 ) then
           DO k=1,maxlvl
              tempprofile(k)=refprofile_summer(k,mref)*dbz_lightning(i,j)
           enddo
           lowest=newlvlAll(3)
           highest=12000.0_r_kind
         endif
         DO k2=1,nsig
           heightGSI=h_bk(i,j,k2)
           if(heightGSI >= lowest .and. heightGSI < highest) then  ! lower 12km ?
              do k=1,maxlvl-1
                if( heightGSI >=newlvlAll(k) .and. heightGSI < newlvlAll(k+1) ) ilvl=k
              enddo
              upref=tempprofile(ilvl+1)
              downref=tempprofile(ilvl)
              wght=(heightGSI-newlvlAll(ilvl))/(newlvlAll(ilvl+1)-newlvlAll(ilvl))
              tempref=(1-wght)*downref + wght*upref
              ref_mos_3d(i,j,k2) = max(ref_mos_3d(i,j,k2),tempref)
           endif
         ENDDO
      endif
    ENDDO
  ENDDO

END SUBROUTINE convert_lghtn2ref

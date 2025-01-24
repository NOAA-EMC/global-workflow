SUBROUTINE vinterp_radar_ref(mype,nlon,nlat,nsig,Nmsclvl,ref_mos_3d,ref_mosaic31,h_bk,zh)
!
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  interp_radar_ref    radar observation vertical interpolation     
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2006-11-17
!
! ABSTRACT: 
!  This subroutine interpolate radar reflectivity vertically
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
!     Nmsclvl     -  vertical level of radar observation ref_mosaic31
!     ref_mosaic31-  radar reflectivity horizontally in analysis grid and vertically
!                      in mosaic grid (height)
!     h_bk        - 3D background height  
!     zh          - terrain
!
!   output argument list:
!     ref_mos_3d  - 3D radar reflectivity in analysis grid
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
  use kinds, only: r_kind,i_kind, r_single
  implicit none

  INTEGER(i_kind), intent(in) ::  mype
  INTEGER(i_kind), intent(in) ::  nlon
  INTEGER(i_kind), intent(in) ::  nlat
  INTEGER(i_kind), intent(in) ::  nsig
  INTEGER(i_kind), intent(in) ::  Nmsclvl
  real(r_single),  intent(in) :: h_bk(nlon,nlat,nsig)                  ! 3D height
  real(r_single),  intent(in) :: zh(nlon,nlat)                         ! terrain
  real(r_kind),    intent(in) :: ref_mosaic31(nlon,nlat,Nmsclvl)
  real(r_kind),    intent(out):: ref_mos_3d(nlon,nlat,nsig)            ! reflectivity in grid
!
!  local
!
  real(r_kind)    :: msclvl21(21),msclvlAll(31)
  INTEGER(i_kind) :: mscX,mscY
  DATA msclvl21/1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 6, 7,  &
              8, 9, 10, 11, 12, 13, 14, 15, 16, 17/
  DATA msclvlAll/0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 2.25, 2.5, 2.75, &
                 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, &
                 9, 10, 11, 12, 13, 14, 15, 16, 18/
!
  REAL(r_kind)    :: heightGSI,upref,downref,wght
  INTEGER(i_kind) :: ilvl,numref

  real(r_kind)    :: ref_mosaic
  INTEGER(i_kind) :: i,j, k2, k

!
  if(Nmsclvl < -888 ) then
     write(6,*) 'interp_radar_ref: No radar reflectivity data in this subdomain !'
     return
  endif
!
  ref_mos_3d=-99999.0_r_kind
  numref=0
  if (Nmsclvl == 31 ) then
      DO k=1,Nmsclvl
        msclvlAll(k)=msclvlAll(k)*1000.0_r_kind
      ENDDO
  elseif( Nmsclvl == 21 ) then
      msclvlAll=0
      DO k=1,Nmsclvl
        msclvlAll(k)=msclvl21(k)*1000.0_r_kind
      ENDDO
  else
      write(6,*) 'interp_radar_ref: Wrong vertical radar mosaic levels'
      write(6,*) ' the level read in is:', msclvlAll
      call stop2(114)
  endif
  
  DO k2=1,nsig
    DO j=2,nlat-1
      DO i=2,nlon-1
        heightGSI=h_bk(i,j,k2)+zh(i,j)
        if(heightGSI >= msclvlAll(1) .and. heightGSI < msclvlAll(Nmsclvl) ) then
           do k=1,Nmsclvl-1
             if( heightGSI >=msclvlAll(k) .and. heightGSI < msclvlAll(k+1) ) ilvl=k
           enddo
           upref=ref_mosaic31(i,j,ilvl+1)
           downref=ref_mosaic31(i,j,ilvl)
           if(abs(upref) <90.0_r_kind .and. abs(downref) <90.0_r_kind ) then
             wght=(heightGSI-msclvlAll(ilvl))/(msclvlAll(ilvl+1)-msclvlAll(ilvl))
             ref_mosaic=(1-wght)*downref + wght*upref
             numref=numref+1
           elseif( abs(upref+99.0_r_kind) < 0.1_r_kind .or. &   
                   abs(downref+99.0_r_kind) <0.1_r_kind ) then
             ref_mosaic=-99.0_r_kind
           else
             ref_mosaic=-99999.0_r_kind
           endif
           ref_mos_3d(i,j,k2)=max(ref_mos_3d(i,j,k2),ref_mosaic)
        else
          ref_mos_3d(i,j,k2)=-99999.0_r_kind
        endif
      ENDDO
    ENDDO
  ENDDO

!
  DO k2=1,nsig
    DO i=2,nlon-1
      ref_mos_3d(i,1,k2)=ref_mos_3d(i,2,k2)
      ref_mos_3d(i,nlat,k2)=ref_mos_3d(i,nlat-1,k2)
    ENDDO
    DO j=2,nlat-1
      ref_mos_3d(1,j,k2)=ref_mos_3d(2,j,k2)
      ref_mos_3d(nlon,j,k2)=ref_mos_3d(nlon-1,j,k2)
    ENDDO
    ref_mos_3d(nlon,nlat,k2)=ref_mos_3d(nlon-1,nlat-1,k2)
    ref_mos_3d(nlon,1,k2)=ref_mos_3d(nlon-1,2,k2)
    ref_mos_3d(1,nlat,k2)=ref_mos_3d(2,nlat-1,k2)
    ref_mos_3d(1,j,k2)=ref_mos_3d(2,2,k2)
  ENDDO


END SUBROUTINE vinterp_radar_ref

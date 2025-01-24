SUBROUTINE cloudLayers(nlat,nlon,nsig,h_bk,zh,cld_cover_3d,cld_type_3d, &
                       cloudlayers_i)
!
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  cloudLayers       find cloud layers                             
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2006-11-17
!
! ABSTRACT: 
!  This subroutine find cloud layer based on cloud cover
!
! PROGRAM HISTORY LOG:
!    2009-01-20  Hu  Add NCO document block
!
!
!   input argument list:
!     nlon        - no. of lons on subdomain (buffer points on ends)
!     nlat        - no. of lats on subdomain (buffer points on ends)
!     nsig        - no. of levels
!     h_bk        - 3D background height  
!     zh          - terrain
!     cld_cover_3d- 3D cloud cover
!     cld_type_3d - 3D cloud type
!
!   output argument list:
!     cloudlayers_i - 3D cloud layer index
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

  use kinds, only: r_single,i_kind

  implicit none

  integer(i_kind),intent(in) :: nlat,nlon,nsig
!
!  background
!
  real(r_single), intent(in) :: zh(nlon,nlat)          ! terrain
  real(r_single), intent(in) :: h_bk(nlon,nlat,nsig)   ! height
!
!  Variables for cloud analysis
!
  real (r_single),intent(in) :: cld_cover_3d(nlon,nlat,nsig)
  integer(i_kind),intent(in) :: cld_type_3d(nlon,nlat,nsig)
!
! output
!
  integer(i_kind),intent(out):: cloudlayers_i(nlon,nlat,21)  ! 5 different layers
!                                      1= the number of layers
!                                      2,4,... bottom
!                                      3,5,... top
!
! threshold
  real (r_single) :: thresh_cvr
  parameter ( thresh_cvr = 0.1 )
!-----------------------------------------------------------
!
! temp.
!
  INTEGER :: i,j,k,k1,nlvl
  INTEGER :: k_top,k_base
  real (r_single) :: zs_1d(nsig)
  real (r_single) :: cv_1d(nsig)
!
!====================================================================
!  Begin
!
  cloudlayers_i=-99999
!-----------------------------------------------------------------------
!
!  Find Cloud Layers and Computing Output Field(s)
!  The procedure works column by column.
!
!-----------------------------------------------------------------------
!

  DO j = 2,nlat-1
    DO i = 2,nlon-1
! Initialize
      DO k = 1,nsig
        zs_1d(k) = h_bk(i,j,k)
        cv_1d(k) = cld_cover_3d(i,j,k)
      END DO
!
!-----------------------------------------------------------------------
!
!  Get Base and Top
!
!-----------------------------------------------------------------------
!
      k=1
      nlvl=0
      DO WHILE (k <= nsig-1)

        IF((cv_1d(k+1) >= thresh_cvr .and. cv_1d(k)<thresh_cvr) .or.     &
           (k == 1 .AND. cv_1d(k) >= thresh_cvr) ) THEN
            k_base = k + 1

            k = k + 1
            DO WHILE (cv_1d(k) >= thresh_cvr .and. k < nsig)
                k_top = k
!
!-----------------------------------------------------------------------
!
!  We have now defined a cloud base and top
!
!-----------------------------------------------------------------------
!
               k=k+1
            enddo
            k=k-1
!-----------------------------------------------------------------------
!
!  Make sure cloud base and top stay in the model domain
!
!-----------------------------------------------------------------------
!
            nlvl=nlvl+2
            if(nlvl > 20 ) then
              write(6,*) 'cloudLayers: Too many cloud layers in grid point:'
              write(6,*) i,j
              call stop2(114)
            endif
            cloudlayers_i(i,j,nlvl)   = MIN(k_base,nsig-1)
            cloudlayers_i(i,j,nlvl+1) = MIN(k_top,nsig-1)
        endif
!
        k=k+1
      ENDDO  ! k
!
      cloudlayers_i(i,j,1) = nlvl/2
    ENDDO
  ENDDO
!
!
!
  DO j = 2,nlat-1
    DO i = 2,nlon-1
      if(cloudlayers_i(i,j,1) > 0 ) then
        do k=1,cloudlayers_i(i,j,1)
          if(cloudlayers_i(i,j,k) < 0 .or. cloudlayers_i(i,j,k) > 55555) then
            write(6,*) 'cloudLayers: ckeck', i,j,k, cloudlayers_i(i,j,k)
          endif
        enddo
      endif
    enddo
  enddo
!

END SUBROUTINE cloudLayers


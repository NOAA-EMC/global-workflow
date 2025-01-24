subroutine map_ctp (ib,jb,nx,ny,nn_obs,numsao,data_s,sat_ctp,sat_tem,w_frac,npts_rad,ioption)

!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  map_ctp   map GOES cloud product to analysis grid              
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2006-03_10
!
! ABSTRACT: 
!  This subroutine map GOES cloud product to analysis grid
!
! PROGRAM HISTORY LOG:
!    2009-01-20  Hu  Add NCO document block
!
!
!   input argument list:
!     ib       - begin i point of this domain
!     jb       - begin j point of this domain
!     nx       - no. of lons on subdomain (buffer points on ends)
!     ny       - no. of lats on subdomain (buffer points on ends)
!     nn_obs   - 1st dimension of observation arry data_s
!     numsao   - number of observation
!     data_s   -  observation array for GOES cloud products
!     npts_rad -  impact radius 
!
!   output argument list:
!     sat_ctp   - GOES cloud top pressure in analysis grid
!     sat_tem   - GOES cloud top temperature in analysis grid
!     w_frac    - GOES cloud coverage in analysis grid
!
! USAGE:
!   INPUT FILES: 
!
!   OUTPUT FILES:
!
! REMARKS:
!
!     adapted according to RUC subroutine rd_cld
! *
! * This routine reads NESDIS (Madison, WI) cloud product produced
! *  from GOES sounder data. The original product is reprocessed onto
! *   MAPS40 grid boxes. There could be more than one cloud product
! *    in a grid-box, so we use the nearest one that falls in the
! *     grid. The routine combines GOES-8 and 10 products.
!
! ===== History =====
!
! * Internal variables:
!     CTP_E, CTP_W           Soft-linked filename for ascii GOES Clouds
!
! * Working variables:
!
! * Working variables used for sorting max size of 10:
!     Pxx, Txx, xdist,xxxdist     (R4)
!     Fxx, Nxx, index, jndex      (I4)
!     ioption              (I4)  = 1  if selection is nearest neighbor
!                                = 2  if selection is median of samples
!
!
! * Output variables on gridpoint (Nx,Ny):
!     sat_ctp, sat_tem (R4)   Cloud-top pressure and temperature
!     w_frac         (R4)   Effective fractional cloud coverage, option=1
!                           fractional coverage within RUC grid, option=2
!     w_eca          (R4)   Effective fractional cloud regardless option
!                             (effective cloud amount - eca)
!     nlev_cld       (I4)   Number of cloud levels. TO BE USED LATER
!                            to incorporate multi-level cloud
!
! * Calling routines
!     sorting
!     sortmed
!
! *
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 
!   MACHINE:  Linux cluster (WJET)
!
!$$$
!
!_____________________________________________________________________
!

      use gsd_kinds, only: r_kind,r_single,i_kind
      use constants, only: zero,one_tenth,one,deg2rad
                         
      implicit none

! input-file variables:
      INTEGER(i_kind),intent(in) :: Nx, Ny
      INTEGER(i_kind),intent(in) :: ib, jb
      INTEGER(i_kind),intent(in) :: numsao, nn_obs
      INTEGER(i_kind),intent(in) :: npts_rad
      INTEGER(i_kind),intent(in) :: ioption
      real(r_kind),dimension(nn_obs,numsao):: data_s
! Output
      real(r_single), intent(out) ::  sat_ctp(Nx,Ny)
      real(r_single), intent(out) ::  sat_tem(Nx,Ny)
      real(r_single), intent(out) ::  w_frac(Nx,Ny)
!
!  misc
      integer(i_kind) ::   nfov
      parameter (nfov=60)

! Working
      real(r_kind)    ::  Pxx(Nx,Ny,nfov),Txx(Nx,Ny,nfov)  
      real(r_kind)    ::  xdist(Nx,Ny,nfov), xxxdist(nfov)
      real(r_kind)    ::  fr,sqrt
      integer(i_kind) ::  Nxx(Nx,Ny,nfov),index(Nx,Ny), jndex(nfov)
      integer(i_kind) ::  ipt,ii,jj,i,med_pt,ii1,jj1

      real(r_kind)    :: xc
      real(r_kind)    :: yc

      real(r_single)  :: w_eca(Nx,Ny)
      integer(i_kind) :: nlev_cld(Nx,Ny)
      integer(i_kind) :: ios

!
! * Initialize outputs since GOES sounder do not scan all MAPS domain
!
      do jj=1,Ny
        do ii=1,Nx
           w_eca (ii,jj) =-99999._r_kind
           index(ii,jj) = 0
        enddo
      enddo

! -- set ios as failed unless valid data points are found below
      ios = 0

! -----------------------------------------------------------
! -----------------------------------------------------------
!     Map each FOV onto RR grid points 
! -----------------------------------------------------------
! -----------------------------------------------------------
      do ipt=1,numsao
 
          xc=data_s(2,ipt) - ib + 1.0_r_kind
          yc=data_s(3,ipt) - jb + 1.0_r_kind
          if(data_s(8,ipt) > 50 ) cycle
 
! * XC,YC should be within subdomain boundary, i.e., XC,YC >0
 
          if(XC >= 1._r_kind .and. XC < Nx .and.        &
             YC >= 1._r_kind .and. YC < Ny) then
             ii1 = int(xc+0.5_r_kind)
             jj1 = int(yc+0.5_r_kind)

             do jj = max(1,jj1-npts_rad), min(ny,jj1+npts_rad)
               if (jj1-1 >= 1 .and. jj1+1 <= ny) then
                 do ii = max(1,ii1-npts_rad), min(nx,ii1+npts_rad)
                   if (ii1-1 >= 1 .and. ii1+1 <= nx) then
             
! * We check multiple data within gridbox

                     if (index(ii,jj) < nfov) then
                       index(ii,jj) = index(ii,jj) + 1
 
                       Pxx(ii,jj,index(ii,jj)) = data_s(4,ipt)
                       Txx(ii,jj,index(ii,jj)) = data_s(6,ipt)
!mhu                   Nxx(ii,jj,index(ii,jj)) = int(data_s(5,ipt))
!mhu  no cloud amount available, assign to 100
                       Nxx(ii,jj,index(ii,jj)) = 100
                       nlev_cld(ii,jj) = 1
                       xdist(ii,jj,index(ii,jj)) = sqrt(      &
                             (XC+1-ii)**2 + (YC+1-jj)**2)
                     end if
                   endif
                 enddo ! ii
               endif
             enddo  ! jj
          endif  ! observation is in the domain
      enddo ! ipt
!
! * ioption = 1 is nearest neighrhood
! * ioption = 2 is median of cloudy fov
      ! remove hard code choice ioption = 2
!
      do jj = 1,Ny
        do ii = 1,Nx
          if ((index(ii,jj) >= 1 .and. index(ii,jj) < 3) .and. npts_rad > 1) then
             sat_ctp(ii,jj) = Pxx(ii,jj,1)
             sat_tem(ii,jj) = Txx(ii,jj,1)
             w_frac(ii,jj) = float(Nxx(ii,jj,1))/100.
             w_eca(ii,jj) =  float(Nxx(ii,jj,1))/100.

          elseif(index(ii,jj) >= 3) then

! * We decided to use nearest neighborhood for ECA values,
! *     a kind of convective signal from GOES platform...

             do i=1,index(ii,jj)
               jndex(i) = i
               xxxdist(i) = xdist(ii,jj,i)
             enddo
             call sorting(xxxdist,index(ii,jj),jndex)
             w_eca(ii,jj) = float(Nxx(ii,jj,jndex(1)))/100._r_kind
! * Sort to find closest distance if more than one sample
             if(ioption == 1) then    !nearest neighborhood
                do i=1,index(ii,jj)
                  jndex(i) = i
                  xxxdist(i) = xdist(ii,jj,i)
                enddo
                call sorting(xxxdist,index(ii,jj),jndex)
                sat_ctp(ii,jj) = Pxx(ii,jj,jndex(1))
                sat_tem(ii,jj) = Txx(ii,jj,jndex(1))
                w_frac(ii,jj) = float(Nxx(ii,jj,jndex(1)))/100._r_kind
             endif
! * Sort to find median value 
             if(ioption == 2) then    !pick median 
                do i=1,index(ii,jj)
                  jndex(i) = i
                  xxxdist(i) = Pxx(ii,jj,i)
                enddo
                call sortmed(xxxdist,index(ii,jj),jndex,fr)
                med_pt = index(ii,jj)/2  + 1
                sat_ctp(ii,jj) = Pxx(ii,jj,jndex(med_pt))
                sat_tem(ii,jj) = Txx(ii,jj,jndex(med_pt))
                w_frac(ii,jj) = fr
             endif
          endif
        enddo  !ii
      enddo  !jj
 
      return
end subroutine map_ctp
 
subroutine sorting(d,n,is) 
      use gsd_kinds, only: r_kind,i_kind
      implicit none

      integer(i_kind), intent(in) :: n
      real(r_kind)   , intent(inout) :: d(n)
      integer(i_kind), intent(inout) :: is(n)
!
      integer(i_kind) :: nm1,ip1,iold,i,j
      real(r_kind)    :: temp
!
!
      nm1 = n-1 
      do 10 i=1,nm1 
      ip1 = i+1 
        do 10 j=ip1,n 
        if(d(i) <= d(j)) goto 10 
          temp = d(i) 
          d(i) = d(j) 
          d(j) = temp 
          iold  = is(i) 
          is(i) = is(j) 
          is(j) = iold 
   10 continue 
      return 
end subroutine  sorting

subroutine sortmed(p,n,is,f) 
      use gsd_kinds, only: r_kind,i_kind
      implicit none
      real(r_kind),    intent(inout) :: p(n)
      integer(i_kind), intent(in)    :: n
      integer(i_kind), intent(inout) :: is(n)
! * count cloudy fov
      real(r_kind),    intent(out)   ::  f
      integer(i_kind) ::  cfov
! 
      integer(i_kind) :: i,j,nm1,ip1,iold
      real(r_kind)    :: temp
!
!
!
      cfov = 0
      do i=1,n
         if(p(i) < 999._r_kind) cfov = cfov + 1
      enddo
      f = float(cfov)/(max(1,n))
! cloud-top pressure is sorted high cld to clear
      nm1 = n-1 
      do 10 i=1,nm1 
      ip1 = i+1 
        do 10 j=ip1,n 
        if(p(i)<=p(j)) goto 10 
          temp = p(i) 
          p(i) = p(j) 
          p(j) = temp 
          iold  = is(i) 
          is(i) = is(j) 
          is(j) = iold 
   10 continue 
      return 
end subroutine sortmed

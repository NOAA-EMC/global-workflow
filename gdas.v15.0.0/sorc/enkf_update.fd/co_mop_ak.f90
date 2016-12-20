subroutine co_mop_ak(g,ga,nlevs,ak,ap)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    co_mop_ak   Application of MOPITT averaging kernel to CO field (after interpolation) 
!   prgmmr: tangborn                          date:  2010-07-12
!
! abstract:  This routine takes the ges field, interpolated to MOPITT
!            averaging kernel levels (in tintrp3), and applies the averaging 
!            kernel. 
!            
!
! program history log:
!   2010-07-12  tangborn - new code 
!
!   input argument list:
!     g        - input field (ges field at averaging kernel levels) 
!     n        - number of interpolatees
!     nlevs    - number of observational levels
!     ap       - apriori profile
!     ak(j,k)  - averaging kernel: First index is the profile level. 
!
!   output argument list:
!     ga        - output guess co profile at observation location with averaging kernel applied)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
!--------
  use kinds, only: r_kind,i_kind
  use guess_grids, only: nfldsig,hrdifsig,ges_prsi
  use gridmod, only: lat2,lon2,nlat,nlon,nsig,lon1,istart,jstart
  use constants, only: zero, one
  implicit none

! Declare passed variables
  integer(i_kind)                               ,intent(in   ) :: nlevs
  real(r_kind),dimension(nlevs),intent(in   ) :: g 
  real(r_kind),dimension(nlevs)                 ,intent(in   ) :: ap
  real(r_kind),dimension(nlevs,nlevs)           ,intent(in   ) :: ak
  real(r_kind),dimension(nlevs)               ,intent(  out) :: ga

! Declare local variables
  integer(i_kind) j,k
  real(r_kind) rsum
  logical,parameter::debug=.false.


!*************************************************************************
! Initialize variables


! Loop over number of observations.


!  Apply averaging kernel 

    do k=1,nlevs 
       rsum=zero
       if(debug) print*,'k=',k
       do j=1,nlevs 
          rsum=rsum+ak(k,j)*(log10(g(j))-log10(ap(j)))
       enddo 
       if(debug) print*,'rsum=',rsum
       rsum=rsum+log10(ap(k))
       ga(k)=10.0**rsum   
       if(debug) print*,'ga=',ga(k)
    enddo 
          


! End of routine
  return
end subroutine co_mop_ak 

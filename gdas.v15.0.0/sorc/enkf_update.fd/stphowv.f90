module stphowvmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   stphowvmod    module for stphowv
!  prgmmr:
!
! abstract: module for stphowv
!
! program history log:
!   2014-04-10  pondeca
!   2015-07-10  pondeca  - force return if no howv data available
!
! subroutines included:
!   sub stphowv
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

PRIVATE
PUBLIC stphowv

contains

subroutine stphowv(howvhead,rval,sval,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stphowv      calculate penalty and contribution to stepsize
!
! abstract: calculate penalty and contribution to stepsize for significant wave height
!            with addition of nonlinear qc
!
! program history log:
!   2014-05-07  pondeca - add howv
!
!   input argument list:
!     howvhead
!     rhowv     - search direction for howv
!     showv     - analysis increment for howv
!     sges     - step size estimate (nstep)
!     nstep    - number of stepsizes  (==0 means use outer iteration values)
!                                         
!   output argument list:         
!     out(1:nstep)   - contribution to penalty for conventional howv - sges(1:nstep)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use obsmod, only: howv_ob_type
  use qcmod, only: nlnqc_iter,varqc_iter
  use constants, only: half,one,two,tiny_r_kind,cg_term,zero_quad
  use gridmod, only: latlon11
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  implicit none

! Declare passed variables
  type(howv_ob_type),pointer           ,intent(in   ) :: howvhead
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_quad),dimension(max(1,nstep)),intent(inout) :: out
  type(gsi_bundle)                    ,intent(in   ) :: rval,sval
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges

! Declare local variables  
  integer(i_kind) j1,j2,j3,j4,kk,ier,istatus
  real(r_kind) w1,w2,w3,w4
  real(r_kind) val,val2
  real(r_kind) cg_howv,howv,wgross,wnotgross
  real(r_kind),dimension(max(1,nstep)):: pen
  real(r_kind) pg_howv
  real(r_kind),pointer,dimension(:) :: showv
  real(r_kind),pointer,dimension(:) :: rhowv
  type(howv_ob_type), pointer :: howvptr

  out=zero_quad

! If no howv data return
  if(.not. associated(howvhead))return

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'howv',showv,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'howv',rhowv,istatus);ier=istatus+ier
  if(ier/=0)return

  howvptr => howvhead
  do while (associated(howvptr))
     if(howvptr%luse)then
        if(nstep > 0)then
           j1=howvptr%ij(1)
           j2=howvptr%ij(2)
           j3=howvptr%ij(3)
           j4=howvptr%ij(4)
           w1=howvptr%wij(1)
           w2=howvptr%wij(2)
           w3=howvptr%wij(3)
           w4=howvptr%wij(4)

           val =w1*rhowv(j1)+w2*rhowv(j2)+w3*rhowv(j3)+w4*rhowv(j4)
           val2=w1*showv(j1)+w2*showv(j2)+w3*showv(j3)+w4*showv(j4)-howvptr%res

           do kk=1,nstep
              howv=val2+sges(kk)*val
              pen(kk)= howv*howv*howvptr%err2
           end do
        else
           pen(1)=howvptr%res*howvptr%res*howvptr%err2
        end if
 
!  Modify penalty term if nonlinear QC
        if (nlnqc_iter .and. howvptr%pg > tiny_r_kind .and.  &
                             howvptr%b  > tiny_r_kind) then
           pg_howv=howvptr%pg*varqc_iter
           cg_howv=cg_term/howvptr%b
           wnotgross= one-pg_howv
           wgross = pg_howv*cg_howv/wnotgross
           do kk=1,max(1,nstep)
              pen(kk)= -two*log((exp(-half*pen(kk)) + wgross)/(one+wgross))
           end do
        endif

        out(1) = out(1)+pen(1)*howvptr%raterr2
        do kk=2,nstep
           out(kk) = out(kk)+(pen(kk)-pen(1))*howvptr%raterr2
        end do
     end if

     howvptr => howvptr%llpoint

  end do
  
  return
end subroutine stphowv

end module stphowvmod

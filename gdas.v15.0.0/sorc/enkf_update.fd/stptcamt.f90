module stptcamtmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   stptcamtmod    module for stptcamt
!  prgmmr:
!
! abstract: module for stptcamt
!
! program history log:
!   2012-01-23  zhu
!
! subroutines included:
!   sub stptcamt
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

PRIVATE
PUBLIC stptcamt

contains

subroutine stptcamt(tcamthead,rval,sval,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stptcamt      calculate penalty and contribution to stepsize
!   prgmmr: zhu           org: np23                date: 2012-01-23
!
! abstract: calculate penalty and contribution to stepsize for surface pressure
!            with addition of nonlinear qc
!
! program history log:
!   2012-01-23  zhu  
!
!   input argument list:
!     tcamthead
!     rtcamt     - search direction for tcamt
!     stcamt     - analysis increment for tcamt
!     sges     - step size estimate (nstep)
!     nstep    - number of stepsizes  (==0 means use outer iteration values)
!                                         
!   output argument list:         
!     out(1:nstep)   - contribution to penalty for conventional tcamt - sges(1:nstep)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use obsmod, only: tcamt_ob_type
  use qcmod, only: nlnqc_iter,varqc_iter
  use constants, only: half,one,two,tiny_r_kind,cg_term,zero_quad
  use gridmod, only: latlon11
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  implicit none

! Declare passed variables
  type(tcamt_ob_type),pointer           ,intent(in   ) :: tcamthead
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_quad),dimension(max(1,nstep)),intent(inout) :: out
  type(gsi_bundle)                    ,intent(in   ) :: rval,sval
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges

! Declare local variables  
  integer(i_kind) j1,j2,j3,j4,kk,ier,istatus
  real(r_kind) w1,w2,w3,w4
  real(r_kind) val,val2
  real(r_kind) cg_tcamt,tcamt,wgross,wnotgross
  real(r_kind),dimension(max(1,nstep)):: pen
  real(r_kind) pg_tcamt
  real(r_kind),pointer,dimension(:) :: stcamt
  real(r_kind),pointer,dimension(:) :: rtcamt
  type(tcamt_ob_type), pointer :: tcamtptr

  out=zero_quad

! If no tcamt data return
  if(.not. associated(tcamthead))return

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'tcamt',stcamt,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'tcamt',rtcamt,istatus);ier=istatus+ier
  if(ier/=0)return

  tcamtptr => tcamthead
  do while (associated(tcamtptr))
     if(tcamtptr%luse)then
        if(nstep > 0)then
           j1=tcamtptr%ij(1)
           j2=tcamtptr%ij(2)
           j3=tcamtptr%ij(3)
           j4=tcamtptr%ij(4)
           w1=tcamtptr%wij(1)
           w2=tcamtptr%wij(2)
           w3=tcamtptr%wij(3)
           w4=tcamtptr%wij(4)

           val =w1*rtcamt(j1)+w2*rtcamt(j2)+w3*rtcamt(j3)+w4*rtcamt(j4)
           val2=w1*stcamt(j1)+w2*stcamt(j2)+w3*stcamt(j3)+w4*stcamt(j4)-tcamtptr%res

           do kk=1,nstep
              tcamt=val2+sges(kk)*val
              pen(kk)= tcamt*tcamt*tcamtptr%err2
           end do
        else
           pen(1)=tcamtptr%res*tcamtptr%res*tcamtptr%err2
        end if
 
!  Modify penalty term if nonlinear QC
        if (nlnqc_iter .and. tcamtptr%pg > tiny_r_kind .and.  &
                             tcamtptr%b  > tiny_r_kind) then
           pg_tcamt=tcamtptr%pg*varqc_iter
           cg_tcamt=cg_term/tcamtptr%b
           wnotgross= one-pg_tcamt
           wgross = pg_tcamt*cg_tcamt/wnotgross
           do kk=1,max(1,nstep)
              pen(kk)= -two*log((exp(-half*pen(kk)) + wgross)/(one+wgross))
           end do
        endif

        out(1) = out(1)+pen(1)*tcamtptr%raterr2
        do kk=2,nstep
           out(kk) = out(kk)+(pen(kk)-pen(1))*tcamtptr%raterr2
        end do
     end if

     tcamtptr => tcamtptr%llpoint

  end do
  
  return
end subroutine stptcamt

end module stptcamtmod

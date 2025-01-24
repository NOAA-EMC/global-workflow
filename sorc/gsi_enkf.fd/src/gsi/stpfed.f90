module stpfedmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   stpfedmod    module for stpfed and its tangent linear stpfed_tl
!  prgmmr:
!
! abstract: module for stpfed and its tangent linear stpfed_tl
!
! program history log:
!   2023-08-23  H. Wang - Modified based on sftdbzmod
!                       - add adjoint of fed operator 
!
! subroutines included:
!   sub stpfed
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

PRIVATE
PUBLIC stpfed

contains

subroutine stpfed(fedhead,rval,sval,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpfed       calculate penalty and contribution to
!                            stepsize with nonlinear qc added.
!   prgmmr: derber           org: np23                date: 1991-02-26
!
!
! program history log:
!   2019-08-23  H.Wang  - added for FED DA
!   input argument list:
!     fedhead
!     sges     - step size estimates (nstep)
!     nstep    - number of step sizes (== 0 means use outer iteration value)
!
!   output argument list     - output for step size calculation
!     out(1:nstep)   - penalty from fed sges(1:nstep)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use qcmod, only: nlnqc_iter,varqc_iter
  use constants, only: half,one,two,tiny_r_kind,cg_term,zero_quad,r3600
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use wrf_vars_mod, only : fed_exist
  use m_obsNode, only: obsNode
  use m_fedNode , only: fedNode
  use m_fedNode , only: fedNode_typecast
  use m_fedNode , only: fedNode_nextcast
!  use directDA_radaruse_mod, only: l_use_fed_directDA

  implicit none

! Declare passed variables
  class(obsNode), pointer             ,intent(in   ) :: fedhead
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_quad),dimension(max(1,nstep)),intent(inout) :: out
  type(gsi_bundle)                    ,intent(in   ) :: rval,sval
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges

! Declare local variables
  integer(i_kind) ier,istatus
  integer(i_kind) j1,j2,j3,j4,j5,j6,j7,j8,kk
  real(r_kind) w1,w2,w3,w4,w5,w6,w7,w8
  real(r_kind) valfed
  real(r_kind) fedcur
  real(r_kind) cg_fed,fed,wgross,wnotgross
  real(r_kind),dimension(max(1,nstep))::pen
  real(r_kind) pg_fed
  real(r_kind),pointer,dimension(:) :: sfed
  real(r_kind),pointer,dimension(:) :: rfed
  type(fedNode), pointer :: fedptr

  out=zero_quad

!  If no fed data return
  if(.not. associated(fedhead))return

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  if(fed_exist)then
    call gsi_bundlegetpointer(sval,'fed',sfed,istatus);ier=istatus+ier
    call gsi_bundlegetpointer(rval,'fed',rfed,istatus);ier=istatus+ier
  else
    return
  end if

  if(ier/=0)return

  fedptr => fedNode_typecast(fedhead)
  do while (associated(fedptr))
     if(fedptr%luse)then
        if(nstep > 0)then
           j1=fedptr%ij(1)
           j2=fedptr%ij(2)
           j3=fedptr%ij(3)
           j4=fedptr%ij(4)
           j5=fedptr%ij(5)
           j6=fedptr%ij(6)
           j7=fedptr%ij(7)
           j8=fedptr%ij(8)
           w1=fedptr%wij(1)
           w2=fedptr%wij(2)
           w3=fedptr%wij(3)
           w4=fedptr%wij(4)
           w5=fedptr%wij(5)
           w6=fedptr%wij(6)
           w7=fedptr%wij(7)
           w8=fedptr%wij(8)

           if( fed_exist )then
             valfed= w1* rfed(j1)+w2*rfed(j2)+w3*rfed(j3)+w4*rfed(j4)+ &
                     w5* rfed(j5)+w6*rfed(j6)+w7*rfed(j7)+w8*rfed(j8)

             fedcur= w1* sfed(j1)+w2* sfed(j2)+w3* sfed(j3)+w4*sfed(j4)+    &
                     w5* sfed(j5)+w6* sfed(j6)+w7* sfed(j7)+w8* sfed(j8)-   &
                     fedptr%res
           end if


           do kk=1,nstep
              fed=fedcur+sges(kk)*valfed
              pen(kk)=fed*fed*fedptr%err2
           end do
        else
           pen(1)=fedptr%res*fedptr%res*fedptr%err2
        end if

!  Modify penalty term if nonlinear QC
        if (nlnqc_iter .and. fedptr%pg > tiny_r_kind .and.  &
                             fedptr%b  > tiny_r_kind) then

              pg_fed=fedptr%pg*varqc_iter
              cg_fed=cg_term/fedptr%b
              wnotgross= one-pg_fed
              wgross = pg_fed*cg_fed/wnotgross
              do kk=1,max(1,nstep)
                 pen(kk)= -two*log((exp(-half*pen(kk)) + wgross)/(one+wgross))
              end do
        end if
        
        out(1) = out(1)+pen(1)*fedptr%raterr2
           kk=1
        do kk=2,nstep
           out(kk) = out(kk)+(pen(kk)-pen(1))*fedptr%raterr2
        end do
     end if

     fedptr => fedNode_nextcast(fedptr)

  end do
  return
end subroutine stpfed

end module stpfedmod

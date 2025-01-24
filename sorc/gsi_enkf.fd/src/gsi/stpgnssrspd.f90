module stpgnssrspdmod


!$$$ module documentation block
!           .      .    .                                       .
! module:   stpgnssrspdmod    module for stpgnssrspd and its tangent linear stpgnssrspd_tl
!  prgmmr: K. Apodaca         org: Spire Global, Inc.      date: 2022-03-12
!   Largely based on other stp_* routines 
!
! abstract: module for stpgnssrspd and its tangent linear stpgnssrspd_tl
!
! program history log:
!   2023-09-21  K. Apodaca  - add documentation
! subroutine included:
!   sub stpgnssrspd
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

PRIVATE
PUBLIC stpgnssrspd

contains

subroutine stpgnssrspd(gnssrspdhead,rval,sval,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpgnssrspd  calculate penalty and stepsize terms
!                for wind speed, with nonlinear qc.
!   2023-03-15  K. Apodaca - add GNSS-R L2 ocean wind speed 
!
! abstract: calculate penalty and stepsize terms for wind speed
!
! program history log:

!
!   input argument list:
!     gnssrspdhead
!     ru       - search direction for u
!     rv       - search direction for v
!     su       - analysis increment for u
!     sv       - analysis increment for v
!     sges     - step size estimates (nstep)
!     nstep    - number of stepsizes  (==0 means use outer iteration values)
!
!   output argument list 
!     out(1:nstep)   - contribution to penalty from wind speed sges(1:nstep)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use qcmod, only: nlnqc_iter,varqc_iter
  use constants, only: zero,half,one,two,tiny_r_kind,cg_term,zero_quad,r3600
  use gsi_4dvar, only: ltlint
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use m_obsNode, only: obsNode
  use m_gnssrspdNode, only: gnssrspdNode
  use m_gnssrspdNode, only: gnssrspdNode_typecast
  use m_gnssrspdNode, only: gnssrspdNode_nextcast
  implicit none

! Declare passed variables
  class(obsNode), pointer             ,intent(in   ) :: gnssrspdhead
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges
  real(r_quad),dimension(max(1,nstep)),intent(inout) :: out
  type(gsi_bundle)                    ,intent(in   ) :: rval,sval

! Declare local variables
  integer(i_kind) j1,j2,j3,j4,kk,ier,istatus
  real(r_kind) w1,w2,w3,w4,time_gnssrspd
  real(r_kind) valu,valv,ucur,vcur,gnssrspdnl,gnssrspdtl,uu,vv,gnssrspd
  real(r_kind),dimension(max(1,nstep)):: pen
  real(r_kind) cg_gnssrspd,pencur,wgross,wnotgross
  real(r_kind) pg_gnssrspd,pentl
  real(r_kind),pointer,dimension(:) :: su,sv
  real(r_kind),pointer,dimension(:) :: ru,rv
  type(gnssrspdNode), pointer :: gnssrspdptr

  out=zero_quad

!  If no gnssrspd data return
  if(.not. associated(gnssrspdhead))return

  time_gnssrspd=zero
! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'u',su,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(sval,'v',sv,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'u',ru,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'v',rv,istatus);ier=istatus+ier
  if(ier/=0)return

  gnssrspdptr => gnssrspdNode_typecast(gnssrspdhead)
  do while (associated(gnssrspdptr))

     if(gnssrspdptr%luse)then
        if(nstep > 0)then
           j1 = gnssrspdptr%ij(1)
           j2 = gnssrspdptr%ij(2)
           j3 = gnssrspdptr%ij(3)
           j4 = gnssrspdptr%ij(4)
           w1 = gnssrspdptr%wij(1)
           w2 = gnssrspdptr%wij(2)
           w3 = gnssrspdptr%wij(3)
           w4 = gnssrspdptr%wij(4)
 
           valu=w1* ru(j1)+w2* ru(j2)+w3* ru(j3)+w4* ru(j4)
           valv=w1* rv(j1)+w2* rv(j2)+w3* rv(j3)+w4* rv(j4)
           ucur=w1* su(j1)+w2* su(j2)+w3* su(j3)+w4* su(j4)+gnssrspdptr%uges
           vcur=w1* sv(j1)+w2* sv(j2)+w3* sv(j3)+w4* sv(j4)+gnssrspdptr%vges

           if (ltlint) then
              gnssrspd=sqrt(ucur*ucur+vcur*vcur)-gnssrspdptr%res
              pencur=gnssrspd*gnssrspd*gnssrspdptr%err2
              do kk=1,nstep
                 gnssrspdnl=sqrt(ucur*ucur+vcur*vcur)
                 gnssrspdtl=ucur*valu+vcur*valv
                 if (gnssrspdnl>tiny_r_kind*100._r_kind) then
                    gnssrspdtl=gnssrspdtl/gnssrspdnl
                 else
                    gnssrspdtl=zero
                 endif
                 pentl  =two*gnssrspdtl*gnssrspd*gnssrspdptr%err2
                 pen(kk)=pencur+sges(kk)*pentl
              end do
           else
              do kk=1,nstep
                 uu=ucur+sges(kk)*valu
                 vv=vcur+sges(kk)*valv
                 gnssrspd=sqrt(uu*uu+vv*vv)-gnssrspdptr%res
                 pen(kk)=gnssrspd*gnssrspd*gnssrspdptr%err2
              end do
           end if
        else
           pen(1)=gnssrspdptr%res*gnssrspdptr%res*gnssrspdptr%err2
        end if

!  Modify penalty term if nonlinear QC
        if (nlnqc_iter .and. gnssrspdptr%pg > tiny_r_kind .and. &
                             gnssrspdptr%b  > tiny_r_kind) then
           pg_gnssrspd=gnssrspdptr%pg*varqc_iter
           cg_gnssrspd=cg_term/gnssrspdptr%b
           wnotgross= one-pg_gnssrspd
           wgross = pg_gnssrspd*cg_gnssrspd/wnotgross
           pencur = -two*log((exp(-half*pencur) + wgross)/(one+wgross))
           do kk=1,max(1,nstep)
              pen(kk) = -two*log((exp(-half*pen(kk)  ) + wgross)/(one+wgross))
           enddo
        endif

        out(1) = out(1)+pen(1)*gnssrspdptr%raterr2
        do kk=2,nstep
           out(kk) = out(kk)+(pen(kk)-pen(1))*gnssrspdptr%raterr2
        end do

     end if
    
     gnssrspdptr => gnssrspdNode_nextcast(gnssrspdptr)

  end do
  return
end subroutine stpgnssrspd

end module stpgnssrspdmod

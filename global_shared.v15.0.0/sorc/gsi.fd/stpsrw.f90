module stpsrwmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   stpsrwmod    module for stpsrw and its tangent linear stpsrw_tl
!  prgmmr:
!
! abstract: module for stpsrw and its tangent linear stpsrw_tl
!
! program history log:
!   2005-05-19  Yanqiu zhu - wrap stpsrw and its tangent linear stpsrw_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2008-12-02  Todling - remove stpsrw_tl
!   2009-08-12  lueken - update documentation
!   2010-05-13  todling - uniform interface across stp routines
!
! subroutines included:
!   sub stpsrw
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

PRIVATE
PUBLIC stpsrw

contains

subroutine stpsrw(srwhead,rval,sval,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpsrw      apply nonlin qc op for radar superob wind
!   prgmmr: parrish          org: np22                date: 2004-06-22
!
! abstract: apply operator for radar superob wind and calculation of 
!             step size using nonlinear qc.
!
! program history log:
!   2004-06-22  parrish, document
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-09  parrish - add nonlinear qc option
!   2005-04-11  treadon - merge stpsrw and stpsrw_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2006-09-18  derber  - modify output values of b1 and b3
!   2007-02-15  rancic - add foto
!   2007-03-19  tremolet - binning of observations
!   2007-06-04  derber  - use quad precision to get reproducability over number of processors
!   2008-06-02  safford - rm unused var and uses
!   2008-12-03  todling - changed handling of ptr%time
!   2010-01-04  zhang,b - bug fix: accumulate penalty for multiple obs bins
!   2010-05-13  todling - update to use gsi_bundle
!
!   input argument list:
!     srwhead
!     ru       - search direction for u
!     su       - analysis increment for u
!     rv       - search direction for v
!     sv       - analysis increment for v
!     sges     - step size estimates (nstep)
!     nstep    - number of stepsizes (==0 means use outer iteration values)
!
!   output argument list  
!     out(1)   - penalty for srw obs - sges(1)
!     out(2)   - penalty for srw obs - sges(2)
!     out(3)   - penalty for srw obs - sges(3)
!     out(4)   - penalty for srw obs - sges(4)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use obsmod, only: srw_ob_type
  use qcmod, only: nlnqc_iter,varqc_iter
  use constants, only: half,one,two,tiny_r_kind,cg_term,zero_quad,r3600
  use gridmod, only: latlon1n
  use jfunc, only: l_foto,xhat_dt,dhat_dt
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  implicit none

! Declare passed variables
  type(srw_ob_type),pointer           ,intent(in)   :: srwhead
  integer(i_kind)                     ,intent(in)   ::nstep
  real(r_quad),dimension(max(1,nstep)),intent(inout):: out
  type(gsi_bundle)                    ,intent(in)   :: rval,sval
  real(r_kind),dimension(max(1,nstep)),intent(in)   :: sges

! Declare local variables
  integer(i_kind) ier,istatus
  integer(i_kind) j1,j2,j3,j4,j5,j6,j7,j8,kk
  real(r_kind) valu,facu,valv,facv,w1,w2,w3,w4,w5,w6,w7,w8,time_srw
  real(r_kind) bigu11,bigu12,bigu21,bigu22,facsrw1,facsrw2,valsrw1,valsrw2
  real(r_kind) cg_srw,uu,vv,wgross,wnotgross
  real(r_kind),dimension(max(1,nstep))::pen
  real(r_kind) pg_srw
  real(r_kind),pointer,dimension(:) :: xhat_dt_u,xhat_dt_v
  real(r_kind),pointer,dimension(:) :: dhat_dt_u,dhat_dt_v
  real(r_kind),pointer,dimension(:) :: su,sv
  real(r_kind),pointer,dimension(:) :: ru,rv
  type(srw_ob_type), pointer :: srwptr

  out=zero_quad

!  If no srw data return
  if(.not. associated(srwhead))return

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'u',su,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(sval,'v',sv,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'u',ru,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'v',rv,istatus);ier=istatus+ier
  if(l_foto) then
     call gsi_bundlegetpointer(xhat_dt,'u',xhat_dt_u,istatus);ier=istatus+ier
     call gsi_bundlegetpointer(xhat_dt,'v',xhat_dt_v,istatus);ier=istatus+ier
     call gsi_bundlegetpointer(dhat_dt,'u',dhat_dt_u,istatus);ier=istatus+ier
     call gsi_bundlegetpointer(dhat_dt,'v',dhat_dt_v,istatus);ier=istatus+ier
  endif
  if(ier/=0)return

  srwptr => srwhead
  do while (associated(srwptr))
     if(srwptr%luse)then
        if(nstep > 0)then
           j1=srwptr%ij(1)
           j2=srwptr%ij(2)
           j3=srwptr%ij(3)
           j4=srwptr%ij(4)
           j5=srwptr%ij(5)
           j6=srwptr%ij(6)
           j7=srwptr%ij(7)
           j8=srwptr%ij(8)
           w1=srwptr%wij(1)
           w2=srwptr%wij(2)
           w3=srwptr%wij(3)
           w4=srwptr%wij(4)
           w5=srwptr%wij(5)
           w6=srwptr%wij(6)
           w7=srwptr%wij(7)
           w8=srwptr%wij(8)

           bigu11=srwptr%rsrw(1)
           bigu21=srwptr%rsrw(2)
           bigu12=srwptr%rsrw(3)
           bigu22=srwptr%rsrw(4)
           valu=w1* ru(j1)+w2* ru(j2)+w3* ru(j3)+w4* ru(j4) &
               +w5* ru(j5)+w6* ru(j6)+w7* ru(j7)+w8* ru(j8) 

           valv=w1* rv(j1)+w2* rv(j2)+w3* rv(j3)+w4* rv(j4) &
               +w5* rv(j5)+w6* rv(j6)+w7* rv(j7)+w8* rv(j8) 


           facu=w1* su(j1)+w2* su(j2)+w3* su(j3)+w4* su(j4) &
               +w5* su(j5)+w6* su(j6)+w7* su(j7)+w8* su(j8) 

           facv=w1* sv(j1)+w2* sv(j2)+w3* sv(j3)+w4* sv(j4) &
               +w5* sv(j5)+w6* sv(j6)+w7* sv(j7)+w8* sv(j8) 
       
           if(l_foto) then
              time_srw=srwptr%time*r3600
              valu=valu+(w1*dhat_dt_u(j1)+w2*dhat_dt_u(j2)+ &
                         w3*dhat_dt_u(j3)+w4*dhat_dt_u(j4)+ &
                         w5*dhat_dt_u(j5)+w6*dhat_dt_u(j6)+ &
                         w7*dhat_dt_u(j7)+w8*dhat_dt_u(j8))*time_srw
              valv=valv+(w1*dhat_dt_v(j1)+w2*dhat_dt_v(j2)+ &
                         w3*dhat_dt_v(j3)+w4*dhat_dt_v(j4)+ &
                         w5*dhat_dt_v(j5)+w6*dhat_dt_v(j6)+ &
                         w7*dhat_dt_v(j7)+w8*dhat_dt_v(j8))*time_srw
              facu=facu+(w1*xhat_dt_u(j1)+w2*xhat_dt_u(j2)+ &
                         w3*xhat_dt_u(j3)+w4*xhat_dt_u(j4)+ &
                         w5*xhat_dt_u(j5)+w6*xhat_dt_u(j6)+ &
                         w7*xhat_dt_u(j7)+w8*xhat_dt_u(j8))*time_srw
              facv=facv+(w1*xhat_dt_v(j1)+w2*xhat_dt_v(j2)+ &
                         w3*xhat_dt_v(j3)+w4*xhat_dt_v(j4)+ &
                         w5*xhat_dt_v(j5)+w6*xhat_dt_v(j6)+ &
                         w7*xhat_dt_v(j7)+w8*xhat_dt_v(j8))*time_srw
           end if

           valsrw1=bigu11*valu+bigu12*valv
           valsrw2=bigu21*valu+bigu22*valv
           facsrw1=bigu11*facu+bigu12*facv-srwptr%res1
           facsrw2=bigu21*facu+bigu22*facv-srwptr%res2

           do kk=1,nstep 
              uu=facsrw1+sges(kk)*valsrw1
              vv=facsrw2+sges(kk)*valsrw2
              pen(kk)= (uu*uu+vv*vv)*srwptr%err2
           end do
        else
           pen(1)=(srwptr%res1*srwptr%res1+srwptr%res2*srwptr%res2)*srwptr%err2
        end if

!  Modify penalty term if nonlinear QC
        if (nlnqc_iter .and. srwptr%pg > tiny_r_kind .and.  &
                             srwptr%b  > tiny_r_kind) then
           pg_srw=srwptr%pg*varqc_iter
           cg_srw=cg_term/srwptr%b
           wnotgross= one-pg_srw
           wgross = pg_srw*cg_srw/wnotgross
           do kk=1,max(1,nstep)
              pen(kk)= -two*log((exp(-half*pen(kk)) + wgross)/(one+wgross))
           end do
        endif

        out(1) = out(1)+pen(1)*srwptr%raterr2
        do kk=2,nstep
           out(kk) = out(kk)+(pen(kk)-pen(1))*srwptr%raterr2
        end do
     end if

     srwptr => srwptr%llpoint

  end do
  return
end subroutine stpsrw

end module stpsrwmod

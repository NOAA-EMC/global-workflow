module stptmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   stptmod    module for stpt and its tangent linear stpt_tl
!  prgmmr:
!
! abstract: module for stpt and its tangent linear stpt_tl
!
! program history log:
!   2005-05-19  Yanqiu zhu - wrap stpt and its tangent linear stpt_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2008-12-02  Todling - remove stpt_tl
!   2009-08-12  lueken - update documentation
!   2013-10-28  todling - rename p3d to prse
!   2014-04-12       su - add non linear qc from Purser's scheme
!   2015-02-26       su - add njqc as an option to choos new non linear qc
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!   2019-09-20  Su      - remove current VQC part and add VQC subroutine call
!
! subroutines included:
!   sub stpt
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
implicit none

PRIVATE
PUBLIC stpt

contains

subroutine stpt(thead,dval,xval,out,sges,nstep,rpred,spred)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpt        calculate penalty and contribution to stepsize
!                            from temperatures, using non-linear qc
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: calculate penalty and contribution to stepsize from temperatures,
!              using nonlinear qc.
!
! program history log:
!   1991-02-26  derber
!   1998-02-03  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-05  parrish - add non-linear qc option
!   2005-04-11  treadon - merge stpt and stpt_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2005-10-21  su      - modify for variational qc
!   2005-12-20  parrish - add code to enable boundary layer forward model option
!   2007-03-19  tremolet - binning of observations
!   2007-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2006-07-28  derber  - modify output for b1 and b3 and add sensible temperature
!   2007-06-04  derber  - use quad precision to get reproducability over number of processors
!   2008-06-02  safford - rm unused var and uses
!   2008-12-03  todling - changed handling of ptr%time
!   2010-01-04  zhang,b - bug fix: accumulate penalty for multiple obs bins
!   2010-03-25  zhu     - use state_vector in the interface;
!                       - add handling of sst case; add pointer_state
!   2010-05-13  todling - update to use gsi_bundle
!                       - on-the-spot handling of non-essential vars
!   2013-05-23  zhu     - add search direction for aircraft data bias predictors
!   2013-10-29  todling - tendencies now in bundle
!   2015-12-21  yang    - Parrish's correction to the previous code in new varqc.
!
!   input argument list:
!     thead
!     rt       - search direction for sensible t
!     st       - analysis increment for sensible t
!     rtv      - search direction for virtual t
!     stv      - analysis increment for virtual t
!     rq       - search direction for q
!     sq       - analysis increment for q
!     ru       - search direction for u
!     su       - analysis increment for u
!     rv       - search direction for v
!     sv       - analysis increment for v
!     rp       - search direction for p
!     sp       - analysis increment for p
!     rsst     - search direction for sst
!     ssst     - analysis increment for sst
!     sges     - step size estimates (nstep)
!     nstep    - number of stepsizes (==0 means use outer iteration values)
!     rpred    - search direction for predictors
!     spred    - input predictor values
!                                         
!   output argument list:         
!     out(1:nstep)   - penalty from temperature observations sges(1:nstep)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use qcmod, only: nlnqc_iter,varqc_iter,njqc,vqc,nvqc
  use constants, only: zero,half,one,two,tiny_r_kind,cg_term,zero_quad,r3600
  use aircraftinfo, only: npredt,ntail,aircraft_t_bc_pof,aircraft_t_bc
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use m_obsNode, only: obsNode
  use m_tNode  , only: tNode
  use m_tNode  , only: tNode_typecast
  use m_tNode  , only: tNode_nextcast
  implicit none

! Declare passed variables
  class(obsNode), pointer             ,intent(in   ) :: thead
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_quad),dimension(max(1,nstep)),intent(inout) :: out
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges
  real(r_kind),dimension(npredt,ntail),optional,intent(in   ) :: rpred,spred
  type(gsi_bundle),intent(in) :: dval
  type(gsi_bundle),intent(in) :: xval

! Declare local variables
  integer(i_kind) ier,istatus,isst
  integer(i_kind) j1,j2,j3,j4,j5,j6,j7,j8,kk,n,ix
  real(r_kind) w1,w2,w3,w4,w5,w6,w7,w8
  real(r_kind) cg_t,val,val2,t_pg,var_jb
  real(r_kind),dimension(max(1,nstep))::pen,tt
  real(r_kind) tg_prime,valq,valq2,valp,valp2,valu,valu2
  real(r_kind) ts_prime,valv,valv2,valsst,valsst2
  real(r_kind) qs_prime
  real(r_kind) us_prime
  real(r_kind) vs_prime
  real(r_kind) psfc_prime
  integer(i_kind) ibb,ikk
  type(tNode), pointer :: tptr
  real(r_kind),pointer,dimension(:) :: rt,st,rtv,stv,rq,sq,ru,su,rv,sv
  real(r_kind),pointer,dimension(:) :: rsst,ssst
  real(r_kind),pointer,dimension(:) :: rp,sp

  out=zero_quad

!  If no t data return
  if(.not. associated(thead))return

! Retrieve pointers
  ier=0; isst=0
  call gsi_bundlegetpointer(xval,'u',   su, istatus);ier=istatus+ier
  call gsi_bundlegetpointer(xval,'v',   sv, istatus);ier=istatus+ier
  call gsi_bundlegetpointer(xval,'tsen',st, istatus);ier=istatus+ier
  call gsi_bundlegetpointer(xval,'tv',  stv,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(xval,'q',   sq, istatus);ier=istatus+ier
  call gsi_bundlegetpointer(xval,'prse',sp, istatus);ier=istatus+ier
  call gsi_bundlegetpointer(xval,'sst',ssst,istatus);isst=istatus+isst
  if(ier/=0)return

  call gsi_bundlegetpointer(dval,'u',   ru, istatus);ier=istatus+ier
  call gsi_bundlegetpointer(dval,'v',   rv, istatus);ier=istatus+ier
  call gsi_bundlegetpointer(dval,'tsen',rt, istatus);ier=istatus+ier
  call gsi_bundlegetpointer(dval,'tv',  rtv,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(dval,'q',   rq, istatus);ier=istatus+ier
  call gsi_bundlegetpointer(dval,'prse',rp, istatus);ier=istatus+ier
  call gsi_bundlegetpointer(dval,'sst',rsst,istatus);isst=istatus+isst
  if(ier/=0)return

  tptr => tNode_typecast(thead)
  do while (associated(tptr))

     if(tptr%luse)then
        if(nstep > 0)then
           j1=tptr%ij(1)
           j2=tptr%ij(2)
           j3=tptr%ij(3)
           j4=tptr%ij(4)
           j5=tptr%ij(5)
           j6=tptr%ij(6)
           j7=tptr%ij(7)
           j8=tptr%ij(8)
           w1=tptr%wij(1)
           w2=tptr%wij(2)
           w3=tptr%wij(3)
           w4=tptr%wij(4)
           w5=tptr%wij(5)
           w6=tptr%wij(6)
           w7=tptr%wij(7)
           w8=tptr%wij(8)

           if(tptr%tv_ob)then
              val= w1*rtv(j1)+w2*rtv(j2)+w3*rtv(j3)+w4*rtv(j4)+ &
                   w5*rtv(j5)+w6*rtv(j6)+w7*rtv(j7)+w8*rtv(j8)
 
              val2=w1*stv(j1)+w2*stv(j2)+w3*stv(j3)+w4*stv(j4)+ &
                   w5*stv(j5)+w6*stv(j6)+w7*stv(j7)+w8*stv(j8)
           else
              val= w1* rt(j1)+w2* rt(j2)+w3* rt(j3)+w4* rt(j4)+ &
                   w5* rt(j5)+w6* rt(j6)+w7* rt(j7)+w8* rt(j8)
              val2=w1* st(j1)+w2* st(j2)+w3* st(j3)+w4* st(j4)+ &
                   w5* st(j5)+w6* st(j6)+w7* st(j7)+w8* st(j8)
           end if

!          contribution from bias correction
           if ((aircraft_t_bc_pof .or. aircraft_t_bc) .and. tptr%idx>0) then
              ix=tptr%idx
              do n=1,npredt
                 val2=val2+spred(n,ix)*tptr%pred(n)
                 val =val +rpred(n,ix)*tptr%pred(n)
              end do 
           end if


           if(tptr%use_sfc_model) then

              if (isst==0) then
                 valsst =w1*rsst(j1)+w2*rsst(j2)+w3*rsst(j3)+w4*rsst(j4)
                 valsst2=w1*ssst(j1)+w2*ssst(j2)+w3*ssst(j3)+w4*ssst(j4)
              else
                 valsst =zero
                 valsst2=zero
              end if
              valq =w1* rq(j1)+w2* rq(j2)+w3* rq(j3)+w4* rq(j4)
              valq2=w1* sq(j1)+w2* sq(j2)+w3* sq(j3)+w4* sq(j4)
              valu =w1* ru(j1)+w2* ru(j2)+w3* ru(j3)+w4* ru(j4)
              valu2=w1* su(j1)+w2* su(j2)+w3* su(j3)+w4* su(j4)
              valv =w1* rv(j1)+w2* rv(j2)+w3* rv(j3)+w4* rv(j4)
              valv2=w1* sv(j1)+w2* sv(j2)+w3* sv(j3)+w4* sv(j4)
              valp =w1* rp(j1)+w2* rp(j2)+w3* rp(j3)+w4* rp(j4)
              valp2=w1* sp(j1)+w2* sp(j2)+w3* sp(j3)+w4* sp(j4)

              do kk=1,nstep
                 ts_prime=val2+sges(kk)*val
                 tg_prime=valsst2+sges(kk)*valsst
                 qs_prime=valq2+sges(kk)*valq
                 us_prime=valu2+sges(kk)*valu
                 vs_prime=valv2+sges(kk)*valv
                 psfc_prime=valp2+sges(1)*valp

                 tt(kk)=psfc_prime*tptr%tlm_tsfc(1) + tg_prime*tptr%tlm_tsfc(2) + &
                        ts_prime  *tptr%tlm_tsfc(3) + qs_prime*tptr%tlm_tsfc(4) + &
                        us_prime  *tptr%tlm_tsfc(5) + vs_prime*tptr%tlm_tsfc(6) - &
                        tptr%res
              end do

           else

              do kk=1,nstep
                 tt(kk)=val2+sges(kk)*val-tptr%res
              end do

           end if
 
        else
           tt(1)=tptr%res
        end if

        do kk=1,max(1,nstep)
           pen(kk) = tt(kk)*tt(kk)*tptr%err2
        end do

!  Modify penalty term if nonlinear QC
! EC VQc
        if (vqc .and. nlnqc_iter .and. tptr%pg > tiny_r_kind &
            .and. tptr%b >tiny_r_kind) then
           t_pg=tptr%pg*varqc_iter
           cg_t=cg_term/tptr%b
        else
           t_pg=zero
           cg_t=zero
        endif


!  Jim Purse's non linear QC scheme
        if(njqc .and. tptr%jb  > tiny_r_kind .and. tptr%jb <10.0_r_kind) then
           var_jb =tptr%jb
        else
           var_jb=zero
        endif
        
!  mix model VQC
       if(nvqc .and. tptr%ib >0) then
          ibb=tptr%ib
          ikk=tptr%ik
       else
          ibb=0
          ikk=0
       endif


       call vqc_stp(pen,nstep,t_pg,cg_t,var_jb,ibb,ikk)

       out(1) = out(1)+pen(1)*tptr%raterr2
       do kk=2,nstep
          out(kk) = out(kk)+(pen(kk)-pen(1))*tptr%raterr2
       end do

     endif
     tptr => tNode_nextcast(tptr)

  end do
  return
end subroutine stpt

end module stptmod

module intsrwmod
!$$$ module documentation block
!           .      .    .                                       .
! module:   intsrwmod    module for intsrw and its tangent linear intsrw_tl
!   prgmmr:
!
! abstract: module for intsrw and its tangent linear intsrw_tl
!
! program history log:
!   2005-05-12  Yanqiu zhu - wrap intsrw and its tangent linear intsrw_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2008-11-26  Todling - remove intsrw_tl; add interface back
!   2009-08-13  lueken - update documentation
!   2012-09-14  Syed RH Rizvi, NCAR/NESL/MMM/DAS  - implemented obs adjoint test  
!
! subroutines included:
!   sub intsrw_
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

PRIVATE
PUBLIC intsrw

interface intsrw; module procedure &
          intsrw_
end interface

contains

subroutine intsrw_(srwhead,rval,sval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intsrw      apply nonlin qc operator for radar superob winds
!   prgmmr: parrish          org: np22                date: 2004-06-22
!
! abstract: apply radar superob wind operator with nonlinear qc operator
!
! program history log:
!   2004-06-22  parrish, document
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-09  parrish - add nonlinear qc option
!   2005-03-01  parrish - nonlinear qc change to account for inflated obs error
!   2005-04-11  treadon - merge intsrw and intsrw_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2007-02-15  rancic  - add foto
!   2007-03-19  tremolet - binning of observations
!   2007-06-05  tremolet - use observation diagnostics structure
!   2007-07-09  tremolet - observation sensitivity
!   2008-01-04  tremolet - Don't apply H^T if l_do_adjoint is false
!   2008-11-28  todling  - turn FOTO optional; changed ptr%time handle
!   2010-05-13  todling  - update to use gsi_bundle; udpate interface
!   2012-09-14  Syed RH Rizvi, NCAR/NESL/MMM/DAS  - introduced ladtest_obs         
!
!   input argument list:
!     srwhead
!     su       - u increment in grid space
!     sv       - v increment in grid space
!     ru
!     rv
!
!   output argument list:
!     ru       - u results from observation operator 
!     rv       - v results from observation operator 
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: half,one,tiny_r_kind,cg_term,r3600
  use obsmod, only: srw_ob_type,lsaveobsens,l_do_adjoint,luse_obsdiag
  use qcmod, only: nlnqc_iter,varqc_iter
  use gridmod, only: latlon1n
  use jfunc, only: jiter,l_foto,xhat_dt,dhat_dt
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_4dvar, only: ladtest_obs
  implicit none

! Declare passed variables
  type(srw_ob_type),pointer,intent(in   ) :: srwhead
  type(gsi_bundle),         intent(in   ) :: sval
  type(gsi_bundle),         intent(inout) :: rval

! Declare local variables  
  integer(i_kind) ier,istatus
  integer(i_kind) i1,i2,i3,i4,i5,i6,i7,i8
! real(r_kind) penalty
  real(r_kind) valu,valv,w1,w2,w3,w4,w5,w6,w7,w8,valsrw1,valsrw2
  real(r_kind) bigu11,bigu21,bigu12,bigu22,time_srw
  real(r_kind) cg_srw,p0,gradsrw1,gradsrw2,wnotgross,wgross,term,pg_srw
  real(r_kind),pointer,dimension(:) :: xhat_dt_u,xhat_dt_v
  real(r_kind),pointer,dimension(:) :: dhat_dt_u,dhat_dt_v
  real(r_kind),pointer,dimension(:) :: su,sv
  real(r_kind),pointer,dimension(:) :: ru,rv
  type(srw_ob_type), pointer :: srwptr

!  If not srw data return
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
     i1=srwptr%ij(1)
     i2=srwptr%ij(2)
     i3=srwptr%ij(3)
     i4=srwptr%ij(4)
     i5=srwptr%ij(5)
     i6=srwptr%ij(6)
     i7=srwptr%ij(7)
     i8=srwptr%ij(8)
     w1=srwptr%wij(1)
     w2=srwptr%wij(2)
     w3=srwptr%wij(3)
     w4=srwptr%wij(4)
     w5=srwptr%wij(5)
     w6=srwptr%wij(6)
     w7=srwptr%wij(7)
     w8=srwptr%wij(8)

!    Forward model
     bigu11=srwptr%rsrw(1)
     bigu21=srwptr%rsrw(2)
     bigu12=srwptr%rsrw(3)
     bigu22=srwptr%rsrw(4)
     valu=w1* su(i1)+w2* su(i2)+w3* su(i3)+w4* su(i4)+&
          w5* su(i5)+w6* su(i6)+w7* su(i7)+w8* su(i8)
     valv=w1* sv(i1)+w2* sv(i2)+w3* sv(i3)+w4* sv(i4)+&
          w5* sv(i5)+w6* sv(i6)+w7* sv(i7)+w8* sv(i8)
     if ( l_foto ) then
        time_srw=srwptr%time*r3600
        valu=valu+&
          (w1*xhat_dt_u(i1)+w2*xhat_dt_u(i2)+ &
           w3*xhat_dt_u(i3)+w4*xhat_dt_u(i4)+ &
           w5*xhat_dt_u(i5)+w6*xhat_dt_u(i6)+ &
           w7*xhat_dt_u(i7)+w8*xhat_dt_u(i8))*time_srw
        valv=valv+&
          (w1*xhat_dt_v(i1)+w2*xhat_dt_v(i2)+ &
           w3*xhat_dt_v(i3)+w4*xhat_dt_v(i4)+ &
           w5*xhat_dt_v(i5)+w6*xhat_dt_v(i6)+ &
           w7*xhat_dt_v(i7)+w8*xhat_dt_v(i8))*time_srw
     endif

     valsrw1=bigu11*valu+bigu12*valv
     valsrw2=bigu21*valu+bigu22*valv

     if (lsaveobsens) then
        srwptr%diagu%obssen(jiter) = valsrw1*srwptr%raterr2*srwptr%err2
        srwptr%diagv%obssen(jiter) = valsrw2*srwptr%raterr2*srwptr%err2
     else
        if (srwptr%luse) then
           srwptr%diagu%tldepart(jiter)=valsrw1
           srwptr%diagv%tldepart(jiter)=valsrw2
        endif
     endif

     if (l_do_adjoint) then
        if (lsaveobsens) then
           gradsrw1 = srwptr%diagu%obssen(jiter)
           gradsrw2 = srwptr%diagv%obssen(jiter)
 
        else
          if( ladtest_obs ) then
             valsrw1=valsrw1
             valsrw2=valsrw2
          else
             valsrw1=valsrw1-srwptr%res1
             valsrw2=valsrw2-srwptr%res2
          end if 

!          gradient of nonlinear operator
           if (nlnqc_iter .and. srwptr%pg > tiny_r_kind .and.  &
                                srwptr%b  > tiny_r_kind) then
              pg_srw=srwptr%pg*varqc_iter
              cg_srw=cg_term/srwptr%b
              wnotgross= one-pg_srw
              wgross = pg_srw*cg_srw/wnotgross
              p0   = wgross/(wgross+exp(-half*srwptr%err2*(valsrw1**2+valsrw2**2)))
              term = (one-p0)
              valsrw1=valsrw1*term
              valsrw2=valsrw2*term
           endif

           if( ladtest_obs)  then
              gradsrw1 = valsrw1
              gradsrw2 = valsrw2
           else
              gradsrw1 = valsrw1*srwptr%raterr2*srwptr%err2
              gradsrw2 = valsrw2*srwptr%raterr2*srwptr%err2
           end if
        endif

        valu=bigu11*gradsrw1+bigu21*gradsrw2
        valv=bigu12*gradsrw1+bigu22*gradsrw2
 
!       Adjoint
        ru(i1)=ru(i1)+w1*valu
        ru(i2)=ru(i2)+w2*valu
        ru(i3)=ru(i3)+w3*valu
        ru(i4)=ru(i4)+w4*valu
        ru(i5)=ru(i5)+w5*valu
        ru(i6)=ru(i6)+w6*valu
        ru(i7)=ru(i7)+w7*valu
        ru(i8)=ru(i8)+w8*valu
        rv(i1)=rv(i1)+w1*valv
        rv(i2)=rv(i2)+w2*valv
        rv(i3)=rv(i3)+w3*valv
        rv(i4)=rv(i4)+w4*valv
        rv(i5)=rv(i5)+w5*valv
        rv(i6)=rv(i6)+w6*valv
        rv(i7)=rv(i7)+w7*valv
        rv(i8)=rv(i8)+w8*valv

        valu=valu*time_srw
        valv=valv*time_srw
        if ( l_foto ) then
           dhat_dt_u(i1)=dhat_dt_u(i1)+w1*valu
           dhat_dt_u(i2)=dhat_dt_u(i2)+w2*valu
           dhat_dt_u(i3)=dhat_dt_u(i3)+w3*valu
           dhat_dt_u(i4)=dhat_dt_u(i4)+w4*valu
           dhat_dt_u(i5)=dhat_dt_u(i5)+w5*valu
           dhat_dt_u(i6)=dhat_dt_u(i6)+w6*valu
           dhat_dt_u(i7)=dhat_dt_u(i7)+w7*valu
           dhat_dt_u(i8)=dhat_dt_u(i8)+w8*valu
           dhat_dt_v(i1)=dhat_dt_v(i1)+w1*valv
           dhat_dt_v(i2)=dhat_dt_v(i2)+w2*valv
           dhat_dt_v(i3)=dhat_dt_v(i3)+w3*valv
           dhat_dt_v(i4)=dhat_dt_v(i4)+w4*valv
           dhat_dt_v(i5)=dhat_dt_v(i5)+w5*valv
           dhat_dt_v(i6)=dhat_dt_v(i6)+w6*valv
           dhat_dt_v(i7)=dhat_dt_v(i7)+w7*valv
           dhat_dt_v(i8)=dhat_dt_v(i8)+w8*valv
        endif
     endif

     srwptr => srwptr%llpoint

  end do
  return
end subroutine intsrw_

end module intsrwmod

module intaodmod
!$$$ module documentation block
!                .      .    .                                       .
! module:    intaodmod    module for intaod and its tangent linear intaod_tl
!   prgmmr: hclin            org: ncar/mmm         date: 2010-10-20
!
! abstract: module for intaod
!
! program history log:
!   2010-10-20  hclin   - modified from intrad for total aod
!
! subroutines included:
!   sub intaod_
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none

  private
  public :: intaod

  interface intaod; module procedure &
            intaod_
  end interface

contains

  subroutine intaod_(aerohead,rval,sval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intaod
!   prgmmr: hclin          org: ncar/mmm                date: 2010-10-20
!
! abstract: apply aod operator and adjoint
!           nonlinear qc is not implemented yet.
!
! program history log:
!   2010-10-20  hclin   - modified from intrad for total aod
!   2014-12-03  derber  - modify so that use of obsdiags can be turned off
!
!   input argument list:
!     aerohead  - obs type pointer to obs structure
!     rval
!     sval
!
!   output argument list:
!     rval
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use kinds, only: r_kind,i_kind,r_quad
    use aeroinfo, only: aerojacnames,aerojacindxs,nsigaerojac
    use state_vectors, only: svars
    use obsmod, only: aero_ob_type,lsaveobsens,l_do_adjoint,luse_obsdiag
    use jfunc, only: jiter,l_foto,xhat_dt,dhat_dt
    use gridmod, only: latlon11,latlon1n,nsig
    use qcmod, only: nlnqc_iter,varqc_iter
    use constants, only: zero,half,one,tiny_r_kind,cg_term,r3600
    use gsi_bundlemod, only: gsi_bundle
    use gsi_bundlemod, only: gsi_bundlegetpointer
    use gsi_bundlemod, only: gsi_bundleputvar
    use gsi_chemguess_mod, only: gsi_chemguess_get
    use mpeu_util, only: getindex
    use mpimod, only: mype
    implicit none

! Declare passed variables
    type(aero_ob_type),pointer,intent(in) :: aerohead
    type(gsi_bundle), intent(in   ) :: sval
    type(gsi_bundle), intent(inout) :: rval

! Declare local variables
    integer(i_kind) j1,j2,j3,j4,i1,i2,i3,i4,n,k,ic,nn
    integer(i_kind) istatus,naero
    integer(i_kind),dimension(nsig) :: i1n,i2n,i3n,i4n
    real(r_kind) val
    real(r_kind) w1,w2,w3,w4
!   real(r_kind) cg_aero,p0,wnotgross,wgross
    type(aero_ob_type), pointer :: aeroptr

    real(r_kind),pointer,dimension(:) :: sv_chem
    real(r_kind),pointer,dimension(:) :: rv_chem
    real(r_kind),dimension(nsigaerojac) :: tval,tdir

  ! Inquire about chemistry
    naero = size(aerojacnames)
    if ( naero <= 0 ) return

    aeroptr => aerohead
    do while (associated(aeroptr))
       j1=aeroptr%ij(1)
       j2=aeroptr%ij(2)
       j3=aeroptr%ij(3)
       j4=aeroptr%ij(4)
       w1=aeroptr%wij(1)
       w2=aeroptr%wij(2)
       w3=aeroptr%wij(3)
       w4=aeroptr%wij(4)

       do k=1,nsigaerojac
          tval(k)=zero
       end do

!  Begin Forward model
!  calculate temperature, q, ozone, sst vector at observation location
       i1n(1) = j1
       i2n(1) = j2
       i3n(1) = j3
       i4n(1) = j4
       do k=2,nsig
          i1n(k) = i1n(k-1)+latlon11
          i2n(k) = i2n(k-1)+latlon11
          i3n(k) = i3n(k-1)+latlon11
          i4n(k) = i4n(k-1)+latlon11
       enddo
       do ic = 1, naero
          call gsi_bundlegetpointer (sval,trim(aerojacnames(ic)),sv_chem,istatus)
          do k=1,nsig
             i1 = i1n(k)
             i2 = i2n(k)
             i3 = i3n(k)
             i4 = i4n(k)
             tdir(k+nsig*(ic-1))=  w1* sv_chem(i1)+w2* sv_chem(i2)+ &
                                   w3* sv_chem(i3)+w4* sv_chem(i4)
          end do
          nullify(sv_chem)
       end do

!  begin channel specific calculations
       do nn=1,aeroptr%nlaero
          ic=aeroptr%icx(nn)

!       include observation increment
          val=zero

!       Include contributions from atmospheric jacobian
          do k=1,nsigaerojac
             val=val+tdir(k)*aeroptr%daod_dvar(k,nn)
          end do

          if(luse_obsdiag)then
             if (lsaveobsens) then
                val = val*aeroptr%err2(nn)*aeroptr%raterr2(nn)
                aeroptr%diags(nn)%ptr%obssen(jiter) = val
             else
                if (aeroptr%luse) aeroptr%diags(nn)%ptr%tldepart(jiter) = val
             endif
          endif

          if (l_do_adjoint) then
             if ( .not. lsaveobsens) then

                val=val-aeroptr%res(nn)

!             Multiply by variance.
              !if (nlnqc_iter .and. pg_aero(ic) > tiny_r_kind .and. &
              !                     b_aero(ic)  > tiny_r_kind) then
              !   cg_aero=cg_term/b_aero(ic)
              !   wnotgross= one-pg_aero(ic)*varqc_iter
              !   wgross = varqc_iter*pg_aero(ic)*cg_aero/wnotgross
              !   p0   = wgross/(wgross+exp(-half*aeroptr%err2(nn)*val*val))
              !   val = val*(one-p0)
              !endif

                val = val*aeroptr%err2(nn)*aeroptr%raterr2(nn)
             endif

!          Begin adjoint

!          Extract contributions from atmospheric jacobian
             do k=1,nsigaerojac
                tval(k)=tval(k)+aeroptr%daod_dvar(k,nn)*val
             end do
 
          endif
       end do

       if (l_do_adjoint) then
!    Distribute adjoint contributions over surrounding grid points
 
          do ic = 1, naero
             call gsi_bundlegetpointer (rval,trim(aerojacnames(ic)),rv_chem,istatus)
             if ( istatus /= 0 ) then
                write(6,*) 'error gsi_bundlegetpointer in intaod for ', aerojacnames(ic)
             end if
             do k=1,nsig
                n = k + nsig*(ic-1)
                i1 = i1n(k)
                i2 = i2n(k)
                i3 = i3n(k)
                i4 = i4n(k)
                rv_chem(i1) = rv_chem(i1) + w1*tval(n)
                rv_chem(i2) = rv_chem(i2) + w2*tval(n)
                rv_chem(i3) = rv_chem(i3) + w3*tval(n)
                rv_chem(i4) = rv_chem(i4) + w4*tval(n)
             end do
             nullify(rv_chem)
          end do
       endif ! < l_do_adjoint >

       aeroptr => aeroptr%llpoint
!       call stop2(999)
    end do

    return
  end subroutine intaod_

end module intaodmod

module intdbzmod
!$$$ module documentation block
!           .      .    .                                       .
! module:   intdbzmod    module for intdbz and its tangent linear intdbz_tl
!   prgmmr:
!
! abstract: module for intdbz and its tangent linear intdbz_tl
!
! program history log:
! 2017-05-12 Y. Wang and X. Wang - add tangent linear of dbz operator to directly assimilate reflectivity
!                                  for both ARW and NMMB models (Wang and Wang 2017 MWR). POC: xuguang.wang@ou.edu
!   2019-02-19  CAPS(C. Tong)  - modified to comply with new type structure
!   2019-07-11  todling - introduced wrf_vars_mod
!   2021-02-08  CAPS(C. Liu)   - add qnr required for TM operator
!   2021-02-08  CAPS(J. Park)  - add 'fv3_regional' flag for FV3 regional domain
!   support
!
! subroutines included:
!   sub intdbz_
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use m_obsNode, only: obsNode
use m_dbzNode, only: dbzNode
use m_dbzNode, only: dbzNode_typecast
use m_dbzNode, only: dbzNode_nextcast
use m_obsdiagNode, only: obsdiagNode_set
implicit none

PRIVATE
PUBLIC intdbz

interface intdbz; module procedure &
          intdbz_
end interface

contains

subroutine intdbz_(dbzhead,rval,sval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intdbz       apply nonlin qc operator for radar reflectivity
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: apply observation operator for radar winds
!             with nonlinear qc operator
!
! program history log:
!   1991-02-26  derber
!   1999-11-22  yang
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-08  parrish - add nonlinear qc option
!   2005-03-01  parrish - nonlinear qc change to account for inflated obs error
!   2005-04-11  treadon - merge intdbz and intdbz_qc into single routine
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
!   2010-05-13  todlng   - update to use gsi_bundle; update interface
!   2012-09-14  Syed RH Rizvi, NCAR/NESL/MMM/DAS  - introduced ladtest_obs         
!   2014-12-03  derber  - modify so that use of obsdiags can be turned off
!   2016-09-xx  G.Zhao   - intdbzmod is based on intqmod, and intrwmod
!                        - using tangent linear dbz operator 
!                        - working with log(qr/qs/qg) (no modification for intdbz)
!
!   input argument list:
!     dbzhead   - obs type pointer to obs structure     
!     sqr       - current qr solution increment
!     sqs       - current qs solution increment
!     sqg       - current qg solution increment
!     sqnr      - current qnr solution increment
!     rqr
!     rqs
!     rqg
!
!   output argument list:
!     rqr        - qr results from dbz observation operator
!     rqs        - qs results from dbz observation operator
!     rqg        - qg results from dbz observation operator
!     rqnr       - qnr results from dbz observation operator
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: half,one,tiny_r_kind,cg_term,r3600
  use obsmod, only: lsaveobsens,l_do_adjoint,luse_obsdiag
  use qcmod, only: nlnqc_iter,varqc_iter
  use gridmod, only: wrf_mass_regional, fv3_regional
  use jfunc, only: jiter
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_4dvar, only: ladtest_obs
  use directDA_radaruse_mod, only: l_use_dbz_directDA
  use radarz_cst, only: mphyopt
  use wrf_vars_mod, only : dbz_exist
  implicit none

! Declare passed variables
  class(obsNode), pointer, intent(in   ) :: dbzhead
  type(gsi_bundle),        intent(in   ) :: sval
  type(gsi_bundle),        intent(inout) :: rval

! Declare local variables
  integer(i_kind) j1,j2,j3,j4,j5,j6,j7,j8,ier,istatus
! real(r_kind) penalty
  real(r_kind) val,w1,w2,w3,w4,w5,w6,w7,w8,valqr,valqs,valqg,valdbz,valqnr
  real(r_kind) cg_dbz,p0,grad,wnotgross,wgross,pg_dbz
  real(r_kind) qrtl,qstl, qgtl, qnrtl
  real(r_kind),pointer,dimension(:) :: sqr,sqs,sqg,sdbz,sqnr
  real(r_kind),pointer,dimension(:) :: rqr,rqs,rqg,rdbz,rqnr
  type(dbzNode), pointer :: dbzptr

!  If no dbz obs type data return
  if(.not. associated(dbzhead))return

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  if(dbz_exist)then
    call gsi_bundlegetpointer(sval,'dbz',sdbz,istatus);ier=istatus+ier
    call gsi_bundlegetpointer(rval,'dbz',rdbz,istatus);ier=istatus+ier
  else
    call gsi_bundlegetpointer(sval,'qr',sqr,istatus);ier=istatus+ier

    if (wrf_mass_regional .or. fv3_regional) then
      call gsi_bundlegetpointer(sval,'qs',sqs,istatus);ier=istatus+ier
      call gsi_bundlegetpointer(sval,'qg',sqg,istatus);ier=istatus+ier

      ! direct reflectivity DA/ TM operator also uses qnr
      if ( mphyopt == 108 .and. l_use_dbz_directDA ) then
         call gsi_bundlegetpointer(sval,'qnr',sqnr,istatus);ier=istatus+ier
      end if
    end if


    call gsi_bundlegetpointer(rval,'qr',rqr,istatus);ier=istatus+ier
    if (wrf_mass_regional .or. fv3_regional) then
      call gsi_bundlegetpointer(rval,'qs',rqs,istatus);ier=istatus+ier
      call gsi_bundlegetpointer(rval,'qg',rqg,istatus);ier=istatus+ier

      ! direct reflectivity DA/ TM operator also uses qnr
      if ( mphyopt == 108 .and. l_use_dbz_directDA ) then
         call gsi_bundlegetpointer(rval,'qnr',rqnr,istatus);ier=istatus+ier
      end if
    end if
  end if

  if(ier/=0)return


  dbzptr => dbzNode_typecast(dbzhead)
  do while (associated(dbzptr))
     j1=dbzptr%ij(1)
     j2=dbzptr%ij(2)
     j3=dbzptr%ij(3)
     j4=dbzptr%ij(4)
     j5=dbzptr%ij(5)
     j6=dbzptr%ij(6)
     j7=dbzptr%ij(7)
     j8=dbzptr%ij(8)
     w1=dbzptr%wij(1)
     w2=dbzptr%wij(2)
     w3=dbzptr%wij(3)
     w4=dbzptr%wij(4)
     w5=dbzptr%wij(5)
     w6=dbzptr%wij(6)
     w7=dbzptr%wij(7)
     w8=dbzptr%wij(8)


!    Forward model
     if( dbz_exist )then
       val = w1* sdbz(j1)+w2* sdbz(j2)+w3* sdbz(j3)+w4* sdbz(j4)+ &
             w5* sdbz(j5)+w6* sdbz(j6)+w7* sdbz(j7)+w8* sdbz(j8)
     else
       qrtl = w1* sqr(j1)+w2* sqr(j2)+w3* sqr(j3)+w4* sqr(j4)+      &
              w5* sqr(j5)+w6* sqr(j6)+w7* sqr(j7)+w8* sqr(j8)
       if ( wrf_mass_regional .or. fv3_regional )then
         qstl  = w1* sqs(j1)+w2* sqs(j2)+w3* sqs(j3)+w4* sqs(j4)+  &
                 w5* sqs(j5)+w6* sqs(j6)+w7* sqs(j7)+w8* sqs(j8)
          
         qgtl  = w1* sqg(j1)+w2* sqg(j2)+w3* sqg(j3)+w4* sqg(j4)+  &
                 w5* sqg(j5)+w6* sqg(j6)+w7* sqg(j7)+w8* sqg(j8)

         ! direct reflectivity DA/ TM operator also uses qnr
         if ( mphyopt == 108 .and. l_use_dbz_directDA ) then
            qnrtl  = w1* sqnr(j1)+w2* sqnr(j2)+w3* sqnr(j3)+w4* sqnr(j4)+      &
                     w5* sqnr(j5)+w6* sqnr(j6)+w7* sqnr(j7)+w8* sqnr(j8)
            val   = (dbzptr%jqr)*qrtl + (dbzptr%jqs)*qstl + (dbzptr%jqg)*qgtl +(dbzptr%jqnr)*qnrtl
         else ! Original calculation: qr, qs, and qg
            val   = (dbzptr%jqr)*qrtl + (dbzptr%jqs)*qstl + (dbzptr%jqg)*qgtl
         end if
       end if
  
     end if

     if(luse_obsdiag)then
        if (lsaveobsens) then
           grad = val*dbzptr%raterr2*dbzptr%err2
           !-- dbzptr%diags%obssen(jiter) = grad
           call obsdiagNode_set(dbzptr%diags,jiter=jiter,obssen=grad)

        else
           !-- if (dbzptr%luse) dbzptr%diags%tldepart(jiter)=val
           if (dbzptr%luse) call obsdiagNode_set(dbzptr%diags,jiter=jiter,tldepart=val)
        endif
     endif

     if (l_do_adjoint) then
        if (.not. lsaveobsens) then
           if( .not. ladtest_obs ) val=val-dbzptr%res

!          gradient of nonlinear operator
           if (nlnqc_iter .and. dbzptr%pg > tiny_r_kind .and. &
                                dbzptr%b  > tiny_r_kind) then
              pg_dbz=dbzptr%pg*varqc_iter
              cg_dbz=cg_term/dbzptr%b
              wnotgross= one-pg_dbz
              wgross = pg_dbz*cg_dbz/wnotgross
              p0   = wgross/(wgross+exp(-half*dbzptr%err2*val**2))
              val = val*(one-p0)
           endif

           if( ladtest_obs)  then
              grad = val
           else
              grad = val*dbzptr%raterr2*dbzptr%err2
           end if

        endif

!       Adjoint
        if(dbz_exist)then
          valdbz = grad
          rdbz(j1)=rdbz(j1)+w1*valdbz
          rdbz(j2)=rdbz(j2)+w2*valdbz
          rdbz(j3)=rdbz(j3)+w3*valdbz
          rdbz(j4)=rdbz(j4)+w4*valdbz
          rdbz(j5)=rdbz(j5)+w5*valdbz
          rdbz(j6)=rdbz(j6)+w6*valdbz
          rdbz(j7)=rdbz(j7)+w7*valdbz
          rdbz(j8)=rdbz(j8)+w8*valdbz
        else
          valqr = dbzptr%jqr*grad
          rqr(j1)=rqr(j1)+w1*valqr
          rqr(j2)=rqr(j2)+w2*valqr
          rqr(j3)=rqr(j3)+w3*valqr
          rqr(j4)=rqr(j4)+w4*valqr
          rqr(j5)=rqr(j5)+w5*valqr
          rqr(j6)=rqr(j6)+w6*valqr
          rqr(j7)=rqr(j7)+w7*valqr
          rqr(j8)=rqr(j8)+w8*valqr

          if ( wrf_mass_regional .or. fv3_regional )then
            valqs=dbzptr%jqs*grad
            valqg=dbzptr%jqg*grad

            rqs(j1)=rqs(j1)+w1*valqs
            rqs(j2)=rqs(j2)+w2*valqs
            rqs(j3)=rqs(j3)+w3*valqs
            rqs(j4)=rqs(j4)+w4*valqs
            rqs(j5)=rqs(j5)+w5*valqs
            rqs(j6)=rqs(j6)+w6*valqs
            rqs(j7)=rqs(j7)+w7*valqs
            rqs(j8)=rqs(j8)+w8*valqs

            rqg(j1)=rqg(j1)+w1*valqg
            rqg(j2)=rqg(j2)+w2*valqg
            rqg(j3)=rqg(j3)+w3*valqg
            rqg(j4)=rqg(j4)+w4*valqg
            rqg(j5)=rqg(j5)+w5*valqg
            rqg(j6)=rqg(j6)+w6*valqg
            rqg(j7)=rqg(j7)+w7*valqg
            rqg(j8)=rqg(j8)+w8*valqg

            ! direct Reflectivity DA/ TM operator also uses qnr
            if ( mphyopt == 108 .and. l_use_dbz_directDA ) then
               valqnr=dbzptr%jqnr*grad

               rqnr(j1)=rqnr(j1)+w1*valqnr
               rqnr(j2)=rqnr(j2)+w2*valqnr
               rqnr(j3)=rqnr(j3)+w3*valqnr
               rqnr(j4)=rqnr(j4)+w4*valqnr
               rqnr(j5)=rqnr(j5)+w5*valqnr
               rqnr(j6)=rqnr(j6)+w6*valqnr
               rqnr(j7)=rqnr(j7)+w7*valqnr
               rqnr(j8)=rqnr(j8)+w8*valqnr
            end if
          end if
        end if
 
     endif

     !dbzptr => dbzptr%llpoint
     dbzptr => dbzNode_nextcast(dbzptr)
  end do
  return
end subroutine intdbz_

end module intdbzmod

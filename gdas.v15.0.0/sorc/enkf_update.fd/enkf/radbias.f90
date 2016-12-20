module radbias
!$$$  module documentation block
!
! module: radbias                      routines for applying and updating
!                                      radiance bias correction.
!
! prgmmr: whitaker         org: esrl/psd               date: 2009-02-23
!
! abstract: apply and update radiance "air-mass" bias correction
!  coefficients following an approach suggested by Takemasa Miyoshi.
!  In this approach, the bias coefficient update and the model state
!  update are coupled through the bias coefficient increment and the
!  model state increment in observation space (i.e. the Hx increment).
!  The two coupled updates are solved iteratively (the number of
!  iterations is controlled by the namelist parameter numiter).  At
!  convergence, the solution should be the same as the VarBC (Dee
!  2004, reference below) update used in the GSI). 
!
! Public Subroutines:
!  apply_biascorr: apply bias correction to observation priors.
!  update_biascorr: update radiance bias correcton coefficients
!   using latest estimate of O-A (observations minus ensemble mean
!   in observation space), with a specified constant
!   background error covariance formulation following 
!   Dee 2004 "Variational bias correction of radiance data in the ECMWF
!   system" in ECMWF Workshop on Assimilation of high spectral resolution
!   sounders in NWP
!   (http://www.ecmwf.int/publications/library/do/references/list/17444)
!
! Private Subroutines:
!  symminv: Cholesky decomp inverse of a symmetric matrix using LAPACK.
!
! Public Variables: None
!
! program history log:
!   2009-02-23  Initial version.
!
! attributes:
!   language: f95
!
!$$$

use mpisetup
use kinds, only: r_kind,i_kind,r_double
use radinfo, only: &
npred,predx,nusis,nuchan,jpch_rad,adp_anglebc,varA,ostats,inew_rad,newpc4pred
use enkf_obsmod, only:  ensmean_ob, ensmean_obnobc, biaspreds, nobs_conv, nobs_oz,&
                        nobs_sat,  indxsat , deltapredx, oberrvarmean, numobspersat, &
                        obfit_post, oberrvar
use params, only : biasvar, numiter
use loadbal, only: nobs_max
use constants, only: zero,two,r10
implicit none

private
public :: apply_biascorr, update_biascorr

contains

subroutine apply_biascorr()
  ! apply bias correction to ob priors using latest estimate
  ! of bias coefficients (first-guess plus increment given by
  ! deltapredx).
  integer nn,np,nob
  nn  = 0
  do nob=nobs_conv+nobs_oz+1,nobs_conv+nobs_oz+nobs_sat
    nn = nn + 1
    if (indxsat(nn) == 0) cycle
    if (.not. adp_anglebc) then
       ! angle-dependent, non-adaptive correction.
       ensmean_ob(nob) = ensmean_obnobc(nob) + biaspreds(1,nn)
    else
       ! angle dependent correction is included in adaptive part.
       ensmean_ob(nob) = ensmean_obnobc(nob) 
    endif
    ! adaptive (air-mass) corrections.
    do np=1,npred
       ensmean_ob(nob) = ensmean_ob(nob) + &
       biaspreds(np+1,nn)*(predx(np,indxsat(nn))+deltapredx(np,indxsat(nn)))
    enddo
  enddo  
end subroutine apply_biascorr

subroutine update_biascorr(niter)
! compute analysis increment (deltapredx) for bias correction coefficients
! given latest esimate of observation increments (obs - ensemble mean at ob
! locations). Upgraded deltapredx broadcast to all tasks.
integer(i_kind) i,m,i1,i2,nn,n
real(r_kind) increment(npred),biaserrvar,a(npred,npred),atmp(npred,npred)
real(r_kind) inctmp(npred)
real(r_kind) buffertmp(npred,jpch_rad)
real(r_kind), allocatable, dimension(:,:) :: biaspredtmp
real(r_kind), allocatable, dimension(:) :: obinc
real(r_kind) deltapredx1(npred,jpch_rad)
real(r_double) t1
integer(i_kind), intent(in) :: niter
integer(i_kind) ierr
character(len=72) fmt
write(fmt, '("(i2,1x,i4,1x,a20,1x,i4,",I0,"(1x,e10.3))")') npred
! satellite bias correction update.
if (nobs_sat > 0) then
  if (nproc == 0) t1 = mpi_wtime() ! do this loop in parallel (a chunk of channels/sensors on each processor).
  i1 = nproc*real(jpch_rad/numproc) + 1 
  i2 = (nproc+1)*real(jpch_rad/numproc)   
  if (nproc == numproc-1) i2 = jpch_rad
  deltapredx1=0._r_kind
  do i=i1,i2
   allocate( biaspredtmp(npred,numobspersat(i)) )
   allocate( obinc(numobspersat(i)) )
   if (newpc4pred) then
       ! set variances for bias predictor coeff. based on diagonal info
       ! of previous analysis error variance. This code copied from berror.f90.
       do n=1,npred
          if (inew_rad(i)) then
             biaserrvar = 10000.0_r_kind
          else
             if (numobspersat(i) == 0) then 
                ! channel missing, increase to twice previous analysis cycle
                ! add 10**-6 to prevent vanishly small error variance
                varA(n,i)=two*varA(n,i)+1.0e-6_r_kind
                biaserrvar=varA(n,i)
             else
                biaserrvar=1.1_r_kind*varA(n,i)+1.0e-6_r_kind
             end if
             if (biaserrvar > r10) biaserrvar = r10
             if (varA(n,i)>10000.0_r_kind) varA(n,i)=10000.0_r_kind
          endif
       end do
   else
       if (biasvar < 0.) then
          ! if biasvar set < 0 in namelist, background err variance
          ! is inversely proportional to number of obs (with -biasvar
          ! as proportionality constant). Maximum allowed value 0.1.
          if (numobspersat(i) > int(-biasvar/0.1_r_kind)) then
             biaserrvar = -biasvar/numobspersat(i)
          else
             biaserrvar = 0.1_r_kind
          endif
          !if (niter .eq. 2) print *,'biaserrvar:',numobspersat(i),trim(adjustl(nusis(i))),nuchan(i),biaserrvar
       else
          biaserrvar = biasvar ! single constant value
       endif
   endif
   if (oberrvarmean(i) > 1.e10 .or. numobspersat(i) == 0) then
      deallocate(biaspredtmp,obinc)
      cycle
   endif
   ! compute B**-1 + p * R**-1 * pT
   a = 0._r_kind
   do n=1,npred
      nn = 0
      do m=1,nobs_sat
          ! only use the numobspersat(i) obs associated with this channel/instrument
          if (indxsat(m) == i) then
             nn = nn + 1
             biaspredtmp(n,nn) = biaspreds(n+1,m)/sqrt(oberrvar(nobs_conv+nobs_oz+m))
          end if
      enddo
      a(n,n) = 1._r_kind/biaserrvar
   enddo
   if (r_kind == kind(1.d0)) then
      call dgemm('N', 'T', npred, npred, nn, 1.d0, &
                 biaspredtmp, npred, biaspredtmp, npred, 0.d0, atmp, npred)
   else
      call sgemm('N', 'T', npred, npred, nn, 1.e0, &
                 biaspredtmp, npred, biaspredtmp, npred, 0.e0, atmp, npred)
   endif
   a = a + atmp
   ! compute inverse of symmetric matrix a = B**-1 + p * R**-1 * pT.
   call symminv(a,npred)
   ! a now contains analysis error covariance matrix (inverse of Hessian).
   ! update bias predictor variance info (varA) assuming a is diagonal.
   if (niter == numiter .and. newpc4pred) then
       do n=1,npred
          varA(n,i) = a(n,n)
       enddo
   endif
   ! p * R**-1
   do n=1,npred 
      nn = 0
      do m=1,nobs_sat
          if (indxsat(m) == i) then
             nn = nn + 1
             biaspredtmp(n,nn) = biaspreds(n+1,m)/oberrvar(nobs_conv+nobs_oz+m)
          end if
      enddo
   enddo
   nn = 0
   do m=1,nobs_sat
      if (indxsat(m) == i) then
         nn = nn + 1
         obinc(nn) = obfit_post(nobs_conv+nobs_oz+m)
      end if
   enddo
   ! update bias correction coefficients for this channel/sensor.
   ! b = b +  (B**-1 + p * R**-1 * pT)**-1 * (p * R**-1) * (y - Hx)
   if (r_kind == kind(1.d0)) then
      call dgemv('N',npred,numobspersat(i),1.d0,biaspredtmp,npred,obinc,1,0.d0,inctmp,1)
      call dgemv('N',npred,npred,1.d0,a,npred,inctmp,1,0.d0,increment,1)
   else
      call sgemv('N',npred,numobspersat(i),1.e0,biaspredtmp,npred,obinc,1,0.e0,inctmp,1)
      call sgemv('N',npred,npred,1.e0,a,npred,inctmp,1,0.e0,increment,1)
   endif
   ! bias correction increment.
   !deltapredx1(:,i) = increment 
   ! blend of previous iteration and new estimate to improve convergence.
   if (niter .eq. 1) then
      deltapredx1(:,i) = increment 
   else
      deltapredx1(:,i) = 0.5*(deltapredx(:,i) + increment)
   end if
   if (index(nusis(i),'amsua') .gt. 0) then
       write(6,fmt) niter,i,trim(adjustl(nusis(i))),nuchan(i),deltapredx1(:,i)
   end if
   deallocate(biaspredtmp)
   deallocate(obinc)
  enddo
  if (nproc == 0)  print *,'time to update bias correction on root',mpi_wtime()-t1,'secs'
  t1 = mpi_wtime()
  call mpi_allreduce(deltapredx1,deltapredx,npred*jpch_rad,mpi_realkind,mpi_sum,mpi_comm_world,ierr)
  if (niter == numiter .and. newpc4pred) then
     ! distribute updated varA to all processors.
     buffertmp=zero
     do i=i1,i2
     do n=1,npred
       buffertmp(n,i) = varA(n,i)
     enddo
     enddo
     call mpi_allreduce(buffertmp,varA,jpch_rad*npred,mpi_realkind,mpi_sum,mpi_comm_world,ierr)
     ! update ostats
     do i=1,jpch_rad
        ostats(i) = numobspersat(i)
     enddo
  endif
  if (nproc == 0) print *,'time in update_biascorr mpi_allreduce on root = ',mpi_wtime()-t1
endif ! nobs_sat > 0

end subroutine update_biascorr

subroutine symminv(a,n)
  ! cholesky decomp inverse of a symm. matrix.
  integer, intent(in) :: n
  real(r_kind), intent(inout) :: a(n,n)
  integer ierr,i,j
  if (r_kind == kind(1.d0)) then
     call dpotrf('L',n,a,n,ierr)
     if (ierr /= 0) then
        print *,'ierr=',ierr,'in dpotrf!'
     end if
     call dpotri('L',n,a,n,ierr)
     if (ierr /= 0) then
        print *,'ierr=',ierr,'in dpotri!'
     end if
  else if (r_kind == kind(1.e0)) then
     call spotrf('L',n,a,n,ierr)
     if (ierr /= 0) then
        print *,'ierr=',ierr,'in spotrf!'
     end if
     call spotri('L',n,a,n,ierr)
     if (ierr /= 0) then
        print *,'ierr=',ierr,'in spotri!'
     end if
  else
     print *,'kind specification must be default real or double precision'
     stop
  endif
  do j=2,n
   do i=1,j-1
     a(i,j) = a(j,i)
   enddo
  enddo
end subroutine symminv

end module radbias

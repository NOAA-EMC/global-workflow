subroutine evaljo(pjo,kobs,kprt,louter)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    evaljo
!   prgmmr: tremolet
!
! abstract: Computes and prints Jo components
!
! program history log:
!   2007-03-01  tremolet
!   2009-01-15  todling  - quad precision for reproducibility
!   2009-08-14  lueken   - update documentation
!
!   input argument list:
!     kprt - print level
!     louter
!
!   output argument list:
!     kobs - Number of obs used in evaluating Jo
!     pjo  - Jo value
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  use kinds, only: r_kind,i_kind,r_quad
  use obs_sensitivity, only: obsensCounts_set
  use gsi_obOperTypeManager, only: obOper_typeInfo
  use m_obsdiags   , only: obsdiags
  use m_obsdiagNode, only: obs_diag
  use gsi_4dvar, only: nobs_bins
  use constants, only: zero_quad
  use mpimod, only: ierror,mpi_comm_world,mpi_sum,mpi_integer,mype
  use jfunc, only: jiter
  use mpl_allreducemod, only: mpl_allreduce
  use mpeu_util, only: perr,die

  implicit none

! Declare passed variables
  real(r_quad)   ,intent(  out) :: pjo
  integer(i_kind),intent(  out) :: kobs
  integer(i_kind),intent(in   ) :: kprt
  logical        ,intent(in   ) :: louter

! Declare local variables
  character(len=*), parameter :: myname='evaljo'
  integer(i_kind) :: ii,jj,ij,ilen
  integer(i_kind) ::    iobs(size(obsdiags,1))
  real(r_quad)    :: zjo,zz
  real(r_kind)    :: zdep
  real(r_quad)    ::    zjo1(size(obsdiags,1))
  real(r_quad)    ::    zjo2(size(obsdiags,1),nobs_bins)
  real(r_quad)    ::  zprods(size(obsdiags,1)*nobs_bins)
  integer(i_kind) :: iobsgrp(size(obsdiags,1),nobs_bins)
  integer(i_kind) :: iobsglb(size(obsdiags,1),nobs_bins)
  type(obs_diag),pointer:: obsptr
  character(len=20):: cobstype_ii
  integer(i_kind) :: nobs_type
! ----------------------------------------------------------

zprods(:)=zero_quad
iobsgrp(:,:)=0
iobsglb(:,:)=0
nobs_type = size(obsdiags,1)

if(size(obsdiags,2)/=nobs_bins) then
  call perr(myname,'size(obsdiags,2)/=nobs_bins, size(obsdiags,2) =',size(obsdiags,2))
  call perr(myname,'                                    nobs_bins =',nobs_bins)
  call  die(myname)
endif

ij=0
do ii=1,nobs_bins
   do jj=1,nobs_type
      ij=ij+1

      !++ if(louter) then
      !++   zprods(ij) = obsLL(jj,ii)%NLDdotprod(jiter,nob=iobsgrp(jj,ii))
      !++ else
      !++   zprods(ij) = obsLL(jj,ii)%DELdotprod(jiter,nob=iobsgrp(jj,ii))
      !++ endif
      obsptr => obsdiags(jj,ii)%head
      do while (associated(obsptr))
         if (obsptr%luse.and.obsptr%muse(jiter)) then
            if (louter) then
               zdep=obsptr%nldepart(jiter)
            else
               zdep=obsptr%tldepart(jiter)-obsptr%nldepart(jiter)
            endif
            zprods(ij) = zprods(ij) + obsptr%wgtjo * zdep * zdep
            iobsgrp(jj,ii)=iobsgrp(jj,ii)+1
         endif
         obsptr => obsptr%next
      enddo

   enddo
enddo

! Sum Jo contributions
call mpl_allreduce(nobs_type*nobs_bins,qpvals=zprods)

! Sum number of observations
ilen=nobs_bins*nobs_type
call mpi_allreduce(iobsgrp,iobsglb,ilen, &
                 & mpi_integer,mpi_sum,mpi_comm_world,ierror)

! Gather Jo contributions

ij=0
do ii=1,nobs_bins
   do jj=1,nobs_type
      ij=ij+1
      zjo2(jj,ii)=zprods(ij)
   enddo
enddo

zjo1=zero_quad
iobs=0
DO ii=1,nobs_bins
   zjo1(:)=zjo1(:)+zjo2(:,ii)
   iobs(:)=iobs(:)+iobsglb(:,ii)
ENDDO

zjo=zero_quad
kobs=0
DO ii=1,nobs_type
   zjo=zjo+zjo1(ii)
   kobs=kobs+iobs(ii)
ENDDO

pjo=zjo

! Prints
IF (kprt>=2.and.mype==0) THEN
   if (louter) then
      write(6,*)'Begin Jo table outer loop'
   else
      write(6,*)'Begin Jo table inner loop'
   endif

   IF (kprt>=3.and.nobs_bins>1) THEN
      write(6,400)'Observation Type','Bin','Nobs','Jo','Jo/n'
      DO ii=1,nobs_type
         cobstype_ii=obOper_typeInfo(ii)
         DO jj=1,nobs_bins
            IF (iobsglb(ii,jj)>0) THEN
               zz=zjo2(ii,jj)/iobsglb(ii,jj)
               write(6,100)cobstype_ii,jj,iobsglb(ii,jj),real(zjo2(ii,jj),r_kind),real(zz,r_kind)
            ENDIF
         ENDDO
      ENDDO
   ENDIF

   write(6,400)'Observation Type',' ','Nobs','Jo','Jo/n'
   DO ii=1,nobs_type
      cobstype_ii=obOper_typeInfo(ii)
      IF (iobs(ii)>0) THEN
         zz=zjo1(ii)/iobs(ii)
         write(6,200)cobstype_ii,iobs(ii),real(zjo1(ii),r_kind),real(zz,r_kind)
      ENDIF
   ENDDO

   IF (kobs>0) THEN
      zz=zjo/kobs
   ELSE
      zz=-999.999_r_quad
   ENDIF
   write(6,400)'  ',' ','Nobs','Jo','Jo/n'
   write(6,300)"Jo Global",kobs,real(zjo,r_kind),real(zz,r_kind)

   if (louter) then
      write(6,*)'End Jo table outer loop'
   else
      write(6,*)'End Jo table inner loop'
   endif
ENDIF

call obsensCounts_set(iobsglb(:,:))

100 format(a20,2x,i3,2x,i8,2x,es24.16,2x,f10.3)
200 format(a20,2x,3x,2x,i8,2x,es24.16,2x,f10.3)
300 format(a20,2x,3x,2x,i8,2x,es24.16,2x,f10.3)
400 format(a20,2x,a3,2x,a8,2x,a24,4x,a8)

! ----------------------------------------------------------
return
end subroutine evaljo

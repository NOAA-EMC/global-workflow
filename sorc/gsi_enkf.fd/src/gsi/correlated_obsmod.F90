!BOI

!  !TITLE: Correlated\_ObsMod: Inter-channel Observation Correlation Module

!  !AUTHORS: Ricardo Todling

!  !AFFILIATION: Global Modeling and Assimilation Office, NASA/GSFC, Greenbelt, MD 20771

!  !DATE: 13 April 2014

!  !INTRODUCTION: Overview
#ifdef __PROTEX__

This module introduces the ability for GSI to account for inter-channel
correlated errors for radiance observations. It assumes an offline estimate of
an observation error covariance for a given instrument is available. 

At GMAO, the offline estimation of the error covariances required by this module
is performed by a FORTRAN program that reads the GSI-diag files and performs 
statistics on the observation-minus-background and observation-minus-analysis
residuals, following the so-called Desroziers approach (e.g., Desroziers et al.
2005; Q. J. R. Meteorol. Soc., 131, 3385-3396).

At NCEP, the offline estimation of the error covariances can be computed
by the cov_calc module, located in util/Correlated_Obs.  This module is also
based on the Desroziers method.

This module defines the so-called Obs\_Error\_Cov.

As Met\_Guess and other like-modules, the idea is for this module to define nearly 
opaque object. However, so far, we have had no need to add inquire-like functions - that
is, no code outside this code needs to know what is inside GSI\_Obs\_Error\_Cov. 
So far, only very general `methods'' are made public from this module, these
being, 

\begin{verbatim}
public :: corr_ob_initialize
public :: corr_ob_amiset
public :: corr_adjust_jacobian
public :: corr_ob_finalize
\end{verbatim}

and never the variables themselves; the only exception being the GSI\_MetGuess\_Bundle itself 
(until it is no longer treated as a common-block).  Some of the public methods above are 
overloaded and all have internal interfaces (name of which appears in the index of this protex 
document. It should be a rule here that any new routine to be made public should
have a declared interface procedure.

\begin{center}
\fbox{Obs\_Error\_Cov is defined via the {\it correlated\_observations} table in a resource file}
\end{center}

\underline{Defining Observation Error Covariance Models} is done via the table {\it correlated\_observations},
usually embedded in the {\it anavinfo} file. An example of such table follows:
\begin{verbatim}
correlated_observations::
! isis       method   kreq   kmut  type    cov_file
  airs_aqua  1      60.   1.0   ice     airs_rcov.bin
  airs_aqua  1      60.   1.0   land    airs_rcov.bin
  airs_aqua  1      60.   1.0   sea     airs_rcov.bin
  airs_aqua  1      60.   1.0   snow    airs_rcov.bin
  airs_aqua  1      60.   1.0   mixed   airs_rcov.bin
# cris_npp      1     -99.   1.0   snow    cris_rcov.bin
# cris_npp      1     -99.   1.0   land    cris_rcov.bin
# cris_npp      1     -99.   1.0   sea     cris_rcov.bin
  iasi_metop-a  2      0.12  1.3   snow    iasi_sea_rcov.bin
  iasi_metop-a  2      0.22  1.3   land    iasi_land_rcov.bin
  iasi_metop-a  2      0.05  1.3   sea     iasi_sea_rcov.bin
  iasi_metop-a  2      0.12  1.3   ice     iasi_sea_rcov.bin
  iasi_metop-a  2      0.12  1.3   mixed   iasi_sea_rcov.bin
# ssmis_f17     1     -99.   1.0   mixed   ssmis_rcov.bin
# ssmis_f17     1     -99.   1.0   land    ssmis_rcov.bin
# ssmis_f17     1     -99.   1.0   sea     ssmis_rcov.bin

::
\end{verbatim}
Notice that the covariance can be supplied for all five surface types,
namely,  ice, snow, mixed, land, and sea. However, they can be made the same, by simply
pointing the different types to the same file. In the example above, only AIRS and
IASI from Metop-A are being specially handled by this module. In the case of
AIRS, no distinction is made among the different types of surfaces, whereas 
in the case of IASI, a distinction is made between land and sea, with everything
else being treated as sea.  It is not necessary to specify a covariance file for
each surface type.

The instrument name is the same as it would be in the satinfo file.

As usual, this table follows INPAK/ESMF convention, begining with a name
(correlated\_observations), followed by double colons (::) to open the table and 
ending with double colons.  Any line starting with an exclamation mark or a pound sign 
is taken as a comment.

The current {\it correlated\_observations} table has four columns defined as follows:

\begin{verbatim}
Column 1: isis   - refers to instrument/platform type (follows, typical GSI nomenclature)
Column 2: method - specify different possibilities for handling the corresponding 
          cov(R) at present:
          <0 - reproduces GSI running with the default(for testing only)
           0 - diag of est(R) only
           1 - using the correlations extracted from est(R) and variances from the satinfo file.
           2 - as (1), but using the full est(R)
           3 - diag of est(R) used as scaling factor to internally-defined errors
Column 3: kreq   - level of required condition for the corresponding cov(R)
          at present:
          if<0 and method=0, 1 or 3  does not recondition matrix
          if>0 and method=1          recondition the (correlation) matrix following
                                     the 2nd method in Weston et al. (2014;
                                     Q. J. R. Meteorol. Soc., DOI: 10.1002/qj.2306)
                                     Note that the resulting correlation matrix has
                                     condition number equal to approximatetly twice kreq.
          if>0 and method=0 or 3     recondition the (covariance) matrix using Westons 2nd method
          if method=2                recondition the covariance matrix by inflating the
                                     diagional so that R_{r,r}=(sqrt{R_{r,r}+kreq)^2
                                     Note that kreq should be specified as 0<kreq<1
Colum  4: kmut - a multiplicitve factor to apply to the entire matrix, for method 2 only
Column 5: type - determines whether to apply covariance over ocean, land, ice, snow or mixed FOVs
Column 6: cov_file - name of file holding estimate of error covariance for the
                     instrument specified in column 1
\end{verbatim}

#endif
!EOI

!-------------------------------------------------------------------------
!  NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1, GMAO  !
!-------------------------------------------------------------------------
!BOP
!  
! !MODULE: correlated_obsmod -- inter-channel correlation ob error capability
!
! !INTERFACE:

module correlated_obsmod
!
! !DESCRIPTION: Module to handle options and components related to
!               specifying inter-channel correlation observation error 
!               covariances.
!
!               This uses wired-in type arrays to hold error covariances
!               for different instruments. This should be revisited in
!               the future.
!
! !USES:

use kinds, only: i_kind, r_single, r_kind, r_double, r_quad
use constants, only: zero,one,zero_quad
use mpimod, only: mype
use timermod, only: timer_ini, timer_fnl
use mpeu_util, only: gettablesize
use mpeu_util, only: gettable
use mpeu_util, only: die
use mpeu_util, only: luavail
use radinfo, only: lupdqc,lqcoef
private
save

! !PUBLIC MEMBER FUNCTIONS:

public idnames
public ObsErrorCov
public GSI_BundleErrorCov
public corr_ob_initialize
public corr_ob_amiset
public corr_adjust_jacobian
public corr_ob_finalize

! !METHOD OVERLOADING:

interface corr_ob_initialize; module procedure ini_; end interface
interface corr_ob_amiset; module procedure amiset_; end interface
interface corr_adjust_jacobian; module procedure adjust_jac_; end interface
interface corr_ob_finalize; module procedure fnl_; end interface

! !REVISION HISTORY:
!
!   15Apr2014 Todling  Initial code.
!   19Dec2014 W. Gu, replace the prescribed obs errors in satinfo with the diag of est(R)
!   02Nov2016 W. Gu  kmut was added to inflate the whole R-covariance matrix.
!   22Apr2019 kbathmann Major update to replace eigendecomposion with Cholesky factorization, 
!                       and genrally speed up the code
!   27Jul2018 W. Gu  code changes to reduce the round-off errors.
!   10Jul2019 Todling keep upd_varch_ as internal routine called within initialization
!   14Aug2019 W. Gu  add lupdqc to replace the obs errors from satinfo with diag of est(R).
!   14Aug2019 W. Gu  add lqcoef to combine the inflation coefficients generated by qc with est(R)
!   28Aug2019 W. Gu  remove Revecs from structure type of ObsErrorCov
!
!EOP
!-------------------------------------------------------------------------
!BOC

integer(i_kind),parameter::MAXSTR=256
character(len=MAXSTR),allocatable :: instruments(:)
character(len=MAXSTR),allocatable :: idnames(:)

integer :: ninstr=-1   ! single instrument for now
logical :: iamroot_
logical :: GMAO_ObsErrorCov=.false.

! !PRIVATE TYPES:
type ObsErrorCov
     character(len=40) :: name                        ! R covariance name
     character(len=20) :: instrument                  ! instrument
     integer(i_kind)   :: nch_active=-1               ! number of channels actively assimilated, according to the satinfo
     integer(i_kind)   :: nctot=-1                    ! total number of channels (active+passive), according to the covariance file
     integer(i_kind)   :: method    =-1               ! define method of computation
     real(r_kind)      :: kreq      =-99._r_kind      ! Weston et al-like spectrum adjustment factor
     real(r_kind)      :: kmut      =-99._r_kind      ! multiplicative inflation factor
     character(len=20) :: mask      ='global'         ! Apply covariance for profiles over all globe
     integer(i_kind),pointer :: indxR(:)   =>NULL()   ! indexes of active channels in between 1 and nchanl, according to the satinfo
     real(r_kind),   pointer :: R(:,:)     =>NULL()   ! nch_active x nch_active
     real(r_kind),   pointer :: Revals(:)  =>NULL()   ! eigenvalues of R
end type

! !PUBLIC TYPES:

type(ObsErrorCov),pointer :: GSI_BundleErrorCov(:)

! strictly internal quantities
character(len=*),parameter :: myname='correlated_obsmod'
logical :: initialized_=.false.
logical, parameter :: VERBOSE_=.true.
integer(i_kind),parameter :: methods_avail(5)=(/-1, & ! do nothing
                                                 0, & ! use dianonal of estimate(R)
                                                 1, & ! use the correlations extracted from the full est(R)
                                                 2, & ! use full est(R)
                                                 3/)  ! use diag est(R), as scaling factor to GSI(R)
contains

!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  ini_ --- Initialize info about correlated obs (read resource table)
!
! !INTERFACE:
!
subroutine ini_ (iamroot)
! !USES:
use mpeu_util, only: die
implicit none
! !INPUT PARAMETERS:
   logical,optional,intent(in) :: iamroot 
! !DESCRIPTION: Define parameters and setting for handling correlated
!               observation errors via resouce file reading.
!
! !REVISION HISTORY:
!   2014-04-13  todling  initial code
!
! !REMARKS:
!   language: f90
!   machine:  discover
!
! !AUTHOR:
!   Ricardo Todling  org: gmao      date: 2014-04-13
!
!EOP
!-------------------------------------------------------------------------
!BOC
character(len=*),parameter:: rcname='anavinfo'  ! filename should have extension
character(len=*),parameter:: tbname='correlated_observations::'
integer(i_kind) luin,ii,ntot,nrows,method
character(len=MAXSTR),allocatable,dimension(:):: utable
character(len=20) instrument, mask
character(len=30) filename
real(r_kind) kreq4
real(r_kind) kmut4
real(r_kind) kreq
real(r_kind) kmut
character(len=*),parameter::myname_=myname//'*ini_'

if(initialized_) return

iamroot_=mype==0
if(present(iamroot)) iamroot_=iamroot 

! load file
luin=luavail()
open(luin,file=rcname,form='formatted')

! Scan file for desired table first
! and get size of table
call gettablesize(tbname,luin,ntot,nrows)
if(nrows==0) then
   close(luin)
   return
endif
ninstr=nrows

! Get contents of table
allocate(utable(ninstr),instruments(ninstr),idnames(ninstr))
call gettable(tbname,luin,ntot,ninstr,utable)

! release file unit
close(luin)

allocate(GSI_BundleErrorCov(ninstr))

! Retrieve each token of interest from table and define
! variables participating in state vector

! Count variables first
if(iamroot_) write(6,*) myname_,': Correlated-Obs for the following instruments'
do ii=1,ninstr
   read(utable(ii),*) instrument, method, kreq4, kmut4, mask, filename ! if adding col to table leave fname as last
   instruments(ii) = trim(instrument)
   idnames(ii) = trim(instrument)//':'//trim(mask)
   kreq=kreq4
   kmut=kmut4
   if(iamroot_) then
      write(6,'(1x,2(a,1x),i4,1x,2f20.16,1x,a)') trim(instrument), trim(mask), method, kreq4, kmut4, trim(filename)
   endif
!  check method validity
   if(ALL(methods_avail/=method)) then
     call die(myname_,' invalid choice of method, aborting')
   endif
   call set_(trim(instrument),trim(filename),mask,method,kreq,kmut,GSI_BundleErrorCov(ii))
enddo

! release table
deallocate(utable)

! initialize 
if(lupdqc)call upd_varch_

end subroutine ini_
!EOC

!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  set_ --- set error covariances for different instruments
!
! !INTERFACE:
!
subroutine set_(instrument,fname,mask,method,kreq,kmut,ErrorCov)
use radinfo, only: nusis,iuse_rad,jpch_rad,varch,nuchan
use constants, only: zero
implicit none

! !INPUT PARAMETERS:

character(len=*),intent(in) :: instrument   ! name of instrument
character(len=*),intent(in) :: fname        ! filename holding cov(R)
character(len=*),intent(in) :: mask         ! land/sea/etc mask
integer,intent(in):: method                 ! method to apply when using this cov(R)
real(r_kind),intent(in) :: kreq             ! conditioning factor for cov(R)
real(r_kind),intent(in) :: kmut             ! multiplicative inflation factor for cov(R)
type(ObsErrorCov),intent(inout) :: ErrorCov ! cov(R) for this instrument

! !DESCRIPTION: Given basic information on the instrument type
!               this routine reads an available estimate
!               of the corresponding fully-correlated error
!               covariance and fills the FORTRAN type defined
!               as ObsErrorCov.
!
! !REVISION HISTORY:
!   2014-04-13  todling  initial code
!   2014-08-06  todling  platform-specific correlated obs handle
!
! !REMARKS:
!   language: f90
!   machine:  discover
!
! !AUTHOR:
!   Ricardo Todling  org: gmao      date: 2014-04-13
!
!EOP
!-------------------------------------------------------------------------
!BOC

character(len=*),parameter :: myname_=myname//'*set'
integer(i_kind) nch_active   !number of channels accounted for in the covariance file
integer(i_kind) nctot        !the total number of channels (active+passive), according to the covariance file
integer(i_kind) lu,ii,jj,ioflag,iprec,coun,couns,istart,indR,nctotf
integer(i_kind),dimension(:),allocatable:: indxR,indxRf !channel indices read in from the covariance file
real(r_kind),dimension(:,:),allocatable:: Rcov   !covariance matrix read in from the covariance file
real(r_single),allocatable, dimension(:,:) :: readR4  ! nch_active x nch_active 
real(r_double),allocatable, dimension(:,:) :: readR8  ! nch_active x nch_active 
real(r_kind),allocatable, dimension(:) :: diag
logical :: corr_obs

   ErrorCov%instrument = trim(instrument)
   ErrorCov%mask = trim(mask)
   ErrorCov%name = trim(instrument)//':'//trim(mask)
   ErrorCov%method = method
   ErrorCov%kreq   = kreq
   ErrorCov%kmut   = kmut

   inquire(file=trim(fname), exist=corr_obs)

   if (corr_obs) then
      lu = luavail()
      open(lu,file=trim(fname),convert='little_endian',form='unformatted')
      if (GMAO_ObsErrorCov) then
         read(lu,IOSTAT=ioflag) nch_active, iprec
      else
         read(lu,IOSTAT=ioflag) nch_active, nctot, iprec
      endif
      if(ioflag/=0) call die(myname_,' failed to read nch from '//trim(fname))
      coun=0
      couns=0
      istart=0 
      nctotf=0
      do ii=1,jpch_rad
        if (nusis(ii)==ErrorCov%instrument) then
           if (couns==0) then 
              istart=ii-1
              couns=1
           endif
           if (iuse_rad(ii)>0) then
              coun=coun+1
           endif
           nctotf=nctotf+1
        endif
      enddo
!     if no data available, turn off Correlated Error
      if (coun==0) then
         if (iamroot_) write(6,*) 'WARNING: ',trim(ErrorCov%instrument), &
                       ' is not initiallized. Turning off Correlated Error'
         return
      endif
      ErrorCov%nch_active = coun
      if (.not.GMAO_ObsErrorCov) ErrorCov%nctot = nctot
      call create_(coun,ErrorCov)
      allocate(indxRf(coun),indxR(nch_active),Rcov(nctot,nctot))

!     Read GSI-like channel numbers used in estimating R for this instrument
      read(lu,IOSTAT=ioflag) indxR
      if(ioflag/=0) call die(myname_,' failed to read indx from '//trim(fname))

!     Read estimate of observation error covariance
      Rcov=0.0_r_kind
      if(iprec==4) then
        allocate(readR4(nch_active,nch_active))
        read(lu,IOSTAT=ioflag) readR4
        if(ioflag/=0) call die(myname_,' failed to read R from '//trim(fname))
        Rcov(1:nch_active,1:nch_active)=readR4
        deallocate(readR4)
      endif
      if(iprec==8) then
        allocate(readR8(nch_active,nch_active))
        read(lu,IOSTAT=ioflag) readR8
        if(ioflag/=0) call die(myname_,' failed to read R from '//trim(fname))
        Rcov(1:nch_active,1:nch_active)=readR8
        deallocate(readR8)
      endif
      if (GMAO_ObsErrorCov) then
         ErrorCov%indxR(1:nch_active)=indxR(1:nch_active)
         ErrorCov%nch_active=nch_active
      else
         coun=0
         ErrorCov%R=zero
         do ii=1,nctotf 
            if (iuse_rad(ii+istart)>0) then
               coun=coun+1
               ErrorCov%indxR(coun)=ii
               indxRf(coun)=nuchan(ii+istart)
            endif
         enddo
!Add rows and columns for active channels in the satinfo that are not in the covariance file
        couns=1
        do ii=1,ErrorCov%nch_active
           indR=0
           do jj=couns,nch_active 
              if (indxR(jj)==indxRf(ii)) then
                 indR=jj
                 couns=jj+1
                 exit
              endif
           enddo
           if (indR==0) then
              do jj=nctot-1,ii,-1
                 Rcov(jj+1,:)=Rcov(jj,:)
                 Rcov(:,jj+1)=Rcov(:,jj)
              enddo
              Rcov(ii,:)=zero
              Rcov(:,ii)=zero
              Rcov(ii,ii)=varch(istart+ErrorCov%indxR(ii))*varch(istart+ErrorCov%indxR(ii))
           endif
        enddo
!Remove rows and columns that are in the covariance file, but not in the satinfo
         couns=1
         do ii=1,nch_active
           indR=0
           do jj=couns,ErrorCov%nch_active
              if (indxRf(jj)==indxR(ii)) then
                 indR=jj
                 couns=jj+1
                 exit
              endif
           enddo
           if (indR==0) then
              do jj=ii,nctot-1
                 Rcov(jj,:)=Rcov(jj+1,:)
                 Rcov(:,jj)=Rcov(:,jj+1) 
              enddo
            endif
         enddo
      endif
      ErrorCov%R(1:ErrorCov%nch_active,1:ErrorCov%nch_active)=Rcov(1:ErrorCov%nch_active,1:ErrorCov%nch_active)
!     Done reading file
      close(lu)
   else
      if (iamroot_) write(6,*) 'No Rcov files found.  Turning off Correlated Error'
      return
   end if
!  If method<0 there is really nothing else to do
!  ----------------------------------------------
   if (method<0) then
      initialized_=.true.
      return
   endif
   nch_active=ErrorCov%nch_active
   if (VERBOSE_) then
       allocate(diag(nch_active))
       do ii=1,nch_active
          diag(ii)=ErrorCov%R(ii,ii)
       enddo
       if(iamroot_) then
          write(6,'(2a)') 'Rcov(stdev) for instrument: ', trim(ErrorCov%name)
          write(6,'(9(es13.6))') sqrt(diag)
          write(6,'(3a)') 'Channels used in estimating Rcov(', trim(ErrorCov%name), ')'
          write(6,'(12(1x,i5))') ErrorCov%indxR
       endif
       deallocate(diag)
   endif

!  Now decompose R
   call solver_(ErrorCov)

   if (VERBOSE_ .and. ErrorCov%method>=0) then
       allocate(diag(nch_active))
       do ii=1,nch_active
          diag(ii)=ErrorCov%R(ii,ii)
       enddo
       if(iamroot_) then
          write(6,'(3a)') 'Rcov(stdev) for instrument: ', trim(ErrorCov%name), ' recond'
          write(6,'(9(es13.6))') sqrt(diag)
       endif
       deallocate(diag)
   endif
   deallocate(indxR,Rcov,indxRf)
   initialized_=.true.
end subroutine set_
!EOC

!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  create_ --- creates type to hold observation error covariance
!
! !INTERFACE:
!
subroutine create_ (nch,ErrorCov)
implicit none
! !INPUT PARAMETERS:
integer(i_kind),intent(in) :: nch
! !INPUT/OUTPUT PARAMETERS:
type(ObsErrorCov),intent(inout) :: ErrorCov
! !DESCRIPTION: Allocates space for FORTRAN type hold observation error
!               covariance and required information.
!
! !REVISION HISTORY:
!   2014-04-13  todling  initial code
!
! !REMARKS:
!   language: f90
!   machine:  discover
!
! !AUTHOR:
!   Ricardo Todling  org: gmao      date: 2014-04-13
!
!EOP
!-------------------------------------------------------------------------
!BOC
   allocate(ErrorCov%R(nch,nch))
   allocate(ErrorCov%indxR(nch))
   allocate(ErrorCov%Revals(nch))
end subroutine create_
!EOC

!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  destroy_ --- destroy type holding observation error covariance
!
! !INTERFACE:
!
subroutine destroy_ (ErrorCov)
implicit none
! !INPUT/OUTPUT PARAMETERS:
type(ObsErrorCov),intent(inout) :: ErrorCov
! !DESCRIPTION: Deallocates space held for observation error covariance.
!
! !REVISION HISTORY:
!   2014-04-13  todling  initial code
!
! !REMARKS:
!   language: f90
!   machine:  discover
!
! !AUTHOR:
!   Ricardo Todling  org: gmao      date: 2014-04-13
!
!EOP
!-------------------------------------------------------------------------
!BOC
   if (associated(ErrorCov%Revals)) deallocate(ErrorCov%Revals)
   if (associated(ErrorCov%indxR))  deallocate(ErrorCov%indxR)
   if (associated(ErrorCov%R))      deallocate(ErrorCov%R)
end subroutine destroy_
!EOC


!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  solver_ --- entry-point to the decomposition of cov(R)
!
! !INTERFACE:
!

subroutine solver_(ErrorCov)
implicit none
! !INPUT/OUTPUT PARAMETERS:
type(ObsErrorCov),intent(inout) :: ErrorCov

! !DESCRIPTION: This routine is the entry point to the eigen-decomposition
!               of the obs error covariance. Depending on the method chosen
!               by the user, it might call the proper routines to recondition
!               the offline estimate of cov(R).
!
! !REVISION HISTORY:
!   2014-04-13  todling  initial code
!   2015-08-18  W. Gu, Switich the reconditioning method from adding a constant value
!                      to each eigenvalue to adding a constant value in standard deviation to
!                      each diagnoal element.
!   2018-07-27  W. Gu, code changes to reduce the round-off errors.
!
! !REMARKS:
!   language: f90
!   machine:  discover
!
! !AUTHOR:
!   Ricardo Todling  org: gmao      date: 2014-04-13
!
!EOP
!-------------------------------------------------------------------------
!BOC
character(len=*), parameter :: myname_=myname//'*solver_'
real(r_kind) lambda_max,lambda_min,lambda_inc
integer(i_kind) ii,jj,ndim
logical adjspec
real(r_kind),allocatable,dimension(:,:):: Revecs
real(r_kind),allocatable,dimension(:):: invstd

ndim = size(ErrorCov%R,1)
allocate(Revecs(ndim,ndim))
 
! This extracts the diagonal of R (error variances), setting the 
! eigenvalues as such and the eigenvectors as the unit vectors
! This is to allow using the estimated error variances, but
! but still pretend the covariance is diagnoal - no correlations.
! This is largely for testing consistency of the implementation.
if ( ErrorCov%method==0 .or. ErrorCov%method==3 ) then
   Revecs = zero
   do ii=1,ndim
      ErrorCov%Revals(ii)    = ErrorCov%R(ii,ii) 
      Revecs(ii,ii) = one
   enddo
   call westonEtAl_spectrum_boost_(adjspec)
   if (adjspec) then
      call rebuild_rcov_
   endif
endif ! method=0

! This takes only corr(Re) and 
! any reconditioning is of correlation matrix
if ( ErrorCov%method==1 ) then
   ! reduce R to correlation matrix
   allocate(invstd(ndim))
   do jj=1,ndim
      invstd(jj) = ErrorCov%R(jj,jj)
   enddo
   do jj=1,ndim
      do ii=1,ndim
         ErrorCov%R(ii,jj) = ErrorCov%R(ii,jj)/sqrt(invstd(ii)*invstd(jj))
      enddo
   enddo
   deallocate(invstd)
   Revecs=ErrorCov%R
   call decompose_(trim(ErrorCov%name),ErrorCov%Revals,Revecs,ndim,.true.)
   call westonEtAl_spectrum_boost_(adjspec)
   if (adjspec) then
      call rebuild_rcov_
      allocate(invstd(ndim))
      do jj=1,ndim
         invstd(jj) = ErrorCov%R(jj,jj)
      enddo
      do jj=1,ndim
         do ii=1,ndim
            ErrorCov%R(ii,jj) = ErrorCov%R(ii,jj)/sqrt(invstd(ii)*invstd(jj))
         enddo
      enddo
      deallocate(invstd)
   endif
endif ! method=1

! This does the actual full eigendecomposition of the R matrix
! Here, recondioning is of covariance matrix
if ( ErrorCov%method==2 ) then
   Revecs=ErrorCov%R
   call decompose_(trim(ErrorCov%name),ErrorCov%Revals,Revecs,ndim,.true.)
   if ((ErrorCov%kreq>zero).or.(ErrorCov%kmut>one)) then
      do jj=1,ndim
         do ii=1,ndim
           if(ii==jj) then
             ! inflated by constant standard deviation 
             ErrorCov%R(ii,ii)=ErrorCov%kmut*ErrorCov%kmut*&
                     (sqrt(ErrorCov%R(ii,ii))+ErrorCov%kreq)**2  
           else
             ErrorCov%R(ii,jj)=ErrorCov%kmut*ErrorCov%kmut*ErrorCov%R(ii,jj)
           endif
        enddo
      enddo
      Revecs=ErrorCov%R
      call decompose_(trim(ErrorCov%name),ErrorCov%Revals,Revecs,ndim,.true.)
   endif
   ! In this case, we can wipe out the eigen-decomp since it will be redone for
   ! each profile at each location at setup time.
   ErrorCov%Revals=zero
endif ! method=2

deallocate(Revecs)

  contains
  subroutine westonEtAl_spectrum_boost_(adjspec)
    implicit none
    logical,intent(out) :: adjspec
    adjspec=.false.
    if(ErrorCov%kreq < zero) return
    lambda_max=maxval(ErrorCov%Revals)
    lambda_min=minval(ErrorCov%Revals)
    lambda_inc=(lambda_max - (lambda_min * ErrorCov%kreq))/(ErrorCov%kreq-1)
    if(lambda_inc>zero) then
       ErrorCov%Revals = ErrorCov%Revals + lambda_inc
    else
       if (iamroot_) then
         write(6,'(2a,1x,es10.3)') myname_, ' Spectrum of cov(R) not changed, poor choice of kreq = ', &
                         ErrorCov%kreq
       endif
    endif
    adjspec=.true.
  end subroutine westonEtAl_spectrum_boost_
  subroutine rebuild_rcov_
  implicit none
  integer(i_kind) ii,jj,kk
  real(r_kind), allocatable, dimension(:,:) :: tmp
  allocate(tmp(ndim,ndim))
  ! D*U^T
  do jj=1,ndim
     tmp(:,jj) = ErrorCov%Revals(:) * Revecs(jj,:)
  enddo
  ! U*(D*U^T)
  ErrorCov%R = matmul(Revecs,tmp)
  Revecs =ErrorCov%R
  call decompose_(trim(ErrorCov%name),ErrorCov%Revals,Revecs,ndim,.true.)
  ! clean up
  deallocate(tmp)
  end subroutine rebuild_rcov_
end subroutine solver_
!EOC


!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  decompose_ --- calculates eigen-decomposition of cov(R)
!
! !INTERFACE:
!
subroutine decompose_(instrument,Evals,Evecs,ndim,lprt)
! !USES:
  use constants, only: tiny_r_kind
  implicit none
! !INPUT PARAMETERS:
  character(len=*),intent(in):: instrument
  integer(i_kind),intent(in) :: ndim
  logical,intent(in)         :: lprt
! !INPUT/OUTPUT PARAMETERS:
  real(r_kind),intent(inout) :: Evals(:)
  real(r_kind),intent(inout) :: Evecs(:,:) ! on entry: matrix to decompose
                                           ! on exit: eigenvectors

! !DESCRIPTION: This routine makes a LAPACK call to eigen-decompose cov(R).
!               Its initial implementation is the crudest possible; it does
!               not make use of the fact that only the upper or lower triangles
!               of the matrix are needed; the problems solver are so small that
!               at present this does not seem to be an issue; this could be 
!               easily revisited in the future.
!
! !REVISION HISTORY:
!   2014-04-13  todling  initial code
!
! !REMARKS:
!   language: f90
!   machine:  discover
!
! !AUTHOR:
!   Ricardo Todling  org: gmao      date: 2014-04-13
!
!EOP
!-------------------------------------------------------------------------
!BOC
  character(len=*),parameter :: myname_=myname//'decompose_'
  character*1 jobz
  integer(i_kind) lwork,info
  real(r_kind) lambda_max,lambda_min,cond
  real(r_kind),allocatable, dimension(:) :: work
  jobz = 'V' ! evals & evecs
  lwork = max(1,3*ndim-1)
  allocate(work(lwork))
  if(r_kind==r_single) then ! this trick only works because this uses the f77 lapack interfaces
     call SSYEV( jobz, 'U', ndim, Evecs, ndim, Evals, WORK, lwork, info )
  else if(r_kind==r_double) then
     call DSYEV( jobz, 'U', ndim, Evecs, ndim, Evals, WORK, lwork, info )
  else
     call die(myname_,'no corresponding LAPACK call for solving eigenproblem')
  endif
  if (info==0) then
     if (lprt) then
        cond=-999._r_kind
        lambda_max=maxval(Evals)
        lambda_min=minval(abs(Evals))
        if(lambda_min>tiny_r_kind) cond=abs(lambda_max/lambda_min) ! formal definition (lambda>0 for SPD matrix)
        if (iamroot_) then
           write(6,'(2a,1x,a,1x,es20.10)') 'Rcov(Evals) for Instrument: ', trim(instrument), ' cond= ', cond
           write(6,'(9(es13.6))') Evals
        endif
     endif
  else
     call die(myname_,'trouble solving eigenproblem')
  endif
  deallocate(work)
end subroutine decompose_
!EOC

!BOP
!
! !IROUTINE:  upd_varch_ ---  replace the obs error prescribed in satinfo for instrument accounted for inter-channel covariance.
!
! !INTERFACE:
!
subroutine upd_varch_
! !USES:
   use mpeu_util, only: die
   use radinfo, only: jpch_rad,iuse_rad,nusis,varch,varch_sea,varch_land,varch_ice,varch_snow,varch_mixed
   implicit none
! !DESCRIPTION: This routine will replace the prescribed obs errors in satinfo for instruments we account 
!               for inter-channel covariances.
!
! !REVISION HISTORY:
!   2014-11-26  W. Gu     Initial code
!   2019-02-26  kbathmann Update to be surface type dependent.
!   2019-08-12  W. Gu     Clean up the code, update varch_sea,varch_land etc directly by using indxR
!
! !REMARKS:
!   language: f90
!   machine:  discover
!
! !AUTHOR:
!   Wei Gu  org: gmao      date: 2014-11-26
!
!EOP
!-------------------------------------------------------------------------
!BOC

   character(len=*),parameter :: myname_=myname//'*upd_varch_'
   character(len=80) covtype
   integer(i_kind) :: nch_active,ii,jj,iii,jjj,mm,nn,ncp,ifound,jj0,itbl,ntrow
   integer(i_kind),dimension(6) ::nsatype
   integer(i_kind)::nsat,isurf,rr
   integer(i_kind),allocatable,dimension(:)   :: ircv
   integer(i_kind),allocatable,dimension(:)   :: ijac
   integer(i_kind),allocatable,dimension(:)   :: IRsubset
   integer(i_kind),allocatable,dimension(:)   :: IJsubset
   integer(i_kind) iinstr,indR
   integer(i_kind),allocatable,dimension(:) :: ich1  ! true channel number
   integer(i_kind),allocatable,dimension(:,:) :: tblidx
   integer(i_kind) :: nchanl1,jc   ! total number of channels in instrument
   if(.not.allocated(idnames)) then
     return
   endif
   ntrow = size(idnames)
   allocate(ich1(jpch_rad),tblidx(5,ntrow))

   nsatype=0
   do jj0=1,ntrow

      if (GSI_BundleErrorCov(jj0)%method > 1 .or. &
          GSI_BundleErrorCov(jj0)%method == 0) then

         covtype=trim(idnames(jj0))
         iinstr=len_trim(covtype)
         if(covtype(iinstr-3:iinstr)==':sea')then
            nsatype(1)=nsatype(1)+1
            nsatype(6)=nsatype(6)+1
            tblidx(1,nsatype(1))=jj0
         endif
         if(covtype(iinstr-4:iinstr)==':land')then
            nsatype(2)=nsatype(2)+1
            nsatype(6)=nsatype(6)+1
            tblidx(2,nsatype(2))=jj0
         endif
         if(covtype(iinstr-3:iinstr)==':ice')then
            nsatype(3)=nsatype(3)+1
            nsatype(6)=nsatype(6)+1
            tblidx(3,nsatype(3))=jj0
         endif
         if(covtype(iinstr-4:iinstr)==':snow')then
            nsatype(4)=nsatype(4)+1
            nsatype(6)=nsatype(6)+1
            tblidx(4,nsatype(4))=jj0
         endif
         if(covtype(iinstr-5:iinstr)==':mixed')then
            nsatype(5)=nsatype(5)+1
            nsatype(6)=nsatype(6)+1
            tblidx(5,nsatype(5))=jj0
         endif
      endif
   enddo

   if(nsatype(6)==0) return

   do isurf=1,5
      nsat=nsatype(isurf)
      if (nsat>0) then

         read_tab: do jj0=1,nsat

            itbl=tblidx(isurf,jj0) !a row number
            jc=0
            covtype = ''
            ich1=0
            do ii=1,jpch_rad
               if (isurf==1) then
                  covtype = trim(nusis(ii))//':sea'
               else if (isurf==2) then
                  covtype = trim(nusis(ii))//':land'
               else if (isurf==3) then
                  covtype = trim(nusis(ii))//':ice'
               else if (isurf==4) then
                  covtype = trim(nusis(ii))//':snow'
               else if (isurf==5) then
                  covtype = trim(nusis(ii))//':mixed'
               end if
               if(trim(idnames(itbl))==trim(covtype)) then
                  jc=jc+1
                  ich1(jc)=ii
               endif
            enddo
            nchanl1=jc

            if(.not.amiset_(GSI_BundleErrorCov(itbl))) then 
               if (iamroot_) write(6,*) trim(myname_), ' WARNING: Error Covariance not set for ',trim(idnames(itbl))
               cycle read_tab
            endif

            nch_active=GSI_BundleErrorCov(itbl)%nch_active
            if(nch_active<0) then
               if (iamroot_) write(6,*) trim(myname_), ' WARNING: No active channels for ',trim(idnames(itbl))
               return
            endif
            
            if(nchanl1==0) call die(myname_,' improperly set GSI_BundleErrorCov')            

            if(GMAO_ObsErrorCov)then
               do jj=1,nch_active
                  nn=GSI_BundleErrorCov(itbl)%indxR(jj)
                  mm=ich1(nn)
                  if(isurf==1) then 
                    if(iamroot_)write(6,'(1x,a6,a20,2i6,2f20.15)')'>>>',idnames(itbl),jj,nn,varch(mm),sqrt(GSI_BundleErrorCov(itbl)%R(jj,jj))
                    varch_sea(mm)=sqrt(GSI_BundleErrorCov(itbl)%R(jj,jj))
                  else if(isurf==2) then
                    varch_land(mm)=sqrt(GSI_BundleErrorCov(itbl)%R(jj,jj))
                  else if(isurf==3) then
                    varch_ice(mm)=sqrt(GSI_BundleErrorCov(itbl)%R(jj,jj))
                  else if(isurf==4) then
                    varch_snow(mm)=sqrt(GSI_BundleErrorCov(itbl)%R(jj,jj))
                  else if(isurf==5) then
                    varch_mixed(mm)=sqrt(GSI_BundleErrorCov(itbl)%R(jj,jj))
                  end if
               enddo
            else
               allocate(ircv(nchanl1))
               allocate(ijac(nchanl1))
               ircv = -1
               ijac = -1
               do jj=1,nchanl1
                  mm=ich1(jj)       ! true channel number (has no bearing here except in iuse)
                  if (iuse_rad(mm)>=1) then
                     ifound=-1
                     do ii=1,nch_active
                        if (GSI_BundleErrorCov(itbl)%nctot>nchanl1) then
                           indR=ii
                        else
                           indR=GSI_BundleErrorCov(itbl)%indxR(ii)
                        end if
                        if(jj==indR) then
                           ifound=ii
                           exit
                        endif
                     enddo
                     if(ifound/=-1) then
                        ijac(jj)=jj      ! index value in 1 to nchanl
                        ircv(jj)=ifound  ! index value in 1 to nch_active 
                     endif
                  endif 
               enddo
               ncp=count(ircv>0) ! number of active channels in profile
               allocate(IRsubset(ncp)) ! these indexes apply to the matrices/vec in ErrorCov
               allocate(IJsubset(ncp)) ! these indexes in 1 to nchanl
               iii=0;jjj=0
               do ii=1,nchanl1
                  if(ircv(ii)>0) then
                     iii=iii+1
                     IRsubset(iii)=ircv(ii)  ! subset indexes in R presently in use
                  endif
                  if(ijac(ii)>0) then
                     jjj=jjj+1
                     IJsubset(iii)=ijac(ii)  ! subset indexes in channels presently in use
                  endif
               enddo
               if (iii/=ncp .or. jjj/=ncp) then
                  if (iamroot_) then
                     write(6,*) myname, ' iii,jjj,ncp= ',iii,jjj,ncp
                  endif
                  call die(myname_,' serious dimensions insconsistency, aborting')
               endif
               do ii=1,ncp
                  nn=IJsubset(ii)
                  mm=ich1(nn)
                  rr=IRsubset(ii)
                  if(isurf==1) then
                    varch_sea(mm)=sqrt(GSI_BundleErrorCov(itbl)%R(rr,rr))
                  else if(isurf==2) then
                    varch_land(mm)=sqrt(GSI_BundleErrorCov(itbl)%R(rr,rr))
                  else if(isurf==3) then
                    varch_ice(mm)=sqrt(GSI_BundleErrorCov(itbl)%R(rr,rr))
                  else if(isurf==4) then
                    varch_snow(mm)=sqrt(GSI_BundleErrorCov(itbl)%R(rr,rr))
                  else if(isurf==5) then
                    varch_mixed(mm)=sqrt(GSI_BundleErrorCov(itbl)%R(rr,rr))
                  end if
               enddo
! clean up
               deallocate(IJsubset)
               deallocate(IRsubset)
               deallocate(ijac)
               deallocate(ircv)
            endif
         enddo read_tab !jj0=1,nsat
      endif !nsat >0
   enddo !isurf=1,5

   deallocate(ich1,tblidx)

end subroutine upd_varch_
!EOC
logical function adjust_jac_ (iinstr,nchanl,nsigradjac,ich,varinv,diagadd,depart,obs, &
                  err2,raterr2,wgtjo,jacobian,method,nchasm,rsqrtinv,rinvdiag)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    adjust_jac_
!
!   prgrmmr:     todling  org: gmao                date: 2014-04-15
!
! abstract:  provide hook to module handling inter-channel ob correlated errors
!
! program history log:
!   2014-04-15  todling - initial code
!   2014-08-06  todling - change obtype to isis for more flexibity
!   2014-10-01  todling - add wgtjo to arg list
!   2015-04-01  W. Gu   - clean the code
!   2015-08-18  W. Gu   - add the dependence of the correlated obs errors on the surface types.
!   2016-06-01  W. Gu   - move the function radinfo_adjust_jacobian from radinfo
!   2017-07-27  kbathmann  Merge subroutine rsqrtinv into scale_jac, define rinvdiag
!                          to fix diag_precon for correlated error, and reorder several nested loops
!   2019-04-22  kbathmann  change to cholesky factorization
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
!$$$ end documentation block
   use constants, only: zero,one
   use mpeu_util, only: die
   implicit none
   integer(i_kind), intent(in) :: nchasm
   integer(i_kind), intent(in) :: iinstr
   integer(i_kind), intent(in) :: nchanl
   integer(i_kind), intent(in) :: nsigradjac
   integer(i_kind), intent(in) :: ich(nchanl)
   integer(i_kind), intent(out) :: method
   real(r_kind), intent(in)    :: varinv(nchanl),diagadd(nchanl)
   real(r_kind), intent(inout) :: depart(nchanl),obs(nchanl)
   real(r_kind), intent(inout) :: err2(nchanl)
   real(r_kind), intent(inout) :: raterr2(nchanl)
   real(r_kind), intent(inout) :: wgtjo(nchanl)
   real(r_kind), intent(inout) :: jacobian(nsigradjac,nchanl)
   real(r_kind), intent(inout) :: rsqrtinv((nchasm*(nchasm+1))/2)
   real(r_kind), intent(inout) :: rinvdiag(nchasm)

   character(len=*),parameter::myname_ = myname//'*adjust_jac_'

   adjust_jac_=.false.

   if(.not.amiset_(GSI_BundleErrorCov(iinstr))) then
      if (iamroot_) write(6,*) 'WARNING: Error Covariance not set for ', &
                    trim(GSI_BundleErrorCov(iinstr)%instrument)
      return
   endif

   if( GSI_BundleErrorCov(iinstr)%nch_active < 0) return

   adjust_jac_ = scale_jac_ (depart,obs,err2,raterr2,jacobian,nchanl,varinv,diagadd,wgtjo, &
                             ich,nchasm,rsqrtinv,rinvdiag,GSI_BundleErrorCov(iinstr))

   method = GSI_BundleErrorCov(iinstr)%method

   end function adjust_jac_

!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  scale_jac_ ---  scale Jacbian, residuals, and related errors
!
! !INTERFACE:
!
logical function scale_jac_(depart,obs,err2,raterr2,jacobian,nchanl,varinv,diagadd,wgtjo, &
                            ich,nchasm,rsqrtinv,rinvdiag,ErrorCov)
! !USES:
   use constants, only: tiny_r_kind
   use radinfo, only: iuse_rad
   use mpeu_util, only: die
   implicit none
! !INPUT PARAMETERS:
   integer(i_kind),intent(in) :: nchasm
   integer(i_kind),intent(in) :: nchanl   ! total number of channels in instrument
   integer(i_kind),intent(in) :: ich(:)   ! true channel numeber
   real(r_kind),   intent(in) :: varinv(:)    ! inverse of specified ob-error-variance 
   real(r_kind),   intent(in) :: diagadd(:)    ! addition to diagonal before cholesky factorization
! !INPUT/OUTPUT PARAMETERS:
   real(r_kind),intent(inout) :: depart(:)    ! observation-minus-guess departures
   real(r_kind),intent(inout) :: obs(:)       ! observations
   real(r_kind),intent(inout) :: err2(:)  ! input: square of inverse of original obs errors
   real(r_kind),intent(inout) :: raterr2(:)  ! input: square of original obs error/inflated obs errors
   real(r_kind),intent(inout) :: wgtjo(:)     ! weight in Jo-term
   real(r_kind),intent(inout) :: jacobian(:,:)! Jacobian matrix
   real(r_kind),intent(inout) :: rsqrtinv(:)
   real(r_kind),intent(inout) :: rinvdiag(:)
   type(ObsErrorCov),intent(inout) :: ErrorCov      

! !DESCRIPTION: This routine is the main entry-point to the outside world. 
!               It redefines the Jacobian matrix so it embeds the inverse of the square root 
!               observation error covariance matrix. Only the sub-matrix related
!               to the active and accepted channels in the given profile is 
!               taken into account.
!
! !REVISION HISTORY:
!   2014-04-13  todling  initial code
!   2014-11-15  W. Gu    bug fix in R-inverse indexation
!   2014-12-19  W. Gu    use the eigenvalue decomposition to form a square root decomposition, and then
!                        apply to correlated R-covariance matrix(R= QD^(1/2)Q^T QD^(1/2)Q^T). 
!   2015-04-01  W. Gu    clean the code
!   2016-04-18  W. Gu    combine QC inflation factors into the correlated obs errors(method=2)
!   2016-10-28  W. Gu    merge the code for method=1 and method=2 together
!   2016-10-28  W. Gu    remove rsqrtinv_ and do the inverse of sqrt(R) directly here.
!   2019-04-22  kbathmann & W. Gu   use of Cholesky factorization of R to update the OMF and Jacobian
!
! !REMARKS:
!   language: f90
!   machine:  discover
!
! !AUTHOR:
!   Ricardo Todling  org: gmao      date: 2014-04-13
!
!EOP
!-------------------------------------------------------------------------
!BOC

   character(len=*),parameter :: myname_=myname//'*scale_jac'
   integer(i_kind) :: chan_count
   integer(i_kind) :: nch_active,ii,jj,kk,iii,jjj,mm,nn,ncp,ifound,nsigjac,indR
   integer(i_kind),allocatable,dimension(:)   :: ircv
   integer(i_kind),allocatable,dimension(:)   :: ijac
   integer(i_kind),allocatable,dimension(:)   :: IRsubset
   integer(i_kind),allocatable,dimension(:)   :: IJsubset
   real(r_quad),   allocatable,dimension(:)   :: col,col2
   real(r_quad),   allocatable,dimension(:,:) :: row
   real(r_kind),   allocatable,dimension(:)   :: qcaj
   real(r_kind),   allocatable,dimension(:,:) :: UT
   logical subset

   scale_jac_=.false.
   nch_active=ErrorCov%nch_active

   call timer_ini('scljac')

! get indexes for the internal channels matching those
! used in estimating the observation error covariance
   allocate(ircv(nchanl))
   allocate(ijac(nchanl))
   ircv = -1
   ijac = -1
   do jj=1,nchanl
     mm=ich(jj)       ! true channel number (has no bearing here except in iuse)
     if (varinv(jj)>tiny_r_kind .and. iuse_rad(mm)>=1) then
       ifound=-1
       do ii=1,nch_active
          if(GMAO_ObsErrorCov)then
             if(jj==ErrorCov%indxR(ii)) then
                ifound=ii       
                exit
             endif
          else
             if (ErrorCov%nctot>nchanl) then
                indR=ii
             else
                indR=ErrorCov%indxR(ii)
             end if
             if(jj==indR) then
                ifound=ii
                exit
             endif
          endif
       enddo
       if(ifound/=-1) then
         ijac(jj)=jj      ! index value applies to the jacobian and departure
         ircv(jj)=ifound  ! index value applies to ErrorCov
       endif
     endif
   enddo

! following should never happen, but just in case ...
   ncp=count(ircv>0) ! number of active channels in profile
   if(ncp==0 .or. ncp>nch_active) then
     call die(myname_,'serious inconsitency in handling correlated obs')
   endif
   if(ncp /= nchasm) then
     call die(myname_,'serious inconsitency in handling correlated obs: ncp .ne. nchasm')
   endif

! Get subset indexes; without QC and other on-the-fly analysis choices these
!                     two indexes would be the same, but because the analysis
!                     remove data here and there, most often there will be less
!                     channels being processed for a given profile than the set
!                     of active channels used to get an offline estimate of R.
   allocate(IRsubset(ncp)) ! these indexes apply to the matrices/vec in ErrorCov
   allocate(IJsubset(ncp)) ! these indexes apply to the Jacobian/departure 
   iii=0;jjj=0
   do ii=1,nchanl
     if(ircv(ii)>0) then
       iii=iii+1
       IRsubset(iii)=ircv(ii)  ! subset indexes in R presently in use
     endif
     if(ijac(ii)>0) then
       jjj=jjj+1
       IJsubset(iii)=ijac(ii)  ! subset indexes in Jac/dep presently in use
     endif
   enddo
   if (iii/=ncp .and. jjj/=ncp) then
     if (iamroot_) then
       write(6,*) myname, ' iii,ncp= ',iii,jjj,ncp
     endif
     call die(myname_,' serious dimensions insconsistency (R), aborting')
   endif

   if( ErrorCov%method<0 ) then
!  Keep departures and Jacobian unchanged
!  Do as GSI would do otherwise
     do jj=1,ncp
       mm=IJsubset(jj)
       wgtjo(mm)    = varinv(mm)
     enddo
   else
     if( ErrorCov%method== 0 ) then

             ! use diag(Re) replaces GSI specified errors
             !    inv(Rg) = inv(De)

       do jj=1,ncp
          mm=IJsubset(jj)
          err2(mm) = one/ErrorCov%R(IRsubset(jj),IRsubset(jj))
          if(.not.lqcoef)raterr2(mm) = one
          wgtjo(mm)   = raterr2(mm)/ErrorCov%R(IRsubset(jj),IRsubset(jj))
       enddo

     else if( ErrorCov%method==1 .or. ErrorCov%method== 2) then

     !  case=1 is default; uses corr(Re) only
     !  case=2: uses full Re;

! decompose the sub-matrix - returning the result in the 
!                            structure holding the full covariance
       allocate(UT(ncp,ncp))
       if( ErrorCov%method==2 ) then
         if(lqcoef)then
           allocate(qcaj(ncp))
           do jj=1,ncp
             qcaj(jj) = raterr2(IJsubset(jj))
           enddo
           subset = choleskydecom_inv_ (IRsubset,IJsubset,ErrorCov,UT,diagadd,qcaj)
           deallocate(qcaj)
         else
           subset = choleskydecom_inv_ (IRsubset,IJsubset,ErrorCov,UT,diagadd) 
         endif
       else if( ErrorCov%method==1 ) then
         allocate(qcaj(ncp))
         do jj=1,ncp
           qcaj(jj) = varinv(IJsubset(jj))
         enddo
         subset = choleskydecom_inv_ (IRsubset,IJsubset,ErrorCov,UT,diagadd,qcaj)
         deallocate(qcaj)

       endif
       if(.not.subset) then
         call die(myname_,' failed to decompose correlated R')
       endif

       chan_count = 0
       do ii=1,ncp 
         do jj=1,ii
           chan_count = chan_count + 1
           rsqrtinv(chan_count) = UT(jj,ii)
         enddo
       enddo

       do ii=1,ncp
         do kk=ii,ncp 
           rinvdiag(ii)=rinvdiag(ii)+UT(ii,kk)**2
         enddo
       end do

       nsigjac=size(jacobian,1)
       allocate(row(nsigjac,ncp))
       allocate(col(ncp),col2(ncp))
!$omp parallel do  schedule(dynamic,1) private(ii,jj,nn)
       do ii=1,ncp
         row(:,ii)=zero_quad
         col(ii)=zero_quad
         col2(ii)=zero_quad
         do jj=1,ii 
            nn=IJsubset(jj)
            col(ii)   = col(ii)   + UT(jj,ii) * depart(nn)
            col2(ii)  = col2(ii)  + UT(jj,ii) * obs(nn)
            row(:,ii) = row(:,ii) + UT(jj,ii) * jacobian(:,nn)
         enddo
       enddo
       deallocate(UT)

!     Place Jacobian and departure in output arrays
       do ii=1,ncp
         mm=IJsubset(ii)
         depart(mm)=col(ii)
         obs(mm)=col2(ii)
         jacobian(:,mm)=row(:,ii)
         raterr2(mm) = one
         err2(mm) = one
         wgtjo(mm)    = one
       enddo

       deallocate(col,col2)
       deallocate(row)

     else if( ErrorCov%method==3 ) then   !use diag(Re) scales GSI specified errors
                                          !    inv(Rg) = inv(De*Dg)
       do jj=1,ncp
          mm=IJsubset(jj)
          raterr2(mm) = raterr2(mm)/ErrorCov%Revals(IRsubset(jj))
          err2(mm) = err2(mm)
          wgtjo(mm)   = varinv(mm)/ErrorCov%Revals(IRsubset(jj))
       enddo
       
       
     endif

   endif

! clean up
   deallocate(IJsubset)
   deallocate(IRsubset)
   deallocate(ijac)
   deallocate(ircv)

   scale_jac_=.true.

   call timer_fnl('scljac')

end function scale_jac_
!EOC

!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  choleskydecom_inv_ ---  compute Choleskyi factorization of cov(R), i.e.,
!                                    R = U^T * U, then invert U
!
! !INTERFACE:
!
logical function choleskydecom_inv_(Isubset,IJsubset,ErrorCov,UT,diagadd,qcaj)
! !USES:
  implicit none
  integer(i_kind),intent(in) :: Isubset(:),IJsubset(:)
  real(r_kind),intent(inout) :: UT(:,:)
  real(r_kind),intent(in   ) :: diagadd(:)
  real(r_kind),optional,intent(in) :: qcaj(:)
  type(ObsErrorCov),intent(in) :: ErrorCov
! !DESCRIPTION: This routine makes a LAPACK call to Cholesky factorization of cov(R),
!               then inverts the lower triangular matrix.
!
! !REVISION HISTORY:
!   2019-04-22  kbathmann/Wei  initial code
!
! !REMARKS:
!   language: f90
!   machine:  discover
!
! !AUTHORS:
!   Kristen Bathmann, EMC  date: 2019-04-22
!   Wei Gu  org: gmao      date: 2019-04-22
!
!EOP
!-------------------------------------------------------------------------
!BOC
  character(len=*),parameter :: myname_=myname//'choleskydecom_inv_'
  integer(i_kind) ii,jj,ncp
  integer(i_kind) info,info1

  choleskydecom_inv_=.false. 
  ncp=size(Isubset) ! number of channels actually used in this profile

! extract subcomponent of R
  if( present(qcaj) ) then
    do jj=1,ncp
      do ii=1,ncp
        UT(ii,jj) = ErrorCov%R(Isubset(ii),Isubset(jj))/sqrt(qcaj(ii)*qcaj(jj))
      enddo
      UT(jj,jj) = UT(jj,jj)+diagadd(IJsubset(jj))
    enddo
  else 
    do jj=1,ncp
      do ii=1,ncp
        UT(ii,jj) = ErrorCov%R(Isubset(ii),Isubset(jj))
      enddo
      UT(jj,jj) = UT(jj,jj)+diagadd(IJsubset(jj))
    enddo
  endif
  if(r_kind==r_single) then ! this trick only works because this uses the f77 lapack interfaces
     call SPOTRF('U', ncp, UT, ncp, info )
  else if(r_kind==r_double) then
     call DPOTRF('U', ncp, UT, ncp, info )
  endif
  if (info==0) then
     if(r_kind==r_single) then
        call STRTRI('U', 'N', ncp, UT, ncp, info1 )
     else if(r_kind==r_double) then
        call DTRTRI('U', 'N', ncp, UT, ncp, info1 )
     endif
     if(info1 /= 0)call die(myname_,'trouble inverting upper triangular matrix ')
  else
     call die(myname_,'trouble performing cholesky factorization') 
  endif

  choleskydecom_inv_=.true.

end function choleskydecom_inv_
!EOC

!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  amiset_ --- checks whether a particular cov(R) has been set or not
!
! !INTERFACE:
!

logical function amiset_ (ErrorCov)
implicit none
! !INPUT/OUTPUT PARAMETERS:
type(ObsErrorCov),intent(in) :: ErrorCov

! !DESCRIPTION: This routine returns the status of a particular instance of 
!               the FORTRAN typing holding the observation error covariance.
!
! !REVISION HISTORY:
!   2014-04-13  todling  initial code
!
! !REMARKS:
!   language: f90
!   machine:  discover
!
! !AUTHOR:
!   Ricardo Todling  org: gmao      date: 2014-04-13
!
!EOP
!-------------------------------------------------------------------------
!BOC
logical failed
failed=.false.
amiset_=.false.
if(ErrorCov%nch_active<0) failed=.true.
if(.not.associated(ErrorCov%indxR)) failed=.true.
if(.not.associated(ErrorCov%R)) failed=.true.
if(.not.associated(ErrorCov%REvals)) failed=.true.
if(.not.failed) amiset_=.true.
end function amiset_
!EOC

!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  fnl_ --- destroy all instances of cov(R)
!
! !INTERFACE:
!
subroutine fnl_
implicit none

! !DESCRIPTION: Deallocates space held for observation error covariance.
!
! !REVISION HISTORY:
!   2014-04-13  todling  initial code
!
! !REMARKS:
!   language: f90
!   machine:  discover
!
! !AUTHOR:
!   Ricardo Todling  org: gmao      date: 2014-04-13
!
!EOP
!-------------------------------------------------------------------------
!BOC
integer(i_kind) ii,ndim
if(.not.initialized_) return
ndim=size(GSI_BundleErrorCov)
do ii=1,ndim
   call destroy_(GSI_BundleErrorCov(ii))
enddo
deallocate(GSI_BundleErrorCov)
if(allocated(idnames)) deallocate(idnames)
if(allocated(instruments)) deallocate(instruments)
end subroutine fnl_
!EOC

end module correlated_obsmod

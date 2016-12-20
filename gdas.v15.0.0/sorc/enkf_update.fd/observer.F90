module observermod
!#define VERBOSE
#include "mytrace.H"

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    observermod   driver for gridpoint statistical 
!                                     interpolation
!   prgmmr: parrish          org: np22                date: 1990-10-06
!
! abstract: executes gridpoint spectral interpolation analysis.
!
! program history log:
!   2009-01-28  todling - split observer into init/set/run/finalize
!   2009-08-19  guo     - modified to support multi-pass observer_run
!   2009-09-14  guo     - moved compact_diff related statements from
!			  glbsoi here.
!   2010-05-19  todling - revist initialization/finalization of chem
!   2011-04-29  todling - add metguess initialization/finalization
!   2013-07-02  parrish - changes to remove tlnmc_type for global tlnmc and add
!                          reg_tlnmc_type for two kinds of regional tlnmc.
!   2013-10-19  todling - update cloud_efr module name
!   2014-02-03  todling - remove B-dependence; move cost-create/destroy out
!   2014-12-03  derber - modify for possibly not using obsdiag arrays, change
!                        call to getsfc
!
!   input argument list:
!     mype - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: i_kind
  use constants, only: rearth
  use mpimod, only: mype,npe
  use jfunc, only: miter,jiter,jiterstart,&
       switch_on_derivatives,tendsflag
  use gridmod, only: nlat,nlon,rlats,regional,twodvar_regional,wgtlats,nsig,&
                     lat2,lon2
  use guess_grids, only: create_ges_grids,create_sfc_grids,&
       destroy_ges_grids,destroy_sfc_grids,nfldsig
  use cloud_efr_mod, only: cloud_init,cloud_final
  use obsmod, only: write_diag,obs_setup,ndat,dirname,lobserver,ndat,nobs_sub, &
       lread_obs_skip,nprof_gps,ditype,obs_input_common,iadate,luse_obsdiag
  use satthin, only: superp,super_val1,getsfc,destroy_sfc
  use gsi_4dvar, only: l4dvar
  use convinfo, only: convinfo_destroy
  use m_gsiBiases, only : create_bias_grids, destroy_bias_grids
  use m_berror_stats, only: berror_get_dims
  use m_berror_stats_reg, only: berror_get_dims_reg
  use timermod, only: timer_ini, timer_fnl
  use read_obsmod, only: read_obs
  use lag_fields, only: lag_guessini

  use mod_strong, only: l_tlnmc,reg_tlnmc_type
  use mod_vtrans, only: nvmodes_keep,create_vtrans,destroy_vtrans
  use strong_fast_global_mod, only: init_strongvars
  use zrnmi_mod, only: zrnmi_initialize
  use turblmod, only: create_turblvars,destroy_turblvars

  use guess_grids, only: create_chemges_grids, destroy_chemges_grids
  use guess_grids, only: create_metguess_grids, destroy_metguess_grids

  implicit none

  private
! public observer_guess_init     ! RT: doesn't really belong here
  public observer_init
  public observer_set
  public observer_run
  public observer_finalize
! public observer_guess_finalize ! RT: doesn't really belong here
  public ndata

  integer(i_kind),allocatable,dimension(:,:):: ndata

  interface observer_guess_init
     module procedure guess_init_
  end interface
  interface observer_init
     module procedure init_
  end interface
  interface observer_set
     module procedure set_
  end interface
  interface observer_run
     module procedure run_
  end interface
  interface observer_finalize
     module procedure final_
  end interface
  interface observer_guess_finalize
     module procedure guess_final_
  end interface

  logical,save:: iamset_         = .false.
  logical,save:: fg_initialized_ = .false.

  logical,save:: ob_initialized_ = .false.

contains

subroutine guess_init_
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    observer_guess_init  driver for gridpoint statistical 
!                                     interpolation
!   prgmmr: parrish          org: np22                date: 1990-10-06
!
! abstract: executes gridpoint spectral interpolation analysis.
!
! program history log:
!   1990-10-06  parrish
!   2007-10-03  todling - created this file from slipt of glbsoi
!   2009-01-28  todling - split observer into init/set/run/finalize
!   2009-03-10  meunier - read in the original position of balloons
!   2010-04-20  todling - add call to create tracer grid
!   2010-05-19  todling - update interface to read_guess
!   2010-06-25  treadon - pass mlat into create_jfunc
!   2011-05-24  yang    - pass iadate(3) (day of the month) into read_guess
!   2014-02-03  todling - remove dependence on B & major cost func settings
!
!   input argument list:
!     mype - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use compact_diffs,only: cdiff_created
  use compact_diffs,only: cdiff_initialized
  use compact_diffs,only: create_cdiff_coefs, inisph
  use mp_compact_diffs_mod1, only: init_mp_compact_diffs1
  use mpeu_util, only: die
  implicit none

! Declare passed variables

! Declare local variables

  integer(i_kind):: ierr

!*******************************************************************************************
!
  if( fg_initialized_ ) call die('observer.guess_init_','already initialized')
  fg_initialized_ = .true.

! Allocate arrays to hold surface and sigma guess fields.
#ifndef HAVE_ESMF
  call create_metguess_grids(mype,ierr)
  call create_chemges_grids(mype,ierr)
#endif /*/ HAVE_ESMF */

  call create_ges_grids(switch_on_derivatives,tendsflag)
  call create_bias_grids()
  call create_sfc_grids()
  call cloud_init()

! Read model guess fields.
  call read_guess(iadate(1),iadate(2),iadate(3),mype)

! Intialize lagrangian data assimilation and read in initial position of balloons
  if(l4dvar) then
#ifdef _LAG_MODEL_
    call lag_guessini()
#endif /* _LAG_MODEL_ */
  endif
 
! Read output from previous min.
  if (l4dvar.and.jiterstart>1) then
  else
  ! If requested and if available, read guess solution.
  endif

! Generate coefficients for compact differencing
  if(.not.regional)then
     if(.not.cdiff_created()) call create_cdiff_coefs()
     if(.not.cdiff_initialized()) call inisph(rearth,rlats(2),wgtlats(2),nlon,nlat-2)
     call init_mp_compact_diffs1(nsig+1,mype,.false.)
  endif

  if (tendsflag) then
     call create_turblvars()
  endif
  if ( (l_tlnmc) .and. nvmodes_keep>0) then
     call create_vtrans(mype)
     if(regional) then
        if(reg_tlnmc_type==1) call zrnmi_initialize(mype)
        if(reg_tlnmc_type==2) call fmg_initialize_e(mype)
     else
        call init_strongvars(mype)
     end if
  end if

! End of routine
end subroutine guess_init_


subroutine init_
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    observer_init      driver for gridpoint statistical 
!                                     interpolation
!   prgmmr: parrish          org: np22                date: 1990-10-06
!
! abstract: executes gridpoint spectral interpolation analysis.
!
! program history log:
!   1990-10-06  parrish
!   2007-10-03  todling - created this file from slipt of glbsoi
!   2009-01-28  todling - split observer into init/set/run/finalize
!
!   input argument list:
!     mype - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use mpeu_util,only : tell, die
  implicit none
  character(len=*),parameter:: Iam='observer_init'

! Declare passed variables

! Declare local variables
_ENTRY_(Iam)


  if(ob_initialized_) call die(Iam,'already initialized')
  ob_initialized_=.true.
  iamset_ = .false.

#ifdef VERBOSE
  call tell('observer.init_','entered')
#endif
!*******************************************************************************************
!
! Initialize timer for this procedure
  call timer_ini('observer')
  call timer_ini('observer.init_')
#ifdef VERBOSE
  call tell('observer.init_','timer_ini_()')
#endif

! Initialize guess
  call guess_init_
#ifdef VERBOSE
  call tell('observer.init_','guess_init_()')
#endif

!     ndata(*,1)- number of prefiles retained for further processing
!     ndata(*,2)- number of observations read
!     ndata(*,3)- number of observations keep after read
#ifdef VERBOSE
  call tell('observer.init_','ndat =',ndat)
#endif
  allocate(ndata(ndat,3))
#ifdef VERBOSE
  call tell('observer.init_','allocate(ndata)')


  call tell('observer.init_','exiting')
#endif
  if(mype==0) write(6,*) Iam, ': successfully initialized'
! End of routine
  call timer_fnl('observer.init_')
_EXIT_(Iam)
end subroutine init_

subroutine set_
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    observer_set       driver for gridpoint statistical 
!                                     interpolation
!   prgmmr: parrish          org: np22                date: 1990-10-06
!
! abstract: executes gridpoint spectral interpolation analysis.
!
! program history log:
!   1990-10-06  parrish
!   2007-10-03  todling - created this file from slipt of glbsoi
!   2009-01-28  todling - split observer into init/set/run/finalize
!
!   input argument list:
!     mype - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use mpeu_util, only: tell,die
  use gsi_io, only: mype_io
  implicit none
  character(len=*), parameter :: Iam="observer_set"

! Declare passed variables

! Declare local variables
  logical:: lhere
  integer(i_kind):: lunsave,istat1,istat2,istat3,ndat_old,npe_old
  
  data lunsave  / 22 /
_ENTRY_(Iam)

!*******************************************************************************************
  call timer_ini('observer.set_')
 
  if ( iamset_ ) call die(Iam,'already set')

! Create file names for pe relative observation data.  obs_setup files are used
! in outer loop setup routines. 
  obs_setup = trim(dirname) // 'obs_setup'

! Read observations or collective observation information
  if (.not.lread_obs_skip) then

!    Read observations from data files
     call read_obs(ndata,mype)

  else

!    Read collective obs selection information to scratch file.
     inquire(file=obs_input_common,exist=lhere)
     if (.not.lhere) then
        if (mype==0) write(6,*)'OBSERVER_SET:  ***ERROR*** file ',&
             trim(obs_input_common),' does NOT exist.  Terminate execution'
        call stop2(329)
     endif
  
     allocate(nobs_sub(npe,ndat))
     open(lunsave,file=obs_input_common,form='unformatted')
     read(lunsave,iostat=istat1) ndata,ndat_old,npe_old,superp,nprof_gps,ditype
     allocate(super_val1(0:superp))
     read(lunsave,iostat=istat2) super_val1
     if (istat1/=0 .or. istat2/=0) then
        if (mype==0) write(6,*)'OBSERVER_SET:  ***ERROR*** reading file ',&
             trim(obs_input_common),' istat1,istat2=',istat1,istat2,'  Terminate execution'
        call stop2(329) 
     endif
     if(npe_old /= npe .or. ndat /= ndat_old) then
        if (mype==0) write(6,*) ' observer_set: inconsistent ndat,npe ',ndat,npe,    &
               ' /= ',ndat_old,npe_old
        call stop2(330)
     end if
     read(lunsave,iostat=istat3) nobs_sub
     if(istat3 /= 0) then
        if(mype == 0) write(6,*) ' observer_set: error reading nobs_sub '
        call stop2(331)
     end if
     close(lunsave)

     if (mype==0) write(6,*)'OBSERVER_SET:  read collective obs selection info from ',&
          trim(obs_input_common)

!    Load isli2 and sno2 arrays using fields from surface guess file.  Arrays
!    isli2 and sno2 are used in intppx (called from setuprad) and setuppcp.
     call getsfc(mype,mype_io,.false.,.false.)
     call destroy_sfc

  endif
  
! Locate observations within each subdomain (not to call in 4dvar inner loop)
  if(lobserver.or.(.not.l4dvar)) call obs_para(ndata,mype)

  iamset_ = .true.

! End of routine
  call timer_fnl('observer.set_')
_EXIT_(Iam)
end subroutine set_

subroutine run_(init_pass,last_pass)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    observer_run       driver for gridpoint statistical 
!                                     interpolation
!   prgmmr: parrish          org: np22                date: 1990-10-06
!
! abstract: executes gridpoint spectral interpolation analysis.
!
! program history log:
!   1990-10-06  parrish
!   2007-10-03  todling - created this file from slipt of glbsoi
!   2009-01-28  todling - split observer into init/set/run/finalize
!
!   input argument list:
!     mype - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use mpeu_util, only: tell,die
  implicit none
  logical,optional,intent(in) :: init_pass
  logical,optional,intent(in) :: last_pass

! Declare passed variables

! Declare local variables
  character(len=*), parameter :: Iam="observer_run"

  integer(i_kind) jiterlast
  logical :: last
  character(len=12) :: clfile
  logical :: init_pass_
  logical :: last_pass_
  
_ENTRY_(Iam)
  call timer_ini('observer.run_')
  init_pass_=.false.
  if(present(init_pass)) init_pass_=init_pass
  last_pass_=.false.
  if(present(last_pass)) last_pass_= last_pass

#ifdef VERBOSE
  call tell(Iam,'init_pass =',init_pass_)
  call tell(Iam,'last_pass =',last_pass_)
#endif

  if(.not.ob_initialized_) call die(Iam,'not initialized')

!*******************************************************************************************
 
! Read observations and scatter, if it is not already been done.
  if(.not.iamset_) call set_

  jiterlast=miter
  if (l4dvar) then
     jiterlast=jiterstart
  else
     write(6,*)'observer should only be called in 4dvar'
     call stop2(157)
  endif
#ifdef VERBOSE
  if(mype==0) then
     call tell(Iam,'miter =',miter)
     call tell(Iam,'jiterstart =',jiterstart)
     call tell(Iam,'jiterlast  =',jiterlast )
  endif
#endif
  if (mype==0) write(6,*)'OBSERVER: jiterstart,jiterlast=',jiterstart,jiterlast

! Main outer analysis loop
  do jiter=jiterstart,jiterlast

!    Set up right hand side of analysis equation
     call setuprhsall(ndata,mype,init_pass_,last_pass_)

     last  = jiter == miter+1 ! there is no obsdiags output if
                              ! jiterstart==miter+1.  e.g. miter=2 and jiterstart=3
     if (l4dvar.and.(.not.last) .and. last_pass_ .and. luse_obsdiag) then
        clfile='obsdiags.ZZZ'
        write(clfile(10:12),'(I3.3)') jiter
        call write_obsdiags(clfile)
     endif

! End of outer iteration loop
  end do

  if (.not.l4dvar) then
     jiter=miter+1
!    If requested, write obs-anl information to output files
     if (write_diag(jiter)) call setuprhsall(ndata,mype,.true.,.true.)

  endif

  call timer_fnl('observer.run_')
! End of routine
_EXIT_(Iam)
end subroutine run_

subroutine final_
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    observer_final     driver for gridpoint statistical 
!                                     interpolation
!   prgmmr: parrish          org: np22                date: 1990-10-06
!
! abstract: executes gridpoint spectral interpolation analysis.
!
! program history log:
!   1990-10-06  parrish
!   2007-10-03  todling - created this file from slipt of glbsoi
!   2009-01-28  todling - split observer into init/set/run/finalize
!
!   input argument list:
!     mype - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use compact_diffs,only: destroy_cdiff_coefs
  use mp_compact_diffs_mod1, only: destroy_mp_compact_diffs1
  use mpeu_util, only: tell,die
  implicit none

! Declare passed variables

! Declare local variables
  character(len=*),parameter:: Iam="observer_final"

!*******************************************************************************************
_ENTRY_(Iam)
  call timer_ini('observer.final_')

  if(.not.ob_initialized_) call die(Iam,'not initialized')
  ob_initialized_=.false.
 
  if (tendsflag) then
     call destroy_turblvars()
  endif
  if ( (l_tlnmc ) .and. nvmodes_keep>0) call destroy_vtrans

  if(.not.regional)then
     call destroy_cdiff_coefs()
     call destroy_mp_compact_diffs1()
  endif
  call guess_final_

! Deallocate arrays
  call convinfo_destroy

  deallocate(ndata)
  if(mype==0) write(6,*) Iam, ': successfully finalized'

! Finalize timer for this procedure
  call timer_fnl('observer.final_')
  call timer_fnl('observer')

! End of routine
_EXIT_(Iam)
end subroutine final_

subroutine guess_final_
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    observer_final     driver for gridpoint statistical 
!                                     interpolation
!   prgmmr: parrish          org: np22                date: 1990-10-06
!
! abstract: executes gridpoint spectral interpolation analysis.
!
! program history log:
!   1990-10-06  parrish
!   2007-10-03  todling - created this file from slipt of glbsoi
!   2009-01-28  todling - split observer into init/set/run/finalize
!   2010-04-20  todling - add call to destroy tracer grid
!   2013-10-23  todling - first-in, last-out alloc/dealloc
!
!   input argument list:
!     mype - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use mpeu_util, only: die
  implicit none

! Declare passed variables

! Declare local variables
  integer(i_kind):: ierr

!*******************************************************************************************
  if ( .not. fg_initialized_ ) call die('observer.guess_final_','object not initialized')
  fg_initialized_=.false.
 
! Deallocate remaining arrays
  call cloud_final()
  call destroy_sfc_grids()
  call destroy_bias_grids()
  call destroy_ges_grids
#ifndef HAVE_ESMF
  call destroy_chemges_grids(ierr)
  call destroy_metguess_grids(mype,ierr)
#endif /* HAVE_ESMF */

! End of routine
end subroutine guess_final_

end module observermod

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  write_all --- Write various output files
!
! !INTERFACE:
!

subroutine write_all(increment)

! !USES:

  use kinds, only: i_kind,r_kind
  
  use mpimod, only: npe,mype

  use constants, only: zero
  
  use jfunc, only: bcoption

  use gridmod, only: regional,fv3_regional
  
  use guess_grids, only: ntguessig

  use m_gsiBiases, only: gsi_bkgbias_bundle
  use m_gsibiases ,only: nbc

  use gsi_bias, only: write_bias

! use regional_io, only: write_regional_analysis
  use regional_io_mod, only: regional_io_class
  use gsi_rfv3io_mod, only: wrfv3_netcdf
  use gsi_rfv3io_mod, only: bg_fv3regfilenameg 

  use ncepgfs_io, only: write_gfs

  use gsi_bundlemod, only: gsi_bundle, gsi_bundlegetpointer
  use gsi_metguess_mod, only: gsi_metguess_bundle

  use mpeu_util, only: die

  use control_vectors, only: control_vector

  implicit none

! !INPUT PARAMETERS:

  integer(i_kind), intent(in   ) :: increment  ! when >0 write out w/ increment

! !OUTPUT PARAMETERS:

! !DESCRIPTION: This routine writes various output files at the end of 
!           a gsi run.  Output files written by this routine include
!    \begin{itemize}
!          \item updated radiance bias correction coefficients
!          \item updated precipitation bias correction coefficients
!          \item regional analysis grid (grid space)
!          \item global atmospheric analysis (spectral coefficients)
!          \item global surface analysis (grid space)
!          \item global bias correction fields (spectral coefficients)
!          \item analysis increment (grid space)
!    \end{itemize}
!
!           Not all of the above files are written in any given gsi
!           run.  Creation of output files is controlled by various
!           flags as indicated in the code below
!
! !REVISION HISTORY:
!
!   1990-10-10  parrish
!   1998-07-10  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-06-15  treadon - update documentation
!   2004-07-15  todling - protex-compliant prologue; added intent/only's
!   2004-11-30  parrish - modify regional calls for netcdf and binary output
!   2004-12-29  treadon - repackage regional analysis write 
!   2005-02-09  kleist  - remove open and close for iosfc
!   2005-03-10  treadon - remove iadate and igdate
!   2005-05-27  pondeca - bypass radinfo_ and pcpinfo_write when twodvar_regional=.t.
!   2005-06-27  guo     - added interface to GMAO gridded fields
!   2005-07-25  treadon - remove "use m_checksums,only : checksums_show" since not used
!   2005-12-09  guo     - comments added
!   2006-01-10  treadon - use ncepgfs_io module
!   2006-03-13  treadon - increase filename to 24 characters
!   2006-04-14  treadon - replace call write_gfsatm for bias with write_bias
!   2006-06-08  zhang,b - change "biascor>0" to "biascor>=0" for debug purpose
!   2006-07-31  kleist  - use ges_ps instead of ln(ps)
!   2006-12-04  todling - merge in GMAO bias correction changes
!   2007-07-20  todling - move solution write-out call to glbsoi
!   2007-11-12  todling - move write of sat/pcp bias to glbsoi
!   2009-01-28  todling - move ESMF if from glbsoi to this routine
!                       - remove original GMAO interface
!   2010-10-18  hcHuang - add flag use_gfs_nemsio and link to read_nems and read_nems_chem
!   2013-10-19  todling - metguess holds ges fields now
!   2014-10-05  todling - background biases now held in bundle
!   2017-10-10  Wu W    - add FV3 option for regional output
!   2020-11-19  Lu & Wang - modify output filename option for fgat. POC:  xuguang.wang@ou.edu
!
! !REMARKS:
!
!   language: f90
!   machine:  ibm RS/6000 SP; SGI Origin 2000; Compaq HP
!
! !AUTHOR: parrish          org: np22                date: 1990-10-10
!
!EOP
!-------------------------------------------------------------------------

! Declare local variables
  character(24):: filename
  integer(i_kind) mype_atm,mype_bias,mype_sfc,iret_bias,ier
  real(r_kind),dimension(:,:),pointer::ges_z=>NULL()
  type(regional_io_class) :: io 

#ifndef HAVE_ESMF
!********************************************************************


! Regional output
  if (regional) then
     if (fv3_regional) then
        call wrfv3_netcdf(bg_fv3regfilenameg(ntguessig))
     else
        call io%write_regional_analysis(mype)
     endif
  endif



! Global output
  if(.not.regional) then

!    NCEP GFS interface

!    Write atmospheric and surface analysis
     mype_atm=0
     mype_sfc=npe/2
     call write_gfs(increment,mype_atm,mype_sfc)

!    Write file bias correction     
     if(abs(bcoption)>0)then
        call gsi_bundlegetpointer (gsi_metguess_bundle(ntguessig),'z',ges_z,ier)
        if(ier/=0)  call die('write_all',': missing require guess, aborting ',ier)
        filename='biascor_out'
        mype_bias=npe-1
        call write_bias(filename,mype_bias,nbc,&
             ges_z,gsi_bkgbias_bundle,iret_bias)
     endif

! End of global block
  end if

#else /* HAVE_ESMF */

!    Write file bias correction     
     if(abs(bcoption)>0)then
        call gsi_bundlegetpointer (gsi_metguess_bundle(ntguessig),'z',ges_z,ier)
        if(ier/=0)  call die('write_all',': missing require guess, aborting ',ier)
        filename='biascor_out'
        mype_bias=npe-1
        call write_bias(filename,mype_bias,nbc,&
             ges_z,gsi_bkgbias_bundle,iret_bias)
     endif
#endif /* HAVE_ESMF */

  return
end subroutine write_all

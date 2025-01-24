subroutine gsisub(init_pass,last_pass)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  gsisub                  high level driver for gridpoint 
!                                      statistical interpolation
!   prgmmr: parrish          org: np22                date: 1992-09-16
!
! abstract: This subroutine embodies the main driver for the gsi analysis
!           code.  This routine 
!           a) get analysis time and vertical coordinate information
!           b) generate grid information
!           c) initialize arrays containing information regarding use
!              of radiance, precipitation rate, and ozone observations
!           d) decomponses the domain into subdomains 
!           e) perform various initializations (subdomain variables, 
!              mpi communications, random number for pcp assimilation)
!           f) if (.not.regional) iniitialize spectral <--> grid transforms
!           g) call outer/inner loop driver
!           h) deallocate arrays
!
! program history log:
!   1991-xx-xx  parrish/derber
!   1991-12-10  parrish/derber   fixed coding error in near sfc anal
!   1992-09-14  derber           improved version of global analysis
!   1998-05-15  weiyu yang       mpp version
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2003-10-31  kleist, d. add capability to run hybrid or sigma
!   2003-12-22  derber, j. comments
!   2004-06-21  treadon - update documentation
!   2004-07-28  treadon - add only to module use, add intent in/out
!   2004-12-29  treadon - replace wrf_nmm_regional and wrf_mass_regional logicals
!                         with regional
!   2005-02-16  xu li   - add sub destroy_sst_an
!   2005-03-07  dee     - support gmao model interface
!   2005-04-18  treadon - remove destroy_sst_an
!   2005-05-27  pondeca - bypass radinfo_ and pcpinfo_write when twodvar_regional=.t.!                         
!   2005-05-25  guo     - added interfaces to handle GMAO first guess gridded fields
!   2005-07-25  treadon - remove redundant call to gengrid_vars
!   2005-09-08  derber - modify to use input group time window and simplify data set handling
!   2005-10-18  treadon - remove obs_load and dload, move deter_subdomain before read_obs
!   2005-11-04  guo     - moved call to gengrid_vars to ncep initialization part
!   2005-11-28  derber - move read_obs to glbsoi
!   2005-12-09  guo     - minor comment change
!   2006-01-10  treadon - consolidate query guess file code in gesinfo
!   2006-04-20  kistler - moved conv_read from read_obs here to parallel other *info modules
!   2006-04-21  parrish - changes for new processing of level 2 radar wind data
!   2007-03-15  todling - merged in da Silva/Cruz ESMF changes
!   2007-10-03  todling - add observer call
!   2009-01-28  todling - update observer calling procedure 
!   2009-08-19  guo     - #ifdef out destroy_gesfinfo() call for multi-pass observer.
!   2010-04-22  tangborn - add carbon monoxide settings
!   2010-05-13  huang   - add aeroinfo_read call
!   2010-05-19  todling - move oneob test outside esmf ifdef
!   2010-05-29  todling - update interface to ozinfo_read,pcpinfo_read,&convinfo_read
!   2010-06-05  todling - repositioned call to init_commvars
!   2010-07-19  lueken  - remove call to deter_subdomain (general_deter_subdomain is also used)
!   2010-11-08  treadon - remove create_mapping and init_subdomain_vars (now in init_grid_vars)
!   2012-06-12  parrish - remove init_commvars (replaced in gsimod.F90 with general_commvars).
!   2013-05-19  zhu     - add aircraft temperature bias correction
!   2014-02-27  sienkiewicz - add additional aircraft bias option (external table)
!   2015-07-20  zhu     - centralize radiance info for the usages of clouds & aerosols
!                       - add radiance_obstype_init,radiance_parameter_cloudy_init,radiance_parameter_aerosol_init 
!   2016-07-28  lippi   - add oneobmakerwsupob if 'rw' single ob test and skips radar_bufr_read_all.
!   2018-02-15  wu      - add code for fv3_regional option
!   2018-01-04  Apodaca - add lightinfo_read call for GOES/GLM lightning observations  
!   2018-07-24  W. Gu   - move routine corr_ob_initialize/finalize from radinfo
!
!   input argument list:
!
!   comments:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: i_kind
  use obsmod, only: iadate,lobserver,l2rwthin  
  use observermod, only: observer_init,observer_run,observer_finalize
  use gridmod, only: twodvar_regional,create_grid_vars,destroy_grid_vars,fv3_regional
  use gridmod, only: wrf_mass_regional,wrf_nmm_regional,nems_nmmb_regional,cmaq_regional
  use mpimod, only: mype,npe,mpi_comm_world,ierror
  use radinfo, only: radinfo_read
  use correlated_obsmod, only: corr_ob_initialize,corr_ob_finalize
  use pcpinfo, only: pcpinfo_read,create_pcp_random,&
       destroy_pcp_random
  use aeroinfo, only: aeroinfo_read
  use convinfo, only: convinfo_read
  use ozinfo, only: ozinfo_read
  use coinfo, only: coinfo_read
  use lightinfo, only: lightinfo_read
  use read_l2bufr_mod, only: radar_bufr_read_all
  use oneobmod, only: oneobtest,oneobmakebufr,oneobmakerwsupob,oneob_type
  use aircraftinfo, only: aircraftinfo_read,aircraft_t_bc_pof,aircraft_t_bc,&
     aircraft_t_bc_ext
  use radiance_mod, only: radiance_obstype_init,radiance_parameter_cloudy_init,radiance_parameter_aerosol_init
  use gsi_io, only: verbose
#ifndef HAVE_ESMF
  use guess_grids, only: destroy_gesfinfo
#endif

  use mpeu_util, only: die,tell

  implicit none

! Declare passed variables
  logical        ,intent(in) :: init_pass
  logical        ,intent(in) :: last_pass
  logical print_verbose
  
  print_verbose=.false.
  if(verbose) print_verbose=.true.

  if(print_verbose)then
     if(mype==0) call tell('gsisub',': starting ...')
     call tell('gsisub','init_pass =',init_pass)
     call tell('gsisub','last_pass =',last_pass)
     call tell('gsisub','iadate(1)=',iadate(1))
     call tell('gsisub','iadate(2)=',iadate(2))
     call tell('gsisub','iadate(3)=',iadate(3))
     call tell('gsisub','iadate(4)=',iadate(4))
     call tell('gsisub','iadate(5)=',iadate(5))
  end if

#ifndef HAVE_ESMF

! Allocate grid arrays.
  call create_grid_vars

! Get date, grid, and other information from model guess files
  call gesinfo

#endif /* !HAVE_ESMF */

! If single ob test, create prep.bufr file with single ob in it
  if (oneobtest) then
     if(mype==0 .and. oneob_type=='rw') then
        call oneobmakerwsupob
     else if(mype==0 .and. oneob_type/='rw') then
        call oneobmakebufr
     end if
     call mpi_barrier(mpi_comm_world,ierror)
  end if

! Process any level 2 bufr format land doppler radar winds and create radar wind superob file
  if(wrf_nmm_regional.or.wrf_mass_regional.or.nems_nmmb_regional .or. cmaq_regional &
          .or. fv3_regional) then
     if(.not. oneobtest .and. (.not. l2rwthin)) call radar_bufr_read_all(npe,mype) 
  end if

! Read info files for assimilation of various obs
  if (init_pass) then
     if (.not.twodvar_regional) then
        call radinfo_read
        call corr_ob_initialize
        call radiance_obstype_init
        call radiance_parameter_cloudy_init
        call ozinfo_read
        call coinfo_read
        call pcpinfo_read
        call aeroinfo_read
        call radiance_parameter_aerosol_init
        if (aircraft_t_bc_pof .or. aircraft_t_bc .or. aircraft_t_bc_ext) &
           call aircraftinfo_read
     endif
     call convinfo_read
     call lightinfo_read
     if(print_verbose)then
        call tell('gsisub','returned from convinfo_read()')
        call tell('gsisub','returned from lightinfo_read()')
     end if
  endif

! Compute random number for precipitation forward model.  
  if(init_pass) then
     call create_pcp_random(iadate,mype)
     if(print_verbose)then
        call tell('gsisub','returned from create_pcp_random()')
     end if
  endif

! Complete setup and execute external and internal minimization loops
  if(print_verbose)then
     call tell('gsisub','lobserver=',lobserver)
  end if
  if (lobserver) then
    if(init_pass) call observer_init()
    if(print_verbose)then
       call tell('gsisub','calling observer_run()')
    end if
    call observer_run(init_pass=init_pass,last_pass=last_pass)
    if(print_verbose)then
       call tell('gsisub','returned from observer_run()')
    end if
    if(last_pass) call observer_finalize()
#ifndef HAVE_ESMF
      call destroy_gesfinfo()   ! paired with gesinfo()
#endif
  else
     call glbsoi
  endif

  
  if(last_pass) then
!    Deallocate arrays
     call corr_ob_finalize
     call destroy_pcp_random
#ifndef HAVE_ESMF
     call destroy_grid_vars
#endif /* HAVE_ESMF */
  endif

  if(mype==0) call tell('gsisub',': complete.')
! End of gsi driver routine
  return
end subroutine gsisub

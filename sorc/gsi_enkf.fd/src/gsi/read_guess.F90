subroutine read_guess(iyear,month,idd,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_guess          read/compute various guess fields
!   prgmmr: parrish          org: np22                date: 1994-02-11
!
! abstract:  This routine performs various functions, all related in one
!            way or the other to the model guess.  Note that this routine
!            is for the global mode of the gsi.  Separate read_guess type
!            routines exist for the regional gsi.
!
!            Functions performed in this routine include the following
!              a) read atmospheric guess bias correction fields (optional)
!              b) read atmospheric guess fields (optionally update with bias correction)
!              c) read surface guess fields
!              d) compute average ozone at each level                           
!
!
! program history log:
!   1994-02-11  parrish
!   1998-04-03  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-06-16  treadon - update documentation
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-12-15  treadon - remove variable mype from call load_geop_hgt
!   2005-01-27  treadon - replace inguesfc with rdgesfc
!   2005-02-23  wu - setup for qoption=2 and output stats of RH
!   2005-03-07  dee - support gmao model interface
!   2005-03-30  treadon - reformat code (cosmetic changes only)
!   2005-05-27  parrish - add call get_derivatives
!   2005-06-27  guo     - support of GMAO gridded fields
!   2005-07-28  guo     - added sfc component for GMAO grided fields
!   2005-09-29  kleist  - get derivatives of surface terrain for Jc term
!   2005-11-21  kleist  - expand calls to new genqsat and calctends
!   2005-11-21  derber  - modify qoption =1 to work consistently with =2
!   2005-11-29  derber - remove external iteration dependent calculations
!   2005-11-29  derber - add ozmz calculation                             
!   2005-12-09  guo     - remove GMAO derivative computation code.  Use
!                         unified NCEP compact_diff procedures.
!   2006-01-10  treadon - consolidate all read*guess calls into this routine
!   2006-02-02  treadon - load 3d pressure guess pressure and geopotential 
!                         height grids
!   2006-02-03  derber  - modify to increase reproducibility (ozmz)
!   2006-03-13  treadon - increase filename to 24 characters
!   2006-04-14  treadon - replace call read_gfsatm for bias with read_bias
!   2006-06-08  zhang,b - change "biascor>0" to "biascor>=0" for debug purpose
!   2006-07-28  derber  - include sensible temperature
!   2006-07-31  kleist  - use ges_ps instead of lnps
!   2006-09-28  treadon - add sfc_rough and load_fact10
!   2006-12-04  todling - merged NCEP & GMAO bias correction schemes
!   2007-03-15  todling - merged in da Silva/Cruz ESMF changes
!   2007-05-30  h.liu   - remove ozmz
!   2008-12-06  todling - some clean; generalized update biasa
!   2009-01-28  todling - remove original GMAO interface
!   2010-03-06  parrish - add option to read ozone from gfs
!   2010-03-15  parrish - add flag regional_ozone to turn on ozone in regional analysis
!   2010-03-31  treadon - replace read_gfsatm with read_gfs
!   2010-05-19  todling - pass year and month for read_gsf_chem; read_guess should never
!   2010-09-17  pagowski - add cmaq
!                         depend on obsmod - that's why idate not passed via common block
!   2010-10-18  hcHuang - add flag use_gfs_nemsio and link to read_nems and read_nems_chem
!   2010-10-21  r. yang - pass dd for read_gsf_chem
!   2012-02-21  wu      - remove regional_ozone--causes conflict with using gfs ozone
!   2012-12-21  s.liu   - add option to use reflectivity
!   2013-10-19  todling - metguess now holds background
!   2013-10-30  jung    - changed zero to qmin in sensible temp calc and re-compute sensible
!                         temperature after clipping supersaturation
!   2014-10-04  todling - bug fix in dim of work array
!   2014-10-05  todling - background biases now held in bundle
!                       - redefined meanning of bcoption
!                       - renamed bkg-bias interface for clarity
!   2015-01-14  Hu      - add function gsd_gen_coast_prox to calculate coast
!                         proximity over full domain instead of subdomain
!   2016-03-02  s.liu/carley - remove use_reflectivity and use i_gsdcldanal_type 
!   2017-10-10  Wu W    - add code for FV3 netcdf guess input 
!   2019-09-18  martin  - added new fields to save guess tsen, q, geop_hgt for writing increment
!   2019-09-23  martin  - add code for FV3 GFS netcdf guess input
!   2020-11-19  Lu & Wang - modify file read for fgat, POC: xuguang.wang@ou.edu
!   2021-01-05  x.zhang/lei  - add code for updating delz analysis in regional da 
!
!   input argument list:
!     mype     - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind,i_kind
  use jfunc, only: bcoption,clip_supersaturation,superfact
  use guess_grids, only: nfldsig,ges_tsen,load_prsges,load_geop_hgt,ges_prsl,&
                         ges_tsen1,geop_hgti,ges_geopi,ges_q1
  use m_gsiBiases,only : bkg_bias_correction,nbc
  use m_gsiBiases, only: gsi_bkgbias_bundle
  use gsi_bias, only: read_bias
  use gridmod, only: lat2,lon2
  use gridmod, only: nsig
  use gridmod, only: wrf_mass_regional,wrf_nmm_regional,cmaq_regional,&
       fv3_regional,&
       twodvar_regional,netcdf,regional,nems_nmmb_regional,use_gfs_ozone
  use gridmod, only: use_gfs_nemsio, use_gfs_ncio, write_fv3_incr
  use gfs_stratosphere, only: use_gfs_stratosphere

  use constants, only: zero,one,fv,qmin
  use ncepgfs_io, only: read_gfs,read_gfs_chem
  use ncepnems_io, only: read_nems,read_nems_chem
  use netcdfgfs_io, only: read_gfsnc, read_gfsnc_chem
  use gsi_metguess_mod, only: gsi_metguess_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsd_update_mod, only: gsd_gen_coast_prox 
  use read_wrf_mass_guess_mod, only: read_wrf_mass_guess_class
  use read_wrf_nmm_guess_mod, only: read_wrf_nmm_guess_class
  use gsi_rfv3io_mod, only: read_fv3_netcdf_guess

  use gridmod,only: l_reg_update_hydro_delz
  use guess_grids, only:geom_hgti,geom_hgti_bg

  use gsi_rfv3io_mod, only: bg_fv3regfilenameg
  use mpimod, only: mpi_comm_world

  implicit none

! Declare passed variables
  integer(i_kind),intent(in   ) :: iyear
  integer(i_kind),intent(in   ) :: month
  integer(i_kind),intent(in   ) :: idd
  integer(i_kind),intent(in   ) :: mype

! Declare local variables
  character(24) filename
  logical :: ice
  integer(i_kind) i,j,k,it,iret_bias,ier,istatus
  integer(i_kind) iderivative
  type(read_wrf_nmm_guess_class) :: nmm_binary_guess
  type(read_wrf_mass_guess_class) :: nmm_mass_guess

  real(r_kind) :: satval
  real(r_kind),dimension(lat2,lon2,nsig) :: satq
  real(r_kind),dimension(:,:,:),allocatable:: work
  real(r_kind),dimension(:,:,:),pointer:: ges_tv=>NULL()
  real(r_kind),dimension(:,:,:),pointer:: ges_q =>NULL()

  iret_bias=0
!-----------------------------------------------------------------------------------
! Certain functions are only done once --> on the first outer iteration. 
! One-time functions include
!    a) read atmospheric guess fields (optionally add bias correction)
!    b) read surface guess fields
!

!    Handle regional interfaces
     if (regional)then
        if (wrf_nmm_regional) then
           if(netcdf) then
              call nmm_binary_guess%read_wrf_nmm_netcdf_guess(mype)
           else
              call nmm_binary_guess%read_wrf_nmm_binary_guess(mype)
           end if
        else if (wrf_mass_regional) then
           if(netcdf) then
              call nmm_mass_guess%read_wrf_mass_netcdf_guess(mype)
           else
              call nmm_mass_guess%read_wrf_mass_binary_guess(mype)
           end if
        else if(twodvar_regional) then
           call read_2d_guess(mype)
        else if (nems_nmmb_regional) then
           call nmm_binary_guess%read_nems_nmmb_guess(mype)
        else if (fv3_regional      ) then
           allocate(bg_fv3regfilenameg(nfldsig))
           do it=1,nfldsig   
              call bg_fv3regfilenameg(it)%init(it)
           end do
           call read_fv3_netcdf_guess(bg_fv3regfilenameg)
        else if (cmaq_regional) then
           call read_cmaq_guess(mype)
        end if
     

!    Otherwise, handle global interface (ie, NCEP GFS)
     else

!       If requested, read bias correction fields
        if (abs(bcoption)>0) then
           filename='biascor_in'
           allocate(work(lat2,lon2,nbc))
           call read_bias(filename,mype,nbc,work,gsi_bkgbias_bundle,iret_bias)
           deallocate(work)
        endif
        
!       Read atmospheric fields
#ifndef HAVE_ESMF
        if ( use_gfs_nemsio ) then
!!           WRITE(6,*)'WARNING :: you elect to read first guess field in NEMSIO format'
           call read_nems
           call read_nems_chem(iyear,month,idd)
        else if ( use_gfs_ncio ) then
           call read_gfsnc
           call read_gfsnc_chem(iyear,month,idd)
        else
           call read_gfs
           call read_gfs_chem(iyear,month,idd)
        end if
#endif

!    End of non-GMAO global interfaces
     endif
        
! If doing SBC, apply bias correction ...

  if(iret_bias==0 .and. bcoption>0) call bkg_bias_correction()

! Get sensible temperature (after bias correction's been applied)

  do it=1,nfldsig
     ier=0
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'q' ,ges_q ,istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'tv',ges_tv,istatus)
     ier=ier+istatus
     if (ier==0) then
       do k=1,nsig
          do j=1,lon2
             do i=1,lat2
!               ges_tsen(i,j,k,it)= ges_tv(i,j,k)/(one+fv*max(qmin,ges_q(i,j,k)))
                ges_tsen(i,j,k,it)= ges_tv(i,j,k)/(one+fv*max(zero,ges_q(i,j,k)))
                if (write_fv3_incr) ges_q1(i,j,k,it) = max(zero, ges_q(i,j,k))
             end do
          end do
       end do
    end if
  end do


! Load 3d subdomain pressure arrays from the guess fields
  call load_prsges


! recompute sensible temperature to remove supersaturation
  if ( clip_supersaturation ) then
    call tpause(mype,'pvoz')
    ice = .true.
    iderivative = 0
    do it=1,nfldsig
      call gsi_bundlegetpointer (gsi_metguess_bundle(it),'q' ,ges_q, istatus)
      ier=ier+istatus
      call gsi_bundlegetpointer (gsi_metguess_bundle(it),'tv',ges_tv,istatus)
      ier=ier+istatus
      call genqsat(satq,ges_tsen(1,1,1,it),ges_prsl(1,1,1,it),lat2,lon2, &
                   nsig,ice,iderivative)
      do k=1,nsig
         do j=1,lon2
            do i=1,lat2
               satval = min(ges_q(i,j,k),superfact*satq(i,j,k))
               satval = max(qmin,satval)
               ges_q(i,j,k) = satval
               ges_tsen(i,j,k,it)= ges_tv(i,j,k)/(one+fv*ges_q(i,j,k))
            end do
         end do
      end do
      if (write_fv3_incr) ges_q1(:,:,:,it) = ges_q(i,j,k)
    end do
  endif   ! clip_supersaturation


! Compute 3d subdomain geopotential heights from the guess fields
  call load_geop_hgt

   if(l_reg_update_hydro_delz) then
     geom_hgti_bg=geom_hgti
   endif

! save guess geopotential height at level interface for use in write_atm
  ges_geopi=geop_hgti

! save this for writing increment
  ges_tsen1(:,:,:,:) = ges_tsen(:,:,:,:)

! Compute the coast proximity
  call gsd_gen_coast_prox

!  If this is a regional run and ozone is desired from the gfs model, bring it in here:
  if(regional.and.use_gfs_ozone.and..not.use_gfs_stratosphere) &
              call read_gfs_ozone_for_regional
  
  return
end subroutine read_guess

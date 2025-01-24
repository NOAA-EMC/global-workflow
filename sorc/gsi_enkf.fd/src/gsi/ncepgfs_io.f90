module ncepgfs_io
!$$$ module documentation block
!           .      .    .                                       .
! module:   ncepgfs_io
!   prgmmr: treadon     org: np23                date: 2006-01-10
!
! abstract: This module contains routines which handle input/output
!           operations for NCEP GFS atmospheric and surface files.
!
! program history log:
!   2006-01-10 treadon
!   2009-08-26 li      - add write_gfs_sfc_nst,read_gfsnst, write_gfsnst
!   2010-02-20 parrish - make sigio_cnvtdv8 public so can be accessed by general_read_gfsatm, when
!                          reading in gefs sigma files at resolution different from analysis.
!   2010-03-31 treadon - add read_gfs, use sp_a and sp_b
!   2010-05-19 todling - add read_gfs_chem
!   2011-04-08 li      - (1) add integer nst_gsi to control the mode of NSST 
!                      - (2) add subroutine write_gfs_sfc_nst to save sfc and nst files
!   2014-04-08 li      - (1) modify write_gfs_sfc_nst for mask dependent interpolation
!                        (2) add write_ens_sfc_nst, write_ens_dsfct
!   2014-12-03 derber - modify for changes to general_read/write_gfsatm
!   2014-12-03 derber - modify read_sfc routines to minimize communications/IO
!   2015-03-13  li     - introduce zsea1 & zsea2 enable to use vertical mean
!                        temperature based on NSST T-Profile. And move Tf analysis increment
!                        interpolation (analysis grid to ensemble grid) to re-center step
!   2015-04-25  li     - modify read_nst, read_gfsnst routines to minimize communications/IO
!   2016-08-18  li     - tic591: add read_sfc_anl & read_gfssfc_anl to read ensemble sfc file (isli only)
!                                use the modified 2d interpolation (sfc_interpolate to intrp22)

!
! Subroutines Included:
!   sub read_gfs          - driver to read ncep gfs atmospheric ("sigma") files
!   sub read_gfssfc       - read ncep gfs surface file, scatter on grid to 
!                           analysis subdomains
!   sub write_gfs         - driver to write ncep gfs atmospheric and surface
!                           analysis files
!   sub write_gfssfc      - gather/write on grid ncep surface analysis file
!   sub read_gfssfc_ens   - read ncep gfs ensemble surface file, scatter on grid to 
!                           analysis subdomains
!   sub read_nst          - driver to read ncep nst file
!   sub read_gfsnst       - read ncep nst filea from one task and then broadcast to others
!   sub write_gfs_sfc_nst - gather/write on grid ncep surface & nst analysis file
!   sub write_ens_sfc_nst - gather/write on ensemble grid ncep surface & nst analysis file
!   sub write_ens_dsfct   - gather/write on ensemble grid ncep Ts analysis increment
!
! Variable Definitions:
!   none
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  use sigio_module, only: sigio_head
  use ncepnems_io, only: tran_gfssfc
  use gridmod, only: sfcnst_comb
  implicit none

  private
  public read_sfc
  public read_gfs
  public read_gfs_chem
  public read_gfssfc
  public read_sfc_anl
  public read_gfssfc_anl
  public read_nst
  public read_gfsnst
  public write_gfs
  public write_gfs_sfc_nst
  public sigio_cnvtdv8
  public sighead 
  public write_ghg_grid

  type(sigio_head) :: sighead 

contains

  subroutine read_gfs
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    read_gfs
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2010-03-31  treadon - create routine
!   2011-05-01  todling - cwmr no longer in guess-grids; use metguess bundle now
!   2011-10-01  mkim    - add calculation of hydrometeor mixing ratio from total condensate (cw)  
!   2011-11-01  eliu    - add call to set_cloud_lower_bound (qcmin) 
!   2011-11-01  eliu    - move then calculation of hydrometeor mixing ratio from total condensate to cloud_efr;
!                         rearrange Min-Jeong's code  
!   2013-10-19  todling - update cloud_efr module name
!   2013-10-29  todling - revisit write to allow skipping vars not in MetGuess
!   2014-11-28  zhu     - assign cwgues0 right after reading in fg,
!                       - set lower bound to cloud after assigning cwgues0
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

    use kinds, only: i_kind,r_kind
    use gridmod, only: hires_b,sp_a,grd_a,jcap_b,nlon,nlat,lat2,lon2,nsig,regional
    use guess_grids, only: ifilesig,nfldsig
    use gsi_metguess_mod, only: gsi_metguess_bundle
    use gsi_bundlemod, only: gsi_bundlegetpointer
    use gsi_bundlemod, only: gsi_bundlecreate
    use gsi_bundlemod, only: gsi_grid
    use gsi_bundlemod, only: gsi_gridcreate
    use gsi_bundlemod, only: gsi_bundle
    use gsi_bundlemod, only: gsi_bundledestroy
    use general_sub2grid_mod, only: sub2grid_info,general_sub2grid_create_info,general_sub2grid_destroy_info
    use mpimod, only: npe,mype
    use mpeu_util, only: die
    use cloud_efr_mod, only: cloud_calc_gfs,set_cloud_lower_bound    
    use general_specmod, only: general_init_spec_vars,general_destroy_spec_vars,spec_vars
    implicit none

    character(24) filename
    logical:: l_cld_derived,zflag,inithead
    integer(i_kind):: it,nlon_b,num_fields,inner_vars
    integer(i_kind):: iret,iret_ql,iret_qi,istatus 

    type(gsi_bundle) :: atm_bundle
    type(gsi_grid)   :: atm_grid
    integer(i_kind),parameter :: n2d=2
    integer(i_kind),parameter :: n3d=8
    character(len=4), parameter :: vars2d(n2d) = (/ 'z   ', 'ps  ' /)
    character(len=4), parameter :: vars3d(n3d) = (/ 'u   ', 'v   ', &
                                                    'vor ', 'div ', &
                                                    'tv  ', 'q   ', &
                                                    'cw  ', 'oz  ' /)

    real(r_kind),pointer,dimension(:,:):: ptr2d   =>null()
    real(r_kind),pointer,dimension(:,:,:):: ptr3d =>null()

    real(r_kind),pointer,dimension(:,:  ):: ges_ps_it   => null()
    real(r_kind),pointer,dimension(:,:  ):: ges_z_it    => null()
    real(r_kind),pointer,dimension(:,:,:):: ges_u_it    => null()
    real(r_kind),pointer,dimension(:,:,:):: ges_v_it    => null()
    real(r_kind),pointer,dimension(:,:,:):: ges_vor_it  => null()
    real(r_kind),pointer,dimension(:,:,:):: ges_div_it  => null()
    real(r_kind),pointer,dimension(:,:,:):: ges_tv_it   => null()
    real(r_kind),pointer,dimension(:,:,:):: ges_q_it    => null()
    real(r_kind),pointer,dimension(:,:,:):: ges_oz_it   => null()
    real(r_kind),pointer,dimension(:,:,:):: ges_cwmr_it => null()
    real(r_kind),pointer,dimension(:,:,:):: ges_ql_it   => null()
    real(r_kind),pointer,dimension(:,:,:):: ges_qi_it   => null()

    type(spec_vars):: sp_b
    type(sub2grid_info) :: grd_t


!   If needed, initialize for hires_b transforms
    nlon_b=((2*jcap_b+1)/nlon+1)*nlon
    if (nlon_b /= sp_a%imax) then
       hires_b=.true.
       call general_init_spec_vars(sp_b,jcap_b,jcap_b,nlat,nlon_b)
       if (mype==0) &
            write(6,*)'READ_GFS:  allocate and load sp_b with jcap,imax,jmax=',&
            sp_b%jcap,sp_b%imax,sp_b%jmax
    endif

    inner_vars=1
   num_fields=min(8*grd_a%nsig+2,npe)
!  Create temporary communication information fore read routines
    call general_sub2grid_create_info(grd_t,inner_vars,grd_a%nlat,grd_a%nlon, &
          grd_a%nsig,num_fields,regional)

!   Allocate bundle used for reading members
    call gsi_gridcreate(atm_grid,lat2,lon2,nsig)
    call gsi_bundlecreate(atm_bundle,atm_grid,'aux-atm-read',istatus,names2d=vars2d,names3d=vars3d)
    if(istatus/=0) then
      write(6,*)' read_gfs: trouble creating atm_bundle'
      call stop2(999)
    endif

    zflag=.true.
    inithead=.true.
    do it=1,nfldsig

       write(filename,100) ifilesig(it)
100    format('sigf',i2.2)
       if (hires_b) then

!         If hires_b, spectral to grid transform for background
!         uses double FFT.   Need to pass in sp_a and sp_b

          call general_read_gfsatm(grd_t,sp_a,sp_b,filename,.true.,.true.,zflag, &
               atm_bundle,&
               inithead,iret)

       else

!         Otherwise, use standard transform.  Use sp_a in place of sp_b.

          call general_read_gfsatm(grd_t,sp_a,sp_a,filename,.true.,.true.,zflag, &
               atm_bundle,&
               inithead,iret)
       endif
       inithead=.false.
       zflag=.false.

!      Set values to actual MetGuess fields
       call set_guess_


       l_cld_derived = associated(ges_cwmr_it).and.&
                       associated(ges_q_it)   .and.&
                       associated(ges_ql_it)  .and.&
                       associated(ges_qi_it)  .and.&
                       associated(ges_tv_it)

!      call set_cloud_lower_bound(ges_cwmr_it)
       if (mype==0) write(6,*)'READ_GFS: l_cld_derived = ', l_cld_derived

       if (l_cld_derived) then
          call cloud_calc_gfs(ges_ql_it,ges_qi_it,ges_cwmr_it,ges_q_it,ges_tv_it,.true.) 
       end if
    end do
    call gsi_bundledestroy(atm_bundle,istatus)

    call general_sub2grid_destroy_info(grd_t)

    if (hires_b) call general_destroy_spec_vars(sp_b)


  contains


  subroutine set_guess_

  call gsi_bundlegetpointer (atm_bundle,'ps',ptr2d,istatus) 
  if (istatus==0) then
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'ps',ges_ps_it  ,istatus) 
     if(istatus==0) ges_ps_it = ptr2d
  endif
  call gsi_bundlegetpointer (atm_bundle,'z',ptr2d,istatus) 
  if (istatus==0) then
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'z' ,ges_z_it   ,istatus) 
     if(istatus==0) ges_z_it = ptr2d
  endif
  call gsi_bundlegetpointer (atm_bundle,'u',ptr3d,istatus) 
  if (istatus==0) then
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'u' ,ges_u_it   ,istatus) 
     if(istatus==0) ges_u_it = ptr3d
  endif
  call gsi_bundlegetpointer (atm_bundle,'v',ptr3d,istatus) 
  if (istatus==0) then
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'v' ,ges_v_it   ,istatus) 
     if(istatus==0) ges_v_it = ptr3d
  endif
  call gsi_bundlegetpointer (atm_bundle,'vor',ptr3d,istatus) 
  if (istatus==0) then
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'vor',ges_vor_it,istatus) 
     if(istatus==0) ges_vor_it = ptr3d
  endif
  call gsi_bundlegetpointer (atm_bundle,'div',ptr3d,istatus) 
  if (istatus==0) then
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'div',ges_div_it,istatus) 
     if(istatus==0) ges_div_it = ptr3d
  endif
  call gsi_bundlegetpointer (atm_bundle,'tv',ptr3d,istatus) 
  if (istatus==0) then
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'tv',ges_tv_it  ,istatus) 
     if(istatus==0) ges_tv_it = ptr3d
  endif
  call gsi_bundlegetpointer (atm_bundle,'q',ptr3d,istatus) 
  if (istatus==0) then
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'q' ,ges_q_it   ,istatus) 
     if(istatus==0) ges_q_it = ptr3d
  endif
  call gsi_bundlegetpointer (atm_bundle,'oz',ptr3d,istatus) 
  if (istatus==0) then
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'oz',ges_oz_it  ,istatus) 
     if(istatus==0) ges_oz_it = ptr3d
  endif
  call gsi_bundlegetpointer (atm_bundle,'cw',ptr3d,istatus) 
  if (istatus==0) then
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'cw',ges_cwmr_it,istatus) 
     if(istatus==0) ges_cwmr_it = ptr3d
  endif
  call gsi_bundlegetpointer (gsi_metguess_bundle(it),'ql',ges_ql_it,  iret_ql) 
  call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qi',ges_qi_it,  iret_qi)           
  if (iret_ql/=0) then 
     if (mype==0) write(6,*)'READ_GFS: cannot get pointer to ql,iret_ql= ',iret_ql 
  endif
  if (iret_qi/=0) then 
     if (mype==0) write(6,*)'READ_GFS: cannot get pointer to qi,iret_qi= ',iret_qi 
  endif

  end subroutine set_guess_

  end subroutine read_gfs

  subroutine read_gfs_chem (iyear, month,idd, it )
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    read_gfs_chem
!
!   prgrmmr: todling
!
! abstract: fills chemguess_bundle with GFS chemistry. 
!
! remarks: 
!    1. Right now, only CO2 is done and even this is treated 
!        as constant througout the assimialation window.
!    2. iyear and month could come from obsmod, but logically
!       this program should never depend on obsmod
! 
!
! program history log:
!   2010-04-15  hou - Initial code
!   2010-05-19  todling - Port Hou's code from compute_derived(!)
!                         into this module and linked with the chemguess_bundle
!   2011-02-01  r. yang - proper initialization of prsi
!   2011-05-24  yang    - add idd for time interpolation of co2 field
!   2011-06-29  todling - no explict reference to internal bundle arrays
!   2013-11-08  todling - revisit check for present of GHG in chem-bundle
!   2016-01-12  todling - allow for full Co2 field to be used when specified by user
!                         (should be extra option in ncepgfs_ghg)
!                       - pass time index (it) as optional arg for when routine
!                         called sequentially in time
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

    use kinds, only: i_kind,r_kind
    use mpimod, only: mype
    use gridmod, only: lat2,lon2,nsig,nlat,rlats,istart
    use ncepgfs_ghg, only: read_gfsco2,read_ch4n2oco
    use guess_grids, only: nfldsig
    use gsi_bundlemod, only: gsi_bundlegetpointer
    use gsi_chemguess_mod, only: gsi_chemguess_bundle
    use gsi_chemguess_mod, only: gsi_chemguess_get



    implicit none

!   Declared argument list
    integer(i_kind), intent(in):: iyear
    integer(i_kind), intent(in):: month
    integer(i_kind), intent(in):: idd
    integer(i_kind), intent(in), optional:: it

!   Declare local variables
    character(len=*),parameter :: myname='read_gfs_chem'
    integer(i_kind)            :: i,j,k,n,it_,ier
    integer(i_kind)            :: ico24crtm,ich44crtm,in2o4crtm,ico4crtm
    character(len=3) :: char_ghg
    real(r_kind),allocatable,dimension(:) :: avefld
    real(r_kind),dimension(lat2):: xlats
    real(r_kind),pointer,dimension(:,:,:)::p_co2=>null()
    real(r_kind),pointer,dimension(:,:,:)::p_ch4=>null()
    real(r_kind),pointer,dimension(:,:,:)::p_n2o=>null()
    real(r_kind),pointer,dimension(:,:,:)::p_co=>null()
    real(r_kind),pointer,dimension(:,:,:)::ptr3d_co2=>null()
    real(r_kind),pointer,dimension(:,:,:)::ptr3d_ch4=>null()
    real(r_kind),pointer,dimension(:,:,:)::ptr3d_n2o=>null()
    real(r_kind),pointer,dimension(:,:,:)::ptr3d_co=>null()

    if(.not.associated(gsi_chemguess_bundle)) return

    it_=1
    if (present(it)) then
       it_=it
    endif
!   Get subdomain latitude array
    j = mype + 1
    do i = 1, lat2
       n = min(max(1, istart(j)+i-2), nlat)
       xlats(i) = rlats(n)
    enddo
!!NOTE: NEED TO CHANGE THIS BLOCK, THE CHECK AND READ OF TRACE GASES ARE HARDWIRED !!!!!!
!!      WILL CHANGE THE CODE FOLLOWING WHAT I DID IN crtm_interface.f90            !!!!!!

! check whether CO2 exist
    call gsi_bundlegetpointer(gsi_chemguess_bundle(it_),'co2',p_co2,ier)
    if (associated(p_co2)) then
       call gsi_chemguess_get ( 'i4crtm::co2', ico24crtm, ier )
       if (ico24crtm >= 0 ) then
          if (ico24crtm<=2) then
             call read_gfsco2 (iyear,month,idd,ico24crtm,xlats,&
                               lat2,lon2,nsig,mype,  &
                               p_co2 )
! Approximation: assign three time slots (nfldsig) of ghg with same values
             if (.not.present(it)) then
                do n=2,nfldsig
                   call gsi_bundlegetpointer(gsi_chemguess_bundle(n),'co2',ptr3d_co2,ier)
                   ptr3d_co2 = p_co2
                enddo
             endif
          else
             allocate(avefld(size(p_co2,3)))
             call glbave(p_co2,avefld)
             if (mype==0) then
                write(6,'(a)') 'Mean Co2'
                do k=1,nsig
                   write(6,'(1p,(e10.3,1x))') avefld(k)
                enddo
             endif
             deallocate(avefld)
          endif
          char_ghg='co2'
! take comment out for printing out the interpolated tracer gas fields.
!        call write_ghg_grid (ptr3d_co2,char_ghg)
       endif
    endif ! <co2>

! check whether CH4 data exist
    call gsi_bundlegetpointer(gsi_chemguess_bundle(it_),'ch4',p_ch4,ier)
    if (associated(p_ch4)) then
       call gsi_chemguess_get ( 'i4crtm::ch4', ich44crtm, ier )
       if (ich44crtm > 0 ) then
          char_ghg='ch4'
          call read_ch4n2oco (iyear,month,idd,char_ghg,xlats,&
                          lat2,lon2,nsig,mype,  &
                          p_ch4 )
          if (.not.present(it))then
             do n=2,nfldsig
                call gsi_bundlegetpointer(gsi_chemguess_bundle(n),'ch4',ptr3d_ch4,ier)
                ptr3d_ch4 = p_ch4
             enddo
          endif
! take comment out for printing out the interpolated tracer gas fields.
!         call write_ghg_grid (ptr3d_ch4,char_ghg)
       endif
    endif ! <ch4>

! check whether N2O data exist
    call gsi_bundlegetpointer(gsi_chemguess_bundle(it_),'n2o',p_n2o,ier)
    if (associated(p_n2o)) then
       call gsi_chemguess_get ( 'i4crtm::n2o', in2o4crtm, ier )
       if (in2o4crtm > 0 ) then
          char_ghg='n2o'
          call read_ch4n2oco (iyear,month,idd,char_ghg,xlats,&
                          lat2,lon2,nsig,mype,  &
                          p_n2o )
          if (.not.present(it))then
             do n=2,nfldsig
                call gsi_bundlegetpointer(gsi_chemguess_bundle(n),'n2o',ptr3d_n2o,ier)
                ptr3d_n2o = p_n2o
             enddo
          endif
! take comment out for printing out the interpolated tracer gas fields.
!        call write_ghg_grid (ptr3d_n2o,char_ghg)
       endif
    endif ! <n2o>

! check whether CO data exist
    call gsi_bundlegetpointer(gsi_chemguess_bundle(it_),'co',p_co,ier)
    if (associated(p_co)) then
       call gsi_chemguess_get ( 'i4crtm::co', ico4crtm, ier )
       if (ico4crtm > 0 ) then
          char_ghg='co'
          call read_ch4n2oco ( iyear,month,idd,char_ghg,xlats,&
                          lat2,lon2,nsig,mype,  &
                          p_co )
          if (.not.present(it)) then
             do n=2,nfldsig
                call gsi_bundlegetpointer(gsi_chemguess_bundle(n),'co',ptr3d_co,ier)
                ptr3d_co = p_co
             enddo
          endif
! take comment out for printing out the interpolated tracer gas fields.
!        call write_ghg_grid (ptr3d_co,char_ghg)
       endif
    endif ! <co>
  end subroutine read_gfs_chem
subroutine write_ghg_grid(a,char_ghg)
!$$$  subroutine documentation block
!
! subprogram:    write_ghg_grid
!
!   prgrmmr:  yang: follow write_bkgvars_grid
!
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:
!
!$$$
  use mpimod, only: mype
  use kinds, only: r_kind,i_kind,r_single
  use gridmod, only: nlat,nlon,nsig,lat2,lon2
  use file_utility, only : get_lun
  implicit none

  real(r_kind),dimension(lat2,lon2,nsig),intent(in   ) :: a
  character(len=3),intent(in) :: char_ghg

  character(255):: grdfile

  real(r_kind),dimension(nlat,nlon,nsig):: ag

  real(r_single),dimension(nlon,nlat,nsig):: a4
  integer(i_kind) ncfggg,iret,i,j,k,lu

! gather stuff to processor 0
  do k=1,nsig
     call gather_stuff2(a(1,1,k),ag(1,1,k),mype,0)
  end do
  if (mype==0) then
     write(6,*) 'WRITE OUT INTERPOLATED ',char_ghg
! load single precision arrays
     do k=1,nsig
        do j=1,nlon
           do i=1,nlat
              a4(j,i,k)=ag(i,j,k)
           end do
        end do
     end do

! Create byte-addressable binary file for grads
     grdfile=trim(char_ghg)//'clim_grd'
     ncfggg=len_trim(grdfile)
     lu=get_lun()
     call baopenwt(lu,grdfile(1:ncfggg),iret)
     call wryte(lu,4*nlat*nlon*nsig,a4)
     call baclose(lu,iret)
  end if

  return
end subroutine write_ghg_grid

  subroutine read_sfc(sfct,soil_moi,sno,soil_temp,veg_frac,fact10,sfc_rough, &
                      veg_type,soil_type,terrain,isli,use_sfc_any)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    read_sfc
!
!   prgrmmr: whitaker
!
! abstract: read a ncep GFS surface file on a specified task,
!           broadcast data to other tasks.
!
! program history log:
!   2012-01-24  whitaker - create routine
!
!   input argument list:
!    use_sfc_any - true if any processor uses extra surface fields
!
!   output argument list:
!     sfct      - surface temperature (skin temp)
!     soil_moi  - soil moisture of first layer
!     sno       - snow depth
!     soil_temp - soil temperature of first layer
!     veg_frac  - vegetation fraction
!     fact10    - 10 meter wind factor
!     sfc_rough - surface roughness
!     veg_type  - vegetation type
!     soil_type - soil type
!     terrain   - terrain height
!     isli      - sea/land/ice mask
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
    ! read data from sfc file on a single task, bcast data to other tasks.
    use sfcio_module, only: sfcio_srohdc,sfcio_head,sfcio_data,sfcio_intkind
    use sfcio_module, only: sfcio_axdata,sfcio_sclose
    use kinds, only: i_kind,r_single,r_kind
    use gridmod, only: nlat_sfc,nlon_sfc
    use guess_grids, only: nfldsfc,ifilesfc

    logical,                                               intent(in   ) :: use_sfc_any
    real(r_single),  dimension(nlat_sfc,nlon_sfc,nfldsfc), intent(  out) :: sfct,soil_moi,sno,soil_temp,veg_frac,fact10,sfc_rough 
    real(r_single),  dimension(nlat_sfc,nlon_sfc),         intent(  out) :: veg_type,soil_type,terrain
    integer(i_kind), dimension(nlat_sfc,nlon_sfc),         intent(  out) :: isli
    integer(i_kind) :: latb,lonb
    integer(i_kind) :: iret,n,i,j
    type(sfcio_head) :: sfc_head
    type(sfcio_data) :: sfc_data
    real(r_single), allocatable, dimension(:,:):: outtmp
    integer(i_kind) :: nsfc,it
    character(24) :: filename
!   Declare local parameters
    integer(sfcio_intkind):: lunges = 11
    integer(i_kind),parameter:: nsfc_all = 11

    do it=1,nfldsfc
! read a surface file on the task
       write(filename,200)ifilesfc(it)
200    format('sfcf',i2.2)
       call sfcio_srohdc(lunges,filename,sfc_head,sfc_data,iret)
!      Check for possible problems
       if (iret /= 0) then
          write(6,*)'READ_SFC:  ***ERROR*** problem reading ',filename,&
               ', iret=',iret
          call sfcio_axdata(sfc_data,iret)
          call stop2(80)
       endif
       lonb = sfc_head%lonb
       latb = sfc_head%latb
       if ( (latb /= nlat_sfc-2) .or. (lonb /= nlon_sfc) ) then
            write(6,*)'READ_GFSSFC:  ***ERROR*** inconsistent grid dimensions.  ',&
                 ', nlon,nlat-2=',nlon_sfc,nlat_sfc-2,' -vs- sfc file lonb,latb=',&
                    lonb,latb
            call sfcio_axdata(sfc_data,iret)
            call stop2(80)
       endif
       if(it == 1)then
         nsfc=nsfc_all
       else
         nsfc=nsfc_all-4
       end if
!$omp parallel do private(n,i,j,outtmp)
       do n = 1, nsfc
 
          if (n == 1) then                               ! skin temperature
   
             call tran_gfssfc(sfc_data%tsea,sfct(1,1,it),lonb,latb)                                 
 
          elseif(n == 2 .and. use_sfc_any) then          ! soil moisture

             call tran_gfssfc(sfc_data%smc(1:lonb,1:latb,1),soil_moi(1,1,it),lonb,latb)  

          elseif(n == 3) then                            ! snow depth

             call tran_gfssfc(sfc_data%sheleg,sno(1,1,it),lonb,latb)        

          elseif(n == 4 .and. use_sfc_any) then          ! soil temperature

             call tran_gfssfc(sfc_data%stc(1:lonb,1:latb,1),soil_temp(1,1,it),lonb,latb)  

          elseif(n == 5 .and. use_sfc_any) then          ! vegetation cover 

             call tran_gfssfc(sfc_data%vfrac,veg_frac(1,1,it),lonb,latb)                       

          elseif(n == 6) then                            ! 10m wind factor

             call tran_gfssfc(sfc_data%f10m,fact10(1,1,it),lonb,latb)                           

          elseif(n == 7) then                            ! suface roughness

             call tran_gfssfc(sfc_data%zorl,sfc_rough(1,1,it),lonb,latb)            

          elseif(n == 8 .and. use_sfc_any) then          ! vegetation type

              call tran_gfssfc(sfc_data%vtype,veg_type,lonb,latb)            

          elseif(n == 9 .and. use_sfc_any) then          ! soil type

             call tran_gfssfc(sfc_data%stype,soil_type,lonb,latb)                     

          elseif(n == 10) then                           ! sea/land/ice flag

             call tran_gfssfc(sfc_data%orog,terrain,lonb,latb)            

          elseif(n == 11) then                           ! terrain

             allocate(outtmp(latb+2,lonb))
             call tran_gfssfc(sfc_data%slmsk,outtmp,lonb,latb)                       
             do j=1,lonb
                do i=1,latb+2
                   isli(i,j) = nint(outtmp(i,j))
                end do
             end do
             deallocate(outtmp)

          endif

!      End of loop over data records
       enddo
!   Print date/time stamp
       write(6,700) latb,lonb,sfc_head%fhour,sfc_head%idate
700    format('READ_SFC:  ges read/scatter, nlat,nlon=',&
            2i6,', hour=',f10.1,', idate=',4i5)
       call sfcio_axdata(sfc_data,iret)
       call sfcio_sclose(lunges,iret)

!   End of loop over time levels
    end do

  end subroutine read_sfc

  subroutine read_gfssfc(iope,sfct,soil_moi,sno,soil_temp,veg_frac,fact10,sfc_rough, &
                         veg_type,soil_type,terrain,isli,use_sfc_any)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_gfssfc     read gfs surface file
!   prgmmr: treadon          org: np23                date: 2003-04-10
!
! abstract: read gfs surface file
!
! program history log:
!   2003-04-10  treadon
!   2004-05-18  kleist, add global isli & documentation
!   2004-09-07  treadon fix mpi bug when npe > nsfc
!   2005-01-27  treadon - rewrite to make use of sfcio module
!   2005-03-07  todling - die gracefully when return error from sfcio
!   2006-09-28  treadon - pull out surface roughness
!   2008-05-28  safford - rm unused vars
!   2009-01-12  gayno   - add read of terrain height
!   2016-03-13  xuli    - modify the document and reorganize the variables order 
!
!   input argument list:
!     iope     - mpi task handling i/o
!     use_sfc_any - true if any processor uses extra surface fields
!
!   output argument list:
!     sfct      - surface temperature (skin temp)
!     soil_moi  - soil moisture of first layer
!     sno       - snow depth
!     soil_temp - soil temperature of first layer
!     veg_frac  - vegetation fraction
!     fact10    - 10 meter wind factor
!     sfc_rough - surface roughness
!     veg_type  - vegetation type
!     soil_type - soil type
!     terrain   - terrain height
!     isli      - sea/land/ice mask 
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use kinds, only: r_kind,i_kind,r_single
    use gridmod, only: nlat_sfc,nlon_sfc
    use guess_grids, only: nfldsfc,sfcmod_mm5,sfcmod_gfs
    use mpimod, only: mpi_itype,mpi_rtype4,mpi_comm_world,mype
    use constants, only: zero
    implicit none

!   Declare passed variables
    integer(i_kind)                      ,intent(in   ) :: iope
    logical                              ,intent(in   ) :: use_sfc_any
    real(r_single),  dimension(nlat_sfc,nlon_sfc,nfldsfc), intent(  out) :: sfct,soil_moi,sno,soil_temp,veg_frac,fact10,sfc_rough
    real(r_single),  dimension(nlat_sfc,nlon_sfc),         intent(  out) :: veg_type,soil_type,terrain
    integer(i_kind), dimension(nlat_sfc,nlon_sfc),         intent(  out) :: isli


!   Declare local variables
    integer(i_kind):: iret,npts,nptsall

!-----------------------------------------------------------------------------
!   Read surface file on processor iope
    if (mype == iope) then
       call read_sfc(sfct,soil_moi,sno,soil_temp,veg_frac,fact10,sfc_rough, &
                     veg_type,soil_type,terrain,isli,use_sfc_any)
    end if

!     Load onto all processors

    npts=nlat_sfc*nlon_sfc
    nptsall=npts*nfldsfc

    call mpi_bcast(sfct,      nptsall,mpi_rtype4,iope,mpi_comm_world,iret)
    call mpi_bcast(fact10,    nptsall,mpi_rtype4,iope,mpi_comm_world,iret)
    call mpi_bcast(sno,       nptsall,mpi_rtype4,iope,mpi_comm_world,iret)
    if(sfcmod_mm5 .or. sfcmod_gfs)then
       call mpi_bcast(sfc_rough, nptsall,mpi_rtype4,iope,mpi_comm_world,iret)
    else
       sfc_rough = zero
    end if
    call mpi_bcast(terrain,   npts,   mpi_rtype4,iope,mpi_comm_world,iret)
    call mpi_bcast(isli,      npts,   mpi_itype, iope,mpi_comm_world,iret)
    if(use_sfc_any)then
       call mpi_bcast(veg_frac, nptsall,mpi_rtype4,iope,mpi_comm_world,iret)
       call mpi_bcast(soil_temp,nptsall,mpi_rtype4,iope,mpi_comm_world,iret)
       call mpi_bcast(soil_moi, nptsall,mpi_rtype4,iope,mpi_comm_world,iret)
       call mpi_bcast(veg_type, npts,   mpi_rtype4,iope,mpi_comm_world,iret)
       call mpi_bcast(soil_type,npts,   mpi_rtype4,iope,mpi_comm_world,iret)
    end if

    return
  end subroutine read_gfssfc

  subroutine read_sfc_anl(isli_anl)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    read_sfc_anl
!
!   prgrmmr: li
!
! abstract: read a ncep GFS surface file with analysis grids resolution on a specified task,
!           broadcast data to other tasks. Currently, isli only.
!
! program history log:
!   2016-08-18  xuli - create routine
!
!
!   output argument list:
!     isli_anl      - sea/land/ice mask at analysis grids (nlat by nlon)
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
    ! read data from sfc file on a single task, bcast data to other tasks.
    use sfcio_module, only: sfcio_srohdc,sfcio_head,sfcio_data,sfcio_intkind
    use sfcio_module, only: sfcio_axdata,sfcio_sclose
    use kinds, only: i_kind,r_single,r_kind
    use gridmod, only: nlat,nlon

    integer(i_kind), dimension(nlat,nlon), intent(  out) :: isli_anl
    integer(i_kind) :: latb,lonb
    integer(i_kind) :: iret,i,j
    type(sfcio_head) :: sfc_head
    type(sfcio_data) :: sfc_data
    real(r_single), allocatable, dimension(:,:):: outtmp
    character(24) :: filename
!   Declare local parameters
    integer(sfcio_intkind):: lunanl = 21

! read a surface file with analysis resolution on the task : isli only currently
    filename='sfcf06_anlgrid'
    call sfcio_srohdc(lunanl,trim(filename),sfc_head,sfc_data,iret)
!   Check for possible problems
    if (iret /= 0) then
       write(6,*)'READ_SFC_ANL:  ***ERROR*** problem reading ',filename,&
            ', iret=',iret
       call sfcio_axdata(sfc_data,iret)
       call stop2(80)
    endif
    lonb = sfc_head%lonb
    latb = sfc_head%latb
    if ( (latb /= nlat-2) .or. (lonb /= nlon) ) then
         write(6,*)'READ_SFC_ANL:  ***ERROR*** inconsistent grid dimensions.  ',&
              ', nlon,nlat-2=',nlon,nlat-2,' -vs- sfc file lonb,latb=',&
                 lonb,latb
         call sfcio_axdata(sfc_data,iret)
         call stop2(80)
    endif

    allocate(outtmp(latb+2,lonb))
    call tran_gfssfc(sfc_data%slmsk,outtmp,lonb,latb)                       
    do j=1,lonb
       do i=1,latb+2
          isli_anl(i,j) = nint(outtmp(i,j))
       end do
    end do
    deallocate(outtmp)

!   Print date/time stamp
    write(6,700) latb,lonb,sfc_head%fhour,sfc_head%idate
700 format('READ_SFC_ANL:  ges read/scatter, nlat,nlon=',&
         2i6,', hour=',f10.1,', idate=',4i5)
    call sfcio_axdata(sfc_data,iret)
    call sfcio_sclose(lunanl,iret)

  end subroutine read_sfc_anl

  subroutine read_gfssfc_anl(iope,isli_anl)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_gfssfc_anl     read gfs ges surface file at analysis resolution
!
!   prgmmr: li          org: np23                date: 2016-08-18
!
! abstract: read gfs surface file
!
! program history log:
!
!   input argument list:
!     iope     - mpi task handling i/o
!
!   output argument list:
!     isli_anl      - sea/land/ice mask at analysis grids
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use kinds, only: r_kind,i_kind,r_single
    use gridmod, only: nlat,nlon
    use mpimod, only: mpi_itype,mpi_comm_world,mype
    use constants, only: zero
    implicit none

!   Declare passed variables
    integer(i_kind)                      ,intent(in   ) :: iope
    integer(i_kind), dimension(nlat,nlon), intent(  out) :: isli_anl

!   Declare local variables
    integer(i_kind):: iret,npts
!-----------------------------------------------------------------------------
!   Read a  surface file on processor iope
    if (mype == iope) then
       call read_sfc_anl(isli_anl)
    end if

!   Load onto all processors
    npts=nlat*nlon

    call mpi_bcast(isli_anl,npts,mpi_itype,iope,mpi_comm_world,iret)

    return
  end subroutine read_gfssfc_anl

  subroutine read_nst(tref,dt_cool,z_c,dt_warm,z_w,c_0,c_d,w_0,w_d)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    read_nst
!
!   prgrmmr: li
!
! abstract: driver to read nst fields
!
! program history log:
!   2015-04-24  li - create routine based on read_sfc
!
!   input argument list:
!     lunges             - unit number to use for IO
!     filename           - nst surface file to read
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
    use nstio_module, only: nstio_srohdc,nstio_head,nstio_data,nstio_intkind
    use nstio_module, only: nstio_axdata,nstio_srclose
    use kinds, only: i_kind,r_single,r_kind
    use gridmod, only: nlat_sfc,nlon_sfc
    use guess_grids, only: nfldnst,ifilenst
    use constants, only: two
    real(r_single), dimension(nlat_sfc,nlon_sfc,nfldnst),intent(  out) :: &
                    tref,dt_cool,z_c,dt_warm,z_w,c_0,c_d,w_0,w_d
    integer(i_kind) :: latb,lonb
    integer(i_kind) :: iret,n
    type(nstio_head) :: nst_head
    type(nstio_data) :: nst_data
    real(r_single),allocatable,dimension(:,:):: dwarm_tmp
    integer(i_kind) :: nnst,it
    character(24) :: filename
!   Declare local parameters
    integer(nstio_intkind):: lunges = 13
    integer(i_kind),parameter:: nnst_all = 9

    do it=1,nfldnst
! read a nst file on the task
       write(filename,200)ifilenst(it)
200    format('nstf',i2.2)
       call nstio_srohdc(lunges,filename,nst_head,nst_data,iret)
!      Check for possible problems
       if (iret /= 0) then
          write(6,*)'READ_NST:  ***ERROR*** problem reading ',filename,&
               ', iret=',iret
          call nstio_axdata(nst_data,iret)
          call stop2(80)
       endif
       lonb = nst_head%lonb
       latb = nst_head%latb
       if ( (latb /= nlat_sfc-2) .or. (lonb /= nlon_sfc) ) then
          write(6,*)'READ_NST:  ***ERROR*** inconsistent grid dimensions.  ',&
                 ', nlon,nlat-2=',nlon_sfc,nlat_sfc-2,' -vs- nst file lonb,latb=',&
                    lonb,latb
          call nstio_axdata(nst_data,iret)
          call stop2(80)
       endif

       nnst=nnst_all

!$omp parallel do private(n,dwarm_tmp)
       do n=1,nnst
 
          if(n == 1)then                            ! foundation temperature (Tf)

             call tran_gfssfc(nst_data%tref,tref(1,1,it),lonb,latb)                                 

          else if(n == 2) then                      ! cooling amount

             call tran_gfssfc(nst_data%dt_cool,dt_cool(1,1,it),lonb,latb)  

          else if(n == 3) then                      ! cooling layer thickness

             call tran_gfssfc(nst_data%z_c,z_c(1,1,it),lonb,latb)        

          else if(n == 4 ) then                     ! warming amount

             allocate(dwarm_tmp(lonb,latb))
             dwarm_tmp(:,:)  = two*nst_data%xt(:,:)/nst_data%xz(:,:)
             call tran_gfssfc(dwarm_tmp,dt_warm(1,1,it),lonb,latb)  
             deallocate(dwarm_tmp)

          else if(n == 5 ) then                     ! warm layer thickness

             call tran_gfssfc(nst_data%xz,z_w(1,1,it),lonb,latb)                       

          else if(n == 6) then                      ! coefficient 1 to get d(Tz)/d(Tf)

             call tran_gfssfc(nst_data%c_0,c_0(1,1,it),lonb,latb)                           

          else if(n == 7) then                      ! coefficient 2 to get d(Tz)/d(Tf)

             call tran_gfssfc(nst_data%c_d,c_d(1,1,it),lonb,latb)            

          else if(n == 8 ) then                     ! coefficient 3 to get d(Tz)/d(Tf)

             call tran_gfssfc(nst_data%w_0,w_0(1,1,it),lonb,latb)            

          else if(n == 9 ) then                     ! coefficient 4 to get d(Tz)/d(Tf)

             call tran_gfssfc(nst_data%w_d,w_d(1,1,it),lonb,latb)                     

          end if

!         End of loop over data records
       end do
!   Print date/time stamp
       write(6,700) latb,lonb,nst_head%fhour,nst_head%idate
700    format('READ_NST:  ges read/scatter, nlat,nlon=',&
            2i6,', hour=',f10.1,', idate=',4i5)
       call nstio_axdata(nst_data,iret)
       call nstio_srclose(lunges,iret)
!   End of loop over time levels
    end do

  end subroutine read_nst


  subroutine read_gfsnst(iope,tref,dt_cool,z_c,dt_warm,z_w,c_0,c_d,w_0,w_d)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_gfsnst   
!   prgmmr: li          org: np23                date: 2009-08-26
!
! abstract: read gfs nst fields from a specific task and then broadcast to others
! 
!
! program history log:
!   2015-04-25  li : modify to minimize communications/IO
!
!   input argument list:
!     iope     - mpi task handling i/o
!
!   output argument list:
!   tref     (:,:)                        ! oceanic foundation temperature
!   dt_cool  (:,:)                        ! sub-layer cooling amount at sub-skin layer
!   z_c      (:,:)                        ! depth of sub-layer cooling layer
!   dt_warm  (:,:)                        ! diurnal warming amount at sea surface (skin layer)
!   z_w      (:,:)                        ! depth of diurnal warming layer
!   c_0      (:,:)                        ! coefficient to calculate d(Tz)/d(tr) in dimensionless
!   c_d      (:,:)                        ! coefficient to calculate d(Tz)/d(tr) in m^-1
!   w_0      (:,:)                        ! coefficient to calculate d(Tz)/d(tr) in dimensionless
!   w_d      (:,:)                        ! coefficient to calculate d(Tz)/d(tr) in m^-1
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use kinds, only: r_kind,i_kind,r_single
    use gridmod, only: nlat_sfc,nlon_sfc
    use guess_grids, only: nfldnst
    use mpimod, only: mpi_itype,mpi_rtype4,mpi_comm_world
    use mpimod, only: mype
    use constants, only: zero
    implicit none

!   Declare passed variables
    integer(i_kind)                      ,intent(in   ) :: iope
    real(r_single), dimension(nlat_sfc,nlon_sfc,nfldnst),intent(  out) :: &
                    tref,dt_cool,z_c,dt_warm,z_w,c_0,c_d,w_0,w_d

!   Declare local variables
    integer(i_kind):: iret,npts,nptsall

!-----------------------------------------------------------------------------
!   Read nst file on processor iope
    if(mype == iope)then
      call read_nst(tref,dt_cool,z_c,dt_warm,z_w,c_0,c_d,w_0,w_d)
    end if

!   Load onto all processors

    npts=nlat_sfc*nlon_sfc
    nptsall=npts*nfldnst

    call mpi_bcast(tref,    nptsall,mpi_rtype4,iope,mpi_comm_world,iret)
    call mpi_bcast(dt_cool, nptsall,mpi_rtype4,iope,mpi_comm_world,iret)
    call mpi_bcast(z_c,     nptsall,mpi_rtype4,iope,mpi_comm_world,iret)
    call mpi_bcast(dt_warm, nptsall,mpi_rtype4,iope,mpi_comm_world,iret)
    call mpi_bcast(z_w,     nptsall,mpi_rtype4,iope,mpi_comm_world,iret)
    call mpi_bcast(c_0,     nptsall,mpi_rtype4,iope,mpi_comm_world,iret)
    call mpi_bcast(c_d,     nptsall,mpi_rtype4,iope,mpi_comm_world,iret)
    call mpi_bcast(w_0,     nptsall,mpi_rtype4,iope,mpi_comm_world,iret)
    call mpi_bcast(w_d,     nptsall,mpi_rtype4,iope,mpi_comm_world,iret)

    return
  end subroutine read_gfsnst

  subroutine write_gfs(increment,mype_atm,mype_sfc)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    write_gfs
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2006-07-31  kleist - pass ges_ps instead of ges_lnps
!   2006-10-11  treadon - update 10m wind factor in sfc file
!   2008-05-28  safford - rm unused vars, add doc block
!   2008-12-05  todling - adjustment for dsfct time dimension addition
!   2009-08-28  li      - add nst i/o
!   2009-11-28  todling - add increment option (hook-only for now)
!   2010-03-31  treadon - add hires_b, sp_a, and sp_b
!   2011-05-01  todling - cwmr no longer in guess-grids; use metguess bundle now
!   2013-02-26  m.kim -  recompute and write cw analysis (= original cw gues + increment)                  
!                        where cw increments are calculated with nonnegative cw
!                        gues while original cw gues still have negative values.
!   2013-10-19  todling - update cloud_efr module name
!   2013-10-29  todling - revisit write to allow skipping vars not in MetGuess
!   2018-05-19  eliu    - add I/O for fv3 hydrometeors
!   2019-03-21  Wei/Martin - write out global aerosol arrays if needed
!   2019-09-04  martin  - added option to write fv3 netcdf increment file
!   2019-09-24  martin  - added logic for when use_gfs_ncio is true, note
!                         writing netCDF analysis for GFS not currently supported
!
!   input argument list:
!     increment          - when >0 will write increment from increment-index slot
!     mype_atm,mype_sfc  -
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

    use kinds, only: i_kind,r_kind
    use mpimod, only: mype
    use guess_grids, only: dsfct
    use guess_grids, only: ntguessig,ntguessfc,ifilesig,nfldsig
    use gridmod, only: hires_b,sp_a,grd_a,jcap_b,nlon,nlat,use_gfs_nemsio,write_fv3_incr,use_gfs_ncio
    use gsi_metguess_mod, only: gsi_metguess_bundle
    use gsi_bundlemod, only: gsi_bundlegetpointer
    use gsi_bundlemod, only: gsi_grid
    use gsi_bundlemod, only: gsi_gridcreate
    use gsi_bundlemod, only: gsi_bundle
    use gsi_bundlemod, only: gsi_bundlecreate
    use gsi_bundlemod, only: gsi_bundledestroy
    use mpeu_util, only: die
    use gsi_nstcouplermod, only: nst_gsi
    use constants, only: qcmin 
    use constants, only:zero
    use general_specmod, only: general_init_spec_vars,general_destroy_spec_vars,spec_vars
    use gsi_4dvar, only: lwrite4danl,nhr_anal
    use ncepnems_io, only: write_nemsatm,write_nemssfc,write_nems_sfc_nst
    use netcdfgfs_io, only: write_gfsncsfc, write_gfsnc_sfc_nst, write_gfsncatm
    use write_incr, only: write_fv3_increment
    use ncepnems_io, only: write_fv3atm_nems     
    use gridmod, only: fv3_full_hydro   
    use gsi_chemguess_mod, only: gsi_chemguess_get,gsi_chemguess_bundle
    use chemmod, only: laeroana_gocart
    use radiance_mod, only: aerosol_names

    implicit none

    integer(i_kind),intent(in   ) :: increment
    integer(i_kind),intent(in   ) :: mype_atm,mype_sfc
    character(24):: filename
    integer(i_kind) :: itoutsig,istatus,iret_write,nlon_b,ntlevs,it

    real(r_kind),pointer,dimension(:,:  ):: aux_ps
    real(r_kind),pointer,dimension(:,:,:):: aux_u
    real(r_kind),pointer,dimension(:,:,:):: aux_v
    real(r_kind),pointer,dimension(:,:,:):: aux_vor
    real(r_kind),pointer,dimension(:,:,:):: aux_div
    real(r_kind),pointer,dimension(:,:,:):: aux_tv
    real(r_kind),pointer,dimension(:,:,:):: aux_q
    real(r_kind),pointer,dimension(:,:,:):: aux_oz
    real(r_kind),pointer,dimension(:,:,:):: aux_cwmr
    real(r_kind),pointer,dimension(:,:,:):: aux_ql
    real(r_kind),pointer,dimension(:,:,:):: aux_qi
    real(r_kind),pointer,dimension(:,:,:):: aux_qr
    real(r_kind),pointer,dimension(:,:,:):: aux_qs
    real(r_kind),pointer,dimension(:,:,:):: aux_qg
    real(r_kind),pointer,dimension(:,:,:):: aux_cf
    real(r_kind),pointer,dimension(:,:  ):: ges_ps_it  =>null()
    real(r_kind),pointer,dimension(:,:,:):: ges_u_it   =>null()
    real(r_kind),pointer,dimension(:,:,:):: ges_v_it   =>null()
    real(r_kind),pointer,dimension(:,:,:):: ges_div_it =>null()
    real(r_kind),pointer,dimension(:,:,:):: ges_vor_it =>null()
    real(r_kind),pointer,dimension(:,:,:):: ges_tv_it  =>null()
    real(r_kind),pointer,dimension(:,:,:):: ges_q_it   =>null()
    real(r_kind),pointer,dimension(:,:,:):: ges_oz_it  =>null()
    real(r_kind),pointer,dimension(:,:,:):: ges_cwmr_it=>null()
    real(r_kind),pointer,dimension(:,:,:):: ges_ql_it  =>null()
    real(r_kind),pointer,dimension(:,:,:):: ges_qi_it  =>null()
    real(r_kind),pointer,dimension(:,:,:):: ges_qr_it  =>null()
    real(r_kind),pointer,dimension(:,:,:):: ges_qs_it  =>null()
    real(r_kind),pointer,dimension(:,:,:):: ges_qg_it  =>null()
    real(r_kind),pointer,dimension(:,:,:):: ges_cf_it  =>null()

!   for aerosols
    real(r_kind),pointer,dimension(:,:,:):: aux_du1,aux_du2,aux_du3,aux_du4,aux_du5
    real(r_kind),pointer,dimension(:,:,:):: aux_ss1,aux_ss2,aux_ss3,aux_ss4,aux_so4
    real(r_kind),pointer,dimension(:,:,:):: aux_oc1,aux_oc2,aux_bc1,aux_bc2
    real(r_kind),pointer,dimension(:,:,:):: ges_du1_it=>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_du2_it=>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_du3_it=>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_du4_it=>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_du5_it=>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_ss1_it=>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_ss2_it=>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_ss3_it=>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_ss4_it=>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_so4_it=>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_oc1_it=>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_oc2_it=>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_bc1_it=>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_bc2_it=>NULL()
    type(gsi_bundle) :: chem_bundle
    type(gsi_bundle) :: atm_bundle
    type(gsi_grid)   :: atm_grid
    integer(i_kind),parameter :: n2d=2
    integer(i_kind),parameter :: n3d=14   
    character(len=4), parameter :: vars2d(n2d) = (/ 'z   ', 'ps  ' /)
    character(len=4), parameter :: vars3d(n3d) = (/ 'u   ', 'v   ', &
                                                    'vor ', 'div ', &
                                                    'tv  ', 'q   ', &
                                                    'cw  ', 'oz  ', &
                                                    'ql  ', 'qi  ', &  
                                                    'qr  ', 'qs  ', &  
                                                    'qg  ', 'cf  ' /)  


    logical :: inithead
    type(spec_vars):: sp_b

    ! Write atmospheric analysis file
    if ( lwrite4danl ) then
       ntlevs=nfldsig
    else
       ntlevs=1
    end if

    ! Allocate bundle used for writing
    call gsi_gridcreate(atm_grid,grd_a%lat2,grd_a%lon2,grd_a%nsig)
    call gsi_bundlecreate(atm_bundle,atm_grid,'aux-atm-write',istatus,names2d=vars2d,names3d=vars3d)
    if ( istatus /= 0 ) then
      write(6,*)' write_gfs: trouble creating atm_bundle'
      call stop2(999)
    endif

    call gsi_bundlegetpointer(atm_bundle,'ps',aux_ps,istatus)
    if ( istatus == 0 ) aux_ps = zero
    call gsi_bundlegetpointer(atm_bundle,'u',aux_u,istatus)
    if ( istatus == 0 ) aux_u = zero
    call gsi_bundlegetpointer(atm_bundle,'v',aux_v,istatus)
    if ( istatus == 0 ) aux_v = zero
    call gsi_bundlegetpointer(atm_bundle,'vor',aux_vor,istatus)
    if ( istatus == 0 ) aux_vor = zero
    call gsi_bundlegetpointer(atm_bundle,'div',aux_div,istatus)
    if ( istatus == 0 ) aux_div = zero
    call gsi_bundlegetpointer(atm_bundle,'tv',aux_tv,istatus)
    if ( istatus == 0 ) aux_tv = zero
    call gsi_bundlegetpointer(atm_bundle,'q',aux_q,istatus)
    if ( istatus == 0 ) aux_q = zero
    call gsi_bundlegetpointer(atm_bundle,'oz',aux_oz,istatus)
    if ( istatus == 0 ) aux_oz = zero
    call gsi_bundlegetpointer(atm_bundle,'ql',aux_ql,istatus)
    if ( istatus == 0 ) aux_ql = zero
    call gsi_bundlegetpointer(atm_bundle,'qi',aux_qi,istatus)
    if ( istatus == 0 ) aux_qi = zero
    call gsi_bundlegetpointer(atm_bundle,'qr',aux_qr,istatus)
    if ( istatus == 0 ) aux_qr = zero
    call gsi_bundlegetpointer(atm_bundle,'qs',aux_qs,istatus)
    if ( istatus == 0 ) aux_qs = zero
    call gsi_bundlegetpointer(atm_bundle,'qg',aux_qg,istatus)
    if ( istatus == 0 ) aux_qg = zero
    call gsi_bundlegetpointer(atm_bundle,'cf',aux_cf,istatus)
    if ( istatus == 0 ) aux_cf = zero
    call gsi_bundlegetpointer(atm_bundle,'cw',aux_cwmr,istatus)
    if ( istatus == 0 ) aux_cwmr = zero
    
    ! if aerosols
    if ( laeroana_gocart ) then
       call gsi_bundlecreate(chem_bundle,atm_grid,'aux-chem-write',istatus,names3d=aerosol_names)
       if ( istatus /= 0 ) then
          write(6,*)' write_gfs: trouble creating chem_bundle'
          call stop2(999)
       endif
       call gsi_bundlegetpointer(chem_bundle,'sulf',aux_so4,istatus)
       if ( istatus == 0 ) aux_so4 = zero
       call gsi_bundlegetpointer(chem_bundle,'bc1',aux_bc1,istatus)
       if ( istatus == 0 ) aux_bc1 = zero
       call gsi_bundlegetpointer(chem_bundle,'bc2',aux_bc2,istatus)
       if ( istatus == 0 ) aux_bc2 = zero
       call gsi_bundlegetpointer(chem_bundle,'oc1',aux_oc1,istatus)
       if ( istatus == 0 ) aux_oc1 = zero
       call gsi_bundlegetpointer(chem_bundle,'oc2',aux_oc2,istatus)
       if ( istatus == 0 ) aux_oc2 = zero
       call gsi_bundlegetpointer(chem_bundle,'dust1',aux_du1,istatus)
       if ( istatus == 0 ) aux_du1 = zero
       call gsi_bundlegetpointer(chem_bundle,'dust2',aux_du2,istatus)
       if ( istatus == 0 ) aux_du2 = zero
       call gsi_bundlegetpointer(chem_bundle,'dust3',aux_du3,istatus)
       if ( istatus == 0 ) aux_du3 = zero
       call gsi_bundlegetpointer(chem_bundle,'dust4',aux_du4,istatus)
       if ( istatus == 0 ) aux_du4 = zero
       call gsi_bundlegetpointer(chem_bundle,'dust5',aux_du5,istatus)
       if ( istatus == 0 ) aux_du5 = zero
       call gsi_bundlegetpointer(chem_bundle,'seas1',aux_ss1,istatus)
       if ( istatus == 0 ) aux_ss1 = zero
       call gsi_bundlegetpointer(chem_bundle,'seas2',aux_ss2,istatus)
       if ( istatus == 0 ) aux_ss2 = zero
       call gsi_bundlegetpointer(chem_bundle,'seas3',aux_ss3,istatus)
       if ( istatus == 0 ) aux_ss3 = zero
       call gsi_bundlegetpointer(chem_bundle,'seas4',aux_ss4,istatus)
       if ( istatus == 0 ) aux_ss4 = zero
    end if ! laeroana_gocart

    inithead=.true.
    do it=1,ntlevs

        if ( lwrite4danl ) then
            ! check to see if we want to output this time.
            ! if not, skip to next time
            if (count(nhr_anal/=0)>0) then
               if (count(nhr_anal==ifilesig(it))==0) cycle
            endif
            itoutsig = it
            if ( it == ntguessig ) then
               if ( increment > 0 .or. write_fv3_incr ) then
                   filename = 'siginc'
               else
                   filename = 'siganl'
               endif
            else
               if ( increment > 0 .or. write_fv3_incr ) then
                   write(filename,"('sigi',i2.2)") ifilesig(it)
               else
                   write(filename,"('siga',i2.2)") ifilesig(it)
               endif
            endif
        else
            itoutsig = ntguessig
            if ( increment > 0 .or. write_fv3_incr ) then
                filename = 'siginc'
            else
                filename = 'siganl'
            endif
        endif

        if ( mype == 0 ) then
            if ( increment > 0 .or. write_fv3_incr ) then
                write(6,'(A,I2.2)') 'WRITE_GFS: writing analysis increment for FHR ', ifilesig(itoutsig)
            else
                write(6,'(A,I2.2)') 'WRITE_GFS: writing full analysis state for FHR ', ifilesig(itoutsig)
            endif
        endif
        
        call gsi_bundlegetpointer (gsi_metguess_bundle(itoutsig),'ps',ges_ps_it  ,istatus) 
        if ( istatus == 0 ) aux_ps = ges_ps_it
        call gsi_bundlegetpointer (gsi_metguess_bundle(itoutsig),'u' ,ges_u_it   ,istatus) 
        if ( istatus == 0 ) aux_u = ges_u_it
        call gsi_bundlegetpointer (gsi_metguess_bundle(itoutsig),'v' ,ges_v_it   ,istatus) 
        if ( istatus == 0 ) aux_v = ges_v_it
        call gsi_bundlegetpointer (gsi_metguess_bundle(itoutsig),'vor',ges_vor_it,istatus) 
        if ( istatus == 0 ) aux_vor = ges_vor_it
        call gsi_bundlegetpointer (gsi_metguess_bundle(itoutsig),'div',ges_div_it,istatus) 
        if ( istatus == 0 ) aux_div = ges_div_it
        call gsi_bundlegetpointer (gsi_metguess_bundle(itoutsig),'tv',ges_tv_it  ,istatus) 
        if ( istatus == 0 ) aux_tv = ges_tv_it
        call gsi_bundlegetpointer (gsi_metguess_bundle(itoutsig),'q' ,ges_q_it   ,istatus) 
        if ( istatus == 0 ) aux_q = ges_q_it
        call gsi_bundlegetpointer (gsi_metguess_bundle(itoutsig),'oz',ges_oz_it  ,istatus) 
        if ( istatus == 0 ) aux_oz = ges_oz_it
        call gsi_bundlegetpointer (gsi_metguess_bundle(itoutsig),'ql',ges_ql_it,istatus)
        if ( istatus == 0 ) aux_ql = ges_ql_it
        call gsi_bundlegetpointer (gsi_metguess_bundle(itoutsig),'qi',ges_qi_it,istatus)
        if ( istatus == 0 ) aux_qi = ges_qi_it
        call gsi_bundlegetpointer (gsi_metguess_bundle(itoutsig),'qr',ges_qr_it,istatus)
        if ( istatus == 0 ) aux_qr = ges_qr_it
        call gsi_bundlegetpointer (gsi_metguess_bundle(itoutsig),'qs',ges_qs_it,istatus)
        if ( istatus == 0 ) aux_qs = ges_qs_it
        call gsi_bundlegetpointer (gsi_metguess_bundle(itoutsig),'qg',ges_qg_it,istatus)
        if ( istatus == 0 ) aux_qg = ges_qg_it
        call gsi_bundlegetpointer (gsi_metguess_bundle(itoutsig),'cf',ges_cf_it,istatus)
        if ( istatus == 0 ) aux_cf = ges_cf_it
        call gsi_bundlegetpointer (gsi_metguess_bundle(itoutsig),'cw',ges_cwmr_it,istatus)
        if ( istatus == 0 ) aux_cwmr = ges_cwmr_it

! if aerosols, get the data from chem bundle to output
        if ( laeroana_gocart ) then
           call gsi_bundlegetpointer(gsi_chemguess_bundle(itoutsig),'dust1',ges_du1_it,istatus)
           if( istatus==0 ) aux_du1 = ges_du1_it
           call gsi_bundlegetpointer(gsi_chemguess_bundle(itoutsig),'dust2',ges_du2_it,istatus)
           if( istatus==0 ) aux_du2 = ges_du2_it
           call gsi_bundlegetpointer(gsi_chemguess_bundle(itoutsig),'dust3',ges_du3_it,istatus)
           if( istatus==0 ) aux_du3 = ges_du3_it
           call gsi_bundlegetpointer(gsi_chemguess_bundle(itoutsig),'dust4',ges_du4_it,istatus)
           if( istatus==0 ) aux_du4 = ges_du4_it
           call gsi_bundlegetpointer(gsi_chemguess_bundle(itoutsig),'dust5',ges_du5_it,istatus)
           if( istatus==0 ) aux_du5 = ges_du5_it
           call gsi_bundlegetpointer(gsi_chemguess_bundle(itoutsig),'seas1',ges_ss1_it,istatus)
           if( istatus==0 ) aux_ss1 = ges_ss1_it
           call gsi_bundlegetpointer(gsi_chemguess_bundle(itoutsig),'seas2',ges_ss2_it,istatus)
           if( istatus==0 ) aux_ss2 = ges_ss2_it
           call gsi_bundlegetpointer(gsi_chemguess_bundle(itoutsig),'seas3',ges_ss3_it,istatus)
           if( istatus==0 ) aux_ss3 = ges_ss3_it
           call gsi_bundlegetpointer(gsi_chemguess_bundle(itoutsig),'seas4',ges_ss4_it,istatus)
           if( istatus==0 ) aux_ss4 = ges_ss4_it
           call gsi_bundlegetpointer (gsi_chemguess_bundle(itoutsig),'sulf',ges_so4_it,istatus)
           if( istatus==0 ) aux_so4 = ges_so4_it
           call gsi_bundlegetpointer (gsi_chemguess_bundle(itoutsig),'oc1',ges_oc1_it,istatus)
           if( istatus==0 ) aux_oc1 = ges_oc1_it
           call gsi_bundlegetpointer (gsi_chemguess_bundle(itoutsig),'oc2',ges_oc2_it,istatus)
           if( istatus==0 ) aux_oc2 = ges_oc2_it
           call gsi_bundlegetpointer (gsi_chemguess_bundle(itoutsig),'bc1',ges_bc1_it,istatus)
           if( istatus==0 ) aux_bc1 = ges_bc1_it
           call gsi_bundlegetpointer (gsi_chemguess_bundle(itoutsig),'bc2',ges_bc2_it,istatus)
           if( istatus==0 ) aux_bc2 = ges_bc2_it
        end if ! laeroana_gocart
        if ( use_gfs_nemsio ) then

            if ( write_fv3_incr ) then
                call write_fv3_increment(grd_a,filename,mype_atm, &
                     atm_bundle,itoutsig)
            else
                if (fv3_full_hydro) then
                   call write_fv3atm_nems(grd_a,sp_a,filename,mype_atm, &
                        atm_bundle,itoutsig)
                else
                   ! if using aerosols, optional chem_bundle argument
                   if ( laeroana_gocart ) then
                       call write_nemsatm(grd_a,sp_a,filename,mype_atm, &
                            atm_bundle,itoutsig,chem_bundle)
                   else
                   ! otherwise, just atm_bundle
                       call write_nemsatm(grd_a,sp_a,filename,mype_atm, &
                            atm_bundle,itoutsig)
                   end if ! laeroana_gocart
                endif  
            end if

        else if ( use_gfs_ncio ) then
            if  ( write_fv3_incr ) then
                call write_fv3_increment(grd_a,filename,mype_atm, &
                     atm_bundle,itoutsig)
            else
                call write_gfsncatm(grd_a,sp_a,filename,mype_atm, &
                     atm_bundle,itoutsig)
            end if
        else

            ! If hires_b, spectral to grid transform for background
            ! uses double FFT.   Need to pass in sp_a and sp_b
            nlon_b=((2*jcap_b+1)/nlon+1)*nlon
            if ( nlon_b /= sp_a%imax ) then
                hires_b=.true.
                call general_init_spec_vars(sp_b,jcap_b,jcap_b,nlat,nlon_b)
                if ( mype == 0 ) &
                    write(6,*)'WRITE_GFS:  allocate and load sp_b with jcap,imax,jmax=',&
                              sp_b%jcap,sp_b%imax,sp_b%jmax
             
                call general_write_gfsatm(grd_a,sp_a,sp_b,filename,mype_atm, &
                     atm_bundle,itoutsig,inithead,iret_write)
             
                call general_destroy_spec_vars(sp_b)
            ! Otherwise, use standard transform.  Use sp_a in place of sp_b.
            else
                call general_write_gfsatm(grd_a,sp_a,sp_a,filename,mype_atm, &
                     atm_bundle,itoutsig,inithead,iret_write)
            endif

        endif

        inithead=.false.

    enddo ! end do over ntlevs

    ! Write surface analysis file
    if ( increment > 0 ) then
        filename='sfcinc.gsi'
        if ( use_gfs_nemsio ) then
            call write_nemssfc(filename,mype_sfc,dsfct(:,:,ntguessfc))
        else if ( use_gfs_ncio ) then
            call write_gfsncsfc(filename,mype_sfc,dsfct(:,:,ntguessfc))
        else
            call write_gfssfc(filename,mype_sfc,dsfct(1,1,ntguessfc))
        endif
    else
       if ( nst_gsi > 0 ) then
          if ( sfcnst_comb ) then
             call write_tf_inc_nc(mype_sfc,dsfct(:,:,ntguessfc))
          else
             if ( use_gfs_nemsio ) then
                 call write_nems_sfc_nst(mype_sfc,dsfct(:,:,ntguessfc))
             else if ( use_gfs_ncio ) then
                 call write_gfsnc_sfc_nst(mype_sfc,dsfct(:,:,ntguessfc))
             else
                 call write_gfs_sfc_nst (mype_sfc,dsfct(1,1,ntguessfc))
             endif
          endif
       else
           filename='sfcanl.gsi'
           if ( use_gfs_nemsio ) then
               call write_nemssfc(filename,mype_sfc,dsfct(:,:,ntguessfc))
           else if ( use_gfs_ncio ) then
               call write_gfsncsfc(filename,mype_sfc,dsfct(:,:,ntguessfc))
           else
               call write_gfssfc (filename,mype_sfc,dsfct(1,1,ntguessfc))
           endif
       endif
    endif

  end subroutine write_gfs

  subroutine write_gfssfc(filename,mype_sfc,dsfct)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    write_gfssfc --- Write surface analysis to file
!
!   prgrmmr:     treadon -  initial version; org: np22
!
! abstract:     This routine writes the updated surface analysis.  At
!               this point (20040615) the only surface field update by 
!               the gsi is the skin temperature.  The current (20040615)
!               GDAS setup does use the updated surface file.  Rather,
!               the output from surface cycle is used as the surface
!               analysis for subsequent GFS runs.
!
!               The routine gathers surface fields from subdomains, 
!               reformats the data records, and then writes each record
!               to the output file.  
!
!               Since the gsi only update the skin temperature, all
!               other surface fields are simply read from the guess
!               surface file and written to the analysis file.
!
!   Structure of GFS surface file  
!       data record  1    label
!       data record  2    date, dimension, version, lons/lat record
!       data record  3    tsf
!       data record  4    soilm(two layers)
!       data record  5    snow
!       data record  6    soilt(two layers)
!       data record  7    tg3
!       data record  8    zor
!       data record  9    cv
!       data record 10    cvb
!       data record 11    cvt
!       data record 12    albedo (four types)
!       data record 13    slimsk
!       data record 14    vegetation cover
!       data record 15    plantr
!       data record 16    f10m
!       data record 17    canopy water content (cnpanl)
!       data record 18    vegetation type
!       data record 19    soil type
!       data record 20    zenith angle dependent vegetation fraction (two types)
!
! program history log:
!   2004-06-15  treadon -  updated documentation
!   2004-07-15  todling -  protex-compliant prologue; added intent/only's
!   2004-12-03  treadon -  replace mpe_igatherv (IBM extension) with
!                          standard mpi_gatherv
!   2005-01-27  treadon - rewrite to make use of sfcio module
!   2005-02-09  kleist  - clean up unit number and filename for updated surface file
!   2005-03-07  todling -  die gracefully when return error from sfcio
!   2005-03-10  treadon - remove iadate from calling list, access via obsmod
!   2006-10-11  treadon - update 10m wind factor in sfc file
!   2008-05-28  safford - rm unused vars
!   2013-10-25  todling - move ltosj/s to comm_vars
!
!   input argument list:
!     filename  - file to open and write to
!     dsfct     - delta skin temperature
!     mype_sfc  - mpi task to write output file
!
!   output argument list:
!
! attributes:
!   language: f90
!   machines: ibm RS/6000 SP; SGI Origin 2000; Compaq HP
!
!$$$ end documentation block

! !USES:
    use kinds, only: r_kind,r_single,i_kind
  
    use mpimod, only: mpi_rtype
    use mpimod, only: mpi_comm_world
    use mpimod, only: ierror
    use mpimod, only: mype
    
    use gridmod, only: nlat,nlon
    use gridmod, only: lat1,lon1
    use gridmod, only: lat2,lon2
    use gridmod, only: iglobal
    use gridmod, only: ijn
    use gridmod, only: displs_g
    use gridmod, only: itotsub
    use gridmod, only: rlats,rlons,rlats_sfc,rlons_sfc
    
    use general_commvars_mod, only: ltosi,ltosj

    use obsmod, only: iadate
    use ncepnems_io, only: intrp22
    
    use constants, only: zero_single
    
    use sfcio_module, only: sfcio_intkind,sfcio_head,sfcio_data,&
         sfcio_srohdc,sfcio_swohdc,sfcio_axdata
    
    implicit none

! !INPUT PARAMETERS:
    character(*)                     ,intent(in   ) :: filename  ! file to open and write to

    real(r_kind),dimension(lat2,lon2),intent(in   ) :: dsfct   ! delta skin temperature

    integer(i_kind)                  ,intent(in   ) :: mype_sfc ! mpi task to write output file

! !OUTPUT PARAMETERS:

!-------------------------------------------------------------------------

!   Declare local parameters
    character( 6),parameter:: fname_ges='sfcf06'
    integer(sfcio_intkind),parameter:: ioges = 12
    integer(sfcio_intkind),parameter:: ioanl = 52

    real(r_kind),parameter :: houra = zero_single

!   Declare local variables
    integer(sfcio_intkind):: iret
    integer(i_kind) latb,lonb,nlatm2
    integer(i_kind) i,j,ip1,jp1,ilat,ilon,jj,mm1

    real(r_kind),dimension(lat1,lon1):: sfcsub
    real(r_kind),dimension(nlon,nlat):: grid
    real(r_kind),dimension(max(iglobal,itotsub)):: sfcall
    real(r_single),dimension(nlon,nlat):: buffer
    real(r_single),allocatable,dimension(:,:):: buffer2

    type(sfcio_head):: head
    type(sfcio_data):: data

  
!*****************************************************************************

!   Initialize local variables
    mm1=mype+1
    nlatm2=nlat-2

!   Gather skin temperature information from all tasks.  
    do j=1,lon1
       jp1 = j+1
       do i=1,lat1
          ip1 = i+1
          sfcsub(i,j)=dsfct(ip1,jp1)
       end do
    end do
    call mpi_gatherv(sfcsub,ijn(mm1),mpi_rtype,&
         sfcall,ijn,displs_g,mpi_rtype,mype_sfc,&
         mpi_comm_world,ierror)

!   Only MPI task mype_sfc writes the surface file.
    if (mype==mype_sfc) then

!      Reorder updated skin temperature to output format
       do i=1,iglobal
          ilon=ltosj(i)
          ilat=ltosi(i)
          grid(ilon,ilat)=sfcall(i)
       end do
       do j=1,nlat
          jj=nlat-j+1
          do i=1,nlon
             buffer(i,j)=grid(i,jj)
          end do
       end do

!      For now, rather than carry around all the surface fields in memory from
!      the read in ingesfc, just read fields from surface file.  Also, for
!      now, only update the 6-hour forecast surface guess file.

!      Read surface guess file
       call sfcio_srohdc(ioges,fname_ges,head,data,iret)
       if (iret /= 0) then
          write(6,*)'WRITE_GFSSFC:  ***ERROR*** problem reading ',fname_ges,&
               ', iret=',iret
          call sfcio_axdata(data,iret)
          call stop2(80)
       endif
       latb=head%latb
       lonb=head%lonb
       allocate(buffer2(lonb,latb))
       if ( (latb /= nlatm2) .or. &
            (lonb /= nlon) ) then
          write(6,*)'WRITE_GFSSFC:  different grid dimensions analysis vs sfc. interpolating sfc temperature  ',&
               ', nlon,nlat-2=',nlon,nlatm2,' -vs- sfc file lonb,latb=',&
               lonb,latb
          call intrp22(buffer, rlons,rlats,nlon,nlat, &
                       buffer2,rlons_sfc,rlats_sfc,lonb,latb)
       else
          do j=1,latb
             do i=1,lonb
                buffer2(i,j)=buffer(i,j+1)
             end do
          end do
       endif

!      Update guess date/time to analysis date/time
       head%fhour = houra       ! forecast hour
       head%idate(1)=iadate(4)  ! hour
       head%idate(2)=iadate(2)  ! month
       head%idate(3)=iadate(3)  ! day
       head%idate(4)=iadate(1)  ! year


       do j=1,latb
          do i=1,lonb
             data%tsea(i,j) = data%tsea(i,j)+buffer2(i,j)
          end do
       end do
       deallocate(buffer2)

!      Write updated information to surface analysis file
       call sfcio_swohdc(ioanl,filename,head,data,iret)


!      Deallocate local work arrays
       call sfcio_axdata(data,iret)

       write(6,100) lonb,latb,houra,iadate(1:4),iret
100    format(' WRITE_GFSSFC:  sfc analysis written  for ',&
            2i6,1x,f4.1,4(i4,1x),' with iret=',i2)

    endif
    
!   End of routine
    return
  end subroutine write_gfssfc

  subroutine write_gfs_sfc_nst(mype_so,dsfct)
!
! abstract: write both sfc and nst analysis files (nst_gsi dependent) for static (full resolution) run
!
!  REMARKS:
!
!   language: f90
!   machines: ibm RS/6000 SP; SGI Origin 2000; Compaq HP
!
!  AUTHOR:
!
!   2009-08-28  xu li -  initial version; org: np22
!   REVISION HISTORY:
!   2014-01-22  xu li -  modified to interpolate with surface mask info accounted
!                        and handle the sea ice melting (new open water grids)
!
!EOP

!  DESCRIPTION:
!  1. Background
!     In the current operational GFS, although the atmospheric variables are
!     analyzed/updated 6-hourly,
!     the surface variables are handled differently. The SST and sea ice are
!     updated 24-hourly with the independent analysis. The land variables are
!     not analyzed yet and the 6-hour forecast is simply used as their analysis.
!     Practically, the analysis file (sfcanl) is generated by updating SST and sea
!     ice in the 6-hour forecasting file (sfcf06 or sfcges) with globale_cycle.
!
!     With NSST model to provide the diurnal warming (dTw) and sub-layer cooling
!     (dTc) at atmospheric model time step, and Tr analysis to provide the
!     foundation temperature (Tf) analysis every 6 hour, SST = Tf + dTw - dTc
!     This enable to update SST (Tr as well) 6-hourly as the atmospheric
!     variables
!     the new files (nstf06, nstges and nstanl) needs to be processed
!
!  2. When nst_gsi > 0, This routine generates the sfc & nst analysis files (sfcanl and nstanl) by
!     (1) reading sfcgcy (sfcf06 applied with global_cycle) and nstf06
!     (2) writing/updating the SST (tsea) and Tr (tref) respectively to get sfcanl and nstanl
!
!  3. The interpolation of global dsfct at one grids (lower resolaution, e.g.,1152 x 576)
!     to another grids (higher resolution, e.g., 1760 x 880) with surface mask
!     info accounted
!     The main ideas of the surface mask dependent interpolation:
!     (1) Bilinear interpolation is applied.
!     (2) A preparation step is adopted to get more specified surface type points in
!         the source. This can be done more than one time
!     (3) For a target point, the candidates from the source must have the identical surface type
!     (4) If none of the 4 nearby grids has the same surface type as the target
!         point, the search area is expanded to one grid futher in each
!         direction.
!         This means 16 more grids will be searched
!     (5) The surface mask dependent interpolation can be done for  more surface type (0, 1, 2, 3  or more)
!         At present, the interpolation is only performed for open water grids(0)
!
!  4. Notes
!     (1) Tr (foundation temperature), instead of skin temperature, is the analysis variable.
!     (2) The generation of sfanl is nst_gsi dependent.
!         nst_gsi = 0 (default): No NST info at all;
!         nst_gsi = 1          : Input NST info but not used in GSI
!         nst_gsi = 2          : Input NST info, used in CRTM simulation but no
!         Tr analysis
!         nst_gsi = 3          : Input NST info, used in both CRTM simulation
!         and Tr analysis
!     (3) The surface file (sfcgcy) read in has been updated with global_cycle
!     (4) Generally, here, the interpolation of the discontinuous field is
!         handled. It is required in more applications, for example, the
!         cloud and ice concentration dependent interpolation.
!
!  USES:
!
    use kinds, only: r_kind,r_single,i_kind

    use mpimod, only: mpi_rtype,mpi_itype
    use mpimod, only: mpi_comm_world
    use mpimod, only: ierror
    use mpimod, only: mype

    use gridmod, only: nlat,nlon,lat1,lon1,lat2,lon2,nlat_sfc,nlon_sfc
    use gridmod, only: iglobal,ijn,displs_g,itotsub
    use gridmod, only: rlats,rlons,rlats_sfc,rlons_sfc
    use general_commvars_mod, only: ltosi,ltosj

    use obsmod,  only: iadate,ianldate
    use constants, only: zero,zero_single,two,tfrozen,z_w_max
    use guess_grids, only: isli2
    use gsi_nstcouplermod, only: nst_gsi,zsea1,zsea2
    use sfcio_module, only: sfcio_intkind,sfcio_head,sfcio_data,&
         sfcio_srohdc,sfcio_swohdc,sfcio_axdata

    use nstio_module, only: nstio_intkind,nstio_head,nstio_data,&
         nstio_srohdc,nstio_swohdc,nstio_axdata

    implicit none
!
!  INPUT PARAMETERS:
!
    integer(i_kind),                   intent(in) :: mype_so               ! mpi task to write output file
    real(r_kind),dimension(lat2,lon2), intent(in) :: dsfct                 ! tr analysis increment in subdomain

!
!  OUTPUT PARAMETERS:
!
!-------------------------------------------------------------------------

!   Declare local parameters
    integer(sfcio_intkind),parameter:: io_nstges = 12
    integer(sfcio_intkind),parameter:: io_sfcges = 13
    integer(sfcio_intkind),parameter:: io_sfcgcy = 14
    integer(sfcio_intkind),parameter:: io_sfctsk = 15
    integer(sfcio_intkind),parameter:: io_sfcanl = 52
    integer(sfcio_intkind),parameter:: io_nstanl = 53
    integer(sfcio_intkind),parameter:: io_dtfanl = 54

    integer(i_kind),parameter:: nprep=15

    real(r_kind),parameter :: houra = zero_single

!   Declare local variables
    character(len=6) :: fname_sfcges,fname_sfcgcy,fname_sfctsk,fname_sfcanl,fname_nstges,fname_nstanl,fname_dtfanl

    character(len=10):: canldate
    integer(i_kind):: iret,n_new_water,n_new_seaice
    integer(i_kind):: latb,lonb,nlatm2
    integer(i_kind):: i,j,ip1,jp1,ilat,ilon,mm1
    real(r_single) :: r_zsea1,r_zsea2

    real(r_kind),    dimension(lat1,lon1):: dsfct_sub
    integer(i_kind), dimension(lat1,lon1):: isli_sub

    real(r_kind),    dimension(max(iglobal,itotsub)):: dsfct_all
    integer(i_kind), dimension(max(iglobal,itotsub)):: isli_all

    real(r_kind),    dimension(nlat,nlon):: dsfct_glb,dsfct_tmp
    integer(i_kind), dimension(nlat,nlon):: isli_glb,isli_tmp

    real(r_kind),    dimension(nlat_sfc,nlon_sfc)  :: dsfct_gsi
    integer(i_kind), dimension(nlat_sfc,nlon_sfc)  :: isli_gsi

    real(r_kind),    dimension(nlon_sfc,nlat_sfc-2):: dsfct_anl
    real(r_single),  dimension(nlon_sfc,nlat_sfc-2):: dtzm
    real(r_single),  dimension(nlat_sfc,nlon_sfc)  :: work

    type(sfcio_head):: head_sfcges,head_sfcgcy,head_sfcanl
    type(sfcio_data):: data_sfcges,data_sfcgcy,data_sfcanl

    type(nstio_head):: head_nst
    type(nstio_data):: data_nst
!*****************************************************************************

!   Initialize local variables
    mm1=mype+1
    nlatm2=nlat-2
!   get file names
    write(canldate,'(I10)') ianldate

    fname_sfcges = 'sfcf06'
    fname_sfcgcy = 'sfcgcy'
    fname_sfctsk = 'sfctsk'
    fname_sfcanl = 'sfcanl'
    fname_nstges = 'nstf06'
    fname_nstanl = 'nstanl'
    fname_dtfanl = 'dtfanl'
!
!   Extract the analysis increment and surface mask in subdomain without the
!   buffer
!
    do j=1,lon1
       jp1 = j+1
       do i=1,lat1
          ip1 = i+1
          dsfct_sub(i,j) = dsfct(ip1,jp1)
          isli_sub (i,j) = isli2(ip1,jp1)
       end do
    end do
!
!   Gather analysis increment and surface mask info from subdomains
!
    call mpi_gatherv(dsfct_sub,ijn(mm1),mpi_rtype,&
         dsfct_all,ijn,displs_g,mpi_rtype,mype_so ,&
         mpi_comm_world,ierror)

    call mpi_gatherv(isli_sub,ijn(mm1),mpi_itype,&
         isli_all,ijn,displs_g,mpi_itype,mype_so ,&
         mpi_comm_world,ierror)
!
!   Only MPI task mype_so, writes the surface & nst file.
!
    if (mype==mype_so) then

      write(*,'(a,5(1x,a6))') 'write_gfs_sfc_nst:',fname_sfcges,fname_nstges,fname_sfctsk,fname_sfcanl,fname_nstanl
!
!     get Tf analysis increment and surface mask at analysis (lower resolution)
!     grids
!
      do i=1,iglobal
         ilon=ltosj(i)
         ilat=ltosi(i)
         dsfct_glb(ilat,ilon) = dsfct_all(i)
         isli_glb (ilat,ilon) = isli_all (i)
      end do
!
!      write dsfct_anl to a data file for later use (at eupd step at present)
!
       open(io_dtfanl,file=fname_dtfanl,form='unformatted')
       write(io_dtfanl) nlon,nlat
       write(io_dtfanl) dsfct_glb
       write(io_dtfanl) isli_glb

!      Read nst guess file for static/full resolution analysis
       call nstio_srohdc(io_nstges,fname_nstges,head_nst,data_nst,iret)
       if (iret /= 0) then
          write(6,*)'WRITE_NST_SFC:  ***ERROR*** problem reading',fname_nstges,', iret=',iret
          call nstio_axdata(data_nst,iret)
          call stop2(80)
       endif

!      Read surface guess file for static/full resolution analysis
       call sfcio_srohdc(io_sfcges,fname_sfcges,head_sfcges,data_sfcges,iret)
       if (iret /= 0) then
          write(6,*)'WRITE_NST_SFC:  ***ERROR*** problem reading',fname_sfcges,', iret=',iret
          call sfcio_axdata(data_sfcges,iret)
          call stop2(80)
       endif

!      Read surface global_cycle file for static/full resolution analysis
       call sfcio_srohdc(io_sfcgcy,fname_sfcgcy,head_sfcgcy,data_sfcgcy,iret)
       if (iret /= 0) then
          write(6,*)'WRITE_NST_SFC:  ***ERROR*** problem reading',fname_sfcgcy,', iret=',iret
          call sfcio_axdata(data_sfcgcy,iret)
          call stop2(80)
       endif

       if ( head_nst%latb /= head_sfcges%latb .or. head_nst%lonb /= head_sfcges%lonb ) then
          write(6,*) 'Inconsistent dimension for sfc & nst files.head_nst%latb,head_nst%lonb : ',head_nst%latb,head_nst%lonb, &
                     'head_sfcges%latb,head_sfcges%lonb : ',head_sfcges%latb,head_sfcges%lonb
          call stop2(80)
       endif

       if ( nlat_sfc /= head_sfcges%latb+2 .or. nlon_sfc /= head_nst%lonb ) then
          write(6,*) 'Inconsistent dimension for used and read.nlat_sfc,nlon_sfc : ',nlat_sfc,nlon_sfc, &
                     'head_sfcges%latb+2,head_sfcges%lonb :',head_sfcges%latb+2,head_sfcges%lonb
       endif

!
!      assign sfcanl as sfcgcy
!
       head_sfcanl = head_sfcgcy
       data_sfcanl = data_sfcgcy

       latb=head_sfcanl%latb
       lonb=head_sfcanl%lonb

       if ( (latb /= nlatm2) .or. (lonb /= nlon) ) then
          write(6,*)'WRITE_NST_SFC:  different grid dimensions analysis vs sfc.interpolating sfc temperature  ',&
               ', nlon,nlat-2=',nlon,nlatm2,' -vs- sfc file lonb,latb=',lonb,latb
          write(6,*) ' WRITE_NST_SFC, nlon_sfc,nlat_sfc : ',  nlon_sfc,nlat_sfc
!
!         Get the expanded values for a surface type (0 = water now) and the new mask
!
          call int2_msk_glb_prep(dsfct_glb,isli_glb,dsfct_tmp,isli_tmp,nlat,nlon,0,nprep)
!
!         Get updated/analysis surface mask info from sfcgcy file
!
          call tran_gfssfc(data_sfcanl%slmsk,work,lonb,latb)
          do j=1,lonb
             do i=1,latb+2
                isli_gsi(i,j) = nint(work(i,j))
             end do
          end do
!
!         Interpolate dsfct_tmp(nlat,nlon) to dsfct_gsi(nlat_sfc,nlon_sfc) with
!         surface mask accounted
!
          call int22_msk_glb(dsfct_tmp,isli_tmp,rlats,rlons,nlat,nlon, &
                             dsfct_gsi,isli_gsi,rlats_sfc,rlons_sfc,nlat_sfc,nlon_sfc,0)
!
!         transform the dsfct_gsi(latb+2,lonb) to dsfct_anl(lonb,latb) for sfc
!         file format
!
          do j = 1, latb
             do i = 1, lonb
                dsfct_anl(i,j) = dsfct_gsi(latb+2-j,i)
             end do
          end do

       else
!
!         transform the dsfct_glb(nlat,nlon) to dsfct_anl(lonb,latb) for sfc file
!         format when nlat == latb-2 & nlon = lonb
!
          do j=1,latb
             do i=1,lonb
                dsfct_anl(i,j)=dsfct_glb(latb+1-j,i)
             end do
          end do
       endif                 ! if ( (latb /= nlatm2) .or. (lonb /= nlon) ) then

!
!      update slmsk in nstanl with slmsk from sfcgcy
!
       data_nst%slmsk = data_sfcanl%slmsk
!
!      update tref (in nst file) & tsea (in the surface file) when Tr analysis is on
!      reset NSSTM variables for new open water grids
!
       if ( nst_gsi > 2 ) then
!
!         For the new open water (sea ice just melted) grids, (1) set dsfct_anl = zero; (2) reset the NSSTM variables
!
!         Notes: data_sfcges%slmsk is the mask of the background
!                data_sfcanl%slmsk is the mask of the analysis since global_cycle has been applied
!
          where ( data_sfcanl%slmsk(:,:) == zero .and. data_sfcges%slmsk(:,:) == two )

            dsfct_anl(:,:)        = zero

            data_nst%xt(:,:)      = zero
            data_nst%xs(:,:)      = zero
            data_nst%xu(:,:)      = zero
            data_nst%xv(:,:)      = zero
            data_nst%xz(:,:)      = z_w_max
            data_nst%zm(:,:)      = zero
            data_nst%xtts(:,:)    = zero
            data_nst%xzts(:,:)    = zero
            data_nst%dt_cool(:,:) = zero
            data_nst%z_c(:,:)     = zero
            data_nst%c_0(:,:)     = zero
            data_nst%c_d(:,:)     = zero
            data_nst%w_0(:,:)     = zero
            data_nst%w_d(:,:)     = zero
            data_nst%d_conv(:,:)  = zero
            data_nst%ifd(:,:)     = zero
            data_nst%tref(:,:)    = tfrozen
            data_nst%qrain(:,:)   = zero
          end where
!
!         update analysis variable: Tref (foundation temperature) for nst file
!
          where ( data_sfcanl%slmsk(:,:) == zero )
             data_nst%tref(:,:) = max(data_nst%tref(:,:) + dsfct_anl(:,:),tfrozen)
          elsewhere
             data_nst%tref(:,:) = data_sfcgcy%tsea(:,:)
          end where
!
!         update SST: tsea for sfc file
!
          r_zsea1 = 0.001_r_single*real(zsea1)
          r_zsea2 = 0.001_r_single*real(zsea2)
          call dtzm_2d(data_nst%xt,data_nst%xz,data_nst%dt_cool,data_nst%z_c, &
                       data_sfcanl%slmsk,r_zsea1,r_zsea2,lonb,latb,dtzm)

          where ( data_sfcanl%slmsk(:,:) == zero )
             data_sfcanl%tsea(:,:) = max(data_nst%tref(:,:) + dtzm(:,:), tfrozen)
          end where
!         Write updated information to surface analysis file
          call sfcio_swohdc(io_sfcanl,fname_sfcanl,head_sfcanl,data_sfcanl,iret)

          write(6,100) fname_sfcanl,lonb,latb,houra,iadate(1:4),iret
100       format(' WRITE_NST_SFC:  sfc analysis written  for ',&
             a6,2i6,1x,f4.1,4(i4,1x),' with iret=',i2)

       else          ! when (nst_gsi <= 2)

          do j=1,latb
             do i=1,lonb
                data_nst%tref(i,j) = data_sfcanl%tsea(i,j)  ! keep tref as tsea before analysis
             end do
          end do
!
!         For the new open water (sea ice just melted) grids, reset the NSSTM variables
!
          where ( data_sfcanl%slmsk(:,:) == zero .and. data_sfcges%slmsk(:,:) == two ) 
             data_nst%xt(:,:)      = zero
             data_nst%xs(:,:)      = zero
             data_nst%xu(:,:)      = zero
             data_nst%xv(:,:)      = zero
             data_nst%xz(:,:)      = z_w_max
             data_nst%zm(:,:)      = zero
             data_nst%xtts(:,:)    = zero
             data_nst%xzts(:,:)    = zero
             data_nst%dt_cool(:,:) = zero
             data_nst%z_c(:,:)     = zero
             data_nst%c_0(:,:)     = zero
             data_nst%c_d(:,:)     = zero
             data_nst%w_0(:,:)     = zero
             data_nst%w_d(:,:)     = zero
             data_nst%d_conv(:,:)  = zero
             data_nst%ifd(:,:)     = zero
             data_nst%tref(:,:)    = tfrozen
             data_nst%qrain(:,:)   = zero
          end where
!
!         update tsea when NO Tf analysis
!
          do j=1,latb
             do i=1,lonb
                data_sfcanl%tsea(i,j) = max(data_sfcges%tsea(i,j) + dsfct_anl(i,j),tfrozen) ! update tsea
             end do
          end do

!         Write updated information to surface analysis file
          call sfcio_swohdc(io_sfctsk,fname_sfctsk,head_sfcanl,data_sfcanl,iret)
          write(6,101) fname_sfctsk,lonb,latb,houra,iadate(1:4),iret
101       format(' WRITE_NST_SFC:  sfc analysis written  for ',&
             a6,2i6,1x,f4.1,4(i4,1x),' with iret=',i2)

       endif                   ! if ( nst_gsi > 2 ) then

!
!      write info on the new open water and new sea ice grids
!
       n_new_water = 0
       n_new_seaice = 0
       do j = 1, latb
          do i = 1, lonb
             if ( data_sfcanl%slmsk(i,j) == zero .and. data_sfcges%slmsk(i,j) == two ) then
                n_new_water = n_new_water + 1
             endif
             if ( data_sfcanl%slmsk(i,j) == two .and. data_sfcges%slmsk(i,j) == zero ) then
                n_new_seaice = n_new_seaice + 1
             endif
          end do
       end do
       write(*,'(a,I3,1x,I8,1x,I8)') 'write_gfs_sfc_nst,nst_gsi,n_new_water,n_new_seaice:',nst_gsi,n_new_water,n_new_seaice

!      Update guess date/time to analysis date/time for nst file
       head_nst%fhour = head_sfcanl%fhour            ! forecast hour
       head_nst%idate(1)=head_sfcanl%idate(1)        ! hour
       head_nst%idate(2)=head_sfcanl%idate(2)        ! month
       head_nst%idate(3)=head_sfcanl%idate(3)        ! day
       head_nst%idate(4)=head_sfcanl%idate(4)        ! year

!      Write updated information to nst analysis file
       call nstio_swohdc(io_nstanl,fname_nstanl,head_nst,data_nst,iret)

       write(6,102) fname_nstanl,lonb,latb,houra,iadate(1:4),iret
102    format(' WRITE_NST_SFC:  nst analysis written  for ',&
            a6,2i6,1x,f4.1,4(i4,1x),' with iret=',i2)

!      Deallocate local work arrays
!      call sfcio_axdata(data_sfcges,iret)
!      call sfcio_axdata(data_sfcgcy,iret)
       call sfcio_axdata(data_sfcanl,iret)
       call nstio_axdata(data_nst,iret)

    endif                               ! if (mype == mype_so ) then

!   End of routine
  end subroutine write_gfs_sfc_nst

  subroutine write_tf_inc_nc(mype_so,xvar2)
!
! abstract: get a global dtf and msk used in GSI by gatjering sub-domanin ones and write them in netCDF 
!
!  REMARKS:
!
!   language: f90
!   machines: ibm RS/6000 SP; SGI Origin 2000; Compaq HP
!
!  AUTHOR:
!
!   2017-09-12  xu li -  initial version; org: np22
!   REVISION HISTORY:
!
!EOP

!  DESCRIPTION:
!  input:
!        mype_so : mpi task no
!        xvar2   : variable in subdomain
!
!  USES:
!
    use netcdf
    use netcdf_mod, only: nc_check

    use kinds, only: r_kind,i_kind

    use mpimod, only: mpi_rtype,mpi_itype
    use mpimod, only: mpi_comm_world
    use mpimod, only: ierror
    use mpimod, only: mype

    use gridmod, only: nlat,nlon,lat1,lon1,lat2,lon2
    use gridmod, only: iglobal,ijn,displs_g,itotsub
    use gridmod, only: rlats,rlons
    use guess_grids, only: isli2
    use general_commvars_mod, only: ltosi,ltosj

    use constants, only: rad2deg

    implicit none
!
!  INPUT PARAMETERS:
!
    integer(i_kind),                   intent(in) :: mype_so          
    real(r_kind),dimension(lat2,lon2), intent(in) :: xvar2            
!
!  OUTPUT PARAMETERS:
!
!-------------------------------------------------------------------------

!   Declare local variables
    integer(i_kind):: i,j,ip1,jp1,ilat,ilon,mm1
    real(r_kind),    dimension(lat1,lon1):: dtf_sub
    integer(i_kind), dimension(lat1,lon1):: msk_sub

    real(r_kind),    dimension(max(iglobal,itotsub)):: dtf_all
    integer(i_kind), dimension(max(iglobal,itotsub)):: msk_all

    real(r_kind),    dimension(nlon,nlat):: dtf
    integer(i_kind), dimension(nlon,nlat):: msk

    integer(i_kind) :: ncid
    character (len = *), parameter :: lat_name = "latitude"
    character (len = *), parameter :: lon_name = "longitude"
    integer(i_kind) :: lat_dimid, lon_dimid
!   The start and count arrays will tell the netCDF library where to write our data.
    integer(i_kind), dimension(2) :: start,count,dimids
!   These program variables hold the latitudes and longitudes.
    integer(i_kind) :: lon_varid, lat_varid
!   We will create two netCDF variables, one each for temperature and slmsk
    character (len = *), parameter :: dtf_name="dtf"
    character (len = *), parameter :: msk_name="msk"
    integer(i_kind) :: dtf_varid,msk_varid
!   define a "units" attribute for each variable.
    character (len = *), parameter :: units = "units"
    character (len = *), parameter :: dtf_units = "kelvin"
    character (len = *), parameter :: msk_units = "none"
    character (len = *), parameter :: lat_units = "degrees_north"
    character (len = *), parameter :: lon_units = "degrees_east"
!*****************************************************************************
!   Initialize local variables
    mm1 = mype + 1
!
!   Extract the xvar and surface mask in subdomain without the buffer
!
    do j=1,lon1
       jp1 = j+1
       do i=1,lat1
          ip1 = i+1
          dtf_sub(i,j) = xvar2(ip1,jp1)
          msk_sub(i,j) = isli2(ip1,jp1)
       end do
    end do
!
!   Gather analysis increment and surface mask info from subdomains
!
    call mpi_gatherv(dtf_sub,ijn(mm1),mpi_rtype,&
         dtf_all,ijn,displs_g,mpi_rtype,mype_so ,&
         mpi_comm_world,ierror)

    call mpi_gatherv(msk_sub,ijn(mm1),mpi_itype,&
         msk_all,ijn,displs_g,mpi_itype,mype_so ,&
         mpi_comm_world,ierror)
!
!   Only MPI task mype_so, writes the surface & nst file.
!
    if ( mype == mype_so ) then
       do i=1,iglobal
          ilon=ltosj(i)
          ilat=ltosi(i)
          dtf(ilon,ilat) = dtf_all(i)
          msk(ilon,ilat) = msk_all(i)
       end do
!      Create the netCDF file.
       call nc_check( nf90_create('dtfanl', cmode=ior(nf90_clobber,nf90_64bit_offset), ncid=ncid),'create_nc','dtfanl' )
!      Define the dimensions.
       call nc_check( nf90_def_dim(ncid, lat_name, nlat, lat_dimid),'lat_name','dtfanl' )
       call nc_check( nf90_def_dim(ncid, lon_name, nlon, lon_dimid),'lon_name','dtfanl' )

!      Define the coordinate variables.
       call nc_check( nf90_def_var(ncid, lat_name, nf90_real, lat_dimid, lat_varid),'lat_dim','dtfanl' )
       call nc_check( nf90_def_var(ncid, lon_name, nf90_real, lon_dimid, lon_varid),'lon_dim','dtfanl' )
! Assign units attributes to coordinate variables
       call nc_check( nf90_put_att(ncid, lat_varid, units, lat_units),'lat_unit','dtfanl' )
       call nc_check( nf90_put_att(ncid, lon_varid, units, lon_units),'lon_unit','dtfanl' )
!      The dimids array is used to pass the dimids of the dimensions of the netCDF variables.
       dimids = (/ lon_dimid, lat_dimid /)
!      Define the netCDF variables for the Tf and slmsk data.
       call nc_check( nf90_def_var(ncid, dtf_name, nf90_double, dimids, dtf_varid),'dtf_type','dtfanl' )
       call nc_check( nf90_def_var(ncid, msk_name, nf90_byte,   dimids, msk_varid),'msk_type','dtfanl' )
!      Assign units attributes to the netCDF variables.
       call nc_check( nf90_put_att(ncid, dtf_varid, units, dtf_units),'lat_name','dtfanl' )
       call nc_check( nf90_put_att(ncid, msk_varid, units, msk_units),'lat_name','dtfanl' )
!      End define mode.
       call nc_check( nf90_enddef(ncid),'Att_End','dtfanl' )
!      Write the coordinate variable data. 
       call nc_check( nf90_put_var(ncid, lat_varid, rlats*rad2deg),'write_lats','dtfanl' )
       call nc_check( nf90_put_var(ncid, lon_varid, rlons*rad2deg),'write_lons','dtfanl' )
!      These settings tell netcdf to write one timestep of data.
       count = (/ nlon, nlat /)
       start = (/ 1, 1 /)
!      Write the data. 
       call nc_check( nf90_put_var(ncid, dtf_varid, dtf, start, count),'write_dtf','dtfanl' )
       call nc_check( nf90_put_var(ncid, msk_varid, msk, start, count),'write_msk','dtfanl' )

       call nc_check( nf90_close(ncid),'close','dtfanl' )
       print *,"*** SUCCESS writing dtf & msk file ", 'dtfanl', "!"
    endif                               ! if (mype == mype_so ) then
  end subroutine write_tf_inc_nc

  subroutine write_ens_sfc_nst(mype_so,dsfct)
!
! abstract: write sfc and nst analysis files (nst_gsi dependent) for
!           ensemble (lower resolution)
!
!
!  REMARKS:
!
!   language: f90
!   machines: ibm RS/6000 SP; SGI Origin 2000; Compaq HP
!
!  AUTHOR:
!
!   2014-01-28  xu li -  initial version; org: np22
!
!EOP
!
!  DESCRIPTION:
!  1. Background
!     In the current operational GFS, although the atmospheric variables are
!     analyzed/updated 6-hourly,
!     the surface variables are handled differently. The SST and sea ice are
!     updated 24-hourly with the independent analysis. The land variables are
!     not analyzed yet and the 6-hour forecast is simply used as their analysis.
!     Practically, the analysis file is generated by updating SST and sea
!     ice in the 6-hour forecasting surface file/files with globale_cycle evry
!     24 hours.
!
!     With th the development of NSST, where the NSST model to provide the
!     diurnal warming (dTw) and sub-layer cooling
!     (dTc) at atmospheric model time step, and Tr analysis to provide the
!     foundation temperature (Tf) analysis every 6 hour, SST = Tf + dTw - dTc
!     This enable to update SST (Tr as well) 6-hourly as the atmospheric
!     variables and
!     the new files (nstf06, nstges and nstanl) needs to be processed
!
!     With the implementation of Hybrid EnKF since May, 2012, there are two
!     types of surface files
!
!     (1) Static analysis with higher resolution
!         handled in write_gfs_sfc_nst
!
!     (2) Ensemble analysis with lower resolution (with member 001 as example:
!     001 to 080)
!         sfcges_mem001 : 6-hour SFC forecast for each member (copied from
!         bfg_yyyymmddhh_fhr06_mem001)
!         sfcgcy_mem001 : from sfcges_mem001 but SST and sea ice updated with
!         global_cycle 24-hourly
!         sfcanl_mem001 : from sfcgcy_mem001, SST is updated with NSST for open
!         water grids in GSI
!
!         nstf06_mem001  : 6-hour NSST forecast for each member
!         nstanl_mem001  : NSST analysis (only tref updated at present)
!
!
!  2. This routine generates the sfc & nst analysis files for ensemble members
!  (001 to 080)
!      (1) reading sfcgcy_mem001 and nstf06_mem001 (1-80)
!      (2) writing/updating the SST (tsea) or Tr (tref) in the above read in
!      files to get sfcanl_mem001, nstanl_mem001,
!          they will be renamed to be sfcanl_yyyymmddhh_mem001 and
!          nstanl_yyyymmddhh_mm001
!  3. Surface mask dependent Interpolation
!     See write_gfs_sfc_nst
!
!  4. Notes
!     (1) Tr (foundation temperature), instead of skin temperature, is the
!     analysis variable.
!     (2) The generation of sfcanl and sfcanl_yyyymmddhh_mm??? is nst_gsi
!     dependent.
!         nst_gsi = 0 (default): No NST info at all;
!         nst_gsi = 1          : Input NST info but not used in GSI
!         nst_gsi = 2          : Input NST info, used in CRTM simulation but no
!         Tr analysis
!         nst_gsi = 3          : Input NST info, used in both CRTM simulation
!         and Tr analysis
!     (3) The mask info is regarded as available for different resolutions
!
!  USES:
!
    use kinds, only: r_kind,r_single,i_kind

    use mpimod, only: mpi_rtype,mpi_itype
    use mpimod, only: mpi_comm_world
    use mpimod, only: ierror
    use mpimod, only: mype

    use gridmod, only: nlat,nlon,lat1,lon1,lat2,lon2
    use gridmod, only: iglobal,ijn,displs_g,itotsub
    use gridmod, only: rlats,rlons
    use general_commvars_mod, only: ltosi,ltosj
    use hybrid_ensemble_parameters, only: n_ens
    use obsmod,  only: ianldate
    use constants, only: zero_single,zero,half,two,pi,tfrozen,z_w_max
    use guess_grids, only: isli2
    use gsi_nstcouplermod, only: nst_gsi
    use sfcio_module, only: sfcio_intkind,sfcio_head,sfcio_data,&
         sfcio_srohdc,sfcio_swohdc,sfcio_axdata

    use nstio_module, only: nstio_intkind,nstio_head,nstio_data,&
         nstio_srohdc,nstio_swohdc,nstio_axdata

    implicit none
!
!  INPUT PARAMETERS:
!
    integer(i_kind),                   intent(in) :: mype_so               ! mpi task to write output file
    real(r_kind),dimension(lat2,lon2), intent(in) :: dsfct                 ! tr analysis increment in subdomain

!
!  OUTPUT PARAMETERS:
!
!-------------------------------------------------------------------------

!   Declare local parameters

    integer(sfcio_intkind),parameter:: io_nstges = 12
    integer(sfcio_intkind),parameter:: io_sfcges = 13
    integer(sfcio_intkind),parameter:: io_sfcgcy = 14
    integer(sfcio_intkind),parameter:: io_nstanl = 52
    integer(sfcio_intkind),parameter:: io_sfcanl = 53

    integer(i_kind),parameter:: nprep=15

    real(r_kind),parameter :: houra = zero_single

!   Declare local variables
    character(len=13) :: fname_nstges,fname_sfcges,fname_sfcgcy,fname_nstanl,fname_sfcanl

    character(len=10) :: canldate
    character(len=3 ) :: cmember
    integer(i_kind):: iret,n_new_water,n_new_seaice
    integer(i_kind):: latb,lonb,nlatm2,nlat_ens_sfc,nlon_ens_sfc
    integer(i_kind):: i,j,k,ip1,jp1,ilat,ilon,mm1,jmax

    real(r_kind),    dimension(lat1,lon1):: dsfct_sub
    integer(i_kind), dimension(lat1,lon1):: isli_sub

    real(r_kind),    dimension(max(iglobal,itotsub)):: dsfct_all
    integer(i_kind), dimension(max(iglobal,itotsub)):: isli_all

    real(r_kind),    dimension(nlat,nlon):: dsfct_glb
    integer(i_kind), dimension(nlat,nlon):: isli_glb

    integer(i_kind), allocatable, dimension(:,:):: isli_tmp,isli_gsi

    real(r_kind),   allocatable, dimension(:)   :: wlatx,slatx,rlats_ens_sfc,rlons_ens_sfc
    real(r_kind),   allocatable, dimension(:,:) :: dsfct_gsi,dsfct_anl
    real(r_kind),   allocatable, dimension(:,:) :: dsfct_tmp
    real(r_single), allocatable, dimension(:,:) :: work

    real(r_kind) :: dlon

    type(sfcio_head):: head_sfcges,head_sfcgcy,head_sfcanl
    type(sfcio_data):: data_sfcges,data_sfcgcy,data_sfcanl

    type(nstio_head):: head_nst
    type(nstio_data):: data_nst
!*****************************************************************************
!   Initialize local variables
    mm1 = mype + 1
    nlatm2  = nlat - 2
!   get analysis date (yyyymmddhh) in character
    write(canldate,'(I10)') ianldate
!
!   Extract the analysis increment and surface mask in subdomain without the
!   buffer
!
    do j=1,lon1
       jp1 = j+1
       do i=1,lat1
          ip1 = i+1
          dsfct_sub(i,j) = dsfct(ip1,jp1)
          isli_sub (i,j) = isli2(ip1,jp1)
       end do
    end do

!
!   Gather analysis increment and surface mask info from subdomains
!
    call mpi_gatherv(dsfct_sub,ijn(mm1),mpi_rtype,&
         dsfct_all,ijn,displs_g,mpi_rtype,mype_so ,&
         mpi_comm_world,ierror)

    call mpi_gatherv(isli_sub,ijn(mm1),mpi_itype,&
         isli_all,ijn,displs_g,mpi_itype,mype_so ,&
         mpi_comm_world,ierror)
!
!   Only MPI task mype_so, processes and writes the surface & nst file.
!
    if (mype==mype_so) then
!
!     get Tr analysis increment and surface mask at analysis grids
!
       do i=1,iglobal
          ilon=ltosj(i)
          ilat=ltosi(i)
          dsfct_glb(ilat,ilon) = dsfct_all(i)
          isli_glb (ilat,ilon) = isli_all (i)
       end do

!
!      update sfc and nst file for each ensemble member
!
       do k = 1, n_ens

          write(cmember,'(i3.3)') k               ! make the a character string
 
          fname_nstges = 'nstf06_mem'//cmember
          fname_sfcges = 'sfcf06_mem'//cmember
          fname_sfcgcy = 'sfcgcy_mem'//cmember
          fname_nstanl = 'nstanl_mem'//cmember
          fname_sfcanl = 'sfcanl_mem'//cmember

!         Read nst guess file of the ensemble member
          call nstio_srohdc(io_nstges,fname_nstges,head_nst,data_nst,iret)
          if (iret /= 0) then
             write(6,*)'WRITE_ENS_NST_SFC:  ***ERROR*** problem reading',fname_nstges,', iret=',iret
             call nstio_axdata(data_nst,iret)
             call stop2(80)
          endif

!         Read surface guess file of the ensemble member
          call sfcio_srohdc(io_sfcges,fname_sfcges,head_sfcges,data_sfcges,iret)
          if (iret /= 0) then
             write(6,*)'WRITE_ENS_NST_SFC:  ***ERROR*** problem reading ',fname_sfcges,', iret=',iret
             call sfcio_axdata(data_sfcges,iret)
             call stop2(80)
          endif

!         Read surface gcycle file (global_cycle applied already to sfcges) of the
!         ensemble member
          call sfcio_srohdc(io_sfcgcy,fname_sfcgcy,head_sfcgcy,data_sfcgcy,iret)
          if (iret /= 0) then
             write(6,*)'WRITE_ENS_NST_SFC:  ***ERROR*** problem reading ',fname_sfcgcy,', iret=',iret
             call sfcio_axdata(data_sfcgcy,iret)
             call stop2(80)
          endif

          if ( head_nst%latb /= head_sfcges%latb .or. head_nst%lonb /= head_sfcges%lonb ) then
             write(6,*) 'Inconsistent dimension for sfc & nst files. head_nst%latb,head_nst%lonb : ',head_nst%latb,head_nst%lonb, &
                        'head_sfcges%latb,head_sfcges%lonb : ',head_sfcges%latb,head_sfcges%lonb
             call stop2(80)
          endif

!
!         assign sfc analysis as sfcges with gcycle applied
!
          head_sfcanl = head_sfcgcy
          data_sfcanl = data_sfcgcy

          latb=head_sfcanl%latb
          lonb=head_sfcanl%lonb
 
          nlat_ens_sfc = latb + 2
          nlon_ens_sfc = lonb
!
!         Get dsfct_anl when k = 1, the first ensemble member only. It is
!         identical for each member if the mask (isli_tmp) is identical, at present
!
          if ( k == 1 ) then

             allocate(dsfct_gsi(nlat_ens_sfc,nlon_ens_sfc),work(nlat_ens_sfc,nlon_ens_sfc), &
                      isli_gsi(nlat_ens_sfc,nlon_ens_sfc),dsfct_anl(nlon_ens_sfc,nlat_ens_sfc-2))


             allocate(dsfct_tmp(nlat,nlon),isli_tmp(nlat,nlon))

             if ( (latb /= nlatm2) .or. (lonb /= nlon) ) then
                write(6,*)'WRITE_ENS_NST_SFC:  different grid dimensions analysis vs sfc. interpolating sfc temperature  ',&
                    ', nlon,nlat_-2=',nlon,nlatm2,' -vs- sfc file lonb,latb=',lonb,latb
!
!               get lats and lons for ensemble grids
!
                jmax=nlat_ens_sfc-2
                allocate(slatx(jmax),wlatx(jmax))
                allocate(rlats_ens_sfc(nlat_ens_sfc),rlons_ens_sfc(nlon_ens_sfc))
                call splat(4,jmax,slatx,wlatx)
                dlon=two*pi/real(nlon_ens_sfc,r_kind)
                do i=1,nlon_ens_sfc
                   rlons_ens_sfc(i)=real(i-1,r_kind)*dlon
                end do
                do i=1,(nlat_ens_sfc-1)/2
                   rlats_ens_sfc(i+1)=-asin(slatx(i))
                   rlats_ens_sfc(nlat_ens_sfc-i)=asin(slatx(i))
                end do
                rlats_ens_sfc(1)=-half*pi
                rlats_ens_sfc(nlat_ens_sfc)=half*pi
                deallocate(slatx,wlatx)
!
!               Get the expanded values for a surface type (0 = water now) and the
!               new mask
!
                call int2_msk_glb_prep(dsfct_glb,isli_glb,dsfct_tmp,isli_tmp,nlat,nlon,0,nprep)
!
!               Get updated/analysis surface mask info from sfcgcy_mem001 (001-080)
!               file
!
                call tran_gfssfc(data_sfcanl%slmsk,work,lonb,latb)
                do j=1,nlon_ens_sfc
                   do i=1,nlat_ens_sfc
                      isli_gsi(i,j) = nint(work(i,j))
                   end do
                end do
!
!               Interpolate dsfct_glb(nlat,nlon) to
!               dsfct_tmp(nlat_ens_sfc,nlon_ens_sfc) with surface mask accounted
!
                call int22_msk_glb(dsfct_tmp,isli_tmp,rlats,rlons,nlat,nlon, &
                                   dsfct_gsi,isli_gsi,rlats_ens_sfc,rlons_ens_sfc,nlat_ens_sfc,nlon_ens_sfc,0)
!
!               transform the dsfct_gsi(latb+2,lonb) to dsfct_anl(lonb,latb) for sfc
!               file format
!
                do j = 1, latb
                   do i = 1, lonb
                      dsfct_anl(i,j) = dsfct_gsi(latb+2-j,i)
                   end do
                end do

             else
!
!               transform the dsfct_glb(nlat,nlon) to dsfct_anl(lonb,latb) for sfc file
!               format when nlat == latb-2 & nlon = lonb
!
                do j=1,latb
                   do i=1,lonb
                      dsfct_anl(i,j)=dsfct_glb(latb+1-j,i)
                   end do
                end do
             endif                 ! if ( (latb /= nlatm2) .or. (lonb /= nlon) ) then
          endif                   ! if ( k == 1 ) then

!
!         update tref (in nst file) & tsea (in the surface file) when Tr analysis is on
!
          if ( nst_gsi > 2 ) then
!
!            For the new open water (sea ice just melted) grids, reset the NSSTM variables
!
!            set tref = tfrozen = 271.2_r_kind
!            note: data_sfcges%slmsk is the mask of the guess
!                  data_sfcanl%slmsk is the mask of the analysis
!
             where ( data_sfcanl%slmsk(:,:) == zero .and. data_sfcges%slmsk(:,:) == two ) 
                data_nst%xt(:,:)      = zero
                data_nst%xs(:,:)      = zero
                data_nst%xu(:,:)      = zero
                data_nst%xv(:,:)      = zero
                data_nst%xz(:,:)      = z_w_max
                data_nst%zm(:,:)      = zero
                data_nst%xtts(:,:)    = zero
                data_nst%xzts(:,:)    = zero
                data_nst%dt_cool(:,:) = zero
                data_nst%z_c(:,:)     = zero
                data_nst%c_0(:,:)     = zero
                data_nst%c_d(:,:)     = zero
                data_nst%w_0(:,:)     = zero
                data_nst%w_d(:,:)     = zero
                data_nst%d_conv(:,:)  = zero
                data_nst%ifd(:,:)     = zero
                data_nst%tref(:,:)    = tfrozen
                data_nst%qrain(:,:)   = zero
             end where
!
!            update analysis variable: Tref (foundation temperature) for nst file
!
             where ( data_sfcanl%slmsk(:,:) == zero ) 
                data_nst%tref(:,:) = max(data_nst%tref(:,:) + dsfct_anl(:,:),tfrozen)
             else where
                data_nst%tref(:,:) = data_sfcanl%tsea(:,:)
             end where
!
!            update SST: tsea for sfc file
!
             where ( data_sfcanl%slmsk(:,:) == zero )
                data_sfcanl%tsea(:,:) = max(data_nst%tref(:,:)  &
                                      + two*data_nst%xt(:,:)/data_nst%xz(:,:) & 
                                      - data_nst%dt_cool(:,:), tfrozen)
             end where



          else          ! when (nst_gsi <= 2)

             do j=1,latb
                do i=1,lonb
                   data_nst%tref(i,j) = data_sfcanl%tsea(i,j)     ! keep tref as tsea before analysis
!
!                  For the new open water (sea ice just melted) grids, reset the NSSTM variables
!
                   if ( data_sfcanl%slmsk(i,j) == zero .and. data_nst%slmsk(i,j) == two ) then

                      data_nst%xt(i,j)      = zero
                      data_nst%xs(i,j)      = zero
                      data_nst%xu(i,j)      = zero
                      data_nst%xv(i,j)      = zero
                      data_nst%xz(i,j)      = z_w_max
                      data_nst%zm(i,j)      = zero
                      data_nst%xtts(i,j)    = zero
                      data_nst%xzts(i,j)    = zero
                      data_nst%dt_cool(i,j) = zero
                      data_nst%z_c(i,j)     = zero
                      data_nst%c_0(i,j)     = zero
                      data_nst%c_d(i,j)     = zero
                      data_nst%w_0(i,j)     = zero
                      data_nst%w_d(i,j)     = zero
                      data_nst%d_conv(i,j)  = zero
                      data_nst%ifd(i,j)     = zero
                      data_nst%tref(i,j)    = tfrozen
                      data_nst%qrain(i,j)   = zero
                   endif

                   data_sfcanl%tsea(i,j) = max(data_sfcanl%tsea(i,j) + dsfct_anl(i,j),271.0_r_kind)  ! update tsea
                end do
             end do
          endif                   ! if ( nst_gsi > 2 ) then

          n_new_water = 0
          n_new_seaice = 0
          do j = 1, latb
             do i = 1, lonb
                if ( data_sfcanl%slmsk(i,j) == zero .and. data_sfcges%slmsk(i,j) == two ) then
                   n_new_water = n_new_water + 1
                endif
                if ( data_sfcanl%slmsk(i,j) == two .and. data_sfcges%slmsk(i,j) == zero ) then
                   n_new_seaice = n_new_seaice + 1
                endif
             end do
          end do
          write(*,'(a,I3,1x,I8,1x,I8)')'write_gfs_sfc_nst,nst_gsi,n_new_water,n_new_seaice:',nst_gsi,n_new_water,n_new_seaice

!         Update guess date/time to analysis date/time for nst file
          head_nst%fhour    = head_sfcanl%fhour                  ! forecast hour
          head_nst%idate(1) = head_sfcanl%idate(1)               ! hour
          head_nst%idate(2) = head_sfcanl%idate(2)               ! month
          head_nst%idate(3) = head_sfcanl%idate(3)               ! day
          head_nst%idate(4) = head_sfcanl%idate(4)               ! year

!         Write updated information to nst analysis file
          call nstio_swohdc(io_nstanl,fname_nstanl,head_nst,data_nst,iret)
 
          write(6,101)fname_nstanl,lonb,latb,head_nst%fhour,head_nst%idate(1:4),iret

101       format(' WRITE_ENS_NST_SFC:  nst analysis written  for ',&
                 a10,1x,2i6,1x,f4.1,4(i4,1x),' with iret=',i2)

          close (io_nstges)
          close (io_sfcges)
          close (io_sfcgcy)
          close (io_nstanl)
          close (io_sfcanl)
       enddo           ! do i = 1, n_ens

!      Deallocate local work arrays
       call sfcio_axdata(data_sfcges,iret)
       call nstio_axdata(data_nst,iret)

    endif                               ! if (mype == mype_so ) then

!   End of routine
  end subroutine write_ens_sfc_nst

  subroutine write_ens_dsfct(mype_so,dsfct)
!
! abstract: write out dsfct (nst_gsi dependent) for ensemble (lower resolution)
!
!  REMARKS:
!
!   language: f90
!   machines: ibm RS/6000 SP; SGI Origin 2000; Compaq HP
!
!  AUTHOR:
!
!   2014-04-28  xu li -  initial version; org: np22
!
!EOP
!
!  DESCRIPTION:
!  1. Background
!     In the current operational GFS, although the atmospheric variables are
!     analyzed/updated 6-hourly,
!     the surface variables are handled differently. The SST and sea ice are
!     updated 24-hourly with the independent analysis. The land variables are
!     not analyzed yet and the 6-hour forecast is simply used as their analysis.
!     Practically, the analysis file is generated by updating SST and sea
!     ice in the 6-hour forecasting surface file/files with globale_cycle evry
!     24 hours.
!
!     With th the development of NSST, where the NSST model to provide the
!     diurnal warming (dTw) and sub-layer cooling
!     (dTc) at atmospheric model time step, and Tr analysis to provide the
!     foundation temperature (Tf) analysis every 6 hour, SST = Tf + dTw - dTc
!     This enable to update SST (Tr as well) 6-hourly as the atmospheric
!     variables and
!     the new files (nstf06, nstges and nstanl) needs to be processed
!
!     With the implementation of Hybrid EnKF since May, 2012, there are two
!     types of surface files
!
!     (1) Static analysis with higher resolution
!         handled in write_gfs_sfc_nst
!
!     (2) Ensemble analysis with lower resolution (with member 001 as example:001 to 080)
!         sfcges_mem001 : 6-hour SFC forecast for each member (copied from
!         bfg_yyyymmddhh_fhr06_mem001)
!         sfcgcy_mem001 : from sfcges_mem001 but SST and sea ice updated with
!         global_cycle 24-hourly
!         sfcanl_mem001 : from sfcgcy_mem001, SST is updated with NSST for open
!         water grids in GSI
!
!         nstf06_mem001  : 6-hour NSST forecast for each member
!         nstanl_mem001  : NSST analysis (only tref updated at present)
!
!
!  2. This routine generates the surface temperature analysis increment for ensemble members (the same for all 80 members)
!      (1) read sfcgcy_mem001 and nstf06_mem001 to get the masks (GES and
!      ANL)for ensemble members
!      (2) get dsfct at ensemble grids (interpolation if needed)
!      (3) write dsfct in a file for later nst (tref) and sfc (tsea) update at
!      recenter step
!
!  3. Surface mask dependent Interpolation
!     See write_gfs_sfc_nst
!
!  4. Notes
!     (1) Tr (foundation temperature), instead of skin temperature, is the
!     analysis variable., but not analyzed yet with the current scheme
!     (2) The mask info is regarded as available for different resolutions
!
!  USES:
!
    use kinds, only: r_kind,r_single,i_kind

    use mpimod, only: mpi_rtype,mpi_itype
    use mpimod, only: mpi_comm_world
    use mpimod, only: ierror
    use mpimod, only: mype

    use gridmod, only: nlat,nlon,lat1,lon1,lat2,lon2
    use gridmod, only: iglobal,ijn,displs_g,itotsub
    use general_commvars_mod, only: ltosi,ltosj
    use gridmod, only: rlats,rlons
    use obsmod,  only: ianldate
    use constants, only: zero_single,zero,half,two,pi,tfrozen
    use guess_grids, only: isli2
    use sfcio_module, only: sfcio_intkind,sfcio_head,sfcio_data,&
         sfcio_srohdc,sfcio_swohdc,sfcio_axdata

    use nstio_module, only: nstio_intkind,nstio_head,nstio_data,&
         nstio_srohdc,nstio_swohdc,nstio_axdata

    implicit none
!
!  INPUT PARAMETERS:
!
    integer(i_kind),                   intent(in) :: mype_so               ! mpi task to write output file
    real(r_kind),dimension(lat2,lon2), intent(in) :: dsfct                 ! tr analysis increment in subdomain

!
!  OUTPUT PARAMETERS:
!
!-------------------------------------------------------------------------

!   Declare local parameters

    integer(sfcio_intkind),parameter:: io_nstges = 12
    integer(sfcio_intkind),parameter:: io_sfcges = 13
    integer(sfcio_intkind),parameter:: io_sfcgcy = 14
    integer(sfcio_intkind),parameter:: io_dtsinc = 54

    integer(i_kind),parameter:: nprep=15

    real(r_kind),parameter :: houra = zero_single

!   Declare local variables
    character(len=14):: fname_sfcges,fname_sfcgcy,fname_nstges
    character(len=10):: fname_dtsinc
    character(len=10) :: canldate
    integer(i_kind):: iret
    integer(i_kind):: latb,lonb,nlatm2,nlat_ens_sfc,nlon_ens_sfc
    integer(i_kind):: i,j,ip1,jp1,ilat,ilon,mm1,jmax

    real(r_kind),    dimension(lat1,lon1):: dsfct_sub
    integer(i_kind), dimension(lat1,lon1):: isli_sub

    real(r_kind),    dimension(max(iglobal,itotsub)):: dsfct_all
    integer(i_kind), dimension(max(iglobal,itotsub)):: isli_all

    real(r_kind),    dimension(nlat,nlon):: dsfct_glb
    integer(i_kind), dimension(nlat,nlon):: isli_glb

    real(r_kind),   allocatable, dimension(:)   :: wlatx,slatx,rlats_ens_sfc,rlons_ens_sfc
    real(r_kind),   allocatable, dimension(:,:) :: dsfct_gsi,dsfct_anl
    real(r_kind),   allocatable, dimension(:,:) :: dsfct_tmp
    real(r_single), allocatable, dimension(:,:) :: work

    integer(i_kind), allocatable, dimension(:,:):: isli_tmp,isli_gsi

    real(r_kind) :: dlon
    type(sfcio_head):: head_sfcges,head_sfcgcy
    type(sfcio_data):: data_sfcges,data_sfcgcy

    type(nstio_head):: head_nst
    type(nstio_data):: data_nst
!*****************************************************************************
!   Initialize local variables
    mm1 = mype + 1
    nlatm2  = nlat - 2
!   get analysis date (yyyymmddhh) in character
    write(canldate,'(I10)') ianldate
!
!   Extract the analysis increment and surface mask in subdomain without the
!   buffer
!
    do j=1,lon1
       jp1 = j+1
       do i=1,lat1
          ip1 = i+1
          dsfct_sub(i,j) = dsfct(ip1,jp1)
          isli_sub (i,j) = isli2(ip1,jp1)
       end do
    end do

!
!   Gather analysis increment and surface mask info from subdomains
!
    call mpi_gatherv(dsfct_sub,ijn(mm1),mpi_rtype,&
         dsfct_all,ijn,displs_g,mpi_rtype,mype_so ,&
         mpi_comm_world,ierror)

    call mpi_gatherv(isli_sub,ijn(mm1),mpi_itype,&
         isli_all,ijn,displs_g,mpi_itype,mype_so ,&
         mpi_comm_world,ierror)
!
!   Only MPI task mype_so, processes and writes the surface & nst file.
!
    if (mype==mype_so) then
!
!      get Tr analysis increment and surface mask at analysis grids
!
       do i=1,iglobal
          ilon=ltosj(i)
          ilat=ltosi(i)
          dsfct_glb(ilat,ilon) = dsfct_all(i)
          isli_glb (ilat,ilon) = isli_all (i)
       end do

!
!      get surface temperature analysis increment at ensemble resolution
!      (identical to each other member)
!
       fname_nstges = 'nstf06_ensmean'
       fname_sfcges = 'sfcf06_ensmean'
       fname_sfcgcy = 'sfcgcy_ensmean'
       fname_dtsinc = 'dtsinc_ens'

!      Read nst guess file of the ensemble member 1
       call nstio_srohdc(io_nstges,fname_nstges,head_nst,data_nst,iret)
       if (iret /= 0) then
          write(6,*)'WRITE_ENS_DSFCT:  ***ERROR*** problem reading',fname_nstges,', iret=',iret
 
          call nstio_axdata(data_nst,iret)
          call stop2(80)
       endif

!      Read surface guess file (the ensemble mean)
       call sfcio_srohdc(io_sfcges,fname_sfcges,head_sfcges,data_sfcges,iret)
       if (iret /= 0) then
          write(6,*)'WRITE_ENS_DSFCT:  ***ERROR*** problem reading',fname_sfcges,', iret=',iret
 
          call sfcio_axdata(data_sfcges,iret)
          call stop2(80)
       endif

!      Read surface gcycle file (the ensemble mean)
       call sfcio_srohdc(io_sfcgcy,fname_sfcgcy,head_sfcgcy,data_sfcgcy,iret)
       if (iret /= 0) then
          write(6,*)'WRITE_ENS_DSFCT:  ***ERROR*** problem reading',fname_sfcgcy,', iret=',iret
          call sfcio_axdata(data_sfcgcy,iret)
          call stop2(80)
       endif

       if ( head_nst%latb /= head_sfcgcy%latb .or. head_nst%lonb /=head_sfcgcy%lonb ) then
          write(6,*) 'Inconsistent dimension for sfc & nst files.head_nst%latb,head_nst%lonb : ',head_nst%latb,head_nst%lonb, &
                     'head_sfcgcy%latb,head_sfcgcy%lonb : ',head_sfcgcy%latb,head_sfcgcy%lonb
 
          call stop2(80)
       endif

       latb=head_sfcgcy%latb
       lonb=head_sfcgcy%lonb

       nlat_ens_sfc = latb + 2
       nlon_ens_sfc = lonb

       allocate(dsfct_gsi(nlat_ens_sfc,nlon_ens_sfc),work(nlat_ens_sfc,nlon_ens_sfc), &
                 isli_gsi(nlat_ens_sfc,nlon_ens_sfc),dsfct_anl(nlon_ens_sfc,nlat_ens_sfc-2))

       allocate(dsfct_tmp(nlat,nlon),isli_tmp(nlat,nlon))

       if ( (latb /= nlatm2) .or. (lonb /= nlon) ) then

          write(6,*)'WRITE_ENS_DSFCT:  different grid dimensions analysis vs sfc. interpolating sfc temperature  ',&
               ', nlon,nlat_-2=',nlon,nlatm2,' -vs- sfc file lonb,latb=',lonb,latb
!
!         get lats and lons for ensemble grids
!
          jmax=nlat_ens_sfc-2
          allocate(slatx(jmax),wlatx(jmax))
          allocate(rlats_ens_sfc(nlat_ens_sfc),rlons_ens_sfc(nlon_ens_sfc))
          call splat(4,jmax,slatx,wlatx)
          dlon=two*pi/real(nlon_ens_sfc,r_kind)
          do i=1,nlon_ens_sfc
             rlons_ens_sfc(i)=real(i-1,r_kind)*dlon
          end do
          do i=1,(nlat_ens_sfc-1)/2
             rlats_ens_sfc(i+1)=-asin(slatx(i))
             rlats_ens_sfc(nlat_ens_sfc-i)=asin(slatx(i))
          end do
          rlats_ens_sfc(1)=-half*pi
          rlats_ens_sfc(nlat_ens_sfc)=half*pi
          deallocate(slatx,wlatx)
!
!         Get the expanded values for a surface type (0 = water now) and the new
!         mask
!
          call int2_msk_glb_prep(dsfct_glb,isli_glb,dsfct_tmp,isli_tmp,nlat,nlon,0,nprep)
!
!         Get updated/analysis surface mask info from sfcgcy_ensmean  file
!
          call tran_gfssfc(data_sfcgcy%slmsk,work,lonb,latb)
          do j=1,nlon_ens_sfc
             do i=1,nlat_ens_sfc
                isli_gsi(i,j) = nint(work(i,j))
             end do
          end do
!
!         Interpolate dsfct_glb(nlat,nlon) to dsfct_tmp(nlat_ens_sfc,nlon_ens_sfc)
!         with surface mask accounted
!
          call int22_msk_glb(dsfct_tmp,isli_tmp,rlats,rlons,nlat,nlon, &
                             dsfct_gsi,isli_gsi,rlats_ens_sfc,rlons_ens_sfc,nlat_ens_sfc,nlon_ens_sfc,0)
!
!         transform the dsfct_gsi(latb+2,lonb) to dsfct_anl(lonb,latb) for sfc
!         file format
!
          do j = 1, latb
             do i = 1, lonb
                dsfct_anl(i,j) = dsfct_gsi(latb+2-j,i)
             end do
          end do

       else       ! when the GSI analysis grid is identical to ensemble one and
                  ! no surface mask change from ges to anl

!
!         transform the dsfct_glb(nlat,nlon) to dsfct_anl(lonb,latb) for sfc file
!         format when nlat == latb-2 & nlon = lonb
!
          write(6,*)'WRITE_ENS_DSFCT:  the same grid dimensions static grids: ',&
               ', nlon,nlat_-2=',nlon,nlatm2,' -vs- ens lonb,latb=',lonb,latb
 
          do j=1,latb
             do i=1,lonb
                dsfct_anl(i,j)=dsfct_glb(latb+1-j,i)
!
!               set the analysis increment to be zero for new melted water grid
!
                if ( data_sfcgcy%slmsk(i,j) == zero .and. data_sfcges%slmsk(i,j) == two ) then
                   dsfct_anl(i,j) = zero
                endif
             end do
          end do

       endif                 ! if ( (latb /= nlatm2) .or. (lonb /= nlon) ) then

!
!      write dsfct_anl to a data file for later use (at eupd step at present)
!
       open(io_dtsinc,file=fname_dtsinc,form='unformatted')
       write(io_dtsinc) dsfct_anl
       write(io_dtsinc) data_sfcgcy%slmsk

       write(6,*)'WRITE_ENS_DSFCT:  dtsinc has been written : ',fname_dtsinc

       close (io_nstges)
       close (io_sfcges)
       close (io_sfcgcy)
       close (io_dtsinc)

!      Deallocate local work arrays
       call sfcio_axdata(data_sfcges,iret)
       call nstio_axdata(data_nst,iret)

    endif                               ! if (mype == mype_so ) then
!   End of routine
  end subroutine write_ens_dsfct

!-------------------------------------------------------------------------------
  subroutine sigio_cnvtdv8(im,ix,km,idvc,idvm,ntrac,iret,t,q,cpi,cnflg)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    sigio_cnvtdv8
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-05-28  safford -- add subprogram doc block
!
!   input argument list:
!     im,ix,km,idvc,idvm,ntrac,cnflg
!     q, cpi
!     t
!
!   output argument list:
!     iret
!     t
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

    use kinds, only: i_kind,r_kind
    use constants, only: zero,one,fv
    implicit none
    integer(i_kind),intent(in   ) :: im,ix,km,idvc,idvm,ntrac,cnflg
    integer(i_kind),intent(  out) :: iret
    real(r_kind)   ,intent(in   ) :: q(ix,km,ntrac), cpi(0:ntrac)
    real(r_kind)   ,intent(inout) :: t(ix,km)
    integer(i_kind) :: thermodyn_id, n
    real(r_kind) :: xcp(ix,km), sumq(ix,km)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=0
    thermodyn_id = mod(IDVM/10,10)
!
    if (thermodyn_id == 3 .and. idvc == 3) then
       xcp(1:im,:)  = zero
       sumq(1:im,:) = zero
       do n=1,NTRAC
          if( cpi(n) /= zero) then
             xcp(1:im,:)  = xcp(1:im,:)  + q(1:im,:,n) * cpi(n)
             sumq(1:im,:) = sumq(1:im,:) + q(1:im,:,n)
          endif
       enddo
       xcp(1:im,:)  = (one-sumq(1:im,:))*cpi(0) + xcp(1:im,:)   ! Mean Cp
!
    else
       xcp(1:im,:) = one + fv*Q(1:im,:,1)        ! Virt factor
    endif
    if (cnflg > 0) then
       t(1:im,:) = t(1:im,:) / xcp(1:im,:)
    else
       t(1:im,:) = t(1:im,:) * xcp(1:im,:)
    endif
!
    return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine sigio_cnvtdv8

  subroutine glbave(fld,ave)
  use kinds, only: r_kind,i_kind,r_quad
  use constants, only: zero_quad,two_quad
  use mpimod, only: mype
  use gridmod, only: lat2,lon2,nlon,istart,wgtlats
  use mpl_allreducemod, only: mpl_allreduce
  implicit none
  real(r_kind),intent(in)    :: fld(:,:,:) 
  real(r_kind),intent(inout) :: ave(:) 
  integer(i_kind) i,j,k,mp1,ii
  real(r_quad),allocatable,dimension(:):: xave
  allocate(xave(size(ave,1)))
  mp1=mype+1
  do k=1,size(ave,1)
     xave(k)=zero_quad
     do j=2,lon2-1
        do i=2,lat2-1
           ii=istart(mp1)+i-2
           xave(k)=xave(k)+fld(i,j,k)*wgtlats(ii)
        enddo
     enddo
  enddo
  xave=xave/(two_quad*real(nlon,r_quad))
  call mpl_allreduce(size(ave,1),qpvals=xave)
  ave=xave
  deallocate(xave)
  end subroutine glbave

end module ncepgfs_io

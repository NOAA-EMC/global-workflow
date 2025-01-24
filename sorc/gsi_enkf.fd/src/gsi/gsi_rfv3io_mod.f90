module gsi_rfv3io_mod
!$$$   module documentation block
!             .      .    .                                       .
! module:     gsi_rfv3io_mod
!   prgmmr:
!
! abstract: IO routines for regional FV3
!
! program history log:
!   2017-03-08  parrish - create new module gsi_rfv3io_mod, starting from
!                           gsi_nemsio_mod as a pattern.
!   2017-10-10  wu      - setup A grid and interpolation coeff in generate_anl_grid
!   2018-02-22  wu      - add subroutines for read/write fv3_ncdf
!   2019        ting    - modifications for use for ensemble IO and cold start files 
!   2019-03-13  CAPS(C. Tong) - Port direct radar DA capabilities.
!   2021-11-01  lei     - modify for fv3-lam parallel IO
!   2022-01-07  Hu      - add code to read/write subdomain restart files.
!                         This function is needed when fv3 model sets
!                         io_layout(2)>1
!   2022-02-15 Lu @ Wang - add time label it for FGAT. POC: xuguang.wang@ou.edu
!   2022-03-01 X.Lu @ X.Wang - add gsi_rfv3io_get_ens_grid_specs for dual ens HAFS. POC: xuguang.wang@ou.edu
!   2022-03-15  Hu      - add code to read/write 2m T and Q for they will be
!                         used as background for surface observation operator
!   2022-04-15  Wang    - add IO for regional FV3-CMAQ (RRFS-CMAQ) model 
!   2022-08-10  Wang    - add IO for regional FV3-SMOKE (RRFS-SMOKE) model 
!   2023-07-30  Zhao    - add IO for the analysis of the significant wave height
!                         (SWH, aka howv in GSI) in fv3-lam based DA (eg., RRFS-3DRTMA)
!   2024-01-24  X.Zhang - bug fix for reading the soil temp and mois from the wram start file 
!
! subroutines included:
!   sub gsi_rfv3io_get_grid_specs
!   sub gsi_rfv3io_get_ens_grid_specs
!   sub read_fv3_files 
!   sub read_fv3_netcdf_guess
!   sub gsi_fv3ncdf2d_read
!   sub gsi_fv3ncdf_read
!   sub gsi_fv3ncdf_readuv
!   sub wrfv3_netcdf
!   sub gsi_fv3ncdf_writeuv
!   sub gsi_fv3ncdf_writeps
!   sub gsi_fv3ncdf_write
!   sub gsi_fv3ncdf_write_v1
!   sub gsi_fv3ncdf2d_write
!   sub check
!
! variable definitions:
!
! attributes:
!   langauge: f90
!    machine:
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use gridmod, only: nlon_regional,nlat_regional,nlon_regionalens,nlat_regionalens
  use constants, only:max_varname_length,max_filename_length
  use gsi_bundlemod, only : gsi_bundle
  use general_sub2grid_mod, only: sub2grid_info
  use gridmod,  only: fv3_io_layout_y
  use guess_grids, only: nfldsig,ntguessig,ifilesig
  use rapidrefresh_cldsurf_mod, only: i_use_2mq4b,i_use_2mt4b
  use chemmod, only: naero_cmaq_fv3,aeronames_cmaq_fv3,imodes_cmaq_fv3,laeroana_fv3cmaq
  use chemmod, only: naero_smoke_fv3,aeronames_smoke_fv3,laeroana_fv3smoke  
  use rapidrefresh_cldsurf_mod, only: i_howv_3dda, i_gust_3dda

  implicit none
  public type_fv3regfilenameg
  public bg_fv3regfilenameg
  public fv3sar_bg_opt

!    directory names (hardwired for now)
  type type_fv3regfilenameg
      character(len=:),allocatable :: grid_spec !='fv3_grid_spec'
      character(len=:),allocatable :: ak_bk     !='fv3_akbk'
      character(len=:),allocatable :: dynvars   !='fv3_dynvars'
      character(len=:),allocatable :: tracers   !='fv3_tracer'
      character(len=:),allocatable :: phyvars   !='fv3_phyvars'
      character(len=:),allocatable :: sfcdata   !='fv3_sfcdata'
      character(len=:),allocatable :: couplerres!='coupler.res'
      contains
      procedure , pass(this):: init=>fv3regfilename_init
  end type type_fv3regfilenameg

  integer(i_kind):: fv3sar_bg_opt=0
  
  type(type_fv3regfilenameg),allocatable:: bg_fv3regfilenameg(:)
  integer(i_kind) nx,ny,nz
  integer(i_kind) nxens,nyens
  integer(i_kind),dimension(:),allocatable :: ny_layout_len,ny_layout_b,ny_layout_e
  integer(i_kind),dimension(:),allocatable :: ny_layout_lenens,ny_layout_bens,ny_layout_eens
  real(r_kind),allocatable:: grid_lon(:,:),grid_lont(:,:),grid_lat(:,:),grid_latt(:,:)
  real(r_kind),allocatable:: ak(:),bk(:)
  integer(i_kind),allocatable:: ijns2d(:),displss2d(:),ijns(:),displss(:)
  integer(i_kind),allocatable:: ijnz(:),displsz_g(:)

  real(r_kind),dimension(:,:  ),allocatable:: ges_ps_bg 
  real(r_kind),dimension(:,:  ),allocatable:: ges_ps_inc 
  real(r_kind),dimension(:,:,:  ),allocatable:: ges_delp_bg 
  type(sub2grid_info) :: grd_fv3lam_dynvar_ionouv 
  type(sub2grid_info) :: grd_fv3lam_tracer_ionouv 
  type(sub2grid_info) :: grd_fv3lam_tracerchem_ionouv
  type(sub2grid_info) :: grd_fv3lam_tracersmoke_ionouv 
  type(sub2grid_info) :: grd_fv3lam_phyvar_ionouv
  type(sub2grid_info) :: grd_fv3lam_uv 
  integer(i_kind) ,parameter:: ndynvarslist=13, ntracerslist=8, nphyvarslist=2

  character(len=max_varname_length), dimension(ndynvarslist), parameter :: &
    vardynvars = [character(len=max_varname_length) :: &
      "u","v","u_w","u_s","v_w","v_s","t","tv","tsen","w","delp","ps","delzinc"]
  character(len=max_varname_length), dimension(ntracerslist+naero_cmaq_fv3+7+naero_smoke_fv3), parameter :: & 
    vartracers =  [character(len=max_varname_length) :: &
      'q','oz','ql','qi','qr','qs','qg','qnr',aeronames_cmaq_fv3,'pm25at','pm25ac','pm25co','pm2_5','amassi','amassj','amassk',aeronames_smoke_fv3]
  character(len=max_varname_length), dimension(nphyvarslist), parameter :: &
    varphyvars = [character(len=max_varname_length) :: 'dbz','fed']
  character(len=max_varname_length), dimension(16+naero_cmaq_fv3+7+naero_smoke_fv3+1), parameter :: &
    varfv3name = [character(len=max_varname_length) :: &
      'u','v','W','T','delp','sphum','o3mr','liq_wat','ice_wat','rainwat','snowwat','graupel','rain_nc','ref_f3d','flash_extent_density','ps','DZ', & 
      aeronames_cmaq_fv3,'pm25at','pm25ac','pm25co','pm2_5','amassi','amassj','amassk',aeronames_smoke_fv3], &
      vgsiname = [character(len=max_varname_length) :: &
        'u','v','w','tsen','delp','q','oz','ql','qi','qr','qs','qg','qnr','dbz','fed','ps','delzinc', &
        aeronames_cmaq_fv3,'pm25at','pm25ac','pm25co','pm2_5','amassi','amassj','amassk',aeronames_smoke_fv3]

  integer(i_kind) ,parameter:: nnonnegtracer=7
  character(len=max_varname_length), dimension(nnonnegtracer), parameter :: &
    vnames_nonnegativetracers = [character(len=max_varname_length) :: &
      "sphum","o3mr","liq_wat","ice_wat","rainwat","snowwat","graupel"]
  character(len=max_varname_length),dimension(:),allocatable:: name_metvars2d
  character(len=max_varname_length),dimension(:),allocatable:: name_metvars3d
  character(len=max_varname_length),dimension(:),allocatable:: name_chemvars3d

! set default to private
  private
! set subroutines to public
  public :: gsi_rfv3io_get_grid_specs
  public :: gsi_rfv3io_get_ens_grid_specs
  public :: gsi_fv3ncdf_read
  public :: gsi_fv3ncdf_read_v1
  public :: gsi_fv3ncdf_readuv
  public :: gsi_fv3ncdf_readuv_v1
  public :: gsi_fv3ncdf_read_ens_parallel_over_ens
  public :: gsi_fv3ncdf_readuv_ens_parallel_over_ens
  public :: read_fv3_files 
  public :: read_fv3_netcdf_guess
  public :: wrfv3_netcdf
  public :: gsi_fv3ncdf2d_read_v1

  public :: mype_u,mype_v,mype_t,mype_q,mype_p,mype_oz,mype_ql
  public :: mype_qi,mype_qr,mype_qs,mype_qg,mype_qnr,mype_w
  public :: k_slmsk,k_tsea,k_vfrac,k_vtype,k_stype,k_zorl,k_smc,k_stc
  public :: k_snwdph,k_f10m,mype_2d,n2d,k_orog,k_psfc,k_t2m,k_q2m,k_howv,k_gust
  public :: ijns,ijns2d,displss,displss2d,ijnz,displsz_g
  public :: fv3lam_io_dynmetvars3d_nouv,fv3lam_io_tracermetvars3d_nouv
  public :: fv3lam_io_tracerchemvars3d_nouv,fv3lam_io_tracersmokevars3d_nouv
  public :: fv3lam_io_phymetvars3d_nouv
  public :: fv3lam_io_dynmetvars2d_nouv,fv3lam_io_tracermetvars2d_nouv

  integer(i_kind) mype_u,mype_v,mype_t,mype_q,mype_p,mype_delz,mype_oz,mype_ql
  integer(i_kind) mype_qi,mype_qr,mype_qs,mype_qg,mype_qnr,mype_w

  integer(i_kind) k_slmsk,k_tsea,k_vfrac,k_vtype,k_stype,k_zorl,k_smc,k_stc
  integer(i_kind) k_snwdph,k_f10m,mype_2d,n2d,k_orog,k_psfc,k_t2m,k_q2m,k_howv,k_gust
  parameter(                   &  
    k_f10m =1,                  &   !fact10
    k_stype=2,                  &   !soil_type
    k_vfrac=3,                  &   !veg_frac
    k_vtype=4,                 &   !veg_type
    k_zorl =5,                &   !sfc_rough
    k_tsea =6,                  &   !sfct ?
    k_snwdph=7,                &   !sno ?
    k_stc  =8,                  &   !soil_temp
    k_smc  =9,                  &   !soil_moi
    k_slmsk=10,                 &   !isli
    k_t2m =11,                 & ! 2 m T
    k_q2m  =12,                 & ! 2 m Q
    k_orog =13,                 & !terrain
    k_howv =14,                 &   ! significant wave height (aka howv in GSI)
    k_gust =15,                 &   ! wind gust (aka gust in GSI)
    n2d=15                   )
  logical :: grid_reverse_flag
  character(len=max_varname_length),allocatable,dimension(:) :: fv3lam_io_dynmetvars3d_nouv 
                                    ! copy of cvars3d excluding uv 3-d fields   
  character(len=max_varname_length),allocatable,dimension(:) :: fv3lam_io_tracermetvars3d_nouv 
                                    ! copy of cvars3d excluding uv 3-d fields   
  character(len=max_varname_length),allocatable,dimension(:) :: fv3lam_io_phymetvars3d_nouv
  character(len=max_varname_length),allocatable,dimension(:) :: fv3lam_io_tracerchemvars3d_nouv
  character(len=max_varname_length),allocatable,dimension(:) :: fv3lam_io_tracersmokevars3d_nouv 
                                    ! copy of cvars3d excluding uv 3-d fields
  character(len=max_varname_length),allocatable,dimension(:) :: fv3lam_io_dynmetvars2d_nouv 
                                    ! copy of cvars3d excluding uv 3-d fields   
  character(len=max_varname_length),allocatable,dimension(:) :: fv3lam_io_tracermetvars2d_nouv 
                                    ! copy of cvars3d excluding uv 3-d fields   
  character(len=max_varname_length),allocatable,dimension(:) :: fv3lam_names_gsibundle_dynvar_nouv 
                                    !to define names in gsibundle 
  character(len=max_varname_length),allocatable,dimension(:) :: fv3lam_names_gsibundle_tracer_nouv 
                                    !to define names in gsibundle 
  character(len=max_varname_length),allocatable,dimension(:) :: fv3lam_names_gsibundle_phyvar_nouv
  type(gsi_bundle):: gsibundle_fv3lam_dynvar_nouv 
  type(gsi_bundle):: gsibundle_fv3lam_tracer_nouv 
  type(gsi_bundle):: gsibundle_fv3lam_phyvar_nouv
  type(gsi_bundle):: gsibundle_fv3lam_tracerchem_nouv
  type(gsi_bundle):: gsibundle_fv3lam_tracersmoke_nouv 
 
contains
  subroutine fv3regfilename_init(this,it)
  implicit None

  class(type_fv3regfilenameg),intent(inout):: this
  integer(i_kind),            intent(in   ) :: it

  character(255):: filename
  if (it == ntguessig) then
    this%grid_spec='fv3_grid_spec'
  else
    write(filename,"(A14,I2.2)") 'fv3_grid_spec_',ifilesig(it)
    this%grid_spec=trim(filename)
  endif
  if (it == ntguessig) then
    this%ak_bk='fv3_ak_bk'
  else
    write(filename,"(A10,I2.2)") 'fv3_ak_bk_',ifilesig(it)
    this%ak_bk=trim(filename)
  endif
  if (it == ntguessig) then
    this%dynvars='fv3_dynvars'
  else
    write(filename,"(A12,I2.2)") 'fv3_dynvars_',ifilesig(it)
    this%dynvars=trim(filename)
  endif
  if (it == ntguessig) then
    this%tracers='fv3_tracer'
  else
    write(filename,"(A11,I2.2)") 'fv3_tracer_',ifilesig(it)
    this%tracers=trim(filename)
  endif
  if (it == ntguessig) then
    this%phyvars='fv3_phyvars'
  else
    write(filename,"(A12,I2.2)") 'fv3_phyvars_',ifilesig(it)
    this%phyvars=trim(filename)
  endif
  if (it == ntguessig) then
    this%sfcdata='fv3_sfcdata'
  else
    write(filename,"(A12,I2.2)") 'fv3_sfcdata_',ifilesig(it)
    this%sfcdata=trim(filename)
  endif
  if (it == ntguessig) then
    this%couplerres='coupler.res'
  else
    write(filename,"(A12,I2.2)") 'coupler.res_',ifilesig(it)
    this%couplerres=trim(filename)
  endif
end subroutine fv3regfilename_init


subroutine gsi_rfv3io_get_grid_specs(ierr)
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    gsi_rfv3io_get_grid_specs
!   pgrmmr: parrish     org: np22                date: 2017-04-03
!
! abstract:  obtain grid dimensions nx,ny and grid definitions
!                grid_x,grid_xt,grid_y,grid_yt,grid_lon,grid_lont,grid_lat,grid_latt
!                nz,ak(nz),bk(nz)
!
! program history log:
!   2017-04-03  parrish - initial documentation
!   2017-10-10  wu - setup A grid and interpolation coeff with generate_anl_grid
!   2018-02-16  wu - read in time info from file coupler.res
!                    read in lat, lon at the center and corner of the grid cell
!                    from file fv3_grid_spec, and vertical grid infor from file fv3_akbk
!                    setup A grid and interpolation/rotation coeff
!   input argument list:
!    grid_spec
!    ak_bk
!    lendian_out
!
!   output argument list:
!    ierr
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use netcdf, only: nf90_open,nf90_close,nf90_get_var,nf90_noerr
  use netcdf, only: nf90_nowrite,nf90_inquire,nf90_inquire_dimension
  use netcdf, only: nf90_inquire_variable
  use mpimod, only: mype
  use mod_fv3_lola, only: generate_anl_grid
  use gridmod,  only:nsig,regional_time,regional_fhr,regional_fmin,aeta1_ll,aeta2_ll
  use gridmod,  only:nlon_regional,nlat_regional,eta1_ll,eta2_ll
  use gridmod,  only:grid_type_fv3_regional
  use kinds, only: i_kind,r_kind
  use constants, only: half,zero
  use mpimod, only: mpi_comm_world,mpi_itype,mpi_rtype

  implicit none
  integer(i_kind),intent(  out) :: ierr

  integer(i_kind) gfile_grid_spec
  character(:),allocatable    :: grid_spec
  character(:),allocatable    :: ak_bk
  character(len=:),allocatable :: coupler_res_filenam 
  integer(i_kind) i,k,ndimensions,iret,nvariables,nattributes,unlimiteddimid
  integer(i_kind) len,gfile_loc
  character(len=max_varname_length) :: name
  integer(i_kind) myear,mmonth,mday,mhour,mminute,msecond
  real(r_kind),allocatable:: abk_fv3(:)
  integer(i_kind) imiddle,jmiddle
! if fv3_io_layout_y > 1
  integer(i_kind) :: nio,nylen
  integer(i_kind),allocatable :: gfile_loc_layout(:)
  character(len=180)  :: filename_layout

    coupler_res_filenam='coupler.res'
    grid_spec='fv3_grid_spec'
    ak_bk='fv3_akbk'

!!!!! set regional_time
    open(24,file=trim(coupler_res_filenam),form='formatted')
    read(24,*)
    read(24,*)
    read(24,*)myear,mmonth,mday,mhour,mminute,msecond
    close(24)
    if(mype==0)  write(6,*)' myear,mmonth,mday,mhour,mminute,msecond=', myear,mmonth,mday,mhour,mminute,msecond
    regional_time(1)=myear
    regional_time(2)=mmonth
    regional_time(3)=mday
    regional_time(4)=mhour
    regional_time(5)=mminute
    regional_time(6)=msecond
    regional_fhr=zero          ! forecast hour set zero for now
    regional_fmin=zero          ! forecast min set zero for now

!!!!!!!!!!    grid_spec  !!!!!!!!!!!!!!!
    ierr=0
    iret=nf90_open(trim(grid_spec),nf90_nowrite,gfile_grid_spec)
    if(iret/=nf90_noerr) then
       write(6,*)' gsi_rfv3io_get_grid_specs: problem opening ',trim(grid_spec),', Status = ',iret
       ierr=1
       return
    endif

    iret=nf90_inquire(gfile_grid_spec,ndimensions,nvariables,nattributes,unlimiteddimid)
    gfile_loc=gfile_grid_spec
    do k=1,ndimensions
       iret=nf90_inquire_dimension(gfile_loc,k,name,len)
       if(trim(name)=='grid_xt') nx=len
       if(trim(name)=='grid_yt') ny=len
    enddo
    nlon_regional=nx
    nlat_regional=ny

    allocate(ny_layout_len(0:fv3_io_layout_y-1))
    allocate(ny_layout_b(0:fv3_io_layout_y-1))
    allocate(ny_layout_e(0:fv3_io_layout_y-1))
    ny_layout_len=ny
    ny_layout_b=0
    ny_layout_e=0
    if(fv3_io_layout_y > 1) then
       allocate(gfile_loc_layout(0:fv3_io_layout_y-1))
       do nio=0,fv3_io_layout_y-1
          write(filename_layout,'(a,a,I4.4)') trim(grid_spec),'.',nio
          iret=nf90_open(filename_layout,nf90_nowrite,gfile_loc_layout(nio))
          if(iret/=nf90_noerr) then
             write(6,*)' problem opening ',trim(filename_layout),', Status =',iret
             ierr=1
             return
          endif
          iret=nf90_inquire(gfile_loc_layout(nio),ndimensions,nvariables,nattributes,unlimiteddimid)
          do k=1,ndimensions
              iret=nf90_inquire_dimension(gfile_loc_layout(nio),k,name,len)
              if(trim(name)=='grid_yt') ny_layout_len(nio)=len
          enddo
          iret=nf90_close(gfile_loc_layout(nio))
       enddo
       deallocate(gfile_loc_layout)
! figure out begin and end of each subdomain restart file
       nylen=0
       do nio=0,fv3_io_layout_y-1
          ny_layout_b(nio)=nylen + 1
          nylen=nylen+ny_layout_len(nio)
          ny_layout_e(nio)=nylen
       enddo
    endif
    if(mype==0)write(6,*),'nx,ny=',nx,ny
    if(mype==0)write(6,*),'ny_layout_len=',ny_layout_len
    if(mype==0)write(6,*),'ny_layout_b=',ny_layout_b
    if(mype==0)write(6,*),'ny_layout_e=',ny_layout_e

!!!    get nx,ny,grid_lon,grid_lont,grid_lat,grid_latt,nz,ak,bk

    allocate(grid_lat(nx+1,ny+1))
    allocate(grid_lon(nx+1,ny+1))
    allocate(grid_latt(nx,ny))
    allocate(grid_lont(nx,ny))

    do k=ndimensions+1,nvariables
       iret=nf90_inquire_variable(gfile_loc,k,name,len)
       if(trim(name)=='grid_lat') then
          iret=nf90_get_var(gfile_loc,k,grid_lat)
       endif
       if(trim(name)=='grid_lon') then
          iret=nf90_get_var(gfile_loc,k,grid_lon)
       endif
       if(trim(name)=='grid_latt') then
          iret=nf90_get_var(gfile_loc,k,grid_latt)
       endif
       if(trim(name)=='grid_lont') then
          iret=nf90_get_var(gfile_loc,k,grid_lont)
       endif
    enddo
!
!  need to decide the grid orientation of the FV regional model    
!
!   grid_type_fv3_regional = 0 : decide grid orientation based on
!                                grid_lat/grid_lon
!                            1 : input is E-W N-S grid
!                            2 : input is W-E S-N grid
!
    if(grid_type_fv3_regional == 0) then
        imiddle=nx/2
        jmiddle=ny/2
        if( (grid_latt(imiddle,1) < grid_latt(imiddle,ny)) .and. &
            (grid_lont(1,jmiddle) < grid_lont(nx,jmiddle)) ) then 
            grid_type_fv3_regional = 2
        else
            grid_type_fv3_regional = 1
        endif
    endif
! check the grid type
    if( grid_type_fv3_regional == 1 ) then
       if(mype==0) write(6,*) 'FV3 regional input grid is  E-W N-S grid'
       grid_reverse_flag=.true.    ! grid is revered comparing to usual map view
    else if(grid_type_fv3_regional == 2) then
       if(mype==0) write(6,*) 'FV3 regional input grid is  W-E S-N grid'
       grid_reverse_flag=.false.   ! grid orientated just like we see on map view    
    else
       write(6,*) 'Error: FV3 regional input grid is unknown grid'
       call stop2(678)
    endif
!
    if(grid_type_fv3_regional == 2) then
       call reverse_grid_r(grid_lont,nx,ny,1)
       call reverse_grid_r(grid_latt,nx,ny,1)
       call reverse_grid_r(grid_lon,nx+1,ny+1,1)
       call reverse_grid_r(grid_lat,nx+1,ny+1,1)
    endif

    iret=nf90_close(gfile_loc)

    iret=nf90_open(ak_bk,nf90_nowrite,gfile_loc)
    if(iret/=nf90_noerr) then
       write(6,*)'gsi_rfv3io_get_grid_specs: problem opening ',trim(ak_bk),', Status = ',iret
       ierr=1
       return
    endif
    iret=nf90_inquire(gfile_loc,ndimensions,nvariables,nattributes,unlimiteddimid)
    do k=1,ndimensions
       iret=nf90_inquire_dimension(gfile_loc,k,name,len)
       if(trim(name)=='xaxis_1') nz=len
    enddo
    if(mype==0)write(6,'(" nz=",i5)') nz

    nsig=nz-1

!!!    get ak,bk

    allocate(aeta1_ll(nsig),aeta2_ll(nsig))
    allocate(eta1_ll(nsig+1),eta2_ll(nsig+1))
    allocate(ak(nz),bk(nz),abk_fv3(nz))

    do k=ndimensions+1,nvariables
       iret=nf90_inquire_variable(gfile_loc,k,name,len)
       if(trim(name)=='ak'.or.trim(name)=='AK') then
          iret=nf90_get_var(gfile_loc,k,abk_fv3)
          do i=1,nz
             ak(i)=abk_fv3(nz+1-i)
          enddo
       endif
       if(trim(name)=='bk'.or.trim(name)=='BK') then
          iret=nf90_get_var(gfile_loc,k,abk_fv3)
          do i=1,nz
             bk(i)=abk_fv3(nz+1-i)
          enddo
       endif
    enddo
    iret=nf90_close(gfile_loc)

!!!!! change unit of ak 
    do i=1,nsig+1
       eta1_ll(i)=ak(i)*0.001_r_kind
       eta2_ll(i)=bk(i)
    enddo
    do i=1,nsig
       aeta1_ll(i)=half*(ak(i)+ak(i+1))*0.001_r_kind
       aeta2_ll(i)=half*(bk(i)+bk(i+1))
    enddo
    if(mype==0)then
       do i=1,nz
          write(6,'(" ak,bk(",i3,") = ",2f17.6)') i,ak(i),bk(i)
       enddo
    endif

!!!!!!! setup A grid and interpolation/rotation coeff.
    call generate_anl_grid(nx,ny,grid_lon,grid_lont,grid_lat,grid_latt)



    deallocate (grid_lon,grid_lat,grid_lont,grid_latt)
    deallocate (ak,bk,abk_fv3)

    return
end subroutine gsi_rfv3io_get_grid_specs

subroutine gsi_rfv3io_get_ens_grid_specs(grid_spec,ierr)
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    gsi_rfv3io_get_ens_grid_specs
! modified from     gsi_rfv3io_get_grid_specs
!   pgrmmr: parrish     org: np22                date: 2017-04-03
!
! abstract:  obtain grid dimensions nx,ny and grid definitions
!                grid_x,grid_xt,grid_y,grid_yt,grid_lon,grid_lont,grid_lat,grid_latt
!                nz,ak(nz),bk(nz)
!
! program history log:
!   2017-04-03  parrish - initial documentation
!   2017-10-10  wu - setup A grid and interpolation coeff with generate_anl_grid
!   2018-02-16  wu - read in time info from file coupler.res
!                    read in lat, lon at the center and corner of the grid cell
!                    from file fv3_grid_spec, and vertical grid infor from file
!                    fv3_akbk
!                    setup A grid and interpolation/rotation coeff
!   input argument list:
!    grid_spec
!    ak_bk
!    lendian_out
!
!   output argument list:
!    ierr
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  use netcdf, only: nf90_open,nf90_close,nf90_get_var,nf90_noerr
  use netcdf, only: nf90_nowrite,nf90_mpiio,nf90_inquire,nf90_inquire_dimension
  use netcdf, only: nf90_inquire_variable
  use mpimod, only: mype
  use mod_fv3_lola, only: definecoef_regular_grids
  use gridmod,  only:nlon_regionalens,nlat_regionalens
  use gridmod,  only:grid_type_fv3_regional
  use kinds, only: i_kind,r_kind
  use constants, only: half,zero
  use mpimod, only: mpi_comm_world,mpi_itype,mpi_rtype
  implicit none
  character(:),allocatable,intent(in   ) :: grid_spec
  integer(i_kind),         intent(  out) :: ierr

  integer(i_kind) gfile_grid_spec
  integer(i_kind) k,ndimensions,iret,nvariables,nattributes,unlimiteddimid
  integer(i_kind) gfile_loc,len
  character(len=128) :: name
  integer(i_kind) :: nio,nylen
  integer(i_kind),allocatable :: gfile_loc_layout(:)
  character(len=180)  :: filename_layout
  integer(i_kind) imiddle,jmiddle,grid_ens_type_fv3_regional


    iret=nf90_open(trim(grid_spec),nf90_nowrite,gfile_grid_spec)
    if(iret/=nf90_noerr) then
       write(6,*)' problem opening1 ',trim(grid_spec),', Status = ',iret
       ierr=1
       return
    endif
    iret=nf90_inquire(gfile_grid_spec,ndimensions,nvariables,nattributes,unlimiteddimid)
    gfile_loc=gfile_grid_spec
    do k=1,ndimensions
       iret=nf90_inquire_dimension(gfile_loc,k,name,len)
       if(trim(name)=='grid_xt') nxens=len
       if(trim(name)=='grid_yt') nyens=len
    enddo
    allocate(grid_lat(nxens+1,nyens+1))
    allocate(grid_lon(nxens+1,nyens+1))
    allocate(grid_latt(nxens,nyens))
    allocate(grid_lont(nxens,nyens))
    do k=ndimensions+1,nvariables
       iret=nf90_inquire_variable(gfile_loc,k,name,len)
       if(trim(name)=='grid_lat') then
          iret=nf90_get_var(gfile_loc,k,grid_lat)
       endif
       if(trim(name)=='grid_lon') then
          iret=nf90_get_var(gfile_loc,k,grid_lon)
       endif
       if(trim(name)=='grid_latt') then
          iret=nf90_get_var(gfile_loc,k,grid_latt)
       endif
       if(trim(name)=='grid_lont') then
          iret=nf90_get_var(gfile_loc,k,grid_lont)
       endif
    enddo
    iret=nf90_close(gfile_loc)

    nlon_regionalens=nxens
    nlat_regionalens=nyens
    allocate(ny_layout_lenens(0:fv3_io_layout_y-1))
    allocate(ny_layout_bens(0:fv3_io_layout_y-1))
    allocate(ny_layout_eens(0:fv3_io_layout_y-1))
    ny_layout_lenens=nyens
    ny_layout_bens=0
    ny_layout_eens=0
    if(fv3_io_layout_y > 1) then
       allocate(gfile_loc_layout(0:fv3_io_layout_y-1))
       do nio=0,fv3_io_layout_y-1
          write(filename_layout,'(a,a,I4.4)') trim(grid_spec),'.',nio
          iret=nf90_open(filename_layout,nf90_nowrite,gfile_loc_layout(nio))
          if(iret/=nf90_noerr) then
             write(6,*)' problem opening ',trim(filename_layout),', Status =',iret
             ierr=1
             return
          endif
          iret=nf90_inquire(gfile_loc_layout(nio),ndimensions,nvariables,nattributes,unlimiteddimid)
          do k=1,ndimensions
              iret=nf90_inquire_dimension(gfile_loc_layout(nio),k,name,len)
              if(trim(name)=='grid_yt') ny_layout_lenens(nio)=len
          enddo
          iret=nf90_close(gfile_loc_layout(nio))
       enddo
       deallocate(gfile_loc_layout)
! figure out begin and end of each subdomain restart file
       nylen=0
       do nio=0,fv3_io_layout_y-1
          ny_layout_bens(nio)=nylen + 1
          nylen=nylen+ny_layout_lenens(nio)
          ny_layout_eens(nio)=nylen
       enddo
    endif
    if(mype==0)write(6,*),'nxens,nyens=',nxens,nyens
    if(mype==0)write(6,*),'ny_layout_lenens=',ny_layout_lenens
    if(mype==0)write(6,*),'ny_layout_bens=',ny_layout_bens
    if(mype==0)write(6,*),'ny_layout_eens=',ny_layout_eens

    imiddle=nxens/2
    jmiddle=nyens/2
    if( (grid_latt(imiddle,1) < grid_latt(imiddle,nyens)) .and. &
        (grid_lont(1,jmiddle) < grid_lont(nxens,jmiddle)) ) then
        grid_ens_type_fv3_regional = 2
    else
        grid_ens_type_fv3_regional = 1
    endif
! check the grid type
    if( grid_type_fv3_regional == grid_ens_type_fv3_regional ) then
       if(mype==0) write(6,*) 'Ensemble has the same orientation as the control, Cool!'
    else
       write(6,*) 'Warning! Ensemble has a different orientation as the control. This case needs further tests, Abort!'
       call stop2(678)
    endif
!
    if(grid_type_fv3_regional == 2) then
       call reverse_grid_r(grid_lont,nxens,nyens,1)
       call reverse_grid_r(grid_latt,nxens,nyens,1)
       call reverse_grid_r(grid_lon,nxens+1,nyens+1,1)
       call reverse_grid_r(grid_lat,nxens+1,nyens+1,1)
    endif

    call definecoef_regular_grids(nxens,nyens,grid_lon,grid_lont,grid_lat,grid_latt)
    deallocate (grid_lon,grid_lat,grid_lont,grid_latt)
    return
end subroutine gsi_rfv3io_get_ens_grid_specs


subroutine read_fv3_files(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_fv3_files
!   prgmmr: wu               org: np22                date: 2017-10-10
!
! abstract: read in from fv3 files and figure out available time levels 
!           of background fields starting from read_files as a pattern
!           temporary setup for one first guess file
! program history log:
!
!   input argument list:
!     mype     - pe number
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$  end documentation block

    use kinds, only: r_kind,i_kind
    use mpimod, only: mpi_comm_world,ierror,mpi_rtype,npe
    use guess_grids, only: nfldsig,nfldsfc,ntguessig,ntguessfc,&
         ifilesig,ifilesfc,hrdifsig,hrdifsfc,create_gesfinfo
    use guess_grids, only: hrdifsig_all,hrdifsfc_all
    use gsi_4dvar, only: l4dvar,l4densvar,iwinbgn,winlen,nhr_assimilation
    use constants, only: zero,one,r60inv
    use obsmod, only: iadate,time_offset
    use gridmod, only:regional_time
    implicit none

! Declare passed variables
    integer(i_kind),intent(in   ) :: mype

! Declare local parameters
    real(r_kind),parameter:: r0_001=0.001_r_kind

! Declare local variables
    logical(4) fexist
    character(6) filename
    character(19) filenames
    integer(i_kind) in_unit
    integer(i_kind) i,j,iwan,npem1
    integer(i_kind) nhr_half
    integer(i_kind) nminanl,nmings,nming2,ndiff,isecond
    integer(i_kind),dimension(4):: idateg
    integer(i_kind),dimension(5):: idate5
    real(r_kind) hourg,temp,t4dv
    real(r_kind),dimension(202,2):: time_ges
    character*1 :: datein(4),dateout(4)
    integer(i_kind) datecc

!-----------------------------------------------------------------------------
! Start read_nems_nmmb_files here.
    nhr_half=nhr_assimilation/2
    if(nhr_half*2 < nhr_assimilation) nhr_half=nhr_half+1
    npem1=npe-1

    do i=1,202
       time_ges(i,1) = 999_r_kind
       time_ges(i,2) = 999_r_kind
    end do


! Let a single task query the guess files.
    if(mype==npem1) then

!    Convert analysis time to minutes relative to fixed date
       call w3fs21(iadate,nminanl)
       write(6,*)'READ_netcdf_fv3_FILES:  analysis date,minutes ',iadate,nminanl

!    Check for consistency of times from sigma guess files.
       in_unit=15
       iwan=0
!WWWWWW setup for one first guess file for now
       do i=0,9 !place holder for FGAT
          if ( i == 6 ) then
             if(fv3_io_layout_y > 1) then
                write(filenames,"(A16)") 'fv3_dynvars.0000'
             else
                write(filenames,"(A11)") 'fv3_dynvars'
             endif
          else
             if(fv3_io_layout_y > 1) then
                write(filenames,"(A17,I2.2)") 'fv3_dynvars.0000_',i
             else
                write(filenames,"(A12,I2.2)") 'fv3_dynvars_',i
             endif
          endif
          INQUIRE(FILE=filenames, EXIST=fexist)
          if(.not.fexist) cycle

!wwww read in from the external file directly, no internal files sigfxx for FV3
          datecc=regional_time(1)/100
          datein(1)=char(regional_time(1)-datecc*100)
          datein(2)=char(regional_time(2))
          datein(3)=char(regional_time(3))
          datein(4)=char(regional_time(4))
          call w3fs15(datein,(i-6),dateout)
          idate5(1)=ichar(dateout(1))+datecc*100
          idate5(2)=ichar(dateout(2))
          idate5(3)=ichar(dateout(3))
          idate5(4)=ichar(dateout(4))
          idate5(5)=  regional_time(5)
          isecond  =  regional_time(6)
          hourg    =  zero ! forcast hour

          call w3fs21(idate5,nmings)
          nming2=nmings+60*hourg
          write(6,*)'READ_netcdf_fv3_FILES:  sigma guess file, nming2 ',hourg,idate5,nming2
          t4dv=real((nming2-iwinbgn),r_kind)*r60inv
          if (l4dvar.or.l4densvar) then
             if (t4dv<zero .OR. t4dv>winlen) then
                write(6,*)'ges file not in time range, t4dv=',t4dv
                cycle ! place holder for FGAT
             endif
          else
             ndiff=nming2-nminanl
!for test with the 3 hr files with FGAT
             if(abs(ndiff) > 60*nhr_half ) then
                write(6,*)'ges file not in time range, ndiff=',ndiff
                cycle ! place holder for FGAT
             endif
          endif
          iwan=iwan+1
          time_ges(iwan,1) =real((nming2-iwinbgn),r_kind)*r60inv
          time_ges(iwan+100,1)=i+r0_001
       end do ! i !place holder for FGAT
       time_ges(201,1)=one
       time_ges(202,1)=one
       if(iwan > 1)then
          do i=1,iwan
             do j=i+1,iwan
                if(time_ges(j,1) < time_ges(i,1))then
                   temp=time_ges(i+100,1)
                   time_ges(i+100,1)=time_ges(j+100,1)
                   time_ges(j+100,1)=temp
                   temp=time_ges(i,1)
                   time_ges(i,1)=time_ges(j,1)
                   time_ges(j,1)=temp
                end if
             end do
             if(abs(time_ges(i,1)-time_offset) < r0_001)time_ges(202,1) = i
          end do
       end if
       time_ges(201,1) = iwan+r0_001

!    Check for consistency of times from surface guess files.
       iwan=0
       do i=0,99
          write(filename,200)i
  200     format('sfcf',i2.2)
          inquire(file=filename,exist=fexist)
          if(fexist)then
             idateg(4)=iadate(1); idateg(2)=iadate(2)
             idateg(3)=iadate(3); idateg(1)=iadate(4)
             hourg = zero
             idate5(1)=idateg(4); idate5(2)=idateg(2)
             idate5(3)=idateg(3); idate5(4)=idateg(1); idate5(5)=0
             call w3fs21(idate5,nmings)
             nming2=nmings+60*hourg
             write(6,*)'READ_netcdf_fv3_FILES:  surface guess file, nming2 ',hourg,idateg,nming2
             ndiff=nming2-nminanl
             if(abs(ndiff) > 60*nhr_half ) then
                write(6,*)'ges file not in time range, ndiff=',ndiff
!               cycle ! place holder for FGAT
             endif
             iwan=iwan+1
             time_ges(iwan,2) =real((nming2-iwinbgn),r_kind)*r60inv
             time_ges(iwan+100,2)=i+r0_001
          end if
          if(iwan==1) exit
       end do
       time_ges(201,2)=one
       time_ges(202,2)=one
       if(iwan > 1)then
          do i=1,iwan
             do j=i+1,iwan
                if(time_ges(j,2) < time_ges(i,2))then
                   temp=time_ges(i+100,2)
                   time_ges(i+100,2)=time_ges(j+100,2)
                   time_ges(j+100,2)=temp
                   temp=time_ges(i,2)
                   time_ges(i,2)=time_ges(j,2)
                   time_ges(j,2)=temp
                end if
             end do
             if(abs(time_ges(i,2)-time_offset) < r0_001)time_ges(202,2) = i
          end do
       end if
       time_ges(201,2) = iwan+r0_001
    end if


! Broadcast guess file information to all tasks
    call mpi_bcast(time_ges,404,mpi_rtype,npem1,mpi_comm_world,ierror)

    nfldsig   = nint(time_ges(201,1))
!!nfldsfc   = nint(time_ges(201,2))
    nfldsfc   = nfldsig

! Allocate space for guess information files
    call create_gesfinfo

    do i=1,nfldsig
       ifilesig(i) = -100
       hrdifsig(i) = zero
    end do

    do i=1,nfldsfc
       ifilesfc(i) = -100
       hrdifsfc(i) = zero
    end do

! Load time information for sigma guess field sinfo into output arrays
    ntguessig = nint(time_ges(202,1))
    do i=1,nfldsig
       hrdifsig(i) = time_ges(i,1)
       ifilesig(i) = nint(time_ges(i+100,1))
       hrdifsig_all(i) = hrdifsig(i)
    end do
    if(mype == 0) write(6,*)'READ_netcdf_fv3_FILES:  sigma fcst files used in analysis  :  ',&
         (ifilesig(i),i=1,nfldsig),(hrdifsig(i),i=1,nfldsig),ntguessig


! Load time information for surface guess field info into output arrays
    ntguessfc = nint(time_ges(202,2))
    do i=1,nfldsfc
       hrdifsfc(i) = time_ges(i,2)
       ifilesfc(i) = nint(time_ges(i+100,2))
       hrdifsfc_all(i) = hrdifsfc(i)
    end do

! Below is a temporary fix. The nems_nmmb regional mode does not have a
! surface
! file.  Instead the surface fields are passed through the atmospheric guess
! file.  Without a separate surface file the code above sets ntguessig and 
! nfldsig to zero.  This causes problems later in the code when arrays for
! the surface fields are allocated --> one of the array dimensions is nfldsfc
! and it will be zero.  This portion of the code should be rewritten, but
! until
! it is, the fix below gets around the above mentioned problem.

    ntguessfc = ntguessig
!!nfldsfc   = nfldsig
    do i=1,nfldsfc
       hrdifsfc(i) = hrdifsig(i)
       ifilesfc(i) = ifilesig(i)
       hrdifsfc_all(i) = hrdifsfc(i)
    end do
    if(mype == 0) write(6,*)'READ_nems_nmb_FILES:  surface fcst files used in analysis:  ',&
         (ifilesfc(i),i=1,nfldsfc),(hrdifsfc(i),i=1,nfldsfc),ntguessfc


! End of routine
    return
end subroutine read_fv3_files

subroutine read_fv3_netcdf_guess(fv3filenamegin)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_fv3_netcdf_guess            read fv3 interface file
!   prgmmr: wu               org: np22                date: 2017-07-06
!
! abstract:  read guess for FV3 regional model
! program history log:
!      2022-04-01 Y. Wang and X. Wang - add capability to read reflectivity
!                                       for direct radar EnVar DA using reflectivity as state 
!                                       variable, poc: xuguang.wang@ou.edu
!      2023-07-30 Zhao   - added code to read significant wave height (howv) field 
!                          from the 2D fv3-lam firstguess file (fv3_sfcdata).
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$  end documentation block
    use kinds, only: r_kind,i_kind
    use mpimod, only: npe
    use mpimod, only: mpi_comm_world
    use guess_grids, only:ges_prsi
    use gridmod, only: lat2,lon2,nsig,ijn,eta1_ll,eta2_ll,ijn_s
    use constants, only: one,fv
    use gsi_metguess_mod, only: gsi_metguess_bundle
    use gsi_chemguess_mod, only: gsi_chemguess_bundle
    use gsi_bundlemod, only: gsi_bundleinquire, gsi_bundlegetpointer
    use gsi_bundlemod, only: gsi_bundlecreate,gsi_bundledestroy
    use general_sub2grid_mod, only: general_sub2grid_create_info
    use mpeu_util, only: die
    use guess_grids, only: nfldsig,ntguessig
    use directDA_radaruse_mod, only: l_use_cvpqx, cvpqx_pval
    use directDA_radaruse_mod, only: l_use_dbz_directDA
    use directDA_radaruse_mod, only: l_cvpnr, cvpnr_pval
    use gridmod,only: regional
    use gridmod,only: l_reg_update_hydro_delz
    use gridmod, only: grd_a
    use mpimod, only: mype
    use gsi_metguess_mod, only: gsi_metguess_get
    use netcdf, only:nf90_open,nf90_close,nf90_inquire,nf90_nowrite, nf90_format_netcdf4
    use gsi_chemguess_mod, only: gsi_chemguess_get
    use obsmod, only: if_model_dbz,if_model_fed

    implicit none

    type (type_fv3regfilenameg),intent (in) :: fv3filenamegin(:)

    integer(i_kind) :: it
    character(len=24),parameter :: myname = 'read_fv3_netcdf_guess'
    integer(i_kind) k,i,j
    integer(i_kind) ier,istatus,ivar
    real(r_kind),dimension(:,:),pointer::ges_ps=>NULL()
    real(r_kind),dimension(:,:),pointer::ges_ps_readin=>NULL()
    real(r_kind),dimension(:,:),pointer::ges_z=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_u=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_v=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_q=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_oz=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_tv=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_tsen_readin=>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_delp  =>NULL()
    real(r_kind),dimension(:,:),pointer::ges_t2m=>NULL()
    real(r_kind),dimension(:,:),pointer::ges_q2m=>NULL()
    real(r_kind),dimension(:,:),pointer::ges_howv=>NULL()
    real(r_kind),dimension(:,:),pointer::ges_gust=>NULL()

    real(r_kind),dimension(:,:,:),pointer::ges_ql=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_qi=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_qr=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_iqr=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_qs=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_qg=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_qnr=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_w=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_dbz=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_fed=>NULL()

    real(r_kind),dimension(:,:,:),pointer::ges_aalj=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_acaj=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_acli=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_aclj=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_aclk=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_acors=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_aeci=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_aecj=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_afej=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_aivpo1j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_akj=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_alvoo1i=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_alvoo2i=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_alvpo1i=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_alvpo1j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_amgj=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_amnj=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_anai=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_anaj=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_anh4i=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_anh4j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_anh4k=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_ano3i=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_ano3j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_ano3k=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_aothri=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_aothrj=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_aseacat=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_asij=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_aso4i=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_aso4j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_aso4k=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_asoil=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_asvoo1i=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_asvoo2i=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_asvpo1i=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_asvpo1j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_asvpo2i=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_asvpo2j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_asvpo3j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_atij=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_atol1j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_atol2j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_atol3j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_abnz1j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_abnz2j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_abnz3j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_aiso1j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_aiso2j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_aiso3j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_atrp1j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_atrp2j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_asqtj=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_aalk1j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_aalk2j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_apah1j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_apah2j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_apah3j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_aorgcj=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_aolgbj=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_aolgaj=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_alvoo1j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_alvoo2j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_asvoo1j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_asvoo2j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_asvoo3j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_apcsoj=>NULL()

    real(r_kind),dimension(:,:,:),pointer::ges_axyl1j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_axyl2j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_axyl3j=>NULL()

    real(r_kind),dimension(:,:,:),pointer::ges_pm25at=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_pm25ac=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_pm25co=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_pm2_5=>NULL()

    real(r_kind),dimension(:,:,:),pointer::ges_amassi=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_amassj=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_amassk=>NULL()

    real(r_kind),dimension(:,:,:),pointer::ges_smoke=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_dust=>NULL()


    character(len=max_varname_length)  :: vartem=""
    character(len=64),dimension(:,:),allocatable:: names  !to be same as in the grid the dummy sub2grid_info
    character(len=64),dimension(:,:),allocatable:: uvnames
    integer(i_kind),dimension(:,:),allocatable:: lnames
    integer(i_kind),dimension(:,:),allocatable:: uvlnames
    integer(i_kind):: inner_vars,numfields
    integer(i_kind):: ndynvario2d,ntracerio2d,ilev,jdynvar,jtracer,jphyvar
    integer(i_kind):: iuv,ndynvario3d,ntracerio3d,nphyvario3d
    integer(i_kind):: ntracerchemio3d,ntracersmokeio3d
    integer(i_kind):: loc_id,ncfmt

!clt this block is still maintained for they would be needed for a certain 2d fields IO 
    it=ntguessig
    mype_2d=mod(1,npe)
    if (.not.allocated(ijns))      allocate(ijns(npe))
    if (.not.allocated(ijns2d))    allocate(ijns2d(npe))
    if (.not.allocated(ijnz))      allocate(ijnz(npe))
    if (.not.allocated(displss))   allocate(displss(npe))
    if (.not.allocated(displss2d)) allocate(displss2d(npe))
    if (.not.allocated(displsz_g)) allocate(displsz_g(npe))

    do i=1,npe
       ijns(i)=ijn_s(i)*nsig
       ijnz(i)=ijn(i)*nsig
       ijns2d(i)=ijn_s(i)*n2d
    enddo
    displss(1)=0
    displsz_g(1)=0
    displss2d(1)=0
    do i=2,npe
       displss(i)=displss(i-1)+ ijns(i-1)
       displsz_g(i)=displsz_g(i-1)+ ijnz(i-1)
       displss2d(i)=displss2d(i-1)+ ijns2d(i-1)
    enddo

    if (.not.allocated(name_metvars2d)) then
      allocate( name_metvars2d(GSI_MetGuess_Bundle(it)%n2d))
    end if
    if (.not.allocated(name_metvars3d)) then
      allocate( name_metvars3d(GSI_MetGuess_Bundle(it)%n3d))
    end if
    
    if (laeroana_fv3cmaq.or.laeroana_fv3smoke) then
      if (.not.allocated(name_chemvars3d)) then
        allocate( name_chemvars3d(GSI_ChemGuess_Bundle(it)%n3d))
      endif
    endif

    call gsi_bundleinquire (GSI_MetGuess_Bundle(it),'shortnames::2d', name_metvars2d,istatus)
    call gsi_bundleinquire (GSI_MetGuess_Bundle(it),'shortnames::3d', name_metvars3d,istatus)
    if (laeroana_fv3cmaq.or.laeroana_fv3smoke) then
      call gsi_bundleinquire (GSI_ChemGuess_Bundle(it),'shortnames::3d', name_chemvars3d,istatus)
    endif

    if(mype == 0) then
      do i=1,GSI_MetGuess_Bundle(it)%n2d
        write(6,*)'metvardeb333-2d name ', trim(name_metvars2d(i))
      enddo
      do i=1,GSI_MetGuess_Bundle(it)%n3d
        write(6,*)'metvardeb333-3d name ', trim(name_metvars3d(i))
      enddo
      if (laeroana_fv3cmaq.or.laeroana_fv3smoke) then
        do i=1,GSI_ChemGuess_Bundle(it)%n3d
          write(6,*)'chemvardeb333-3d name ', trim(name_chemvars3d(i))
        enddo
      endif
    endif
  
!here a strict requirment for the names of "u" and "v" is rquired
!maybe more flexibile procedure would relax it
     iuv=0
     ndynvario3d=0
     ntracerio3d=0
     nphyvario3d=0
     do i=1,size(name_metvars3d)
       vartem=trim(name_metvars3d(i))
       if(trim(vartem)=='u'.or.trim(vartem)=='v') then
         iuv=iuv+1
       else
         if(.not.(trim(vartem)=='iqr')) then
           if (ifindstrloc(vardynvars,trim(vartem))> 0) then
              ndynvario3d=ndynvario3d+1
           else if (ifindstrloc(vartracers,trim(vartem))> 0) then
              ntracerio3d=ntracerio3d+1
           else if (ifindstrloc(varphyvars,trim(vartem))> 0) then
              nphyvario3d=nphyvario3d+1
           else 
               write(6,*)'the metvarname1 ',trim(vartem),' has not been considered yet, stop'
               call stop2(333)
           endif 
         endif ! iqr is the inital qr, need not to be in IO 
       endif
      end do 
      if (iuv /= 2.or. ndynvario3d<=0.or.ntracerio3d<=0 ) then
        write(6,*)"the set up for met variable is not as expected, abort"
        call stop2(222)
      endif

      ivar=0 ; if (if_model_dbz) ivar=ivar+1; if(if_model_fed)  ivar=ivar+1 
      if ( ivar >  nphyvario3d ) then 
         write(6,*)"the set up for met variable (dbz and fed in phyvar) is not as expected,abort"
         call stop2(223)
      end if     

      if (fv3sar_bg_opt == 0.and.ifindstrloc(name_metvars3d,'delp') <= 0)then
         ndynvario3d=ndynvario3d+1  ! for delp  
      endif
      if (fv3sar_bg_opt == 1.and.ifindstrloc(name_metvars3d,'delp') > 0)then
         ndynvario3d=ndynvario3d-1  ! delp should not be used in IO  
      endif
      if (l_reg_update_hydro_delz.and.fv3sar_bg_opt==0) ndynvario3d=ndynvario3d+1 ! for delzinc
      allocate(fv3lam_io_dynmetvars3d_nouv(ndynvario3d))
      allocate(fv3lam_io_tracermetvars3d_nouv(ntracerio3d))
      allocate(fv3lam_io_phymetvars3d_nouv(nphyvario3d))

      if (laeroana_fv3cmaq) then
        allocate(fv3lam_io_tracerchemvars3d_nouv(naero_cmaq_fv3+7))
      endif 
  
      if (laeroana_fv3smoke) then 
        allocate(fv3lam_io_tracersmokevars3d_nouv(naero_smoke_fv3+1)) 
      endif

      jdynvar=0
      jtracer=0
      jphyvar=0
      do i=1,size(name_metvars3d)
        vartem=trim(name_metvars3d(i))
        if(.not.(trim(vartem)=='u'.or.trim(vartem)=='v'.or.trim(vartem)=='iqr')) then
          if(trim(vartem)=="tv" ) then
            jdynvar=jdynvar+1
            fv3lam_io_dynmetvars3d_nouv(jdynvar)="tsen"
          else if (ifindstrloc(vardynvars,trim(vartem)) > 0) then
            if(.not.(fv3sar_bg_opt==1.and.trim(vartem)=="delp")) then
              jdynvar=jdynvar+1
              fv3lam_io_dynmetvars3d_nouv(jdynvar)=trim(vartem)
            endif
           else if (ifindstrloc(vartracers,trim(vartem)) > 0) then
             jtracer=jtracer+1
             fv3lam_io_tracermetvars3d_nouv(jtracer)=trim(vartem)
           else if (ifindstrloc(varphyvars,trim(vartem)) > 0) then
             jphyvar=jphyvar+1
             fv3lam_io_phymetvars3d_nouv(jphyvar)=trim(vartem)
           else
              write(6,*)'the metvarname ',vartem,' is not expected, stop'
              call stop2(333)
           endif
        endif
      enddo
      if(fv3sar_bg_opt == 0.and.ifindstrloc( fv3lam_io_dynmetvars3d_nouv(1:jdynvar),"delp")<= 0)  then
        jdynvar=jdynvar+1
        fv3lam_io_dynmetvars3d_nouv(jdynvar)="delp"
      endif
      if (l_reg_update_hydro_delz.and.fv3sar_bg_opt==0) then 
         jdynvar=jdynvar+1
         fv3lam_io_dynmetvars3d_nouv(jdynvar)="delzinc"
      endif
      if(jdynvar /= ndynvario3d.or.jtracer /= ntracerio3d.or.jphyvar /= nphyvario3d  ) then
          write(6,*)'ndynvario3d is not as expected, stop'
          call stop2(333)
      endif
      if(mype == 0) then
        write(6,*) ' fv3lam_io_dynmetvars3d_nouv is ',(trim(fv3lam_io_dynmetvars3d_nouv(i)),i=1,ndynvario3d)
        write(6,*) ' fv3lam_io_tracermevars3d_nouv is ',(trim(fv3lam_io_tracermetvars3d_nouv(i)),i=1,ntracerio3d)
        write(6,*) ' fv3lam_io_phymetvars3d_nouv is ',(trim(fv3lam_io_phymetvars3d_nouv(i)),i=1,nphyvario3d)
      endif
 
      ndynvario2d=0
      ntracerio2d=0
      do i=1,size(name_metvars2d)
        vartem=trim(name_metvars2d(i))
        if(.not. (trim(vartem)=='ps'.and.fv3sar_bg_opt==0)) then
          if (ifindstrloc(vardynvars,trim(vartem))> 0) then
            ndynvario2d=ndynvario2d+1
          else if (ifindstrloc(vartracers,trim(vartem)) > 0) then
            ntracerio2d=ntracerio2d+1
          else if(trim(vartem)=='z') then
             if(mype == 0) write(6,*)'the metvarname ',trim(vartem),' will be dealt separately'
          else if(trim(vartem)=='t2m') then
          else if(trim(vartem)=='q2m') then
          else if(trim(vartem)=='howv') then
          else if(trim(vartem)=='gust') then
          else 
            write(6,*)'the metvarname2 ',trim(vartem),' has not been considered yet, stop'
            call stop2(333)
        endif 
       endif
      end do 
      if (ndynvario2d > 0) then
        if (.not. allocated(fv3lam_io_dynmetvars2d_nouv)) then
          allocate(fv3lam_io_dynmetvars2d_nouv(ndynvario2d))
        end if
      endif
      if (ntracerio2d > 0) then
        allocate(fv3lam_io_tracermetvars2d_nouv(ntracerio2d))
      end if
      jdynvar=0
      jtracer=0
      do i=1,size(name_metvars2d)
        vartem=trim(name_metvars2d(i))
        if(.not.( (trim(vartem)=='ps'.and.fv3sar_bg_opt==0).or.(trim(vartem)=="z") &
                  .or.(trim(vartem)=="t2m").or.(trim(vartem)=="q2m")               &
                  .or.(trim(vartem)=="howv").or.(trim(vartem)=="gust")))  then ! z is treated separately
          if (ifindstrloc(vardynvars,trim(vartem)) > 0) then
            jdynvar=jdynvar+1
            fv3lam_io_dynmetvars2d_nouv(jdynvar)=trim(vartem)
          else if (ifindstrloc(vartracers,trim(vartem)) > 0) then
            jtracer=jtracer+1
            fv3lam_io_tracermetvars2d_nouv(jtracer)=trim(vartem)
          else 
            write(6,*)'the metvarname3 ',trim(vartem),' has not been considered yet, stop'
            call stop2(333)
          endif 
        endif
      end do 
      if(mype == 0) then
        if (allocated(fv3lam_io_dynmetvars2d_nouv)) &
          write(6,*)' fv3lam_io_dynmetvars2d_nouv is ',(trim(fv3lam_io_dynmetvars2d_nouv(i)), i=1,ndynvario2d)
        if (allocated(fv3lam_io_tracermetvars2d_nouv))&
          write(6,*)'fv3lam_io_tracermetvars2d_nouv is ',(trim(fv3lam_io_tracermetvars2d_nouv(i)),i=1,ntracerio2d)
      endif      

      if (laeroana_fv3cmaq) then
        jtracer = 0
!name_chemvars3d chemguess from anavinfo
        do i=1,size(name_chemvars3d)
          vartem=trim(name_chemvars3d(i))
          if (ifindstrloc(aeronames_cmaq_fv3,trim(vartem)) > 0) then
            jtracer=jtracer+1
            fv3lam_io_tracerchemvars3d_nouv(jtracer)=trim(vartem)
            write(6,*)'the chemvarname ',jtracer,vartem,' is found '
          else
            write(6,*)'the chemvarname ',vartem,' is not in aeronames_cmaq_fv3, !!!!!!!!!!'
          endif
        enddo

        ntracerchemio3d=jtracer+7
        fv3lam_io_tracerchemvars3d_nouv(jtracer+1)="pm25at"
        fv3lam_io_tracerchemvars3d_nouv(jtracer+2)="pm25ac"
        fv3lam_io_tracerchemvars3d_nouv(jtracer+3)="pm25co"
        fv3lam_io_tracerchemvars3d_nouv(jtracer+4)="pm2_5"
        fv3lam_io_tracerchemvars3d_nouv(jtracer+5)="amassi"
        fv3lam_io_tracerchemvars3d_nouv(jtracer+6)="amassj"
        fv3lam_io_tracerchemvars3d_nouv(jtracer+7)="amassk"

        if (mype == 0) then
          write(6,*) ' fv3lam_io_tracerchemvars3d_nouv is',(trim(fv3lam_io_tracerchemvars3d_nouv(i)),i=1,ntracerchemio3d)
        endif

      endif !laeroana_fv3cmaq

      if (laeroana_fv3smoke) then
        jtracer = 0
!name_chemvars3d chemguess from anainfo
        do i=1,size(name_chemvars3d)
          vartem=trim(name_chemvars3d(i))
           if (ifindstrloc(aeronames_smoke_fv3,trim(vartem)) > 0) then
             jtracer=jtracer+1
             fv3lam_io_tracersmokevars3d_nouv(jtracer)=trim(vartem)
              write(6,*)'the chemvarname ',jtracer,vartem,' is found '
              if (trim(vartem) == "pm2_5")then
                write(6,*)'the chemvarname ',vartem,' will be calculated.'
              endif

           else
              if (trim(vartem) /= "pm2_5")then
                write(6,*)'the chemvarname ',vartem,' is not in aeronames_smoke_fv3 !!!'
              endif
           endif
        enddo
        ntracersmokeio3d=jtracer+1
        fv3lam_io_tracersmokevars3d_nouv(jtracer+1)="pm2_5"

        if (mype == 0) then
          write(6,*) ' fv3lam_io_tracersmokevars3d_nouv is',(trim(fv3lam_io_tracersmokevars3d_nouv(i)),i=1,ntracersmokeio3d)
        endif

      endif !laeroana_fv3smoke


      if (allocated(fv3lam_io_dynmetvars2d_nouv) ) then   
        call gsi_bundlecreate(gsibundle_fv3lam_dynvar_nouv,GSI_MetGuess_Bundle(it)%grid,'gsibundle_fv3lam_dynvar_nouv',istatus,&
                 names2d=fv3lam_io_dynmetvars2d_nouv,names3d=fv3lam_io_dynmetvars3d_nouv)
        ndynvario2d=size(fv3lam_io_dynmetvars2d_nouv)
      else
        call gsi_bundlecreate(gsibundle_fv3lam_dynvar_nouv,GSI_MetGuess_Bundle(it)%grid,'gsibundle_fv3lam_dynvar_nouv',istatus, &
                 names3d=fv3lam_io_dynmetvars3d_nouv)
        ndynvario2d=0
      endif
      if (allocated(fv3lam_io_tracermetvars2d_nouv) ) then   
        call gsi_bundlecreate(gsibundle_fv3lam_tracer_nouv,GSI_MetGuess_Bundle(it)%grid,'gsibundle_fv3lam_tracer_varnouv',istatus,&
                 names2d=fv3lam_io_tracermetvars2d_nouv,names3d=fv3lam_io_tracermetvars2d_nouv)
        ntracerio2d=size(fv3lam_io_tracermetvars2d_nouv)
      else
        call gsi_bundlecreate(gsibundle_fv3lam_tracer_nouv,GSI_MetGuess_Bundle(it)%grid,'gsibundle_fv3lam_tracer_nouv',istatus, &
                 names3d=fv3lam_io_tracermetvars3d_nouv)
        ntracerio2d=0
      endif
 
      if( allocated(fv3lam_io_phymetvars3d_nouv) )then
        call gsi_bundlecreate(gsibundle_fv3lam_phyvar_nouv,GSI_MetGuess_Bundle(it)%grid,'gsibundle_fv3lam_phyvar_nouv',istatus, &
                 names3d=fv3lam_io_phymetvars3d_nouv)
      end if
 
      if (laeroana_fv3cmaq) then
        if (allocated(fv3lam_io_tracerchemvars3d_nouv) ) then
          call gsi_bundlecreate(gsibundle_fv3lam_tracerchem_nouv,GSI_ChemGuess_Bundle(it)%grid,'gsibundle_fv3lam_tracerchem_nouv',istatus, &
                 names3d=fv3lam_io_tracerchemvars3d_nouv)
        endif
      endif

      if (laeroana_fv3smoke) then
        if (allocated(fv3lam_io_tracersmokevars3d_nouv) ) then
          call gsi_bundlecreate(gsibundle_fv3lam_tracersmoke_nouv,GSI_ChemGuess_Bundle(it)%grid,'gsibundle_fv3lam_tracersmoke_nouv',istatus,&
                 names3d=fv3lam_io_tracersmokevars3d_nouv)
        endif
      endif


      inner_vars=1
      numfields=inner_vars*(ndynvario3d*grd_a%nsig+ndynvario2d) 
      allocate(lnames(1,numfields),names(1,numfields))
      ilev=1
      do i=1,ndynvario3d
        do k=1,grd_a%nsig
          lnames(1,ilev)=k
          names(1,ilev)=trim(fv3lam_io_dynmetvars3d_nouv(i))
          ilev=ilev+1
        enddo
      enddo
      do i=1,ndynvario2d
        lnames(1,ilev)=1
        names(1,ilev)=trim(fv3lam_io_dynmetvars2d_nouv(i))
        ilev=ilev+1
      enddo
      call general_sub2grid_create_info(grd_fv3lam_dynvar_ionouv,inner_vars,grd_a%nlat,&
            grd_a%nlon,grd_a%nsig,numfields,regional,names=names,lnames=lnames)
      inner_vars=1
      numfields=inner_vars*(ntracerio3d*grd_a%nsig+ntracerio2d) 
      deallocate(lnames,names)
      allocate(lnames(1,numfields),names(1,numfields))
      ilev=1
      do i=1,ntracerio3d
        do k=1,grd_a%nsig
          lnames(1,ilev)=k
          names(1,ilev)=trim(fv3lam_io_tracermetvars3d_nouv(i))
          ilev=ilev+1
        enddo
      enddo
      do i=1,ntracerio2d
         lnames(1,ilev)=1
         names(1,ilev)=trim(fv3lam_io_tracermetvars2d_nouv(i))
         ilev=ilev+1
      enddo
      call general_sub2grid_create_info(grd_fv3lam_tracer_ionouv,inner_vars,grd_a%nlat,&
            grd_a%nlon,grd_a%nsig,numfields,regional,names=names,lnames=lnames)

      if (laeroana_fv3cmaq) then
        inner_vars=1
        numfields=inner_vars*(ntracerchemio3d*grd_a%nsig)
        deallocate(lnames,names)
        allocate(lnames(1,numfields),names(1,numfields))
        ilev=1
        do i=1,ntracerchemio3d
          do k=1,grd_a%nsig
            lnames(1,ilev)=k
            names(1,ilev)=trim(fv3lam_io_tracerchemvars3d_nouv(i))
            ilev=ilev+1
          enddo
        enddo
        call general_sub2grid_create_info(grd_fv3lam_tracerchem_ionouv,inner_vars,grd_a%nlat,&
            grd_a%nlon,grd_a%nsig,numfields,regional,names=names,lnames=lnames)

      endif

      if (laeroana_fv3smoke) then
        inner_vars=1
        numfields=inner_vars*(ntracersmokeio3d*grd_a%nsig)
        deallocate(lnames,names)
        allocate(lnames(1,numfields),names(1,numfields))
        ilev=1
        do i=1,ntracersmokeio3d
          do k=1,grd_a%nsig
            lnames(1,ilev)=k
            names(1,ilev)=trim(fv3lam_io_tracersmokevars3d_nouv(i))
            ilev=ilev+1
          enddo
        enddo
        call general_sub2grid_create_info(grd_fv3lam_tracersmoke_ionouv,inner_vars,grd_a%nlat,&
              grd_a%nlon,grd_a%nsig,numfields,regional,names=names,lnames=lnames)

      endif

      if ( if_model_dbz .or. if_model_fed )then
        inner_vars=1
        numfields=inner_vars*(nphyvario3d*grd_a%nsig)
        deallocate(lnames,names)
        allocate(lnames(1,numfields),names(1,numfields))
        ilev=1
        do i=1,nphyvario3d
          do k=1,grd_a%nsig
            lnames(1,ilev)=k
            names(1,ilev)=trim(fv3lam_io_phymetvars3d_nouv(i))
            ilev=ilev+1
          enddo
        enddo
        call general_sub2grid_create_info(grd_fv3lam_phyvar_ionouv,inner_vars,grd_a%nlat,&
              grd_a%nlon,grd_a%nsig,numfields,regional,names=names,lnames=lnames)
      end if

      inner_vars=2
      numfields=grd_a%nsig
      allocate(uvlnames(inner_vars,numfields),uvnames(inner_vars,numfields))
      do k=1,grd_a%nsig
        uvlnames(1,k)=k
        uvlnames(2,k)=k
        uvnames(1,k)='u'
        uvnames(2,k)='v'
      enddo
      call general_sub2grid_create_info(grd_fv3lam_uv,inner_vars,grd_a%nlat,&
           grd_a%nlon,grd_a%nsig,numfields,regional,names=uvnames,lnames=uvlnames)

      deallocate(lnames,names,uvlnames,uvnames)

      do it=1,nfldsig
         ier=0
         call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'ps' ,ges_ps ,istatus );ier=ier+istatus
         
         call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'z' , ges_z ,istatus );ier=ier+istatus
         call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'u' , ges_u ,istatus );ier=ier+istatus
         call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'v' , ges_v ,istatus );ier=ier+istatus
         call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'tv' ,ges_tv ,istatus );ier=ier+istatus
         call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'q'  ,ges_q ,istatus );ier=ier+istatus
         call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'oz'  ,ges_oz ,istatus );ier=ier+istatus
         if (l_use_dbz_directDA .or. nphyvario3d > 0) then
            call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'ql' ,ges_ql ,istatus );ier=ier+istatus
            call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qi' ,ges_qi ,istatus );ier=ier+istatus
            call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qr' ,ges_qr ,istatus );ier=ier+istatus
            call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qs' ,ges_qs ,istatus );ier=ier+istatus
            call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qg' ,ges_qg ,istatus );ier=ier+istatus
            call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'w' , ges_w ,istatus );ier=ier+istatus
            if (l_use_dbz_directDA) then
               call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'iqr' ,ges_iqr ,istatus );ier=ier+istatus
               call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qnr',ges_qnr ,istatus );ier=ier+istatus
            end if
            if(if_model_dbz) &
               call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'dbz' , ges_dbz ,istatus );ier=ier+istatus
            if(if_model_fed) &
               call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'fed' , ges_fed ,istatus );ier=ier+istatus
         end if
         if (ier/=0) call die(trim(myname),'cannot get pointers for fv3 met-fields, ier =',ier)
   
         if(i_use_2mq4b > 0 .and. i_use_2mt4b > 0 ) then
            call GSI_BundleGetPointer(GSI_MetGuess_Bundle(it),'q2m',ges_q2m,istatus ); ier=ier+istatus
            if (ier/=0) call die(trim(myname),'cannot get pointers for q2m, ier=',ier)
            call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it),'t2m',ges_t2m,istatus );ier=ier+istatus
            if (ier/=0) call die(trim(myname),'cannot get pointers for t2m,ier=',ier)
         endif

!---     significant wave height (howv)
         if ( i_howv_3dda == 1 ) then
            call GSI_BundleGetPointer(GSI_MetGuess_Bundle(it),'howv',ges_howv,istatus ); ier=ier+istatus
            if (ier/=0) call die(trim(myname),'cannot get pointers for howv, ier=',ier)
         endif

!---     wind gust (gust)
         if ( i_gust_3dda == 1 ) then
            call GSI_BundleGetPointer(GSI_MetGuess_Bundle(it),'gust',ges_gust,istatus ); ier=ier+istatus
            if (ier/=0) call die(trim(myname),'cannot get pointers for gust, ier=',ier)
         endif

         if(mype == 0 ) then
           call check(nf90_open(fv3filenamegin(it)%dynvars,nf90_nowrite,loc_id))
           call check(nf90_inquire(loc_id,formatNum=ncfmt))
           call check(nf90_close(loc_id))
           if(ncfmt /= nf90_format_netcdf4) then
              write(6,*) &
              'the current GSI parallelization IO for fv3_lam only works for netcdf4',&
              'ncfmt should be ', nf90_format_netcdf4,&
              ' GSI will stop  while, dynvars file is ', ncfmt
              call stop2(333)
           endif
           call check(nf90_open(fv3filenamegin(it)%tracers,nf90_nowrite,loc_id))
           call check(nf90_inquire(loc_id,formatNum=ncfmt))
           call check(nf90_close(loc_id))
           if(ncfmt /= nf90_format_netcdf4) then
              write(6,*) &
              'the current GSI parallelization IO for fv3_lam only works for netcdf4',&
              'ncfmt should be ', nf90_format_netcdf4,&
              ' GSI will stop  while, tracer file is ', ncfmt
              call stop2(333)
           endif
         endif

         if (laeroana_fv3cmaq) then
            ier=0
            call gsi_bundlegetpointer_fv3lam_chemguess
            if (ier/=0) call die(trim(myname),'cannot get pointers for fv3chem-fields, ier =',ier)
         end if

         if (laeroana_fv3smoke) then
           call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it),'smoke',ges_smoke,istatus );ier=ier+istatus
           call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it),'dust', ges_dust,istatus );ier=ier+istatus
           call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it),'pm2_5',ges_pm2_5,istatus );ier=ier+istatus
         end if

         if( fv3sar_bg_opt == 0) then 
            call gsi_fv3ncdf_readuv(grd_fv3lam_uv,ges_u,ges_v,fv3filenamegin(it),.false.)
         else
           call gsi_fv3ncdf_readuv_v1(grd_fv3lam_uv,ges_u,ges_v,fv3filenamegin(it),.false.)
         endif

         if( fv3sar_bg_opt == 0) then 
            call gsi_fv3ncdf_read(grd_fv3lam_dynvar_ionouv,gsibundle_fv3lam_dynvar_nouv &
            & ,fv3filenamegin(it)%dynvars,fv3filenamegin(it),.false.)
            call gsi_fv3ncdf_read(grd_fv3lam_tracer_ionouv,gsibundle_fv3lam_tracer_nouv &
            & ,fv3filenamegin(it)%tracers,fv3filenamegin(it),.false.)
            if( nphyvario3d > 0 )then
               call gsi_fv3ncdf_read(grd_fv3lam_phyvar_ionouv,gsibundle_fv3lam_phyvar_nouv &
               & ,fv3filenamegin(it)%phyvars,fv3filenamegin(it),.false.)
            end if
            if (laeroana_fv3cmaq) then
              call gsi_fv3ncdf_read(grd_fv3lam_tracerchem_ionouv,gsibundle_fv3lam_tracerchem_nouv &
              & ,fv3filenamegin(it)%tracers,fv3filenamegin(it),.false.)
            endif
            if (laeroana_fv3smoke) then
              call gsi_fv3ncdf_read(grd_fv3lam_tracersmoke_ionouv,gsibundle_fv3lam_tracersmoke_nouv &
              & ,fv3filenamegin(it)%tracers,fv3filenamegin(it),.false.)
            endif
         else
            call gsi_fv3ncdf_read_v1(grd_fv3lam_dynvar_ionouv,gsibundle_fv3lam_dynvar_nouv &
            & ,fv3filenamegin(it)%dynvars,fv3filenamegin(it),.false.)
            call gsi_fv3ncdf_read_v1(grd_fv3lam_tracer_ionouv,gsibundle_fv3lam_tracer_nouv &
            & ,fv3filenamegin(it)%tracers,fv3filenamegin(it),.false.)
            if (laeroana_fv3cmaq) then
              call gsi_fv3ncdf_read_v1(grd_fv3lam_tracerchem_ionouv,gsibundle_fv3lam_tracerchem_nouv &
              & ,fv3filenamegin(it)%tracers,fv3filenamegin(it),.false.)
            endif
            if (laeroana_fv3smoke) then
              call gsi_fv3ncdf_read_v1(grd_fv3lam_tracersmoke_ionouv,gsibundle_fv3lam_tracersmoke_nouv &
              & ,fv3filenamegin(it)%tracers,fv3filenamegin(it),.false.)
            endif
         endif
   
         if (laeroana_fv3cmaq) then
           ier=0
           call gsi_bundlegetpointer_fv3lam_tracerchem_nouv
           if (ier/=0) call die(trim(myname),'cannot get pointers for fv3lam_tracerchem_nouv, ier =',ier)

         ! Compute background amassi, amassj, amassk and pm2_5   
           do k=1,nsig
             do j=1,lon2
               do i=1,lat2
                 ! imodes
                 ges_amassi(i,j,k)=ges_aso4i(i,j,k)+ges_ano3i(i,j,k)+ges_anh4i(i,j,k)+ &
                               ges_anai(i,j,k) +ges_acli(i,j,k) +ges_aeci(i,j,k) + &
                               ges_alvpo1i(i,j,k)+ges_asvpo1i(i,j,k)+ges_asvpo2i(i,j,k)+&
                               ges_alvoo1i(i,j,k)+ges_alvoo2i(i,j,k)+ &
                               ges_asvoo1i(i,j,k)+ges_asvoo2i(i,j,k)+ges_aothri(i,j,k)
                 ! jmodes
                 ges_amassj(i,j,k)=ges_aso4j(i,j,k)+ges_ano3j(i,j,k)+ges_anh4j(i,j,k)+ &
                               ges_anaj(i,j,k) +ges_aclj(i,j,k) +ges_aecj(i,j,k) + &
                               ges_alvpo1j(i,j,k)+ges_asvpo1j(i,j,k)+ges_asvpo2j(i,j,k)+&
                               ges_asvpo3j(i,j,k)+ges_aivpo1j(i,j,k)+ &
                               ges_axyl1j(i,j,k) +ges_axyl2j(i,j,k)+ges_axyl3j(i,j,k)+ges_atol1j(i,j,k)+ges_aothrj(i,j,k)+&
                               ges_afej(i,j,k)   +ges_asij(i,j,k)  +ges_atij(i,j,k)  +ges_acaj(i,j,k)+&
                               ges_amgj(i,j,k)   +ges_amnj(i,j,k)  +ges_aalj(i,j,k)  +ges_akj(i,j,k)+&
                               ges_atol2j(i,j,k) +ges_atol3j(i,j,k)+&
                               ges_abnz1j(i,j,k) +ges_abnz2j(i,j,k)+ges_abnz3j(i,j,k)+&
                               ges_aiso1j(i,j,k) +ges_aiso2j(i,j,k)+ges_aiso3j(i,j,k)+&
                               ges_atrp1j(i,j,k) +ges_atrp2j(i,j,k)+ges_asqtj(i,j,k)+&
                               ges_aalk1j(i,j,k) +ges_aalk2j(i,j,k)+&
                               ges_apah1j(i,j,k) +ges_apah2j(i,j,k)+ges_apah3j(i,j,k)+&
                               ges_aorgcj(i,j,k) +ges_aolgbj(i,j,k)+ges_aolgaj(i,j,k)+&
                               ges_alvoo1j(i,j,k)+ges_alvoo2j(i,j,k)+&
                               ges_asvoo1j(i,j,k)+ges_asvoo2j(i,j,k)+ges_asvoo3j(i,j,k)+&
                               ges_apcsoj(i,j,k)
                 ! kmodes
                 ges_amassk(i,j,k)=ges_aso4k(i,j,k)+ges_ano3k(i,j,k)+ges_anh4k(i,j,k)+ &
                               ges_asoil(i,j,k)+ges_acors(i,j,k)+ges_aseacat(i,j,k)+ges_aclk(i,j,k)

                 ges_pm2_5(i,j,k)=ges_pm25at(i,j,k)*ges_amassi(i,j,k) +  ges_pm25ac(i,j,k)*ges_amassj(i,j,k) + &
                               ges_pm25co(i,j,k)*ges_amassk(i,j,k)
               enddo
             enddo
           enddo
         end if ! laeroana_fv3cmaq 

         if (laeroana_fv3smoke) then
           ier=0
           call GSI_BundleGetPointer ( gsibundle_fv3lam_tracersmoke_nouv,'smoke',ges_smoke,istatus );ier=ier+istatus
           call GSI_BundleGetPointer ( gsibundle_fv3lam_tracersmoke_nouv,'dust',ges_dust,istatus );ier=ier+istatus
           call GSI_BundleGetPointer ( gsibundle_fv3lam_tracersmoke_nouv,'pm2_5',ges_pm2_5,istatus );ier=ier+istatus
           if (ier/=0) call die(trim(myname),'cannot get pointers for fv3 chem-fields, ier =',ier)
           ! Calculate  pm2_5
             do k=1,nsig
               do j=1,lon2
                 do i=1,lat2
                   ges_pm2_5(i,j,k)=ges_smoke(i,j,k) +  ges_dust(i,j,k)
                 enddo
               enddo
             enddo
         endif !laeroana_fv3smoke 

         if( fv3sar_bg_opt == 0) then
           call GSI_BundleGetPointer ( gsibundle_fv3lam_dynvar_nouv, 'delp'  ,ges_delp ,istatus );ier=ier+istatus
           if(istatus==0) ges_delp=ges_delp*0.001_r_kind
         endif
         call gsi_copy_bundle(gsibundle_fv3lam_dynvar_nouv,GSI_MetGuess_Bundle(it))
         call gsi_copy_bundle(gsibundle_fv3lam_tracer_nouv,GSI_MetGuess_Bundle(it))
         if (laeroana_fv3cmaq) then
           call gsi_copy_bundle(gsibundle_fv3lam_tracerchem_nouv,GSI_ChemGuess_Bundle(it))
         endif
         if (laeroana_fv3smoke) then
           call gsi_copy_bundle(gsibundle_fv3lam_tracersmoke_nouv,GSI_ChemGuess_Bundle(it))
         endif
         if ( nphyvario3d > 0 ) then 
           call gsi_copy_bundle(gsibundle_fv3lam_phyvar_nouv,GSI_MetGuess_Bundle(it))
         end if
         call GSI_BundleGetPointer ( gsibundle_fv3lam_dynvar_nouv, 'tsen' ,ges_tsen_readin ,istatus );ier=ier+istatus
     !!  tsen2tv  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         do k=1,nsig
            do j=1,lon2
               do i=1,lat2
                  ges_tv(i,j,k)=ges_tsen_readin(i,j,k)*(one+fv*ges_q(i,j,k))
               enddo
            enddo
         enddo
         if( fv3sar_bg_opt == 0) then
           if (.not.allocated(ges_delp_bg))allocate(ges_delp_bg(lat2,lon2,nsig))
           if (.not.allocated(ges_ps_bg))allocate(ges_ps_bg(lat2,lon2))
           ges_delp_bg=ges_delp
           ges_prsi(:,:,nsig+1,it)=eta1_ll(nsig+1)
           do i=nsig,1,-1
              ges_prsi(:,:,i,it)=ges_delp(:,:,i)+ges_prsi(:,:,i+1,it)
           enddo
           ges_ps(:,:)=ges_prsi(:,:,1,it)
           ges_ps_bg=ges_ps
         else
           call GSI_BundleGetPointer ( gsibundle_fv3lam_dynvar_nouv, 'ps'  ,ges_ps_readin ,istatus );ier=ier+istatus
           ges_ps_readin=ges_ps_readin*0.001_r_kind  !which is from 
           ges_ps=ges_ps_readin
           ges_ps_bg=ges_ps
           ges_prsi(:,:,nsig+1,it)=eta1_ll(nsig+1)
           do k=1,nsig
              ges_prsi(:,:,k,it)=eta1_ll(k)+eta2_ll(k)*ges_ps
           enddo



         endif


         call gsi_fv3ncdf2d_read(fv3filenamegin(it),it,ges_z,ges_t2m,ges_q2m,   &
                                 ges_howv,ges_gust)

         if(i_use_2mq4b > 0 .and. i_use_2mt4b > 0 ) then
! Convert 2m guess mixing ratio to specific humidity
            ges_q2m = ges_q2m/(one+ges_q2m)
         endif

         if (l_use_dbz_directDA ) then
           if( fv3sar_bg_opt == 0) then
             ges_iqr=ges_qr
           else
              write(6,*) "FV3 IO READ for 'fv3sar_bg_opt == 0' is only available for now in direct reflectivity DA"
              stop
           end if

           call convert_qx_to_cvpqx(ges_qr, ges_qs, ges_qg, l_use_cvpqx, cvpqx_pval) ! convert Qx
           call convert_nx_to_cvpnx(ges_qnr, l_cvpnr, cvpnr_pval)                          ! convert Qnx

         end if

      end do
contains
subroutine gsi_bundlegetpointer_fv3lam_chemguess 
    implicit none

    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'aalj',ges_aalj,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'acaj',ges_acaj,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'acli',ges_acli,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'aclj',ges_aclj,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'aclk',ges_aclk,istatus );ier=ier+istatus

    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'acors',ges_acors,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'aeci',ges_aeci,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'aecj',ges_aecj,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'afej',ges_afej,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'aivpo1j',ges_aivpo1j,istatus );ier=ier+istatus

    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'akj',ges_akj,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'alvoo1i',ges_alvoo1i,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'alvoo2i',ges_alvoo2i,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'alvpo1i',ges_alvpo1i,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'alvpo1j',ges_alvpo1j,istatus );ier=ier+istatus

    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'amgj',ges_amgj,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'amnj',ges_amnj,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'anai',ges_anai,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'anaj',ges_anaj,istatus );ier=ier+istatus

    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'anh4i',ges_anh4i,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'anh4j',ges_anh4j,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'anh4k',ges_anh4k,istatus );ier=ier+istatus

    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'ano3i',ges_ano3i,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'ano3j',ges_ano3j,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'ano3k',ges_ano3k,istatus );ier=ier+istatus

    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'aothri',ges_aothri,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'aothrj',ges_aothrj,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'aseacat',ges_aseacat,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'asij',ges_asij,istatus );ier=ier+istatus

    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'aso4i',ges_aso4i,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'aso4j',ges_aso4j,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'aso4k',ges_aso4k,istatus );ier=ier+istatus

    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'asoil',ges_asoil,istatus );ier=ier+istatus

    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'asvoo1i',ges_asvoo1i,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'asvoo2i',ges_asvoo2i,istatus );ier=ier+istatus

    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'asvpo1i',ges_asvpo1i,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'asvpo1j',ges_asvpo1j,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'asvpo2i',ges_asvpo2i,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'asvpo2j',ges_asvpo2j,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'asvpo3j',ges_asvpo3j,istatus );ier=ier+istatus

    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'atij',ges_atij,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'atol1j',ges_atol1j,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'atol2j',ges_atol2j,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'atol3j',ges_atol3j,istatus );ier=ier+istatus


    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'abnz1j',ges_abnz1j,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'abnz2j',ges_abnz2j,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'abnz3j',ges_abnz3j,istatus );ier=ier+istatus

    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'aiso1j',ges_aiso1j,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'aiso2j',ges_aiso2j,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'aiso3j',ges_aiso3j,istatus );ier=ier+istatus

    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'atrp1j',ges_atrp1j,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'atrp2j',ges_atrp2j,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'asqtj' ,ges_asqtj,istatus );ier=ier+istatus

    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'aalk1j',ges_aalk1j,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'aalk2j',ges_aalk2j,istatus );ier=ier+istatus

    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'apah1j',ges_apah1j,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'apah2j',ges_apah2j,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'apah3j',ges_apah3j,istatus );ier=ier+istatus

    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'aorgcj',ges_aorgcj,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'aolgbj',ges_aolgbj,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'aolgaj',ges_aolgaj,istatus );ier=ier+istatus

    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'alvoo1j',ges_alvoo1j,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'alvoo2j',ges_alvoo2j,istatus );ier=ier+istatus

    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'asvoo1j',ges_asvoo1j,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'asvoo2j',ges_asvoo2j,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'asvoo3j',ges_asvoo3j,istatus );ier=ier+istatus

    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'apcsoj',ges_apcsoj,istatus );ier=ier+istatus

    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'axyl1j',ges_axyl1j,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'axyl2j',ges_axyl2j,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'axyl3j',ges_axyl3j,istatus );ier=ier+istatus

    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it),'pm25at',ges_pm25at,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it),'pm25ac',ges_pm25ac,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it),'pm25co',ges_pm25co,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it),'pm2_5',ges_pm2_5,istatus );ier=ier+istatus

    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'amassi',ges_amassi,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'amassj',ges_amassj,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'amassk',ges_amassk,istatus );ier=ier+istatus
end subroutine gsi_bundlegetpointer_fv3lam_chemguess

subroutine gsi_bundlegetpointer_fv3lam_tracerchem_nouv
    implicit none

    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'aalj',ges_aalj,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'acaj',ges_acaj,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'acli',ges_acli,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'aclj',ges_aclj,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'aclk',ges_aclk,istatus );ier=ier+istatus

    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'acors',ges_acors,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'aeci',ges_aeci,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'aecj',ges_aecj,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'afej',ges_afej,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'aivpo1j',ges_aivpo1j,istatus );ier=ier+istatus

    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'akj',ges_akj,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'alvoo1i',ges_alvoo1i,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'alvoo2i',ges_alvoo2i,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'alvpo1i',ges_alvpo1i,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'alvpo1j',ges_alvpo1j,istatus );ier=ier+istatus

    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'amgj',ges_amgj,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'amnj',ges_amnj,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'anai',ges_anai,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'anaj',ges_anaj,istatus );ier=ier+istatus

    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'anh4i',ges_anh4i,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'anh4j',ges_anh4j,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'anh4k',ges_anh4k,istatus );ier=ier+istatus

    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'ano3i',ges_ano3i,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'ano3j',ges_ano3j,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'ano3k',ges_ano3k,istatus );ier=ier+istatus

    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'aothri',ges_aothri,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'aothrj',ges_aothrj,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'aseacat',ges_aseacat,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'asij',ges_asij,istatus );ier=ier+istatus

    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'aso4i',ges_aso4i,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'aso4j',ges_aso4j,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'aso4k',ges_aso4k,istatus );ier=ier+istatus

    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'asoil',ges_asoil,istatus );ier=ier+istatus

    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'asvoo1i',ges_asvoo1i,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'asvoo2i',ges_asvoo2i,istatus );ier=ier+istatus

    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'asvpo1i',ges_asvpo1i,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'asvpo1j',ges_asvpo1j,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'asvpo2i',ges_asvpo2i,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'asvpo2j',ges_asvpo2j,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'asvpo3j',ges_asvpo3j,istatus );ier=ier+istatus

    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'atij',ges_atij,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'atol1j',ges_atol1j,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'atol2j',ges_atol2j,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'atol3j',ges_atol3j,istatus );ier=ier+istatus


    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'abnz1j',ges_abnz1j,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'abnz2j',ges_abnz2j,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'abnz3j',ges_abnz3j,istatus );ier=ier+istatus

    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'aiso1j',ges_aiso1j,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'aiso2j',ges_aiso2j,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'aiso3j',ges_aiso3j,istatus );ier=ier+istatus

    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'atrp1j',ges_atrp1j,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'atrp2j',ges_atrp2j,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'asqtj' ,ges_asqtj,istatus );ier=ier+istatus

    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'aalk1j',ges_aalk1j,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'aalk2j',ges_aalk2j,istatus );ier=ier+istatus

    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'apah1j',ges_apah1j,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'apah2j',ges_apah2j,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'apah3j',ges_apah3j,istatus );ier=ier+istatus

    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'aorgcj',ges_aorgcj,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'aolgbj',ges_aolgbj,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'aolgaj',ges_aolgaj,istatus );ier=ier+istatus

    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'alvoo1j',ges_alvoo1j,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'alvoo2j',ges_alvoo2j,istatus );ier=ier+istatus

    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'asvoo1j',ges_asvoo1j,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'asvoo2j',ges_asvoo2j,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'asvoo3j',ges_asvoo3j,istatus );ier=ier+istatus

    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'apcsoj',ges_apcsoj,istatus );ier=ier+istatus

    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'axyl1j',ges_axyl1j,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'axyl2j',ges_axyl2j,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'axyl3j',ges_axyl3j,istatus );ier=ier+istatus

    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'pm25at',ges_pm25at,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'pm25ac',ges_pm25ac,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'pm25co',ges_pm25co,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'pm2_5',ges_pm2_5,istatus );ier=ier+istatus

    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'amassi',ges_amassi,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'amassj',ges_amassj,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( gsibundle_fv3lam_tracerchem_nouv, 'amassk',ges_amassk,istatus );ier=ier+istatus
end subroutine gsi_bundlegetpointer_fv3lam_tracerchem_nouv

end subroutine read_fv3_netcdf_guess

subroutine gsi_fv3ncdf2d_read(fv3filenamegin,it,ges_z,ges_t2m,ges_q2m,          &
                              ges_howv,ges_gust)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    gsi_fv3ncdf2d_read       
!   prgmmr: wu w             org: np22                date: 2017-10-17
!
! abstract: read in 2d fields from fv3_sfcdata file in mype_2d 
!                Scatter the field to each PE 
! program history log:
!   2023-02-14  Hu   -  Bug fix for read in subdomain surface restart files
!   2023-07-30  Zhao -  added IO to read significant wave height (howv) from 2D FV3-LAM
!                         firstguess file (fv3_sfcdata)
!
!   input argument list:
!     it    - time index for 2d fields
!
!   output argument list:
!     ges_z - surface elevation
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$  end documentation block
    use kinds, only: r_kind,i_kind
    use mpimod, only: ierror,mpi_comm_world,npe,mpi_rtype,mype,mpi_itype
    use mpeu_util, only: die
    use guess_grids, only: fact10,soil_type,veg_frac,veg_type,sfc_rough, &
         sfct,sno,soil_temp,soil_moi,isli
    use gridmod, only: lat2,lon2,itotsub,ijn_s
    use general_commvars_mod, only: ltosi_s,ltosj_s
    use netcdf, only: nf90_open,nf90_close,nf90_get_var,nf90_noerr
    use netcdf, only: nf90_nowrite,nf90_inquire,nf90_inquire_dimension
    use netcdf, only: nf90_inquire_variable
    use netcdf, only: nf90_inq_varid
    use netcdf, only: nf90_noerr
    use mod_fv3_lola, only: fv3_h_to_ll,nxa,nya
    use constants, only: grav
    use constants, only: zero

    implicit none

    integer(i_kind),            intent(in) :: it   
    real(r_kind),               intent(in),dimension(:,:),pointer::ges_z
    real(r_kind),               intent(in),dimension(:,:),pointer::ges_t2m
    real(r_kind),               intent(in),dimension(:,:),pointer::ges_q2m
    real(r_kind),               intent(in),dimension(:,:),pointer::ges_howv
    real(r_kind),               intent(in),dimension(:,:),pointer::ges_gust
    type (type_fv3regfilenameg),intent(in) :: fv3filenamegin

    character(len=max_varname_length) :: name
    integer(i_kind),allocatable,dimension(:):: dim
    real(r_kind),allocatable,dimension(:):: work
    real(r_kind),allocatable,dimension(:,:):: a
    real(r_kind),allocatable,dimension(:,:,:):: sfcn2d
    real(r_kind),allocatable,dimension(:,:,:):: sfc
    real(r_kind),allocatable,dimension(:,:):: sfc1
    integer(i_kind) iret,gfile_loc,i,k,len,ndim
    integer(i_kind) ndimensions,nvariables,nattributes,unlimiteddimid
    integer(i_kind) kk,n,ns,j,ii,jj,mm1
      character(len=:),allocatable :: sfcdata   !='fv3_sfcdata'
      character(len=:),allocatable :: dynvars   !='fv3_dynvars'
! for checking the existence of howv/gust in firstguess file
    integer(i_kind) id_howv, id_gust
    integer(i_kind) iret_bcast

! for io_layout > 1
    real(r_kind),allocatable,dimension(:,:):: sfc_fulldomain
    integer(i_kind) :: nio
    integer(i_kind),allocatable :: gfile_loc_layout(:)
    character(len=180)  :: filename_layout

! for sfc 2d vaiable exist or not
    logical, dimension(n2d) :: sfc_var_exist

    sfcdata= fv3filenamegin%sfcdata
    dynvars= fv3filenamegin%dynvars

    mm1=mype+1
    allocate(a(nya,nxa))
    allocate(work(itotsub*n2d))
    allocate( sfcn2d(lat2,lon2,n2d))

!-- initialisation of the array for howv/gust
    sfcn2d(:,:,k_howv) = zero
    sfcn2d(:,:,k_gust) = zero

!-- initialisation of the array for sfc_var_exist 
    sfc_var_exist = .false.

    if(mype==mype_2d ) then
       allocate(sfc_fulldomain(nx,ny))

       if(fv3_io_layout_y > 1) then
         allocate(gfile_loc_layout(0:fv3_io_layout_y-1))
         do nio=0,fv3_io_layout_y-1
           write(filename_layout,'(a,a,I4.4)') trim(sfcdata),'.',nio
           iret=nf90_open(filename_layout,nf90_nowrite,gfile_loc_layout(nio))
           if(iret/=nf90_noerr) then
             write(6,*)' problem opening3 ',trim(filename_layout),', Status = ',iret
             return
           endif
         enddo
         gfile_loc=gfile_loc_layout(0)
       else
         iret=nf90_open(sfcdata,nf90_nowrite,gfile_loc)
         if(iret/=nf90_noerr) then
            write(6,*)' problem opening3 ',trim(sfcdata),', Status = ',iret
            return
         endif
       endif
       iret=nf90_inquire(gfile_loc,ndimensions,nvariables,nattributes,unlimiteddimid)
       allocate(dim(ndimensions))
       do k=1,ndimensions
          iret=nf90_inquire_dimension(gfile_loc,k,name,len)
          dim(k)=len
       enddo

!---   check the existence of significant wave height (howv) in 2D FV3-LAM firstguess file
!      if howv is set in anavinfo (as i_howv_3dda=1), then check its existence in firstguess,
!      but if it is not found in firstguess, then stop GSI run and set i_howv_3dda = 0.
       if ( i_howv_3dda == 1 ) then
         iret = nf90_inq_varid(gfile_loc,'howv',id_howv)
         if ( iret /= nf90_noerr ) then
           iret = nf90_inq_varid(gfile_loc,'HOWV',id_howv) ! double check with name in uppercase
         end if
         if ( iret /= nf90_noerr ) then
           i_howv_3dda = 0                ! howv does not exist in firstguess, then stop GSI run.
           call die('gsi_fv3ncdf2d_read','Warning: CANNOT find howv in firstguess, aborting..., iret = ', iret)
         else
           write(6,'(1x,A,1x,A,1x,A,1x,I4,1x,I4,1x,A,1x,I4.4,A)') 'gsi_fv3ncdf2d_read:: Found howv in firstguess ',  &
             trim(sfcdata), ', iret, varid = ',iret, id_howv,' (on pe: ', mype,').'
         end if
       end if
!---   check the existence of wind gust (gust) in 2D FV3-LAM firstguess file
!      (similar as done above for howv)
       if ( i_gust_3dda == 1 ) then
         iret = nf90_inq_varid(gfile_loc,'gust',id_gust)
         if ( iret /= nf90_noerr ) then
           iret = nf90_inq_varid(gfile_loc,'GUST',id_gust) ! double check with name in uppercase
         end if
         if ( iret /= nf90_noerr ) then
           i_gust_3dda = 0                ! gust does not exist in firstguess, then stop GSI run.
           call die('gsi_fv3ncdf2d_read','Warning: CANNOT find gust in firstguess, aborting..., iret = ', iret)
         else
           write(6,'(1x,A,1x,A,1x,A,1x,I4,1x,I4,1x,A,1x,I4.4,A)') 'gsi_fv3ncdf2d_read:: Found gust in firstguess ',  &
             trim(sfcdata), ', iret, varid = ',iret, id_gust,' (on pe: ', mype,').'
         end if
       end if

   !!!!!!!!!!!! read in 2d variables !!!!!!!!!!!!!!!!!!!!!!!!!!
       do i=ndimensions+1,nvariables
          iret=nf90_inquire_variable(gfile_loc,i,name,len)
          if( trim(name)=='f10m'.or.trim(name)=='F10M' ) then
             k=k_f10m
             sfc_var_exist(k) = .true.
          else if( trim(name)=='stype'.or.trim(name)=='STYPE' ) then
             k=k_stype
             sfc_var_exist(k) = .true.
          else if( trim(name)=='vfrac'.or.trim(name)=='VFRAC' ) then
             k=k_vfrac
             sfc_var_exist(k) = .true.
          else if( trim(name)=='vtype'.or.trim(name)=='VTYPE' ) then
             k=k_vtype
             sfc_var_exist(k) = .true.
          else if( trim(name)=='zorl'.or.trim(name)=='ZORL' ) then
             k=k_zorl
             sfc_var_exist(k) = .true.
          else if( trim(name)=='tsea'.or.trim(name)=='TSEA' ) then
             k=k_tsea
             sfc_var_exist(k) = .true.
          else if( trim(name)=='sheleg'.or.trim(name)=='SHELEG' ) then
             k=k_snwdph
             sfc_var_exist(k) = .true.
          else if( trim(name)=='stc'.or.trim(name)=='tslb' ) then
             k=k_stc 
             sfc_var_exist(k) = .true.
          else if( trim(name)=='smc'.or.trim(name)=='smois' ) then
             k=k_smc
             sfc_var_exist(k) = .true.
          else if( trim(name)=='SLMSK'.or.trim(name)=='slmsk' ) then
             k=k_slmsk
             sfc_var_exist(k) = .true.
          else if( trim(name)=='T2M'.or.trim(name)=='t2m' ) then
             k=k_t2m
             sfc_var_exist(k) = .true.
          else if( trim(name)=='Q2M'.or.trim(name)=='q2m' ) then
             k=k_q2m
             sfc_var_exist(k) = .true.
          else if( trim(name)=='HOWV'.or.trim(name)=='howv' ) then
             k=k_howv
             sfc_var_exist(k) = .true.
          else if( trim(name)=='GUST'.or.trim(name)=='gust' ) then
             k=k_gust
             sfc_var_exist(k) = .true.
          else
             cycle 
          endif
          iret=nf90_inquire_variable(gfile_loc,i,ndims=ndim)
          if(ndim < 2) then
             write(*,*) "wrong dimension number ndim =",ndim
             call stop2(119)
          endif
          if(fv3_io_layout_y > 1) then
             do nio=0,fv3_io_layout_y-1
                if(allocated(sfc       )) deallocate(sfc       )
                allocate(sfc(nx,ny_layout_len(nio),1))
                if(ndim >=3) then
                   iret=nf90_get_var(gfile_loc_layout(nio),i,sfc)
                else if (ndim == 2) then
                   iret=nf90_get_var(gfile_loc_layout(nio),i,sfc(:,:,1))
                endif
                sfc_fulldomain(:,ny_layout_b(nio):ny_layout_e(nio))=sfc(:,:,1)
             enddo
          else
             if(allocated(sfc       )) deallocate(sfc       )
             allocate(sfc(nx,ny,1))
             if(ndim >=3) then 
                iret=nf90_get_var(gfile_loc,i,sfc)
             else if (ndim == 2) then
                iret=nf90_get_var(gfile_loc,i,sfc(:,:,1))
             endif
             sfc_fulldomain(:,:)=sfc(:,:,1)
          endif
          call fv3_h_to_ll(sfc_fulldomain,a,nx,ny,nxa,nya,grid_reverse_flag)

          kk=0
          do n=1,npe
             ns=displss2d(n)+(k-1)*ijn_s(n)
             do j=1,ijn_s(n)
                ns=ns+1
                kk=kk+1
                ii=ltosi_s(kk)
                jj=ltosj_s(kk)
                work(ns)=a(ii,jj)
             end do
          end do
       enddo ! i
       if(fv3_io_layout_y > 1) then
         do nio=0,fv3_io_layout_y-1
           iret=nf90_close(gfile_loc_layout(nio))
         enddo
         deallocate (gfile_loc_layout)
       else
         iret=nf90_close(gfile_loc)
       endif

   !!!! read in orog from dynam !!!!!!!!!!!!
       if(fv3_io_layout_y > 1) then
         allocate(gfile_loc_layout(0:fv3_io_layout_y-1))
         do nio=0,fv3_io_layout_y-1
           write(filename_layout,'(a,a,I4.4)') trim(dynvars),'.',nio
           iret=nf90_open(filename_layout,nf90_nowrite,gfile_loc_layout(nio))
           if(iret/=nf90_noerr) then
             write(6,*)' problem opening4 ',trim(filename_layout),', Status =',iret
             return
           endif
         enddo
         gfile_loc=gfile_loc_layout(0)
                  
       else
         iret=nf90_open(dynvars,nf90_nowrite,gfile_loc)
         if(iret/=nf90_noerr) then
            write(6,*)' problem opening4 ',trim(dynvars ),gfile_loc,', Status = ',iret
            return
         endif
       endif

       iret=nf90_inquire(gfile_loc,ndimensions,nvariables,nattributes,unlimiteddimid)
       if(allocated(dim    )) deallocate(dim    )
       allocate(dim(ndimensions))

       do k=1,ndimensions
          iret=nf90_inquire_dimension(gfile_loc,k,name,len)
          dim(k)=len
       enddo


       do k=ndimensions+1,nvariables
          iret=nf90_inquire_variable(gfile_loc,k,name,len)
          if(trim(name)=='PHIS'   .or. trim(name)=='phis'  ) then
             sfc_var_exist(k_orog) = .true.
             iret=nf90_inquire_variable(gfile_loc,k,ndims=ndim)
             if(fv3_io_layout_y > 1) then
                do nio=0,fv3_io_layout_y-1
                  if(allocated(sfc1       )) deallocate(sfc1       )
                  allocate(sfc1(nx,ny_layout_len(nio)) )
                  iret=nf90_get_var(gfile_loc_layout(nio),k,sfc1)
                  sfc_fulldomain(:,ny_layout_b(nio):ny_layout_e(nio))=sfc1
                enddo
             else
                if(allocated(sfc1       )) deallocate(sfc1       )
                allocate(sfc1(nx,ny) )
                iret=nf90_get_var(gfile_loc,k,sfc1)
                sfc_fulldomain=sfc1
             endif
             exit
          endif
       enddo     !   k
       if(fv3_io_layout_y > 1) then
         do nio=0,fv3_io_layout_y-1
           iret=nf90_close(gfile_loc_layout(nio))
         enddo
         deallocate(gfile_loc_layout)
       else
         iret=nf90_close(gfile_loc)
       endif

       k=k_orog
       call fv3_h_to_ll(sfc_fulldomain,a,nx,ny,nxa,nya,grid_reverse_flag)

       kk=0
       do n=1,npe
          ns=displss2d(n)+(k-1)*ijn_s(n)
          do j=1,ijn_s(n)
             ns=ns+1
             kk=kk+1
             ii=ltosi_s(kk)
             jj=ltosj_s(kk)
             work(ns)=a(ii,jj)
          end do
       end do

       if(allocated(sfc1) .and. allocated(sfc)) deallocate (sfc,sfc1)
       if(allocated(dim)) deallocate (dim)
       if(allocated(sfc_fulldomain)) deallocate (sfc_fulldomain)
    endif  ! mype

!-- broadcast the updated i_howv_3dda, i_gust_3dda to all tasks (!!!!)
    call mpi_bcast(i_howv_3dda, 1, mpi_itype, mype_2d, mpi_comm_world, iret_bcast)
    call mpi_bcast(i_gust_3dda, 1, mpi_itype, mype_2d, mpi_comm_world, iret_bcast)

!-- broadcast the updated sfc_var_exist to all tasks (!!!!)
    call mpi_bcast(sfc_var_exist, n2d, mpi_itype, mype_2d, mpi_comm_world, iret_bcast)

!!!!!!! scatter !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    call mpi_scatterv(work,ijns2d,displss2d,mpi_rtype,&
      sfcn2d,ijns2d(mm1),mpi_rtype,mype_2d,mpi_comm_world,ierror)

    deallocate ( work )

    if ( sfc_var_exist(k_f10m)  ) fact10(:,:,it)=sfcn2d(:,:,k_f10m)
    if ( sfc_var_exist(k_stype) ) soil_type(:,:,it)=sfcn2d(:,:,k_stype)
    if ( sfc_var_exist(k_vfrac) ) veg_frac(:,:,it)=sfcn2d(:,:,k_vfrac)
    if ( sfc_var_exist(k_vtype) ) veg_type(:,:,it)=sfcn2d(:,:,k_vtype)
    if ( sfc_var_exist(k_zorl)  ) sfc_rough(:,:,it)=sfcn2d(:,:,k_zorl)
    if ( sfc_var_exist(k_tsea)  ) sfct(:,:,it)=sfcn2d(:,:,k_tsea)
    if ( sfc_var_exist(k_snwdph)) sno(:,:,it)=sfcn2d(:,:,k_snwdph)
    if ( sfc_var_exist(k_stc)   ) soil_temp(:,:,it)=sfcn2d(:,:,k_stc)
    if ( sfc_var_exist(k_smc)   ) soil_moi(:,:,it)=sfcn2d(:,:,k_smc)
    if ( sfc_var_exist(k_orog)  ) ges_z(:,:)=sfcn2d(:,:,k_orog)/grav
    if ( sfc_var_exist(k_slmsk) ) isli(:,:,it)=nint(sfcn2d(:,:,k_slmsk))
    if(i_use_2mq4b > 0 .and. i_use_2mt4b > 0 ) then
       if ( sfc_var_exist(k_t2m) ) ges_t2m(:,:)=sfcn2d(:,:,k_t2m)
       if ( sfc_var_exist(k_q2m)   ) ges_q2m(:,:)=sfcn2d(:,:,k_q2m)
    endif
    if ( i_howv_3dda == 1 ) then
       if ( sfc_var_exist(k_howv)  ) ges_howv(:,:)=sfcn2d(:,:,k_howv)
    endif
    if ( i_gust_3dda == 1 ) then
       if ( sfc_var_exist(k_gust)  ) ges_gust(:,:)=sfcn2d(:,:,k_gust)
    endif
    deallocate (sfcn2d,a)
    return
end subroutine gsi_fv3ncdf2d_read
subroutine gsi_fv3ncdf2d_read_v1(filenamein,varname,varname2,work_sub,mype_io)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    gsi_fv23ncdf2d_readv1       
!   prgmmr: T. Lei                               date: 2019-03-28
!           modified from gsi_fv3ncdf_read and gsi_fv3ncdf2d_read
!
! abstract: read in a 2d field from a netcdf FV3 file in mype_io
!          then scatter the field to each PE 
! program history log:
!
!   input argument list:
!     filename    - file name to read from       
!     varname     - variable name to read in
!     varname2    - variable name to read in
!     mype_io     - pe to read in the field
!
!   output argument list:
!     work_sub    - output sub domain field
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$  end documentation block


    use kinds, only: r_kind,i_kind
    use mpimod, only: ierror,mpi_comm_world,npe,mpi_rtype,mype
    use gridmod, only: lat2,lon2,nlat,nlon,itotsub,ijn_s,displs_s
    use netcdf, only: nf90_open,nf90_close,nf90_get_var,nf90_noerr
    use netcdf, only: nf90_nowrite,nf90_inquire,nf90_inquire_dimension
    use netcdf, only: nf90_inq_varid
    use netcdf, only: nf90_inquire_variable
    use mod_fv3_lola, only: fv3_h_to_ll
    use general_commvars_mod, only: ltosi_s,ltosj_s

    implicit none
    character(*)   ,   intent(in   ) :: varname,varname2,filenamein
    real(r_kind)   ,   intent(out  ) :: work_sub(lat2,lon2) 
    integer(i_kind)   ,intent(in   ) :: mype_io
    real(r_kind),allocatable,dimension(:,:,:):: uu
    real(r_kind),allocatable,dimension(:):: work
    real(r_kind),allocatable,dimension(:,:):: a


    integer(i_kind) n,ndim
    integer(i_kind) gfile_loc,var_id,iret
    integer(i_kind) kk,j,mm1,ii,jj
    integer(i_kind) ndimensions,nvariables,nattributes,unlimiteddimid

    mm1=mype+1
    allocate (work(itotsub))

    if(mype==mype_io ) then
       iret=nf90_open(filenamein,nf90_nowrite,gfile_loc)
       if(iret/=nf90_noerr) then
          write(6,*)' gsi_fv3ncdf2d_read_v1: problem opening ',trim(filenamein),gfile_loc,', Status = ',iret
          write(6,*)' gsi_fv3ncdf2d_read_v1: problem opening with varnam ',trim(varname)
          return
       endif

       iret=nf90_inquire(gfile_loc,ndimensions,nvariables,nattributes,unlimiteddimid)
       allocate(a(nlat,nlon))

       iret=nf90_inq_varid(gfile_loc,trim(adjustl(varname)),var_id)
       if(iret/=nf90_noerr) then
         iret=nf90_inq_varid(gfile_loc,trim(adjustl(varname2)),var_id)
         if(iret/=nf90_noerr) then
           write(6,*)' wrong to get var_id ',var_id
         endif
       endif

       iret=nf90_inquire_variable(gfile_loc,var_id,ndims=ndim)
       if(allocated(uu       )) deallocate(uu       )
       allocate(uu(nx,ny,1))
       iret=nf90_get_var(gfile_loc,var_id,uu)
          call fv3_h_to_ll(uu(:,:,1),a,nx,ny,nlon,nlat,grid_reverse_flag)
          kk=0
          do n=1,npe
             do j=1,ijn_s(n)
                kk=kk+1
                ii=ltosi_s(kk)
                jj=ltosj_s(kk)
                work(kk)=a(ii,jj)
             end do
          end do

       iret=nf90_close(gfile_loc)
       deallocate (uu,a)

    endif !mype

    call mpi_scatterv(work,ijn_s,displs_s,mpi_rtype,&
       work_sub,ijn_s(mm1),mpi_rtype,mype_io,mpi_comm_world,ierror)

    deallocate (work)
    return
end subroutine  gsi_fv3ncdf2d_read_v1 

subroutine gsi_fv3ncdf_read(grd_ionouv,cstate_nouv,filenamein,fv3filenamegin,ensgrid)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    gsi_fv3ncdf_read       
!   prgmmr: wu               org: np22                date: 2017-10-10
!           lei  re-write for parallelization         date: 2021-09-29
!                 similar for horizontal recurisve filtering
! abstract: read in fields excluding u and v
! program history log:
!
!   input argument list:
!     filename    - file name to read from       
!     varname     - variable name to read in
!     varname2    - variable name to read in
!     mype_io     - pe to read in the field
!
!   output argument list:
!     work_sub    - output sub domain field
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$  end documentation block


    use kinds, only: r_kind,i_kind
    use mpimod, only: mpi_comm_world,mpi_rtype,mype,npe,setcomm,mpi_integer,mpi_max
    use mpimod, only:  MPI_INFO_NULL
    use netcdf, only: nf90_open,nf90_close,nf90_get_var,nf90_noerr
    use netcdf, only: nf90_nowrite,nf90_mpiio,nf90_inquire,nf90_inquire_dimension
    use netcdf, only: nf90_inquire_variable
    use netcdf, only: nf90_inq_varid
    use mod_fv3_lola, only: fv3_h_to_ll,fv3_h_to_ll_ens
    use gsi_bundlemod, only: gsi_bundle
    use general_sub2grid_mod, only: sub2grid_info,general_grid2sub

    implicit none
    type(sub2grid_info),        intent(in   ) :: grd_ionouv 
    type(gsi_bundle),           intent(inout) :: cstate_nouv
    character(*),               intent(in   ) :: filenamein
    type (type_fv3regfilenameg),intent(in   ) ::fv3filenamegin
    logical,                    intent(in   ) :: ensgrid

    real(r_kind),allocatable,dimension(:,:):: uu2d
    real(r_kind),dimension(1,grd_ionouv%nlat,grd_ionouv%nlon,grd_ionouv%kbegin_loc:grd_ionouv%kend_alloc):: hwork
    character(len=max_varname_length) :: varname,vgsiname
    character(len=max_varname_length) :: name
    character(len=max_filename_length) :: filenamein2
    real(r_kind),allocatable,dimension(:,:):: uu2d_tmp
    integer(i_kind) :: countloc_tmp(4),startloc_tmp(4)

    integer(i_kind) nlatcase,nloncase,nxcase,nycase,countloc(4),startloc(4)
    integer(i_kind) ilev,ilevtot,inative
    integer(i_kind) kbgn,kend,len
    logical   :: phy_smaller_domain
    integer(i_kind) gfile_loc,iret,var_id
    integer(i_kind) nz,nzp1,mm1,nx_phy

    integer(i_kind):: iworld,iworld_group,nread,mpi_comm_read,i,ierror
    integer(i_kind),dimension(npe):: members,members_read,mype_read_rank
    logical:: procuse

! for io_layout > 1
    real(r_kind),allocatable,dimension(:,:):: uu2d_layout
    integer(i_kind) :: nio
    integer(i_kind),allocatable :: gfile_loc_layout(:)
    character(len=180)  :: filename_layout

    mm1=mype+1
    nloncase=grd_ionouv%nlon
    nlatcase=grd_ionouv%nlat
    if (ensgrid) then
     nxcase=nxens
     nycase=nyens
    else
     nxcase=nx
     nycase=ny
    end if
    kbgn=grd_ionouv%kbegin_loc
    kend=grd_ionouv%kend_loc
    allocate(uu2d(nxcase,nycase))

    procuse = .false.
    members=-1
    members_read=-1
    if (kbgn<=kend) then
       procuse = .true.
       members(mm1) = mype
    endif
    call mpi_allreduce(members,members_read,npe,mpi_integer,mpi_max,mpi_comm_world,ierror)

    nread=0
    mype_read_rank=-1
    do i=1,npe
       if (members_read(i) >= 0) then
          nread=nread+1
          mype_read_rank(nread) = members_read(i)
       endif
    enddo
    
    call setcomm(iworld,iworld_group,nread,mype_read_rank,mpi_comm_read,ierror)

    if (procuse) then

       if(fv3_io_layout_y > 1) then
          allocate(gfile_loc_layout(0:fv3_io_layout_y-1))
          do nio=0,fv3_io_layout_y-1
             write(filename_layout,'(a,a,I4.4)') trim(filenamein),'.',nio
             iret=nf90_open(filename_layout,ior(nf90_nowrite,nf90_mpiio),gfile_loc_layout(nio),comm=mpi_comm_read,info=MPI_INFO_NULL) !clt
             if(iret/=nf90_noerr) then
                write(6,*)' gsi_fv3ncdf_read: problem opening ',trim(filename_layout),gfile_loc_layout(nio),', Status = ',iret
                call stop2(333)
             endif
          enddo
       else
          iret=nf90_open(filenamein,ior(nf90_nowrite,nf90_mpiio),gfile_loc,comm=mpi_comm_read,info=MPI_INFO_NULL) !clt
          if(iret/=nf90_noerr) then
             write(6,*)' gsi_fv3ncdf_read: problem opening ',trim(filenamein),gfile_loc,', Status = ',iret
             call stop2(333)
          endif
       endif
       do ilevtot=kbgn,kend
          vgsiname=grd_ionouv%names(1,ilevtot)
          if(trim(vgsiname)=='delzinc') cycle  !delzinc is not read from DZ ,it's started from hydrostatic height 
          if(trim(vgsiname)=='amassi') cycle 
          if(trim(vgsiname)=='amassj') cycle 
          if(trim(vgsiname)=='amassk') cycle 
          if(trim(vgsiname)=='pm2_5') cycle 
          call getfv3lamfilevname(vgsiname,fv3filenamegin,filenamein2,varname)
          name=trim(varname)
          if(trim(filenamein) /= trim(filenamein2)) then
             write(6,*)'filenamein and filenamein2 are not the same as expected, stop'
             call stop2(333)
          endif
          ilev=grd_ionouv%lnames(1,ilevtot)
          nz=grd_ionouv%nsig
          nzp1=nz+1
          inative=nzp1-ilev
          startloc=(/1,1,inative,1/)
          countloc=(/nxcase,nycase,1,1/)
          ! Variable ref_f3d in phy_data.nc has a smaller domain size than
          ! dynvariables and tracers as well as a reversed order in vertical
          if ( trim(adjustl(varname)) == 'ref_f3d' .or. trim(adjustl(varname)) == 'flash_extent_density' )then
             iret=nf90_inquire_dimension(gfile_loc,1,name,len)
             if(trim(name)=='xaxis_1') nx_phy=len
             if( nx_phy == nxcase )then
                allocate(uu2d_tmp(nxcase,nycase))
                countloc_tmp=(/nxcase,nycase,1,1/)
                phy_smaller_domain = .false.
             else
                allocate(uu2d_tmp(nxcase-6,nycase-6))
                countloc_tmp=(/nxcase-6,nycase-6,1,1/)
                phy_smaller_domain = .true.
             end if
             startloc_tmp=(/1,1,ilev,1/)
          end if
          
          if(fv3_io_layout_y > 1) then
             do nio=0,fv3_io_layout_y-1
                if (ensgrid) then
                   countloc=(/nxcase,ny_layout_lenens(nio)+1,1,1/)
                   allocate(uu2d_layout(nxcase,ny_layout_lenens(nio)+1))
                else
                   countloc=(/nxcase,ny_layout_len(nio),1,1/)
                   allocate(uu2d_layout(nxcase,ny_layout_len(nio)))
                end if
                iret=nf90_inq_varid(gfile_loc_layout(nio),trim(adjustl(varname)),var_id)
                iret=nf90_get_var(gfile_loc_layout(nio),var_id,uu2d_layout,start=startloc,count=countloc)
                if (ensgrid) then
                   uu2d(:,ny_layout_bens(nio):ny_layout_eens(nio))=uu2d_layout
                else
                   uu2d(:,ny_layout_b(nio):ny_layout_e(nio))=uu2d_layout
                end if
                deallocate(uu2d_layout)
             enddo
          else
             iret=nf90_inq_varid(gfile_loc,trim(adjustl(varname)),var_id)
             if ( trim(adjustl(varname)) == 'ref_f3d'.or. trim(adjustl(varname)) == 'flash_extent_density' )then
                uu2d = 0.0_r_kind
                iret=nf90_get_var(gfile_loc,var_id,uu2d_tmp,start=startloc_tmp,count=countloc_tmp)
                where(uu2d_tmp < 0.0_r_kind)
                   uu2d_tmp = 0.0_r_kind
                endwhere
                
                if( phy_smaller_domain )then
                   uu2d(4:nxcase-3,4:nycase-3) = uu2d_tmp
                else
                   uu2d(1:nxcase,1:nycase) = uu2d_tmp
                end if
                deallocate(uu2d_tmp)
             else
                iret=nf90_get_var(gfile_loc,var_id,uu2d,start=startloc,count=countloc)
             end if
          endif
          
          if (ensgrid) then
             call fv3_h_to_ll_ens(uu2d,hwork(1,:,:,ilevtot),nxcase,nycase,nloncase,nlatcase,grid_reverse_flag)
          else
             call fv3_h_to_ll(uu2d,hwork(1,:,:,ilevtot),nxcase,nycase,nloncase,nlatcase,grid_reverse_flag)
          endif
       enddo  ! ilevtot
       
       if(fv3_io_layout_y > 1) then
          do nio=1,fv3_io_layout_y-1
             iret=nf90_close(gfile_loc_layout(nio))
          enddo
          deallocate(gfile_loc_layout)
       else
          iret=nf90_close(gfile_loc)
       endif
    endif
    call mpi_barrier(mpi_comm_world,ierror)
       
    deallocate (uu2d)
    call general_grid2sub(grd_ionouv,hwork,cstate_nouv%values)
    
    return
  end subroutine gsi_fv3ncdf_read

subroutine gsi_fv3ncdf_read_v1(grd_ionouv,cstate_nouv,filenamein,fv3filenamegin,ensgrid)
  
!$$$  subprogram documentation block
!                 .      .    .                                       .
! subprogram:    gsi_fv3ncdf_read _v1      
!            Lei modified from gsi_fv3ncdf_read
!   prgmmr: wu               org: np22                date: 2017-10-10
!
! abstract: read in a field from a netcdf FV3 file in mype_io
!          then scatter the field to each PE 
! program history log:
!
!   input argument list:
!     filename    - file name to read from       
!     varname     - variable name to read in
!     varname2    - variable name to read in
!     mype_io     - pe to read in the field
!
!   output argument list:
!     work_sub    - output sub domain field
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$  end documentation block


    use kinds, only: r_kind,i_kind
    use mpimod, only:  npe,mpi_rtype,mpi_comm_world,mype,MPI_INFO_NULL
    use mpimod, only: mpi_comm_world,mpi_rtype,mype,setcomm,mpi_integer,mpi_max
    use netcdf, only: nf90_open,nf90_close,nf90_get_var,nf90_noerr
    use netcdf, only: nf90_nowrite,nf90_mpiio,nf90_inquire,nf90_inquire_dimension
    use netcdf, only: nf90_inquire_variable
    use netcdf, only: nf90_inq_varid
    use mod_fv3_lola, only: fv3_h_to_ll,fv3_h_to_ll_ens
    use gsi_bundlemod, only: gsi_bundle
    use general_sub2grid_mod, only: sub2grid_info,general_grid2sub

    implicit none
    type(sub2grid_info),         intent(in):: grd_ionouv 
    character(*),                intent(in):: filenamein
    logical,                     intent(in ) :: ensgrid
    type (type_fv3regfilenameg), intent(in) :: fv3filenamegin
    type(gsi_bundle),            intent(inout) :: cstate_nouv

    real(r_kind),allocatable,dimension(:,:):: uu2d
    real(r_kind),dimension(1,grd_ionouv%nlat,grd_ionouv%nlon,grd_ionouv%kbegin_loc:grd_ionouv%kend_alloc):: hwork
    character(len=max_filename_length) :: filenamein2
    character(len=max_varname_length) :: varname,vgsiname


    integer(i_kind) nlatcase,nloncase,nxcase,nycase,countloc(4),startloc(4)
    integer(i_kind) kbgn,kend
    integer(i_kind) var_id
    integer(i_kind) inative,ilev,ilevtot
    integer(i_kind) gfile_loc,iret
    integer(i_kind) nzp1,mm1
    
    integer(i_kind):: iworld,iworld_group,nread,mpi_comm_read,i,ierror
    integer(i_kind),dimension(npe):: members,members_read,mype_read_rank
    logical:: procuse



    mm1=mype+1

    nloncase=grd_ionouv%nlon
    nlatcase=grd_ionouv%nlat
    if (ensgrid) then
     nxcase=nxens
     nycase=nyens
    else
     nxcase=nx
     nycase=ny
    end if
    allocate(uu2d(nxcase,nycase))

    kbgn=grd_ionouv%kbegin_loc
    kend=grd_ionouv%kend_loc
    procuse = .false.
    members=-1
    members_read=-1
    if (kbgn<=kend) then
       procuse = .true.
       members(mm1) = mype
    endif
    call mpi_allreduce(members,members_read,npe,mpi_integer,mpi_max,mpi_comm_world,ierror)

    nread=0
    mype_read_rank=-1
    do i=1,npe
       if (members_read(i) >= 0) then
          nread=nread+1
          mype_read_rank(nread) = members_read(i)
       endif
    enddo
    
    call setcomm(iworld,iworld_group,nread,mype_read_rank,mpi_comm_read,ierror)

    if (procuse) then 
    iret=nf90_open(filenamein,ior(nf90_nowrite,nf90_mpiio),gfile_loc,comm=mpi_comm_read,info=MPI_INFO_NULL) !clt
    if(iret/=nf90_noerr) then
       write(6,*)' gsi_fv3ncdf_read_v1: problem opening ',trim(filenamein),gfile_loc,', Status = ',iret
       call stop2(333)
    endif


    do ilevtot=kbgn,kend
      vgsiname=grd_ionouv%names(1,ilevtot)
      call getfv3lamfilevname(vgsiname,fv3filenamegin,filenamein2,varname)
      if(trim(filenamein) /= trim(filenamein2)) then
        write(6,*)'filenamein and filenamein2 are not the same as expected, stop'
        call stop2(333)
      endif
      ilev=grd_ionouv%lnames(1,ilevtot)
      nz=grd_ionouv%nsig
      nzp1=nz+1
      inative=nzp1-ilev
      startloc=(/1,1,inative+1,1/)
      countloc=(/nxcase,nycase,1,1/)
      iret=nf90_inq_varid(gfile_loc,trim(adjustl(varname)),var_id)
      if(iret/=nf90_noerr) then
        write(6,*)' wrong to get var_id ',var_id
        call stop2(333)
      endif
      
      iret=nf90_get_var(gfile_loc,var_id,uu2d,start=startloc,count=countloc)

      if (ensgrid) then
        call fv3_h_to_ll_ens(uu2d,hwork(1,:,:,ilevtot),nxcase,nycase,nloncase,nlatcase,grid_reverse_flag)
      else
        call fv3_h_to_ll(uu2d,hwork(1,:,:,ilevtot),nxcase,nycase,nloncase,nlatcase,grid_reverse_flag)
      end if
        
    enddo ! i
    iret=nf90_close(gfile_loc)
    endif
    call general_grid2sub(grd_ionouv,hwork,cstate_nouv%values)

    deallocate (uu2d)


    return
end subroutine gsi_fv3ncdf_read_v1

subroutine gsi_fv3ncdf_readuv(grd_uv,ges_u,ges_v,fv3filenamegin,ensgrid)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    gsi_fv3ncdf_readuv
!   prgmmr: wu w             org: np22                date: 2017-11-22
!
! abstract: read in a field from a netcdf FV3 file in mype_u,mype_v
!           then scatter the field to each PE 
! program history log:
!
!   input argument list:
!
!   output argument list:
!     ges_u       - output sub domain u field
!     ges_v       - output sub domain v field
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$  end documentation block
    use kinds, only: r_kind,i_kind
    use mpimod, only: mpi_comm_world,mpi_rtype,mype,mpi_info_null,npe,setcomm,mpi_integer,mpi_max
    use netcdf, only: nf90_open,nf90_close,nf90_get_var,nf90_noerr
    use netcdf, only: nf90_nowrite,nf90_mpiio,nf90_inquire,nf90_inquire_dimension
    use netcdf, only: nf90_inquire_variable
    use netcdf, only: nf90_inq_varid
    use mod_fv3_lola, only: fv3_h_to_ll,fv3uv2earth,fv3_h_to_ll_ens,fv3uv2earthens
    use general_sub2grid_mod, only: sub2grid_info,general_grid2sub

    implicit none
    type(sub2grid_info),        intent(in):: grd_uv 
    real(r_kind),dimension(grd_uv%lat2,grd_uv%lon2,grd_uv%nsig),intent(inout)::ges_u
    real(r_kind),dimension(grd_uv%lat2,grd_uv%lon2,grd_uv%nsig),intent(inout)::ges_v
    type (type_fv3regfilenameg),intent (in) :: fv3filenamegin
    logical,                    intent(in ) :: ensgrid

    real(r_kind),dimension(2,grd_uv%nlat,grd_uv%nlon,grd_uv%kbegin_loc:grd_uv%kend_alloc):: hwork
    character(:), allocatable:: filenamein
    real(r_kind),allocatable,dimension(:,:):: u2d,v2d
    real(r_kind),allocatable,dimension(:,:):: uc2d,vc2d
    character(len=max_filename_length) :: filenamein2
    character(len=max_varname_length) :: varname,vgsiname
    real(r_kind),allocatable,dimension(:,:,:,:):: worksub
    integer(i_kind) u_grd_VarId,v_grd_VarId
    integer(i_kind) nlatcase,nloncase
    integer(i_kind) nxcase,nycase
    integer(i_kind) u_countloc(4),u_startloc(4),v_countloc(4),v_startloc(4)
    integer(i_kind) inative,ilev,ilevtot
    integer(i_kind) kbgn,kend

    integer(i_kind) gfile_loc,iret
    integer(i_kind) nz,nzp1,mm1

    integer(i_kind):: iworld,iworld_group,nread,mpi_comm_read,i,ierror
    integer(i_kind),dimension(npe):: members,members_read,mype_read_rank
    logical:: procuse

! for fv3_io_layout_y > 1
    real(r_kind),allocatable,dimension(:,:):: u2d_layout,v2d_layout
    integer(i_kind) :: nio
    integer(i_kind),allocatable :: gfile_loc_layout(:)
    character(len=180)  :: filename_layout

    mm1=mype+1
    nloncase=grd_uv%nlon
    nlatcase=grd_uv%nlat
    if (ensgrid) then
     nxcase=nxens
     nycase=nyens
    else
     nxcase=nx
     nycase=ny
    end if
    kbgn=grd_uv%kbegin_loc
    kend=grd_uv%kend_loc
    allocate(u2d(nxcase,nycase+1))
    allocate(v2d(nxcase+1,nycase))
    allocate(uc2d(nxcase,nycase))
    allocate(vc2d(nxcase,nycase))
    allocate (worksub(2,grd_uv%lat2,grd_uv%lon2,grd_uv%nsig))
    filenamein=fv3filenamegin%dynvars

    procuse = .false.
    members=-1
    members_read=-1
    if (kbgn<=kend) then
       procuse = .true.
       members(mm1) = mype
    endif

    call mpi_allreduce(members,members_read,npe,mpi_integer,mpi_max,mpi_comm_world,ierror)

    nread=0
    mype_read_rank=-1
    do i=1,npe
       if (members_read(i) >= 0) then
          nread=nread+1
          mype_read_rank(nread) = members_read(i)
       endif
    enddo

    call setcomm(iworld,iworld_group,nread,mype_read_rank,mpi_comm_read,ierror)

    if (procuse) then
       if(fv3_io_layout_y > 1) then
          allocate(gfile_loc_layout(0:fv3_io_layout_y-1))
          do nio=0,fv3_io_layout_y-1
             write(filename_layout,'(a,a,I4.4)') trim(filenamein),".",nio
             iret=nf90_open(filename_layout,nf90_nowrite,gfile_loc_layout(nio),comm=mpi_comm_read,info=MPI_INFO_NULL)
             if(iret/=nf90_noerr) then
                write(6,*)'problem opening6 ',trim(filename_layout),gfile_loc_layout(nio),', Status = ',iret
                call stop2(333)
             endif
          enddo
       else
          iret=nf90_open(filenamein,ior(nf90_nowrite,nf90_mpiio),gfile_loc,comm=mpi_comm_read,info=MPI_INFO_NULL) !clt
          if(iret/=nf90_noerr) then
             write(6,*)' problem opening6 ',trim(filenamein),', Status = ',iret
             call stop2(333)
          endif
       endif
       
       do ilevtot=kbgn,kend
          vgsiname=grd_uv%names(1,ilevtot)
          call getfv3lamfilevname(vgsiname,fv3filenamegin,filenamein2,varname)
          if(trim(filenamein) /= trim(filenamein2)) then
             write(6,*)'filenamein and filenamein2 are not the same as expected, stop'
             call stop2(333)
          endif
          ilev=grd_uv%lnames(1,ilevtot)
          nz=grd_uv%nsig
          nzp1=nz+1
          inative=nzp1-ilev
          u_countloc=(/nxcase,nycase+1,1,1/)
          v_countloc=(/nxcase+1,nycase,1,1/)
          u_startloc=(/1,1,inative,1/)
          v_startloc=(/1,1,inative,1/)
          if(fv3_io_layout_y > 1) then
             do nio=0,fv3_io_layout_y-1
                if (ensgrid) then
                   u_countloc=(/nxcase,ny_layout_lenens(nio)+1,1,1/)
                   allocate(u2d_layout(nxcase,ny_layout_lenens(nio)+1))
                else
                   u_countloc=(/nxcase,ny_layout_len(nio)+1,1,1/)
                   allocate(u2d_layout(nxcase,ny_layout_len(nio)+1))
                end if
                call check( nf90_inq_varid(gfile_loc_layout(nio),'u',u_grd_VarId) ) 
                iret=nf90_get_var(gfile_loc_layout(nio),u_grd_VarId,u2d_layout,start=u_startloc,count=u_countloc)
                if (ensgrid) then
                   u2d(:,ny_layout_bens(nio):ny_layout_eens(nio))=u2d_layout(:,1:ny_layout_lenens(nio))
                   if(nio==fv3_io_layout_y-1) u2d(:,ny_layout_eens(nio)+1)=u2d_layout(:,ny_layout_lenens(nio)+1) 
                   deallocate(u2d_layout)
                   v_countloc=(/nxcase+1,ny_layout_lenens(nio),1,1/)
                   allocate(v2d_layout(nxcase+1,ny_layout_lenens(nio)))
                else
                   u2d(:,ny_layout_b(nio):ny_layout_e(nio))=u2d_layout(:,1:ny_layout_len(nio))
                   if(nio==fv3_io_layout_y-1) u2d(:,ny_layout_e(nio)+1)=u2d_layout(:,ny_layout_len(nio)+1) 
                   deallocate(u2d_layout)
                   v_countloc=(/nxcase+1,ny_layout_len(nio),1,1/)
                   allocate(v2d_layout(nxcase+1,ny_layout_len(nio)))
                end if
                call check( nf90_inq_varid(gfile_loc_layout(nio),'v',v_grd_VarId) ) 
                iret=nf90_get_var(gfile_loc_layout(nio),v_grd_VarId,v2d_layout,start=v_startloc,count=v_countloc)
                if (ensgrid) then
                   v2d(:,ny_layout_bens(nio):ny_layout_eens(nio))=v2d_layout
                else
                   v2d(:,ny_layout_b(nio):ny_layout_e(nio))=v2d_layout
                end if
                deallocate(v2d_layout)
             enddo
          else
             call check( nf90_inq_varid(gfile_loc,'u',u_grd_VarId) ) 
             iret=nf90_get_var(gfile_loc,u_grd_VarId,u2d,start=u_startloc,count=u_countloc)
             call check( nf90_inq_varid(gfile_loc,'v',v_grd_VarId) ) 
             iret=nf90_get_var(gfile_loc,v_grd_VarId,v2d,start=v_startloc,count=v_countloc)
          endif
          
          if(.not.grid_reverse_flag) then 
             call reverse_grid_r_uv (u2d,nxcase,nycase+1,1)
             call reverse_grid_r_uv (v2d,nxcase+1,nycase,1)
          endif
          if (ensgrid) then
             call fv3uv2earthens(u2d(:,:),v2d(:,:),nxcase,nycase,uc2d,vc2d)
          else
             call fv3uv2earth(u2d(:,:),v2d(:,:),nxcase,nycase,uc2d,vc2d)
          end if
          
          !    NOTE on transfor to earth u/v:
          !       The u and v before transferring need to be in E-W/N-S grid, which is
          !       defined as reversed grid here because it is revered from map view.
          !
          !       Have set the following flag for grid orientation
          !         grid_reverse_flag=true:  E-W/N-S grid
          !         grid_reverse_flag=false: W-E/S-N grid 
          !
          !       So for preparing the wind transferring, need to reverse the grid from
          !       W-E/S-N grid to E-W/N-S grid when grid_reverse_flag=false:
          !
          !            if(.not.grid_reverse_flag) call reverse_grid_r_uv
          !
          !       and the last input parameter for fv3_h_to_ll is alway true:
          !
          !
          if (ensgrid) then
            call fv3_h_to_ll_ens(uc2d,hwork(1,:,:,ilevtot),nxcase,nycase,nloncase,nlatcase,.true.)
            call fv3_h_to_ll_ens(vc2d,hwork(2,:,:,ilevtot),nxcase,nycase,nloncase,nlatcase,.true.)
          else
            call fv3_h_to_ll(uc2d,hwork(1,:,:,ilevtot),nxcase,nycase,nloncase,nlatcase,.true.)
            call fv3_h_to_ll(vc2d,hwork(2,:,:,ilevtot),nxcase,nycase,nloncase,nlatcase,.true.)
          end if
       enddo ! i
       
       if(fv3_io_layout_y > 1) then
          do nio=0,fv3_io_layout_y-1
             iret=nf90_close(gfile_loc_layout(nio))
          enddo
          deallocate(gfile_loc_layout)
       else
          iret=nf90_close(gfile_loc)
       endif
    endif

    call mpi_barrier(mpi_comm_world,ierror)
    deallocate(u2d,v2d,uc2d,vc2d)
    
    call general_grid2sub(grd_uv,hwork,worksub) 
    ges_u=worksub(1,:,:,:)
    ges_v=worksub(2,:,:,:)
    deallocate(worksub)

end subroutine gsi_fv3ncdf_readuv
subroutine gsi_fv3ncdf_readuv_v1(grd_uv,ges_u,ges_v,fv3filenamegin,ensgrid)
!$$$  subprogram documentation block
! subprogram:    gsi_fv3ncdf_readuv_v1
!   prgmmr: wu w             org: np22                date: 2017-11-22
! program history log:
!   2019-04 lei  modified from  gsi_fv3ncdf_readuv to deal with cold start files                                       .
! abstract: read in a field from a "cold start" netcdf FV3 file in mype_u,mype_v
!           then scatter the field to each PE 
! program history log:
!
!   input argument list:
!
!   output argument list:
!     ges_u       - output sub domain u field
!     ges_v       - output sub domain v field
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$  end documentation block
    use constants, only:  half
    use kinds, only: r_kind,i_kind
    use mpimod, only: setcomm,mpi_integer,mpi_max, npe,mpi_comm_world,mpi_rtype,mype,mpi_info_null
    use netcdf, only: nf90_open,nf90_close,nf90_get_var,nf90_noerr
    use netcdf, only: nf90_nowrite,nf90_mpiio,nf90_inquire,nf90_inquire_dimension
    use netcdf, only: nf90_var_par_access,nf90_netcdf4
    use netcdf, only: nf90_inquire_variable
    use netcdf, only: nf90_inq_varid
    use mod_fv3_lola, only: fv3_h_to_ll,fv3_h_to_ll_ens
    use general_sub2grid_mod, only: sub2grid_info,general_grid2sub

    implicit none
    type(sub2grid_info),        intent(in):: grd_uv 
    real(r_kind)   ,            intent(out  ) :: ges_u(grd_uv%lat2,grd_uv%lon2,grd_uv%nsig) 
    real(r_kind)   ,            intent(out  ) :: ges_v(grd_uv%lat2,grd_uv%lon2,grd_uv%nsig) 
    type (type_fv3regfilenameg),intent (in) :: fv3filenamegin
    logical,                    intent(in ) :: ensgrid

    real(r_kind),dimension(2,grd_uv%nlat,grd_uv%nlon,grd_uv%kbegin_loc:grd_uv%kend_alloc):: hwork
    character(len=:),allocatable :: filenamein
    real(r_kind),allocatable,dimension(:,:):: us2d,vw2d
    real(r_kind),allocatable,dimension(:,:):: uorv2d
    real(r_kind),allocatable,dimension(:,:,:,:):: worksub
    character(len=max_filename_length) :: filenamein2 
    character(len=max_varname_length) :: varname
    integer(i_kind) nlatcase,nloncase
    integer(i_kind) kbgn,kend

    integer(i_kind) var_id
    integer(i_kind) gfile_loc,iret
    integer(i_kind) j,nzp1,mm1
    integer(i_kind) ilev,ilevtot,inative
    integer(i_kind) nxcase,nycase
    integer(i_kind) us_countloc(3),us_startloc(3)
    integer(i_kind) vw_countloc(3),vw_startloc(3)
    integer(i_kind):: iworld,iworld_group,nread,mpi_comm_read,i,ierror
    integer(i_kind),dimension(npe):: members,members_read,mype_read_rank
    logical:: procuse

    allocate (worksub(2,grd_uv%lat2,grd_uv%lon2,grd_uv%nsig))
    mm1=mype+1
    nloncase=grd_uv%nlon
    nlatcase=grd_uv%nlat
    if (ensgrid) then
     nxcase=nxens
     nycase=nyens
    else
     nxcase=nx
     nycase=ny
    end if
    kbgn=grd_uv%kbegin_loc
    kend=grd_uv%kend_loc
    allocate (us2d(nxcase,nycase+1),vw2d(nxcase+1,nycase))
    allocate (uorv2d(nxcase,nycase))
    procuse = .false.
    members=-1
    members_read=-1
    if (kbgn<=kend) then
       procuse = .true.
       members(mm1) = mype
    endif

    call mpi_allreduce(members,members_read,npe,mpi_integer,mpi_max,mpi_comm_world,ierror)

    nread=0
    mype_read_rank=-1
    do i=1,npe
       if (members_read(i) >= 0) then
          nread=nread+1
          mype_read_rank(nread) = members_read(i)
       endif
    enddo

    call setcomm(iworld,iworld_group,nread,mype_read_rank,mpi_comm_read,ierror)

    if (procuse) then
    
    filenamein=fv3filenamegin%dynvars
    iret=nf90_open(filenamein,ior(nf90_netcdf4,ior(nf90_nowrite,nf90_mpiio)),gfile_loc,comm=mpi_comm_read,info=MPI_INFO_NULL) !clt
    if(iret/=nf90_noerr) then
       write(6,*)' gsi_fv3ncdf_read_v1: problem opening ',trim(filenamein),gfile_loc,', Status = ',iret
       call stop2(333)
    endif
    
    do ilevtot=kbgn,kend
      varname=grd_uv%names(1,ilevtot)
      filenamein2=fv3filenamegin%dynvars
      if(trim(filenamein) /=  trim(filenamein2)) then
        write(6,*)'filenamein and filenamein2 are not the same as expected, stop'
        call stop2(333)
      endif
      ilev=grd_uv%lnames(1,ilevtot)
      nz=grd_uv%nsig
      nzp1=nz+1
      inative=nzp1-ilev
      if (ensgrid) then
       us_countloc= (/nlon_regionalens,nlat_regionalens+1,1/)
       vw_countloc= (/nlon_regionalens+1,nlat_regionalens,1/)
      else
       us_countloc= (/nlon_regional,nlat_regional+1,1/)
       vw_countloc= (/nlon_regional+1,nlat_regional,1/)
      end if
      us_startloc=(/1,1,inative+1/)
      vw_startloc=(/1,1,inative+1/)


! transfor to earth u/v, interpolate to analysis grid, reverse vertical order
      call check(nf90_inq_varid(gfile_loc,trim(adjustl("u_s")),var_id))
           
      call check(nf90_get_var(gfile_loc,var_id,us2d,start=us_startloc,count=us_countloc))
      iret=nf90_inq_varid(gfile_loc,trim(adjustl("v_w")),var_id)
      iret=nf90_get_var(gfile_loc,var_id,vw2d,start=vw_startloc,count=vw_countloc)
      do j=1,ny
        uorv2d(:,j)=half*(us2d(:,j)+us2d(:,j+1))
      enddo
          
      if (ensgrid) then
        call fv3_h_to_ll_ens(uorv2d(:,:),hwork(1,:,:,ilevtot),nxcase,nycase,nloncase,nlatcase,.true.)
      else
        call fv3_h_to_ll(uorv2d(:,:),hwork(1,:,:,ilevtot),nxcase,nycase,nloncase,nlatcase,grid_reverse_flag)
      end if
      do j=1,nx
        uorv2d(j,:)=half*(vw2d(j,:)+vw2d(j+1,:))
      enddo
      if (ensgrid) then
        call fv3_h_to_ll_ens(uorv2d(:,:),hwork(2,:,:,ilevtot),nxcase,nycase,nloncase,nlatcase,.true.)
      else
        call fv3_h_to_ll(uorv2d(:,:),hwork(2,:,:,ilevtot),nxcase,nycase,nloncase,nlatcase,grid_reverse_flag)
      end if
          
    enddo ! iilevtoto
    iret=nf90_close(gfile_loc)
    endif !procuse
    call general_grid2sub(grd_uv,hwork,worksub) 
    ges_u=worksub(1,:,:,:)
    ges_v=worksub(2,:,:,:)
    deallocate (us2d,vw2d,worksub)

end subroutine gsi_fv3ncdf_readuv_v1

subroutine gsi_fv3ncdf_read_ens_parallel_over_ens(filenamein,fv3filenamegin, &
           delp,tsen,w,q,oz,ql,qr,qs,qi,qg,dbz,fed,iope)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    gsi_fv3ncdf_read_ens_parallel_over_ens    
! program history log:
!     2022-04-01 Y. Wang and X. Wang, changed from gsi_fv3ncdf_read_ens 
!                                     for FV3LAM ensemble parallel IO in hybrid EnVar
!                                     poc: xuguang.wang@ou.edu    
!
! abstract: read in fields excluding u and v
! program history log:
!
!   input argument list:
!     filenamein    - file name to read from       
!     iope     - pe to read in the field
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$  end documentation block


    use kinds, only: r_kind,i_kind
    use mpimod, only: mpi_comm_world,mpi_rtype,mype
    use mpimod, only:  MPI_INFO_NULL
    use netcdf, only: nf90_open,nf90_close,nf90_get_var,nf90_noerr
    use netcdf, only: nf90_nowrite,nf90_mpiio,nf90_inquire,nf90_inquire_dimension
    use netcdf, only: nf90_inquire_variable
    use netcdf, only: nf90_inq_varid
    use gridmod, only: nsig,nlon,nlat
    use mod_fv3_lola, only: fv3_h_to_ll
    use gsi_bundlemod, only: gsi_bundle
    use general_sub2grid_mod, only: sub2grid_info,general_grid2sub

    implicit none
    character(*),                          intent(in) :: filenamein
    type (type_fv3regfilenameg),           intent(in) ::fv3filenamegin
    integer(i_kind)   ,                    intent(in) :: iope
    real(r_kind),dimension(nlat,nlon,nsig),intent(out),optional:: delp,tsen,w,q,oz,ql,qr,qs,qi,qg,dbz,fed
    real(r_kind),allocatable,dimension(:,:):: uu2d, uu2d_tmp
    real(r_kind),dimension(nlat,nlon,nsig):: hwork
    character(len=max_varname_length) :: varname
    character(len=max_varname_length) :: name
    character(len=max_filename_length), allocatable,dimension(:) :: varname_files

    integer(i_kind) nlatcase,nloncase,nxcase,nycase,countloc(4),startloc(4),countloc_tmp(4),startloc_tmp(4)
    integer(i_kind) ilev,ilevtot,inative,ivar
    integer(i_kind) kbgn,kend
    integer(i_kind) gfile_loc,iret,var_id
    integer(i_kind) nz,nzp1,mm1,len,nx_phy
    logical  :: phy_smaller_domain
! for io_layout > 1
    real(r_kind),allocatable,dimension(:,:):: uu2d_layout
    integer(i_kind) :: nio
    integer(i_kind),allocatable :: gfile_loc_layout(:)
    character(len=180)  :: filename_layout

    mm1=mype+1
    nloncase=nlon
    nlatcase=nlat
    nxcase=nx
    nycase=ny
    kbgn=1
    kend=nsig

    if( mype == iope )then
       allocate(uu2d(nxcase,nycase))
       if( present(delp).or.present(tsen).or.present(w) )then  ! dynvars
          if( present(w) )then
             allocate(varname_files(3))
             varname_files = (/'T   ','delp','W   '/)
          else
             allocate(varname_files(2))
             varname_files = (/'T   ','delp'/)
          end if
       end if
       if( present(q).or.present(ql).or.present(qr) )then ! tracers
          if(present(qr))then
             allocate(varname_files(7))
             varname_files = (/'sphum  ','o3mr   ','liq_wat','ice_wat','rainwat','snowwat','graupel'/)
          else
             allocate(varname_files(2))
             varname_files = (/'sphum',' o3mr'/)
          end if
       end if
       if( present(dbz) .and. present(fed) )then  ! phyvars: dbz, fed
          allocate(varname_files(2))
          varname_files = (/'ref_f3d             ','flash_extent_density'/)
       elseif( present(dbz) )then            ! phyvars: dbz
          allocate(varname_files(1))
          varname_files = (/'ref_f3d'/)
       elseif( present(fed) )then            ! phyvars: fed 
          allocate(varname_files(1))
          varname_files = (/'flash_extent_density'/)
       end if


       if(fv3_io_layout_y > 1) then
          allocate(gfile_loc_layout(0:fv3_io_layout_y-1))
          do nio=0,fv3_io_layout_y-1
             write(filename_layout,'(a,a,I4.4)') trim(filenamein),'.',nio
             iret=nf90_open(filename_layout,nf90_nowrite,gfile_loc_layout(nio),comm=mpi_comm_world,info=MPI_INFO_NULL)
             if(iret/=nf90_noerr) then
                write(6,*)' gsi_fv3ncdf_read: problem opening ',trim(filename_layout),gfile_loc_layout(nio),', Status = ',iret
                call stop2(333)
             endif
          enddo
       else
          iret=nf90_open(filenamein,ior(nf90_nowrite,nf90_mpiio),gfile_loc)
          if(iret/=nf90_noerr) then
             write(6,*)' gsi_fv3ncdf_read: problem opening ',trim(filenamein),gfile_loc,', Status = ',iret
             call stop2(333)
          endif
       endif
       do ivar = 1, size(varname_files)
          do ilevtot=kbgn,kend
             ilev=ilevtot
             nz=nsig
             nzp1=nz+1
             inative=nzp1-ilev
             startloc=(/1,1,inative,1/)
             countloc=(/nxcase,nycase,1,1/)
             varname = trim(varname_files(ivar))
             ! Variable ref_f3d in phy_data.nc has a smaller domain size than
             ! dynvariables and tracers as well as a reversed order in vertical
             if ( trim(adjustl(varname)) == 'ref_f3d' .or. trim(adjustl(varname)) == 'flash_extent_density' )then
                iret=nf90_inquire_dimension(gfile_loc,1,name,len)
                if(trim(name)=='xaxis_1') nx_phy=len
                if( nx_phy == nxcase )then
                   allocate(uu2d_tmp(nxcase,nycase))
                   countloc_tmp=(/nxcase,nycase,1,1/)
                   phy_smaller_domain = .false.
                else
                   allocate(uu2d_tmp(nxcase-6,nycase-6))
                   countloc_tmp=(/nxcase-6,nycase-6,1,1/)
                   phy_smaller_domain = .true.
                end if
                startloc_tmp=(/1,1,ilev,1/)
             end if

             if(fv3_io_layout_y > 1) then
                do nio=0,fv3_io_layout_y-1
                  countloc=(/nxcase,ny_layout_len(nio),1,1/)
                  allocate(uu2d_layout(nxcase,ny_layout_len(nio)))
                  iret=nf90_inq_varid(gfile_loc_layout(nio),trim(adjustl(varname)),var_id)
                  iret=nf90_get_var(gfile_loc_layout(nio),var_id,uu2d_layout,start=startloc,count=countloc)
                  uu2d(:,ny_layout_b(nio):ny_layout_e(nio))=uu2d_layout
                  deallocate(uu2d_layout)
                enddo
             else
                iret=nf90_inq_varid(gfile_loc,trim(adjustl(varname)),var_id)
                if ( trim(adjustl(varname)) == 'ref_f3d' .or. trim(adjustl(varname)) == 'flash_extent_density' )then
                   uu2d = 0.0_r_kind
                   iret=nf90_get_var(gfile_loc,var_id,uu2d_tmp,start=startloc_tmp,count=countloc_tmp)
                   where(uu2d_tmp < 0.0_r_kind)
                      uu2d_tmp = 0.0_r_kind
                   endwhere
                   if(phy_smaller_domain)then
                      uu2d(4:nxcase-3,4:nycase-3) = uu2d_tmp
                   else
                      uu2d = uu2d_tmp
                   end if
                   deallocate(uu2d_tmp)
                else
                   iret=nf90_get_var(gfile_loc,var_id,uu2d,start=startloc,count=countloc)
                end if
             endif
             call fv3_h_to_ll(uu2d,hwork(:,:,ilevtot),nxcase,nycase,nloncase,nlatcase,grid_reverse_flag)
          enddo  ! ilevtot
          if( present(delp).or.present(tsen).or.present(w) )then  ! dynvars
              if(ivar == 1)then
                 tsen = hwork
              else if(ivar == 2)then
                 delp = hwork
              end if
              if( present(w) .and. ivar == 3 )then
                 w = hwork
              end if
          end if
          if( present(q).or.present(ql).or.present(qr) )then ! tracers
            if(ivar == 1)then
               q = hwork
            else if(ivar == 2)then
               oz = hwork
            end if
            if(present(qr))then
              if(ivar == 3)then
                 ql = hwork
              else if(ivar == 4)then
                 qi = hwork
              else if(ivar == 5)then
                 qr = hwork
              else if(ivar == 6)then
                 qs = hwork
              else if(ivar == 7)then
                 qg = hwork
              end if
            end if
          end if
          if( present(dbz) .and. present(fed) )then ! phyvars: dbz,fed
            if(ivar == 1) dbz = hwork
            if(ivar == 2) fed = hwork
          elseif( present(dbz) )then            ! phyvars: dbz
            dbz = hwork
          elseif( present(fed) )then            ! phyvars: fed 
            fed = hwork
          end if

       end do

       if(fv3_io_layout_y > 1) then
          do nio=1,fv3_io_layout_y-1
            iret=nf90_close(gfile_loc_layout(nio))
          enddo
          deallocate(gfile_loc_layout)
        else
          iret=nf90_close(gfile_loc)
       endif

       deallocate (uu2d,varname_files)
    end if

    return
end subroutine gsi_fv3ncdf_read_ens_parallel_over_ens

subroutine gsi_fv3ncdf_readuv_ens_parallel_over_ens(ges_u,ges_v,fv3filenamegin,iope)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    gsi_fv3ncdf_readuv_ens_parallel_over_ens    
! program history log:
!     2022-04-01 Y. Wang and X. Wang, changed from gsi_fv3ncdf_readuv_ens 
!                                     for FV3LAM ensemble parallel IO in hybrid EnVar
!                                     poc: xuguang.wang@ou.edu 
!
! abstract: read in a field from a netcdf FV3 file in mype_u,mype_v
!           then scatter the field to each PE 
! program history log:
!
!   input argument list:
!
!   output argument list:
!     ges_u       - output sub domain u field
!     ges_v       - output sub domain v field
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$  end documentation block
    use kinds, only: r_kind,i_kind
    use mpimod, only: mpi_comm_world,mpi_rtype,mype,mpi_info_null
    use gridmod, only: nsig,nlon,nlat
    use netcdf, only: nf90_open,nf90_close,nf90_get_var,nf90_noerr
    use netcdf, only: nf90_nowrite,nf90_inquire,nf90_inquire_dimension
    use netcdf, only: nf90_inquire_variable
    use netcdf, only: nf90_inq_varid
    use mod_fv3_lola, only: fv3_h_to_ll,fv3uv2earth
    use general_sub2grid_mod, only: sub2grid_info,general_grid2sub

    implicit none
    real(r_kind)   ,intent(out  ) :: ges_u(nlat,nlon,nsig)
    real(r_kind)   ,intent(out  ) :: ges_v(nlat,nlon,nsig)
    type (type_fv3regfilenameg),intent (in) :: fv3filenamegin
    integer(i_kind),intent(in)    :: iope

    real(r_kind),dimension(2,nlat,nlon,nsig):: hwork
    character(:), allocatable:: filenamein
    real(r_kind),allocatable,dimension(:,:):: u2d,v2d
    real(r_kind),allocatable,dimension(:,:):: uc2d,vc2d
    integer(i_kind) u_grd_VarId,v_grd_VarId
    integer(i_kind) nlatcase,nloncase
    integer(i_kind) nxcase,nycase
    integer(i_kind) u_countloc(4),u_startloc(4),v_countloc(4),v_startloc(4)
    integer(i_kind) inative,ilev,ilevtot
    integer(i_kind) kbgn,kend

    integer(i_kind) gfile_loc,iret
    integer(i_kind) nz,nzp1,mm1

! for fv3_io_layout_y > 1
    real(r_kind),allocatable,dimension(:,:):: u2d_layout,v2d_layout
    integer(i_kind) :: nio
    integer(i_kind),allocatable :: gfile_loc_layout(:)
    character(len=180)  :: filename_layout

    mm1=mype+1
    nloncase=nlon
    nlatcase=nlat
    nxcase=nx
    nycase=ny
    kbgn=1
    kend=nsig
    if( mype == iope )then
       allocate(u2d(nxcase,nycase+1))
       allocate(v2d(nxcase+1,nycase))
       allocate(uc2d(nxcase,nycase))
       allocate(vc2d(nxcase,nycase))
       filenamein=fv3filenamegin%dynvars

       if(fv3_io_layout_y > 1) then
         allocate(gfile_loc_layout(0:fv3_io_layout_y-1))
         do nio=0,fv3_io_layout_y-1
            write(filename_layout,'(a,a,I4.4)') trim(filenamein),".",nio
            iret=nf90_open(filename_layout,nf90_nowrite,gfile_loc_layout(nio),comm=mpi_comm_world,info=MPI_INFO_NULL)
            if(iret/=nf90_noerr) then
               write(6,*)'problem opening ',trim(filename_layout),gfile_loc_layout(nio),', Status = ',iret
               call stop2(333)
            endif
         enddo
       else
          iret=nf90_open(filenamein,nf90_nowrite,gfile_loc)
          if(iret/=nf90_noerr) then
             write(6,*)' problem opening ',trim(filenamein),', Status = ',iret
             call stop2(333)
          endif
       endif
       do ilevtot=kbgn,kend
          ilev=ilevtot
          nz=nsig
          nzp1=nz+1
          inative=nzp1-ilev
          u_countloc=(/nxcase,nycase+1,1,1/)
          v_countloc=(/nxcase+1,nycase,1,1/)
          u_startloc=(/1,1,inative,1/)
          v_startloc=(/1,1,inative,1/)

          if(fv3_io_layout_y > 1) then
             do nio=0,fv3_io_layout_y-1
                u_countloc=(/nxcase,ny_layout_len(nio)+1,1,1/)
                allocate(u2d_layout(nxcase,ny_layout_len(nio)+1))
                call check( nf90_inq_varid(gfile_loc_layout(nio),'u',u_grd_VarId) )
                iret=nf90_get_var(gfile_loc_layout(nio),u_grd_VarId,u2d_layout,start=u_startloc,count=u_countloc)
                u2d(:,ny_layout_b(nio):ny_layout_e(nio))=u2d_layout(:,1:ny_layout_len(nio))
                if(nio==fv3_io_layout_y-1) u2d(:,ny_layout_e(nio)+1)=u2d_layout(:,ny_layout_len(nio)+1)
                deallocate(u2d_layout)

                v_countloc=(/nxcase+1,ny_layout_len(nio),1,1/)
                allocate(v2d_layout(nxcase+1,ny_layout_len(nio)))
                call check( nf90_inq_varid(gfile_loc_layout(nio),'v',v_grd_VarId) )
                iret=nf90_get_var(gfile_loc_layout(nio),v_grd_VarId,v2d_layout,start=v_startloc,count=v_countloc)
                v2d(:,ny_layout_b(nio):ny_layout_e(nio))=v2d_layout
                deallocate(v2d_layout)
             enddo
          else
             call check( nf90_inq_varid(gfile_loc,'u',u_grd_VarId) )
             iret=nf90_get_var(gfile_loc,u_grd_VarId,u2d,start=u_startloc,count=u_countloc)
             call check( nf90_inq_varid(gfile_loc,'v',v_grd_VarId) )
             iret=nf90_get_var(gfile_loc,v_grd_VarId,v2d,start=v_startloc,count=v_countloc)
          endif

          if(.not.grid_reverse_flag) then
             call reverse_grid_r_uv (u2d,nxcase,nycase+1,1)
             call reverse_grid_r_uv (v2d,nxcase+1,nycase,1)
          endif
          call fv3uv2earth(u2d(:,:),v2d(:,:),nxcase,nycase,uc2d,vc2d)

     !    NOTE on transfor to earth u/v:
     !       The u and v before transferring need to be in E-W/N-S grid, which is
     !       defined as reversed grid here because it is revered from map view.
     !
     !       Have set the following flag for grid orientation
     !         grid_reverse_flag=true:  E-W/N-S grid
     !         grid_reverse_flag=false: W-E/S-N grid 
     !
     !       So for preparing the wind transferring, need to reverse the grid
     !       from
     !       W-E/S-N grid to E-W/N-S grid when grid_reverse_flag=false:
     !
     !            if(.not.grid_reverse_flag) call reverse_grid_r_uv
     !
     !       and the last input parameter for fv3_h_to_ll is alway true:
     !
     !
          call fv3_h_to_ll(uc2d,hwork(1,:,:,ilevtot),nxcase,nycase,nloncase,nlatcase,.true.)
          call fv3_h_to_ll(vc2d,hwork(2,:,:,ilevtot),nxcase,nycase,nloncase,nlatcase,.true.)
       enddo ! ilevtot
       if(fv3_io_layout_y > 1) then
          do nio=0,fv3_io_layout_y-1
             iret=nf90_close(gfile_loc_layout(nio))
          enddo
          deallocate(gfile_loc_layout)
       else
          iret=nf90_close(gfile_loc)
       endif
       deallocate(u2d,v2d,uc2d,vc2d)
       ges_u = hwork(1,:,:,:)
       ges_v = hwork(2,:,:,:)
    end if ! mype

end subroutine gsi_fv3ncdf_readuv_ens_parallel_over_ens


subroutine wrfv3_netcdf(fv3filenamegin)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    wrfv3_netcdf           write out FV3 analysis increments
!   prgmmr: wu               org: np22                date: 2017-10-23
!
! abstract:  write FV3 analysis  in netcdf format
!
! program history log:
!   2019-04-18  CAPS(C. Tong) - import direct reflectivity DA capabilities
!   2019-11-22  CAPS(C. Tong) - modify "add_saved" to properly output analyses
!   2021-01-05  x.zhang/lei  - add code for updating delz analysis in regional da
!   2022-04-01  Y. Wang and X. Wang - add code for updating reflectivity
!   2023-07-30  Zhao         - added code for the output of the analysis of
!                                significant wave height (howv)
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use kinds, only: r_kind,i_kind
    use guess_grids, only: ntguessig,ges_tsen
    use gsi_metguess_mod, only: gsi_metguess_bundle
    use gsi_chemguess_mod, only: gsi_chemguess_bundle
    use gsi_bundlemod, only: gsi_bundlegetpointer,gsi_bundleputvar
    use mpeu_util, only: die
    use gridmod, only: lat2,lon2,nsig

    use gridmod,only: l_reg_update_hydro_delz
    use guess_grids, only:geom_hgti,geom_hgti_bg

    use directDA_radaruse_mod, only: l_use_cvpqx, cvpqx_pval, cld_nt_updt
    use directDA_radaruse_mod, only: l_use_dbz_directDA
    use directDA_radaruse_mod, only: l_cvpnr, cvpnr_pval
    use gridmod,  only: eta1_ll,eta2_ll
    use constants, only: one
    use obsmod, only: if_model_dbz,if_model_fed


    implicit none
    type (type_fv3regfilenameg),intent(in) :: fv3filenamegin

! Declare local constants
    logical add_saved
 ! variables for cloud info
    integer(i_kind) ier,istatus,it
    real(r_kind),pointer,dimension(:,:  ):: ges_ps  =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_u   =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_v   =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_q   =>NULL()
    real(r_kind),pointer,dimension(:,:  ):: ges_t2m =>NULL()
    real(r_kind),pointer,dimension(:,:  ):: ges_q2m  =>NULL()
    real(r_kind),pointer,dimension(:,:  ):: ges_howv =>NULL()
    real(r_kind),pointer,dimension(:,:  ):: ges_gust =>NULL()
   
    integer(i_kind) i,k

    real(r_kind),pointer,dimension(:,:,:):: ges_ql  =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_qi  =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_qr  =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_qs  =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_qg  =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_qnr =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_w   =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_dbz   =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_fed   =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_delzinc   =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_delp  =>NULL()
    real(r_kind),dimension(:,:  ),allocatable:: ges_ps_write


    real(r_kind), dimension(lat2,lon2,nsig) :: io_arr_qr, io_arr_qs
    real(r_kind), dimension(lat2,lon2,nsig) :: io_arr_qg, io_arr_qnr
    real(r_kind), dimension(:,:,:),allocatable ::g_prsi 


    real(r_kind),dimension(:,:,:),pointer::ges_aalj=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_acaj=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_acli=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_aclj=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_aclk=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_acors=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_aeci=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_aecj=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_afej=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_aivpo1j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_akj=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_alvoo1i=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_alvoo2i=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_alvpo1i=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_alvpo1j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_amgj=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_amnj=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_anai=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_anaj=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_anh4i=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_anh4j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_anh4k=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_ano3i=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_ano3j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_ano3k=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_aothri=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_aothrj=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_aseacat=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_asij=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_aso4i=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_aso4j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_aso4k=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_asoil=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_asvoo1i=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_asvoo2i=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_asvpo1i=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_asvpo1j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_asvpo2i=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_asvpo2j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_asvpo3j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_atij=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_atol1j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_atol2j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_atol3j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_abnz1j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_abnz2j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_abnz3j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_aiso1j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_aiso2j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_aiso3j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_atrp1j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_atrp2j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_asqtj=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_aalk1j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_aalk2j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_apah1j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_apah2j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_apah3j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_aorgcj=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_aolgbj=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_aolgaj=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_alvoo1j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_alvoo2j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_asvoo1j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_asvoo2j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_asvoo3j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_apcsoj=>NULL()

    real(r_kind),dimension(:,:,:),pointer::ges_axyl1j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_axyl2j=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_axyl3j=>NULL()


    it=ntguessig
    ier=0
    call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'ps' ,ges_ps ,istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'u' , ges_u ,istatus);ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'v' , ges_v ,istatus);ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'q'  ,ges_q ,istatus);ier=ier+istatus
    if (l_use_dbz_directDA .or. if_model_dbz .or.if_model_fed) then
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'ql' ,ges_ql ,istatus);ier=ier+istatus
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qi' ,ges_qi ,istatus);ier=ier+istatus
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qr' ,ges_qr ,istatus);ier=ier+istatus
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qs' ,ges_qs ,istatus);ier=ier+istatus
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qg' ,ges_qg ,istatus);ier=ier+istatus
       if (l_use_dbz_directDA) &
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qnr',ges_qnr,istatus);ier=ier+istatus
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'w' , ges_w ,istatus);ier=ier+istatus
       if( if_model_dbz )&
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'dbz' , ges_dbz ,istatus);ier=ier+istatus
       if( if_model_fed )&
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'fed' , ges_fed ,istatus);ier=ier+istatus
    end if
    if(i_use_2mq4b > 0 .and. i_use_2mt4b > 0 ) then
       call GSI_BundleGetPointer (GSI_MetGuess_Bundle(it),'q2m',ges_q2m,istatus); ier=ier+istatus
       call GSI_BundleGetPointer (GSI_MetGuess_Bundle(it),'t2m',ges_t2m,istatus );ier=ier+istatus
    endif
    if ( i_howv_3dda == 1 ) then
       call GSI_BundleGetPointer (GSI_MetGuess_Bundle(it),'howv',ges_howv,istatus); ier=ier+istatus
    endif
    if ( i_gust_3dda == 1 ) then
       call GSI_BundleGetPointer (GSI_MetGuess_Bundle(it),'gust',ges_gust,istatus); ier=ier+istatus
    endif
    if (ier/=0) call die('wrfv3_netcdf','cannot get pointers for fv3 met-fields, ier =',ier)

    if (laeroana_fv3cmaq) then
      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'aalj',ges_aalj,istatus );ier=ier+istatus
      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'acaj',ges_acaj,istatus );ier=ier+istatus
      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'acli',ges_acli,istatus );ier=ier+istatus
      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'aclj',ges_aclj,istatus );ier=ier+istatus
      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'aclk',ges_aclk,istatus );ier=ier+istatus

      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'acors',ges_acors,istatus );ier=ier+istatus
      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'aeci',ges_aeci,istatus );ier=ier+istatus
      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'aecj',ges_aecj,istatus );ier=ier+istatus
      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'afej',ges_afej,istatus );ier=ier+istatus
      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'aivpo1j',ges_aivpo1j,istatus );ier=ier+istatus

      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'akj',ges_akj,istatus );ier=ier+istatus
      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'alvoo1i',ges_alvoo1i,istatus );ier=ier+istatus
      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'alvoo2i',ges_alvoo2i,istatus );ier=ier+istatus
      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'alvpo1i',ges_alvpo1i,istatus );ier=ier+istatus
      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'alvpo1j',ges_alvpo1j,istatus );ier=ier+istatus

      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'amgj',ges_amgj,istatus );ier=ier+istatus
      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'amnj',ges_amnj,istatus );ier=ier+istatus
      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'anai',ges_anai,istatus );ier=ier+istatus
      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'anaj',ges_anaj,istatus );ier=ier+istatus

      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'anh4i',ges_anh4i,istatus );ier=ier+istatus
      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'anh4j',ges_anh4j,istatus );ier=ier+istatus
      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'anh4k',ges_anh4k,istatus );ier=ier+istatus

      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'ano3i',ges_ano3i,istatus );ier=ier+istatus
      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'ano3j',ges_ano3j,istatus );ier=ier+istatus
      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'ano3k',ges_ano3k,istatus );ier=ier+istatus

      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'aothri',ges_aothri,istatus );ier=ier+istatus
      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'aothrj',ges_aothrj,istatus );ier=ier+istatus
      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'aseacat',ges_aseacat,istatus );ier=ier+istatus
      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'asij',ges_asij,istatus );ier=ier+istatus

      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'aso4i',ges_aso4i,istatus );ier=ier+istatus
      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'aso4j',ges_aso4j,istatus );ier=ier+istatus
      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'aso4k',ges_aso4k,istatus );ier=ier+istatus

      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'asoil',ges_asoil,istatus );ier=ier+istatus

      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'asvoo1i',ges_asvoo1i,istatus );ier=ier+istatus
      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'asvoo2i',ges_asvoo2i,istatus );ier=ier+istatus

      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'asvpo1i',ges_asvpo1i,istatus );ier=ier+istatus
      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'asvpo1j',ges_asvpo1j,istatus );ier=ier+istatus
      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'asvpo2i',ges_asvpo2i,istatus );ier=ier+istatus
      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'asvpo2j',ges_asvpo2j,istatus );ier=ier+istatus
      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'asvpo3j',ges_asvpo3j,istatus );ier=ier+istatus


      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'atij',ges_atij,istatus );ier=ier+istatus
      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'atol1j',ges_atol1j,istatus );ier=ier+istatus
      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'atol2j',ges_atol2j,istatus );ier=ier+istatus
      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'atol3j',ges_atol3j,istatus );ier=ier+istatus

      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'abnz1j',ges_abnz1j,istatus );ier=ier+istatus
      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'abnz2j',ges_abnz2j,istatus );ier=ier+istatus
      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'abnz3j',ges_abnz3j,istatus );ier=ier+istatus

      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'aiso1j',ges_aiso1j,istatus );ier=ier+istatus
      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'aiso2j',ges_aiso2j,istatus );ier=ier+istatus
      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'aiso3j',ges_aiso3j,istatus );ier=ier+istatus

      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'atrp1j',ges_atrp1j,istatus );ier=ier+istatus
      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'atrp2j',ges_atrp2j,istatus );ier=ier+istatus
      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'asqtj' ,ges_asqtj,istatus );ier=ier+istatus

      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'aalk1j',ges_aalk1j,istatus );ier=ier+istatus
      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'aalk2j',ges_aalk2j,istatus );ier=ier+istatus

      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'apah1j',ges_apah1j,istatus );ier=ier+istatus
      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'apah2j',ges_apah2j,istatus );ier=ier+istatus
      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'apah3j',ges_apah3j,istatus );ier=ier+istatus

      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'aorgcj',ges_aorgcj,istatus );ier=ier+istatus
      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'aolgbj',ges_aolgbj,istatus );ier=ier+istatus
      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'aolgaj',ges_aolgaj,istatus );ier=ier+istatus

      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'alvoo1j',ges_alvoo1j,istatus );ier=ier+istatus
      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'alvoo2j',ges_alvoo2j,istatus );ier=ier+istatus

      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'asvoo1j',ges_asvoo1j,istatus );ier=ier+istatus
      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'asvoo2j',ges_asvoo2j,istatus );ier=ier+istatus
      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'asvoo3j',ges_asvoo3j,istatus );ier=ier+istatus

      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'apcsoj',ges_apcsoj,istatus );ier=ier+istatus

      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'axyl1j',ges_axyl1j,istatus );ier=ier+istatus
      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'axyl2j',ges_axyl2j,istatus );ier=ier+istatus
      call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'axyl3j',ges_axyl3j,istatus );ier=ier+istatus
    end if

    if(l_reg_update_hydro_delz) then
      allocate(ges_delzinc(lat2,lon2,nsig))
      do k=1,nsig
        ges_delzinc(:,:,k)=geom_hgti(:,:,k+1,it)-geom_hgti_bg(:,:,k+1,it)-geom_hgti(:,:,k,it)+geom_hgti_bg(:,:,k,it)
      enddo
      call gsi_bundleputvar (gsibundle_fv3lam_dynvar_nouv,'delzinc',ges_delzinc,istatus)
    endif
! additional I/O for direct reflectivity DA capabilities
    if (l_use_dbz_directDA) then
      if( fv3sar_bg_opt == 0) then
        write(6,*) "FV3 IO Write for 'fv3sar_bg_opt == 0 is only available for now in direct relfectivity DA"
      endif
 
      io_arr_qr=ges_qr
      io_arr_qs=ges_qs
      io_arr_qg=ges_qg
      io_arr_qnr=ges_qnr

      call convert_cvpqx_to_qx(io_arr_qr, io_arr_qs, io_arr_qg, l_use_cvpqx, cvpqx_pval)  ! Convert Qx back
      call convert_cvpnx_to_nx(io_arr_qnr, l_cvpnr, cvpnr_pval, cld_nt_updt, ges_q, io_arr_qr, ges_ps) ! Convert Nx back 
      ges_qr=io_arr_qr
      ges_qs=io_arr_qs
      ges_qg=io_arr_qg
      ges_qnr=io_arr_qnr
    endif

    call gsi_copy_bundle(GSI_MetGuess_Bundle(it),gsibundle_fv3lam_dynvar_nouv) 
    call gsi_copy_bundle(GSI_MetGuess_Bundle(it),gsibundle_fv3lam_tracer_nouv) 
    if( if_model_dbz .or. if_model_fed )  call gsi_copy_bundle(GSI_MetGuess_Bundle(it),gsibundle_fv3lam_phyvar_nouv)
    if (laeroana_fv3cmaq) then
      call gsi_copy_bundle(GSI_ChemGuess_Bundle(it),gsibundle_fv3lam_tracerchem_nouv)
    end if
    if (laeroana_fv3smoke) then
      call gsi_copy_bundle(GSI_ChemGuess_Bundle(it),gsibundle_fv3lam_tracersmoke_nouv)
    end if

    call gsi_bundleputvar (gsibundle_fv3lam_dynvar_nouv,'tsen',ges_tsen(:,:,:,it),istatus)

    if( fv3sar_bg_opt == 0) then
      call GSI_BundleGetPointer ( gsibundle_fv3lam_dynvar_nouv, 'delp'  ,ges_delp ,istatus );ier=ier+istatus
      allocate(g_prsi(lat2,lon2,nsig+1))
      allocate(ges_ps_inc(lat2,lon2))
      ges_ps_inc=ges_ps-ges_ps_bg 
      g_prsi(:,:,nsig+1)=eta1_ll(nsig+1)
      do i=nsig,1,-1
        g_prsi(:,:,i)=ges_delp_bg(:,:,i)+g_prsi(:,:,i+1) 
      enddo
      do i=1,nsig+1
        g_prsi(:,:,i)=g_prsi(:,:,i)+eta2_ll(i)*ges_ps_inc
      enddo
      do i=1,nsig
        ges_delp(:,:,i)=g_prsi(:,:,i)-g_prsi(:,:,i+1)
      enddo
      ges_delp=ges_delp*1000.0_r_kind
      deallocate(g_prsi,ges_ps_inc)
    
    else
      allocate(ges_ps_write(lat2,lon2))
      ges_ps_write=ges_ps*1000.0_r_kind
      call gsi_bundleputvar (gsibundle_fv3lam_dynvar_nouv,'ps',ges_ps_write,istatus)
      deallocate(ges_ps_write)
    endif
!   write out
    if (ier/=0) call die('get ges','cannot get pointers for fv3 met-fields, ier =',ier)

    if(i_use_2mq4b > 0 .and. i_use_2mt4b > 0 ) then
! Convert 2m guess specific humidity to mixing ratio
      ges_q2m = ges_q2m/(one-ges_q2m)
    endif

    add_saved=.true.
!   write out
    if(fv3sar_bg_opt == 0) then
      call gsi_fv3ncdf_write(grd_fv3lam_dynvar_ionouv,gsibundle_fv3lam_dynvar_nouv,&
                             add_saved,fv3filenamegin%dynvars,fv3filenamegin)
      call gsi_fv3ncdf_write(grd_fv3lam_tracer_ionouv,gsibundle_fv3lam_tracer_nouv, &
                             add_saved,fv3filenamegin%tracers,fv3filenamegin)
      if( if_model_dbz .or. if_model_fed ) then
         call gsi_fv3ncdf_write(grd_fv3lam_phyvar_ionouv,gsibundle_fv3lam_phyvar_nouv,&
                                add_saved,fv3filenamegin%phyvars,fv3filenamegin)
      end if

      call gsi_fv3ncdf_writeuv(grd_fv3lam_uv,ges_u,ges_v,add_saved,fv3filenamegin)
      if (laeroana_fv3cmaq) then
        call gsi_fv3ncdf_write(grd_fv3lam_tracerchem_ionouv,gsibundle_fv3lam_tracerchem_nouv, &
                             add_saved,fv3filenamegin%tracers,fv3filenamegin)
      endif
      if (laeroana_fv3smoke) then
        call gsi_fv3ncdf_write(grd_fv3lam_tracersmoke_ionouv,gsibundle_fv3lam_tracersmoke_nouv,&
                             add_saved,fv3filenamegin%tracers,fv3filenamegin)
      endif

    else
      call gsi_fv3ncdf_write_v1(grd_fv3lam_dynvar_ionouv,gsibundle_fv3lam_dynvar_nouv,& 
                                add_saved,fv3filenamegin%dynvars,fv3filenamegin)
      call gsi_fv3ncdf_write_v1(grd_fv3lam_tracer_ionouv,gsibundle_fv3lam_tracer_nouv,&
                                add_saved,fv3filenamegin%tracers,fv3filenamegin)
      call gsi_fv3ncdf_writeuv_v1(grd_fv3lam_uv,ges_u,ges_v,add_saved,fv3filenamegin)
      if (laeroana_fv3cmaq) then
        call gsi_fv3ncdf_write_v1(grd_fv3lam_tracerchem_ionouv,gsibundle_fv3lam_tracerchem_nouv,&
                                add_saved,fv3filenamegin%tracers,fv3filenamegin)
      endif
      if (laeroana_fv3smoke) then
        call gsi_fv3ncdf_write_v1(grd_fv3lam_tracersmoke_ionouv,gsibundle_fv3lam_tracersmoke_nouv,&
                                add_saved,fv3filenamegin%tracers,fv3filenamegin)
      endif

    endif

    if(i_use_2mq4b > 0 .and. i_use_2mt4b > 0 ) then
      call gsi_fv3ncdf_write_sfc(fv3filenamegin,'t2m',ges_t2m,add_saved)
      call gsi_fv3ncdf_write_sfc(fv3filenamegin,'q2m',ges_q2m,add_saved)
    endif
!-- output analysis of howv
    if ( i_howv_3dda == 1 ) then
      call gsi_fv3ncdf_write_sfc(fv3filenamegin,'howv',ges_howv,add_saved)
    endif
!-- output analysis of gust
    if ( i_gust_3dda == 1 ) then
      call gsi_fv3ncdf_write_sfc(fv3filenamegin,'gust',ges_gust,add_saved)
    endif

    if(allocated(g_prsi)) deallocate(g_prsi)

    deallocate(ny_layout_len)
    deallocate(ny_layout_b)
    deallocate(ny_layout_e)
! additional I/O for direct reflectivity DA capabilities
    
end subroutine wrfv3_netcdf

subroutine gsi_fv3ncdf_writeuv(grd_uv,ges_u,ges_v,add_saved,fv3filenamegin)
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    gsi_nemsio_writeuv
!   pgrmmr: wu
!
! abstract: gather u/v fields to mype_io, put u/v in FV3 model defined directions & orders
!           then write out
!
! program history log:
!
!   input argument list:
!    varu,varv
!    add_saved - true: add analysis increments to readin guess then write out
!              - false: write out total analysis fields
!    mype_io
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    use mpimod, only:  mpi_rtype,mpi_comm_world,mype,mpi_info_null,npe,setcomm,mpi_integer,mpi_max
    use netcdf, only: nf90_nowrite,nf90_inquire,nf90_inquire_dimension
    use gridmod, only: nlon_regional,nlat_regional
    use mod_fv3_lola, only: fv3_ll_to_h,fv3_h_to_ll, &
                            fv3uv2earth,earthuv2fv3
    use netcdf, only: nf90_open,nf90_close,nf90_noerr
    use netcdf, only: nf90_write,nf90_mpiio,nf90_inq_varid,nf90_var_par_access,nf90_collective
    use netcdf, only: nf90_put_var,nf90_get_var
    use general_sub2grid_mod, only: sub2grid_info,general_sub2grid

    implicit none
    type(sub2grid_info), intent(in):: grd_uv 

    logical,                    intent(in   ) :: add_saved
    type (type_fv3regfilenameg),intent(in) ::fv3filenamegin
    real(r_kind),dimension(grd_uv%lat2,grd_uv%lon2,grd_uv%nsig),intent(inout)::ges_u
    real(r_kind),dimension(grd_uv%lat2,grd_uv%lon2,grd_uv%nsig),intent(inout)::ges_v

    real(r_kind),dimension(2,grd_uv%nlat,grd_uv%nlon,grd_uv%kbegin_loc:grd_uv%kend_alloc):: hwork
    integer(i_kind) :: ugrd_VarId,gfile_loc,vgrd_VarId
    integer(i_kind) i,j,mm1,k,nzp1
    integer(i_kind) kbgn,kend
    integer(i_kind) inative,ilev,ilevtot
    integer(i_kind) nlatcase,nloncase
    integer(i_kind) nxcase,nycase
    integer(i_kind) u_countloc(4),u_startloc(4),v_countloc(4),v_startloc(4)
    character(:),allocatable:: filenamein ,varname
    real(r_kind),allocatable,dimension(:,:,:,:):: worksub
    real(r_kind),allocatable,dimension(:,:):: work_au,work_av
    real(r_kind),allocatable,dimension(:,:,:):: work_bu,work_bv
    real(r_kind),allocatable,dimension(:,:):: u2d,v2d,workau2,workav2
    real(r_kind),allocatable,dimension(:,:):: workbu2,workbv2

    integer(i_kind):: iworld,iworld_group,nread,mpi_comm_read,ierror
    integer(i_kind),dimension(npe):: members,members_read,mype_read_rank
    logical:: procuse

! for fv3_io_layout_y > 1
    real(r_kind),allocatable,dimension(:,:,:):: u2d_layout,v2d_layout
    integer(i_kind) :: nio
    integer(i_kind),allocatable :: gfile_loc_layout(:)
    character(len=180)  :: filename_layout
    integer(i_kind):: kend_native,kbgn_native
    integer(i_kind):: istat

    mm1=mype+1
    
    nloncase=grd_uv%nlon
    nlatcase=grd_uv%nlat
    nxcase=nx
    nycase=ny
    kbgn=grd_uv%kbegin_loc
    kend=grd_uv%kend_loc
    allocate( u2d(nlon_regional,nlat_regional+1))
    allocate( v2d(nlon_regional+1,nlat_regional))
    allocate (worksub(2,grd_uv%lat2,grd_uv%lon2,grd_uv%nsig))
    allocate( work_au(nlatcase,nloncase),work_av(nlatcase,nloncase))
    do k=1,grd_uv%nsig
       do j=1,grd_uv%lon2
          do i=1,grd_uv%lat2
             worksub(1,i,j,k)=ges_u(i,j,k)
             worksub(2,i,j,k)=ges_v(i,j,k)
          end do
       end do
    end do
    call general_sub2grid(grd_uv,worksub,hwork)
    filenamein=fv3filenamegin%dynvars


    procuse = .false.
    members=-1
    members_read=-1
    if (kbgn<=kend) then
       procuse = .true.
       members(mm1) = mype
    endif

    call mpi_allreduce(members,members_read,npe,mpi_integer,mpi_max,mpi_comm_world,ierror)

    nread=0
    mype_read_rank=-1
    do i=1,npe
       if (members_read(i) >= 0) then
          nread=nread+1
          mype_read_rank(nread) = members_read(i)
       endif
    enddo

    call setcomm(iworld,iworld_group,nread,mype_read_rank,mpi_comm_read,ierror)

    if (procuse) then
       if(fv3_io_layout_y > 1) then
          allocate(gfile_loc_layout(0:fv3_io_layout_y-1))
          do nio=0,fv3_io_layout_y-1
             write(filename_layout,'(a,a,I4.4)') trim(filenamein),".",nio
             call check( nf90_open(filename_layout,ior(nf90_write, nf90_mpiio),gfile_loc_layout(nio),comm=mpi_comm_read,info=MPI_INFO_NULL) )
          enddo
          gfile_loc=gfile_loc_layout(0)
       else
          call check( nf90_open(filenamein,ior(nf90_write, nf90_mpiio),gfile_loc,comm=mpi_comm_read,info=MPI_INFO_NULL) )
       endif
       nz=grd_uv%nsig
       nzp1=nz+1
       kend_native=nzp1-grd_uv%lnames(1,kbgn)
       kbgn_native=nzp1-grd_uv%lnames(1,kend)
       allocate( work_bu(nlon_regional,nlat_regional+1,kbgn_native:kend_native))
       allocate( work_bv(nlon_regional+1,nlat_regional,kbgn_native:kend_native))
       u_startloc=(/1,1,kbgn_native,1/)
       u_countloc=(/nxcase,nycase+1,kend_native-kbgn_native+1,1/)
       v_startloc=(/1,1,kbgn_native,1/)
       v_countloc=(/nxcase+1,nycase,kend_native-kbgn_native+1,1/)
       if(fv3_io_layout_y > 1) then
          do nio=0,fv3_io_layout_y-1
             allocate(u2d_layout(nxcase,ny_layout_len(nio)+1,kend_native-kbgn_native+1))
             u_countloc=(/nxcase,ny_layout_len(nio)+1,kend_native-kbgn_native+1,1/)
             call check( nf90_get_var(gfile_loc_layout(nio),ugrd_VarId,u2d_layout,start=u_startloc,count=u_countloc) )
             work_bu(:,ny_layout_b(nio):ny_layout_e(nio),:)=u2d_layout(:,1:ny_layout_len(nio),:)
             if(nio==fv3_io_layout_y-1) work_bu(:,ny_layout_e(nio)+1,:)=u2d_layout(:,ny_layout_len(nio)+1,:)
             deallocate(u2d_layout)
             
             allocate(v2d_layout(nxcase+1,ny_layout_len(nio),kend_native-kbgn_native+1))
             v_countloc=(/nxcase+1,ny_layout_len(nio),kend_native-kbgn_native+1,1/)
             call check( nf90_get_var(gfile_loc_layout(nio),vgrd_VarId,v2d_layout,start=v_startloc,count=v_countloc) )
             work_bv(:,ny_layout_b(nio):ny_layout_e(nio),:)=v2d_layout
             deallocate(v2d_layout)
          enddo
       else
          call check( nf90_inq_varid(gfile_loc,'u',ugrd_VarId) )
          call check( nf90_inq_varid(gfile_loc,'v',vgrd_VarId) )
          call check( nf90_var_par_access(gfile_loc, ugrd_VarId, nf90_collective))
          call check( nf90_var_par_access(gfile_loc, vgrd_VarId, nf90_collective))
          call check( nf90_get_var(gfile_loc,ugrd_VarId,work_bu,start=u_startloc,count=u_countloc) )
          call check( nf90_get_var(gfile_loc,vgrd_VarId,work_bv,start=v_startloc,count=v_countloc) )
       endif


       
       do ilevtot=kbgn,kend
          varname=grd_uv%names(1,ilevtot)
          ilev=grd_uv%lnames(1,ilevtot)
          inative=nzp1-ilev
          
          work_au=hwork(1,:,:,ilevtot)
          work_av=hwork(2,:,:,ilevtot)
          
          
          if(add_saved)then
             allocate( workau2(nlatcase,nloncase),workav2(nlatcase,nloncase))
             allocate( workbu2(nlon_regional,nlat_regional+1))
             allocate( workbv2(nlon_regional+1,nlat_regional))
!!!!!!!!  readin work_b !!!!!!!!!!!!!!!!
 
!clt for fv3_io_layout<=1  now the nf90_get_var has been moved outside of this do loop 
!to avoid failure on hercules when L_MPI_EXTRA_FILESYSTEM=1 
             if(.not.grid_reverse_flag) then
                call reverse_grid_r_uv(work_bu(:,:,inative),nlon_regional,nlat_regional+1,1)
                call reverse_grid_r_uv(work_bv(:,:,inative),nlon_regional+1,nlat_regional,1)
             endif
             call fv3uv2earth(work_bu(:,:,inative),work_bv(:,:,inative),nlon_regional,nlat_regional,u2d,v2d)
             call fv3_h_to_ll(u2d,workau2,nlon_regional,nlat_regional,nloncase,nlatcase,.true.)
             call fv3_h_to_ll(v2d,workav2,nlon_regional,nlat_regional,nloncase,nlatcase,.true.)
!!!!!!!! find analysis_inc:  work_a !!!!!!!!!!!!!!!!
             work_au(:,:)=work_au(:,:)-workau2(:,:)
             work_av(:,:)=work_av(:,:)-workav2(:,:)
             call fv3_ll_to_h(work_au(:,:),u2d,nloncase,nlatcase,nlon_regional,nlat_regional,.true.)
             call fv3_ll_to_h(work_av(:,:),v2d,nloncase,nlatcase,nlon_regional,nlat_regional,.true.)
             call earthuv2fv3(u2d,v2d,nlon_regional,nlat_regional,workbu2,workbv2)
!!!!!!!!  add analysis_inc to readin work_b !!!!!!!!!!!!!!!!
             work_bu(:,:,inative)=work_bu(:,:,inative)+workbu2(:,:)
             work_bv(:,:,inative)=work_bv(:,:,inative)+workbv2(:,:)
             deallocate(workau2,workbu2,workav2,workbv2)
          else
             call fv3_ll_to_h(work_au(:,:),u2d,nloncase,nlatcase,nlon_regional,nlat_regional,.true.)
             call fv3_ll_to_h(work_av(:,:),v2d,nloncase,nlatcase,nlon_regional,nlat_regional,.true.)
             call earthuv2fv3(u2d,v2d,nlon_regional,nlat_regional,work_bu(:,:,inative),work_bv(:,:,inative))
          endif
          if(.not.grid_reverse_flag) then
             call reverse_grid_r_uv(work_bu(:,:,inative),nlon_regional,nlat_regional+1,1)
             call reverse_grid_r_uv(work_bv(:,:,inative),nlon_regional+1,nlat_regional,1)
          endif
       enddo !ilevltot
          
       if(fv3_io_layout_y > 1) then
             do nio=0,fv3_io_layout_y-1
                allocate(u2d_layout(nxcase,ny_layout_len(nio)+1,kend_native-kbgn_native+1))
                u_countloc=(/nxcase,ny_layout_len(nio)+1,kend_native-kbgn_native+1,1/)
                u2d_layout=work_bu(:,ny_layout_b(nio):ny_layout_e(nio)+1,:)
                call check( nf90_put_var(gfile_loc_layout(nio),ugrd_VarId,u2d_layout,start=u_startloc,count=u_countloc) )
                deallocate(u2d_layout)
                
                allocate(v2d_layout(nxcase+1,ny_layout_len(nio),kend_native-kbgn_native+1))
                v_countloc=(/nxcase+1,ny_layout_len(nio),kend_native-kbgn_native+1,1/)
                v2d_layout=work_bv(:,ny_layout_b(nio):ny_layout_e(nio),:)
                call check( nf90_put_var(gfile_loc_layout(nio),vgrd_VarId,v2d_layout,start=v_startloc,count=v_countloc) )
                deallocate(v2d_layout)
             enddo
       else
             call check( nf90_put_var(gfile_loc,ugrd_VarId,work_bu,start=u_startloc,count=u_countloc) )
             call check( nf90_put_var(gfile_loc,vgrd_VarId,work_bv,start=v_startloc,count=v_countloc) )
       endif

       if(fv3_io_layout_y > 1) then
          do nio=0,fv3_io_layout_y-1
             call check( nf90_close(gfile_loc_layout(nio)) )
          enddo
          deallocate(gfile_loc_layout)
       else
          call check( nf90_close(gfile_loc) )
       endif
     deallocate(work_bu,work_bv)
    endif

    call mpi_barrier(mpi_comm_world,ierror)
 
    deallocate(u2d,v2d)
    deallocate(work_au,work_av)

end subroutine gsi_fv3ncdf_writeuv
subroutine gsi_fv3ncdf_writeuv_v1(grd_uv,ges_u,ges_v,add_saved,fv3filenamegin)
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    gsi_nemsio_writeuv
!   pgrmmr: wu
!
! abstract: gather u/v fields to mype_io, put u/v in FV3 model defined directions & orders
!           then write out
!
! program history log:
! 2019-04-22  lei   modified from gsi_nemsio_writeuv_v1 for update
! u_w,v_w,u_s,v_s in the cold start files!
! 2020-03-06  lei   added ilev0 fix
!   input argument list:
!    varu,varv
!    add_saved - true: add analysis increments to readin guess then write out
!              - false: write out total analysis fields
!    mype_io
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    use constants, only: half,zero
    use mpimod, only:  npe, setcomm,mpi_integer,mpi_max,mpi_rtype,mpi_comm_world,mype,mpi_info_null
    use gridmod, only: nlon_regional,nlat_regional
    use mod_fv3_lola, only: fv3_ll_to_h,fv3_h_to_ll, &
                            fv3uv2earth,earthuv2fv3
    use netcdf, only: nf90_open,nf90_close,nf90_noerr
    use netcdf, only: nf90_write, nf90_mpiio,nf90_inq_varid,nf90_var_par_access,nf90_collective
    use netcdf, only: nf90_put_var,nf90_get_var
    use general_sub2grid_mod, only: sub2grid_info,general_sub2grid
    implicit none
    type(sub2grid_info),        intent(in) :: grd_uv 
    real(r_kind),dimension(grd_uv%lat2,grd_uv%lon2,grd_uv%nsig),intent(inout)::ges_u
    real(r_kind),dimension(grd_uv%lat2,grd_uv%lon2,grd_uv%nsig),intent(inout)::ges_v
    logical,                    intent(in) :: add_saved
    type (type_fv3regfilenameg),intent(in) :: fv3filenamegin

    real(r_kind),dimension(2,grd_uv%nlat,grd_uv%nlon,grd_uv%kbegin_loc:grd_uv%kend_alloc):: hwork
    character(len=:),allocatable :: filenamein
    character(len=max_varname_length) :: varname

    integer(i_kind) :: gfile_loc
    integer(i_kind) :: u_wgrd_VarId,v_wgrd_VarId
    integer(i_kind) :: u_sgrd_VarId,v_sgrd_VarId
    integer(i_kind) i,j,mm1,k,nzp1
    integer(i_kind) kbgn,kend
    integer(i_kind) inative,ilev,ilevtot
    real(r_kind),allocatable,dimension(:,:,:,:):: worksub
    real(r_kind),allocatable,dimension(:,:):: work_au,work_av
    real(r_kind),allocatable,dimension(:,:,:):: work_bu_s,work_bv_s
    real(r_kind),allocatable,dimension(:,:,:):: work_bu_w,work_bv_w
    real(r_kind),allocatable,dimension(:,:):: u2d,v2d,workau2,workav2
    real(r_kind),allocatable,dimension(:,:):: workbu_s2,workbv_s2
    real(r_kind),allocatable,dimension(:,:):: workbu_w2,workbv_w2
    integer(i_kind) nlatcase,nloncase,nxcase,nycase
    integer(i_kind) uw_countloc(4),us_countloc(4),uw_startloc(4),us_startloc(4)
    integer(i_kind) vw_countloc(4),vs_countloc(4),vw_startloc(4),vs_startloc(4)
    integer(i_kind):: kend_native,kbgn_native,kdim_native


    integer(i_kind):: iworld,iworld_group,nread,mpi_comm_read,ierror
    integer(i_kind),dimension(npe):: members,members_read,mype_read_rank
    logical:: procuse

    mm1=mype+1
    nloncase=grd_uv%nlon
    nlatcase=grd_uv%nlat
    nxcase=nx
    nycase=ny
    kbgn=grd_uv%kbegin_loc
    kend=grd_uv%kend_loc
    allocate (worksub(2,grd_uv%lat2,grd_uv%lon2,grd_uv%nsig))
    do k=1,grd_uv%nsig
       do j=1,grd_uv%lon2
          do i=1,grd_uv%lat2
             worksub(1,i,j,k)=ges_u(i,j,k)
             worksub(2,i,j,k)=ges_v(i,j,k)
          end do
       end do
    end do
    call general_sub2grid(grd_uv,worksub,hwork)

    allocate( u2d(nlon_regional,nlat_regional)) 
    allocate( v2d(nlon_regional,nlat_regional))
    allocate( work_au(nlatcase,nloncase),work_av(nlatcase,nloncase))

    if(add_saved) allocate( workau2(nlatcase,nloncase),workav2(nlatcase,nloncase))
    allocate( workbu_w2(nlon_regional+1,nlat_regional))
    allocate( workbv_w2(nlon_regional+1,nlat_regional))
    allocate( workbu_s2(nlon_regional,nlat_regional+1))
    allocate( workbv_s2(nlon_regional,nlat_regional+1))
    filenamein=fv3filenamegin%dynvars


    procuse = .false.
    members=-1
    members_read=-1
    if (kbgn<=kend) then
       procuse = .true.
       members(mm1) = mype
    endif

    call mpi_allreduce(members,members_read,npe,mpi_integer,mpi_max,mpi_comm_world,ierror)

    nread=0
    mype_read_rank=-1
    do i=1,npe
       if (members_read(i) >= 0) then
          nread=nread+1
          mype_read_rank(nread) = members_read(i)
       endif
    enddo

    call setcomm(iworld,iworld_group,nread,mype_read_rank,mpi_comm_read,ierror)

    if (procuse) then





    call check( nf90_open(filenamein,ior(nf90_write, nf90_mpiio),gfile_loc,comm=mpi_comm_read,info=MPI_INFO_NULL) )

    call check( nf90_inq_varid(gfile_loc,'u_s',u_sgrd_VarId) )
    call check( nf90_var_par_access(gfile_loc, u_sgrd_VarId, nf90_collective))
    call check( nf90_inq_varid(gfile_loc,'u_w',u_wgrd_VarId) )
    call check( nf90_var_par_access(gfile_loc, u_wgrd_VarId, nf90_collective))
    call check( nf90_inq_varid(gfile_loc,'v_s',v_sgrd_VarId) )
    call check( nf90_var_par_access(gfile_loc, v_sgrd_VarId, nf90_collective))
    call check( nf90_inq_varid(gfile_loc,'v_w',v_wgrd_VarId) )
    call check( nf90_var_par_access(gfile_loc, v_wgrd_VarId, nf90_collective))
    nz=grd_uv%nsig
    nzp1=nz+1
    kend_native=nzp1-grd_uv%lnames(1,kbgn)
    kbgn_native=nzp1-grd_uv%lnames(1,kend)
    kdim_native=kend_native-kbgn_native+1

    uw_countloc= (/nlon_regional+1,nlat_regional,kdim_native,1/)
    us_countloc= (/nlon_regional,nlat_regional+1,kdim_native,1/)
    vw_countloc= (/nlon_regional+1,nlat_regional,kdim_native,1/)
    vs_countloc= (/nlon_regional,nlat_regional+1,kdim_native,1/)
      
    uw_startloc=(/1,1,kbgn_native+1,1/)  !In the coldstart files, there is an extra top level 
    us_startloc=(/1,1,kbgn_native+1,1/)
    vw_startloc=(/1,1,kbgn_native+1,1/)
    vs_startloc=(/1,1,kbgn_native+1,1/)
    allocate( work_bu_s(nlon_regional,nlat_regional+1,kbgn_native:kend_native))
    allocate( work_bv_s(nlon_regional,nlat_regional+1,kbgn_native:kend_native))
    allocate( work_bu_w(nlon_regional+1,nlat_regional,kbgn_native:kend_native))
    allocate( work_bv_w(nlon_regional+1,nlat_regional,kbgn_native:kend_native))

!!!!!!!!  readin work_b !!!!!!!!!!!!!!!!
    call check( nf90_get_var(gfile_loc,u_sgrd_VarId,work_bu_s,start=us_startloc,count=us_countloc) )
    call check( nf90_get_var(gfile_loc,u_wgrd_VarId,work_bu_w,start=uw_startloc,count=uw_countloc) )
    call check( nf90_get_var(gfile_loc,v_sgrd_VarId,work_bv_s,start=vs_startloc,count=vs_countloc) )
    call check( nf90_get_var(gfile_loc,v_wgrd_VarId,work_bv_w,start=vw_startloc,count=vw_countloc) )
    do ilevtot=kbgn,kend
      varname=grd_uv%names(1,ilevtot)
      ilev=grd_uv%lnames(1,ilevtot)
      inative=nzp1-ilev

      work_au=hwork(1,:,:,ilevtot)
      work_av=hwork(2,:,:,ilevtot)





      if(add_saved)then
        do j=1,nlat_regional
          u2d(:,j)=half * (work_bu_s(:,j,inative)+ work_bu_s(:,j+1,inative))
        enddo
        do i=1,nlon_regional
          v2d(i,:)=half*(work_bv_w(i,:,inative)+work_bv_w(i+1,:,inative))
        enddo
        call fv3_h_to_ll(u2d,workau2,nlon_regional,nlat_regional,nloncase,nlatcase,grid_reverse_flag)
        call fv3_h_to_ll(v2d,workav2,nlon_regional,nlat_regional,nloncase,nlatcase,grid_reverse_flag)
!!!!!!!! find analysis_inc:  work_a !!!!!!!!!!!!!!!!
        work_au(:,:)=work_au(:,:)-workau2(:,:)
        work_av(:,:)=work_av(:,:)-workav2(:,:)
        call fv3_ll_to_h(work_au(:,:),u2d,nloncase,nlatcase,nlon_regional,nlat_regional,grid_reverse_flag)
        call fv3_ll_to_h(work_av(:,:),v2d,nloncase,nlatcase,nlon_regional,nlat_regional,grid_reverse_flag)
!!!!!!!!  add analysis_inc to readin work_b !!!!!!!!!!!!!!!!
        do i=2,nlon_regional
          workbu_w2(i,:)=half*(u2d(i-1,:)+u2d(i,:))
          workbv_w2(i,:)=half*(v2d(i-1,:)+v2d(i,:))
        enddo
        workbu_w2(1,:)=u2d(1,:)
        workbv_w2(1,:)=v2d(1,:)
        workbu_w2(nlon_regional+1,:)=u2d(nlon_regional,:)
        workbv_w2(nlon_regional+1,:)=v2d(nlon_regional,:)

        do j=2,nlat_regional
          workbu_s2(:,j)=half*(u2d(:,j-1)+u2d(:,j))
          workbv_s2(:,j)=half*(v2d(:,j-1)+v2d(:,j))
        enddo
        workbu_s2(:,1)=u2d(:,1)
        workbv_s2(:,1)=v2d(:,1)
        workbu_s2(:,nlat_regional+1)=u2d(:,nlat_regional)
        workbv_s2(:,nlat_regional+1)=v2d(:,nlat_regional)



        work_bu_w(:,:,inative)=work_bu_w(:,:,inative)+workbu_w2(:,:)
        work_bu_s(:,:,inative)=work_bu_s(:,:,inative)+workbu_s2(:,:)
        work_bv_w(:,:,inative)=work_bv_w(:,:,inative)+workbv_w2(:,:)
        work_bv_s(:,:,inative)=work_bv_s(:,:,inative)+workbv_s2(:,:)
      else
        call fv3_ll_to_h(work_au(:,:),u2d,nloncase,nlatcase,nlon_regional,nlat_regional,grid_reverse_flag)
        call fv3_ll_to_h(work_av(:,:),v2d,nloncase,nlatcase,nlon_regional,nlat_regional,grid_reverse_flag)

        do i=2,nlon_regional
          work_bu_w(i,:,inative)=half*(u2d(i-1,:)+u2d(i,:))
          work_bv_w(i,:,inative)=half*(v2d(i-1,:)+v2d(i,:))
        enddo
        work_bu_w(1,:,inative)=u2d(1,:)
        work_bv_w(1,:,inative)=v2d(1,:)
        work_bu_w(nlon_regional+1,:,inative)=u2d(nlon_regional,:)
        work_bv_w(nlon_regional+1,:,inative)=v2d(nlon_regional,:)

        do j=2,nlat_regional
          work_bu_s(:,j,inative)=half*(u2d(:,j-1)+u2d(:,j))
          work_bv_s(:,j,inative)=half*(v2d(:,j-1)+v2d(:,j))
        enddo
        work_bu_s(:,1,inative)=u2d(:,1)
        work_bv_s(:,1,inative)=v2d(:,1)
        work_bu_s(:,nlat_regional+1,inative)=u2d(:,nlat_regional)
        work_bv_s(:,nlat_regional+1,inative)=v2d(:,nlat_regional)


      endif
    enddo !

    call check( nf90_put_var(gfile_loc,u_wgrd_VarId,work_bu_w,start=uw_startloc,count=uw_countloc) )
    call check( nf90_put_var(gfile_loc,u_sgrd_VarId,work_bu_s,start=us_startloc,count=us_countloc) )
    call check( nf90_put_var(gfile_loc,v_wgrd_VarId,work_bv_w,start=vw_startloc,count=vw_countloc) )
    call check( nf90_put_var(gfile_loc,v_sgrd_VarId,work_bv_s,start=vs_startloc,count=vs_countloc) )
      
    call check( nf90_close(gfile_loc) )
    deallocate(work_bu_w,work_bv_w)
    deallocate(work_bu_s,work_bv_s)
    endif !procuse

    deallocate(work_au,work_av,u2d,v2d)
    if(add_saved) deallocate(workau2,workav2)
    if (allocated(workbu_w2)) then
      deallocate(workbu_w2,workbv_w2)
      deallocate(workbu_s2,workbv_s2)
    endif

    if(allocated(worksub))deallocate(worksub)

end subroutine gsi_fv3ncdf_writeuv_v1

subroutine gsi_fv3ncdf_write_sfc(fv3filenamegin,varname,var,add_saved)
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    gsi_fv3ncdf_write_sfc
!   pgrmmr: wu
!
! abstract:
!
! program history log:
! 2022-02-25  Hu  write surface fields  
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    use mpimod, only: ierror,mpi_comm_world,mpi_rtype,mype
    use gridmod, only: lat1,lon1,lat2,lon2,nlat,nlon
    use gridmod, only: ijn,displs_g,itotsub,iglobal
    use gridmod,  only: nlon_regional,nlat_regional
    use mod_fv3_lola, only: fv3_ll_to_h,fv3_h_to_ll
    use general_commvars_mod, only: ltosi,ltosj
    use netcdf, only: nf90_open,nf90_close
    use netcdf, only: nf90_write,nf90_inq_varid
    use netcdf, only: nf90_put_var,nf90_get_var
    use gridmod, only: strip
    implicit none

    real(r_kind)   ,intent(in   ) :: var(lat2,lon2)
    logical        ,intent(in   ) :: add_saved
    character(*)   ,intent(in   ) :: varname
    type (type_fv3regfilenameg),intent (in) :: fv3filenamegin

    integer(i_kind) :: VarId,gfile_loc
    integer(i_kind) i,mm1
    real(r_kind),allocatable,dimension(:):: work
    real(r_kind),allocatable,dimension(:,:):: work_sub,work_a
    real(r_kind),allocatable,dimension(:,:):: work_b
    real(r_kind),allocatable,dimension(:,:):: workb2,worka2
    character(len=80)   :: filename

! for io_layout > 1
    real(r_kind),allocatable,dimension(:,:):: work_b_layout
    integer(i_kind) :: nio
    integer(i_kind),allocatable :: gfile_loc_layout(:)
    character(len=180)  :: filename_layout

    filename=trim(fv3filenamegin%sfcdata)

    mm1=mype+1
    allocate(work(max(iglobal,itotsub)),work_sub(lat1,lon1))
    call strip(var,work_sub)
    call mpi_gatherv(work_sub,ijn(mm1),mpi_rtype, &
          work,ijn,displs_g,mpi_rtype,0,mpi_comm_world,ierror)
    deallocate(work_sub)

    if(mype==0) then
       allocate( work_a(nlat,nlon))
       do i=1,iglobal
          work_a(ltosi(i),ltosj(i))=work(i)
       end do
       allocate( work_b(nlon_regional,nlat_regional))


       if(fv3_io_layout_y > 1) then
         allocate(gfile_loc_layout(0:fv3_io_layout_y-1))
         do nio=0,fv3_io_layout_y-1
            write(filename_layout,'(a,a,I4.4)') trim(filename),'.',nio
            call check(nf90_open(trim(filename_layout),nf90_write,gfile_loc_layout(nio)) )
         enddo
         gfile_loc=gfile_loc_layout(0)
       else
         call check( nf90_open(trim(filename),nf90_write,gfile_loc) )
       endif
       call check( nf90_inq_varid(gfile_loc,trim(varname),VarId) )

       if(add_saved)then
          allocate( workb2(nlon_regional,nlat_regional))
          allocate( worka2(nlat,nlon))
!!!!!!!! read in guess !!!!!!!!!!!!!!

          if(fv3_io_layout_y > 1) then
             do nio=0,fv3_io_layout_y-1
                allocate(work_b_layout(nlon_regional,ny_layout_len(nio)))
                call check(nf90_get_var(gfile_loc_layout(nio),VarId,work_b_layout) )
                work_b(:,ny_layout_b(nio):ny_layout_e(nio))=work_b_layout
                deallocate(work_b_layout)
              enddo
          else
             call check( nf90_get_var(gfile_loc,VarId,work_b) )
          endif
          call fv3_h_to_ll(work_b,worka2,nlon_regional,nlat_regional,nlon,nlat,grid_reverse_flag)
!!!!!!! analysis_inc work_a
          work_a(:,:)=work_a(:,:)-worka2(:,:)
          call fv3_ll_to_h(work_a,workb2,nlon,nlat,nlon_regional,nlat_regional,grid_reverse_flag)
             work_b(:,:)=work_b(:,:)+workb2(:,:)
          deallocate(worka2,workb2)
       else
          call fv3_ll_to_h(work_a,work_b,nlon,nlat,nlon_regional,nlat_regional,grid_reverse_flag)
       endif

       if(fv3_io_layout_y > 1) then
         do nio=0,fv3_io_layout_y-1
            allocate(work_b_layout(nlon_regional,ny_layout_len(nio)))
            work_b_layout=work_b(:,ny_layout_b(nio):ny_layout_e(nio))
            call check( nf90_put_var(gfile_loc_layout(nio),VarId,work_b_layout) )
            deallocate(work_b_layout)
         enddo
       else
          call check( nf90_put_var(gfile_loc,VarId,work_b) )
       endif

       if(fv3_io_layout_y > 1) then
         do nio=0,fv3_io_layout_y-1
           call check(nf90_close(gfile_loc_layout(nio)))
         enddo
         deallocate(gfile_loc_layout)
       else
         call check(nf90_close(gfile_loc))
       endif
       deallocate(work_b,work_a)
    end if !mype_io

    deallocate(work)

end subroutine gsi_fv3ncdf_write_sfc

subroutine gsi_fv3ncdf_write(grd_ionouv,cstate_nouv,add_saved,filenamein,fv3filenamegin)
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    gsi_nemsio_write
!   pgrmmr: wu
!
! abstract:
!
! program history log:
!
!   input argument list:
!    varu,varv
!    add_saved
!    mype     - mpi task id
!    mype_io
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    use mpimod, only: mpi_rtype,mpi_comm_world,mype,mpi_info_null,npe,setcomm,mpi_integer,mpi_max
    use mod_fv3_lola, only: fv3_ll_to_h
    use mod_fv3_lola, only: fv3_h_to_ll
    use netcdf, only: nf90_open,nf90_close
    use netcdf, only: nf90_write,nf90_netcdf4, nf90_mpiio,nf90_inq_varid
    use netcdf, only: nf90_put_var,nf90_get_var,nf90_independent,nf90_var_par_access
    use netcdf, only: nf90_inquire_dimension
    use gsi_bundlemod, only: gsi_bundle
    use general_sub2grid_mod, only: sub2grid_info,general_sub2grid
    implicit none

    type(sub2grid_info),           intent(in)   :: grd_ionouv 
    type(gsi_bundle),              intent(inout):: cstate_nouv
    logical,                       intent(in   ):: add_saved
    character(len=:), allocatable, intent(in)   :: filenamein
    type (type_fv3regfilenameg),   intent(in)   :: fv3filenamegin

    real(r_kind),dimension(1,grd_ionouv%nlat,grd_ionouv%nlon,grd_ionouv%kbegin_loc:grd_ionouv%kend_alloc):: hwork
    character(len=max_filename_length) :: filenamein2 
    character(len=max_varname_length) :: varname,vgsiname,name

    integer(i_kind) nlatcase,nloncase,nxcase,nycase,countloc(4),startloc(4)
    integer(i_kind) countloc_tmp(4),startloc_tmp(4)
    integer(i_kind) kbgn,kend
    integer(i_kind) inative,ilev,ilevtot
    integer(i_kind) :: VarId,gfile_loc
    integer(i_kind) mm1,nzp1,len,nx_phy,iret
    logical  :: phy_smaller_domain
    real(r_kind),allocatable,dimension(:,:):: work_a
    real(r_kind),allocatable,dimension(:,:):: work_b
    real(r_kind),allocatable,dimension(:,:):: workb2,worka2
    real(r_kind),allocatable,dimension(:,:):: work_b_tmp

    integer(i_kind):: iworld,iworld_group,nread,mpi_comm_read,i,ierror
    integer(i_kind),dimension(npe):: members,members_read,mype_read_rank
    logical:: procuse
    
! for io_layout > 1
    real(r_kind),allocatable,dimension(:,:):: work_b_layout
    integer(i_kind) :: nio
    integer(i_kind),allocatable :: gfile_loc_layout(:)
    character(len=180)  :: filename_layout

    mm1=mype+1
    ! Convert from subdomain to full horizontal field distributed among
    ! processors
    call general_sub2grid(grd_ionouv,cstate_nouv%values,hwork)
    nloncase=grd_ionouv%nlon
    nlatcase=grd_ionouv%nlat
    nxcase=nx
    nycase=ny
    kbgn=grd_ionouv%kbegin_loc
    kend=grd_ionouv%kend_loc
    allocate( work_a(nlatcase,nloncase))
    allocate( work_b(nlon_regional,nlat_regional))
    allocate( workb2(nlon_regional,nlat_regional))
    allocate( worka2(nlatcase,nloncase))

    procuse = .false.
    members=-1
    members_read=-1
    if (kbgn<=kend) then
       procuse = .true.
       members(mm1) = mype
    endif

    call mpi_allreduce(members,members_read,npe,mpi_integer,mpi_max,mpi_comm_world,ierror)

    nread=0
    mype_read_rank=-1
    do i=1,npe
       if (members_read(i) >= 0) then
          nread=nread+1
          mype_read_rank(nread) = members_read(i)
       endif
    enddo

    call setcomm(iworld,iworld_group,nread,mype_read_rank,mpi_comm_read,ierror)

    if (procuse) then
       if(fv3_io_layout_y > 1) then
          allocate(gfile_loc_layout(0:fv3_io_layout_y-1))
          do nio=0,fv3_io_layout_y-1
             write(filename_layout,'(a,a,I4.4)') trim(filenamein),'.',nio
             call check( nf90_open(filename_layout,ior(nf90_netcdf4,ior(nf90_write, nf90_mpiio)),gfile_loc_layout(nio),comm=mpi_comm_read,info=MPI_INFO_NULL) )
          enddo
          gfile_loc=gfile_loc_layout(0)
       else
          call check( nf90_open(filenamein,ior(nf90_netcdf4,ior(nf90_write, nf90_mpiio)),gfile_loc,comm=mpi_comm_read,info=MPI_INFO_NULL) )
       endif
       
       do ilevtot=kbgn,kend
          vgsiname=grd_ionouv%names(1,ilevtot)
          if(trim(vgsiname)=='amassi') cycle
          if(trim(vgsiname)=='amassj') cycle
          if(trim(vgsiname)=='amassk') cycle
          if(trim(vgsiname)=='pm2_5') cycle 
          call getfv3lamfilevname(vgsiname,fv3filenamegin,filenamein2,varname)
          if(trim(filenamein) /= trim(filenamein2)) then
             write(6,*)'filenamein and filenamein2 are not the same as expected, stop'
             call stop2(333)
          endif
          ilev=grd_ionouv%lnames(1,ilevtot)
          nz=grd_ionouv%nsig
          nzp1=nz+1
          inative=nzp1-ilev
          countloc=(/nxcase,nycase,1,1/)
          startloc=(/1,1,inative,1/)
          
          work_a=hwork(1,:,:,ilevtot)
          
          if( trim(varname) == 'ref_f3d' .or. trim(adjustl(varname)) == 'flash_extent_density' )then
             iret=nf90_inquire_dimension(gfile_loc,1,name,len)
             if(trim(name)=='xaxis_1') nx_phy=len
             if( nx_phy == nxcase )then
                allocate(work_b_tmp(nxcase,nycase))
                countloc_tmp=(/nxcase,nycase,1,1/)
                phy_smaller_domain = .false.
             else
                allocate(work_b_tmp(nxcase-6,nycase-6))
                countloc_tmp=(/nxcase-6,nycase-6,1,1/)
                phy_smaller_domain = .true.
             end if
             startloc_tmp=(/1,1,ilev,1/)
          end if
          
          call check( nf90_inq_varid(gfile_loc,trim(varname),VarId) )
          call check( nf90_var_par_access(gfile_loc, VarId, nf90_independent))
          
          
          if(index(vgsiname,"delzinc") > 0) then
             if(fv3_io_layout_y > 1) then
                do nio=0,fv3_io_layout_y-1
                   countloc=(/nxcase,ny_layout_len(nio),1,1/)
                   allocate(work_b_layout(nxcase,ny_layout_len(nio)))
                   call check( nf90_get_var(gfile_loc_layout(nio),VarId,work_b_layout,start = startloc, count = countloc) )
                   work_b(:,ny_layout_b(nio):ny_layout_e(nio))=work_b_layout
                   deallocate(work_b_layout)
                enddo
             else
                call check( nf90_get_var(gfile_loc,VarId,work_b,start = startloc, count = countloc) )
             endif
             call fv3_ll_to_h(work_a(:,:),workb2,nloncase,nlatcase,nlon_regional,nlat_regional,grid_reverse_flag)
             work_b(:,:)=work_b(:,:)+workb2(:,:)
          else
             if(add_saved)then
                if(fv3_io_layout_y > 1) then
                   do nio=0,fv3_io_layout_y-1
                      countloc=(/nxcase,ny_layout_len(nio),1,1/)
                      allocate(work_b_layout(nxcase,ny_layout_len(nio)))
                      call check( nf90_get_var(gfile_loc_layout(nio),VarId,work_b_layout,start = startloc, count = countloc) )
                      work_b(:,ny_layout_b(nio):ny_layout_e(nio))=work_b_layout
                      deallocate(work_b_layout)
                   enddo
                else
                   if( trim(varname) == 'ref_f3d' .or. trim(varname) == 'flash_extent_density' )then
                      work_b = 0.0_r_kind
                      call check( nf90_get_var(gfile_loc,VarId,work_b_tmp,start = startloc_tmp, count = countloc_tmp) )
                      where(work_b_tmp < 0.0_r_kind)
                         work_b_tmp = 0.0_r_kind
                      end where
                      if(phy_smaller_domain)then
                         work_b(4:nxcase-3,4:nycase-3) = work_b_tmp
                      else
                         work_b(1:nxcase,1:nycase) = work_b_tmp
                      end if
                   else
                      call check( nf90_get_var(gfile_loc,VarId,work_b,start = startloc, count = countloc) )
                   end if
                endif
                call fv3_h_to_ll(work_b(:,:),worka2,nlon_regional,nlat_regional,nloncase,nlatcase,grid_reverse_flag)
!!!!!!!! analysis_inc:  work_a !!!!!!!!!!!!!!!!
                work_a(:,:)=work_a(:,:)-worka2(:,:)
                call fv3_ll_to_h(work_a(:,:),workb2,nloncase,nlatcase,nlon_regional,nlat_regional,grid_reverse_flag)
                work_b(:,:)=work_b(:,:)+workb2(:,:)
             else  
                call fv3_ll_to_h(work_a(:,:),work_b(:,:),nloncase,nlatcase,nlon_regional,nlat_regional,grid_reverse_flag)
             endif
          endif
          if (ifindstrloc(vnames_nonnegativetracers,trim(varname))> 0) then
              where (work_b <0.0_r_kind)  work_b=0.0_r_kind
          endif
          if(fv3_io_layout_y > 1) then
             do nio=0,fv3_io_layout_y-1
                countloc=(/nxcase,ny_layout_len(nio),1,1/)
                allocate(work_b_layout(nxcase,ny_layout_len(nio)))
                work_b_layout=work_b(:,ny_layout_b(nio):ny_layout_e(nio))
                call check( nf90_put_var(gfile_loc_layout(nio),VarId,work_b_layout, start = startloc, count = countloc) )
                deallocate(work_b_layout)
             enddo
          else
             if( trim(varname) == 'ref_f3d' .or. trim(varname) == 'flash_extent_density' )then
                if(phy_smaller_domain)then
                   work_b_tmp = work_b(4:nxcase-3,4:nycase-3)
                else
                   work_b_tmp = work_b(1:nxcase,1:nycase)
                end if
                where(work_b_tmp < 0.0_r_kind)
                   work_b_tmp = 0.0_r_kind
                end where
                call check( nf90_put_var(gfile_loc,VarId,work_b_tmp, start = startloc_tmp, count = countloc_tmp) )
                deallocate(work_b_tmp)
             else
                call check( nf90_put_var(gfile_loc,VarId,work_b, start = startloc, count = countloc) )
             end if
          endif
          
       enddo !ilevtotl loop
       if(fv3_io_layout_y > 1) then
          do nio=0,fv3_io_layout_y-1
             call check(nf90_close(gfile_loc_layout(nio)))
          enddo
          deallocate(gfile_loc_layout)
       else
          call check(nf90_close(gfile_loc))
       endif
    endif

    call mpi_barrier(mpi_comm_world,ierror)

    deallocate(work_b,work_a)
    deallocate(workb2,worka2)

end subroutine gsi_fv3ncdf_write
subroutine check(status)
    use kinds, only: i_kind
    use netcdf, only: nf90_noerr,nf90_strerror
    integer(i_kind), intent ( in) :: status

    if(status /= nf90_noerr) then
       print *,'ncdf error ', trim(nf90_strerror(status))
       stop  
    end if
end subroutine check
subroutine gsi_fv3ncdf_write_v1(grd_ionouv,cstate_nouv,add_saved,filenamein,fv3filenamegin)
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    gsi_nemsio_write
!   pgrmmr: wu
!
! abstract:
!
! program history log:
! 2020-03-05  lei  modified from gsi_fv3ncdf_write to gsi_fv3ncdf_write_v1  
!   input argument list:
!    varu,varv
!    add_saved
!    mype     - mpi task id
!    mype_io
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    use mpimod, only: npe, setcomm,mpi_integer,mpi_max,mpi_rtype,mpi_comm_world,mype,mpi_info_null
    use mod_fv3_lola, only: fv3_ll_to_h
    use mod_fv3_lola, only: fv3_h_to_ll
    use netcdf, only: nf90_open,nf90_close
    use netcdf, only: nf90_write, nf90_netcdf4,nf90_mpiio,nf90_inq_varid
    use netcdf, only: nf90_put_var,nf90_get_var
    use netcdf, only: nf90_independent,nf90_var_par_access
    use gsi_bundlemod, only: gsi_bundle
    use general_sub2grid_mod, only: sub2grid_info,general_sub2grid
    implicit none

    type(sub2grid_info),        intent(in)   :: grd_ionouv 
    type(gsi_bundle),           intent(inout):: cstate_nouv
    logical,                    intent(in   ):: add_saved
    character(*),               intent(in)   :: filenamein
    type (type_fv3regfilenameg),intent(in)   :: fv3filenamegin

    real(r_kind),dimension(1,grd_ionouv%nlat,grd_ionouv%nlon,grd_ionouv%kbegin_loc:grd_ionouv%kend_alloc):: hwork
    character(len=max_filename_length) :: filenamein2 

    integer(i_kind) kbgn,kend
    integer(i_kind) inative,ilev,ilevtot
    integer(i_kind) :: VarId,gfile_loc
    integer(i_kind) mm1,nzp1
    real(r_kind),allocatable,dimension(:,:):: work_a
    real(r_kind),allocatable,dimension(:,:):: work_b
    real(r_kind),allocatable,dimension(:,:):: workb2,worka2
    character(len=max_varname_length) :: varname,vgsiname
    integer(i_kind) nlatcase,nloncase,nxcase,nycase,countloc(3),startloc(3)

    integer(i_kind):: iworld,iworld_group,nread,mpi_comm_read,i,ierror
    integer(i_kind),dimension(npe):: members,members_read,mype_read_rank
    logical:: procuse


    mm1=mype+1
    nloncase=grd_ionouv%nlon
    nlatcase=grd_ionouv%nlat

    call general_sub2grid(grd_ionouv,cstate_nouv%values,hwork)
    nxcase=nx
    nycase=ny
    kbgn=grd_ionouv%kbegin_loc
    kend=grd_ionouv%kend_loc
    allocate( work_a(nlatcase,nloncase))
    allocate( work_b(nlon_regional,nlat_regional))
    allocate( workb2(nlon_regional,nlat_regional))
    allocate( worka2(nlatcase,nloncase))

    procuse = .false.
    members=-1
    members_read=-1
    if (kbgn<=kend) then
       procuse = .true.
       members(mm1) = mype
    endif

    call mpi_allreduce(members,members_read,npe,mpi_integer,mpi_max,mpi_comm_world,ierror)

    nread=0
    mype_read_rank=-1
    do i=1,npe
       if (members_read(i) >= 0) then
          nread=nread+1
          mype_read_rank(nread) = members_read(i)
       endif
    enddo

    call setcomm(iworld,iworld_group,nread,mype_read_rank,mpi_comm_read,ierror)

    if (procuse) then
    call check ( nf90_open(filenamein,ior(nf90_netcdf4,ior(nf90_write, nf90_mpiio)),gfile_loc,comm=mpi_comm_read,info=MPI_INFO_NULL)) !clt
    do ilevtot=kbgn,kend
      vgsiname=grd_ionouv%names(1,ilevtot)
      if(trim(vgsiname)=='amassi') cycle
      if(trim(vgsiname)=='amassj') cycle
      if(trim(vgsiname)=='amassk') cycle
      if(trim(vgsiname)=='pm2_5') cycle 
      call getfv3lamfilevname(vgsiname,fv3filenamegin,filenamein2,varname)
      if(trim(filenamein) /= trim(filenamein2)) then
        write(6,*)'filenamein and filenamein2 are not the same as expected, stop'
        call stop2(333)
      endif
      ilev=grd_ionouv%lnames(1,ilevtot)
      nz=grd_ionouv%nsig
      nzp1=nz+1
      inative=nzp1-ilev
      startloc=(/1,1,inative+1/)
      countloc=(/nxcase,nycase,1/)

      work_a=hwork(1,:,:,ilevtot)


      call check( nf90_inq_varid(gfile_loc,trim(varname),VarId) )
      call check( nf90_var_par_access(gfile_loc, VarId, nf90_independent))
      call check( nf90_get_var(gfile_loc,VarId,work_b,start=startloc,count=countloc) )
      if(index(vgsiname,"delzinc") > 0) then
        write(6,*)'delz is not in the cold start fiels with this option, incompatible setup , stop'
        call stop2(333)
      endif

      if(add_saved)then
! for being now only lev between (including )  2 and nsig+1 of work_b (:,:,lev) 
! are updated
        call fv3_h_to_ll(work_b(:,:),worka2,nlon_regional,nlat_regional,nloncase,nlatcase,grid_reverse_flag)
!!!!!!!! analysis_inc:  work_a !!!!!!!!!!!!!!!!
        work_a(:,:)=work_a(:,:)-worka2(:,:)
        call fv3_ll_to_h(work_a(:,:),workb2,nloncase,nlatcase,nlon_regional,nlat_regional,grid_reverse_flag)
        work_b(:,:)=work_b(:,:)+workb2(:,:)
      else
        call fv3_ll_to_h(work_a(:,:),work_b(:,:),nloncase,nlatcase,nlon_regional,nlat_regional,grid_reverse_flag)
      endif
      if (ifindstrloc(vnames_nonnegativetracers,trim(varname))> 0) then
           where (work_b <0.0_r_kind)  work_b=0.0_r_kind
      endif
      call check( nf90_put_var(gfile_loc,VarId,work_b,start=startloc,count=countloc) )
    enddo  !ilevtot
    call check(nf90_close(gfile_loc))
    endif
    deallocate(work_b,work_a)
    deallocate(worka2,workb2)

end subroutine gsi_fv3ncdf_write_v1

subroutine reverse_grid_r(grid,nx,ny,nz)
!
!  reverse the first two dimension of the array grid
!
    use kinds, only: r_kind,i_kind

    implicit none
    integer(i_kind),  intent(in     ) :: nx,ny,nz
    real(r_kind),     intent(inout  ) :: grid(nx,ny,nz)

    real(r_kind)                      :: tmp_grid(nx,ny)
    integer(i_kind)                   :: i,j,k
!
    do k=1,nz
       tmp_grid(:,:)=grid(:,:,k)
       do j=1,ny
          do i=1,nx
             grid(i,j,k)=tmp_grid(nx+1-i,ny+1-j)
          enddo        
       enddo
    enddo

end subroutine reverse_grid_r

subroutine reverse_grid_r_uv(grid,nx,ny,nz)
!
!  reverse the first two dimension of the array grid
!
    use kinds, only: r_kind,i_kind

    implicit none
    integer(i_kind), intent(in     ) :: nx,ny,nz
    real(r_kind),    intent(inout  ) :: grid(nx,ny,nz)

    real(r_kind)                     :: tmp_grid(nx,ny)
    integer(i_kind)                  :: i,j,k
!
    do k=1,nz
       tmp_grid(:,:)=grid(:,:,k)
       do j=1,ny
          do i=1,nx
             grid(i,j,k)=-tmp_grid(nx+1-i,ny+1-j)
          enddo
       enddo
    enddo

end subroutine reverse_grid_r_uv

subroutine convert_qx_to_cvpqx(qr_arr,qs_arr,qg_arr,use_cvpqx,cvpqx_pvalue)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    convert_qx_to_cvpqx
!   prgmmr: J. Park(CAPS)                     date: 2021-05-05
!
! abstract: convert qx(mixing ratio) to cvpqx using power transform for qr, qs, qg
!
! program history log:
!   2021-05-05 - initial commit 
!              - this is used when GSI reads qx data from a background file
!                (subroutine read_fv3_netcdf_guess)
!              - since minimum qr, qs, and qg are set for CVlogq,
!                it reads three qx arrays and then processes.
!
!   input argument list:
!     qr_arr         - array of qr 
!     qs_arr         - array of qs 
!     qg_arr         - array of qg 
!     use_cvpqx      - flag to use power transform or not
!     cvpqx_pvalue - value to be used for power transform
!
!   output argument list:
!     qr_arr           - updated array of qr after power transform
!     qs_arr           - updated array of qs after
!     qg_arr           - updated array of qg after power transfrom
!
! attributes:
!   language: f90
!
    use kinds, only: r_kind,i_kind
    use gridmod, only: lat2,lon2,nsig
    use guess_grids, only: ges_tsen
    use mpimod, only: mype
    use guess_grids, only: ntguessig
    use constants, only: zero, one_tenth

    implicit none
    real(r_kind), intent(inout  ) :: qr_arr(lat2,lon2,nsig)
    real(r_kind), intent(inout  ) :: qs_arr(lat2,lon2,nsig)
    real(r_kind), intent(inout  ) :: qg_arr(lat2,lon2,nsig)
    logical,      intent(in     ) :: use_cvpqx
    real(r_kind), intent(in     ) :: cvpqx_pvalue

    integer(i_kind)                   :: i, j, k, it

    real(r_kind) :: qr_min, qs_min, qg_min
    real(r_kind) :: qr_thrshd, qs_thrshd, qg_thrshd
!
    it=ntguessig
!

!   print info message: CVq, CVlogq, and CVpq
    if(mype==0)then
       if (use_cvpqx) then
          if ( cvpqx_pvalue == 0._r_kind ) then        ! CVlogq
              write(6,*)'read_fv3_netcdf_guess: ',     &
                        ' reset zero of qr/qs/qg to specified values (~0dbz)', &
                        'before log transformation. (for dbz assimilation)' 
              write(6,*)'read_fv3_netcdf_guess: convert qr/qs/qg to log transform.'
          else if ( cvpqx_pvalue > 0._r_kind ) then   ! CVpq
              write(6,*)'read_fv3_netcdf_guess: convert qr/qs/qg with power transform .'
          end if
       else                                         ! CVq
          write(6,*)'read_fv3_netcdf_guess: only reset (qr/qs/qg) to &
                     0.0 for negative analysis value. (regular qx)'
       end if
    end if

    do k=1,nsig
      do i=1,lon2
        do j=1,lat2
!         Apply power transform if option is ON 
          if (use_cvpqx) then
             if ( cvpqx_pvalue == 0._r_Kind ) then ! CVlogq
                 if (ges_tsen(j,i,k,it) > 274.15_r_kind) then
                      qr_min=2.9E-6_r_kind
                      qr_thrshd=qr_min * one_tenth
                      qs_min=0.1E-9_r_kind
                      qs_thrshd=qs_min
                      qg_min=3.1E-7_r_kind
                      qg_thrshd=qg_min * one_tenth
                 else if (ges_tsen(j,i,k,it) <= 274.15_r_kind .and. &
                          ges_tsen(j,i,k,it) >= 272.15_r_kind) then
                      qr_min=2.0E-6_r_kind
                      qr_thrshd=qr_min * one_tenth
                      qs_min=1.3E-7_r_kind
                      qs_thrshd=qs_min * one_tenth
                      qg_min=3.1E-7_r_kind
                      qg_thrshd=qg_min * one_tenth
                 else if (ges_tsen(j,i,k,it) < 272.15_r_kind) then
                      qr_min=0.1E-9_r_kind
                      qr_thrshd=qr_min
                      qs_min=6.3E-6_r_kind
                      qs_thrshd=qs_min * one_tenth
                      qg_min=3.1E-7_r_kind
                      qg_thrshd=qg_min * one_tenth
                 end if

                 if ( qr_arr(j,i,k) <= qr_thrshd )  qr_arr(j,i,k) = qr_min
                 if ( qs_arr(j,i,k) <= qs_thrshd )  qs_arr(j,i,k) = qs_min
                 if ( qg_arr(j,i,k) <= qg_thrshd )  qg_arr(j,i,k) = qg_min

                 qr_arr(j,i,k) = log(qr_arr(j,i,k))
                 qs_arr(j,i,k) = log(qs_arr(j,i,k))
                 qg_arr(j,i,k) = log(qg_arr(j,i,k))

             else if ( cvpqx_pvalue > 0._r_kind ) then   ! CVpq
                 qr_arr(j,i,k)=((max(qr_arr(j,i,k),1.0E-6_r_kind))**cvpqx_pvalue-1)/cvpqx_pvalue
                 qs_arr(j,i,k)=((max(qs_arr(j,i,k),1.0E-6_r_kind))**cvpqx_pvalue-1)/cvpqx_pvalue
                 qg_arr(j,i,k)=((max(qg_arr(j,i,k),1.0E-6_r_kind))**cvpqx_pvalue-1)/cvpqx_pvalue
             end if
          else ! CVq
              qr_min=zero
              qs_min=zero
              qg_min=zero
              qr_arr(j,i,k) = max(qr_arr(j,i,k), qr_min)
              qs_arr(j,i,k) = max(qs_arr(j,i,k), qs_min)
              qg_arr(j,i,k) = max(qg_arr(j,i,k), qg_min)
          end if
        end do
      end do
    end do

end subroutine convert_qx_to_cvpqx

subroutine convert_nx_to_cvpnx(qnx_arr,cvpnr,cvpnr_pvalue)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    convert_nx_to_cvpnx
!   prgmmr: J. Park(CAPS)                     date: 2021-05-05
!
! abstract: convert nx (number concentration) to cvpnx using power transform
!
! program history log:
!   2021-05-05 - initial commit 
!              - this is used when GSI reads nx data from a background file
!                (subroutine read_fv3_netcdf_guess)
!              - this can be used for other nx variables
!
!   input argument list:
!     qnx_arr        - array of qnx
!     cvpnr          - flag to use power transform or not
!     cvpnr_pvalue   - value to be used for power transform
!
!   output argument list:
!     qnx_arr           - updated array of qnx after power transform
!
! attributes:
!   language: f90
!
    use kinds, only: r_kind,i_kind
    use gridmod, only: lat2,lon2,nsig
    use mpimod, only: mype
    use constants, only: zero, one_tenth

    implicit none
    real(r_kind), intent(inout  ) :: qnx_arr(lat2,lon2,nsig)
    logical,      intent(in     ) :: cvpnr
    real(r_kind), intent(in     ) :: cvpnr_pvalue

    integer(i_kind)                   :: i, j, k
!

!   print info message: CVpnr
    if (mype==0 .and. cvpnr)then
       write(6,*)'read_fv3_netcdf_guess: convert qnx with power transform .'
    end if

    do k=1,nsig
      do i=1,lon2
        do j=1,lat2

!          Treatment on qnx ; power transform
           if (cvpnr) then
              qnx_arr(j,i,k)=((max(qnx_arr(j,i,k),1.0E-2_r_kind)**cvpnr_pvalue)-1)/cvpnr_pvalue
           endif

        end do
      end do
    end do
end subroutine convert_nx_to_cvpnx

subroutine convert_cvpqx_to_qx(qr_arr,qs_arr,qg_arr,use_cvpqx,cvpqx_pvalue)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    convert_cvpqx_to_qx
!   prgmmr: J. Park(CAPS)                     date: 2021-05-05
!
! abstract: convert cvpqx to qx for qr, qs, qg
!
! program history log:
!   2021-05-05 - initial commit 
!              - this is used when GSI writes qx data to a background file
!                (subroutine wrfv3_netcdf)
!              - since minimum qr, qs, and qg are set for CVlogq,
!                it reads three qx arrays and then processes.
!
!   input argument list:
!     qr_arr         - array of qr 
!     qs_arr         - array of qs 
!     qg_arr         - array of qg 
!     use_cvpqx      - flag to use power transform or not
!     cvpqx_pvalue   - value to be used for power transform
!
!   output argument list:
!     qr_arr           - updated array of qr after power transform
!     qs_arr           - updated array of qs after
!     qg_arr           - updated array of qg after power transfrom
!
! attributes:
!   language: f90
!
    use kinds, only: r_kind,i_kind
    use gridmod, only: lat2,lon2,nsig
    use guess_grids, only: ges_tsen
    use mpimod, only: mype
    use guess_grids, only: ntguessig
    use constants, only: zero, one_tenth,r0_01

    implicit none
    real(r_kind), intent(inout  ) :: qr_arr(lat2,lon2,nsig)
    real(r_kind), intent(inout  ) :: qs_arr(lat2,lon2,nsig)
    real(r_kind), intent(inout  ) :: qg_arr(lat2,lon2,nsig)
    logical,      intent(in     ) :: use_cvpqx
    real(r_kind), intent(in     ) :: cvpqx_pvalue

    integer(i_kind)               :: i, j, k, it

    real(r_kind), dimension(lat2,lon2,nsig) :: tmparr_qr, tmparr_qs
    real(r_kind), dimension(lat2,lon2,nsig) :: tmparr_qg

    real(r_kind) :: qr_min, qs_min, qg_min
    real(r_kind) :: qr_tmp, qs_tmp, qg_tmp
    real(r_kind) :: qr_thrshd, qs_thrshd, qg_thrshd
!
    it=ntguessig
!

!   print info message: CVq, CVlogq, and CVpq
    if(mype==0)then
       if (use_cvpqx) then
          if ( cvpqx_pvalue == 0._r_kind ) then        ! CVlogq
              write(6,*)'wrfv3_netcdf: convert log(qr/qs/qg) back to qr/qs/qg.'
              write(6,*)'wrfv3_netcdf: then reset (qr/qs/qg) to 0.0 for some cases.'
          else if ( cvpqx_pvalue > 0._r_kind ) then   ! CVpq
              write(6,*)'wrfv3_netcdf: convert power transformed (qr/qs/qg) back to qr/qs/qg.'
              write(6,*)'wrfv3_netcdf: then reset (qr/qs/qg) to 0.0 for some cases.'
          end if
       else                                         ! CVq
          write(6,*)'wrfv3_netcdf: only reset (qr/qs/qg) to 0.0 for negative analysis value. (regular qx)'
       end if
    end if

!   Initialized temporary arrays with ges. Will be recalculated later if cvlogq or cvpq is used
    tmparr_qr =qr_arr
    tmparr_qs =qs_arr
    tmparr_qg =qg_arr

    do k=1,nsig
      do i=1,lon2
        do j=1,lat2

!          initialize hydrometeors as zero
           qr_tmp=zero
           qs_tmp=zero
           qg_tmp=zero

           if ( use_cvpqx ) then
              if ( cvpqx_pvalue == 0._r_kind ) then ! CVlogq

                 if (ges_tsen(j,i,k,it) > 274.15_r_kind) then
                    qr_min=2.9E-6_r_kind
                    qr_thrshd=qr_min * one_tenth
                    qs_min=0.1E-9_r_kind
                    qs_thrshd=qs_min
                    qg_min=3.1E-7_r_kind
                    qg_thrshd=qg_min * one_tenth
                 else if (ges_tsen(j,i,k,it) <= 274.15_r_kind .and. &
                          ges_tsen(j,i,k,it) >= 272.15_r_kind ) then
                    qr_min=2.0E-6_r_kind
                    qr_thrshd=qr_min * one_tenth
                    qs_min=1.3E-7_r_kind
                    qs_thrshd=qs_min * one_tenth
                    qg_min=3.1E-7_r_kind
                    qg_thrshd=qg_min * one_tenth
                 else if (ges_tsen(j,i,k,it) < 272.15_r_kind) then
                    qr_min=0.1E-9_r_kind
                    qr_thrshd=qr_min
                    qs_min=6.3E-6_r_kind
                    qs_thrshd=qs_min * one_tenth
                    qg_min=3.1E-7_r_kind
                    qg_thrshd=qg_min * one_tenth
                 end if

                 qr_tmp=exp(qr_arr(j,i,k))
                 qs_tmp=exp(qs_arr(j,i,k))
                 qg_tmp=exp(qg_arr(j,i,k))

!                if no update or very tiny value of qr/qs/qg, re-set/clear it
!                off to zero
                 if ( abs(qr_tmp - qr_min) < (qr_min*r0_01) ) then
                    qr_tmp=zero
                 else if (qr_tmp < qr_thrshd) then
                    qr_tmp=zero
                 end if

                 if ( abs(qs_tmp - qs_min) < (qs_min*r0_01) ) then
                    qs_tmp=zero
                 else if (qs_tmp < qs_thrshd) then
                    qs_tmp=zero
                 end if

                 if ( abs(qg_tmp - qg_min) < (qg_min*r0_01) ) then
                    qg_tmp=zero
                 else if (qg_tmp < qg_thrshd) then
                    qg_tmp=zero
                 end if

              else if ( cvpqx_pvalue > 0._r_kind ) then   ! CVpq

                 qr_tmp=max((cvpqx_pvalue*qr_arr(j,i,k)+1)**(1/cvpqx_pvalue)-1.0E-6_r_kind,0.0_r_kind)
                 qs_tmp=max((cvpqx_pvalue*qs_arr(j,i,k)+1)**(1/cvpqx_pvalue)-1.0E-6_r_kind,0.0_r_kind)
                 qg_tmp=max((cvpqx_pvalue*qg_arr(j,i,k)+1)**(1/cvpqx_pvalue)-1.0E-6_r_kind,0.0_r_kind)

                 !Set a upper limit to hydrometeors to disable overshooting
                 qr_tmp=min(qr_tmp,1E-2_r_kind)
                 qs_tmp=min(qs_tmp,1.0E-2_r_kind)
                 qg_tmp=min(qg_tmp,2E-2_r_kind)
              end if

           else   ! For CVq
              qr_min=zero
              qs_min=zero
              qg_min=zero

              qr_tmp=qr_arr(j,i,k)-1.0E-8_r_kind
              qs_tmp=qs_arr(j,i,k)-1.0E-8_r_kind
              qg_tmp=qg_arr(j,i,k)-1.0E-8_r_kind


              qr_tmp=max(qr_tmp,qr_min)
              qs_tmp=max(qs_tmp,qs_min)
              qg_tmp=max(qg_tmp,qg_min)

           end if         ! cvpqx

           tmparr_qr(j,i,k)=qr_tmp
           tmparr_qs(j,i,k)=qs_tmp
           tmparr_qg(j,i,k)=qg_tmp

        end do
      end do
    end do

    qr_arr=tmparr_qr
    qs_arr=tmparr_qs
    qg_arr=tmparr_qg

end subroutine convert_cvpqx_to_qx

subroutine convert_cvpnx_to_nx(qnx_arr,cvpnr,cvpnr_pvalue,cloud_nt_updt,q_arr,qr_arr,ps_arr)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    convert_cvpnx_to_nx
!   prgmmr: J. Park(CAPS)                     date: 2021-05-05
!
! abstract: convert cvpnx to nx (number concentration)
!
! program history log:
!   2021-05-05 - initial commit 
!              - this is used when GSI writes nx data from a background file
!                (subroutine wrfv3_netcdf)
!              - this can be used for other nx variables
!
!   input argument list:
!     qnx_arr        - array of qnx
!     cvpnr          - flag to use power transform or not
!     cvpnr_pvalue   - value to be used for power transform
!     cloud_nt_updt  - integer flag to use re-initialisation of QNRAIN with analyzed qr and n0r
!     q_arr          - array of qv, used only if cloud_nt_up_dt is 2
!     qr_arr         - array of qr, used only if cloud_nt_up_dt is 2
!     ps_arr         - array of ps, used only if cloud_nt_up_dt is 2
!
!   output argument list:
!     qnx_arr           - updated array of qnx after power transform
!
! attributes:
!   language: f90
!
    use kinds, only: r_kind,i_kind
    use gridmod, only: lat2,lon2,nsig
    use mpimod, only: mype
    use constants, only: zero, one, one_tenth
    use directDA_radaruse_mod, only: init_mm_qnr
    use guess_grids, only: ges_tsen
    use guess_grids, only: ntguessig
    use gridmod, only: pt_ll, aeta1_ll
    use constants, only: r10, r100, rd


    implicit none
    real(r_kind),    intent(inout)    :: qnx_arr(lat2,lon2,nsig)
    logical,         intent(in   )    :: cvpnr
    real(r_kind),    intent(in   )    :: cvpnr_pvalue
    integer(i_kind), intent(in   )    :: cloud_nt_updt
    real(r_kind),    intent(in   )    :: q_arr(lat2,lon2,nsig)
    real(r_kind),    intent(in   )    :: qr_arr(lat2,lon2,nsig)
    real(r_kind),    intent(in   )    :: ps_arr(lat2,lon2)

    real(r_kind), dimension(lat2,lon2,nsig) :: tmparr_qnr
    integer(i_kind)                   :: i, j, k, it
    real(r_kind)                      :: qnr_tmp

    real(r_kind) :: P1D,T1D,Q1D,RHO,QR1D
    real(r_kind),parameter:: D608=0.608_r_kind


!
    it=ntguessig
!

!   print info message: CVpnr
    if (mype==0 .and. cvpnr)then
       write(6,*)'wrfv3_netcdf: convert power transformed (qnx) back to qnx.'
    end if

! Initialized temp arrays with ges.
    tmparr_qnr=qnx_arr

    do k=1,nsig
      do i=1,lon2
        do j=1,lat2

!          re-initialisation of QNRAIN with analyzed qr and N0r(which is single-moment parameter)
!          equation is used in subroutine init_MM of initlib3d.f90 in arps package
          qnr_tmp = zero
          if ( cloud_nt_updt == 2 ) then
             T1D=ges_tsen(j,i,k,it)                                 ! sensible temperature (K)
             P1D=r100*(aeta1_ll(k)*(r10*ps_arr(j,i)-pt_ll)+pt_ll)   ! pressure hPa --> Pa
             Q1D=q_arr(j,i,k)/(one-q_arr(j,i,k))                    ! mixing ratio 
             RHO=P1D/(rd*T1D*(one+D608*Q1D))                        ! air density in kg m^-3
             QR1D=qr_arr(j,i,k)
             CALL init_mm_qnr(RHO,QR1D,qnr_tmp)
             qnr_tmp = max(qnr_tmp, zero)

          else
            if (cvpnr) then ! power transform
              qnr_tmp=max((cvpnr_pvalue*qnx_arr(j,i,k)+1)**(1/cvpnr_pvalue)-1.0E-2_r_kind,0.0_r_kind)
            else
                qnr_tmp=qnx_arr(j,i,k)
            end if

          end if
          tmparr_qnr(j,i,k)=qnr_tmp

        end do
      end do
    end do

    qnx_arr=tmparr_qnr

end subroutine convert_cvpnx_to_nx
subroutine gsi_copy_bundle(bundi,bundo) 
    use gsi_bundlemod, only:gsi_bundleinquire, gsi_bundlegetpointer,gsi_bundleputvar
    implicit none  
     
 !  copy the variables in the gsi_metguess_bundle_inout to gsi_bundle_inout or
 !  vice versa, according to icopy_flag  
 ! !INPUT PARAMETERS:

    type(gsi_bundle), intent(in   ) :: bundi
    type(gsi_bundle), intent(inout) :: bundo

 ! !INPUT/OUTPUT PARAMETERS:

    character(len=max_varname_length),dimension(:),allocatable:: src_name_vars2d
    character(len=max_varname_length),dimension(:),allocatable:: src_name_vars3d
    character(len=max_varname_length),dimension(:),allocatable:: target_name_vars2d
    character(len=max_varname_length),dimension(:),allocatable:: target_name_vars3d
    character(len=max_varname_length) ::varname 
    real(r_kind),dimension(:,:,:),pointer:: pvar3d=>NULL()
    real(r_kind),dimension(:,:),pointer:: pvar2d =>NULL()
    integer(i_kind):: src_nc3d,src_nc2d,target_nc3d,target_nc2d
    integer(i_kind):: ivar,jvar,istatus
    src_nc3d=bundi%n3d
    src_nc2d=bundi%n2d
    target_nc3d=bundo%n3d
    target_nc2d=bundo%n2d
    allocate(src_name_vars3d(src_nc3d),src_name_vars2d(src_nc2d))
    allocate(target_name_vars3d(target_nc3d),target_name_vars2d(target_nc2d))
    call gsi_bundleinquire(bundi,'shortnames::3d',src_name_vars3d,istatus)
    call gsi_bundleinquire(bundi,'shortnames::2d',src_name_vars2d,istatus)
    call gsi_bundleinquire(bundo,'shortnames::3d',target_name_vars3d,istatus)
    call gsi_bundleinquire(bundo,'shortnames::2d',target_name_vars2d,istatus)
    do ivar=1,src_nc3d
      varname=trim(src_name_vars3d(ivar))
      do jvar=1,target_nc3d
        if(index(target_name_vars3d(jvar),varname) > 0)  then
          call GSI_BundleGetPointer (bundi,varname,pvar3d,istatus)
          call gsi_bundleputvar (bundo,varname,pvar3d,istatus)
          exit
        endif
      enddo
    enddo
    do ivar=1,src_nc2d
      varname=trim(src_name_vars2d(ivar))
      do jvar=1,target_nc2d
        if(index(target_name_vars2d(jvar),varname) > 0)  then
          call GSI_BundleGetPointer (bundi,varname,pvar2d,istatus)
          call gsi_bundleputvar (bundo,varname,pvar2d,istatus)
          exit
        endif
      enddo
    enddo
    deallocate(src_name_vars3d,src_name_vars2d)
    deallocate(target_name_vars3d,target_name_vars2d)
    return
end subroutine gsi_copy_bundle
subroutine getfv3lamfilevname(vgsinamein,fv3filenamegref,filenameout,vname)

    type (type_fv3regfilenameg),intent (in) :: fv3filenamegref
    character(len=*),intent(out):: vname
    character(len=*),intent(out):: filenameout
    character(len=*),intent( in):: vgsinamein

    if (ifindstrloc(vgsiname,vgsinamein)<= 0) then
      write(6,*)'the name ',vgsinamein ,'cannot be treated correctly in getfv3lamfilevname,stop'
      call stop2(333)
    endif
    if(ifindstrloc(vardynvars,vgsinamein)> 0)  then 
        filenameout=fv3filenamegref%dynvars
    else if(ifindstrloc(vartracers,vgsinamein)> 0 )  then 
        filenameout=fv3filenamegref%tracers
    else if(ifindstrloc(varphyvars,vgsinamein)> 0)  then
        filenameout=fv3filenamegref%phyvars
    else
        write(6,*)'the filename corresponding to var ',trim(vgsinamein),' is not found, stop ' 
        call stop2(333)
    endif
    vname=varfv3name(ifindstrloc(vgsiname,vgsinamein))
    if(trim(vname)=="T".and. fv3sar_bg_opt==1) then
       vname="t"
    endif 
    
    return
end subroutine getfv3lamfilevname
function ifindstrloc(str_array,strin)
    integer(i_kind) ifindstrloc
    character(len=max_varname_length),dimension(:) :: str_array
    character(len=*) :: strin
    integer(i_kind) i
    ifindstrloc=0
    do i=1,size(str_array)
      if(trim(str_array(i)) == trim(strin)) then 
        ifindstrloc=i
        exit
      endif
    enddo
end function ifindstrloc
    

end module gsi_rfv3io_mod

module read_wrf_nmm_guess_mod
use abstract_read_wrf_nmm_guess_mod
  type, extends(abstract_read_wrf_nmm_guess_class) :: read_wrf_nmm_guess_class 
  contains
    procedure, pass(this) :: read_wrf_nmm_binary_guess => read_wrf_nmm_binary_guess_wrf 
    procedure, pass(this) :: read_wrf_nmm_netcdf_guess => read_wrf_nmm_netcdf_guess_wrf
    procedure, pass(this) :: read_nems_nmmb_guess => read_nems_nmmb_guess_wrf
  end type read_wrf_nmm_guess_class 
contains
  subroutine read_wrf_nmm_binary_guess_wrf(this,mype)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    read_wrf_nmm_binary_guess        read wrf_nmm interface file
  !   prgmmr: parrish          org: np22                date: 2003-09-05
  !
  ! abstract: in place of read_guess for global application, read guess
  !             from regional model, in this case the wrf nmm (non-hydrostatic
  !             mesoscale model).  This version reads a binary file created
  !             in a previous step that interfaces with the wrf infrastructure.
  !             A later version will read directly from the wrf restart file.
  !             The guess is read in by complete horizontal fields, one field
  !             per processor, in parallel.  Each horizontal input field is 
  !             converted from the staggered e-grid to an unstaggered a-grid.
  !             If filled_grid=.true., then the unstaggered a-grid has all the
  !             holes of the e-grid filled, and has twice as many points as the
  !             input grid.  If half_grid=.true., then the unstaggered grid is
  !             derived from every other row of the input e-grid, and has 1/2 as
  !             many points as the input grid.
  !
  ! program history log:
  !   2003-09-05  parrish
  !   2004-06-22  parrish, document
  !   2004-08-02  treadon - add only to module use, add intent in/out
  !   2004-11-20  parrish - change to mpi-io for binary file format
  !   2004-12-15  treadon - remove variable mype from call load_geop_hgt,
  !                         rename variable wrf_ges_filename as wrfges
  !   2005-02-17  todling - ifdef'ed wrf code out
  !   2005-02-23  wu - setup for qoption=2 and output stats of RH
  !   2005-04-01  treadon - add initialization of ges_oz, prsi_oz; comestic 
  !                         format changes
  !   2005-05-27  parrish - add call get_derivatives
  !   2005-06-13  treadon - remove extra open(nfcst...) statement
  !   2005-07-06  parrish - add changes to read pint if available 
  !                         (update_pint=.true.)
  !   2005-07-22  parrish - read surface roughness length
  !   2005-09-29  parrish - add call to get_zderivs
  !   2005-11-21  kleist - new calls to genqsat and calctends
  !   2005-11-21  derber - make qoption=1 work same as qoption=2
  !   2005-11-29  derber - remove external iteration dependent calculations
  !   2005-11-29  parrish - correct error in reading of roughness length field
  !   2005-12-15  treadon - remove initialization of certain guess arrays (done elsewhere)
  !   2006-02-02  treadon - remove load_prsges,load_geop_hgt,prsi,prsl (not used)
  !   2006-03-06  parrish - correct u10,v10 grid type flag, igtype.  s/b 1, not 2
  !   2006-04-03  derber  - include tuned fact10 for 10m winds
  !   2006-04-06  middlecoff - changed nfcst from 11 to lendian_in
  !   2006-06-19  wu - changes to allow nfldsig=3 (multiple first guess)
  !   2006-07-28  derber  - include sensible temperature
  !   2006-07-31  kleist - change to use ges_ps instead of lnps
  !   2007-03-13  derber - remove unused qsinv2 from jfunc use list
  !   2007-04-12  parrish - add modifications to allow any combination of ikj or ijk
  !                          grid ordering for input 3D fields
  !   2007-05-02  parrish - fix bug to prevent out of memory reference when pint missing
  !   2008-04-16  safford - rm unused uses
  !   2012-01-14  zhu     - add cloud hydrometeors for cloudy radiance
  !   2012-10-11  parrish - add option to swap bytes immediately after every call to mpi_file_read_at.
  !                           (to handle cases of big-endian file/little-endian machine and vice-versa)
  !   2012-11-30  tong    - added the calculation of ges_prsl for the caculation of cloud mixing ratio,
  !                         because load_prsges is called after this subroutine is called.                       
  !   2013-10-19  todling - efr_q variables now in cloud_efr module (update mod name too)
  !   2013-10-30  todling - ltosj/i now live in commvars
  !   2014-06-27  S.Liu   - detach use_reflectivity from n_actual_clouds
  !   2015_05_12  wu      - bug fixes for FGAT
  !   2015_09_20  s.liu   - convert nmmb F_ICE, F_RAIN to water content before interpolation
  !   2016_03_02  s.liu/carley   - remove use_reflectivity and use i_gsdcldanal_type
  !   2016_04_28  eliu    - remove cwgues0 
  !
  !   input argument list:
  !     mype     - pe number
  !
  !     NOTES:  need to pay special attention to various surface fields to make sure
  !             they are correct (check units).  fact10 needs to be computed from 
  !             10m wind, which is included in wrf nmm interface file.
  !
  !             Cloud water currently set to zero.  Need to fix before doing precip
  !             assimilation--wait until global adopts Brad Ferrier's multi-species 
  !             scheme??
  !
  !             Ozone currently set to zero.  No ozone variable in wrf nmm--climatology
  !             instead.  Do we use climatology?
  !
  !             No background bias yet. (biascor ignored)
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$  end documentation block
    use kinds, only: r_kind,r_single,i_long,i_llong,i_kind
    use mpimod, only: ierror,mpi_integer,mpi_sum,mpi_comm_world,npe,mpi_rtype, &
         mpi_offset_kind,mpi_info_null,mpi_mode_rdonly,mpi_status_size
    use guess_grids, only: & 
         fact10,soil_type,veg_frac,veg_type,sfc_rough,sfct,sno,soil_temp,soil_moi,&
         isli,nfldsig,ifilesig,ges_tsen,ges_prsl
    use cloud_efr_mod, only: efr_ql,efr_qi,efr_qr,efr_qs,efr_qg,efr_qh
    use cloud_efr_mod, only: cloud_calc
    use gridmod, only: lat2,lon2,itotsub,&
         pdtop_ll,pt_ll,nlon,nlat,nlon_regional,nsig,nlat_regional,half_grid,&
         filled_grid,aeta1_ll,aeta2_ll, &
        displs_s,ijn_s,half_nmm_grid2a,fill_nmm_grid2a3
    use general_commvars_mod, only: ltosi_s,ltosj_s
    use constants, only: zero,one_tenth,half,one,grav,fv,zero_single,r0_01,ten
    use wrf_params_mod, only: update_pint
    use gsi_io, only: lendian_in
    use gsi_metguess_mod, only: gsi_metguess_get,gsi_metguess_bundle
    use gsi_bundlemod, only: gsi_bundlegetpointer
    use mpeu_util, only: die,getindex
    use control_vectors, only: cvars3d
    use native_endianness, only: byte_swap
    use gfs_stratosphere, only: use_gfs_stratosphere,nsig_save,add_gfs_stratosphere
    use read_wrf_mass_guess_mod, only: read_wrf_mass_guess_class
  
    implicit none
  
  ! Declare passed variables here
    class(read_wrf_nmm_guess_class),intent(inout) :: this
    integer(i_kind),intent(in):: mype
  
  ! Declare local parameters
    type(read_wrf_mass_guess_class) :: wrf_mass 
  ! Declare local variables
    character(len=*),parameter :: myname='read_wrf_nmm_binary_guess:: '
    integer(i_kind) kpint,kt,kq,ku,kv
    integer(i_kind) kcwm,kf_ice,kf_rain,kf_rimef
  
  ! NMM variable names stuck in here
    integer(i_kind) mfcst
  
  ! other internal variables
    real(r_single),allocatable:: tempa(:,:)
    real(r_single),allocatable::temp1(:,:)
    real(r_single),allocatable::all_loc(:,:,:)
    integer(i_kind),allocatable::igtype(:),kdim(:),kord(:)
    integer(kind=mpi_offset_kind),allocatable::offset(:)
    integer(kind=mpi_offset_kind) this_offset
    integer(i_kind),allocatable::length(:)
    integer(i_kind) this_length
    integer(i_llong) num_swap
    character(9) wrfges
    character(6) filename 
    integer(i_kind) ifld,im,jm,lm,num_nmm_fields
    integer(i_kind) num_loc_groups,num_j_groups
    integer(i_kind) i,it,j,k
    integer(i_kind) i_pd,i_fis,i_pint,i_t,i_q,i_u,i_v,i_sno,i_u10,i_v10,i_smc,i_stc
    integer(i_kind) i_sm,i_sice,i_sst,i_tsk,i_ivgtyp,i_isltyp,i_vegfrac
    integer(i_kind) i_cwm,i_f_ice,i_f_rain,i_f_rimef
    integer(i_kind) isli_this
    integer(i_kind) nsig_read  
    real(r_kind) pd,psfc_this,sm_this,sice_this,wmag
    integer(i_kind) num_doubtful_sfct,num_doubtful_sfct_all
    integer(i_llong) n_position
    integer(i_kind) iskip,ksize,jextra,nextra
    integer(i_kind) status(mpi_status_size)
    integer(i_kind) jbegin(0:npe),jend(0:npe-1)
    integer(i_kind) kbegin(0:npe),kend(0:npe-1)
    integer(i_long),allocatable:: ibuf(:,:)
    integer(i_long),allocatable:: jbuf(:,:,:)
    real(r_kind) rough0(nlon,nlat)
    real(r_single) rough_in(nlon_regional,nlat_regional)
    real(r_kind) rough_in2(nlon_regional,nlat_regional)
    real(r_kind) work(itotsub)                        
    integer(i_kind) mm1                               
    integer(i_kind) iadd
    character(132) memoryorder
  
  ! variables for cloud info
    integer(i_kind) n_actual_clouds,istatus,ier,iret
    integer(i_kind) iqtotal,icw4crtm
    real(r_kind),dimension(lat2,lon2,nsig):: clwmr,fice,frain,frimef
    real(r_kind),pointer,dimension(:,:  ):: ges_pd  =>NULL()
    real(r_kind),pointer,dimension(:,:  ):: ges_ps  =>NULL()
    real(r_kind),pointer,dimension(:,:  ):: ges_z   =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_u   =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_v   =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_tv  =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_pint=>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_q   =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_cwmr=>NULL()
  
    real(r_kind),pointer,dimension(:,:,:):: ges_ql=>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_qi=>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_qr=>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_qs=>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_qg=>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_qh=>NULL()
  
    associate( this => this ) ! eliminates warning for unused dummy argument needed for binding
    end associate
  !  NMM input grid dimensions in module reg_glob_ll
  !      These are the following:
  !          im -- number of NMM longitudes (x-points) on E-grid
  !          jm -- number of NMM latitudes (y-points) on E-grid
  !          lm -- number of NMM vertical levels ( = nsig for now)
  
  
       im=nlon_regional
       jm=nlat_regional
       lm=nsig
       nsig_read=nsig
       if (use_gfs_stratosphere) then
          lm=nsig_save
          nsig_read=nsig_save
       endif
  
  !    Inquire about cloud guess fields
       call gsi_metguess_get('clouds::3d',n_actual_clouds,istatus)
  
  !    Determine whether or not cloud-condensate is the control variable (ges_cw=ges_ql+ges_qi)
       icw4crtm=getindex(cvars3d,'cw')
  
  !    Determine whether or not total moisture (water vapor+total cloud condensate) is the control variable
       iqtotal=getindex(cvars3d,'qt')
  
  
  !    Following is for convenient NMM/WRF NMM input
       num_nmm_fields=20+4*lm
       if(update_pint) num_nmm_fields=num_nmm_fields+lm+1   ! add contribution of PINT
       if (n_actual_clouds>0) num_nmm_fields=num_nmm_fields+4*lm       ! add hydrometeors
       num_loc_groups=num_nmm_fields/npe
  
       if(mype == 0) write(6,'(" at 1 in read_wrf_nmm_binary_guess, update_pint   =",l6)')update_pint   
       if(mype == 0) write(6,'(" at 1 in read_wrf_nmm_binary_guess, nsig          =",i6)')nsig               
       if(mype == 0) write(6,'(" at 1 in read_wrf_nmm_binary_guess, lm            =",i6)')lm           
       if(mype == 0) write(6,'(" at 1 in read_wrf_nmm_binary_guess, im            =",i6)')im            
       if(mype == 0) write(6,'(" at 1 in read_wrf_nmm_binary_guess, jm            =",i6)')jm                 
       if(mype == 0) write(6,'(" at 1 in read_wrf_nmm_binary_guess, num_nmm_fields=",i6)')num_nmm_fields   
       if(mype == 0) write(6,'(" at 1 in read_wrf_nmm_binary_guess, nfldsig       =",i6)')nfldsig                 
       if(mype == 0) write(6,'(" at 1 in read_wrf_nmm_binary_guess, npe           =",i6)')npe  
       if(mype == 0) write(6,'(" at 1 in read_wrf_nmm_binary_guess, num_loc_groups=",i6)')num_loc_groups          
  
       allocate(offset(num_nmm_fields))
       allocate(igtype(num_nmm_fields),kdim(num_nmm_fields),kord(num_nmm_fields))
       allocate(length(num_nmm_fields))
       allocate(all_loc(lat2,lon2,num_nmm_fields))
       allocate(temp1(im,jm))
  
  !    igtype is a flag indicating whether each input NMM field is h-grid or v-grid
  !    and whether integer or real
  !     abs(igtype)=1 for h-grid
  !                =2 for v-grid
  !     igtype < 0 for integer field
  
  !    offset is the byte count preceding each record to be read from the wrf binary file.
  !       used as individual file pointers by mpi_file_read_at
  
       do it=1,nfldsig
          num_doubtful_sfct=0
          write(filename,'("sigf",i2.2)')ifilesig(it)
          open(lendian_in,file=filename,form='unformatted') ; rewind lendian_in
          if(mype == 0) write(6,*)'READ_WRF_NMM_OFFSET_FILE:  open lendian_in=',&
               lendian_in,' to file=',filename
          do iskip=1,9
             read(lendian_in)
          end do
          read(lendian_in) wrfges
          if(mype==0) write(6,*)' in read_wrf_nmm_binary_guess, wrfges=',trim(wrfges)
          read(lendian_in) ! n_position          !  offset for START_DATE record
          read(lendian_in) ! n_position          !  offset for NSTART_HOUR record
          
          i=0
          i=i+1 ; i_pd=i                                                ! pd
          read(lendian_in) n_position
          offset(i)=n_position ; length=im*jm ; igtype(i)=1 ; kdim(i)=1
          if(mype == 0) write(6,*)' pd, i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
          
          i=i+1 ; i_fis=i                                                ! fis
          read(lendian_in) n_position
          offset(i)=n_position ; length=im*jm ; igtype(i)=1 ; kdim(i)=1
          if(mype == 0) write(6,*)' fis, i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
          
          i_pint=i+1
          if(update_pint) then
             i_pint=i+1
             read(lendian_in) n_position,memoryorder
             do k=1,lm+1
                i=i+1                                                       ! pint(k)
                if(trim(memoryorder)=='XZY') then
                   iadd=0
                   kord(i)=lm+1
                else
                   iadd=(k-1)*im*jm*4
                   kord(i)=1
                end if
                offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=lm+1
                if(mype == 0.and.k==1) write(6,*)' pint i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
             end do
          end if
          
          i_t=i+1
          read(lendian_in) n_position,memoryorder
          do k=1,lm
             i=i+1                                                       ! t(k)
             if(trim(memoryorder)=='XZY') then
                iadd=0
                kord(i)=lm
             else
                iadd=(k-1)*im*jm*4
                kord(i)=1
             end if
             offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=lm
             if(mype == 0.and.k==1) write(6,*)' temp i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
          end do
          
          i_q=i+1
          read(lendian_in) n_position,memoryorder
          do k=1,lm
             i=i+1                                                       ! q(k)
             if(trim(memoryorder)=='XZY') then
                iadd=0
                kord(i)=lm
             else
                iadd=(k-1)*im*jm*4
                kord(i)=1
             end if
             offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=lm
             if(mype == 0.and.k==1) write(6,*)' q i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
          end do
          
          i_u=i+1
          read(lendian_in) n_position,memoryorder
          do k=1,lm
             i=i+1                                                       ! u(k)
             if(trim(memoryorder)=='XZY') then
                iadd=0
                kord(i)=lm
             else
                iadd=(k-1)*im*jm*4
                kord(i)=1
             end if
             offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=2 ; kdim(i)=lm
             if(mype == 0.and.k==1) write(6,*)' u i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
          end do
          
          i_v=i+1
          read(lendian_in) n_position,memoryorder
          do k=1,lm
             i=i+1                                                       ! v(k)
             if(trim(memoryorder)=='XZY') then
                iadd=0
                kord(i)=lm
             else
                iadd=(k-1)*im*jm*4
                kord(i)=1
             end if
             offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=2 ; kdim(i)=lm
             if(mype == 0.and.k==1) write(6,*)' v i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
          end do
          
          i=i+1 ; i_sm=i                                                ! sm
          read(lendian_in) n_position
          offset(i)=n_position ; length=im*jm ; igtype(i)=1 ; kdim(i)=1
          if(mype == 0) write(6,*)' sm, i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
          
          i=i+1 ; i_sice=i                                                ! sice
          read(lendian_in) n_position
          offset(i)=n_position ; length=im*jm ; igtype(i)=1 ; kdim(i)=1
          if(mype == 0) write(6,*)' sice, i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
          
          i=i+1 ; i_sst=i                                                ! sst
          read(lendian_in) n_position
          offset(i)=n_position ; length=im*jm ; igtype(i)=1 ; kdim(i)=1
          if(mype == 0) write(6,*)' sst, i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
          
          i=i+1 ; i_ivgtyp=i                                                ! ivgtyp
          read(lendian_in) n_position
          offset(i)=n_position ; length=im*jm ; igtype(i)=-1 ; kdim(i)=1
          if(mype == 0) write(6,*)' ivgtyp, i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
          
          i=i+1 ; i_isltyp=i                                                ! isltyp
          read(lendian_in) n_position
          offset(i)=n_position ; length=im*jm ; igtype(i)=-1 ; kdim(i)=1
          if(mype == 0) write(6,*)' isltyp, i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
          
          i=i+1 ; i_vegfrac=i                                                ! vegfrac
          read(lendian_in) n_position
          offset(i)=n_position ; length=im*jm ; igtype(i)=1 ; kdim(i)=1
          if(mype == 0) write(6,*)' vegfrac, i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
          
          i=i+1 ; i_sno=i                                                ! sno
          read(lendian_in) n_position
          offset(i)=n_position ; length=im*jm ; igtype(i)=1 ; kdim(i)=1
          if(mype == 0) write(6,*)' sno, i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
          
          i=i+1 ; i_u10=i                                                ! u10
          read(lendian_in) n_position
          offset(i)=n_position ; length=im*jm ; igtype(i)=1 ; kdim(i)=1
          if(mype == 0) write(6,*)' u10, i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
  
          i=i+1 ; i_v10=i                                                ! v10
          read(lendian_in) n_position
          offset(i)=n_position ; length=im*jm ; igtype(i)=1 ; kdim(i)=1
          if(mype == 0) write(6,*)' v10, i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
          
          i=i+1 ; i_smc=i                                             ! smc
          read(lendian_in) n_position,ksize,memoryorder
          if(trim(memoryorder)=='XZY') then
             kord(i)=ksize
          else
             kord(i)=1
          end if
          offset(i)=n_position ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=ksize
          if(mype == 0) write(6,*)' smc i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
          do k=2,ksize
             i=i+1
             if(trim(memoryorder)=='XZY') then
                iadd=0
                kord(i)=ksize
             else
                iadd=(k-1)*im*jm*4
                kord(i)=1
             end if
             offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=ksize
          end do
          
          i=i+1 ; i_stc=i                                             ! stc
          read(lendian_in) n_position,ksize,memoryorder
          if(trim(memoryorder)=='XZY') then
             kord(i)=ksize
          else
             kord(i)=1
          end if
          offset(i)=n_position ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=ksize
          if(mype == 0) write(6,*)' stc i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
          do k=2,ksize
             i=i+1
             if(trim(memoryorder)=='XZY') then
                iadd=0
                kord(i)=ksize
             else
                iadd=(k-1)*im*jm*4
                kord(i)=1
             end if
             offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=ksize
          end do
          
          i=i+1 ; i_tsk=i                                                ! tsk
          read(lendian_in) n_position
          offset(i)=n_position ; length=im*jm ; igtype(i)=1 ; kdim(i)=1
          if(mype == 0) write(6,*)' tsk, i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
  
          if (n_actual_clouds>0) then
             i_cwm=i+1
             read(lendian_in) n_position,memoryorder
             do k=1,lm
                i=i+1                                                       ! cwm(k)
                if(trim(memoryorder)=='XZY') then
                   iadd=0
                   kord(i)=lm
                else
                   iadd=(k-1)*im*jm*4
                   kord(i)=1
                end if
                offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=lm
                if(mype == 0.and.k==1) write(6,*)' cwm i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
             end do
  
             i_f_ice=i+1
             read(lendian_in) n_position,memoryorder
             do k=1,lm
                i=i+1                                                       ! f_ice(k)
                if(trim(memoryorder)=='XZY') then
                   iadd=0
                   kord(i)=lm
                else
                   iadd=(k-1)*im*jm*4
                   kord(i)=1
                end if
                offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=lm
                if(mype == 0.and.k==1) write(6,*)' f_ice i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
             end do
  
             i_f_rain=i+1
             read(lendian_in) n_position,memoryorder
             do k=1,lm
                i=i+1                                                       ! f_rain(k)
                if(trim(memoryorder)=='XZY') then
                   iadd=0
                   kord(i)=lm
                else
                   iadd=(k-1)*im*jm*4
                   kord(i)=1
                end if
                offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=lm
                if(mype == 0.and.k==1) write(6,*)' f_rain i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
             end do
  
             i_f_rimef=i+1
             read(lendian_in) n_position,memoryorder
             do k=1,lm
                i=i+1                                                       ! f_rimef(k)
                if(trim(memoryorder)=='XZY') then
                   iadd=0
                   kord(i)=lm
                else
                   iadd=(k-1)*im*jm*4
                   kord(i)=1
                end if
                offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=lm
                if(mype == 0.and.k==1) write(6,*)' f_rimef i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
             end do
          end if  ! end of n_actual_clouds>0
  
  !       bring in z0 (roughness length)
          mm1=mype+1
          read(lendian_in) rough_in
          if(half_grid) call half_nmm_grid2a(rough_in,nlon_regional,nlat_regional,rough0,1)
          if(filled_grid) then
             rough_in2=rough_in
             call fill_nmm_grid2a3(rough_in2,nlon_regional,nlat_regional,rough0)
          end if
          do k=1,itotsub
             i=ltosi_s(k)
             j=ltosj_s(k)
             work(k)=rough0(j,i)
          end do
          call mpi_scatterv(work,ijn_s,displs_s,mpi_rtype, &
                         sfc_rough,ijn_s(mm1),mpi_rtype,0,mpi_comm_world,ierror)
  
          close(lendian_in)
  
  !    End of stuff from NMM restart file
  
  !          set up evenly distributed index range over all processors for all input fields
  
  
          num_loc_groups=num_nmm_fields/npe
          nextra=num_nmm_fields-num_loc_groups*npe
          kbegin(0)=1
          if(nextra > 0) then
             do k=1,nextra
                kbegin(k)=kbegin(k-1)+1+num_loc_groups
             end do
          end if
          do k=nextra+1,npe
             kbegin(k)=kbegin(k-1)+num_loc_groups
          end do
          do k=0,npe-1
             kend(k)=kbegin(k+1)-1
          end do
!         if(mype == 0) then
!            write(6,*)' kbegin=',kbegin
!            write(6,*)' kend= ',kend
!         end if
          num_j_groups=jm/npe
          jextra=jm-num_j_groups*npe
          jbegin(0)=1
          if(jextra > 0) then
             do j=1,jextra
                jbegin(j)=jbegin(j-1)+1+num_j_groups
             end do
          end if
          do j=jextra+1,npe
             jbegin(j)=jbegin(j-1)+num_j_groups
          end do
          do j=0,npe-1
             jend(j)=min(jbegin(j+1)-1,jm)
          end do
!         if(mype == 0) then
!            write(6,*)' jbegin=',jbegin
!            write(6,*)' jend= ',jend
!         end if
          
          allocate(ibuf(im*jm,kbegin(mype):kend(mype)))
  
          call mpi_file_open(mpi_comm_world,trim(wrfges),mpi_mode_rdonly,mpi_info_null,mfcst,ierror)
          
  !                                    read pint
          if(update_pint.and.kord(i_pint)/=1) then
             allocate(jbuf(im,lm+1,jbegin(mype):jend(mype)))
             this_offset=offset(i_pint)+(jbegin(mype)-1)*4*im*(lm+1)
             this_length=(jend(mype)-jbegin(mype)+1)*im*(lm+1)
             call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer, &
                                   status,ierror)
             if(byte_swap) then
                num_swap=this_length
                call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
             end if
             call wrf_mass%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                  jbegin,jend,kbegin,kend,mype,npe,im,jm,lm+1,im,jm,i_pint,i_pint+lm)
             deallocate(jbuf)
          end if
          
  !                                    read temps
          if(kord(i_t)/=1) then
             allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
             this_offset=offset(i_t)+(jbegin(mype)-1)*4*im*lm
             this_length=(jend(mype)-jbegin(mype)+1)*im*lm
             call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer, &
                                   status,ierror)
             if(byte_swap) then
                num_swap=this_length
                call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
             end if
             call wrf_mass%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                  jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im,jm,i_t,i_t+lm-1)
             deallocate(jbuf)
          end if
  
  !                                    read q
          if(kord(i_q)/=1) then
             allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
             this_offset=offset(i_q)+(jbegin(mype)-1)*4*im*lm
             this_length=(jend(mype)-jbegin(mype)+1)*im*lm
             call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer, &
                                   status,ierror)
             if(byte_swap) then
                num_swap=this_length
                call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
             end if
             call wrf_mass%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                  jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im,jm,i_q,i_q+lm-1)
             deallocate(jbuf)
          end if
  
  !                                    read u
          if(kord(i_u)/=1) then
             allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
             this_offset=offset(i_u)+(jbegin(mype)-1)*4*im*lm
             this_length=(jend(mype)-jbegin(mype)+1)*im*lm
             call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer, &
                                   status,ierror)
             if(byte_swap) then
                num_swap=this_length
                call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
             end if
             call wrf_mass%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                  jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im,jm,i_u,i_u+lm-1)
             deallocate(jbuf)
          end if
  
  !                                    read v
          if(kord(i_v)/=1) then
             allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
             this_offset=offset(i_v)+(jbegin(mype)-1)*4*im*lm
             this_length=(jend(mype)-jbegin(mype)+1)*im*lm
             call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer, &
                                   status,ierror)
             if(byte_swap) then
                num_swap=this_length
                call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
             end if
             call wrf_mass%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                  jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im,jm,i_v,i_v+lm-1)
             deallocate(jbuf)
          end if
  
  !                                    read smc
          if(kord(i_smc)/=1) then
             allocate(jbuf(im,ksize,jbegin(mype):jend(mype)))
             this_offset=offset(i_smc)+(jbegin(mype)-1)*4*im*ksize
             this_length=(jend(mype)-jbegin(mype)+1)*im*ksize
             call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer, &
                                   status,ierror)
             if(byte_swap) then
                num_swap=this_length
                call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
             end if
             call wrf_mass%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                  jbegin,jend,kbegin,kend,mype,npe,im,jm,ksize,im,jm,i_smc,i_smc)
             deallocate(jbuf)
          end if
  
  !                                    read stc
          if(kord(i_stc)/=1) then
             allocate(jbuf(im,ksize,jbegin(mype):jend(mype)))
             this_offset=offset(i_stc)+(jbegin(mype)-1)*4*im*ksize
             this_length=(jend(mype)-jbegin(mype)+1)*im*ksize
             call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer, &
                                   status,ierror)
             if(byte_swap) then
                num_swap=this_length
                call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
             end if
             call wrf_mass%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                  jbegin,jend,kbegin,kend,mype,npe,im,jm,ksize,im,jm,i_stc,i_stc)
             deallocate(jbuf)
          end if
  
          if (n_actual_clouds>0) then
  !                                    read cwm
             if(kord(i_cwm)/=1) then
                allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
                this_offset=offset(i_cwm)+(jbegin(mype)-1)*4*im*lm
                this_length=(jend(mype)-jbegin(mype)+1)*im*lm
                call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer, &
                                      status,ierror)
                if(byte_swap) then
                   num_swap=this_length
                   call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
                end if
                call wrf_mass%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                     jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im,jm,i_cwm,i_cwm+lm-1)
                deallocate(jbuf)
             end if
  
  !                                    read f_ice
             if(kord(i_f_ice)/=1) then
                allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
                this_offset=offset(i_f_ice)+(jbegin(mype)-1)*4*im*lm
                this_length=(jend(mype)-jbegin(mype)+1)*im*lm
                call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer, &
                                      status,ierror)
                if(byte_swap) then
                   num_swap=this_length
                   call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
                end if
                call wrf_mass%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                     jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im,jm,i_f_ice,i_f_ice+lm-1)
                deallocate(jbuf)
             end if
  
  !                                    read f_rain
             if(kord(i_f_rain)/=1) then
                allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
                this_offset=offset(i_f_rain)+(jbegin(mype)-1)*4*im*lm
                this_length=(jend(mype)-jbegin(mype)+1)*im*lm
                call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer, &
                                      status,ierror)
                if(byte_swap) then
                   num_swap=this_length
                   call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
                end if
                call wrf_mass%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                     jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im,jm,i_f_rain,i_f_rain+lm-1)
                deallocate(jbuf)
             end if
  
  !                                    read f_rimef
             if(kord(i_f_rimef)/=1) then
                allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
                this_offset=offset(i_f_rimef)+(jbegin(mype)-1)*4*im*lm
                this_length=(jend(mype)-jbegin(mype)+1)*im*lm
                call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer, &
                                      status,ierror)
                if(byte_swap) then
                   num_swap=this_length
                   call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
                end if
                call wrf_mass%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                     jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im,jm,i_f_rimef,i_f_rimef+lm-1)
                deallocate(jbuf)
             end if
          end if
  
  !---------------------- read surface files last
          do k=kbegin(mype),kend(mype)
             if(kdim(k)==1.or.kord(k)==1) then
                call mpi_file_read_at(mfcst,offset(k),ibuf(1,k),length(k),mpi_integer,status,ierror)
                if(byte_swap) then
                   num_swap=length(k)
                   call to_native_endianness_i4(ibuf(1,k),num_swap)
                end if
             end if
          end do
  
          call mpi_file_close(mfcst,ierror)
  
  !   next interpolate to analysis grid, then distribute to subdomains
  
          allocate(tempa(itotsub,kbegin(mype):kend(mype)))
          do ifld=kbegin(mype),kend(mype)
             if(igtype(ifld) >  0) then
                call wrf_mass%move_ibuf_hg(ibuf(1,ifld),temp1,im,jm,im,jm)
             else
                call wrf_mass%move_ibuf_ihg(ibuf(1,ifld),temp1,im,jm,im,jm)
             end if
             if(filled_grid) call fill_nmm_grid2(temp1,im,jm,tempa(1,ifld),abs(igtype(ifld)),1)
             if(half_grid)   call half_nmm_grid2(temp1,im,jm,tempa(1,ifld),abs(igtype(ifld)),1)
          end do
          deallocate(ibuf)
  
          call wrf_mass%generic_grid2sub(tempa,all_loc,kbegin(mype),kend(mype),kbegin,kend,mype,num_nmm_fields)
       
          deallocate(tempa)
  !    Next do conversion of units as necessary and
  !    reorganize into WeiYu's format--
  
          ier=0
          call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'ps' ,ges_ps ,istatus );ier=ier+istatus
          call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'z' , ges_z  ,istatus );ier=ier+istatus
          call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'u' , ges_u  ,istatus );ier=ier+istatus
          call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'v' , ges_v  ,istatus );ier=ier+istatus
          call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'tv' ,ges_tv ,istatus );ier=ier+istatus
          call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'q'  ,ges_q  ,istatus );ier=ier+istatus
          if (ier/=0) call die(trim(myname),'cannot get pointers for met-fields, ier =',ier)
  
  !       Get pointer to cloud water mixing ratio
          if (n_actual_clouds>0) then
             call gsi_bundlegetpointer (gsi_metguess_bundle(it),'ql',ges_ql,iret); ier=iret
             call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qi',ges_qi,iret); ier=ier+iret
             call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qr',ges_qr,iret); ier=ier+iret
             call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qs',ges_qs,iret); ier=ier+iret
             call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qg',ges_qg,iret); ier=ier+iret
             call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qh',ges_qh,iret); ier=ier+iret
          end if
  
          kt=i_t-1
          kq=i_q-1
          ku=i_u-1
          kv=i_v-1
          if (n_actual_clouds>0) then 
             kcwm=i_cwm-1
             kf_ice=i_f_ice-1
             kf_rain=i_f_rain-1
             kf_rimef=i_f_rimef-1
          end if
          do k=1,nsig_read  
             kt=kt+1      
             kq=kq+1
             ku=ku+1
             kv=kv+1
             if (n_actual_clouds>0) then
                kcwm=kcwm+1
                kf_ice=kf_ice+1
                kf_rain=kf_rain+1
                kf_rimef=kf_rimef+1
             end if
             do i=1,lon2
                do j=1,lat2
                   ges_u(j,i,k) = real(all_loc(j,i,ku),r_kind)
                   ges_v(j,i,k) = real(all_loc(j,i,kv),r_kind)
                   ges_q(j,i,k)   = real(all_loc(j,i,kq),r_kind)
                   ges_tsen(j,i,k,it)  =real(all_loc(j,i,kt),r_kind) ! actually holds sensible temperature
  
                   if (n_actual_clouds>0) then
                      clwmr(j,i,k) = real(all_loc(j,i,kcwm),r_kind)
                      fice(j,i,k) = real(all_loc(j,i,kf_ice),r_kind)
                      frain(j,i,k) = real(all_loc(j,i,kf_rain),r_kind)
                      frimef(j,i,k) = real(all_loc(j,i,kf_rimef),r_kind)
                   end if
                end do
             end do
             if (n_actual_clouds>0 .and. (icw4crtm>0 .or. iqtotal>0) .and. ier==0) then 
                do i=1,lon2
                   do j=1,lat2
                      ges_prsl(j,i,k,it)=one_tenth* &
                                  (aeta1_ll(k)*pdtop_ll + &
                                   aeta2_ll(k)*(ten*ges_ps(j,i)-pdtop_ll-pt_ll) + &
                                   pt_ll)
                   end do
                end do
                call cloud_calc(ges_prsl(:,:,k,it),ges_q(:,:,k),ges_tsen(:,:,k,it),clwmr(:,:,k), &
                     fice(:,:,k),frain(:,:,k),frimef(:,:,k), &
                     ges_ql(:,:,k),ges_qi(:,:,k),ges_qr(:,:,k),ges_qs(:,:,k),ges_qg(:,:,k),ges_qh(:,:,k), &
                     efr_ql(:,:,k,it),efr_qi(:,:,k,it),efr_qr(:,:,k,it),efr_qs(:,:,k,it),efr_qg(:,:,k,it),efr_qh(:,:,k,it))
             end if
          end do
          do k=nsig_read+1,nsig
             do i=1,lon2
                do j=1,lat2
                   ges_u(j,i,k)    = zero
                   ges_v(j,i,k)    = zero
                   ges_q(j,i,k)    = zero
                   ges_tsen(j,i,k,it) = zero
                   if (n_actual_clouds>0) then
                      clwmr(j,i,k)  = zero
                      fice(j,i,k)   = zero
                      frain(j,i,k)  = zero
                      frimef(j,i,k) = zero
                   end if
                end do
             end do
          end do
  
          if (n_actual_clouds>0) then
             call gsi_bundlegetpointer (gsi_metguess_bundle(it),'cw',ges_cwmr,iret)
             if (iret==0) ges_cwmr=clwmr
          end if
  
          do i=1,lon2
             do j=1,lat2
                ges_z(j,i)    = real(all_loc(j,i,i_fis),r_kind)/grav ! NMM surface elevation multiplied by g
                
  !             convert wrf nmm pd variable to psfc in mb, and then to log(psfc) in cb
                
                pd=r0_01*real(all_loc(j,i,i_pd),r_kind)
                psfc_this=pd+pdtop_ll+pt_ll
                ges_ps(j,i)=one_tenth*psfc_this   ! convert from mb to cb
                if(j == 116 .and. i== 12)write(6,*) ' ges ps ',mype,ges_ps(j,i)
                sno(j,i,it)=real(all_loc(j,i,i_sno),r_kind)
                soil_moi(j,i,it)=real(all_loc(j,i,i_smc),r_kind)
                soil_temp(j,i,it)=real(all_loc(j,i,i_stc),r_kind)
             end do
          end do
          if(update_pint) then
             ier=0
             call gsi_bundlegetpointer(gsi_metguess_bundle(it),'pint', ges_pint, istatus)
             ier=ier+istatus
             call gsi_bundlegetpointer(gsi_metguess_bundle(it),'pd'  , ges_pd  , istatus)
             ier=ier+istatus
             if (ier/=0) then ! doesn't need to die (but needs careful revision)
                call die(myname,': missing pint/pd fields',ier)
             endif
             kpint=i_pint-1
             do k=1,nsig_read+1 
                kpint=kpint+1
                do i=1,lon2
                   do j=1,lat2
                      ges_pint(j,i,k)  = real(all_loc(j,i,kpint),r_kind)
                   end do
                end do
             end do
             do k=nsig_read+2,nsig+1 
                do i=1,lon2
                   do j=1,lat2
                      ges_pint(j,i,k)  = zero
                   enddo
                enddo
             enddo
             do i=1,lon2
                do j=1,lat2
                   ges_pd(j,i)=real(all_loc(j,i,i_pd),r_kind)
                end do
             end do
          end if
  
  !       Convert sensible temp to virtual temp
          do k=1,nsig_read       
             do i=1,lon2       
                do j=1,lat2
                   ges_tv(j,i,k) = ges_tsen(j,i,k,it) * (one+fv*ges_q(j,i,k))
                end do
             end do
          end do
             do k=nsig_read+1,nsig 
                do i=1,lon2
                   do j=1,lat2
                      ges_tv(j,i,k)  = zero
                   enddo
                enddo
            enddo
       
  !    Transfer surface fields
          do i=1,lon2
             do j=1,lat2
                fact10(j,i,it)=one    !  later fix this by using correct w10/w(1)
                wmag=sqrt(ges_u(j,i,1)**2+ges_v(j,i,1)**2)
                if(wmag > zero)fact10(j,i,it)=sqrt(real(all_loc(j,i,i_u10),r_kind)**2 + &
                        real(all_loc(j,i,i_v10),r_kind)**2)/wmag
                fact10(j,i,it)=min(max(fact10(j,i,it),half),0.95_r_kind)
                veg_type(j,i,it)=real(all_loc(j,i,i_ivgtyp),r_kind)
                veg_frac(j,i,it)=real(all_loc(j,i,i_vegfrac),r_kind)
                soil_type(j,i,it)=real(all_loc(j,i,i_isltyp),r_kind)
                sm_this=zero
                if(all_loc(j,i,i_sm) /= zero_single) sm_this=one
                sice_this=zero
                if(all_loc(j,i,i_sice) /= zero_single) sice_this=one
                
                isli_this=0
                if(sice_this == one) isli_this=2
                if(sice_this == zero.and.sm_this == zero) isli_this=1
                isli(j,i,it)=isli_this
                
                sfct(j,i,it)=real(all_loc(j,i,i_sst),r_kind)
                if(isli(j,i,it) /= 0) sfct(j,i,it)=real(all_loc(j,i,i_tsk),r_kind)
                if(sfct(j,i,it) < one) then
  
  !             For now, replace missing skin temps with 1st sigma level temp
                   sfct(j,i,it)=real(all_loc(j,i,i_t),r_kind)
                   num_doubtful_sfct=num_doubtful_sfct+1
                   if(num_doubtful_sfct <= 100) &
                        write(6,*)' doubtful skint replaced with 1st sigma level t, j,i,mype,sfct=',&
                        j,i,mype,sfct(j,i,it)
                end if
             end do
          end do
  
       
          call mpi_reduce(num_doubtful_sfct,num_doubtful_sfct_all,1,mpi_integer,mpi_sum,&
               0,mpi_comm_world,ierror)
          if(mype == 0) write(6,*)' in read_wrf_nmm_binary_guess, num_doubtful_sfct_all = ',num_doubtful_sfct_all
          if(mype == 0) write(6,*)' in read_wrf_nmm_binary_guess, num_doubtful_sfct_all = ',num_doubtful_sfct_all
       end do ! enddo it     
       deallocate(all_loc)
       deallocate(temp1,igtype,kdim,kord,offset,length)
       
       if (use_gfs_stratosphere) then
          if (mype==0) write(6,*)'in read_wrf_binary_netcdf: use_gfs_stratosphere ...beg'                          
          call add_gfs_stratosphere
          if (mype==0) write(6,*)'in read_wrf_binary_netcdf: use_gfs_stratosphere ...end'                         
       endif
  
       return 
  end subroutine read_wrf_nmm_binary_guess_wrf
  
  subroutine read_wrf_nmm_netcdf_guess_wrf(this,mype)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    read_wrf_nmm_netcdf_guess        read wrf_nmm interface file
  !   prgmmr: parrish          org: np22                date: 2003-09-05
  !
  ! abstract: in place of read_guess for global application, read guess
  !             from regional model, in this case the wrf nmm (non-hydrostatic
  !             mesoscale model).  This version reads a binary file created
  !             in a previous step that interfaces with the wrf infrastructure.
  !             A later version will read directly from the wrf restart file.
  !             The guess is read in by complete horizontal fields, one field
  !             per processor, in parallel.  Each horizontal input field is 
  !             converted from the staggered e-grid to an unstaggered a-grid.
  !             If filled_grid=.true., then the unstaggered a-grid has all the
  !             holes of the e-grid filled, and has twice as many points as the
  !             input grid.  If half_grid=.true., then the unstaggered grid is
  !             derived from every other row of the input e-grid, and has 1/2 as
  !             many points as the input grid.
  !
  ! program history log:
  !   2003-09-05  parrish
  !   2004-06-22  parrish, document
  !   2004-08-02  treadon - add only to module use, add intent in/out
  !   2005-02-23  wu - setup for qoption=2 and output stats of RH
  !   2005-04-01  treadon - add initialization of ges_oz, prsi_oz; comestic format changes
  !   2005-05-27  parrish - add call get_derivatives
  !   2005-07_06  parrish - add changes to read pint if available (update_pint=.true.)
  !   2005-11-21  derber - make qoption=1 work same as qoption=2
  !   2005-11-29  derber - remove external iteration dependent calculations
  !   2005-12-15  treadon - remove initialization of certain guess arrays (done elsewhere)
  !   2006-02-02  treadon - remove load_prsges,load_geop_hgt,prsi,prsl (not used)
  !   2006-03-06  parrish - correct u10,v10 grid type flag, igtype.  s/b 1, not 2
  !   2006-04-03  derber  - include tuned fact10 for 10m winds
  !   2006-04-06  middlecoff - changed nfcst from 11 to lendian_in
  !   2006-07-28  derber  - include sensible temperature
  !   2006-07-31  kleist - change to use ges_ps instead of lnps
  !   2007-03-13  derber - remove unused qsinv2 from jfunc use list
  !   2008-04-16  safford - rm unused uses
  !   2012-10-11  eliu - add capability of uing gfs-regional blended vertical coordinate 
  !                      for wrf_nmm_regional (HWRF)                 
  !   2012-11-30  tong  - added the calculation of ges_prsl for the caculation of cloud mixing ratio,
  !                       because load_prsges is called after this subroutine is called.
  !   2013-10-13  todling - efr_q vars now in cloud_efr module (update mod name too)
  !
  !   input argument list:
  !     mype     - pe number
  !
  !     NOTES:  need to pay special attention to various surface fields to make sure
  !             they are correct (check units).  fact10 needs to be computed from 
  !             10m wind, which is included in wrf nmm interface file.
  !
  !             Cloud water currently set to zero.  Need to fix before doing precip
  !             assimilation--wait until global adopts Brad Ferrier's multi-species 
  !             scheme??
  !
  !             Ozone currently set to zero.  No ozone variable in wrf nmm--climatology
  !             instead.  Do we use climatology?
  !
  !             No background bias yet. (biascor ignored)
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$
    use kinds, only: r_kind,r_single,i_kind
    use mpimod, only: ierror,mpi_integer,mpi_sum,mpi_real4,mpi_comm_world,npe
    use guess_grids, only: &
         fact10,soil_type,veg_frac,veg_type,sfct,sno,soil_temp,soil_moi,&
         isli,nfldsig,ifilesig,ges_tsen,ges_prsl,sfc_rough
    use cloud_efr_mod, only: efr_ql,efr_qi,efr_qr,efr_qs,efr_qg,efr_qh
    use cloud_efr_mod, only: cloud_calc
    use gridmod, only: lat2,lon2,itotsub,displs_s,ijn_s,&
         pdtop_ll,pt_ll,nlon_regional,nsig,nlat_regional,half_grid,&
         filled_grid,aeta1_ll,aeta2_ll
    use constants, only: zero,one_tenth,half,one,grav,fv,zero_single,r0_01,ten
    use wrf_params_mod, only: update_pint
    use gsi_io, only: lendian_in
    use gfs_stratosphere, only: use_gfs_stratosphere,nsig_save,good_o3mr,add_gfs_stratosphere
    use gsi_metguess_mod, only: gsi_metguess_get,gsi_metguess_bundle
    use gsi_bundlemod, only: gsi_bundlegetpointer
    use mpeu_util, only: die,getindex
    use control_vectors, only: cvars3d
    use gsi_io, only: verbose
    implicit none
  
  ! Declare passed variables here
    class(read_wrf_nmm_guess_class),intent(inout) :: this
    integer(i_kind),intent(in):: mype
  
  ! Declare local parameters
    character(len=*),parameter :: myname='read_wrf_nmm_netcdf_guess:: '
  
  ! Declare local variables
    integer(i_kind) kpint,kt,kq,ku,kv
    integer(i_kind) kcwm,kf_ice,kf_rain,kf_rimef
  
  ! NMM variable names stuck in here
  
  ! other internal variables
    real(r_single) tempa(itotsub)
    real(r_single),allocatable::temp1(:,:)
    real(r_single),allocatable::all_loc(:,:,:)
    integer(i_kind),allocatable::itemp1(:,:)
    integer(i_kind),allocatable::igtype(:),jsig_skip(:)
    character(60),allocatable::identity(:)
    character(6) filename 
    integer(i_kind) irc_s_reg(npe),ird_s_reg(npe)
    integer(i_kind) ifld,im,jm,lm,num_nmm_fields
    integer(i_kind) num_all_fields,num_loc_groups,num_all_pad
    integer(i_kind) i,icount,icount_prev,it,j,k
    integer(i_kind) i_0,i_pd,i_fis,i_pint,i_t,i_q,i_u,i_v,i_sno,i_u10,i_v10,i_smc,i_stc
    integer(i_kind) i_sm,i_sice,i_sst,i_tsk,i_ivgtyp,i_isltyp,i_vegfrac
    integer(i_kind) i_cwm,i_f_ice,i_f_rain,i_f_rimef
    integer(i_kind) isli_this
    integer(i_kind) nsig_read 
    real(r_kind) pd,psfc_this,sm_this,sice_this,wmag
    integer(i_kind) num_doubtful_sfct,num_doubtful_sfct_all
  
  ! variables for cloud info
    integer(i_kind) n_actual_clouds,istatus,ier,iret
    integer(i_kind) iqtotal,icw4crtm
    real(r_kind),dimension(lat2,lon2,nsig):: clwmr,fice,frain,frimef
    real(r_kind),pointer,dimension(:,:  ):: ges_pd  =>NULL()
    real(r_kind),pointer,dimension(:,:  ):: ges_ps  =>NULL()
    real(r_kind),pointer,dimension(:,:  ):: ges_z   =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_u   =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_v   =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_tv  =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_pint=>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_q   =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_oz  =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_cwmr=>NULL()
  
    real(r_kind),pointer,dimension(:,:,:):: ges_ql=>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_qi=>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_qr=>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_qs=>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_qg=>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_qh=>NULL()

    real(r_kind),pointer,dimension(:,:,:):: ges_fice=>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_frain=>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_frimef=>NULL()
    logical print_verbose
  
    associate( this => this ) ! eliminates warning for unused dummy argument needed for binding
    end associate
  !  NMM input grid dimensions in module reg_glob_ll
  !      These are the following:
  !          im -- number of NMM longitudes (x-points) on E-grid
  !          jm -- number of NMM latitudes (y-points) on E-grid
  !          lm -- number of NMM vertical levels ( = nsig for now)
  
       print_verbose=.false.
       if(verbose)print_verbose=.true.
       sfc_rough = 0.05_r_kind   !default value
       good_o3mr=.false.  ! no input guess for ozone; will use gfs ozone
  
       num_doubtful_sfct=0
  
       im=nlon_regional
       jm=nlat_regional
       if(use_gfs_stratosphere) then
          nsig_read=nsig_save
          lm=nsig_save
       else
          nsig_read=nsig
          lm=nsig
       end if
  
  !    Inquire about cloud guess fields
       call gsi_metguess_get('clouds::3d',n_actual_clouds,istatus)
  
  !    Determine whether or not cloud-condensate is the control variable (ges_cw=ges_ql+ges_qi)
       icw4crtm=getindex(cvars3d,'cw')
  
  !    Determine whether or not total moisture (water vapor+total cloud condensate) is the control variable
       iqtotal=getindex(cvars3d,'qt')
  
  !    Following is for convenient NMM/WRF NMM input
       num_nmm_fields=14+4*lm
       if(update_pint) num_nmm_fields=num_nmm_fields+lm+1   ! add contribution of PINT
       if (n_actual_clouds>0) num_nmm_fields=num_nmm_fields+4*lm
       num_all_fields=num_nmm_fields*nfldsig
       num_loc_groups=num_all_fields/npe
  
       if(mype == 0 .and. print_verbose) then
          write(6,'(" at 1 in read_wrf_nmm_netcdf_guess, lm            =",i6)')lm
          write(6,'(" at 1 in read_wrf_nmm_netcdf_guess, num_nmm_fields=",i6)')num_nmm_fields
          write(6,'(" at 1 in read_wrf_nmm_netcdf_guess, nfldsig       =",i6)')nfldsig
          write(6,'(" at 1 in read_wrf_nmm_netcdf_guess, num_all_fields=",i6)')num_all_fields
          write(6,'(" at 1 in read_wrf_nmm_netcdf_guess, npe           =",i6)')npe
          write(6,'(" at 1 in read_wrf_nmm_netcdf_guess, num_loc_groups=",i6)')num_loc_groups
       end if
       do 
          num_all_pad=num_loc_groups*npe
          if(num_all_pad >= num_all_fields) exit
          num_loc_groups=num_loc_groups+1
       end do
       if(mype == 0 .and. print_verbose) then
           write(6,'(" at 1 in read_wrf_nmm_netcdf_guess, num_all_pad   =",i6)')num_all_pad
           write(6,'(" at 1 in read_wrf_nmm_netcdf_guess, num_loc_groups=",i6)')num_loc_groups
       end if
  
       allocate(all_loc(lat2,lon2,num_all_pad))
       allocate(jsig_skip(num_nmm_fields))
       allocate(igtype(num_nmm_fields))
       allocate(identity(num_nmm_fields))
  
  !    igtype is a flag indicating whether each input NMM field is h-grid or v-grid
  !    and whether integer or real
  !     abs(igtype)=1 for h-grid
  !                =2 for v-grid
  !     igtype < 0 for integer field
  
       i=0
       i=i+1 ; i_pd=i                                                ! pd
       write(identity(i),'("record ",i3,"--pd")')i
       jsig_skip(i)=9     ! number of files to skip before getting to pd
       igtype(i)=1
       i=i+1 ; i_fis=i                                               ! fis
       write(identity(i),'("record ",i3,"--fis")')i
       jsig_skip(i)=0
       igtype(i)=1
  
       if(update_pint) then
          i_pint=i+1
          do k=1,lm+1
             i=i+1                                                       ! pint(k)
             write(identity(i),'("record ",i3,"--pint(",i2,")")')i,k
             jsig_skip(i)=0
             igtype(i)=1
          end do
       end if
  
       i_t=i+1
       do k=1,lm
          i=i+1                                                       ! t(k)
          write(identity(i),'("record ",i3,"--t(",i2,")")')i,k
          jsig_skip(i)=0
          igtype(i)=1
       end do
       i_q=i+1
       do k=1,lm
          i=i+1                                                       ! q(k)
          write(identity(i),'("record ",i3,"--q(",i2,")")')i,k
          jsig_skip(i)=0 ; igtype(i)=1
       end do
       i_u=i+1
       do k=1,lm
          i=i+1                                                       ! u(k)
          write(identity(i),'("record ",i3,"--u(",i2,")")')i,k
          jsig_skip(i)=0 ; igtype(i)=2
       end do
       i_v=i+1
       do k=1,lm
          i=i+1                                                       ! v(k)
          write(identity(i),'("record ",i3,"--v(",i2,")")')i,k
          jsig_skip(i)=0 ; igtype(i)=2
       end do
       i=i+1   ; i_sm=i                                              ! sm
       write(identity(i),'("record ",i3,"--sm")')i
       jsig_skip(i)=0 ; igtype(i)=1
       i=i+1 ; i_sice=i                                              ! sice
       write(identity(i),'("record ",i3,"--sice")')i
       jsig_skip(i)=0 ; igtype(i)=1
       i=i+1 ; i_sst=i                                               ! sst
       write(identity(i),'("record ",i3,"--sst")')i
       jsig_skip(i)=0 ; igtype(i)=1
       i=i+1 ; i_ivgtyp=i                                            ! ivgtyp
       write(identity(i),'("record ",i3,"--ivgtyp")')i
       jsig_skip(i)=0 ; igtype(i)=-1
       i=i+1 ; i_isltyp=i                                            ! isltyp
       write(identity(i),'("record ",i3,"--isltyp")')i
       jsig_skip(i)=0 ; igtype(i)=-1
       i=i+1 ; i_vegfrac=i                                           ! vegfrac
       write(identity(i),'("record ",i3,"--vegfrac")')i
       jsig_skip(i)=0 ; igtype(i)=1
       i=i+1 ; i_sno=i                                               ! sno
       write(identity(i),'("record ",i3,"--sno")')i
       jsig_skip(i)=0 ; igtype(i)=1
       i=i+1 ; i_u10=i                                               ! u10
       write(identity(i),'("record ",i3,"--u10")')i
       jsig_skip(i)=0 ; igtype(i)=1
       i=i+1 ; i_v10=i                                               ! v10
       write(identity(i),'("record ",i3,"--v10")')i
       jsig_skip(i)=0 ; igtype(i)=1
       i=i+1 ; i_smc=i                                               ! smc
       write(identity(i),'("record ",i3,"--smc(",i2,")")')i,k
       jsig_skip(i)=0 ; igtype(i)=1
       i=i+1 ; i_stc=i                                               ! stc
       write(identity(i),'("record ",i3,"--stc(",i2,")")')i,k
       jsig_skip(i)=0 ; igtype(i)=1
       i=i+1 ; i_tsk=i                                               ! tsk
       write(identity(i),'("record ",i3,"--sst")')i
       jsig_skip(i)=0 ; igtype(i)=1
       if (n_actual_clouds>0) then
          i_cwm=i+1
          do k=1,lm
             i=i+1                                                   ! cwm(k)
             write(identity(i),'("record ",i3,"--cwm(",i2,")")')i,k
             jsig_skip(i)=0
             igtype(i)=1
          end do
          i_f_ice=i+1
          do k=1,lm
             i=i+1                                                   ! f_ice(k)
             write(identity(i),'("record ",i3,"--f_ice(",i2,")")')i,k
             jsig_skip(i)=0
             igtype(i)=1
          end do
          i_f_rain=i+1
          do k=1,lm
             i=i+1                                                   ! f_rain(k)
             write(identity(i),'("record ",i3,"--f_rain(",i2,")")')i,k
             jsig_skip(i)=0
             igtype(i)=1
          end do
          i_f_rimef=i+1
          do k=1,lm
             i=i+1                                                   ! f_rimef(k)
             write(identity(i),'("record ",i3,"--f_rimef(",i2,")")')i,k
             jsig_skip(i)=0
             igtype(i)=1
          end do
       end if  ! end of n_actual_clouds>0
  
  !    End of stuff from NMM restart file
  
       allocate(temp1(im,jm),itemp1(im,jm))
       
       do i=1,npe
          irc_s_reg(i)=ijn_s(mype+1)
       end do
       ird_s_reg(1)=0
       do i=1,npe
          if(i /= 1) ird_s_reg(i)=ird_s_reg(i-1)+irc_s_reg(i-1)
       end do
       
  !    Read wrf NMM fixed format file created from external interface
  !    This is done by reading in parallel from every pe, and redistributing
  !    to local domains once for every npe fields read in, using 
  !    mpi_all_to_allv
  
  !     if (mype==0) then
  !        do ifld=1,num_nmm_fields
  !           write(6,'("ifld,identity,icount,icount_prev,jsig_skip:",i12,a30,2x,i12,2x,i12)') &
  !                       ifld,identity(ifld),jsig_skip(ifld),igtype(ifld)
  !        enddo
  !        do i=1,npe
  !          write(6,'(" i,irec_s_reg,ird_s_reg = ",i12,2x,i12,2x,i12)') &
  !                       i,irc_s_reg(i),ird_s_reg(i)
  !        enddo
  !     endif
  
       icount=0
       icount_prev=1
       do it=1,nfldsig
          write(filename,'("sigf",i2.2)')ifilesig(it)
          open(lendian_in,file=filename,form='unformatted') ; rewind lendian_in
          if (mype==0) &
          write(6,*) 'in read_wrf_nmm_netcdf: it filename = ', it, filename
  
  !       Read, interpolate, and distribute NMM restart fields
          do ifld=1,num_nmm_fields
             icount=icount+1
             if(jsig_skip(ifld) > 0) then
                do i=1,jsig_skip(ifld)
                   read(lendian_in)
                end do
             end if
             if(mype == mod(icount-1,npe)) then
                if(igtype(ifld) > 0) then
                   read(lendian_in)((temp1(i,j),i=1,im),j=1,jm)
                else
                   read(lendian_in)((itemp1(i,j),i=1,im),j=1,jm)
                   do j=1,jm
                      do i=1,im
                         temp1(i,j)=itemp1(i,j)
                      end do
                   end do
                end if
!               write(6,'(" ifld, temp1(im/2,jm/2)=",i6,5x,a30,e30.20)')ifld,identity(ifld),temp1(im/2,jm/2)                                                  
                if(filled_grid) call fill_nmm_grid2(temp1,im,jm,tempa,abs(igtype(ifld)),1)
                if(half_grid)   call half_nmm_grid2(temp1,im,jm,tempa,abs(igtype(ifld)),1)
  
             else
                read(lendian_in)
             end if
             
  !          Distribute to local domains everytime we have npe fields
             if(mod(icount,npe) == 0.or.icount == num_all_fields) then
                call mpi_alltoallv(tempa,ijn_s,displs_s,mpi_real4, &
                     all_loc(1,1,icount_prev),irc_s_reg,ird_s_reg,mpi_real4,mpi_comm_world,ierror)
                icount_prev=icount+1
             end if
  
          end do
          close(lendian_in)
       end do
  
  
  !    Next do conversion of units as necessary and
  !    reorganize into WeiYu's format--
  
       do it=1,nfldsig
  
  !       Get pointers to typical met-fields
          ier=0
          call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'ps' ,ges_ps ,istatus );ier=ier+istatus
          call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'z' , ges_z  ,istatus );ier=ier+istatus
          call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'u' , ges_u  ,istatus );ier=ier+istatus
          call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'v' , ges_v  ,istatus );ier=ier+istatus
          call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'tv' ,ges_tv ,istatus );ier=ier+istatus
          call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'q'  ,ges_q  ,istatus );ier=ier+istatus
          call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'oz' ,ges_oz ,istatus );ier=ier+istatus
          if (ier/=0) call die(trim(myname),'cannot get pointers for met-fields, ier =',ier)
  
  !       Get pointer to cloud water mixing ratio
          if (n_actual_clouds>0) then
             call gsi_bundlegetpointer (gsi_metguess_bundle(it),'ql',ges_ql,iret); ier=iret
             call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qi',ges_qi,iret); ier=ier+iret
             call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qr',ges_qr,iret); ier=ier+iret
             call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qs',ges_qs,iret); ier=ier+iret
             call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qg',ges_qg,iret); ier=ier+iret
             call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qh',ges_qh,iret); ier=ier+iret
          end if
  
          i_0=(it-1)*num_nmm_fields
          kt=i_0+i_t-1
          kq=i_0+i_q-1
          ku=i_0+i_u-1
          kv=i_0+i_v-1
          if (n_actual_clouds>0) then
             kcwm=i_0+i_cwm-1
             kf_ice=i_0+i_f_ice-1
             kf_rain=i_0+i_f_rain-1
             kf_rimef=i_0+i_f_rimef-1
          end if
  
          do i=1,lon2
             do j=1,lat2
                ges_z(j,i)    = real(all_loc(j,i,i_0+i_fis),r_kind)/grav ! NMM surface elevation multiplied by g
  
  !             convert wrf nmm pd variable to psfc in mb, and then to log(psfc) in cb
                
                pd=r0_01*real(all_loc(j,i,i_0+i_pd),r_kind)
                psfc_this=pd+pdtop_ll+pt_ll
                ges_ps(j,i)=one_tenth*psfc_this   ! convert from mb to cb
!               if(j+istart(mype+1)-2 == 116 .and. i== 12)write(6,*) ' ges ps ',mype,ges_ps(j,i)
! ix=ix1-istart(m1)+2
! iy=iy1-jstart(m1)+2

                sno(j,i,it)=real(all_loc(j,i,i_0+i_sno),r_kind)
                soil_moi(j,i,it)=real(all_loc(j,i,i_0+i_smc),r_kind)
                soil_temp(j,i,it)=real(all_loc(j,i,i_0+i_stc),r_kind)
             end do
          end do

          do k=1,nsig_read
             kt=kt+1
             kq=kq+1
             ku=ku+1
             kv=kv+1
             if (n_actual_clouds>0) then
                kcwm=kcwm+1
                kf_ice=kf_ice+1
                kf_rain=kf_rain+1
                kf_rimef=kf_rimef+1
             end if
  
             do i=1,lon2
                do j=1,lat2
                   ges_u(j,i,k) = real(all_loc(j,i,ku),r_kind)
                   ges_v(j,i,k) = real(all_loc(j,i,kv),r_kind)
                   ges_q(j,i,k)   = real(all_loc(j,i,kq),r_kind)
                   ges_tsen(j,i,k,it)  = real(all_loc(j,i,kt) ,r_kind)! actually holds sensible temperature
                   ges_oz(j,i,k) = zero                  ! set to zero for now 
  
                   if (n_actual_clouds>0) then
                      clwmr(j,i,k) = real(all_loc(j,i,kcwm),r_kind)
                      fice(j,i,k) = real(all_loc(j,i,kf_ice),r_kind)
                      frain(j,i,k) = real(all_loc(j,i,kf_rain),r_kind)
                      frimef(j,i,k) = real(all_loc(j,i,kf_rimef),r_kind)
                   end if
                end do
             end do
             if (n_actual_clouds>0 .and. (icw4crtm>0 .or. iqtotal>0) .and. ier==0) then 
                do i=1,lon2
                   do j=1,lat2
                      ges_prsl(j,i,k,it)=one_tenth* &
                                  (aeta1_ll(k)*pdtop_ll + &
                                   aeta2_ll(k)*(ten*ges_ps(j,i)-pdtop_ll-pt_ll) + &
                                   pt_ll)
                   end do
                end do
                call cloud_calc(ges_prsl(:,:,k,it),ges_q(:,:,k),ges_tsen(:,:,k,it),clwmr(:,:,k), &
                     fice(:,:,k),frain(:,:,k),frimef(:,:,k), &
                     ges_ql(:,:,k),ges_qi(:,:,k),ges_qr(:,:,k),ges_qs(:,:,k),ges_qg(:,:,k),ges_qh(:,:,k), &
                     efr_ql(:,:,k,it),efr_qi(:,:,k,it),efr_qr(:,:,k,it),efr_qs(:,:,k,it),efr_qg(:,:,k,it),efr_qh(:,:,k,it))
             end if
          end do
          do k=nsig_read+1,nsig
             do i=1,lon2
                do j=1,lat2
                   ges_u(j,i,k)    = zero 
                   ges_v(j,i,k)    = zero 
                   ges_q(j,i,k)    = zero 
                   ges_tsen(j,i,k,it) = zero
                   ges_oz(j,i,k)   = zero 
  
                   if (n_actual_clouds>0) then
                      clwmr(j,i,k)  = zero 
                      fice(j,i,k)   = zero 
                      frain(j,i,k)  = zero
                      frimef(j,i,k) = zero 
                   end if
                enddo
             enddo
          enddo
          if (n_actual_clouds>0) then
             call gsi_bundlegetpointer (gsi_metguess_bundle(it),'cw',ges_cwmr,iret)
             if (iret==0) ges_cwmr=clwmr
             call gsi_bundlegetpointer (gsi_metguess_bundle(it),'fice',ges_fice,iret)
             if (iret==0) ges_fice=fice
             call gsi_bundlegetpointer (gsi_metguess_bundle(it),'frain',ges_frain,iret)
             if (iret==0) ges_frain=frain
             call gsi_bundlegetpointer (gsi_metguess_bundle(it),'frimef',ges_frimef,iret)
             if (iret==0) ges_frimef=frimef
          end if
  
  
!         if(mype == 10) write(6,*)' in read_wrf_nmm_netcdf_guess, min,max(ges_ps)=', &          
!              minval(ges_ps),maxval(ges_ps)                                                        
!         if(mype == 10) write(6,*)' in read_wrf_nmm_netcdf_guess, min,max(soil_moi)=', &
!              minval(soil_moi),maxval(soil_moi)
!         if(mype == 10) write(6,*)' in read_wrf_nmm_netcdf_guess, min,max(soil_temp)=', &
!              minval(soil_temp),maxval(soil_temp)
          if(update_pint) then
             ier=0
             call gsi_bundlegetpointer(gsi_metguess_bundle(it),'pint', ges_pint, istatus)
             ier=ier+istatus
             call gsi_bundlegetpointer(gsi_metguess_bundle(it),'pd'  , ges_pd  , istatus)
             ier=ier+istatus
             if (ier/=0) then ! doesn't need to die (but needs careful revision)
                call die(myname,': missing pint/pd fields',ier)
             endif
             kpint=i_0+i_pint-1
             do k=1,nsig_read+1
                kpint=kpint+1
                do i=1,lon2
                   do j=1,lat2
                      ges_pint(j,i,k)  = real(all_loc(j,i,kpint),r_kind) ! actually holds sensible temperature
                   end do
                end do
             end do
             do k=nsig_read+2,nsig+1
                do i=1,lon2
                   do j=1,lat2
                      ges_pint(j,i,k) = zero
                   end do
                end do
             end do
  
             do i=1,lon2
                do j=1,lat2
                   ges_pd(j,i)  = real(all_loc(j,i,i_0+i_pd),r_kind)
                end do
             end do
          end if
  
  !       Convert sensible temp to virtual temp
          do k=1,nsig_read
             do i=1,lon2
                do j=1,lat2
                   ges_tv(j,i,k) = ges_tsen(j,i,k,it) * (one+fv*ges_q(j,i,k))
                end do
             end do
          end do
          do k=nsig_read+1,nsig
             do i=1,lon2
                do j=1,lat2
                   ges_tv(j,i,k) = zero 
                end do
             end do
          end do
  
  !    Transfer rest of surface fields
          do i=1,lon2
             do j=1,lat2
                fact10(j,i,it)=one    !  later fix this by using correct w10/w(1)
                wmag=sqrt(ges_u(j,i,1)**2+ges_v(j,i,1)**2)
                if(wmag > zero)fact10(j,i,it)=sqrt(real(all_loc(j,i,i_0+i_u10),r_kind)**2 + &
                        real(all_loc(j,i,i_0+i_v10),r_kind)**2)/wmag
                fact10(j,i,it)=min(max(fact10(j,i,it),half),0.95_r_kind)
                veg_type(j,i,it)=real(all_loc(j,i,i_0+i_ivgtyp),r_kind)
                veg_frac(j,i,it)=real(all_loc(j,i,i_0+i_vegfrac),r_kind)
                soil_type(j,i,it)=real(all_loc(j,i,i_0+i_isltyp),r_kind)
  !             soil_temp(j,i,it)=real(all_loc(j,i,i_0+i_stc),r_kind)
  !             soil_moi(j,i,it)=real(all_loc(j,i,i_0+i_smc),r_kind)
                sm_this=zero
                if(all_loc(j,i,i_0+i_sm) /= zero_single) sm_this=one
                sice_this=zero
                if(all_loc(j,i,i_0+i_sice) /= zero_single) sice_this=one
                
                isli_this=0
                if(sice_this == one) isli_this=2
                if(sice_this == zero.and.sm_this == zero) isli_this=1
                isli(j,i,it)=isli_this
                
                sfct(j,i,it)=real(all_loc(j,i,i_0+i_sst),r_kind)
                if(isli(j,i,it) /= 0) sfct(j,i,it)=real(all_loc(j,i,i_0+i_tsk),r_kind)
                if(sfct(j,i,it) < one) then
  
  !             For now, replace missing skin temps with 1st sigma level temp
                   sfct(j,i,it)=real(all_loc(j,i,i_0+i_t),r_kind)
                   write(6,*)' doubtful skint replaced with 1st sigma level t, j,i,mype,sfct=',&
                        j,i,mype,sfct(j,i,it)
                   num_doubtful_sfct=num_doubtful_sfct+1
                end if
             end do
          end do
       end do ! it loop 
       
       call mpi_reduce(num_doubtful_sfct,num_doubtful_sfct_all,1,mpi_integer,mpi_sum,&
            0,mpi_comm_world,ierror)
       if(mype == 0 .and. print_verbose) then
          write(6,*)' in read_wrf_nmm_netcdf_guess, num_doubtful_sfct_all = ',num_doubtful_sfct_all
          write(6,*)' in read_wrf_nmm_netcdf_guess, num_doubtful_sfct_all = ',num_doubtful_sfct_all
       end if
!      if(mype == 10) write(6,*)' in read_wrf_nmm_netcdf_guess, min,max(sfct)=', &
!           minval(sfct),maxval(sfct)
!      if(mype == 10) write(6,*)' in read_wrf_nmm_netcdf_guess, min,max(veg_type)=', &
!           minval(veg_type),maxval(veg_type)
!      if(mype == 10) write(6,*)' in read_wrf_nmm_netcdf_guess, min,max(veg_frac)=', &
!           minval(veg_frac),maxval(veg_frac)
!      if(mype == 10) write(6,*)' in read_wrf_nmm_netcdf_guess, min,max(soil_type)=', &
!           minval(soil_type),maxval(soil_type)
!      if(mype == 10) write(6,*)' in read_wrf_nmm_netcdf_guess, min,max(isli)=', &
!           minval(isli),maxval(isli)
       
       deallocate(all_loc,jsig_skip,igtype,identity)
       deallocate(temp1,itemp1)
  
       if (use_gfs_stratosphere) then
          if (mype==0 .and. print_verbose) write(6,*)'in read_wrf_nmm_netcdf: use_gfs_stratosphere ...beg'     
          call add_gfs_stratosphere
          if (mype==0 .and. print_verbose) write(6,*)'in read_wrf_nmm_netcdf: use_gfs_stratosphere ...end'         
       endif
  
       if (mype==0 .and. print_verbose) then
          do k=1,nsig
             write(6,*)' in read_wrf_nmm_netcdf_k,ges_tv  =',k,ges_tv(10,10,k)   !debug            
             write(6,*)' in read_wrf_nmm_netcdf_k,ges_tsen=',k,ges_tsen(10,10,k,1) !debug        
          enddo
       endif
  
  end subroutine read_wrf_nmm_netcdf_guess_wrf

  subroutine read_nems_nmmb_guess_wrf(this,mype)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    read_nems_nmmb_guess             read nems_nmmb guess file
  !   prgmmr: parrish          org: np22                date: 2003-09-05
  !
  ! abstract: in place of read_guess for global application, read guess
  !             from regional model, in this case the nems nmmb (non-hydrostatic
  !             mesoscale model).  This version reads directly from the nems input file
  !             using nemsio routines.  Each horizontal input field is interpolated
  !             from the b-grid to the a-grid with different resolution, determined
  !             by parameter grid_ratio_nmmb.
  !
  ! program history log:
  !   2009-03-18  parrish
  !   2010-03-12  parrish - add option to read ozone from location "o3mr".  If use_gfs_ozone =.true.
  !                           then skip reading of ozone, since it will be brought in directly
  !                           from gfs sigma file to analysis grid with later call to
  !                           read_gfs_ozone_for_regional.
  !   2010-03-15  parrish - add option regional_ozone to turn on ozone in regional analysis
  !   2011-06-16  zhu     - add option to read cloud info for cloudy radiance
  !   2012-02-16  parrish - include option to replace nmmb stratosphere with gfs stratosphere.
  !   2012-10-18  s.liu   - add use_reflectivity option for cloud analysis variables.
  !   2012-12-16  s.liu   - add gsd cloud analysis variables.
  !   2013-10-19  todling - efr fields now live in cloud_efr_mod
  !   2013-02-26  zhu - add cold_start option when the restart file is from the GFS
  !   2016_03_02  s.liu/carley   - remove use_reflectivity and use i_gsdcldanal_type
  !   2016_06_21  s.liu   - delete unused variable qhtmp
  !   2016_06_30  s.liu   - delete unused variable gridtype in read fraction
  !   2017-05-12  Y. Wang, X. Wang  - add code to read vertical velocity (W), hydrometeors and
  !                                   Reflectivity (REFL_10CM) for direct radar DA, POC: xuguang.wang@ou.edu
  !   2016-08-12  lippi   - add include_w. If true, reads in guess vertical velocity (w) profile.
  !
  !   input argument list:
  !     mype     - pe number
  !
  !     NOTES:  need to pay special attention to various surface fields to make sure
  !             they are correct (check units).  fact10 needs to be computed from 
  !             10m wind, which is included in wrf nmm interface file.
  !
  !             Cloud water currently set to zero.  Need to fix before doing precip
  !             assimilation--wait until global adopts Brad Ferrier's multi-species 
  !             scheme??
  !
  !             Ozone currently set to zero.  No ozone variable in wrf nmm--climatology
  !             instead.  Do we use climatology?
  !
  !             No background bias yet. (biascor ignored)
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$
    use kinds, only: r_kind,i_kind
    use mpimod, only: ierror,mpi_comm_world,mpi_integer,mpi_sum
    use guess_grids, only: &
         fact10,soil_type,veg_frac,veg_type,sfc_rough,sfct,sno,soil_temp,soil_moi,&
         isli,nfldsig,ges_tsen,ges_prsl,ifilesig
    use cloud_efr_mod, only: efr_ql,efr_qi,efr_qr,efr_qs,efr_qg,efr_qh
    use guess_grids, only: ges_prsl
    use gridmod, only: lat2,lon2,pdtop_ll,pt_ll,nsig,nmmb_verttype,use_gfs_ozone,regional_ozone,& 
         aeta1_ll,aeta2_ll
    use rapidrefresh_cldsurf_mod, only: i_gsdcldanal_type  
    use constants, only: zero,one_tenth,half,one,fv,rd_over_cp,r100,r0_01,ten,rd,r1000
    use wrf_params_mod, only: update_pint, cold_start

    use gsi_nemsio_mod, only: gsi_nemsio_open,gsi_nemsio_close,gsi_nemsio_read,gsi_nemsio_read_fraction, gsi_nemsio_read_fractionnew
    use gfs_stratosphere, only: use_gfs_stratosphere,nsig_save,good_o3mr,add_gfs_stratosphere  
    use gsi_metguess_mod, only: gsi_metguess_get,gsi_metguess_bundle
    use gsi_bundlemod, only: gsi_bundlegetpointer
    use mpeu_util, only: die,getindex
    use control_vectors, only: cvars3d
    use cloud_efr_mod, only: cloud_calc,cloud_calc_gfs
    use obsmod,only: if_model_dbz
    use wrf_vars_mod, only : dbz_exist
    implicit none
  
  ! Declare passed variables here
    class(read_wrf_nmm_guess_class),intent(inout) :: this
    integer(i_kind),intent(in):: mype
  
  ! Declare local parameters
  
  ! Declare local variables
    logical include_w  

  ! other internal variables
    character(255) wrfges
    character(len=*),parameter :: myname='read_nems_nmmb_guess:: '
    integer(i_kind) i,it,j,k,kr,mype_input
    integer(i_kind) isli_this,nsig_read
    real(r_kind) pd,psfc_this,wmag,pd_to_ps
    integer(i_kind) num_doubtful_sfct,num_doubtful_sfct_all
    real(r_kind),dimension(lat2,lon2):: smthis,sicethis,u10this,v10this,sstthis,tskthis

    real(r_kind)    :: Cr=3.6308e9_r_kind          ! Rain constant coef.
    real(r_kind)    :: Cli=3.268e9_r_kind          ! Precip. ice constant coef.
  
  ! variables for cloud info
    logical good_fice, good_frain, good_frimef
    integer(i_kind) iqtotal,icw4crtm,ier,iret,n_actual_clouds,istatus,ierr, &
                    i_radar_qr,i_radar_qli,i_radar_qh
    real(r_kind),dimension(lat2,lon2,nsig):: clwmr,fice,frain,frimef,ges_rho,Ze,Zer, Zeli
    real(r_kind),pointer,dimension(:,:  ):: ges_pd  =>NULL()
    real(r_kind),pointer,dimension(:,:  ):: ges_ps  =>NULL()
    real(r_kind),pointer,dimension(:,:  ):: ges_z   =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_u   =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_v   =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_w   =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_tv  =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_pint=>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_q   =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_oz  =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_cwmr=>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_ref =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: dfi_tten=>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_dw   =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_dbz =>NULL()
  
    real(r_kind),pointer,dimension(:,:,:):: ges_ql=>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_qi=>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_qr=>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_qs=>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_qg=>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_qh=>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_qli=>NULL()
  
    associate( this => this ) ! eliminates warning for unused dummy argument needed for binding
    end associate
  !  check to see if using GFS stratosphere:
    if(use_gfs_stratosphere) then
       nsig_read=nsig_save
    else
       nsig_read=nsig
    end if
  
  ! get conversion factor for pd to psfc
  
    if(nmmb_verttype=='OLD') then
       pd_to_ps=pdtop_ll+pt_ll
    else
       pd_to_ps=pt_ll
    end if
        write(6,*) ' in read_nems_nmmb_guess, nmmb_verttype,pdtop_ll,pt_ll,pd_to_ps=', &
                                              nmmb_verttype,pdtop_ll,pt_ll,pd_to_ps
  
  ! Determine whether or not cloud-condensate is the control variable (ges_cw=ges_ql+ges_qi)
    icw4crtm=getindex(cvars3d,'cw')
  
  ! Determine whether or not total moisture (water vapor+total cloud condensate) is the control variable
    iqtotal=getindex(cvars3d,'qt')

  ! Determine if qr and qli are control variables for radar data assimilation,
    i_radar_qr=0
    i_radar_qli=0
    i_radar_qh=0
    i_radar_qr=getindex(cvars3d,'qr')
    i_radar_qli=getindex(cvars3d,'qli')
    i_radar_qh=getindex(cvars3d,'qh')
  
  ! Inquire about cloud guess fields
    call gsi_metguess_get('clouds::3d',n_actual_clouds,istatus)
  
  ! do serial input for now, with mpi_send to put on appropriate processor.
  
    mype_input=0
    do it=1,nfldsig
       num_doubtful_sfct=0
         
       ier=0
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'ps' ,ges_ps ,istatus );ier=ier+istatus
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'z' , ges_z  ,istatus );ier=ier+istatus
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'u' , ges_u  ,istatus );ier=ier+istatus
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'v' , ges_v  ,istatus );ier=ier+istatus
       if(dbz_exist) call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'dbz' , ges_dbz  ,istatus );ier=ier+istatus
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'tv' ,ges_tv ,istatus );ier=ier+istatus
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'q'  ,ges_q  ,istatus );ier=ier+istatus
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'oz' ,ges_oz ,istatus );ier=ier+istatus
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'pd' ,ges_pd,istatus );ier=ier+istatus
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'w'  ,ges_w  ,istatus)
       if (istatus==0) then
          include_w=.true.
          if(mype==0) write(6,*)'READ_WRF_NMM_GUESS: Using vertical velocity.'
          call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'dw' , ges_dw  ,istatus );ier=ier+istatus
       else
          include_w=.false.
          if(mype==0) write(6,*)'READ_WRF_NMM_GUESS: NOT using vertical velocity.'
       end if

       if (ier/=0) call die(trim(myname),'cannot get pointers for met-fields, ier =',ier)
  
       if(mype==mype_input) then
             write(wrfges,'("wrf_inout",i2.2)')ifilesig(it)
       end if
       call gsi_nemsio_open(wrfges,'READ', &
                            'READ_NEMS_NMMB_GUESS:  problem with wrfges',mype,mype_input,ierr)
       if(ierr==1)cycle
  
  !                            ! pd
  
       call gsi_nemsio_read('dpres','hybrid sig lev','H',1,ges_pd,mype,mype_input)
       do i=1,lon2
          do j=1,lat2
  !               convert wrf nmm pd variable to psfc in mb, and then to log(psfc) in cb
             pd=r0_01*ges_pd(j,i)
             psfc_this=pd+pd_to_ps
             ges_ps(j,i)=one_tenth*psfc_this
          end do
       end do
  
  !                          !   fis
  
       call gsi_nemsio_read('hgt','sfc','H',1,ges_z(:,:),mype,mype_input)
  
  !                          !   u,v,q,tsen,tv
       do k=nsig_read+1,nsig
          ges_u(:,:,k)=zero
          ges_v(:,:,k)=zero
          ges_q(:,:,k)=zero
          ges_tsen(:,:,k,it)=zero
          ges_oz(:,:,k)=zero
          if(include_w) then
            ges_w(:,:,k)=zero
            ges_dw(:,:,k)=zero
          end if
          if(dbz_exist) ges_dbz(:,:,k)=zero
       end do
       do kr=1,nsig_read
          k=nsig_read+1-kr
          call gsi_nemsio_read('ugrd','mid layer','V',kr,ges_u(:,:,k),   mype,mype_input)
          call gsi_nemsio_read('vgrd','mid layer','V',kr,ges_v(:,:,k),   mype,mype_input)
          call gsi_nemsio_read('spfh','mid layer','H',kr,ges_q(:,:,k),   mype,mype_input)
          call gsi_nemsio_read('tmp' ,'mid layer','H',kr,ges_tsen(:,:,k,it),mype,mype_input)
          if(include_w) then
             call gsi_nemsio_read('w_tot','mid layer','H',kr,ges_w(:,:,k),mype,mype_input)
             call gsi_nemsio_read('dwdt','mid layer','H',kr,ges_dw(:,:,k),   mype,mype_input)
          end if
          do i=1,lon2
             do j=1,lat2
                ges_tv(j,i,k) = ges_tsen(j,i,k,it) * (one+fv*ges_q(j,i,k))
             end do
          end do
          if(regional_ozone) then
             if(use_gfs_ozone) then
                ges_oz(:,:,k)=zero
             else
                good_o3mr=.false.
                call gsi_nemsio_read('o3mr' ,'mid layer','H',kr,ges_oz(:,:,k),mype,mype_input,good_o3mr)
                if(.not.good_o3mr) then
                   write(6,*)' IN READ_NEMS_NMMB_GUESS, O3MR FIELD NOT YET AVAILABLE'
                   ges_oz(:,:,k)=zero
                end if
             end if
          end if
       end do
  
       if( i_gsdcldanal_type == 0 .and. i_radar_qr>0 .and. i_radar_qli>0 )then 
        ! For directly assimilating radar reflectivity rather than cloud analysis

          ier = 0
          call gsi_bundlegetpointer (gsi_metguess_bundle(it),'ql',ges_ql,iret); ier=iret
          call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qr',ges_qr,iret); ier=ier+iret
          call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qi',ges_qi,iret); ier=ier+iret
          call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qli',ges_qli,iret); ier=ier+iret
          if(dbz_exist) call gsi_bundlegetpointer (gsi_metguess_bundle(it),'dbz',ges_dbz,iret); ier=ier+iret
          if (ier/=0) call die(trim(myname),'cannot get pointers for met-fields related to hydrometeor, ier =',ier)
          
          if (ier==0) then
              frain=zero
              fice=zero
              clwmr=zero
              do kr=1,nsig
                 k=nsig+1-kr
                 if( dbz_exist .and. if_model_dbz ) then
                     call gsi_nemsio_read('refl_10cm' ,'mid layer','H',kr,ges_dbz(:,:,k),mype,mype_input)
                     where( ges_dbz(:,:,k) < 0.0_r_kind )
                          ges_dbz(:,:,k) = 0.0_r_kind
                     end where
                 end if
                 call gsi_nemsio_read_fractionnew('f_rain','f_ice','clwmr','f_rimef','mid layer',kr, &
                      ges_qi(:,:,k),ges_qli(:,:,k),ges_qr(:,:,k),ges_ql(:,:,k), mype,mype_input)
                 if( dbz_exist .and. (.not. if_model_dbz) )then
                   do i=1,lon2
                      do j=1,lat2
                         ges_prsl(j,i,k,it)=one_tenth* &
                                     (aeta1_ll(k)*pdtop_ll + &
                                      aeta2_ll(k)*(ten*ges_ps(j,i)-pdtop_ll-pt_ll) + &
                                      pt_ll)
                         ges_rho(j,i,k)=(ges_prsl(j,i,k,it)/(ges_tv(j,i,k)*rd))*r1000
                      end do
                   end do
                
                    Zer(:,:,k)  = Cr * (ges_rho(:,:,k) * ges_qr(:,:,k))**(1.75_r_kind)
                    Zeli(:,:,k) = Cli * (ges_rho(:,:,k) * ges_qli(:,:,k))**(2.0_r_kind)
                    Ze(:,:,k)=Zer(:,:,k)+Zeli(:,:,k)

                    ges_dbz(:,:,k) = 0.0_r_kind
                    
                    where ( Ze(:,:,k) > 0.0_r_kind )
                      ges_dbz(:,:,k) = ten * log10(Ze(:,:,k))
                    end where
                    where( ges_dbz(:,:,k) < 0.0_r_kind )
                          ges_dbz(:,:,k) = 0.0_r_kind
                    end where
                 end if
              end do
              if (mype==0) then
                write(6,*)'QLI,max, min,',maxval(ges_qli),minval(ges_qli)
                write(6,*)'QR,max, min,',maxval(ges_qr),minval(ges_qr)
                write(6,*)'QL,max, min,',maxval(ges_ql),minval(ges_ql)
                if( dbz_exist ) write(6,*)'DBZ,max, min,',maxval(ges_dbz),minval(ges_dbz)
              end if
          else
              if (mype==0) write(6,*) 'ERROR GETTING POINTERS FOR HYDROMETEORS WITH RADAR DATA ASSIMILATION IN READ NMMB GUESS'
          end if  ! ier==0

       else ! i_radar_qli > 0

  !                          !  cloud liquid water,ice,snow,graupel,hail,rain for cloudy radiance
          if (n_actual_clouds>0 .and. (i_gsdcldanal_type/=2) .and. i_radar_qh > 0 ) then
   
  !          Get pointer to cloud water mixing ratio
             call gsi_bundlegetpointer (gsi_metguess_bundle(it),'ql',ges_ql,iret); ier=iret
             call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qi',ges_qi,iret); ier=ier+iret
             call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qr',ges_qr,iret); ier=ier+iret
             call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qs',ges_qs,iret); ier=ier+iret
             call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qg',ges_qg,iret); ier=ier+iret
             call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qh',ges_qh,iret); ier=ier+iret
             if ((icw4crtm>0 .or. iqtotal>0) .and. ier==0) then
                ges_ql=zero; ges_qi=zero; ges_qr=zero; ges_qs=zero; ges_qg=zero; ges_qh=zero
                efr_ql=zero; efr_qi=zero; efr_qr=zero; efr_qs=zero; efr_qg=zero; efr_qh=zero
                do kr=1,nsig_read
                   k=nsig_read+1-kr
                   call gsi_nemsio_read('clwmr', 'mid layer','H',kr,clwmr(:,:,k), mype,mype_input) !read total condensate
                   call gsi_nemsio_read('f_ice', 'mid layer','H',kr,fice(:,:,k),  mype,mype_input,good_fice) !read ice fraction
                   call gsi_nemsio_read('f_rain','mid layer','H',kr,frain(:,:,k), mype,mype_input,good_frain) !read rain fraction
                   call gsi_nemsio_read('f_rimef','mid layer','H',kr,frimef(:,:,k), mype,mype_input,good_frimef) !read rime factor
                   if (good_fice .and. good_frain .and. good_frimef) cold_start=.false.
                   if (.not. cold_start) then
                      do i=1,lon2
                        do j=1,lat2
                           ges_prsl(j,i,k,it)=one_tenth* &
                                       (aeta1_ll(k)*pdtop_ll + &
                                       aeta2_ll(k)*(ten*ges_ps(j,i)-pdtop_ll-pt_ll) + &
                                       pt_ll)
                        end do
                      end do
                      call cloud_calc(ges_prsl(:,:,k,it),ges_q(:,:,k),ges_tsen(:,:,k,it),clwmr(:,:,k), &
                           fice(:,:,k),frain(:,:,k),frimef(:,:,k), &
                           ges_ql(:,:,k),ges_qi(:,:,k),ges_qr(:,:,k),ges_qs(:,:,k),ges_qg(:,:,k),ges_qh(:,:,k), &
                           efr_ql(:,:,k,it),efr_qi(:,:,k,it),efr_qr(:,:,k,it),efr_qs(:,:,k,it),efr_qg(:,:,k,it),efr_qh(:,:,k,it))
                   end if
                end do
                if (cold_start) call cloud_calc_gfs(ges_ql,ges_qi,clwmr,ges_q,ges_tv,.true.)
  
                call gsi_bundlegetpointer (gsi_metguess_bundle(it),'cw',ges_cwmr,iret)
                if (iret==0) ges_cwmr=clwmr 
             end if  ! icw4crtm>10 .or. iqtotal>0
          end if    ! end of (n_actual_clouds>0)

       end if ! i_radar_qli > 0
  
  
  !    if (n_actual_clouds>0 .and. use_reflectivity) then
       if (i_gsdcldanal_type==2) then
  
  !       Get pointer to cloud water mixing ratio
          call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qi',ges_qi,iret); ier=iret
          call gsi_bundlegetpointer (gsi_metguess_bundle(it),'ql',ges_ql,iret); ier=ier+iret
          call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qr',ges_qr,iret); ier=ier+iret
          call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qs',ges_qs,iret); ier=ier+iret
          call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qg',ges_qg,iret); ier=ier+iret
          call gsi_bundlegetpointer (GSI_MetGuess_Bundle(it),'ref',ges_ref,istatus);ier=ier+istatus
          call gsi_bundlegetpointer (GSI_MetGuess_Bundle(it),'tten',dfi_tten,istatus);ier=ier+istatus
             do kr=1,nsig
                k=nsig+1-kr
                call gsi_nemsio_read_fraction('f_rain','f_ice','clwmr','tmp','mid layer',kr, &
                         ges_qi(:,:,k),ges_qs(:,:,k),ges_qr(:,:,k),ges_ql(:,:,k), mype,mype_input) !read total condensate
             end do
  
       end if 
  
                                     !   pint
       if(update_pint) then
  
          ier=0
          call gsi_bundlegetpointer(gsi_metguess_bundle(it),'pint', ges_pint, istatus)
          ier=ier+istatus
          call gsi_bundlegetpointer(gsi_metguess_bundle(it),'pd'  , ges_pd  , istatus)
          ier=ier+istatus
          if (ier/=0) then ! doesn't need to die (but needs careful revision)
             call die(myname,': missing pint/pd fields',ier)
          endif
  
          do k=nsig_read+2,nsig+1
             ges_pint(:,:,k)=zero
          end do
          do kr=1,nsig_read+1
             k=nsig_read+2-kr
             call gsi_nemsio_read('pres' ,'layer','H',kr,ges_pint(:,:,k),mype,mype_input)
          end do
  
       end if
  
  !                            ! sno
       call gsi_nemsio_read('sno' ,'sfc','H',1,sno(:,:,it),mype,mype_input)
  
  !                            ! surface roughness
       call gsi_nemsio_read('zorl' ,'sfc','H',1,sfc_rough(:,:,it),mype,mype_input)
  
  !                            ! soil_moisture
       call gsi_nemsio_read('smc' ,'soil layer','H',1,soil_moi(:,:,it),mype,mype_input)
  
  !                            ! soil_temp
       call gsi_nemsio_read('stc' ,'soil layer','H',1,soil_temp(:,:,it),mype,mype_input)
  
  !                            ! veg type
       call gsi_nemsio_read('vgtyp' ,'sfc','H',1,veg_type(:,:,it),mype,mype_input)
  
      !           because veg_type is integer quantity, do an nint operation to remove fractional values
      !           due to interpolation
       do i=1,lon2
          do j=1,lat2
             veg_type(j,i,it)=real(nint(veg_type(j,i,it)),r_kind)
          end do
       end do
  !                            ! veg frac
       call gsi_nemsio_read('vegfrc' ,'sfc','H',1,veg_frac(:,:,it),mype,mype_input)
  
  
  !                            ! soil type
       call gsi_nemsio_read('sltyp' ,'sfc','H',1,soil_type(:,:,it),mype,mype_input)
  
      !           because soil_type is integer quantity, do an nint operation to remove fractional values
      !           due to interpolation
       do i=1,lon2
          do j=1,lat2
             soil_type(j,i,it)=real(nint(soil_type(j,i,it)),r_kind)
          end do
       end do
  
  !                            ! sm
       call gsi_nemsio_read('sm' ,'sfc','H',1,smthis(:,:),mype,mype_input)
  
      !           because sm is integer quantity, do an nint operation to remove fractional values
      !           due to interpolation
       do i=1,lon2
          do j=1,lat2
             smthis(j,i)=real(nint(smthis(j,i)),r_kind)
          end do
       end do
  
  !                            ! sice
       call gsi_nemsio_read('sice' ,'sfc','H',1,sicethis(:,:),mype,mype_input)
  
      !           because sice is integer quantity, do an nint operation to remove fractional values
      !           due to interpolation
       do i=1,lon2
          do j=1,lat2
             sicethis(j,i)=real(nint(sicethis(j,i)),r_kind)
          end do
       end do
  
  !                            ! sst
       call gsi_nemsio_read('tsea' ,'sfc','H',1,sstthis(:,:),mype,mype_input)
  
  !                            ! tsk
       call gsi_nemsio_read('ths' ,'sfc','H',1,tskthis(:,:),mype,mype_input)
  !                 convert tsk from potential to virtual temperature
       if(mype==0) write(6,*)' in read_nems_nmmb_guess, rd_over_cp=',rd_over_cp
       do i=1,lon2
          do j=1,lat2
             tskthis(j,i)=tskthis(j,i)*(ges_ps(j,i)/r100)**rd_over_cp
          end do
       end do
  
  !                            ! u10,v10
       call gsi_nemsio_read('u10' ,'10 m above gnd','H',1,u10this(:,:),mype,mype_input)
       call gsi_nemsio_read('v10' ,'10 m above gnd','H',1,v10this(:,:),mype,mype_input)
  
       do i=1,lon2
          do j=1,lat2
             fact10(j,i,it)=one    !  later fix this by using correct w10/w(1)
             wmag=sqrt(ges_u(j,i,1)**2+ges_v(j,i,1)**2)
             if(wmag > zero)fact10(j,i,it)=sqrt(u10this(j,i)**2+v10this(j,i)**2)/wmag
             fact10(j,i,it)=min(max(fact10(j,i,it),half),0.95_r_kind)
  
             if(smthis(j,i)/=zero) smthis(j,i)=one
             if(sicethis(j,i)/=zero) sicethis(j,i)=one
             isli_this=0
             if(sicethis(j,i)==one) isli_this=2
             if(sicethis(j,i)==zero.and.smthis(j,i)==zero) isli_this=1
             isli(j,i,it)=isli_this
  
             sfct(j,i,it)=sstthis(j,i)
             if(isli(j,i,it)/=0) sfct(j,i,it)=tskthis(j,i)
             if(sfct(j,i,it)<one) then
  
  !             For now, replace missing skin temps with 1st sigma level temp
                sfct(j,i,it)=ges_tsen(j,i,1,it)
                num_doubtful_sfct=num_doubtful_sfct+1
                if(num_doubtful_sfct <= 100) &
                     write(6,*)' doubtful skint replaced with 1st sigma level t, j,i,mype,sfct=',&
                     j,i,mype,sfct(j,i,it)
             end if
          end do
       end do
  
       call gsi_nemsio_close(wrfges,'READ_NEMS_NMMB_GUESS',mype,mype_input)
  
  !    read in radar reflectivity
  !    mype_input=0
       if(i_gsdcldanal_type==2) then
       if(mype==mype_input) then
          wrfges = 'obsref.nemsio'
  !       write(6,*)'start to read obsref.nemsio'
       end if
       call gsi_nemsio_open(wrfges,'READ', &
                      'READ_radar_reflecitivity_mosaic:  problem with obsref.nemsio',mype,mype_input,ierr)
       do kr=1,nsig
          k=nsig+1-kr
          call gsi_nemsio_read('obs_ref' ,'mid layer','H',kr,ges_ref(:,:,k),mype,mype_input)
  !       write(6,*)'reading obsref.nemsio'
       end do
  
       call gsi_nemsio_close(wrfges,'READ_radar_reflectivity_mosaic',mype,mype_input)
       end if
  !    end read in radar reflectivity
  
  
       call mpi_reduce(num_doubtful_sfct,num_doubtful_sfct_all,1,mpi_integer,mpi_sum,&
                       0,mpi_comm_world,ierror)
       if(mype == 0) write(6,*)' in read_nems_nmmb_binary_guess, num_doubtful_sfct_all = ', &
                                                             num_doubtful_sfct_all
    end do ! enddo it
  
    if(use_gfs_stratosphere) call add_gfs_stratosphere
  
       do k=1,nsig
                       if(mype==0) &
           write(6,*)' k,ges_tv=',k,ges_tv(10,10,k)  ! debug
       end do
  
    return 
  end subroutine read_nems_nmmb_guess_wrf
end module read_wrf_nmm_guess_mod

module wrwrfnmma_mod
use abstract_wrwrfnmma_mod
  type, extends(abstract_wrwrfnmma_class) :: wrwrfnmma_class
  contains
    procedure, pass(this) :: wrwrfnmma_binary => wrwrfnmma_binary_wrf  
    procedure, pass(this) :: wrwrfnmma_netcdf => wrwrfnmma_netcdf_wrf  
    procedure, nopass :: get_bndy_file
    procedure, pass(this) :: wrnemsnmma_binary
  end type wrwrfnmma_class
contains
  subroutine wrwrfnmma_binary_wrf(this,mype)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    wrwrfnmma              write out wrf NMM restart file
  !   prgmmr: parrish          org: np22                date: 2004-06-23
  !
  ! abstract:  read wrf NMM guess restart interface file, add analysis
  !            increment, and write out wrf NMM analysis restart 
  !            interface file.
  !
  ! program history log:
  !   2004-06-23  parrish, document
  !   2004-08-03  treadon - add only to module use, add intent in/out
  !   2004-11-22  parrish - rewrite for mpi-io
  !   2004-12-15  treadon - write analysis to file "wrf_inout"
  !   2005-07-06  parrish - update and write out pint if update_pint=.true.
  !   2006-04-06  middlecoff - changed nfcst from 11 to lendian_in
  !   2006-07-28  derber - include sensible temperature
  !   2006-07-31  kleist - change to use ges_ps instead of lnps
  !   2007-04-12  parrish - add modifications to allow any combination of ikj or ijk
  !                          grid ordering for input 3D fields
  !   2007-05-02  parrish - fix bug to prevent out of memory reference when pint missing
  !   2008-04-01  safford - rm unused uses
  !   2008-12-05  todling - adjustment for dsfct time dimension addition
  !   2012-01-15  zhu     - add cloud hydrometeors
  !   2012-03-09  parrish - added the output of boundary variables
  !   2012-10-11  parrish - add option to swap bytes immediately after every call to mpi_file_read_at and
  !                           before every call to mpi_file_write_at (to handle cases of big-endian
  !                           file/little-endian machine and vice-versa)
  !   2013-10-19  todling - metguess now holds background
  !
  !   input argument list:
  !     mype     - pe number
  !
  !   output argument list:
  !     no output arguments
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$
    use kinds, only: r_kind,r_single,i_long,i_llong,i_kind
    use wrf_params_mod, only: update_pint
    use guess_grids, only: &
         ntguessfc,ntguessig,ifilesig,dsfct,ges_tsen
    use mpimod, only: mpi_comm_world,ierror,mpi_byte,mpi_integer4,mpi_real4,mpi_sum,npe, &
         mpi_offset_kind,mpi_info_null,mpi_mode_rdwr,mpi_status_size
    use gridmod, only: iglobal,itotsub,pt_ll,update_regsfc,&
         half_grid,filled_grid,pdtop_ll,nlat_regional,nlon_regional,&
         nsig,lat1,lon1,eta2_ll,lat2,lon2
    use constants, only: zero_single,r10,r100,qcmin,zero,one
    use gsi_io, only: lendian_in,lendian_out
    use gsi_metguess_mod, only: gsi_metguess_get,gsi_metguess_bundle
    use gsi_bundlemod, only: gsi_bundlegetpointer
    use mpeu_util, only: die,getindex
    use control_vectors, only: cvars3d
    use native_endianness, only: byte_swap
    use gfs_stratosphere, only: use_gfs_stratosphere,nsig_save
    use gfs_stratosphere, only: revert_to_nmmb,restore_nmmb_gfs
    use mpeu_util, only: die
    use wrwrfmassa_mod, only: wrwrfmassa_class
    use read_wrf_mass_guess_mod, only: read_wrf_mass_guess_class
  
    implicit none
  
  ! Declare passed variables
    class(wrwrfnmma_class), intent(inout) :: this 
    integer(i_kind),intent(in   ) :: mype
  
  ! Declare local constants
    type(read_wrf_mass_guess_class) :: read_wrf
    type(wrwrfmassa_class) :: wrwrfmassa
    real(r_kind),parameter:: r225=225.0_r_kind
  
  ! Declare local variables
    character(10) wrfanl
  
    integer(i_kind) im,jm,lm
    integer(i_kind) nsig_write 
    real(r_single),allocatable::temp1(:),tempa(:,:),tempb(:,:)
    real(r_single),allocatable::all_loc(:,:,:)
    integer(i_kind),allocatable::igtype(:),kdim(:),kord(:)
    integer(kind=mpi_offset_kind),allocatable::offset(:)
    integer(kind=mpi_offset_kind) this_offset,offset_start_date,offset_nstart_hour
    integer(i_kind),allocatable::length(:)
    integer(i_kind) this_length,length_start_date
    integer(i_llong) num_swap
    character(6) filename
    integer(i_kind) i,j,k,kpint,kt,kq,ku,kv,it,i_pd,i_pint,i_t,i_q,i_u,i_v
    integer(i_kind) i_sst,i_tsk,i_cwm,i_f_ice,i_f_rain
    integer(i_kind) kcwm,kf_ice,kf_rain
    integer(i_kind) num_nmm_fields,num_j_groups,num_loc_groups
    real(r_kind) pd,psfc_this
    integer(i_llong) n_position
    integer(i_kind) iskip,jextra,nextra
    integer(i_kind) status(mpi_status_size)
    integer(i_kind) jbegin(0:npe),jend(0:npe-1)
    integer(i_kind) kbegin(0:npe),kend(0:npe-1)
    integer(i_long),allocatable:: ibuf(:,:)
    integer(i_long),allocatable:: jbuf(:,:,:)
    integer(i_kind) ifld,mfcst
    integer(i_long) iyear,imonth,iday,ihour,iminute,isecond
    character(1) chdrbuf(2048)
    integer(i_kind) iadd
    character(132) memoryorder
  
  ! variables for cloud info
    integer(i_kind) iret,ier,n_actual_clouds
    integer(i_kind) icw4crtm,iqtotal,istatus
    real(r_kind) total_ice
    real(r_kind),dimension(lat2,lon2):: work_clwmr,work_fice,work_frain
    real(r_kind),pointer,dimension(:,:  ):: ges_pd  =>NULL()
    real(r_kind),pointer,dimension(:,:  ):: ges_ps  =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_u   =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_v   =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_pint=>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_q   =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_ql  =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_qi  =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_qr  =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_qs  =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_qg  =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_qh  =>NULL()
    real(r_single),allocatable:: pdbg(:),tbg(:,:),qbg(:,:),cwmbg(:,:),ubg(:,:),vbg(:,:)
    real(r_single),allocatable:: pdba(:),tba(:,:),qba(:,:),cwmba(:,:),uba(:,:),vba(:,:)
    real(r_single),allocatable:: pdbg0(:),tbg0(:,:),qbg0(:,:),cwmbg0(:,:),ubg0(:,:),vbg0(:,:)
    real(r_single),allocatable:: pdba0(:),tba0(:,:),qba0(:,:),cwmba0(:,:),uba0(:,:),vba0(:,:)
    integer(i_kind) bdim
    integer(i_kind) nstart_hour
  
  !   1. get offsets etc only for records to be updated
  
  !        they are PD, T, Q, U, V, skint/sst
  
    im=nlon_regional
    jm=nlat_regional
    lm=nsig
  
  ! if use_gfs_stratosphere is true, then convert ges fields from nmm-gfs
  ! extended vertical coordinate to nmmb vertical coordinate.
    if(use_gfs_stratosphere) then
       call revert_to_nmmb
       nsig_write=nsig_save
       lm=nsig_save
    else
       nsig_write=nsig
       lm=nsig
    endif
     if (mype==0) write(6,*)'wrwrfnmma_binary: nsig_write =   ', nsig_write
     if (mype==0) write(6,*)'wrwrfnmma_binary: nsig =         ', nsig
     if (mype==0) write(6,*)'wrwrfnmma_binary: lm =           ', lm
     if (mype==0) write(6,*)'wrwrfnmma_binary: jm =           ', jm
     if (mype==0) write(6,*)'wrwrfnmma_binary: im =           ', im
     if (mype==0) write(6,*)'wrwrfnmma_binary: nlat_regional =', nlat_regional                     
     if (mype==0) write(6,*)'wrwrfnmma_binary: nlon_regional =', nlon_regional                 
  
  !  allocate boundary file arrays
    bdim=2*im+jm-3
    allocate(pdbg(bdim),tbg(bdim,lm),qbg(bdim,lm),cwmbg(bdim,lm),ubg(bdim,lm),vbg(bdim,lm))
    allocate(pdba(bdim),tba(bdim,lm),qba(bdim,lm),cwmba(bdim,lm),uba(bdim,lm),vba(bdim,lm))
    allocate(pdbg0(bdim),tbg0(bdim,lm),qbg0(bdim,lm),cwmbg0(bdim,lm),ubg0(bdim,lm),vbg0(bdim,lm))
    allocate(pdba0(bdim),tba0(bdim,lm),qba0(bdim,lm),cwmba0(bdim,lm),uba0(bdim,lm),vba0(bdim,lm))
    pdbg=zero  ; tbg=zero  ; qbg=zero  ; cwmbg=zero  ; ubg=zero  ; vbg=zero
    pdba=zero  ; tba=zero  ; qba=zero  ; cwmba=zero  ; uba=zero  ; vba=zero
    pdbg0=zero ; tbg0=zero ; qbg0=zero ; cwmbg0=zero ; ubg0=zero ; vbg0=zero
    pdba0=zero ; tba0=zero ; qba0=zero ; cwmba0=zero ; uba0=zero ; vba0=zero
  
    it=ntguessig
  
  ! inquiry cloud guess
    call gsi_metguess_get('clouds::3d',n_actual_clouds,iret)
    if (n_actual_clouds>0) then
  !    Determine whether or not cloud-condensate is the control variable
       icw4crtm=getindex(cvars3d,'cw')
  
  !    Determine whether total moisture (water vapor+total cloud condensate) is the control variable
       iqtotal=getindex(cvars3d,'qt')
  
  !    Get pointer to cloud water mixing ratio
       ier=0
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'ql',ges_ql,iret); ier=iret
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qi',ges_qi,iret); ier=ier+iret
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qr',ges_qr,iret); ier=ier+iret
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qs',ges_qs,iret); ier=ier+iret
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qg',ges_qg,iret); ier=ier+iret
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qh',ges_qh,iret); ier=ier+iret
  
       if ((icw4crtm<=0 .and. iqtotal<=0) .or. ier/=0) n_actual_clouds=0
    end if
  
  
    num_nmm_fields=3+4*lm
    if(update_pint) num_nmm_fields=num_nmm_fields+lm+1  ! contribution from PINT
    if (n_actual_clouds>0) num_nmm_fields=num_nmm_fields+3*lm
    allocate(offset(num_nmm_fields))
    allocate(igtype(num_nmm_fields),kdim(num_nmm_fields),kord(num_nmm_fields))
    allocate(length(num_nmm_fields))
  
  !    igtype is a flag indicating whether each input NMM field is h-, or v-grid
  !    and whether integer or real
  !     abs(igtype)=1 for h-grid
  !                =2 for u-grid
  !
  !     igtype = -1 for integer field
  
  !    offset is the byte count preceding each record to be read/written from/to the wrf binary file.
  !       used as individual file pointers by mpi_file_read/mpi_file_write
  
    it=ntguessig
  
    write(filename,'("sigf",i2.2)')ifilesig(it)
    open(lendian_in,file=filename,form='unformatted') ; rewind lendian_in
    if(mype == 0) write(6,*)'READ_WRF_NMM_OFFSET_FILE:  open lendian_in=',lendian_in,' to file=',filename
    read(lendian_in) iyear,imonth,iday,ihour,iminute,isecond
  
    do iskip=2,9
       read(lendian_in)
    end do
    read(lendian_in) 
    read(lendian_in) n_position          !  offset for START_DATE record
    offset_start_date=n_position
    length_start_date=2048
  
  !     open wrf file for mpi-io reading and writing
    wrfanl = 'wrf_inout'
    call mpi_file_open(mpi_comm_world,trim(wrfanl),mpi_mode_rdwr,mpi_info_null,mfcst,ierror)
  
  !     update START_DATE record so it contains new analysis time in place of old starting time
    call mpi_file_read_at(mfcst,offset_start_date,chdrbuf,length_start_date,mpi_byte,status,ierror)
    if(mype==0)  then
       call wrwrfmassa%update_start_date(chdrbuf,iyear,imonth,iday,ihour,iminute,isecond)
       call mpi_file_write_at(mfcst,offset_start_date,chdrbuf,length_start_date,mpi_byte,status,ierror)
    end if
  
  !    update NSTART_HOUR for wrf restart file
    read(lendian_in) n_position
    offset_nstart_hour=n_position
    if(offset_nstart_hour > 0)then
       call mpi_file_read_at(mfcst,offset_nstart_hour,nstart_hour,1,mpi_integer4,status,ierror)
       if(mype==0)print *,'nstart_hour=', nstart_hour
       if(mype==0)  then
          nstart_hour=ihour
          print *,'new nstart_hour=', nstart_hour
          call mpi_file_write_at(mfcst,offset_nstart_hour,nstart_hour,1,mpi_integer4,status,ierror)
       end if
    end if
  
    if(mype==0) write(6,*)' in read_wrf_nmm_binary_guess, wrfanl=',trim(wrfanl)
  
    i=0
    i=i+1 ; i_pd=i                                                ! pd
    read(lendian_in) n_position
    offset(i)=n_position ; length=im*jm ; igtype(i)=1 ; kdim(i)=1
    if(mype == 0) write(6,*)' pd, i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
  
    read(lendian_in)                                                   ! fis
  
    i_pint=i+1
    if(update_pint) then
       i_pint=i+1
       read(lendian_in) n_position,memoryorder
       do k=1,lm+1
          i=i+1                                                     ! pint(k)
          if(trim(memoryorder)=='XZY') then
             iadd=0
             kord(i)=lm+1
          else
             iadd=(k-1)*im*jm*4
             kord(i)=1
          end if
          offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=lm+1
          if(mype == 0.and.k==1) write(6,*)' temp i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
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
  
    read(lendian_in)                                                    ! sm
    read(lendian_in)                                                    ! sice
  
    i=i+1 ; i_sst=i                                                ! sst
    read(lendian_in) n_position
    offset(i)=n_position ; length=im*jm ; igtype(i)=1 ; kdim(i)=1
    if(mype == 0) write(6,*)' sst, i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
  
    read(lendian_in)                                                    ! ivgtyp
    read(lendian_in)                                                    ! isltyp
    read(lendian_in)                                                    ! vegfrac
    read(lendian_in)                                                    ! sno
    read(lendian_in)                                                    ! u10
    read(lendian_in)                                                    ! v10
    read(lendian_in)                                                    ! smc
    read(lendian_in)                                                    ! stc
  
    i=i+1 ; i_tsk=i                                                ! tsk
    read(lendian_in) n_position
    offset(i)=n_position ; length=im*jm ; igtype(i)=1 ; kdim(i)=1
    if(mype == 0) write(6,*)' tsk, i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
  
    if (n_actual_clouds>0) then
       i_cwm=i+1
       read(lendian_in) n_position,memoryorder
       do k=1,lm
          i=i+1                                                    ! cwm(k)
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
          i=i+1                                                    ! f_ice(k)
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
          i=i+1                                                    ! f_rain(k)
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
    end if  ! end of n_actual_clouds>0
  
    close(lendian_in)
  
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
!   if(mype == 0) then
!      write(6,*)' kbegin=',kbegin
!      write(6,*)' kend= ',kend
!   end if
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
!   if(mype == 0) then
!      write(6,*)' jbegin=',jbegin
!      write(6,*)' jend= ',jend
!   end if
  
    ier=0
    call gsi_bundlegetpointer (gsi_metguess_bundle(it),'ps',ges_ps,iret); ier=iret
    call gsi_bundlegetpointer (gsi_metguess_bundle(it),'u' ,ges_u ,iret); ier=iret
    call gsi_bundlegetpointer (gsi_metguess_bundle(it),'v' ,ges_v ,iret); ier=iret
    call gsi_bundlegetpointer (gsi_metguess_bundle(it),'q' ,ges_q ,iret); ier=iret
    if (ier/=0) then ! doesn't need to die (but needs careful revision)
       call die('wrwrfnmma_binary',': missing guess fields',ier)
    endif
    
  ! Create all_loc from ges_*
    allocate(all_loc(lat1,lon1,num_nmm_fields))
    all_loc=zero_single
    kt=i_t-1
    kq=i_q-1
    ku=i_u-1
    kv=i_v-1
    do k=1,nsig_write   
       kt=kt+1
       kq=kq+1
       ku=ku+1
       kv=kv+1
       do i=1,lon1
          do j=1,lat1
             all_loc(j,i,ku)=ges_u(j+1,i+1,k)
             all_loc(j,i,kv)=ges_v(j+1,i+1,k)
             all_loc(j,i,kq)=ges_q(j+1,i+1,k)
             all_loc(j,i,kt)=ges_tsen(j+1,i+1,k,it)   ! sensible temperature
          end do
       end do
       if (mype==0) then
          write(6,*)'all_loc for t    = ',k,maxval(all_loc(:,:,kt)),minval(all_loc(:,:,kt))                
          write(6,*)'all_loc for q    = ',k,maxval(all_loc(:,:,kq)),minval(all_loc(:,:,kq)) 
          write(6,*)'all_loc for u    = ',k,maxval(all_loc(:,:,ku)),minval(all_loc(:,:,ku))              
          write(6,*)'all_loc for v    = ',k,maxval(all_loc(:,:,kv)),minval(all_loc(:,:,kv))                   
        endif
  
    end do
    do i=1,lon1
       do j=1,lat1
          psfc_this=r10*ges_ps(j+1,i+1)   ! convert from mb to cb
          pd=psfc_this-pdtop_ll-pt_ll
          all_loc(j,i,i_pd)=r100*pd
       end do
    end do
    if (mype==0) &
    write(6,*)'all_loc for pd   = ',k,maxval(all_loc(:,:,i_pd)),minval(all_loc(:,:,i_pd))                 
  
  !                    update pint by adding eta2(k)*pdinc
    if(update_pint) then
       ier=0
       call gsi_bundlegetpointer(gsi_metguess_bundle(it),'pint', ges_pint, istatus)
       ier=ier+istatus
       call gsi_bundlegetpointer(gsi_metguess_bundle(it),'pd'  , ges_pd  , istatus)
       ier=ier+istatus
       if (ier/=0) then ! doesn't need to die (but needs careful revision)
          call die('wrwrfnmma_binary',': missing pint/pd fields',ier)
       endif
       kpint=i_pint-1
       do k=1,nsig_write+1
          kpint=kpint+1
          do i=1,lon1
             do j=1,lat1
                all_loc(j,i,kpint)=ges_pint(j+1,i+1,k) &
                            +eta2_ll(k)*(all_loc(j,i,i_pd)-ges_pd(j+1,i+1))   ! pint
             end do
          end do
          if (mype==0) &
          write(6,*)'all_loc for pint = ',k,maxval(all_loc(:,:,kpint)),minval(all_loc(:,:,kpint))      
       end do
    end if
    if(update_regsfc) then
       do i=1,lon1
          do j=1,lat1
             all_loc(j,i,i_sst)=dsfct(j+1,i+1,ntguessfc)
             all_loc(j,i,i_tsk)=dsfct(j+1,i+1,ntguessfc)
          end do
       end do
    end if
  ! cloud info: currently no new f_rimef is written out
    if (n_actual_clouds>0) then  
       kcwm=i_cwm-1
       kf_ice=i_f_ice-1
       kf_rain=i_f_rain-1
       do k=1,nsig
          do i=1,lon2
             do j=1,lat2
                if (ges_ql(j,i,k)<=qcmin) ges_ql(j,i,k)=zero
                if (ges_qi(j,i,k)<=qcmin) ges_qi(j,i,k)=zero
                if (ges_qs(j,i,k)<=qcmin) ges_qs(j,i,k)=zero
                if (ges_qg(j,i,k)<=qcmin) ges_qg(j,i,k)=zero
                if (ges_qh(j,i,k)<=qcmin) ges_qh(j,i,k)=zero
                if (ges_qr(j,i,k)<=qcmin) ges_qr(j,i,k)=zero
                total_ice=ges_qi(j,i,k)+ges_qs(j,i,k)+ges_qg(j,i,k)+ges_qh(j,i,k)
                work_clwmr(j,i)=total_ice+ges_ql(j,i,k)+ges_qr(j,i,k)
                if (work_clwmr(j,i)>zero) then
                   work_fice(j,i)=total_ice/work_clwmr(j,i)
                   if (work_fice(j,i)<one) then
                      work_frain(j,i)=ges_qr(j,i,k)/(work_clwmr(j,i)*(one-work_fice(j,i)))
                   else
                      work_frain(j,i)=zero
                   end if
                else
                   work_fice(j,i)=zero
                   work_frain(j,i)=zero
                end if
             end do
          end do
  
          kcwm=kcwm+1
          kf_ice=kf_ice+1
          kf_rain=kf_rain+1
          do i=1,lon1
             do j=1,lat1
                all_loc(j,i,kcwm)=work_clwmr(j+1,i+1)
                all_loc(j,i,kf_ice)=work_fice(j+1,i+1)
                all_loc(j,i,kf_rain)=work_frain(j+1,i+1)
             end do
          end do
       end do
    end if  ! end of n_actual_clouds>0
  
    
    allocate(tempa(itotsub,kbegin(mype):kend(mype)))
    call wrwrfmassa%generic_sub2grid(all_loc,tempa,kbegin(mype),kend(mype),kbegin,kend,mype,num_nmm_fields)
    deallocate(all_loc)
  
    allocate(ibuf(im*jm,kbegin(mype):kend(mype)))
  
  !   2.  create ibuf with records to be updated read in
  
  !                                    read pint
    if(update_pint.and.kord(i_pint)/=1) then
       allocate(jbuf(im,lm+1,jbegin(mype):jend(mype)))
       this_offset=offset(i_pint)+(jbegin(mype)-1)*4*im*(lm+1)
       this_length=(jend(mype)-jbegin(mype)+1)*im*(lm+1)
       call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
       if(byte_swap) then
          num_swap=this_length
          call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
       end if
       call read_wrf%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                          jbegin,jend,kbegin,kend,mype,npe,im,jm,lm+1,im,jm,i_pint,i_pint+lm)
       deallocate(jbuf)
    end if
  
  !                                    read temps
    if(kord(i_t)/=1) then
       allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
       this_offset=offset(i_t)+(jbegin(mype)-1)*4*im*lm
       this_length=(jend(mype)-jbegin(mype)+1)*im*lm
       call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
       if(byte_swap) then
          num_swap=this_length
          call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
       end if
       call read_wrf%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                          jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im,jm,i_t,i_t+lm-1)
       deallocate(jbuf)
    end if
  
  !                                    read q
    if(kord(i_q)/=1) then
       allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
       this_offset=offset(i_q)+(jbegin(mype)-1)*4*im*lm
       this_length=(jend(mype)-jbegin(mype)+1)*im*lm
       call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
       if(byte_swap) then
          num_swap=this_length
          call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
       end if
       call read_wrf%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                          jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im,jm,i_q,i_q+lm-1)
       deallocate(jbuf)
    end if
  
  !                                    read u
    if(kord(i_u)/=1) then
       allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
       this_offset=offset(i_u)+(jbegin(mype)-1)*4*im*lm
       this_length=(jend(mype)-jbegin(mype)+1)*im*lm
       call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
       if(byte_swap) then
          num_swap=this_length
          call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
       end if
       call read_wrf%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                          jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im,jm,i_u,i_u+lm-1)
       deallocate(jbuf)
    end if
  
  !                                    read v
    if(kord(i_v)/=1) then
       allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
       this_offset=offset(i_v)+(jbegin(mype)-1)*4*im*lm
       this_length=(jend(mype)-jbegin(mype)+1)*im*lm
       call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
       if(byte_swap) then
          num_swap=this_length
          call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
       end if
       call read_wrf%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                          jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im,jm,i_v,i_v+lm-1)
       deallocate(jbuf)
    end if
  
    if (n_actual_clouds>0) then 
  !                                    read cwm (no read-in for cloud info, so whole field is write out later)
       if(kord(i_cwm)/=1) then
          allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
          jbuf=zero  
          call read_wrf%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                             jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im,jm,i_cwm,i_cwm+lm-1)
          deallocate(jbuf)
       end if
  !                                    read f_ice (no read-in for cloud info, so whole field is write out later)
       if(kord(i_f_ice)/=1) then
          allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
          jbuf=zero
          call read_wrf%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                             jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im,jm,i_f_ice,i_f_ice+lm-1)
          deallocate(jbuf)
       end if
  !                                    read f_rain (no read-in for cloud info, so whole field is write out later)
       if(kord(i_f_rain)/=1) then
          allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
          jbuf=zero
          call read_wrf%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                             jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im,jm,i_f_rain,i_f_rain+lm-1)
          deallocate(jbuf)
       end if
    end if
  
  
  !---------------------- read surface files last
    do k=kbegin(mype),kend(mype)
       if(kdim(k)==1.or.kord(k)==1) then
          call mpi_file_read_at(mfcst,offset(k),ibuf(1,k),length(k),mpi_integer4,status,ierror)
          if(byte_swap) then
             num_swap=length(k)
             call to_native_endianness_i4(ibuf(1,k),num_swap)
          end if
       end if
    end do
  
  !   5.  tempa --> updated ibuf --> jbuf --> write out
  
    allocate(tempb(itotsub,kbegin(mype):kend(mype)))
    allocate(temp1(im*jm))
    do ifld=kbegin(mype),kend(mype)
       if((ifld==i_sst.or.ifld==i_tsk).and..not.update_regsfc) cycle
       call read_wrf%move_ibuf_hg(ibuf(1,ifld),temp1,im,jm,im,jm)
       call this%get_bndy_file(temp1,pdbg,tbg,qbg,cwmbg,ubg,vbg,ifld,i_pd,i_t,i_q,i_cwm,i_u,i_v, &
                          n_actual_clouds,im,jm,lm,bdim,igtype(ifld))
       if(filled_grid) call fill_nmm_grid2(temp1,im,jm,tempb(1,ifld),igtype(ifld),2)
       if(half_grid)   call half_nmm_grid2(temp1,im,jm,tempb(1,ifld),igtype(ifld),2)
       if(ifld==i_sst.or.ifld==i_tsk) then
          do i=1,iglobal
             if(tempb(i,ifld) < r225) then
                tempa(i,ifld)=zero_single
             else
                tempa(i,ifld)=tempa(i,ifld)-tempb(i,ifld)
             end if
          end do
       else
          do i=1,iglobal
             tempa(i,ifld)=tempa(i,ifld)-tempb(i,ifld)
          end do
       end if
       if(filled_grid) call unfill_nmm_grid2(tempa(1,ifld),im,jm,temp1,igtype(ifld),2)
       if(half_grid)   call unhalf_nmm_grid2(tempa(1,ifld),im,jm,temp1,igtype(ifld),2)
       call this%get_bndy_file(temp1,pdba,tba,qba,cwmba,uba,vba,ifld,i_pd,i_t,i_q,i_cwm,i_u,i_v, &
                          n_actual_clouds,im,jm,lm,bdim,igtype(ifld))
       call wrwrfmassa%move_hg_ibuf(temp1,ibuf(1,ifld),im,jm,im,jm)
    end do
  
  !  do reduce add to pe 0 of all boundary variables, then write out boundary variables.
        ! write(6,'(" pdbg(1)=",e10.1)')pdbg(1)
        ! do k=1,lm
        !    write(6,'(" k,t,q,u,v=",i3,4e10.1)')k,tbg(1,k),qbg(1,k),ubg(1,k),vbg(1,k)
        ! end do
    call mpi_reduce(pdbg,pdbg0,bdim,mpi_real4,mpi_sum,0,mpi_comm_world,ierror)
    call mpi_reduce(pdba,pdba0,bdim,mpi_real4,mpi_sum,0,mpi_comm_world,ierror)
    call mpi_reduce(tbg,tbg0,bdim*lm,mpi_real4,mpi_sum,0,mpi_comm_world,ierror)
    call mpi_reduce(tba,tba0,bdim*lm,mpi_real4,mpi_sum,0,mpi_comm_world,ierror)
    call mpi_reduce(qbg,qbg0,bdim*lm,mpi_real4,mpi_sum,0,mpi_comm_world,ierror)
    call mpi_reduce(qba,qba0,bdim*lm,mpi_real4,mpi_sum,0,mpi_comm_world,ierror)
    call mpi_reduce(cwmbg,cwmbg0,bdim*lm,mpi_real4,mpi_sum,0,mpi_comm_world,ierror)
    call mpi_reduce(cwmba,cwmba0,bdim*lm,mpi_real4,mpi_sum,0,mpi_comm_world,ierror)
    call mpi_reduce(ubg,ubg0,bdim*lm,mpi_real4,mpi_sum,0,mpi_comm_world,ierror)
    call mpi_reduce(uba,uba0,bdim*lm,mpi_real4,mpi_sum,0,mpi_comm_world,ierror)
    call mpi_reduce(vbg,vbg0,bdim*lm,mpi_real4,mpi_sum,0,mpi_comm_world,ierror)
    call mpi_reduce(vba,vba0,bdim*lm,mpi_real4,mpi_sum,0,mpi_comm_world,ierror)
    if(mype==0) then
       open(lendian_out,file='wrf_nmm_bnd',form='unformatted')
       write(lendian_out)'WRF-NMM-BINARY'
       write(lendian_out) iyear,imonth,iday,ihour,iminute,isecond,im,jm,lm,bdim
       write(lendian_out) pdbg0,tbg0,qbg0,cwmbg0,ubg0,vbg0
       write(lendian_out) pdba0,tba0,qba0,cwmba0,uba0,vba0
       close(lendian_out)
         write(6,*)' min,max pdbg0=',minval(pdbg0),maxval(pdbg0)
         write(6,*)' min,max tbg0=',minval(tbg0),maxval(tbg0)
         write(6,*)' min,max qbg0=',minval(qbg0),maxval(qbg0)
         write(6,*)' min,max cwmbg0=',minval(cwmbg0),maxval(cwmbg0)
         write(6,*)' min,max ubg0=',minval(ubg0),maxval(ubg0)
         write(6,*)' min,max vbg0=',minval(vbg0),maxval(vbg0)
         write(6,*)' min,max pdba0=',minval(pdba0),maxval(pdba0)
         write(6,*)' min,max tba0=',minval(tba0),maxval(tba0)
         write(6,*)' min,max qba0=',minval(qba0),maxval(qba0)
         write(6,*)' min,max cwmba0=',minval(cwmba0),maxval(cwmba0)
         write(6,*)' min,max uba0=',minval(uba0),maxval(uba0)
         write(6,*)' min,max vba0=',minval(vba0),maxval(vba0)
         write(6,*)' min,max pdba0-pdbg0=',minval(pdba0-pdbg0),maxval(pdba0-pdbg0)
         write(6,*)' min,max tba0-tbg0=',minval(tba0-tbg0),maxval(tba0-tbg0)
         write(6,*)' min,max qba0-qbg0=',minval(qba0-qbg0),maxval(qba0-qbg0)
         write(6,*)' min,max cwmba0-cwmbg0=',minval(cwmba0-cwmbg0),maxval(cwmba0-cwmbg0)
         write(6,*)' min,max uba0-ubg0=',minval(uba0-ubg0),maxval(uba0-ubg0)
         write(6,*)' min,max vba0-vbg0=',minval(vba0-vbg0),maxval(vba0-vbg0)
    end if
  
    deallocate(pdbg ,tbg ,qbg ,cwmbg ,ubg ,vbg )
    deallocate(pdba ,tba ,qba ,cwmba ,uba ,vba )
    deallocate(pdbg0,tbg0,qbg0,cwmbg0,ubg0,vbg0)
    deallocate(pdba0,tba0,qba0,cwmba0,uba0,vba0)
  
  !           finally write ibuf back out ( ibuf --> jbuf -->  mpi_file_write )
  
  !                                    write pint
    if(update_pint.and.kord(i_pint)/=1) then
       allocate(jbuf(im,lm+1,jbegin(mype):jend(mype)))
       this_length=(jend(mype)-jbegin(mype)+1)*im*(lm+1)
  
       call wrwrfmassa%transfer_ibuf2jbuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                          jbegin,jend,kbegin,kend,mype,npe,im,jm,lm+1,im,jm,i_pint,i_pint+lm)
       this_offset=offset(i_pint)+(jbegin(mype)-1)*4*im*(lm+1)
       if(byte_swap) then
          num_swap=this_length
          call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
       end if
       call mpi_file_write_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
       deallocate(jbuf)
    end if
  
  !                                    write temps
    if(kord(i_t)/=1) then
       allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
       this_length=(jend(mype)-jbegin(mype)+1)*im*lm
   
       call wrwrfmassa%transfer_ibuf2jbuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                          jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im,jm,i_t,i_t+lm-1)
       this_offset=offset(i_t)+(jbegin(mype)-1)*4*im*lm
       if(byte_swap) then
          num_swap=this_length
          call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
       end if
       call mpi_file_write_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
       deallocate(jbuf)
    end if
  
  !                                    write q
    if(kord(i_q)/=1) then
       allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
       call wrwrfmassa%transfer_ibuf2jbuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                          jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im,jm,i_q,i_q+lm-1)
       this_offset=offset(i_q)+(jbegin(mype)-1)*4*im*lm
       if(byte_swap) then
          num_swap=this_length
          call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
       end if
       call mpi_file_write_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
       deallocate(jbuf)
    end if
  
  !                                    write u
    if(kord(i_u)/=1) then
       allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
       call wrwrfmassa%transfer_ibuf2jbuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                          jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im,jm,i_u,i_u+lm-1)
       this_offset=offset(i_u)+(jbegin(mype)-1)*4*im*lm
       if(byte_swap) then
          num_swap=this_length
          call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
       end if
       call mpi_file_write_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
       deallocate(jbuf)
    end if
  
  !                                    write v
    if(kord(i_v)/=1) then
       allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
       call wrwrfmassa%transfer_ibuf2jbuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                          jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im,jm,i_v,i_v+lm-1)
       this_offset=offset(i_v)+(jbegin(mype)-1)*4*im*lm
       if(byte_swap) then
          num_swap=this_length
          call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
       end if
       call mpi_file_write_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
       deallocate(jbuf)
    end if
  
    if (n_actual_clouds>0) then
  !                                    write cwm
       if(kord(i_cwm)/=1) then
          allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
          call wrwrfmassa%transfer_ibuf2jbuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                             jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im,jm,i_cwm,i_cwm+lm-1)
          this_offset=offset(i_cwm)+(jbegin(mype)-1)*4*im*lm
          if(byte_swap) then
             num_swap=this_length
             call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
          end if
          call mpi_file_write_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
          deallocate(jbuf)
       end if
  !                                    write f_ice
       if(kord(i_f_ice)/=1) then
          allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
          call wrwrfmassa%transfer_ibuf2jbuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                             jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im,jm,i_f_ice,i_f_ice+lm-1)
          this_offset=offset(i_f_ice)+(jbegin(mype)-1)*4*im*lm
          if(byte_swap) then
             num_swap=this_length
             call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
          end if
          call mpi_file_write_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
          deallocate(jbuf)
       end if
  !                                    write f_rain
       if(kord(i_f_rain)/=1) then
          allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
          call wrwrfmassa%transfer_ibuf2jbuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                             jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im,jm,i_f_rain,i_f_rain+lm-1)
          this_offset=offset(i_f_rain)+(jbegin(mype)-1)*4*im*lm
          if(byte_swap) then
             num_swap=this_length
             call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
          end if
          call mpi_file_write_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
          deallocate(jbuf)
       end if
    end if
  !---------------------- write surface files last
    do k=kbegin(mype),kend(mype)
       if(kdim(k)==1.or.kord(k)==1) then
          if(byte_swap) then
             num_swap=length(k)
             call to_native_endianness_i4(ibuf(1,k),num_swap)
          end if
          call mpi_file_write_at(mfcst,offset(k),ibuf(1,k),length(k),mpi_integer4,status,ierror)
       end if
    end do
  
    deallocate(ibuf)
    deallocate(offset)
    deallocate(igtype)
    deallocate(kdim)
    deallocate(kord)
    deallocate(length)
    deallocate(tempa)
    deallocate(tempb)
    deallocate(temp1)
  
    call mpi_file_close(mfcst,ierror)
   
    if(use_gfs_stratosphere) then
        if(mype==0) write(6,*)' at wrwrfnmma_binary: restore ges fields back to extended vertical grid'    
        call restore_nmmb_gfs 
    endif
    
  end subroutine wrwrfnmma_binary_wrf
  
  subroutine get_bndy_file(temp1,pdb,tb,qb,cwmb,ub,vb,ifld,i_pd,i_t,i_q,i_cwm,i_u,i_v, &
                           n_actual_clouds,im,jm,lm,bdim,igtype)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    get_bndy_file          collect boundary variables on nmm grid
  !   prgmmr: parrish          org: np22                date: 2012-03-09
  !
  ! abstract:  transfer boundary values from wrf nmm (E-grid) variables to boundary arrays.
  !
  ! program history log:
  !   2004-06-23  parrish, document
  !   2012-11-19  tong, added n_actual_clouds > 0 condition for cloud variable cwm.
  !   2013-10-19  todling - metguess now holds background
  !
  !   input argument list:
  !     temp1    - input 2d field
  !     ifld     - counter for all vars/levels concatenated together
  !     i_pd     - if ifld==i_pd, then contents of temp1 are i_pd
  !     i_t      - if ifld>=i_t and ifld-i_t+1 <= lm, then contents of temp1 are T(ifld-i_t+1)
  !     i_q      - same as for i_t but for specific humidity
  !     i_cwm    - same as for i_t but for cloud variable
  !     i_u      - same as for i_t but for u
  !     i_v      - same as for i_t but for v
  !     n_actual_clouds   - number of cloud guess variables
  !     im,jm,lm - wrf nmm grid dimensions
  !     bdim     - number of points around boundary
  !     igtype   - =1, then h grid, =2, then v grid
  !
  !
  !   output argument list:
  !     pdb      - boundary values of pd
  !     tb       - same for T
  !     qb       - same for q
  !     cwmb     - same for cwm
  !     ub       - same for u
  !     vb       - same for v
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$
    use kinds, only: r_single,i_kind
    implicit none
  
    integer(i_kind),intent(in   ) :: ifld,i_pd,i_t,i_q,i_cwm,i_u,i_v,im,jm,lm,bdim,igtype
    integer(i_kind),intent(in   ) :: n_actual_clouds
    real(r_single), intent(in   ) :: temp1(im,jm)
    real(r_single), intent(  out) :: pdb(bdim),tb(bdim,lm),qb(bdim,lm),cwmb(bdim,lm),ub(bdim,lm),vb(bdim,lm)
  
    integer(i_kind) i,j,ii
    real(r_single) bndy(bdim)
  
  !   transfer boundary points to bndy
  
    if(igtype==1) then
  
  !    transfer h-grid boundary points:
  
       ii=0
       do i=1,im
          ii=ii+1
          bndy(ii)=temp1(i,1)
       end do
       do i=1,im
          ii=ii+1
          bndy(ii)=temp1(i,jm)
       end do
       do j=3,jm-2,2
          ii=ii+1
          bndy(ii)=temp1(1,j)
       end do
       do j=3,jm-2,2
          ii=ii+1
          bndy(ii)=temp1(im,j)
       end do
  
    else
  
  !    transfer v-grid boundary points:
  
       ii=0
       do i=1,im-1
          ii=ii+1
          bndy(ii)=temp1(i,1)
       end do
       do i=1,im-1
          ii=ii+1
          bndy(ii)=temp1(i,jm)
       end do
       do j=2,jm-1,2
          ii=ii+1
          bndy(ii)=temp1(1,j)
       end do
       do j=2,jm-1,2
          ii=ii+1
          bndy(ii)=temp1(im,j)
       end do
  
    end if
  
  !   now transfer bndy to appropriate output array:
  
    if(ifld==i_pd) then
  
       pdb(:)=bndy(:)
  
    elseif(ifld>=i_t.and.ifld-i_t+1 <= lm) then
  
       tb(:,ifld-i_t+1)=bndy(:)
  
    elseif(ifld >= i_q .and. ifld-i_q+1 <= lm) then
  
       qb(:,ifld-i_q+1)=bndy(:)
  
    elseif(ifld >= i_cwm .and. ifld-i_cwm+1 <= lm .and. n_actual_clouds > 0) then
  
       cwmb(:,ifld-i_cwm+1)=bndy(:)
  
    elseif(ifld >= i_u .and. ifld-i_u+1 <= lm) then
  
       ub(:,ifld-i_u+1)=bndy(:)
  
    elseif(ifld >= i_v .and. ifld-i_v+1 <= lm) then
  
       vb(:,ifld-i_v+1)=bndy(:)
  
    end if
  
  end subroutine get_bndy_file
  
  subroutine wrnemsnmma_binary(this,mype)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    wrwrfnmma              write out wrf NMM restart file
  !   prgmmr: parrish          org: np22                date: 2004-06-23
  !
  ! abstract:  read wrf NMM guess restart interface file, add analysis
  !            increment, and write out wrf NMM analysis restart 
  !            interface file.
  !
  ! program history log:
  !   2004-06-23  parrish, document
  !   2004-08-03  treadon - add only to module use, add intent in/out
  !   2004-11-22  parrish - rewrite for mpi-io
  !   2004-12-15  treadon - write analysis to file "wrf_inout"
  !   2005-07-06  parrish - update and write out pint if update_pint=.true.
  !   2006-04-06  middlecoff - changed nfcst from 11 to lendian_in
  !   2006-07-28  derber - include sensible temperature
  !   2006-07-31  kleist - change to use ges_ps instead of lnps
  !   2007-04-12  parrish - add modifications to allow any combination of ikj or ijk
  !                          grid ordering for input 3D fields
  !   2007-05-02  parrish - fix bug to prevent out of memory reference when pint missing
  !   2008-04-01  safford - rm unused uses
  !   2008-12-05  todling - adjustment for dsfct time dimension addition
  !   2010-01-18  parrish - add update of 10m wind, 2m pot temp, 2m specific humidity
  !   2010-03-12  parrish - add write of ozone to 3d field labeled "o3mr"  (might be changed to "o3")
  !   2010-03-15  parrish - add flag regional_ozone to turn on ozone in regional analysis
  !   2011-07-18  zhu     - add write-out for updated cloud info
  !   2012-12-04  s.liu   - add gsd cloud analsyis variables
  !   2013-10-18  s.liu   - add use_reflectivity option for cloud analysis variables
  !   2013-10-19  todling - upper-air guess now in metguess
  !   2014-04-11  zhu     - add cold_start option for the case when the restart file is from the GFS
  !   2014-06-05  carley  - bug fix for writing out cloud analysis variables 
  !   2014-06-27  S.Liu   - detach use_reflectivity from n_actual_clouds
  !   2015-05-12  wu      - write analysis to file "wrf_inout(nhr_assimilation)"
  !   2015-05-12  S.Liu   - interpolate water content before converting to fraction
  !   2016-03-02  s.liu/carley - remove use_reflectivity and use i_gsdcldanal_type
  !   2016-06-23  lippi   - add read of vertical velocity (w).
  !   2016-06-30  s.liu - remove gridtype, add_saved in write_fraction
  !   2017-05-12  Y. Wang and X. Wang - add write of hydrometeor-related
  !                        variables (f_rain, f_ice, clwmr and refl_10cm) and W for radar DA, 
  !                        POC: xuguang.wang@ou.edu
  !
  !   input argument list:
  !     mype     - pe number
  !
  !   output argument list:
  !     no output arguments
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$
    use kinds, only: r_kind,i_kind
    use wrf_params_mod, only: update_pint,cold_start
    use guess_grids, only: &
          ntguessfc,ntguessig,ges_tsen,dsfct,isli,geop_hgtl,ges_prsl
    use gridmod, only: pt_ll,update_regsfc,pdtop_ll,nsig,lat2,lon2,eta2_ll,nmmb_verttype,&
          use_gfs_ozone,regional_ozone
    use rapidrefresh_cldsurf_mod, only: i_gsdcldanal_type
    use constants, only: zero,half,one,two,rd_over_cp,r10,r100,qcmin
    use gsi_nemsio_mod, only: gsi_nemsio_open,gsi_nemsio_close,gsi_nemsio_read,gsi_nemsio_write
    use gsi_nemsio_mod, only: gsi_nemsio_update
    use gsi_metguess_mod, only: gsi_metguess_get,gsi_metguess_bundle
    use gsi_bundlemod, only: gsi_bundlegetpointer
    use mpeu_util, only: die,getindex
    use control_vectors, only: cvars3d
    use gfs_stratosphere, only: use_gfs_stratosphere,nsig_save
    use gfs_stratosphere, only: revert_to_nmmb,restore_nmmb_gfs
    use mpimod, only: mpi_comm_world,ierror,mpi_rtype,mpi_integer4,mpi_min,mpi_max,mpi_sum
    use gsi_4dvar, only: nhr_assimilation
    use gsi_nemsio_mod, only: gsi_nemsio_update,gsi_nemsio_write_fraction,gsi_nemsio_write_fractionnew 
    use wrf_vars_mod, only : dbz_exist
    use obsmod,only: if_model_dbz
    implicit none
  
  ! Declare passed variables
    class(wrwrfnmma_class), intent(inout) :: this 
    integer(i_kind),intent(in   ) :: mype
  
  ! Declare local variables
  
    character(255) wrfanl
    logical add_saved
  
    integer(i_kind) i,it,j,k,kr,mype_input,nsig_write
    integer(i_kind) near_sfc,kp
    integer(i_kind) icw4crtm,iqtotal,i_radar_qr,i_radar_qli
    real(r_kind) pd,psfc_this,pd_to_ps,wmag
    real(r_kind),dimension(lat2,lon2):: work_sub,pd_new,delu10,delv10,u10this,v10this,fact10_local
    real(r_kind),dimension(lat2,lon2):: work_sub_t,work_sub_i,work_sub_r,work_sub_l, work_sub_s
    real(r_kind),dimension(lat2,lon2):: delt2,delq2,t2this,q2this,fact2t_local,fact2q_local
    real(r_kind),dimension(lat2,lon2,6):: delu,delv,delw,delt,delq,pott
    real(r_kind) hmin,hmax,hmin0,hmax0,ten,wgt1,wgt2
    logical use_fact10,use_fact2
    logical good_u10,good_v10,good_tshltr,good_qshltr,good_o3mr
  
  ! variables for cloud info
    integer(i_kind) iret,ier_cloud,n_actual_clouds,istatus,ierr
    real(r_kind) total_ice
    real(r_kind),dimension(lat2,lon2):: work_clwmr,work_fice,work_frain
    real(r_kind),pointer,dimension(:,:,:):: ges_cw  =>NULL()
    real(r_kind),pointer,dimension(:,:  ):: ges_pd  =>NULL()
    real(r_kind),pointer,dimension(:,:  ):: ges_ps  =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_u   =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_v   =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_w   =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_q   =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_oz  =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_ql  =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_qi  =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_qr  =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_qs  =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_qg  =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_qh  =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: dfi_tten=>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_ref =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_dw   =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_qli   =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_dbz   =>NULL()
    
  !   if use_gfs_stratosphere is true, then convert ges fields from nmmb-gfs 
  !        extended vertical coordinate to nmmb vertical coordinate.
    associate( this => this ) ! eliminates warning for unused dummy argument needed for binding
    end associate
 
    if(use_gfs_stratosphere) then
       call revert_to_nmmb
       nsig_write=nsig_save
    else
       nsig_write=nsig
    end if
  
    use_fact10=.true.
    use_fact2=.false.
  
  !   decide how many near surface layers to save for interpolation/extrapolation to get u10,v10,t2,q2
  
    near_sfc=1
    do k=1,6
       hmin=minval(geop_hgtl(:,:,k,ntguessig))
       hmax=maxval(geop_hgtl(:,:,k,ntguessig))
       call mpi_allreduce(hmin,hmin0,1,mpi_rtype,mpi_min,mpi_comm_world,ierror)
       call mpi_allreduce(hmax,hmax0,1,mpi_rtype,mpi_max,mpi_comm_world,ierror)
       if(mype == 0) write(6,*)' k,min,max geop_hgtl=',k,hmin0,hmax0
       if(hmin0 < 40._r_kind) near_sfc=k
       hmin=minval(ges_prsl(:,:,k,ntguessig))
       hmax=maxval(ges_prsl(:,:,k,ntguessig))
       call mpi_allreduce(hmin,hmin0,1,mpi_rtype,mpi_min,mpi_comm_world,ierror)
       call mpi_allreduce(hmax,hmax0,1,mpi_rtype,mpi_max,mpi_comm_world,ierror)
       if(mype == 0) write(6,*)' k,min,max ges_prsl=',k,hmin0,hmax0
    end do
    near_sfc=max(near_sfc,2)
    if(mype == 0) write(6,*)' in wrnemsnmma_binary near_sfc=',near_sfc
  
                    
                       
  !     get conversion factor for pd to psfc
  
    if(nmmb_verttype=='OLD') then
       pd_to_ps=pdtop_ll+pt_ll
    else
       pd_to_ps=pt_ll
    end if
  
    it=ntguessig
    mype_input=0
    add_saved=.true.
    i_radar_qr = 0
    i_radar_qli = 0
  
    call gsi_metguess_get('clouds::3d',n_actual_clouds,iret)
    if(mype == 0) write(6,*)' in wrnemsnmma_binary after gsi_metguess_get, nclouds,iret=',&
                  n_actual_clouds,iret
    if (n_actual_clouds>0 .and. (i_gsdcldanal_type/=2)) then
  
  !    Determine whether or not cloud-condensate is the control variable
       icw4crtm=getindex(cvars3d,'cw')
  
  !    Determine whether or not total moisture (water vapor+total cloud condensate) is the control variable
       iqtotal=getindex(cvars3d,'qt')

  !    Determine if qr and qli are control variables for radar data assimilation,
       i_radar_qr=getindex(cvars3d,'qr')
       i_radar_qli=getindex(cvars3d,'qli')
  
  !    Get pointer to cloud water mixing ratio
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'cw',ges_cw,iret); ier_cloud=iret
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'ql',ges_ql,iret); ier_cloud=ier_cloud+iret
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qi',ges_qi,iret); ier_cloud=ier_cloud+iret
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qr',ges_qr,iret); ier_cloud=ier_cloud+iret
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qs',ges_qs,iret); ier_cloud=ier_cloud+iret
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qg',ges_qg,iret); ier_cloud=ier_cloud+iret
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qh',ges_qh,iret); ier_cloud=ier_cloud+iret
  
       if ((icw4crtm<=0 .and. iqtotal<=0) .or. ier_cloud/=0) n_actual_clouds=0

       if (i_radar_qr>0 .and. i_radar_qli>0) then
   !    Get pointer to cloud water mixing ratio
        call gsi_bundlegetpointer (gsi_metguess_bundle(it),'ql',ges_ql,iret); ier_cloud=iret
        call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qi',ges_qi,iret); ier_cloud=iret
        call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qr',ges_qr,iret); ier_cloud=ier_cloud+iret
        call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qli',ges_qli,iret); ier_cloud=ier_cloud+iret
        if(dbz_exist)&
        call gsi_bundlegetpointer (gsi_metguess_bundle(it),'dbz',ges_dbz,iret); ier_cloud=ier_cloud+iret
        if (ier_cloud/=0) n_actual_clouds=0
       else
         n_actual_clouds=0
       end if
  
    else if (i_gsdcldanal_type==2)then
      
  !    Get pointer to hydrometeor mixing ratios, reflectivity, and temperature tendency
  !       for the cloud analysis
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'ql',ges_ql,iret); ier_cloud=iret
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qi',ges_qi,iret); ier_cloud=ier_cloud+iret
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qr',ges_qr,iret); ier_cloud=ier_cloud+iret
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qs',ges_qs,iret); ier_cloud=ier_cloud+iret
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qg',ges_qg,iret); ier_cloud=ier_cloud+iret 
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'ref',ges_ref,istatus);ier_cloud=ier_cloud+iret
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'tten',dfi_tten,istatus);ier_cloud=ier_cloud+iret
       if(ier_cloud/=0) then
          write(6,*)'wrwrfnmma.F90 :: missng hydrometeor/tten/ref fields for cloud analysis nothing to do'
  	n_actual_clouds=0
       end if	
    end if 
  
    if(mype==mype_input) write(wrfanl,'("wrf_inout",i2.2)') nhr_assimilation
  
  !   update date info so start time is analysis time, and forecast time = 0
    call gsi_nemsio_update(wrfanl,'WRNEMSNMMA_BINARY:  problem with update of wrfanl',mype,mype_input)
  
  !   open output file for read-write so we can update fields.
    call gsi_nemsio_open(wrfanl,'rdwr','WRNEMSNMMA_BINARY:  problem with wrfanl',mype,mype_input,ierr)
  
    do kr=1,nsig_write
  
       k=nsig_write+1-kr
                                     !   u
  
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'u',ges_u,iret)
       if (iret==0)then
          call gsi_nemsio_read('ugrd','mid layer','V',kr,work_sub(:,:),mype,mype_input)
          do i=1,lon2
             do j=1,lat2
                work_sub(j,i)=ges_u(j,i,k)-work_sub(j,i)
             end do
          end do
          if(k <= near_sfc) then
             do i=1,lon2
                do j=1,lat2
                   delu(j,i,k)=work_sub(j,i)
                end do
             end do
          end if
          call gsi_nemsio_write('ugrd','mid layer','V',kr,work_sub(:,:),mype,mype_input,add_saved)
       endif
  
                                     !   v
  
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'v',ges_v,iret)
       if (iret==0) then
          call gsi_nemsio_read('vgrd','mid layer','V',kr,work_sub(:,:),mype,mype_input)
          do i=1,lon2
             do j=1,lat2
                work_sub(j,i)=ges_v(j,i,k)-work_sub(j,i)
             end do
          end do
          if(k <= near_sfc) then
             do i=1,lon2
                do j=1,lat2
                   delv(j,i,k)=work_sub(j,i)
                end do
             end do
          end if
          call gsi_nemsio_write('vgrd','mid layer','V',kr,work_sub(:,:),mype,mype_input,add_saved)
       endif
                                   !   w
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'w',ges_w,iret)
     if (iret==0) then
        call gsi_nemsio_read('w_tot','mid layer','H',kr,work_sub(:,:),mype,mype_input)
        do i=1,lon2
           do j=1,lat2
              work_sub(j,i)=ges_w(j,i,k)-work_sub(j,i)
           end do
        end do
        if(k <= near_sfc) then
           do i=1,lon2
              do j=1,lat2
                 delw(j,i,k)=work_sub(j,i)
              end do
           end do
        end if
        call gsi_nemsio_write('w_tot','mid layer','H',kr,work_sub(:,:),mype,mype_input,add_saved)
     endif
 
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'dwdt',ges_dw,iret)
     if (iret==0) then
        call gsi_nemsio_read('dwdt','mid layer','H',kr,work_sub(:,:),mype,mype_input)
        do i=1,lon2
           do j=1,lat2
              work_sub(j,i)=ges_dw(j,i,k)-work_sub(j,i)
           end do
        end do
        if(k <= near_sfc) then
           do i=1,lon2
              do j=1,lat2
                 delv(j,i,k)=work_sub(j,i)
              end do
           end do
        end if
        call gsi_nemsio_write('dwdt','mid layer','H',kr,work_sub(:,:),mype,mype_input,add_saved)
     endif
  
                                     !   q
  
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'q',ges_q,iret)
       if (iret==0) then
          call gsi_nemsio_read('spfh','mid layer','H',kr,work_sub(:,:),mype,mype_input)
          do i=1,lon2
             do j=1,lat2
                work_sub(j,i)=ges_q(j,i,k)-work_sub(j,i)
             end do
          end do
          if(k <= near_sfc) then
             do i=1,lon2
                do j=1,lat2
                   delq(j,i,k)=work_sub(j,i)
                end do
             end do
          end if
          where( ges_q < zero )
            ges_q = zero
          end where
          work_sub(:,:)=ges_q(:,:,k)
          call gsi_nemsio_write('spfh','mid layer','H',kr,work_sub(:,:),mype,mype_input,add_saved)
       endif
  
  !* use GSD cloud analysis for NMMB
       if(i_gsdcldanal_type==2) then
  !    write(6,*)'sliu in wrwrfnmma.F90:: enter dump dfi_tten'
       do i=1,lon2
          do j=1,lat2
             work_sub_t(j,i)=ges_tsen(j,i,k,it)
             work_sub_i(j,i)=ges_qi(j,i,k)
             work_sub_r(j,i)=ges_qr(j,i,k)
             work_sub_l(j,i)=ges_ql(j,i,k)
          end do
       end do

       add_saved=.false.
       call gsi_nemsio_write_fraction('f_rain','f_ice','mid layer',kr,       &
                   work_sub_t(:,:),work_sub_i(:,:),work_sub_r(:,:),work_sub_l(:,:),mype,mype_input)
  
       do i=1,lon2
          do j=1,lat2
             work_sub(j,i)=ges_qg(j,i,k)
          end do
       end do
       call gsi_nemsio_write('clwmr','mid layer','H',kr,work_sub(:,:),mype,mype_input,add_saved)
  
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'tten',dfi_tten,iret)
       do i=1,lon2
          do j=1,lat2
             work_sub(j,i)=dfi_tten(j,i,k)
          end do
       end do
       call gsi_nemsio_write('dfi_tten','mid layer','H',kr,work_sub(:,:),mype,mype_input,add_saved)
       add_saved=.true.
       end if
  !* use GSD cloud analysis for NMMB
  
                                     !   tsen
  
       call gsi_nemsio_read('tmp','mid layer','H',kr,work_sub(:,:),mype,mype_input)
       do i=1,lon2
          do j=1,lat2
             work_sub(j,i)=ges_tsen(j,i,k,it)-work_sub(j,i)
          end do
       end do
       if(k <= near_sfc) then
          do i=1,lon2
             do j=1,lat2
                delt(j,i,k)=work_sub(j,i)*(r100/ges_prsl(j,i,k,it))**rd_over_cp  ! convert to pot temp
                pott(j,i,k)=ges_tsen(j,i,k,it)*(r100/ges_prsl(j,i,k,it))**rd_over_cp
             end do
          end do
       end if
       call gsi_nemsio_write('tmp','mid layer','H',kr,work_sub(:,:),mype,mype_input,add_saved)
  
                                     !   ozone
  
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'oz',ges_oz,iret)
       if(iret==0.and.regional_ozone) then
          good_o3mr=.false.
          call gsi_nemsio_read('o3mr','mid layer','H',kr,work_sub(:,:),mype,mype_input,good_o3mr)
          if(good_o3mr) then
             if(use_gfs_ozone) then
  !                                  gfs ozone interpolated directly to analysis grid and nmmb guess
  !                                   not used, so set work_sub=zero
                work_sub=zero
             end if
             do i=1,lon2
                do j=1,lat2
                   work_sub(j,i)=ges_oz(j,i,k)-work_sub(j,i)
                end do
             end do
             call gsi_nemsio_write('o3mr','mid layer','H',kr,work_sub(:,:),mype,mype_input, &
                                   add_saved.and..not.use_gfs_ozone)
          else
             if(mype==0) write(6,*)' O3MR FIELD NOT YET AVAILABLE IN NMMB, OZONE DATA USED BUT NOT UPDATED'
          end if
       end if
  
                               ! cloud
       if (n_actual_clouds>0 .and. (i_gsdcldanal_type/=2) .and. i_radar_qli <= 0) then
          call gsi_nemsio_read('clwmr','mid layer','H',kr,work_sub(:,:),mype,mype_input)
          if (cold_start) then
             do i=1,lon2
                do j=1,lat2
                   if (ges_ql(j,i,k)<=qcmin) ges_ql(j,i,k)=qcmin
                   if (ges_qi(j,i,k)<=qcmin) ges_qi(j,i,k)=qcmin
                   work_clwmr(j,i)=ges_ql(j,i,k)+ges_qr(j,i,k)
                   work_sub(j,i)=work_clwmr(j,i)-max(work_sub(j,i),qcmin)
                end do
             end do
             call gsi_nemsio_write('clwmr','mid layer','H',kr,work_sub(:,:),mype,mype_input,add_saved)
          else
             do i=1,lon2
                do j=1,lat2
                   if (ges_ql(j,i,k)<=qcmin) ges_ql(j,i,k)=qcmin
                   if (ges_qi(j,i,k)<=qcmin) ges_qi(j,i,k)=qcmin
                   if (ges_qs(j,i,k)<=qcmin) ges_qs(j,i,k)=qcmin
                   if (ges_qg(j,i,k)<=qcmin) ges_qg(j,i,k)=qcmin
                   if (ges_qh(j,i,k)<=qcmin) ges_qh(j,i,k)=qcmin
                   if (ges_qr(j,i,k)<=qcmin) ges_qr(j,i,k)=qcmin
                   total_ice=ges_qi(j,i,k)+ges_qs(j,i,k)+ges_qg(j,i,k)+ges_qh(j,i,k)
                   work_clwmr(j,i)=total_ice+ges_ql(j,i,k)+ges_qr(j,i,k)
                   work_sub(j,i)=work_clwmr(j,i)-max(work_sub(j,i),qcmin)
  
                   if (work_clwmr(j,i)>zero) then
                      work_fice(j,i)=total_ice/work_clwmr(j,i)
                      if (work_fice(j,i)<one) then
                         work_frain(j,i)=ges_qr(j,i,k)/(work_clwmr(j,i)*(one-work_fice(j,i)))
                      else
                         work_frain(j,i)=zero
                      end if
                   else
                      work_fice(j,i)=zero
                      work_frain(j,i)=zero
                   end if
                end do
             end do
             call gsi_nemsio_write('clwmr','mid layer','H',kr,work_sub(:,:),mype,mype_input,add_saved)
             call gsi_nemsio_write('f_ice','mid layer','H',kr,work_fice(:,:),mype,mype_input,.false.)
             call gsi_nemsio_write('f_rain','mid layer','H',kr,work_frain(:,:),mype,mype_input,.false.)
             call gsi_nemsio_read('f_rimef','mid layer','H',kr,work_sub(:,:),mype,mype_input)
             call gsi_nemsio_write('f_rimef','mid layer','H',kr,work_sub(:,:),mype,mype_input,.false.)
          end if ! end of non-coldstart

          if (i_radar_qr>0 .and. i_radar_qli>0)then

             if( dbz_exist .and. if_model_dbz )then
               ! refl_10cm
               call gsi_bundlegetpointer (gsi_metguess_bundle(it),'dbz',ges_dbz,iret)
               if (iret==0) then
                 where( ges_dbz < zero )
                   ges_dbz = zero
                 end where
                 work_sub(:,:)=ges_dbz(:,:,k)
                 call gsi_nemsio_write('refl_10cm','mid layer','H',kr,work_sub(:,:),mype,mype_input,.false.)
               endif
             end if
    
             do i=1,lon2
               do j=1,lat2
                 work_sub_s(j,i)=ges_qli(j,i,k)
                 work_sub_r(j,i)=ges_qr(j,i,k)
                 work_sub_l(j,i)=ges_ql(j,i,k)
                 work_sub_i(j,i)=ges_qi(j,i,k)
               end do
             end do
    
             add_saved=.false.
             call gsi_nemsio_write_fractionnew('f_rain','f_ice','f_rimef','mid layer',kr,       &
                  work_sub_s(:,:),work_sub_i(:,:),work_sub_r(:,:),work_sub_l(:,:),mype,mype_input)
             ges_qg=ges_ql+ges_qr+ges_qli
    
             do i=1,lon2
               do j=1,lat2
                  work_sub(j,i)=ges_qg(j,i,k)
               end do
             end do
             call gsi_nemsio_write('clwmr','mid layer','H',kr,work_sub(:,:),mype,mype_input,add_saved)
               
          end if

       end if  ! end of nguess
  
    end do
  
                               ! pd
    call gsi_bundlegetpointer (gsi_metguess_bundle(it),'ps',ges_ps,iret)
    if (iret==0) then
       do i=1,lon2
          do j=1,lat2
             psfc_this=r10*ges_ps(j,i)   ! convert from mb to cb
             pd=psfc_this-pd_to_ps
             pd_new(j,i)=r100*pd
          end do
       end do
  
       call gsi_nemsio_read('dpres','hybrid sig lev','H',1,work_sub(:,:),mype,mype_input)
       do i=1,lon2
          do j=1,lat2
             work_sub(j,i)=pd_new(j,i)-work_sub(j,i)
          end do
       end do
       call gsi_nemsio_write('dpres','hybrid sig lev','H',1,work_sub(:,:),mype,mype_input,add_saved)
    endif
  
  
  !                    update pint by adding eta2(k)*pdinc
    if(update_pint) then
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'pd',ges_pd,iret)
       if (iret/=0) then ! doesn't need to die (but needs careful revision)
          call die('wrnemsnmma_binary',': missing pd field',iret)
       endif
       do kr=1,nsig_write+1
          k=nsig_write+2-kr
          call gsi_nemsio_read('pres','layer','H',kr,work_sub(:,:),mype,mype_input)
  
          do i=1,lon2
             do j=1,lat2
                work_sub(j,i)=eta2_ll(k)*(pd_new(j,i)-ges_pd(j,i))   ! pint analysis increment
             end do
          end do
          call gsi_nemsio_write('pres','layer','H',kr,work_sub(:,:),mype,mype_input,add_saved)
       end do
    end if
  
    if(update_regsfc) then
  !              land points first
       call gsi_nemsio_read('tg'   ,'sfc','H',1,work_sub(:,:),mype,mype_input)
       do i=1,lon2
          do j=1,lat2
             if(isli(j,i,it)/=0) then
  !               land points--
                work_sub(j,i)=dsfct(j,i,ntguessfc)
             else
  !               water points
                work_sub(j,i)=zero
             end if
          end do
       end do
       call gsi_nemsio_write('tg','sfc','H',1,work_sub(:,:),mype,mype_input,add_saved)
  !          now water points
       call gsi_nemsio_read('tsea' ,'sfc','H',1,work_sub(:,:),mype,mype_input)
       do i=1,lon2
          do j=1,lat2
             if(isli(j,i,it)/=0) then
  !               land points--
                work_sub(j,i)=zero
             else
  !               water points
                work_sub(j,i)=dsfct(j,i,ntguessfc)
             end if
          end do
       end do
       call gsi_nemsio_write('tsea','sfc','H',1,work_sub(:,:),mype,mype_input,add_saved)
    end if
  
  !   fact10 method follows:
  
    good_u10=.false.
    good_v10=.false.
    call gsi_nemsio_read ('u10' ,'10 m above gnd','H',1,u10this(:,:),mype,mype_input,good_u10)
    call gsi_nemsio_read ('v10' ,'10 m above gnd','H',1,v10this(:,:),mype,mype_input,good_v10)
    call gsi_bundlegetpointer (gsi_metguess_bundle(it),'u',ges_u,istatus);iret=istatus
    call gsi_bundlegetpointer (gsi_metguess_bundle(it),'v',ges_v,istatus);iret=iret+istatus
    if(iret==0.and.good_u10.and.good_v10) then
       if(use_fact10) then
  !          recompute fact10 (store as fact10_local)  (this code lifted from read_wrf_nmm_guess.F90)
          do i=1,lon2
             do j=1,lat2
                fact10_local(j,i)=one    !  later fix this by using correct w10/w(1)
                wmag=sqrt(ges_u(j,i,1)**2+ges_v(j,i,1)**2)
                if(wmag > zero)fact10_local(j,i)=sqrt(u10this(j,i)**2+v10this(j,i)**2)/wmag
                fact10_local(j,i)=min(max(fact10_local(j,i),half),0.95_r_kind)
                delu10(j,i)=fact10_local(j,i)*delu(j,i,1)
                delv10(j,i)=fact10_local(j,i)*delv(j,i,1)
             end do
          end do
  
       else
  
  !    vertical interpolation/extrapolation follows:
  
          ten=10._r_kind
          do i=1,lon2
             do j=1,lat2
                if(ten <  geop_hgtl(j,i,1,it)) then
                   delu10(j,i)=delu(j,i,1)
                   delv10(j,i)=delv(j,i,1)
                else
                   do k=1,near_sfc-1
                      kp=k+1
                      if(ten >= geop_hgtl(j,i,k,it).and.ten <  geop_hgtl(j,i,kp,it)) then
                         wgt1=(geop_hgtl(j,i,kp,it)-ten)/(geop_hgtl(j,i,kp,it)-geop_hgtl(j,i,k,it))
                         wgt2=one-wgt1
                         delu10(j,i)=wgt1*delu(j,i,k)+wgt2*delu(j,i,kp)
                         delv10(j,i)=wgt1*delv(j,i,k)+wgt2*delv(j,i,kp)
                         exit
                      end if
                   end do
                end if
             end do
          end do
  
       end if
  
  
  !         update 10m wind 
  !                     (read to work_sub, but only so u10 is saved internally in module gsi_nemsio_mod)
       call gsi_nemsio_read ('u10' ,'10 m above gnd','H',1,work_sub(:,:),mype,mype_input)
  !                previously computed 10m u increment added to guess u10 here:
       call gsi_nemsio_write('u10' ,'10 m above gnd','H',1,delu10(:,:),mype,mype_input,add_saved)
  !             repeat for 10m v component
       call gsi_nemsio_read ('v10' ,'10 m above gnd','H',1,work_sub(:,:),mype,mype_input)
       call gsi_nemsio_write('v10' ,'10 m above gnd','H',1,delv10(:,:),mype,mype_input,add_saved)
  
    end if
  
  !         update 2m potential temp and 2m specific humidity
  
  !   fact2 method follows:
  
    good_tshltr=.false.
    good_qshltr=.false.
    call gsi_nemsio_read ('tshltr' ,'sfc','H',1,t2this(:,:),mype,mype_input,good_tshltr)
    call gsi_nemsio_read ('qshltr' ,'sfc','H',1,q2this(:,:),mype,mype_input,good_qshltr)
    if(good_tshltr.and.good_qshltr) then
       if(use_fact2) then
  !       compute fact2t, fact2q
          call gsi_nemsio_read ('tshltr' ,'sfc','H',1,t2this(:,:),mype,mype_input)
          call gsi_nemsio_read ('qshltr' ,'sfc','H',1,q2this(:,:),mype,mype_input)
          do i=1,lon2
             do j=1,lat2
                fact2t_local(j,i)=max(half,min(t2this(j,i)/pott(j,i,1),two))
                fact2q_local(j,i)=max(half,min(q2this(j,i)/ges_q(j,i,1),two))
                delt2(j,i)=fact2t_local(j,i)*delt(j,i,1)
                delq2(j,i)=fact2q_local(j,i)*delq(j,i,1)
             end do
          end do
  
       else
  
  !    vertical interpolation/extrapolation follows:
  
          do i=1,lon2
             do j=1,lat2
                if(two <  geop_hgtl(j,i,1,it)) then
                   delt2(j,i)=delt(j,i,1)
                   delq2(j,i)=delq(j,i,1)
                else
                   do k=1,near_sfc-1
                      kp=k+1
                      if(two >= geop_hgtl(j,i,k,it).and.two <  geop_hgtl(j,i,kp,it)) then
                         wgt1=(geop_hgtl(j,i,kp,it)-two)/(geop_hgtl(j,i,kp,it)-geop_hgtl(j,i,k,it))
                         wgt2=one-wgt1
                         delt2(j,i)=wgt1*delt(j,i,k)+wgt2*delt(j,i,kp)
                         delq2(j,i)=wgt1*delq(j,i,k)+wgt2*delq(j,i,kp)
                         exit
                      end if
                   end do
                end if
             end do
          end do
  
       end if
  
  
  !         update 2m t and q
  !                     (read to work_sub, but only so tshltr is saved internally in module gsi_nemsio_mod)
       call gsi_nemsio_read ('tshltr' ,'sfc','H',1,work_sub(:,:),mype,mype_input)
  !                previously computed 2m t increment added to guess tshltr here:
       call gsi_nemsio_write('tshltr' ,'sfc','H',1,delt2(:,:),mype,mype_input,add_saved)
  !             repeat for 2m q
       call gsi_nemsio_read ('qshltr' ,'sfc','H',1,work_sub(:,:),mype,mype_input)
       call gsi_nemsio_write('qshltr' ,'sfc','H',1,delq2(:,:),mype,mype_input,add_saved)
  
    end if
  
    call gsi_nemsio_close(wrfanl,'WRNEMSNMMA_BINARY',mype,mype_input)
  
    if(use_gfs_stratosphere) call restore_nmmb_gfs
    
  end subroutine wrnemsnmma_binary
  
  subroutine wrwrfnmma_netcdf_wrf(this,mype)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    wrwrfnmma              write out wrf NMM restart file
  !   prgmmr: parrish          org: np22                date: 2004-06-23
  !
  ! abstract:  read wrf NMM guess restart interface file, add analysis
  !            increment, and write out wrf NMM analysis restart 
  !            interface file.
  !
  ! program history log:
  !   2004-06-23  parrish, document
  !   2004-08-03  treadon - add only to module use, add intent in/out
  !   2005-07-06  parrish - update and write out pint if update_pint=.true.
  !   2006-04-06  middlecoff - changed iog  from 11 to lendian_in
  !                            changed ioan from 51 to lendian_out
  !   2006-07-28  derber - include sensible temperature
  !   2006-07-31  kleist - change to use ges_ps instead of lnps
  !   2008-04-01  safford - rm unused uses
  !   2008-12-05  todling - adjustment for dsfct time dimension addition
  !   2010-04-01  treadon - move strip_single to gridmod
  !   2012-01-18  zhu     - add cloud hydrometeors
  !   2012-07-19  tong    - added the output of boundary variables
  !   2012-10-11  eliu    - add capability of using global-regional blended 
  !                         vertical coordinate for wrf_nmm_regional (HWRF)      
  !   2013-10-19  todling - metguess now holds background
  !   2013-10-24  todling - general interface to strip
  !
  !   input argument list:
  !     mype     - pe number
  !
  !   output argument list:
  !     no output arguments
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$
    use kinds, only: r_kind,r_single,i_kind
    use wrf_params_mod, only: update_pint
    use guess_grids, only: &
         ntguessfc,ntguessig,ifilesig,dsfct,ges_tsen
    use mpimod, only: mpi_comm_world,ierror,mpi_real4,mpi_sum
    use gridmod, only: iglobal,itotsub,pt_ll,update_regsfc,&
         half_grid,filled_grid,pdtop_ll,nlat_regional,nlon_regional,&
         nsig,lat1,lon1,ijn,displs_g,eta2_ll,strip,lat2,lon2
    use constants, only: zero_single,r10,r100,qcmin,zero,one
    use gsi_io, only: lendian_in, lendian_out
    use gsi_metguess_mod, only: gsi_metguess_get,gsi_metguess_bundle
    use gsi_bundlemod, only: gsi_bundlegetpointer
    use mpeu_util, only: die,getindex
    use control_vectors, only: cvars3d
    use gfs_stratosphere, only: use_gfs_stratosphere,nsig_save
    use gfs_stratosphere, only: eta1_save,aeta1_save,deta1_save
    use gfs_stratosphere, only: eta2_save,aeta2_save,deta2_save
    use gfs_stratosphere, only: revert_to_nmmb,restore_nmmb_gfs
  
    implicit none
  
  ! Declare passed variables
    class(wrwrfnmma_class), intent(inout) :: this 
    integer(i_kind),intent(in   ) :: mype
  
  ! Declare local constants
    real(r_kind),parameter:: r225=225.0_r_kind
  
  ! Declare local variables
    integer(i_kind) im,jm,lm
    integer(i_kind) nsig_write  
    real(r_single),allocatable::temp1(:),tempa(:),tempb(:)
    real(r_single),allocatable::all_loc(:,:,:)
    real(r_single),allocatable::strp(:)
    character(6) filename
    integer(i_kind) i,j,k,kpint,kt,kq,ku,kv,it,i_pd,i_pint,i_t,i_q,i_u,i_v
    integer(i_kind) i_sst,i_tsk,i_cwm,i_f_ice,i_f_rain,kcwm,kf_ice,kf_rain
    integer(i_kind) igtypeh,igtypev,num_nmm_fields,num_all_fields,num_all_pad
    integer(i_kind) regional_time0(6),nlon_regional0,nlat_regional0,nsig0
    real(r_kind) pd,psfc_this
    real(r_single) dlmd0,dphd0,pt0,pdtop0
    real(r_single) deta10(nsig),aeta10(nsig),eta10(nsig+1),deta20(nsig),&
         aeta20(nsig),eta20(nsig+1)
    real(r_single) glon0(nlon_regional,nlat_regional),glat0(nlon_regional,nlat_regional)
    real(r_single) dx0_nmm(nlon_regional,nlat_regional),dy0_nmm(nlon_regional,nlat_regional)
  
  ! variables for cloud info
    integer(i_kind) iret,ier,n_actual_clouds
    integer(i_kind) icw4crtm,iqtotal,istatus
    real(r_kind) total_ice
    real(r_kind),dimension(lat2,lon2):: work_clwmr,work_fice,work_frain
    real(r_kind),pointer,dimension(:,:  ):: ges_pd  =>NULL()
    real(r_kind),pointer,dimension(:,:  ):: ges_ps  =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_u   =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_v   =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_pint=>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_q   =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_ql  =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_qi  =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_qr  =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_qs  =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_qg  =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_qh  =>NULL()
  
    real(r_single),allocatable:: pdbg(:),tbg(:,:),qbg(:,:),cwmbg(:,:),ubg(:,:),vbg(:,:)
    real(r_single),allocatable:: pdba(:),tba(:,:),qba(:,:),cwmba(:,:),uba(:,:),vba(:,:)
    integer(i_kind) bdim
  
  ! if use_gfs_stratosphere is true, then convert ges fields from nmm-gfs
  ! extended vertical coordinate to nmmb vertical coordinate.
    if(use_gfs_stratosphere) then
       call revert_to_nmmb
       nsig_write=nsig_save
    else
       nsig_write=nsig
    endif
  
    im=nlon_regional
    jm=nlat_regional
    lm=nsig_write      
  
    if (mype==0) write(6,*)'wrwrfnmma_netcdf: nsig_write =   ', nsig_write
    if (mype==0) write(6,*)'wrwrfnmma_netcdf: nsig =         ', nsig
    if (mype==0) write(6,*)'wrwrfnmma_netcdf: lm =           ', lm
    if (mype==0) write(6,*)'wrwrfnmma_netcdf: jm =           ', jm
    if (mype==0) write(6,*)'wrwrfnmma_netcdf: im =           ', im
    if (mype==0) write(6,*)'wrwrfnmma_netcdf: nlat_regional =', nlat_regional
    if (mype==0) write(6,*)'wrwrfnmma_netcdf: nlon_regional =', nlon_regional
  
  !  allocate boundary file arrays
    bdim=2*im+jm-3
    allocate(pdbg(bdim),tbg(bdim,lm),qbg(bdim,lm),cwmbg(bdim,lm),ubg(bdim,lm),vbg(bdim,lm))
    allocate(pdba(bdim),tba(bdim,lm),qba(bdim,lm),cwmba(bdim,lm),uba(bdim,lm),vba(bdim,lm))
    pdbg=zero  ; tbg=zero  ; qbg=zero  ; cwmbg=zero  ; ubg=zero  ; vbg=zero
    pdba=zero  ; tba=zero  ; qba=zero  ; cwmba=zero  ; uba=zero  ; vba=zero
  
    it=ntguessig
  
  ! inquiry cloud guess
    call gsi_metguess_get('clouds::3d',n_actual_clouds,iret)
    if (n_actual_clouds>0) then
  !    Determine whether or not cloud-condensate is the control variable
       icw4crtm=getindex(cvars3d,'cw')
  
  !    Determine whether total moisture (water vapor+total cloud condensate) is the control variable
       iqtotal=getindex(cvars3d,'qt')
  
  !    Get pointer to cloud water mixing ratio
       ier=0
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'ql',ges_ql,iret); ier=iret
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qi',ges_qi,iret); ier=ier+iret
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qr',ges_qr,iret); ier=ier+iret
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qs',ges_qs,iret); ier=ier+iret
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qg',ges_qg,iret); ier=ier+iret
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qh',ges_qh,iret); ier=ier+iret
  
       if ((icw4crtm<=0 .and. iqtotal<=0) .or. ier/=0) n_actual_clouds=0
    end if
  
    num_nmm_fields=3+4*lm
    if(update_pint) num_nmm_fields=num_nmm_fields+lm+1  ! contribution from PINT
    if (n_actual_clouds>0) num_nmm_fields=num_nmm_fields+4*lm
    num_all_fields=num_nmm_fields
    num_all_pad=num_all_fields
    allocate(all_loc(lat1+2,lon1+2,num_all_pad))
    allocate(strp(lat1*lon1))
  
    i_pd=1
    if(update_pint) then
       i_pint=2
       i_t=i_pint+lm+1
    else
       i_t=2
    end if
    i_q=i_t+lm
    i_u=i_q+lm
    i_v=i_u+lm
    i_sst=i_v+lm
    i_tsk=i_sst+1
    if (n_actual_clouds>0) then
       i_cwm=i_tsk+1
       i_f_ice=i_cwm+lm
       i_f_rain=i_f_ice+lm
    end if
    igtypeh=1
    igtypev=2
    
    allocate(temp1(im*jm))
  
  ! if(mype == 0) write(6,*)' at 2 in wrwrfnmma'
  
  
    if(mype == 0) then
       write(filename,'("sigf",i2.2)')ifilesig(ntguessig)
       open (lendian_in,file=filename,form='unformatted')
       open (lendian_out,file='siganl',form='unformatted')
       rewind lendian_in ; rewind lendian_out
    end if
  
  ! Convert analysis variables to NMM variables
    it=ntguessig
  
    ier=0
    call gsi_bundlegetpointer (gsi_metguess_bundle(it),'ps',ges_ps,iret); ier=iret
    call gsi_bundlegetpointer (gsi_metguess_bundle(it),'u' ,ges_u ,iret); ier=iret
    call gsi_bundlegetpointer (gsi_metguess_bundle(it),'v' ,ges_v ,iret); ier=iret
    call gsi_bundlegetpointer (gsi_metguess_bundle(it),'q' ,ges_q ,iret); ier=iret
    if (ier/=0) then ! doesn't need to die (but needs careful revision)
       call die('wrwrfnmma_netcdf',': missing guess fields',ier)
    endif
  
  ! Create all_loc from ges_*
  ! if(mype == 0) write(6,*)' at 3 in wrwrfnmma'
    all_loc=zero_single
    kt=i_t-1
    kq=i_q-1
    ku=i_u-1
    kv=i_v-1
    do k=1,nsig_write        
       kt=kt+1
       kq=kq+1
       ku=ku+1
       kv=kv+1
       do i=1,lon1+2
          do j=1,lat1+2
             all_loc(j,i,ku)=ges_u(j,i,k)
  !          if(k == 1.and.abs(all_loc(j,i,ku)) > 1.e15_r_single) &
  !               write(6,*)' at 3.01 in wrwrfnmma, j,i,ku,all_loc(j,i,ku)=',j,i,ku,all_loc(j,i,ku)
             all_loc(j,i,kv)=ges_v(j,i,k)
             all_loc(j,i,kq)=ges_q(j,i,k)
             all_loc(j,i,kt)=ges_tsen(j,i,k,it)   ! sensible temperature
          end do
       end do
    end do
    do i=1,lon1+2
       do j=1,lat1+2
          psfc_this=r10*ges_ps(j,i)   ! convert from mb to cb
          pd=psfc_this-pdtop_ll-pt_ll
          all_loc(j,i,i_pd)=r100*pd
       end do
    end do
    if (mype==0) &
    write(6,*)'all_loc for pd   = ',k,maxval(all_loc(:,:,i_pd)),minval(all_loc(:,:,i_pd))                                                                                                               
  !                    update pint by adding eta2(k)*pdinc
    if(update_pint) then
       ier=0
       call gsi_bundlegetpointer(gsi_metguess_bundle(it),'pint', ges_pint, istatus)
       ier=ier+istatus
       call gsi_bundlegetpointer(gsi_metguess_bundle(it),'pd'  , ges_pd  , istatus)
       ier=ier+istatus
       if (ier/=0) then ! doesn't need to die (but needs careful revision)
          call die('wrwrfnmma_netcdf',': missing pint/pd fields',ier)
       endif
       kpint=i_pint-1
       do k=1,nsig_write+1         
          kpint=kpint+1
          do i=1,lon1+2
             do j=1,lat1+2
                all_loc(j,i,kpint)=ges_pint(j,i,k) &
                            +eta2_ll(k)*(all_loc(j,i,i_pd)-ges_pd(j,i))   ! pint
             end do
          end do
          if (mype==0) &
          write(6,*)'all_loc for pint = ',k,maxval(all_loc(:,:,kpint)),minval(all_loc(:,:,kpint))                                                                                                               
       end do
    end if
  
  ! cloud info: currently no new f_rimef info is written out
    if (n_actual_clouds>0) then ! cloud
       kcwm=i_cwm-1
       kf_ice=i_f_ice-1
       kf_rain=i_f_rain-1
       do k=1,nsig_write       
          do i=1,lon2
             do j=1,lat2
                if (ges_ql(j,i,k)<=qcmin) ges_ql(j,i,k)=zero
                if (ges_qi(j,i,k)<=qcmin) ges_qi(j,i,k)=zero
                if (ges_qs(j,i,k)<=qcmin) ges_qs(j,i,k)=zero
                if (ges_qg(j,i,k)<=qcmin) ges_qg(j,i,k)=zero
                if (ges_qh(j,i,k)<=qcmin) ges_qh(j,i,k)=zero
                if (ges_qr(j,i,k)<=qcmin) ges_qr(j,i,k)=zero
                total_ice=ges_qi(j,i,k)+ges_qs(j,i,k)+ges_qg(j,i,k)+ges_qh(j,i,k)
                work_clwmr(j,i)=total_ice+ges_ql(j,i,k)+ges_qr(j,i,k)
                if (work_clwmr(j,i)>zero) then
                   work_fice(j,i)=total_ice/work_clwmr(j,i)
                   if (work_fice(j,i)<one) then
                      work_frain(j,i)=ges_qr(j,i,k)/(work_clwmr(j,i)*(one-work_fice(j,i)))
                   else
                      work_frain(j,i)=zero
                   end if
                else
                   work_fice(j,i)=zero
                   work_frain(j,i)=zero
                end if
             end do
          end do
  
          kcwm=kcwm+1
          kf_ice=kf_ice+1
          kf_rain=kf_rain+1
          do i=1,lon1+2
             do j=1,lat1+2
                all_loc(j,i,kcwm)=work_clwmr(j,i)
                all_loc(j,i,kf_ice)=work_fice(j,i)
                all_loc(j,i,kf_rain)=work_frain(j,i)
             end do
          end do
       end do
    end if ! end of n_actual_clouds>0
  
    if(mype == 0) then
       read(lendian_in) regional_time0,nlon_regional0,nlat_regional0,nsig0,dlmd0,dphd0,pt0,pdtop0
       read(lendian_in) deta10
       read(lendian_in) aeta10
       read(lendian_in) eta10
       read(lendian_in) deta20
       read(lendian_in) aeta20
       read(lendian_in) eta20
       read(lendian_in) glat0,dx0_nmm
       read(lendian_in) glon0,dy0_nmm
       if (use_gfs_stratosphere) then
          write(lendian_out) regional_time0,nlon_regional0,nlat_regional0,nsig_save,dlmd0,dphd0,pt0,pdtop0
          write(lendian_out) deta1_save       
          write(lendian_out) aeta1_save      
          write(lendian_out) eta1_save        
          write(lendian_out) deta2_save      
          write(lendian_out) aeta2_save       
          write(lendian_out) eta2_save       
       else
          write(lendian_out) regional_time0,nlon_regional0,nlat_regional0,nsig0,dlmd0,dphd0,pt0,pdtop0
          write(lendian_out) deta10       
          write(lendian_out) aeta10      
          write(lendian_out) eta10        
          write(lendian_out) deta20      
          write(lendian_out) aeta20       
          write(lendian_out) eta20       
       endif 
       write(lendian_out) glat0,dx0_nmm
       write(lendian_out) glon0,dy0_nmm
    end if
    
  ! Update pd
  ! if(mype == 0) write(6,*)' at 6 in wrwrfnmma'
    if(mype == 0) write(6,*)' at wrwrfnmma_netcdf: update PD' 
  
    allocate(tempa(itotsub),tempb(itotsub))
    tempa=0.0_r_single
    tempb=0.0_r_single
    if(mype == 0) read(lendian_in)temp1
  ! if(mype == 0) write(6,*)' at 6.1 in wrwrfnmma,max,min(temp1)=',maxval(temp1),minval(temp1)
    if(mype == 0) write(6,*)' max,min(temp1) PD in       =',maxval(temp1),minval(temp1)       
    call strip(all_loc(:,:,i_pd),strp)
    call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
         tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
    if(mype == 0) then
       call this%get_bndy_file(temp1,pdbg,tbg,qbg,cwmbg,ubg,vbg,i_pd,i_pd,i_t,i_q,i_cwm,i_u,i_v, &
                          n_actual_clouds,im,jm,lm,bdim,igtypeh)
  !    if(mype == 0) write(6,*)' at 6.2 in wrwrfnmma,max,min(tempa)=',maxval(tempa),minval(tempa)
       if(filled_grid) call fill_nmm_grid2(temp1,im,jm,tempb,igtypeh,2)
       if(half_grid)   call half_nmm_grid2(temp1,im,jm,tempb,igtypeh,2)
  !     if(mype == 0) write(6,*)' at 6.3 in wrwrfnmma,max,min(tempb)=',maxval(tempb),minval(tempb)
       do i=1,iglobal
          tempa(i)=tempa(i)-tempb(i)
       end do
  !    if(mype == 0) write(6,*)' at 6.4 in wrwrfnmma,max,min(tempa)=',maxval(tempa),minval(tempa)
       if(filled_grid) call unfill_nmm_grid2(tempa,im,jm,temp1,igtypeh,2)
  !    if(mype == 0) write(6,*)' at 6.5 in wrwrfnmma'
       if(half_grid)   call unhalf_nmm_grid2(tempa,im,jm,temp1,igtypeh,2)
  !    if(mype == 0) write(6,*)' at 6.6 in wrwrfnmma,max,min(temp1)=',maxval(temp1),minval(temp1)
       write(lendian_out)temp1
       if(mype == 0) write(6,*)' max,min(temp1) PD out      =',maxval(temp1),minval(temp1)     
       call this%get_bndy_file(temp1,pdba,tba,qba,cwmba,uba,vba,i_pd,i_pd,i_t,i_q,i_cwm,i_u,i_v, &
                          n_actual_clouds,im,jm,lm,bdim,igtypeh)
    end if
  
  !  FIS read/write
    if(mype == 0) then
       read(lendian_in)temp1
       write(lendian_out)temp1
    end if
  
    if(mype == 0) write(6,*)' at wrwrfnmma_netcdf: update PINT' 
  ! Update pint
    if(update_pint) then
       kpint=i_pint-1
       do k=1,nsig_write+1    
          kpint=kpint+1
          if(mype == 0) read(lendian_in)temp1
          if(mype == 0) write(6,*)' k,max,min(temp1) PINT in   =',k,maxval(temp1),minval(temp1)                                           
          call strip(all_loc(:,:,kpint),strp)
          call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
               tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
          if(mype == 0) then
             if(filled_grid) call fill_nmm_grid2(temp1,im,jm,tempb,igtypeh,2)
             if(half_grid)   call half_nmm_grid2(temp1,im,jm,tempb,igtypeh,2)
             do i=1,iglobal
                tempa(i)=tempa(i)-tempb(i)
             end do
             if(filled_grid) call unfill_nmm_grid2(tempa,im,jm,temp1,igtypeh,2)
             if(half_grid)   call unhalf_nmm_grid2(tempa,im,jm,temp1,igtypeh,2)
             write(lendian_out)temp1
             write(6,*)' k,max,min(temp1) PINT out  =',k,maxval(temp1),minval(temp1)                                 
          end if
       end do
    endif
  
    if(mype == 0) write(6,*)' at wrwrfnmma_netcdf: update PINT' 
  ! Update t
    kt=i_t-1
    do k=1,nsig_write    
       kt=kt+1
       if(mype == 0) read(lendian_in)temp1
       if(mype == 0) write(6,*)' k,max,min(temp1) T in      =',k,maxval(temp1),minval(temp1)                                             
       call strip(all_loc(:,:,kt),strp)
       call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
            tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
       if(mype == 0) then
          call this%get_bndy_file(temp1,pdbg,tbg,qbg,cwmbg,ubg,vbg,kt,i_pd,i_t,i_q,i_cwm,i_u,i_v, &
                             n_actual_clouds,im,jm,lm,bdim,igtypeh)
          if(filled_grid) call fill_nmm_grid2(temp1,im,jm,tempb,igtypeh,2)
          if(half_grid)   call half_nmm_grid2(temp1,im,jm,tempb,igtypeh,2)
          do i=1,iglobal
             tempa(i)=tempa(i)-tempb(i)
          end do
          if(filled_grid) call unfill_nmm_grid2(tempa,im,jm,temp1,igtypeh,2)
          if(half_grid)   call unhalf_nmm_grid2(tempa,im,jm,temp1,igtypeh,2)
          write(lendian_out)temp1
          write(6,*)' k,max,min(temp1) T out     =',k,maxval(temp1),minval(temp1)                                  
          call this%get_bndy_file(temp1,pdba,tba,qba,cwmba,uba,vba,kt,i_pd,i_t,i_q,i_cwm,i_u,i_v, &
                             n_actual_clouds,im,jm,lm,bdim,igtypeh)
       end if
    end do
  ! if(mype == 0) write(6,*)' at 7 in wrwrfnmma'
  
  
    if(mype == 0) write(6,*)' at wrwrfnmma_netcdf: update Q'  
  ! Update q
    kq=i_q-1
    do k=1,nsig_write    
       kq=kq+1
       if(mype == 0) read(lendian_in)temp1
       if(mype == 0) write(6,*)' k,max,min(temp1) Q in    =',k,maxval(temp1),minval(temp1)                                             
       call strip(all_loc(:,:,kq),strp)
       call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
            tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
       if(mype == 0) then
          call this%get_bndy_file(temp1,pdbg,tbg,qbg,cwmbg,ubg,vbg,kq,i_pd,i_t,i_q,i_cwm,i_u,i_v, &
                             n_actual_clouds,im,jm,lm,bdim,igtypeh)
          if(filled_grid) call fill_nmm_grid2(temp1,im,jm,tempb,igtypeh,2)
          if(half_grid)   call half_nmm_grid2(temp1,im,jm,tempb,igtypeh,2)
          do i=1,iglobal
             tempa(i)=tempa(i)-tempb(i)
          end do
          if(filled_grid) call unfill_nmm_grid2(tempa,im,jm,temp1,igtypeh,2)
          if(half_grid)   call unhalf_nmm_grid2(tempa,im,jm,temp1,igtypeh,2)
          write(lendian_out)temp1
          write(6,*)' k,max,min(temp1) Q out   =',k,maxval(temp1),minval(temp1)        
          call this%get_bndy_file(temp1,pdba,tba,qba,cwmba,uba,vba,kq,i_pd,i_t,i_q,i_cwm,i_u,i_v, &
                             n_actual_clouds,im,jm,lm,bdim,igtypeh)
       end if
    end do
  
    if(mype == 0) write(6,*)' at wrwrfnmma_netcdf: update U'  
  ! Update u
    ku=i_u-1
    do k=1,nsig_write       
       ku=ku+1
       if(mype == 0) read(lendian_in)temp1
       if(mype == 0) write(6,*)' k,max,min(temp1) U in    =',k,maxval(temp1),minval(temp1)                                             
       call strip(all_loc(:,:,ku),strp)
       call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
            tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
  !    if(mype == 0) write(6,*)' at 7.2 in wrwrfnmma,k,max,min(tempa)=',k,maxval(tempa),minval(tempa)
       if(mype == 0) then
          call this%get_bndy_file(temp1,pdbg,tbg,qbg,cwmbg,ubg,vbg,ku,i_pd,i_t,i_q,i_cwm,i_u,i_v, &
                             n_actual_clouds,im,jm,lm,bdim,igtypev)
          if(filled_grid) call fill_nmm_grid2(temp1,im,jm,tempb,igtypev,2)
          if(half_grid)   call half_nmm_grid2(temp1,im,jm,tempb,igtypev,2)
  !       if(mype == 0) write(6,*)' at 7.21 in wrwrfnmma,k,max,min(temp1)=',&
  !            k,maxval(temp1),minval(temp1)
  !       if(mype == 0) write(6,*)' at 7.22 in wrwrfnmma,k,max,min(tempb)=',&
  !            k,maxval(tempb),minval(tempb)
          do i=1,iglobal
             tempa(i)=tempa(i)-tempb(i)
          end do
  !       if(mype == 0) write(6,*)' at 7.3 in wrwrfnmma,k,max,min(tempa)=',k,maxval(tempa),minval(tempa)
          if(filled_grid) call unfill_nmm_grid2(tempa,im,jm,temp1,igtypev,2)
          if(half_grid)   call unhalf_nmm_grid2(tempa,im,jm,temp1,igtypev,2)
  !       if(mype == 0) write(6,*)' at 7.4 in wrwrfnmma,k,max,min(temp1)=',k,maxval(temp1),minval(temp1)
          write(lendian_out)temp1
          write(6,*)' k,max,min(temp1) U out   =',k,maxval(temp1),minval(temp1)                                  
          call this%get_bndy_file(temp1,pdba,tba,qba,cwmba,uba,vba,ku,i_pd,i_t,i_q,i_cwm,i_u,i_v, &
                             n_actual_clouds,im,jm,lm,bdim,igtypev)
       end if
    end do
  ! if(mype == 0) write(6,*)' at 8 in wrwrfnmma'
  
    if(mype == 0) write(6,*)' at wrwrfnmma_netcdf: update V'  
  ! Update v
    kv=i_v-1
    do k=1,nsig_write    
       kv=kv+1
       if(mype == 0) read(lendian_in)temp1
       if(mype == 0) write(6,*)' k,max,min(temp1) V in    =',k,maxval(temp1),minval(temp1)                                             
       call strip(all_loc(:,:,kv),strp)
       call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
            tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
       if(mype == 0) then
          call this%get_bndy_file(temp1,pdbg,tbg,qbg,cwmbg,ubg,vbg,kv,i_pd,i_t,i_q,i_cwm,i_u,i_v, &
                             n_actual_clouds,im,jm,lm,bdim,igtypev)
          if(filled_grid) call fill_nmm_grid2(temp1,im,jm,tempb,igtypev,2)
          if(half_grid)   call half_nmm_grid2(temp1,im,jm,tempb,igtypev,2)
          do i=1,iglobal
             tempa(i)=tempa(i)-tempb(i)
          end do
          if(filled_grid) call unfill_nmm_grid2(tempa,im,jm,temp1,igtypev,2)
          if(half_grid)   call unhalf_nmm_grid2(tempa,im,jm,temp1,igtypev,2)
          write(lendian_out)temp1
          write(6,*)' k,max,min(temp1) V out   =',k,maxval(temp1),minval(temp1)                                  
          call this%get_bndy_file(temp1,pdba,tba,qba,cwmba,uba,vba,kv,i_pd,i_t,i_q,i_cwm,i_u,i_v, &
                             n_actual_clouds,im,jm,lm,bdim,igtypev)
       end if
    end do
  
    if(mype == 0) write(6,*)' at wrwrfnmma_netcdf: update surface '                
    if(mype == 0) write(6,*)' at wrwrfnmma_netcdf: update_regsfc= ',update_regsfc
  ! Load updated skin temperature array if writing out to analysis file
    if (update_regsfc) then
       do i=1,lon1+2
          do j=1,lat1+2
             all_loc(j,i,i_sst)=dsfct(j,i,ntguessfc)
             all_loc(j,i,i_tsk)=dsfct(j,i,ntguessfc)
          end do
       end do
    end if
  
    if(mype == 0) then
  ! SM
       read(lendian_in)temp1
       write(lendian_out)temp1
  ! SICE
       read(lendian_in)temp1
       write(lendian_out)temp1
    end if
  
  ! SST
    if(update_regsfc) then
       if(mype == 0) write(6,*)' at wrwrfnmma_netcdf: update SST '    
       if(mype == 0) read(lendian_in)temp1
  !    if (mype==0)write(6,*)' at 9.1 in wrwrfnmma,max,min(temp1)=',maxval(temp1),minval(temp1)
       call strip(all_loc(:,:,i_sst),strp)
       call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
            tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
       if(mype == 0) then
  !       if(mype == 0) write(6,*)' at 9.2 in wrwrfnmma,max,min(tempa)=',maxval(tempa),minval(tempa)
          if(filled_grid) call fill_nmm_grid2(temp1,im,jm,tempb,igtypeh,2)
          if(half_grid)   call half_nmm_grid2(temp1,im,jm,tempb,igtypeh,2)
  !       if(mype == 0) write(6,*)' at 9.3 in wrwrfnmma,max,min(tempb)=',maxval(tempb),minval(tempb)
          do i=1,iglobal
             if(tempb(i) < r225) then
                tempa(i)=zero_single
             else
                tempa(i)=tempa(i)-tempb(i)
             end if
          end do
  !       if(mype == 0) write(6,*)' at 9.4 in wrwrfnmma,max,min(tempa)=',maxval(tempa),minval(tempa)
          if(filled_grid) call unfill_nmm_grid2(tempa,im,jm,temp1,igtypeh,2)
  !       if(mype == 0) write(6,*)' at 9.5 in wrwrfnmma'
          if(half_grid)   call unhalf_nmm_grid2(tempa,im,jm,temp1,igtypeh,2)
  !       if(mype == 0) write(6,*)' at 9.6 in wrwrfnmma,max,min(temp1)=',maxval(temp1),minval(temp1)
          write(lendian_out)temp1
       end if     !endif mype==0
    else
       if(mype==0) then
          write(6,*)' at wrwrfnmma_netcdf: read/write SST '    
          read(lendian_in)temp1
          write(lendian_out)temp1
       end if
    end if   !end if check updatesfc
    
  ! REST OF FIELDS
    if (mype == 0) then
       write(6,*)' at wrwrfnmma_netcdf: read/write various surface fields '    
       do k=4,11
          read(lendian_in)temp1
          write(lendian_out)temp1
       end do
    end if
    
  ! Update SKIN TEMP
    if(update_regsfc) then
       if(mype == 0) write(6,*)' at wrwrfnmma_netcdf: update TSK '   
       if(mype == 0) read(lendian_in)temp1
  !    if (mype==0)write(6,*)' at 10.0 in wrwrfnmma,max,min(temp1)=',maxval(temp1),minval(temp1)
       call strip(all_loc(:,:,i_tsk),strp)
       call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
            tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
       if (mype==0)write(6,*)' at 10.1'
       if(mype == 0) then
          if(filled_grid) call fill_nmm_grid2(temp1,im,jm,tempb,igtypeh,2)
          if(half_grid)   call half_nmm_grid2(temp1,im,jm,tempb,igtypeh,2)
          do i=1,iglobal
             if(tempb(i) < r225) then
                tempa(i)=zero_single
             else 
                tempa(i)=tempa(i)-tempb(i)
             end if
          end do
          if(filled_grid) call unfill_nmm_grid2(tempa,im,jm,temp1,igtypeh,2)
          if(half_grid)   call unhalf_nmm_grid2(tempa,im,jm,temp1,igtypeh,2)
          write(lendian_out)temp1
       end if
    else
       if (mype == 0) then
          write(6,*)' at wrwrfnmma_netcdf: read/write TSK '   
          read(lendian_in)temp1
          write(lendian_out)temp1
       end if
    end if
  
  ! update cloud hydrometeors
    if (n_actual_clouds>0) then
       if(mype == 0) write(6,*)' at wrwrfnmma_netcdf: update clouds '   
  !    Update cwm
       kcwm=i_cwm-1
       do k=1,nsig_write   
          kcwm=kcwm+1
          if(mype == 0) read(lendian_in)temp1 
          if(mype == 0) write(6,*)' k,max,min(temp1) CWM in   =',k,maxval(temp1),minval(temp1) 
          call strip(all_loc(:,:,kcwm),strp)
          call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
               tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
          if(mype == 0) then
             call this%get_bndy_file(temp1,pdbg,tbg,qbg,cwmbg,ubg,vbg,kcwm,i_pd,i_t,i_q,i_cwm,i_u,i_v, &
                                n_actual_clouds,im,jm,lm,bdim,igtypeh)
             if(filled_grid) call fill_nmm_grid2(temp1,im,jm,tempb,igtypeh,2)
             if(half_grid)   call half_nmm_grid2(temp1,im,jm,tempb,igtypeh,2)
             do i=1,iglobal
               tempa(i)=tempa(i)-tempb(i)
             end do
             if(filled_grid) call unfill_nmm_grid2(tempa,im,jm,temp1,igtypeh,2)
             if(half_grid)   call unhalf_nmm_grid2(tempa,im,jm,temp1,igtypeh,2)
             write(lendian_out)temp1
             write(6,*)' k,max,min(temp1) CWM out  =',k,maxval(temp1),minval(temp1)
             call this%get_bndy_file(temp1,pdba,tba,qba,cwmba,uba,vba,kcwm,i_pd,i_t,i_q,i_cwm,i_u,i_v, &
                                n_actual_clouds,im,jm,lm,bdim,igtypeh)
          end if
       end do
  
  !    Update f_ice
       kf_ice=i_f_ice-1
       do k=1,nsig_write
          kf_ice=kf_ice+1
          if(mype == 0) read(lendian_in)temp1 
          if(mype == 0) write(6,*)' k,max,min(temp1) F_ICE in   =',k,maxval(temp1),minval(temp1) 
          call strip(all_loc(:,:,kf_ice),strp)
          call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
               tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
          if(mype == 0) then
             if(filled_grid) call fill_nmm_grid2(temp1,im,jm,tempb,igtypeh,2)
             if(half_grid)   call half_nmm_grid2(temp1,im,jm,tempb,igtypeh,2)
             do i=1,iglobal
               tempa(i)=tempa(i)-tempb(i)
             end do
             if(filled_grid) call unfill_nmm_grid2(tempa,im,jm,temp1,igtypeh,2)
             if(half_grid)   call unhalf_nmm_grid2(tempa,im,jm,temp1,igtypeh,2)
             write(lendian_out)temp1
             write(6,*)' k,max,min(temp1) F_ICE out  =',k,maxval(temp1),minval(temp1)
          end if
       end do
  
  !    Update f_rain
       kf_rain=i_f_rain-1
       do k=1,nsig_write
          kf_rain=kf_rain+1
          if(mype == 0) temp1=zero  ! no read-in of guess fields
          if(mype == 0) read(lendian_in)temp1
          if(mype == 0) write(6,*) ' k,max,min(temp1) F_RAIN in    =',k,maxval(temp1),minval(temp1)
          call strip(all_loc(:,:,kf_rain),strp)
          call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
               tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
          if(mype == 0) then
             if(filled_grid) call fill_nmm_grid2(temp1,im,jm,tempb,igtypeh,2)
             if(half_grid)   call half_nmm_grid2(temp1,im,jm,tempb,igtypeh,2)
             do i=1,iglobal
               tempa(i)=tempa(i)-tempb(i)
             end do
             if(filled_grid) call unfill_nmm_grid2(tempa,im,jm,temp1,igtypeh,2)
             if(half_grid)   call unhalf_nmm_grid2(tempa,im,jm,temp1,igtypeh,2)
             write(lendian_out)temp1
             write(6,*) ' k,max,min(temp1) F_RAIN out   =',k,maxval(temp1),minval(temp1)
          end if
       end do
  
  !    write out f_rimef
       if (mype == 0) then
          do k=1,nsig_write
             read(lendian_in)temp1
             write(lendian_out)temp1
          end do
       end if
    end if  ! end of n_actual_clouds>0
  
    if (mype==0) then
       close(lendian_in)
       close(lendian_out)
    endif
  
    if(use_gfs_stratosphere) then 
        if(mype==0) write(6,*)' at wrwrfnmma_netcdf: restore ges fields back to extended vertical grid'                             
        call restore_nmmb_gfs  
    endif
  
  ! write out boundary variables
    if(mype==0) then
       open(lendian_out,file='wrf_nmm_bnd',form='unformatted')
       write(lendian_out)'WRF-NMM-NETCDF'
       write(lendian_out) regional_time0,im,jm,lm,bdim   
       write(lendian_out) pdbg,tbg,qbg,cwmbg,ubg,vbg
       write(lendian_out) pdba,tba,qba,cwmba,uba,vba 
       close(lendian_out)
       write(6,*)' min,max pdbg=',minval(pdbg),maxval(pdbg)
       write(6,*)' min,max tbg=',minval(tbg),maxval(tbg)
       write(6,*)' min,max qbg=',minval(qbg),maxval(qbg)
       write(6,*)' min,max cwmbg=',minval(cwmbg),maxval(cwmbg)
       write(6,*)' min,max ubg=',minval(ubg),maxval(ubg)
       write(6,*)' min,max vbg=',minval(vbg),maxval(vbg)
       write(6,*)' min,max pdba=',minval(pdba),maxval(pdba)
       write(6,*)' min,max tba=',minval(tba),maxval(tba)
       write(6,*)' min,max qba=',minval(qba),maxval(qba)
       write(6,*)' min,max cwmba=',minval(cwmba),maxval(cwmba)
       write(6,*)' min,max uba=',minval(uba),maxval(uba)
       write(6,*)' min,max vba=',minval(vba),maxval(vba)
       write(6,*)' min,max pdba-pdbg=',minval(pdba-pdbg),maxval(pdba-pdbg)
       write(6,*)' min,max tba-tbg=',minval(tba-tbg),maxval(tba-tbg)
       write(6,*)' min,max qba-qbg=',minval(qba-qbg),maxval(qba-qbg)
       write(6,*)' min,max cwmba-cwmbg=',minval(cwmba-cwmbg),maxval(cwmba-cwmbg)
       write(6,*)' min,max uba-ubg=',minval(uba-ubg),maxval(uba-ubg)
       write(6,*)' min,max vba-vbg=',minval(vba-vbg),maxval(vba-vbg)
    end if
  
    deallocate(pdbg ,tbg ,qbg ,cwmbg ,ubg ,vbg )
    deallocate(pdba ,tba ,qba ,cwmba ,uba ,vba )
  
    
    deallocate(all_loc)
    deallocate(strp)
    deallocate(temp1)
    deallocate(tempa)
    deallocate(tempb)
    
  end subroutine wrwrfnmma_netcdf_wrf
end module wrwrfnmma_mod

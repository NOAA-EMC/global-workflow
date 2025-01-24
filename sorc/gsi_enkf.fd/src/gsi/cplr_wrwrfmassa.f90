module wrwrfmassa_mod
use abstract_wrwrfmassa_mod
  type, extends(abstract_wrwrfmassa_class) :: wrwrfmassa_class
  contains
    procedure, pass(this) :: wrwrfmassa_binary => wrwrfmassa_binary_wrf  
    procedure, pass(this) :: wrwrfmassa_netcdf => wrwrfmassa_netcdf_wrf  
    procedure, nopass :: update_start_date
    procedure, pass(this) :: generic_sub2grid
    procedure, nopass :: reorder_s 
    procedure, nopass :: contract_ibuf
    procedure, nopass :: transfer_ibuf2jbuf
    procedure, nopass :: move_hg_ibuf
  end type wrwrfmassa_class
contains
  subroutine wrwrfmassa_binary_wrf(this,mype)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    wrwrfmassa              write out wrf MASS restart file
  !   prgmmr: parrish          org: np22                date: 2004-06-23
  !
  ! abstract:  read wrf MASS guess restart interface file, add analysis
  !            increment, and write out wrf MASS analysis restart
  !            interface file.
  !
  ! program history log:
  !   2004-06-23  parrish, document
  !   2004-08-03  treadon - add only to module use, add intent in/out
  !   2004-11-18  parrish, rewrite for mpi-io
  !   2004-12-15  treadon - write analysis to file "wrf_inout"
  !   2005-02-17  todling - ifdef'ed wrf code out
  !   2005-11-29  derber - remove qsat_fix array
  !   2006-02-02  treadon - rename prslk as ges_prslk
  !   2006-02-15  treadon - convert specific humidity to moisture mixing ratio
  !   2006-03-07  treadon - convert virtual temperature to potential temperature
  !   2006-04-06  middlecoff - changed nfcst from 11 to lendian_in
  !   2006-07-28  derber  - include sensible temperature
  !   2006-07-31  kleist - change to use ges_ps instead of lnps
  !   2007-03-13  derber - remove unused qsinv2 from jfunc use list
  !   2007-04-12  parrish - add modifications to allow any combination of ikj or ijk
  !                          grid ordering for input 3D fields
  !   2008-03-31  safford - rm unused uses
  !   2008-12-05  todling - adjustment for dsfct time dimension addition
  !   2010-06-24  hu     - add code to write cloud/hydrometeor analysis fields to "wrf_inout"
  !   2011-04-29  todling - introduce MetGuess and wrf_mass_guess_mod
  !   2012-10-11  parrish - add option to swap bytes immediately after every call to mpi_file_read_at and
  !                           before every call to mpi_file_write_at (to handle cases of big-endian
  !                           file/little-endian machine and vice-versa)
  !   2012-11-26  hu     - add code to write updated soil fields to "wrf_inout"
  !   2013-01-26  parrish - WCOSS debug compile type mismatch error -- 
  !                            change to_native_endianness_i4 to to_native_endianness_r4
  !   2013-10-19  todling - metguess now holds background
  !   2014-03-12  hu     - add code to read ges_q2 (2m Q), 
  !                               Qnr(rain number concentration), 
  !                               and nsoil (number of soil levels)
  !   2015-01-13  ladwig - add code to read Qni and Qnc (cloud ice and water
  !                        number concentration)
  !   2015-01-15  hu     - add i_snowT_check to control temperature adjustment
  !                               over snow  
  !   2017-03-23  Hu      - add code to use hybrid vertical coodinate in WRF MASS
  !                               core
  !   2020-09-13 CAPS(C. Liu, L. Chen, and H. Li) 
  !                            - add code for hydrometer variables needed for
  !                            direct reflectivity DA
  !                            - add CV transform option on hydrometer variables
  !                            for direct reflectivity DA
  !   2022-03-15  Hu  change all th2 to t2m and convert 2m temperature 
  !                        from sensible to potentionl temperature for writing
  !
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
    use mpimod, only: mpi_byte,mpi_integer4,mpi_real4,mpi_comm_world,npe,ierror, &
         mpi_offset_kind,mpi_info_null,mpi_mode_rdwr,mpi_status_size
    use guess_grids, only: dsfct,&
         ntguessfc,ntguessig,ifilesig,ges_tsen
    use wrf_mass_guess_mod, only: ges_tten
    use gridmod, only: lon1,lat1,nlat_regional,nlon_regional,&
         nsig,nsig_soil,eta1_ll,pt_ll,itotsub,iglobal,update_regsfc,&
         aeta1_ll,eta2_ll,aeta2_ll
    use constants, only: one,zero_single,rd_over_cp_mass,one_tenth,h300,r10,r100
    use constants, only: soilmoistmin
    use gsi_io, only: lendian_in,verbose
    use rapidrefresh_cldsurf_mod, only: l_hydrometeor_bkio,l_gsd_soilTQ_nudge,&
         i_use_2mq4b,i_use_2mt4b
    use wrf_mass_guess_mod, only: destroy_cld_grids
    use gsi_bundlemod, only: GSI_BundleGetPointer
    use gsi_metguess_mod, only: gsi_metguess_get,GSI_MetGuess_Bundle
    use native_endianness, only: byte_swap
    use mpeu_util, only: die
    use read_wrf_mass_guess_mod, only: read_wrf_mass_guess_class
  
    implicit none
  
  ! Declare passed variables
    class(wrwrfmassa_class), intent(inout) :: this 
    integer(i_kind),intent(in   ) :: mype
  
  ! Declare local parameters
    type(read_wrf_mass_guess_class) :: read_wrf
    character(len=*),parameter::myname='wrwrfmassa_binary'
    real(r_kind),parameter:: r225=225.0_r_kind
    type(read_wrf_mass_guess_class) :: wrf_mass_guess
  
  ! Declare local variables
    real(r_single),allocatable::tempa(:,:),tempb(:,:)
    real(r_single),allocatable::temp1(:,:),temp1u(:,:),temp1v(:,:)
    real(r_single),allocatable::all_loc(:,:,:)
    integer(i_kind),allocatable::itemp1(:,:)
    integer(i_kind),allocatable::igtype(:),kdim(:),kord(:)
    integer(kind=mpi_offset_kind),allocatable::offset(:)
    integer(kind=mpi_offset_kind) this_offset,offset_mub,offset_start_date
    integer(i_kind),allocatable::length(:)
    integer(i_kind) this_length,length_mub,igtype_mub,length_start_date
    integer(i_llong) num_swap
    character(6) filename
    character(9) wrfanl
    integer(i_kind) ifld,im,jm,lm,num_mass_fields
    integer(i_kind) num_loc_groups,num_j_groups
    integer(i_kind) i,it,j,k
    integer(i_kind) i_mu,i_t,i_q,i_u,i_v
    integer(i_kind) i_qc,i_qi,i_qr,i_qs,i_qg,i_qnr,i_qni,i_qnc
    integer(i_kind) kqc,kqi,kqr,kqs,kqg,kqnr,kqni,kqnc,i_tt,ktt
    integer(i_kind) i_th2,i_q2,i_soilt1,i_tslb,i_smois,ktslb,ksmois,ksize
    integer(i_kind) i_sst,i_tsk
    real(r_kind) psfc_this,psfc_this_dry
    real(r_kind):: work_prsl,work_prslk
    real(r_kind),dimension(lat1+2,lon1+2):: q_integral,q_integralc4h
    integer(i_llong) n_position
    integer(i_kind) iskip,jextra,nextra
    integer(i_kind) status(mpi_status_size)
    integer(i_kind) request,request_ldmk,request_ldmk_snow,request_ldmk_seaice
    integer(i_kind) jbegin(0:npe),jend(0:npe-1),jend2(0:npe-1)
    integer(i_kind) kbegin(0:npe),kend(0:npe-1)
    integer(i_long),allocatable:: ibuf(:,:)
    integer(i_long),allocatable:: jbuf(:,:,:)
    real(r_single),allocatable::mub(:,:), landmask(:,:),snow(:,:),seaice(:,:)
    real(r_single),allocatable::t1st2d(:,:)
    integer(i_kind) kdim_mub,i_snowT_check
    integer(i_kind) kt,kq,ku,kv
    integer(i_kind) mfcst
    integer(i_long) iyear,imonth,iday,ihour,iminute,isecond,dummy3(3)
    real(r_single) pt_regional_single
    real(r_kind) deltasigma,deltasigmac4h
    integer(i_kind) ip1,jp1
    character(1) chdrbuf(2048)
    integer(i_kind) iadd,ier,istatus,n_actual_clouds
    character(132) memoryorder
  
    real(r_kind), pointer :: ges_ps(:,:)=>NULL()
    real(r_kind), pointer :: ges_tsk(:,:)=>NULL()
    real(r_kind), pointer :: ges_t2m(:,:)=>NULL()
    real(r_kind), pointer :: ges_q2(:,:)=>NULL()
    real(r_kind), pointer :: ges_soilt1(:,:)=>NULL()
    real(r_kind), pointer :: ges_tslb_it(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_smois_it(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_u (:,:,:)=>NULL()
    real(r_kind), pointer :: ges_v (:,:,:)=>NULL()
    real(r_kind), pointer :: ges_q (:,:,:)=>NULL()
    real(r_kind), pointer :: ges_qc(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_qi(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_qr(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_qs(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_qg(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_qnr(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_qni(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_qnc(:,:,:)=>NULL()
    logical print_verbose
  
    print_verbose=.false.
    if(verbose .and. mype == 0)print_verbose=.true.
    it=ntguessig
  
    ksize=0
  
  ! Inquire about cloud guess fields
    call gsi_metguess_get('clouds::3d',n_actual_clouds,istatus)
    if (n_actual_clouds>0) then
  !    get pointer to relevant instance of cloud-related backgroud
       ier=0
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'ql', ges_qc, istatus );ier=ier+istatus
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qi', ges_qi, istatus );ier=ier+istatus
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qr', ges_qr, istatus );ier=ier+istatus
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qs', ges_qs, istatus );ier=ier+istatus
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qg', ges_qg, istatus );ier=ier+istatus
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qnr',ges_qnr,istatus );ier=ier+istatus
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qni',ges_qni,istatus );ier=ier+istatus
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qnc',ges_qnc,istatus );ier=ier+istatus
       if (ier/=0) n_actual_clouds=0
    end if
  
  !   1.  get offsets etc only for records to be updated
  
  !        they are MU, T, Q, U, V,  skint/sst       (for MU, need MU_B and pt_regional)
  
    im=nlon_regional
    jm=nlat_regional
    lm=nsig
  
    num_mass_fields=4*lm+4
    if(l_hydrometeor_bkio .and. n_actual_clouds>0) num_mass_fields=4*lm+4+9*lm
    if(l_gsd_soilTQ_nudge) num_mass_fields=4*lm+4+2*nsig_soil+2
    if(l_hydrometeor_bkio .and. l_gsd_soilTQ_nudge) num_mass_fields=4*lm+4+9*lm+2*nsig_soil+2
    allocate(offset(num_mass_fields))
    allocate(igtype(num_mass_fields),kdim(num_mass_fields),kord(num_mass_fields))
    allocate(length(num_mass_fields))
    allocate(t1st2d(im,jm))
  
  !    igtype is a flag indicating whether each input MASS field is h-, u-, or v-grid
  !    and whether integer or real
  !     abs(igtype)=1 for h-grid
  !                =2 for u-grid
  !                =3 for v-grid
  !
  !     igtype < 0 for integer field
  
  !    offset is the byte count preceding each record to be read/written from/to the wrf binary file.
  !       used as individual file pointers by mpi_file_read/mpi_file_write
  
    write(filename,'("sigf",i2.2)')ifilesig(it)
    open(lendian_in,file=filename,form='unformatted') ; rewind lendian_in
    if(mype == 0) write(6,*)'READ_WRF_MASS_OFFSET_FILE:  open lendian_in=',lendian_in,' to file=',filename
    read(lendian_in) iyear,imonth,iday,ihour,iminute,isecond,dummy3,pt_regional_single
    do iskip=2,5
       read(lendian_in)
    end do
    read(lendian_in) 
    read(lendian_in) n_position          !  offset for START_DATE record
    offset_start_date=n_position
    length_start_date=2048
  
    i=0
    read(lendian_in) n_position                                         ! mub
    offset_mub=n_position
    length_mub=im*jm
    igtype_mub=1
    kdim_mub=1
  
  
  !     open wrf file for mpi-io reading and writing
    wrfanl = 'wrf_inout'
    call mpi_file_open(mpi_comm_world,trim(wrfanl),mpi_mode_rdwr,mpi_info_null,mfcst,ierror)
  
  !     update START_DATE record so it contains new analysis time in place of old starting time
    call mpi_file_read_at(mfcst,offset_start_date,chdrbuf,length_start_date,mpi_byte,status,ierror)
    if(mype==0)  then
       call update_start_date(chdrbuf,iyear,imonth,iday,ihour,iminute,isecond)
       call mpi_file_write_at(mfcst,offset_start_date,chdrbuf,length_start_date,mpi_byte,status,ierror)
    end if
  
  !          begin read of mub while doing rest of offset setup
    allocate(mub(im,jm))
    call mpi_file_iread_at(mfcst,offset_mub,mub,length_mub,mpi_real4,request,ierror)
  
  
    i=i+1 ; i_mu =i                                                ! mu
    read(lendian_in) n_position
    offset(i)=n_position ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=1
    if(print_verbose) write(6,*)' mu, i,igtype,offset,kdim(i) = ',i,igtype(i),offset(i),kdim(i)
  
    read(lendian_in) n_position                                    !  geopotential  (should this be updated??)
  
    i_t=i+1
    read(lendian_in) n_position,memoryorder
    do k=1,lm
       i=i+1                                                       ! theta(k)  (pot temp)
       if(trim(memoryorder)=='XZY') then
          iadd=0
          kord(i)=lm
       else
          iadd=(k-1)*im*jm*4
          kord(i)=1
       end if
       offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=lm
       if(print_verbose.and.k==1) write(6,*)' temp i,igtype,offset,kdim(i) = ',i,igtype(i),offset(i),kdim(i)
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
       if(print_verbose .and.k==1) write(6,*)' q i,igtype,offset,kdim(i) = ',i,igtype(i),offset(i),kdim(i)
    end do
  
    i_u=i+1
    read(lendian_in) n_position,memoryorder
    do k=1,lm
       i=i+1                                                       ! u(k)
       if(trim(memoryorder)=='XZY') then
          iadd=0
          kord(i)=lm
       else
          iadd=(k-1)*(im+1)*jm*4
          kord(i)=1
       end if
       offset(i)=n_position+iadd
       igtype(i)=2 ; kdim(i)=lm
       length(i)=(im+1)*jm
       if(print_verbose.and.k==1) write(6,*)' u i,igtype,offset,kdim(i),kord(i) = ', &
                                                             i,igtype(i),offset(i),kdim(i),kord(i)
    end do
  
    i_v=i+1
    read(lendian_in) n_position,memoryorder
    do k=1,lm
       i=i+1                                                       ! v(k)
       if(trim(memoryorder)=='XZY') then
          iadd=0
          kord(i)=lm
       else
          iadd=(k-1)*im*(jm+1)*4
          kord(i)=1
       end if
       offset(i)=n_position+iadd ; length(i)=im*(jm+1) ; igtype(i)=3 ; kdim(i)=lm
       if(print_verbose.and.k==1) write(6,*)' v i,igtype,offset,kdim(i),kord(i) = ', &
                                                              i,igtype(i),offset(i),kdim(i),kord(i)
    end do
  
    read(lendian_in) n_position                                         ! landmask
    allocate(landmask(im,jm))
    call mpi_file_iread_at(mfcst,n_position,landmask,im*jm,mpi_real4,request_ldmk,ierror)
  
    read(lendian_in) n_position                                         ! xice
    allocate(seaice(im,jm))
    call mpi_file_iread_at(mfcst,n_position,seaice,im*jm,mpi_real4,request_ldmk_seaice,ierror)
  
    i=i+1 ; i_sst=i                                                ! sst
    read(lendian_in) n_position
    offset(i)=n_position ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=1
    if(print_verbose) write(6,*)' sst i,igtype,offset,kdim(i) = ',i,igtype(i),offset(i),kdim(i)
  
    read(lendian_in)                                                    ! ivgtyp
    read(lendian_in)                                                    ! isltyp
    read(lendian_in)                                                    ! vegfrac
  
    read(lendian_in) n_position                                         ! sno
    allocate(snow(im,jm))
    call mpi_file_iread_at(mfcst,n_position,snow,im*jm,mpi_real4,request_ldmk_snow,ierror)
  
    read(lendian_in)                                                    ! u10
    read(lendian_in)                                                    ! v10
    if(l_gsd_soilTQ_nudge) then
       i_smois=i+1
       read(lendian_in) n_position,ksize,memoryorder
       do k=1,ksize
          i=i+1                                                         ! smois(k)
          if(trim(memoryorder)=='XZY') then
             iadd=0
             kord(i)=ksize
          else
             iadd=(k-1)*im*jm*4
             kord(i)=1
          end if
          offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=ksize
          if(print_verbose.and.k==1) write(6,*)' smois i,igtype,offset,kdim(i) = ',i,igtype(i),offset(i),kdim(i)
       end do
  
       i_tslb=i+1
       read(lendian_in) n_position,ksize,memoryorder
       do k=1,ksize
          i=i+1                                                      ! tslb(k)
          if(trim(memoryorder)=='XZY') then
             iadd=0
             kord(i)=ksize
          else
             iadd=(k-1)*im*jm*4
             kord(i)=1
          end if
          offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=ksize
          if(print_verbose.and.k==1) write(6,*)' tslb i,igtype,offset,kdim(i) = ',i,igtype(i),offset(i),kdim(i)
       end do
    else
       i_tslb=0
       i_smois=0
       read(lendian_in)                                                 ! smois
       read(lendian_in)                                                 ! tslb
    endif
  
    i=i+1 ; i_tsk=i                                                ! tsk
    read(lendian_in) n_position
    offset(i)=n_position ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=1
    if(print_verbose) write(6,*)' tsk i,igtype,offset,kdim(i) = ',i,igtype(i),offset(i),kdim(i)
  
  
    i=i+1 ; i_q2=i                                                ! q2
    read(lendian_in) n_position
    offset(i)=n_position ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=1
    if(print_verbose) write(6,*)' q2 i,igtype,offset,kdim(i) = ',i,igtype(i),offset(i),kdim(i)
  
    if(l_gsd_soilTQ_nudge) then
       i=i+1 ; i_soilt1=i                                            ! soilt1
       read(lendian_in) n_position
       offset(i)=n_position ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=1
       if(print_verbose) write(6,*)' soilt1 i,igtype,offset,kdim(i) = ',i,igtype(i),offset(i),kdim(i)
  
       i=i+1 ; i_th2=i                                               ! th2
       read(lendian_in) n_position
       offset(i)=n_position ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=1
       if(print_verbose) write(6,*)' th2, i,igtype,offset,kdim(i) = ',i,igtype(i),offset(i),kdim(i)
    else
       i_soilt1=0
       i_th2=0
    endif
  
  ! for cloud/hydrometeor analysis fields
    if(l_hydrometeor_bkio .and. n_actual_clouds>0) then
  
       i_qc=i+1
       read(lendian_in) n_position,memoryorder
       do k=1,lm
          i=i+1                                                       ! qc(k)
          if(trim(memoryorder)=='XZY') then
             iadd=0
             kord(i)=lm
          else
             iadd=(k-1)*im*jm*4
             kord(i)=1
          end if
          offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=lm
          if(print_verbose.and.k==1) write(6,*)' qc i,igtype,offset,kdim(i) = ',i,igtype(i),offset(i),kdim(i)
       end do
  
       i_qr=i+1
       read(lendian_in) n_position,memoryorder
       do k=1,lm
          i=i+1                                                       ! qr(k)
          if(trim(memoryorder)=='XZY') then
             iadd=0
             kord(i)=lm
          else
             iadd=(k-1)*im*jm*4
             kord(i)=1
           end if
          offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=lm
          if(print_verbose.and.k==1) write(6,*)' qr i,igtype,offset,kdim(i) = ',i,igtype(i),offset(i),kdim(i)
       end do
  
       i_qi=i+1
       read(lendian_in) n_position,memoryorder
       do k=1,lm
          i=i+1                                                       ! qi(k)
          if(trim(memoryorder)=='XZY') then
             iadd=0
             kord(i)=lm
          else
             iadd=(k-1)*im*jm*4
             kord(i)=1
          end if
          offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=lm
          if(print_verbose.and.k==1) write(6,*)' qi i,igtype,offset,kdim(i) = ',i,igtype(i),offset(i),kdim(i)
       end do
  
       i_qs=i+1
       read(lendian_in) n_position,memoryorder
       do k=1,lm
          i=i+1                                                       ! qs(k)
          if(trim(memoryorder)=='XZY') then
             iadd=0
             kord(i)=lm
          else
             iadd=(k-1)*im*jm*4
             kord(i)=1
          end if
          offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=lm
          if(print_verbose.and.k==1) write(6,*)' qs i,igtype,offset,kdim(i) = ',i,igtype(i),offset(i),kdim(i)
       end do
   
       i_qg=i+1
       read(lendian_in) n_position,memoryorder
       do k=1,lm
          i=i+1                                                       ! qg(k)
          if(trim(memoryorder)=='XZY') then
             iadd=0
             kord(i)=lm
          else
             iadd=(k-1)*im*jm*4
             kord(i)=1
          end if
          offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=lm
          if(print_verbose.and.k==1) write(6,*)' qg i,igtype,offset,kdim(i) = ',i,igtype(i),offset(i),kdim(i)
       end do
  
       i_qnr=i+1
       read(lendian_in) n_position,memoryorder
       do k=1,lm
          i=i+1                                                       !  qnr(k)
          if(trim(memoryorder)=='XZY') then
             iadd=0
             kord(i)=lm
          else
             iadd=(k-1)*im*jm*4
             kord(i)=1
          end if
          offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=lm
          if(print_verbose.and.k==1) write(6,*)' qnr i,igtype,offset,kdim(i) = ',i,igtype(i),offset(i),kdim(i)
       end do

       i_qni=i+1
       read(lendian_in) n_position,memoryorder
       do k=1,lm
          i=i+1                                                       !  qni(k)
          if(trim(memoryorder)=='XZY') then
             iadd=0
             kord(i)=lm
          else
             iadd=(k-1)*im*jm*4
             kord(i)=1
          end if
          offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=lm
          if(print_verbose.and.k==1) write(6,*)' qni i,igtype,offset,kdim(i) = ',i,igtype(i),offset(i),kdim(i)
       end do

       i_qnc=i+1
       read(lendian_in) n_position,memoryorder
       do k=1,lm
          i=i+1                                                       !  qnc(k)
          if(trim(memoryorder)=='XZY') then
             iadd=0
             kord(i)=lm
          else
             iadd=(k-1)*im*jm*4
             kord(i)=1
          end if
          offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=lm
          if(print_verbose.and.k==1) write(6,*)' qnc i,igtype,offset,kdim(i) = ',i,igtype(i),offset(i),kdim(i)
       end do
  
       i_tt=i+1
       read(lendian_in) n_position,memoryorder
       do k=1,lm
          i=i+1                                                       ! tt(k)
          if(trim(memoryorder)=='XZY') then
             iadd=0
             kord(i)=lm
          else
             iadd=(k-1)*im*jm*4
             kord(i)=1
          end if
          offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=lm
          if(print_verbose.and.k==1) write(6,*)' tt i,igtype,offset,kdim(i) = ',i,igtype(i),offset(i),kdim(i)
       end do
  
    endif    ! l_hydrometeor_bkio
  
    close(lendian_in)
  
  !          set up evenly distributed index range over all processors for all input fields
  
  
    num_loc_groups=num_mass_fields/npe
    nextra=num_mass_fields-num_loc_groups*npe
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
  
  !     sub2grid to get tempa for fields to be updated
  
  
  ! Create all_loc from ges_*
    allocate(all_loc(lat1,lon1,num_mass_fields))
    all_loc=zero_single
    kt=i_t-1
    kq=i_q-1
    ku=i_u-1
    kv=i_v-1
    q_integral=one
    q_integralc4h=zero_single
  ! for hydrometeors
    if(l_hydrometeor_bkio .and. n_actual_clouds>0) then
  !    get pointer to relevant instance of cloud-related backgroud
       ier=0
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'ql', ges_qc, istatus );ier=ier+istatus
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qi', ges_qi, istatus );ier=ier+istatus
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qr', ges_qr, istatus );ier=ier+istatus
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qs', ges_qs, istatus );ier=ier+istatus
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qg', ges_qg, istatus );ier=ier+istatus
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qnr',ges_qnr,istatus );ier=ier+istatus
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qni',ges_qni,istatus );ier=ier+istatus
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qnc',ges_qnc,istatus );ier=ier+istatus
       if (ier/=0) then
           write(6,*)'wrwrfmassa_binary: getpointer failed, cannot do cloud analysis'
           call stop2(999)
       endif
       kqc=i_qc-1
       kqi=i_qi-1
       kqr=i_qr-1
       kqs=i_qs-1
       kqg=i_qg-1
       kqnr=i_qnr-1
       kqni=i_qni-1
       kqnc=i_qnc-1
       ktt=i_tt-1
    endif
    ier=0
    call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'ps', ges_ps, istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'u' , ges_u , istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'v' , ges_v , istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'q' , ges_q , istatus );ier=ier+istatus
    if (ier/=0) then ! doesn't have to die - code can be generalized to bypass missing vars
        write(6,*)'wrwrfmassa_binary: getpointer failed, cannot retrieve ps,u,v,q'
        call stop2(999)
    endif
    do k=1,nsig
       deltasigma=eta1_ll(k)-eta1_ll(k+1)
       deltasigmac4h=eta2_ll(k)-eta2_ll(k+1)
       kt=kt+1
       kq=kq+1
       ku=ku+1
       kv=kv+1
  ! for hydrometeors
       if(l_hydrometeor_bkio .and. n_actual_clouds>0) then
          kqc=kqc+1
          kqi=kqi+1
          kqr=kqr+1
          kqs=kqs+1
          kqg=kqg+1
          kqnr=kqnr+1
          kqni=kqni+1
          kqnc=kqnc+1
          ktt=ktt+1
       endif
       do i=1,lon1
          ip1=i+1
          do j=1,lat1
             jp1=j+1
             all_loc(j,i,ku)=ges_u(jp1,ip1,k)
             all_loc(j,i,kv)=ges_v(jp1,ip1,k)
  
  
  !          Convert sensible temperature to potential temperature
             work_prsl  = one_tenth*(aeta1_ll(k)*(r10*ges_ps(jp1,ip1)-pt_ll)+aeta2_ll(k)+pt_ll)
             work_prslk = (work_prsl/r100)**rd_over_cp_mass
             all_loc(j,i,kt) = ges_tsen(jp1,ip1,k,it)/work_prslk
  
  
  !          Subtract offset to get output potential temperature
             all_loc(j,i,kt)=all_loc(j,i,kt) - h300
  
  
  !          Convert specific humidity to mixing ratio
             all_loc(j,i,kq)= ges_q(jp1,ip1,k)/(one-ges_q(jp1,ip1,k))
  
  ! for hydrometeors
             if(l_hydrometeor_bkio .and. n_actual_clouds>0) then
                all_loc(j,i,kqc)=ges_qc(jp1,ip1,k)
                all_loc(j,i,kqi)=ges_qi(jp1,ip1,k)
                all_loc(j,i,kqr)=ges_qr(jp1,ip1,k)
                all_loc(j,i,kqs)=ges_qs(jp1,ip1,k)
                all_loc(j,i,kqg)=ges_qg(jp1,ip1,k)
                all_loc(j,i,kqnr)=ges_qnr(jp1,ip1,k)
                all_loc(j,i,kqni)=ges_qni(jp1,ip1,k)
                all_loc(j,i,kqnc)=ges_qnc(jp1,ip1,k)
                all_loc(j,i,ktt)=ges_tten(jp1,ip1,k,it)
             endif
  
             q_integral(jp1,ip1)=q_integral(jp1,ip1)+deltasigma*ges_q(jp1,ip1,k)/(one-ges_q(jp1,ip1,k))
             q_integralc4h(jp1,ip1)=q_integralc4h(jp1,ip1)+deltasigmac4h*ges_q(jp1,ip1,k)/(one-ges_q(jp1,ip1,k))
  
          end do
       end do
    end do
  
    do i=1,lon1
       ip1=i+1
       do j=1,lat1
          jp1=j+1
          psfc_this=r10*ges_ps(jp1,ip1)   ! convert from cb to mb
          psfc_this_dry=pt_ll+((psfc_this-pt_ll)-q_integralc4h(jp1,ip1))/q_integral(jp1,ip1)
          all_loc(j,i,i_mu)=r100*psfc_this_dry
       end do
    end do
  
  ! Load updated skin temperature array if writing out to analysis file
    if (update_regsfc) then
       do i=1,lon1
          ip1=i+1
          do j=1,lat1
             jp1=j+1
             all_loc(j,i,i_sst)=dsfct(jp1,ip1,ntguessfc)
             all_loc(j,i,i_tsk)=dsfct(jp1,ip1,ntguessfc)
          end do
       end do
    end if
  
  ! for soil nudging
    if(l_gsd_soilTQ_nudge) then
       ier=0
       call GSI_BundleGetPointer (GSI_MetGuess_Bundle(it),'smoist',ges_smois_it,istatus)
       ier=ier+istatus
       call GSI_BundleGetPointer (GSI_MetGuess_Bundle(it),'tslb' ,ges_tslb_it ,istatus)
       ier=ier+istatus
       if (ier/=0) call die(trim(myname),'cannot get pointers for tslb/smois, ier =',ier)
       ktslb=i_tslb-1
       ksmois=i_smois-1
       do k=1,nsig_soil
          ktslb=ktslb+1
          ksmois=ksmois+1
          do i=1,lon1
             ip1=i+1
             do j=1,lat1
                jp1=j+1
                all_loc(j,i,ktslb)=ges_tslb_it(jp1,ip1,k)
                all_loc(j,i,ksmois)=ges_smois_it(jp1,ip1,k)
             end do
          end do
       end do
       ier=0
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'tsoil', ges_soilt1, istatus );ier=ier+istatus
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'tskn' , ges_tsk , istatus );ier=ier+istatus
       if (ier/=0) then ! doesn't have to die - code can be generalized to bypass missing vars
           write(6,*)'wrwrfmassa_binary: getpointer failed, cannot retrieve soil stuff'
           call stop2(999)
       endif
       do i=1,lon1
          ip1=i+1
          do j=1,lat1
             jp1=j+1
             all_loc(j,i,i_tsk)=ges_tsk(jp1,ip1)
             all_loc(j,i,i_soilt1)=ges_soilt1(jp1,ip1)
          end do
       end do
    endif ! l_gsd_soilTQ_nudge
    if(i_use_2mt4b > 0 ) then
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 't2m',  ges_t2m, istatus );ier=ier+istatus
       do i=1,lon1
          ip1=i+1
          do j=1,lat1
  !  Convert 2m sensible T to potential T
             jp1=j+1
             all_loc(j,i,i_th2)=ges_t2m(jp1,ip1)*(r100/ges_ps(jp1,ip1))**rd_over_cp_mass
          end do
       end do
    endif
    if (i_use_2mq4b > 0) then
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'q2m' ,  ges_q2,  istatus );ier=ier+istatus
       do i=1,lon1
          ip1=i+1
          do j=1,lat1
  !  Convert 2m specific humidity to mixing ratio
             jp1=j+1
             all_loc(j,i,i_q2)=ges_q2(jp1,ip1)/(one-ges_q2(jp1,ip1))
          end do
       end do
    endif
  
    allocate(tempa(itotsub,kbegin(mype):kend(mype)))
    call this%generic_sub2grid(all_loc,tempa,kbegin(mype),kend(mype),kbegin,kend,mype,num_mass_fields)
    deallocate(all_loc)
  
    allocate(ibuf((im+1)*(jm+1),kbegin(mype):kend(mype)))
  !      finish reading in mub
    call mpi_wait(request,status,ierror)
    if(byte_swap) then
       num_swap=length_mub
       call to_native_endianness_r4(mub(1,1),num_swap)
    end if
    call mpi_wait(request_ldmk,status,ierror)
    if(byte_swap) then
       num_swap=im*jm
       call to_native_endianness_r4(landmask(1,1),num_swap) !got landmask for
                                                            ! soil nudging in 2X
                                                            ! grid
    end if
    call mpi_wait(request_ldmk_snow,status,ierror)
    if(byte_swap) then
       num_swap=im*jm
       call to_native_endianness_r4(snow(1,1),num_swap)     !got snow for
                                                            ! soil nudging in 2X
                                                            ! grid
    end if
    call mpi_wait(request_ldmk_seaice,status,ierror)
    if(byte_swap) then
       num_swap=im*jm
       call to_native_endianness_r4(seaice(1,1),num_swap)   !got seaice for
                                                            ! soil nudging in 2X
                                                            ! grid
    end if
  
  
  !   2.  create ibuf with records to be updated read in
  
  !                                    read temps
    if(kord(i_t)/=1) then
       allocate(jbuf(im,lm,jbegin(mype):min(jend(mype),jm)))
       this_offset=offset(i_t)+(jbegin(mype)-1)*4*im*lm
       this_length=(jend(mype)-jbegin(mype)+1)*im*lm
       call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
       if(byte_swap) then
          num_swap=this_length
          call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
       end if
       call read_wrf%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                          jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im+1,jm+1,i_t,i_t+lm-1)
       deallocate(jbuf)
    end if
  
  !                                    read q
    if(kord(i_q)/=1) then
       allocate(jbuf(im,lm,jbegin(mype):min(jend(mype),jm)))
       this_offset=offset(i_q)+(jbegin(mype)-1)*4*im*lm
       this_length=(jend(mype)-jbegin(mype)+1)*im*lm
       call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
       if(byte_swap) then
          num_swap=this_length
          call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
       end if
       call read_wrf%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                          jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im+1,jm+1,i_q,i_q+lm-1)
       deallocate(jbuf)
    end if
  
  !                                    read u
    if(kord(i_u)/=1) then
       allocate(jbuf(im+1,lm,jbegin(mype):min(jend(mype),jm)))
       this_offset=offset(i_u)+(jbegin(mype)-1)*4*(im+1)*lm
       this_length=(jend(mype)-jbegin(mype)+1)*(im+1)*lm
       call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
       if(byte_swap) then
          num_swap=this_length
          call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
       end if
       call read_wrf%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                          jbegin,jend,kbegin,kend,mype,npe,im+1,jm,lm,im+1,jm+1,i_u,i_u+lm-1)
       deallocate(jbuf)
    end if
  
  
  !                                    read v
    if(kord(i_v)/=1) then
       jend2=jend
       jend2(npe-1)=jend2(npe-1)+1
       allocate(jbuf(im,lm,jbegin(mype):jend2(mype)))
       this_offset=offset(i_v)+(jbegin(mype)-1)*4*im*lm
       this_length=(jend2(mype)-jbegin(mype)+1)*im*lm
       call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
       if(byte_swap) then
          num_swap=this_length
          call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
       end if
       call read_wrf%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend2(mype),ibuf,kbegin(mype),kend(mype), &
                          jbegin,jend2,kbegin,kend,mype,npe,im,jm+1,lm,im+1,jm+1,i_v,i_v+lm-1)
       deallocate(jbuf)
    end if
  
    if(l_gsd_soilTQ_nudge) then
       lm=nsig_soil
  !                                    read smois
       if(kord(i_smois)/=1) then
          allocate(jbuf(im,lm,jbegin(mype):min(jend(mype),jm)))
          this_offset=offset(i_smois)+(jbegin(mype)-1)*4*im*lm
          this_length=(jend(mype)-jbegin(mype)+1)*im*lm
          call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
          if(byte_swap) then
             num_swap=this_length
             call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
          end if
          call read_wrf%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                         jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im+1,jm+1,i_smois,i_smois+lm-1)
          deallocate(jbuf)
       end if
  !                                    read tslb
       if(kord(i_tslb)/=1) then
          allocate(jbuf(im,lm,jbegin(mype):min(jend(mype),jm)))
          this_offset=offset(i_tslb)+(jbegin(mype)-1)*4*im*lm
          this_length=(jend(mype)-jbegin(mype)+1)*im*lm
          call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
          if(byte_swap) then
             num_swap=this_length
             call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
          end if
          call read_wrf%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                         jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im+1,jm+1,i_tslb,i_tslb+lm-1)
          deallocate(jbuf)
       end if
  
       lm=nsig
    endif
  
  ! read hydrometeors
    if(l_hydrometeor_bkio .and. n_actual_clouds>0) then
  !                                    read qc
       if(kord(i_qc)/=1) then
          allocate(jbuf(im,lm,jbegin(mype):min(jend(mype),jm)))
          this_offset=offset(i_qc)+(jbegin(mype)-1)*4*im*lm
          this_length=(jend(mype)-jbegin(mype)+1)*im*lm
          call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
          if(byte_swap) then
             num_swap=this_length
             call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
          end if
          call read_wrf%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                           jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im+1,jm+1,i_qc,i_qc+lm-1)
          deallocate(jbuf)
       end if
  
  !                                    read qr
       if(kord(i_qr)/=1) then
          allocate(jbuf(im,lm,jbegin(mype):min(jend(mype),jm)))
          this_offset=offset(i_qr)+(jbegin(mype)-1)*4*im*lm
          this_length=(jend(mype)-jbegin(mype)+1)*im*lm
          call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
          if(byte_swap) then
             num_swap=this_length
             call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
          end if
          call read_wrf%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                           jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im+1,jm+1,i_qr,i_qr+lm-1)
          deallocate(jbuf)
       end if
  
  !                                    read qi
       if(kord(i_qi)/=1) then
          allocate(jbuf(im,lm,jbegin(mype):min(jend(mype),jm)))
          this_offset=offset(i_qi)+(jbegin(mype)-1)*4*im*lm
          this_length=(jend(mype)-jbegin(mype)+1)*im*lm
          call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
          if(byte_swap) then
             num_swap=this_length
             call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
          end if
          call read_wrf%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                           jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im+1,jm+1,i_qi,i_qi+lm-1)
          deallocate(jbuf)
       end if
  
  !                                    read qs
       if(kord(i_qs)/=1) then
          allocate(jbuf(im,lm,jbegin(mype):min(jend(mype),jm)))
          this_offset=offset(i_qs)+(jbegin(mype)-1)*4*im*lm
          this_length=(jend(mype)-jbegin(mype)+1)*im*lm
          call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
          if(byte_swap) then
             num_swap=this_length
             call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
          end if
          call read_wrf%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                           jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im+1,jm+1,i_qs,i_qs+lm-1)
          deallocate(jbuf)
       end if
  
  !                                    read qg
       if(kord(i_qg)/=1) then
          allocate(jbuf(im,lm,jbegin(mype):min(jend(mype),jm)))
          this_offset=offset(i_qg)+(jbegin(mype)-1)*4*im*lm
          this_length=(jend(mype)-jbegin(mype)+1)*im*lm
          call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
          if(byte_swap) then
             num_swap=this_length
             call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
          end if
          call read_wrf%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                           jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im+1,jm+1,i_qg,i_qg+lm-1)
          deallocate(jbuf)
       end if
   
  !                                    read qnr
       if(kord(i_qnr)/=1) then
          allocate(jbuf(im,lm,jbegin(mype):min(jend(mype),jm)))
          this_offset=offset(i_qnr)+(jbegin(mype)-1)*4*im*lm
          this_length=(jend(mype)-jbegin(mype)+1)*im*lm
          call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
          if(byte_swap) then
             num_swap=this_length
             call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
          end if
          call read_wrf%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                           jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im+1,jm+1,i_qnr,i_qnr+lm-1)
          deallocate(jbuf)
       end if

  !                                    read qni
       if(kord(i_qni)/=1) then
          allocate(jbuf(im,lm,jbegin(mype):min(jend(mype),jm)))
          this_offset=offset(i_qni)+(jbegin(mype)-1)*4*im*lm
          this_length=(jend(mype)-jbegin(mype)+1)*im*lm
          call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
          if(byte_swap) then
             num_swap=this_length
             call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
          end if
          call read_wrf%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                           jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im+1,jm+1,i_qni,i_qni+lm-1)
          deallocate(jbuf)
       end if
  
  !                                    read qnc
       if(kord(i_qnc)/=1) then
          allocate(jbuf(im,lm,jbegin(mype):min(jend(mype),jm)))
          this_offset=offset(i_qnc)+(jbegin(mype)-1)*4*im*lm
          this_length=(jend(mype)-jbegin(mype)+1)*im*lm
          call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
          if(byte_swap) then
             num_swap=this_length
             call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
          end if
          call read_wrf%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                           jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im+1,jm+1,i_qnc,i_qnc+lm-1)
          deallocate(jbuf)
       end if
  
  !                                    read tt
       if(kord(i_tt)/=1) then
          allocate(jbuf(im,lm,jbegin(mype):min(jend(mype),jm)))
          this_offset=offset(i_tt)+(jbegin(mype)-1)*4*im*lm
          this_length=(jend(mype)-jbegin(mype)+1)*im*lm
          call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
          if(byte_swap) then
             num_swap=this_length
             call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
          end if
          call read_wrf%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                           jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im+1,jm+1,i_tt,i_tt+lm-1)
          deallocate(jbuf)
       end if
  
    endif   ! l_hydrometeor_bkio
  
  !---------------------- read surface files last
    do k=kbegin(mype),kend(mype)
       if(kdim(k)==1.or.kord(k)==1) then
          call mpi_file_read_at(mfcst,offset(k),ibuf(1,k),length(k),mpi_integer4,status,ierror)
          if(byte_swap) then
             num_swap=length(k)
             call to_native_endianness_i4(ibuf(1,k),num_swap)
          end if
          if(igtype(k)==1) call wrf_mass_guess%expand_ibuf(ibuf(1,k),im  ,jm     ,im+1,jm+1)
          if(igtype(k)==2) call wrf_mass_guess%expand_ibuf(ibuf(1,k),im+1,jm     ,im+1,jm+1)
          if(igtype(k)==3) call wrf_mass_guess%expand_ibuf(ibuf(1,k),im  ,jm+1,im+1,jm+1)
       end if
    end do
  ! call mpi_barrier(mpi_comm_world,ierror)
  
  !   5.  tempa --> updated ibuf --> jbuf --> write out
  
    allocate(tempb(itotsub,kbegin(mype):kend(mype)))
    allocate(temp1(im,jm),itemp1(im,jm),temp1u(im+1,jm),temp1v(im,jm+1))
    do ifld=kbegin(mype),kend(mype)
       if((ifld==i_sst).and..not.update_regsfc) cycle
       if((ifld==i_tsk).and..not.(update_regsfc .or. l_gsd_soilTQ_nudge)) cycle
       if(igtype(ifld) == 1) then
          call wrf_mass_guess%move_ibuf_hg(ibuf(1,ifld),temp1,im+1,jm+1,im,jm)
          if(ifld==i_mu) then
             temp1=temp1+mub+pt_regional_single
          end if
          call fill_mass_grid2t(temp1,im,jm,tempb(1,ifld),2)
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
          if( ifld==i_tsk .or. ifld==i_soilt1 .or. & 
             (ifld >=i_smois .and. ifld <=i_smois+ksize-1) .or. &
             (ifld >=i_tslb .and. ifld <=i_tslb+ksize-1) ) then 
  ! for 2X soil nudging
             i_snowT_check=0
             if(ifld==i_tsk) i_snowT_check=1
             if(ifld==i_soilt1) i_snowT_check=4
             if(ifld==i_tslb) i_snowT_check=3
             if(ifld >=i_smois .and. ifld <=i_smois+ksize-1) i_snowT_check=2
             call unfill_mass_grid2t_ldmk(tempa(1,ifld),im,jm,temp1,landmask, &
                                          snow,seaice,t1st2d,i_snowT_check)
  ! make sure soil moisture is larger than soilmoistmin (whih is 0.002 (sand)).
             if(ifld >=i_smois .and. ifld <=i_smois+ksize-1) then
                do i=1,im
                   do j=1,jm
                      temp1(i,j) = max(temp1(i,j),soilmoistmin)
                   end do
                end do
             endif
          else
             call unfill_mass_grid2t(tempa(1,ifld),im,jm,temp1)
          endif
          if(ifld==i_mu) then
             temp1=temp1-mub-pt_regional_single
          end if
          call this%move_hg_ibuf(temp1,ibuf(1,ifld),im+1,jm+1,im,jm)
       else if(igtype(ifld) == 2) then
          call wrf_mass_guess%move_ibuf_hg(ibuf(1,ifld),temp1u,im+1,jm+1,im+1,jm)
          call fill_mass_grid2u(temp1u,im,jm,tempb(1,ifld),2)
          tempa(:,ifld)=tempa(:,ifld)-tempb(:,ifld)
          call unfill_mass_grid2u(tempa(1,ifld),im,jm,temp1u)
          call this%move_hg_ibuf(temp1u,ibuf(1,ifld),im+1,jm+1,im+1,jm)
       else if(igtype(ifld) == 3) then
          call wrf_mass_guess%move_ibuf_hg(ibuf(1,ifld),temp1v,im+1,jm+1,im,jm+1)
          call fill_mass_grid2v(temp1v,im,jm,tempb(1,ifld),2)
          tempa(:,ifld)=tempa(:,ifld)-tempb(:,ifld)
          call unfill_mass_grid2v(tempa(1,ifld),im,jm,temp1v)
          call this%move_hg_ibuf(temp1v,ibuf(1,ifld),im+1,jm+1,im,jm+1)
       end if
    end do
  
  !           finally write ibuf back out ( ibuf --> jbuf -->  mpi_file_write )
  
  !                                    write temps
    if(kord(i_t)/=1) then
       allocate(jbuf(im,lm,jbegin(mype):min(jend(mype),jm)))
       call this%transfer_ibuf2jbuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                          jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im+1,jm+1,i_t,i_t+lm-1)
       this_offset=offset(i_t)+(jbegin(mype)-1)*4*im*lm
       this_length=(jend(mype)-jbegin(mype)+1)*im*lm
       if(byte_swap) then
          num_swap=this_length
          call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
       end if
       call mpi_file_write_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
       deallocate(jbuf)
    end if
  
  !                                    write q
    if(kord(i_q)/=1) then
       allocate(jbuf(im,lm,jbegin(mype):min(jend(mype),jm)))
       call this%transfer_ibuf2jbuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                          jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im+1,jm+1,i_q,i_q+lm-1)
       this_offset=offset(i_q)+(jbegin(mype)-1)*4*im*lm
       this_length=(jend(mype)-jbegin(mype)+1)*im*lm
       if(byte_swap) then
          num_swap=this_length
          call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
       end if
       call mpi_file_write_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
       deallocate(jbuf)
    end if
  
  !                                    write u
    if(kord(i_u)/=1) then
       allocate(jbuf(im+1,lm,jbegin(mype):min(jend(mype),jm)))
       call this%transfer_ibuf2jbuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                          jbegin,jend,kbegin,kend,mype,npe,im+1,jm,lm,im+1,jm+1,i_u,i_u+lm-1)
       this_offset=offset(i_u)+(jbegin(mype)-1)*4*(im+1)*lm
       this_length=(jend(mype)-jbegin(mype)+1)*(im+1)*lm
       if(byte_swap) then
          num_swap=this_length
          call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
       end if
       call mpi_file_write_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
       deallocate(jbuf)
    end if
  
  !                                    write v
    if(kord(i_v)/=1) then
       jend2=jend
       jend2(npe-1)=jend2(npe-1)+1
       allocate(jbuf(im,lm,jbegin(mype):jend2(mype)))
       call this%transfer_ibuf2jbuf(jbuf,jbegin(mype),jend2(mype),ibuf,kbegin(mype),kend(mype), &
                          jbegin,jend2,kbegin,kend,mype,npe,im,jm+1,lm,im+1,jm+1,i_v,i_v+lm-1)
       this_offset=offset(i_v)+(jbegin(mype)-1)*4*im*lm
       this_length=(jend2(mype)-jbegin(mype)+1)*im*lm
       if(byte_swap) then
          num_swap=this_length
          call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
       end if
       call mpi_file_write_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
       deallocate(jbuf)
    end if
  
    if(l_gsd_soilTQ_nudge) then
       lm=nsig_soil
  !                                    write smois
       if(kord(i_smois)/=1) then
          allocate(jbuf(im,lm,jbegin(mype):min(jend(mype),jm)))
          call this%transfer_ibuf2jbuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                           jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im+1,jm+1,i_smois,i_smois+lm-1)
          this_offset=offset(i_smois)+(jbegin(mype)-1)*4*im*lm
          this_length=(jend(mype)-jbegin(mype)+1)*im*lm
          if(byte_swap) then
             num_swap=this_length
             call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
          end if
          call mpi_file_write_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
          deallocate(jbuf)
       end if
  
  !                                    write tslb
       if(kord(i_tslb)/=1) then
          allocate(jbuf(im,lm,jbegin(mype):min(jend(mype),jm)))
          call this%transfer_ibuf2jbuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                           jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im+1,jm+1,i_tslb,i_tslb+lm-1)
          this_offset=offset(i_tslb)+(jbegin(mype)-1)*4*im*lm
          this_length=(jend(mype)-jbegin(mype)+1)*im*lm
          if(byte_swap) then
             num_swap=this_length
             call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
          end if
          call mpi_file_write_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
          deallocate(jbuf)
       end if
  
       lm=nsig
    endif
  
  !  write hydrometeors
    if(l_hydrometeor_bkio .and. n_actual_clouds>0) then
  !                                    write qc
       if(kord(i_qc)/=1) then
          allocate(jbuf(im,lm,jbegin(mype):min(jend(mype),jm)))
          call this%transfer_ibuf2jbuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                           jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im+1,jm+1,i_qc,i_qc+lm-1)
          this_offset=offset(i_qc)+(jbegin(mype)-1)*4*im*lm
          this_length=(jend(mype)-jbegin(mype)+1)*im*lm
          if(byte_swap) then
             num_swap=this_length
             call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
          end if
          call mpi_file_write_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
          deallocate(jbuf)
       end if
  
  !                                    write qr
       if(kord(i_qr)/=1) then
          allocate(jbuf(im,lm,jbegin(mype):min(jend(mype),jm)))
          call this%transfer_ibuf2jbuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                           jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im+1,jm+1,i_qr,i_qr+lm-1)
          this_offset=offset(i_qr)+(jbegin(mype)-1)*4*im*lm
          this_length=(jend(mype)-jbegin(mype)+1)*im*lm
          if(byte_swap) then
             num_swap=this_length
             call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
          end if
          call mpi_file_write_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
          deallocate(jbuf)
       end if
  
  !                                    write qi
       if(kord(i_qi)/=1) then
          allocate(jbuf(im,lm,jbegin(mype):min(jend(mype),jm)))
          call this%transfer_ibuf2jbuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                           jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im+1,jm+1,i_qi,i_qi+lm-1)
          this_offset=offset(i_qi)+(jbegin(mype)-1)*4*im*lm
          this_length=(jend(mype)-jbegin(mype)+1)*im*lm
          if(byte_swap) then
             num_swap=this_length
             call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
          end if
          call mpi_file_write_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
          deallocate(jbuf)
       end if
  
  !                                    write qs
       if(kord(i_qs)/=1) then
          allocate(jbuf(im,lm,jbegin(mype):min(jend(mype),jm)))
          call this%transfer_ibuf2jbuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                           jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im+1,jm+1,i_qs,i_qs+lm-1)
          this_offset=offset(i_qs)+(jbegin(mype)-1)*4*im*lm
          this_length=(jend(mype)-jbegin(mype)+1)*im*lm
          if(byte_swap) then
             num_swap=this_length
             call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
          end if
          call mpi_file_write_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
          deallocate(jbuf)
       end if
  
  !                                    write qg
       if(kord(i_qg)/=1) then
          allocate(jbuf(im,lm,jbegin(mype):min(jend(mype),jm)))
          call this%transfer_ibuf2jbuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                           jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im+1,jm+1,i_qg,i_qg+lm-1)
          this_offset=offset(i_qg)+(jbegin(mype)-1)*4*im*lm
          this_length=(jend(mype)-jbegin(mype)+1)*im*lm
          if(byte_swap) then
             num_swap=this_length
             call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
          end if
          call mpi_file_write_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
          deallocate(jbuf)
       end if
  
  !                                    write qnr
       if(kord(i_qnr)/=1) then
          allocate(jbuf(im,lm,jbegin(mype):min(jend(mype),jm)))
          call this%transfer_ibuf2jbuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                           jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im+1,jm+1,i_qnr,i_qnr+lm-1)
          this_offset=offset(i_qnr)+(jbegin(mype)-1)*4*im*lm
          this_length=(jend(mype)-jbegin(mype)+1)*im*lm
          if(byte_swap) then
             num_swap=this_length
             call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
          end if
          call mpi_file_write_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
          deallocate(jbuf)
       end if
  
  !                                    write qni
       if(kord(i_qni)/=1) then
          allocate(jbuf(im,lm,jbegin(mype):min(jend(mype),jm)))
          call this%transfer_ibuf2jbuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                           jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im+1,jm+1,i_qni,i_qni+lm-1)
          this_offset=offset(i_qni)+(jbegin(mype)-1)*4*im*lm
          this_length=(jend(mype)-jbegin(mype)+1)*im*lm
          if(byte_swap) then
             num_swap=this_length
             call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
          end if
          call mpi_file_write_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
          deallocate(jbuf)
       end if
  
  !                                    write qnc
       if(kord(i_qnc)/=1) then
          allocate(jbuf(im,lm,jbegin(mype):min(jend(mype),jm)))
          call this%transfer_ibuf2jbuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                           jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im+1,jm+1,i_qnc,i_qnc+lm-1)
          this_offset=offset(i_qnc)+(jbegin(mype)-1)*4*im*lm
          this_length=(jend(mype)-jbegin(mype)+1)*im*lm
          if(byte_swap) then
             num_swap=this_length
             call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
          end if
          call mpi_file_write_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
          deallocate(jbuf)
       end if
  
  !                                    write tt
       if(kord(i_tt)/=1) then
          allocate(jbuf(im,lm,jbegin(mype):min(jend(mype),jm)))
          call this%transfer_ibuf2jbuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                           jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im+1,jm+1,i_tt,i_tt+lm-1)
          this_offset=offset(i_tt)+(jbegin(mype)-1)*4*im*lm
          this_length=(jend(mype)-jbegin(mype)+1)*im*lm
          if(byte_swap) then
             num_swap=this_length
             call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
          end if
          call mpi_file_write_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length,mpi_integer4,status,ierror)
          deallocate(jbuf)
       end if
  
    end if ! l_hydrometeor_bkio
  !---------------------- write surface files last
    do k=kbegin(mype),kend(mype)
       if(kdim(k)==1.or.kord(k)==1) then
          if(igtype(k)==1) call this%contract_ibuf(ibuf(1,k),im  ,jm     ,im+1,jm+1)
          if(igtype(k)==2) call this%contract_ibuf(ibuf(1,k),im+1,jm     ,im+1,jm+1)
          if(igtype(k)==3) call this%contract_ibuf(ibuf(1,k),im  ,jm+1,im+1,jm+1)
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
    deallocate(mub)
    deallocate(landmask)
    deallocate(snow)
    deallocate(seaice)
    deallocate(t1st2d)
    deallocate(tempa)
    deallocate(tempb)
    deallocate(temp1)
    deallocate(itemp1)
    deallocate(temp1u)
    deallocate(temp1v)
    call destroy_cld_grids
    call mpi_file_close(mfcst,ierror)
  
  end subroutine wrwrfmassa_binary_wrf
  
  subroutine generic_sub2grid(this,all_loc,tempa,kbegin_loc,kend_loc,kbegin,kend,mype,num_fields)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    generic_sub2grid   converts from subdomains to full horizontal grid
  !   prgmmr: parrish          org: np22                date: 2004-11-29
  !
  ! abstract: variation on subroutine sub2grid, with more general distribution of variables
  !              along the k index.
  !
  ! program history log:
  !   2004-02-03  kleist, new mpi strategy
  !   2004-05-06  derber
  !   2004-07-15  treadon - handle periodic subdomains
  !   2004-07-28  treadon - add only on use declarations; add intent in/out
  !   2004-10-26  kleist - u,v removed; periodicity accounted for only in
  !               sub2grid routine if necessary
  !   2004-11-29  parrish - adapt sub2grid for related use with mpi io.
  !
  !   input argument list:
  !     all_loc  - input grid values in vertical subdomain mode
  !     kbegin_loc - starting k index for tempa on local processor
  !     kend_loc   - ending k index for tempa on local processor
  !     kbegin     - starting k indices for tempa for all processors
  !     kend       - ending k indices for tempa for all processors
  !     mype       - local processor number
  !     num_fields - total range of k index (1 <= k <= num_fields)
  !
  !   output argument list:
  !     tempa    - output grid values in horizontal slab mode.
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$
    use mpimod, only: ierror,mpi_comm_world,mpi_real4,npe
    use gridmod, only: ijn,itotsub,lat1,lon1
    use kinds, only: r_single,i_kind
    implicit none
  
    class(wrwrfmassa_class), intent(inout) :: this 
    integer(i_kind),intent(in   ) :: kbegin_loc,kend_loc,mype,num_fields
    integer(i_kind),intent(in   ) :: kbegin(0:npe),kend(0:npe-1)
    real(r_single) ,intent(  out) :: tempa(itotsub,kbegin_loc:kend_loc)
    real(r_single) ,intent(in   ) :: all_loc(lat1*lon1*num_fields)
  
    integer(i_kind) k
    integer(i_kind) sendcounts(0:npe-1),sdispls(0:npe),recvcounts(0:npe-1),rdispls(0:npe)
  
  ! first get alltoallv indices
  
    sdispls(0)=0
    do k=0,npe-1
       sendcounts(k)=ijn(k+1)*(kend_loc-kbegin_loc+1)
       sdispls(k+1)=sdispls(k)+sendcounts(k)
    end do
    rdispls(0)=0
    do k=0,npe-1
       recvcounts(k)=ijn(mype+1)*(kend(k)-kbegin(k)+1)
       rdispls(k+1)=rdispls(k)+recvcounts(k)
    end do
  
    call mpi_alltoallv(all_loc,recvcounts,rdispls,mpi_real4, &
                  tempa,sendcounts,sdispls,mpi_real4,mpi_comm_world,ierror)
  
    call this%reorder_s(tempa,kend_loc-kbegin_loc+1)
  
  end subroutine generic_sub2grid
  
    subroutine reorder_s(work,k_in)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    reorder_s 
  !   prgmmr: parrish          org: np22                date: 2004-11-29
  !
  ! abstract:  adapt reorder to work with single precision
  !
  ! program history log:
  !   2004-01-25  kleist
  !   2004-05-14  kleist, documentation
  !   2004-07-15  todling, protex-complaint prologue
  !   2004-11-29  adapt reorder to work with single precision
  !   2008-03-27  safford - add standard doc block, rm unused vars
  !
  !   input argument list:
  !     k_in     - number of levs in work array
  !     work     - array to reorder
  !
  !   output argument list:
  !     work     - array to reorder
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm rs/6000 sp; sgi origin 2000; compaq/hp
  !
  !$$$
  
      use mpimod, only: npe
      use kinds, only: r_single,i_kind
      use constants, only: zero
      use gridmod, only: ijn,itotsub
      implicit none
  
      integer(i_kind)                       , intent(in   ) :: k_in    ! number of levs in work array
  
      real(r_single),dimension(itotsub*k_in), intent(inout) :: work ! array to reorder
  
  
      integer(i_kind) iloc,iskip,i,k,n
      real(r_single),dimension(itotsub,k_in):: temp
  
  ! Zero out temp array
      do k=1,k_in
         do i=1,itotsub
            temp(i,k)=zero
         end do
      end do
  
  ! Load temp array in desired order
      do k=1,k_in
         iskip=0
         iloc=0
         do n=1,npe
            if (n/=1) then
               iskip=iskip+ijn(n-1)*k_in
            end if
            do i=1,ijn(n)
               iloc=iloc+1
               temp(iloc,k)=work(i + iskip + &
                        (k-1)*ijn(n))
            end do
         end do
      end do
  
  ! Load the temp array back into work
      iloc=0
      do k=1,k_in
         do i=1,itotsub
            iloc=iloc+1
            work(iloc)=temp(i,k)
         end do
      end do
  
      return
    end subroutine reorder_s
  
  subroutine contract_ibuf(ibuf,im,jm,imp,jmp)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    contract_ibuf    contract array in place
  !   prgmmr: parrish          org: np22                date: 2004-11-29
  !
  ! abstract: contract array in place from imp,jmp to im,jm
  !
  ! program history log:
  !   2004-11-29  parrish
  !   2005-06-10  devenyi/treadon - remove ii=ii+1 from itemp(i,j)=0 loop
  !   2007-04-12  parrish - replace im+1, jm+1 with inputs imp, jmp to allow
  !                           for use with u and v fields, where im=imp or jm=jmp
  !
  !   input argument list:
  !     ibuf     - input grid values in imp,jmp
  !     im       - first grid index
  !     jm       - second grid index
  !     imp
  !     jmp
  !
  !   output argument list:
  !     ibuf     - output grid values in im,jm
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$
  
  !   field of dim imp*jmp needs to be converted to field of im*jm
  
    use kinds, only: i_long,i_kind
    implicit none
  
    integer(i_kind),intent(in   ) :: im,jm,imp,jmp
    integer(i_long),intent(inout) :: ibuf(imp*jmp)
  
    integer(i_kind) i,ii,j
    integer(i_long) itemp(imp,jmp)
  
    do j=1,jmp
       do i=1,imp
          itemp(i,j)=0_i_long
       end do
    end do
    ii=0
    do j=1,jmp
       do i=1,imp
          ii=ii+1
          itemp(i,j)=ibuf(ii)
       end do
    end do
  
    ibuf=0_i_long
    ii=0
    do j=1,jm
       do i=1,im
          ii=ii+1
          ibuf(ii)=itemp(i,j)
       end do
    end do
  
  end subroutine contract_ibuf
  
  subroutine transfer_ibuf2jbuf(jbuf,jbegin_loc,jend_loc,ibuf,kbegin_loc,kend_loc, &
                       jbegin,jend,kbegin,kend,mype,npe,im_jbuf,jm_jbuf,lm_jbuf, &
                       im_ibuf,jm_ibuf,k_start,k_end)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    transfer_jbuf2ibuf   flip from ijk to ikj
  !   prgmmr: parrish          org: np22                date: 2004-11-29
  !
  ! abstract: redistribute 3-d field from ijk order to ikj order across processors
  !
  ! program history log:
  !   2004-11-29  parrish
  !   2005-02-16  todling, replaced "use mpi" by mpimod
  !
  !   input argument list:
  !     ibuf     - input grid redistributed to ijk order (full i, full j, k_start <= k <= k_end)
  !     jbegin_loc - local processor starting j index
  !     jend_loc   - local processor ending j index
  !     kbegin_loc - local processor starting k index
  !     kend_loc   - local processor ending k index
  !     jbegin     - starting j indices for all processors
  !     jend       - ending j indices for all processors
  !     kbegin     - starting k indices for all processors
  !     kend       - ending k indices for all processors
  !     mype       - local processor number
  !     npe        - total number of processors
  !     im_jbuf    - full range of i index for jbuf array
  !     jm_jbuf    - full range of j index for jbuf array
  !     lm_jbuf    - full range of k index for jbuf array
  !     im_ibuf    - full range of i index for ibuf array
  !     jm_ibuf    - full range of j index for ibuf array
  !     k_start    - beginning index for range of k in ibuf array
  !     k_end      - ending index for range of k in ibuf array
  !
  !   output argument list:
  !     jbuf     - output grid values distributed as all ik and a range of j on each processor
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$
  
  !  flip around from ijk to ikj, moving result from ibuf to jbuf
  
    use mpimod, only: mpi_comm_world,mpi_integer4
    use kinds, only: i_long,i_kind
    implicit none
  
    integer(i_kind),intent(in   ) :: jbegin_loc,jend_loc,kbegin_loc,kend_loc,mype,npe,im_jbuf,jm_jbuf,lm_jbuf
    integer(i_kind),intent(in   ) :: im_ibuf,jm_ibuf,k_start,k_end
  
    integer(i_long),intent(  out) :: jbuf(im_jbuf,lm_jbuf,jbegin_loc:jend_loc)
    integer(i_long),intent(in   ) :: ibuf(im_ibuf,jm_ibuf,kbegin_loc:kend_loc)
    integer(i_kind),intent(in   ) :: jbegin(0:npe),jend(0:npe-1)
    integer(i_kind),intent(in   ) :: kbegin(0:npe),kend(0:npe-1)
  
    integer(i_long) sendbuf(im_jbuf*lm_jbuf*(min(jend_loc,jm_jbuf)-jbegin_loc+1))
    integer(i_long) recvbuf(im_jbuf*jm_jbuf*(kend_loc-kbegin_loc+1))
    integer(i_long) recvcounts(0:npe-1),displs(0:npe)
    integer(i_kind) i,ipe,j,ierror,k,n,ii,k_t_start,k_t_end,sendcount
  
    do ipe=0,npe-1
       k_t_start=max(k_start,kbegin(ipe))
       k_t_end=  min(k_end,kend(ipe))
       if(k_t_end < k_t_start) cycle
  
       displs(0)=0_i_long
       do i=0,npe-1
          recvcounts(i)=im_jbuf*(k_t_end-k_t_start+1_i_long)*(jend(i)-jbegin(i)+1_i_long)
          displs(i+1)=displs(i)+recvcounts(i)
       end do
  
       if(ipe==mype) then
          ii=0
          do n=0,npe-1
             do k=k_t_start,k_t_end
                do j=jbegin(n),jend(n)
                   do i=1,im_jbuf
                      ii=ii+1
                      recvbuf(ii)=ibuf(i,j,k)
                   end do
                end do
             end do
          end do
       end if
       ii=0
       do k=k_t_start,k_t_end
          do j=jbegin_loc,jend_loc
             do i=1,im_jbuf
                ii=ii+1
             end do
          end do
       end do
       sendcount=ii
       call mpi_scatterv(recvbuf,recvcounts,displs,mpi_integer4, &
                         sendbuf,sendcount,mpi_integer4,ipe,mpi_comm_world,ierror)
       ii=0
       do k=k_t_start,k_t_end
          do j=jbegin_loc,jend_loc
             do i=1,im_jbuf
                ii=ii+1
                jbuf(i,k-k_start+1,j)=sendbuf(ii)
             end do
          end do
       end do
  
  
    end do
  
  end subroutine transfer_ibuf2jbuf
  
  subroutine move_hg_ibuf(temp1,ibuf,im_buf,jm_buf,im_out,jm_out)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    move_hg_ibuf  copy from one array to another
  !   prgmmr: parrish          org: np22                date: 2004-11-29
  !
  ! abstract: copy from one array to another
  !
  ! program history log:
  !   2004-11-29  parrish
  !
  !   input argument list:
  !     temp1    - input grid values
  !     im_buf   - first index of input array buf
  !     jm_buf   - second index of input array buf
  !     im_out   - first index of output array temp1
  !     jm_out   - second index of output array temp1
  !
  !   output argument list:
  !     ibuf      - output grid values
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$
  
  !        cp temp1 to ibuf
  
    use kinds, only: r_single,i_kind,i_long
    use constants, only: zero_ilong
    implicit none
  
    integer(i_kind),intent(in   ) :: im_buf,jm_buf,im_out,jm_out
    real(r_single) ,intent(in   ) :: temp1(im_out,jm_out)
    integer(i_long),intent(  out) :: ibuf(im_buf,jm_buf)
  
    integer(i_kind) i,j
  
    ibuf=zero_ilong
    do j=1,jm_out
       do i=1,im_out
          ibuf(i,j)=transfer(temp1(i,j),zero_ilong)
       end do
    end do
  
  end subroutine move_hg_ibuf
  
  subroutine wrwrfmassa_netcdf_wrf(this,mype)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    wrwrfmassa              write out wrf MASS restart file
  !   prgmmr: parrish          org: np22                date: 2004-06-23
  !
  ! abstract:  read wrf MASS guess restart interface file, add analysis
  !            increment, and write out wrf MASS analysis restart 
  !            interface file.
  !
  ! program history log:
  !   2004-06-23  parrish, document
  !   2004-08-03  treadon - add only to module use, add intent in/out
  !   2006-02-15  treadon - convert specific humidity to moisture mixing ratio
  !   2006-03-07  treadon - convert virtual temperature to potential temperature
  !   2006-04-06  middlecoff - changed iog  from 11 to lendian_in
  !                            changed ioan from 51 to lendian_out
  !   2006-07-28  derber  - include sensible temperature
  !   2006-07-31  kleist - change to use ges_ps instead of lnps
  !   2008-03-27  safford - rm unused vars and uses
  !   2008-12-05  todling - adjustment for dsfct time dimension addition
  !   2010-03-29  hu     - add code to gether cloud/hydrometeor fields and write out
  !   2010-04-01  treadon - move strip_single to gridmod
  !   2011-04-29  todling - introduce MetGuess and wrf_mass_guess_mod
  !   2011-09-20  hclin   - added 15 wrfchem/gocart fields for aod
  !   2012-04-13  whitaker - don't call GSI_BundleGetPointer if n_actual_clouds = 0
  !   2013-10-19  todling - metguess now holds background
  !   2013-10-24  todling - general interface to strip
  !   2014-03-12  hu     - add code to read ges_q2 (2m Q), 
  !                               Qnr(rain number concentration), 
  !                               and nsoil (number of soil levels)
  !   2015-01-13  ladwig - add code to read Qni (cloud water number
  !                               concentration)
  !   2017-03-23  Hu     - add code to use hybrid vertical coodinate in WRF MASS
  !                        core
  !   2017-03-30  g.zhao - special treatment to final analysis of qr/qs/qg, re-set
  !                        to zero value if no update or only trivial update
  !                         assign different tiny values to qr/qs/qg (~0dbz)
  !                         (match the configuration in read_wrf_mass_guess)
  !                         if T > 274.15
  !                                qr_min=2.9E-6
  !                                qs_min=0.0E-6
  !                                qg_min=3.1E-7
  !                         if T < 274.15  and  T > 272.15
  !                                qr_min=2.0E-6
  !                                qs_min=1.3E-7
  !                                qg_min=3.1E-7
  !                         if T < 272.15
  !                                qr_min=0.0E-6
  !                                qs_min=6.3E-6
  !                                qg_min=3.1E-7
  !   2018-02-xx  g.zhao - only reset the value for log transformed qx
  !                        for regular qx, the non-zero tiny value is applied to
  !                        qx on obs point in setupdbz
  !   2018-10-23   C.Liu   add w 
  !   2019-04-18  cTong  - add code of treatments for logarithmic q conversion
  !   2019-05-30  cTong  - add w analysis output 
  !   2022-03-15  Hu  change all th2 to t2m and convert 2m temperature 
  !                        from sensible to potentionl temperature for writing
  !   2022-03-15  Hu  add a check to remove the negative moisture if rh>4%
  !                   for the lowest 3 model level
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
    use guess_grids, only: ntguessfc,ntguessig,ifilesig,dsfct,&
         ges_tsen, ges_w_btlev
    use wrf_mass_guess_mod, only: ges_tten
    use mpimod, only: mpi_comm_world,ierror,mpi_real4
    use gridmod, only: pt_ll,eta1_ll,lat2,iglobal,itotsub,update_regsfc,&
         lon2,nsig,nsig_soil,lon1,lat1,nlon_regional,nlat_regional,ijn,displs_g,&
         aeta1_ll,strip,eta2_ll,aeta2_ll
    use constants, only: one,zero_single,rd_over_cp_mass,one_tenth,r10,r100
    use constants, only: soilmoistmin
    use gsi_io, only: lendian_in, lendian_out
    use rapidrefresh_cldsurf_mod, only: l_hydrometeor_bkio,l_gsd_soilTQ_nudge,&
         i_use_2mq4b,i_use_2mt4b
    use chemmod, only: laeroana_gocart,wrf_pm2_5
    use gsi_bundlemod, only: GSI_BundleGetPointer
    use gsi_metguess_mod, only: gsi_metguess_get,GSI_MetGuess_Bundle
    use gsi_chemguess_mod, only: GSI_ChemGuess_Bundle, gsi_chemguess_get
    use mpeu_util, only: die
    use wrf_vars_mod, only : w_exist, dbz_exist
    use obsmod,only: if_model_dbz
    use constants, only: zero,r0_01
    use directDA_radaruse_mod, only: l_use_cvpqx, cvpqx_pval, cld_nt_updt
    use directDA_radaruse_mod, only: l_use_dbz_directDA
    use directDA_radaruse_mod, only: init_mm_qnr
    use directDA_radaruse_mod, only: l_cvpnr, cvpnr_pval

    implicit none
  
  ! Declare passed variables
    class(wrwrfmassa_class), intent(inout) :: this 
    integer(i_kind),intent(in   ) :: mype
  
  ! Declare local parameters
    character(len=*),parameter::myname='wrwrfmassa_netcdf'
    real(r_kind),parameter:: r225=225.0_r_kind
    integer(i_kind), parameter :: nlvl_qcheck=3
    real(r_kind),parameter:: D608=0.608_r_kind
  
  ! Declare local variables
    integer(i_kind) im,jm,lm
    real(r_single),allocatable::temp1(:),temp1u(:),temp1v(:),tempa(:),tempb(:)
    real(r_single),allocatable::all_loc(:,:,:)
    real(r_single),allocatable::strp(:)
    real(r_single),allocatable::landmask(:),snow(:),seaice(:),t1st2d(:),pt2t(:)
    real(r_single),allocatable::qs1st2d(:,:),sfcprs(:)
    real(r_kind),allocatable::tmp2da(:,:),tmp2db(:,:),tmp2dc(:,:)
    character(6) filename
    integer(i_kind) i,j,k,kt,kq,ku,kv,it,i_psfc,i_t,i_q,i_u,i_v,i_w,i_dbz,ij
    integer(i_kind) i_qc,i_qi,i_qr,i_qs,i_qg,i_qnr,i_qni,i_qnc
    integer(i_kind) kqc,kqi,kqr,kqs,kqg,kqnr,kqni,kqnc,i_tt,ktt,kw,kdbz
    integer(i_kind) i_sst,i_skt,i_th2,i_q2,i_soilt1,i_tslb,i_smois,ktslb,ksmois
    integer(i_kind) :: iv, n_gocart_var,i_snowT_check
    integer(i_kind),allocatable :: i_chem(:), kchem(:)
    integer(i_kind) num_mass_fields,num_all_fields,num_all_pad,num_mass_fields_base
    integer(i_kind) regional_time0(6),nlon_regional0,nlat_regional0,nsig0,nsoil
    integer(i_kind) n_actual_clouds,ier,istatus
    real(r_kind) psfc_this,psfc_this_dry
    real(r_kind),dimension(lat2,lon2):: q_integral,q_integralc4h
    real(r_kind) deltasigma,deltasigmac4h
    real(r_kind):: work_prsl,work_prslk
    real(r_single) pt0
    real(r_single) aeta10(nsig),eta10(nsig+1),aeta20(nsig),eta20(nsig+1)
    real(r_single) glon0(nlon_regional,nlat_regional),glat0(nlon_regional,nlat_regional)
    real(r_single) dx_mc0(nlon_regional,nlat_regional),dy_mc0(nlon_regional,nlat_regional)
  
    real(r_kind), pointer :: ges_ps(:,:  )=>NULL()
    real(r_kind), pointer :: ges_tsk(:,:)=>NULL()
    real(r_kind), pointer :: ges_t2m(:,:)=>NULL()
    real(r_kind), pointer :: ges_q2(:,:)=>NULL()
    real(r_kind), pointer :: ges_soilt1(:,:)=>NULL()
    real(r_kind), pointer :: ges_tslb_it(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_smois_it(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_u (:,:,:)=>NULL()
    real(r_kind), pointer :: ges_v (:,:,:)=>NULL()
    real(r_kind), pointer :: ges_w (:,:,:)=>NULL()
    real(r_kind), pointer :: ges_q (:,:,:)=>NULL()
    real(r_kind), pointer :: ges_qc(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_qi(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_qr(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_qs(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_qg(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_qnr(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_qni(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_qnc(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_dbz(:,:,:)=>NULL()
  
    real(r_kind), pointer :: ges_sulf (:,:,:)=>NULL()
    real(r_kind), pointer :: ges_bc1  (:,:,:)=>NULL()
    real(r_kind), pointer :: ges_bc2  (:,:,:)=>NULL()
    real(r_kind), pointer :: ges_oc1  (:,:,:)=>NULL()
    real(r_kind), pointer :: ges_oc2  (:,:,:)=>NULL()
    real(r_kind), pointer :: ges_dust1(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_dust2(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_dust3(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_dust4(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_dust5(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_seas1(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_seas2(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_seas3(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_seas4(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_p25  (:,:,:)=>NULL()
    real(r_kind), pointer :: ges_pm2_5  (:,:,:)=>NULL()

    associate( this => this ) ! eliminates warning for unused dummy argument needed for binding
    end associate
    it=ntguessig
  
  ! Inquire about cloud guess fields
    call gsi_metguess_get('clouds::3d',n_actual_clouds,istatus)
    if (n_actual_clouds>0) then
  !    get pointer to relevant instance of cloud-related backgroud
       ier=0
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'ql', ges_qc, istatus );ier=ier+istatus
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qi', ges_qi, istatus );ier=ier+istatus
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qr', ges_qr, istatus );ier=ier+istatus
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qs', ges_qs, istatus );ier=ier+istatus
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qg', ges_qg, istatus );ier=ier+istatus
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qnr',ges_qnr,istatus );ier=ier+istatus
       if ( .not. l_use_dbz_directDA ) then ! direct reflectivity DA does not use qni and qnc
          call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qni',ges_qni,istatus );ier=ier+istatus
          call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qnc',ges_qnc,istatus );ier=ier+istatus
       end if
       if (ier/=0) n_actual_clouds=0
    end if
  
    im=nlon_regional
    jm=nlat_regional
    lm=nsig
  
    num_mass_fields_base=2+4*lm + 1
    num_mass_fields=num_mass_fields_base
    if ( l_use_dbz_directDA ) then
!    The 7 3D cloud analysis fields are: ql,qi,qr,qs,qg,qnr,tt
       if(l_hydrometeor_bkio .and. n_actual_clouds>0) num_mass_fields=num_mass_fields + 7*lm
    else
!    The 9 3D cloud analysis fields are: ql,qi,qr,qs,qg,qnr,qni,qnc,tt
       if(l_hydrometeor_bkio .and. n_actual_clouds>0) num_mass_fields=num_mass_fields + 9*lm
    end if
    if(l_gsd_soilTQ_nudge) num_mass_fields=num_mass_fields+2*nsig_soil+1
    if(i_use_2mt4b > 0 ) num_mass_fields=num_mass_fields+2
    if(i_use_2mt4b <= 0 .and. i_use_2mq4b > 0) num_mass_fields=num_mass_fields+1
    if ( laeroana_gocart ) then
       call gsi_chemguess_get ( 'aerosols::3d', n_gocart_var, ier )
       if ( n_gocart_var > 0 ) then
          num_mass_fields = num_mass_fields + n_gocart_var*lm
          allocate(i_chem(n_gocart_var))
          allocate(kchem(n_gocart_var))
       else
          laeroana_gocart = .false.
       endif
    endif
  
    if ( wrf_pm2_5 ) then
       num_mass_fields = num_mass_fields + lm
       allocate(i_chem(1))
       allocate(kchem(1))
    endif

    if(w_exist) num_mass_fields = num_mass_fields +lm+1
    if(dbz_exist.and.if_model_dbz) num_mass_fields = num_mass_fields +lm
    
    num_all_fields=num_mass_fields
    num_all_pad=num_all_fields
    allocate(all_loc(lat2,lon2,num_all_pad))
    allocate(strp(lat1*lon1))
  
    i_psfc=1
    i_t=2
    i_q=i_t+lm
    i_u=i_q+lm
    i_v=i_u+lm
    if(w_exist)then
      i_w=i_v+lm
      i_sst=i_w+lm+1
    else
      i_sst=i_v+lm
    endif
    if(i_use_2mt4b > 0) then
       i_th2=i_sst+1
    else
       i_th2=i_sst
    endif
    if(l_gsd_soilTQ_nudge) then
       i_tslb=i_th2+1
       i_smois=i_tslb+nsig_soil
       i_soilt1=i_smois+nsig_soil
       i_skt=i_soilt1+1
    else
       i_skt=i_th2+1
       i_tslb=0
       i_smois=0
       i_soilt1=0
    endif
    if(i_use_2mt4b > 0 .or. i_use_2mq4b > 0) then
       i_q2=i_skt+1
    else
       i_q2=i_skt
    endif
  
  ! for hydrometeors
    if(l_hydrometeor_bkio .and. n_actual_clouds>0) then
       i_qc=i_q2+1
       i_qr=i_qc+lm
       i_qs=i_qr+lm
       i_qi=i_qs+lm
       i_qg=i_qi+lm
       i_qnr=i_qg+lm
       if ( .not. l_use_dbz_directDA) then ! direct reflectivity DA does use qni and qnc
          i_qni=i_qnr+lm
          i_qnc=i_qni+lm
       end if
       if(dbz_exist.and.if_model_dbz)then
         i_dbz=i_qnc+lm
         i_tt=i_dbz+lm
       else
         if ( l_use_dbz_directDA) then ! direct reflectivity DA : qnr, tt
            i_tt=i_qnr+lm
         else                           
            i_tt=i_qnc+lm          ! ORG: qnc, tt
         end if
       end if
       if ( laeroana_gocart ) then
          do iv = 1, n_gocart_var
             i_chem(iv)=i_tt+(iv-1)*lm+1
          end do
       endif
  
       if ( wrf_pm2_5 ) then
          iv=1
          i_chem(iv)=i_tt+(iv-1)*lm+1
       endif
       
    else
       if ( laeroana_gocart) then
          do iv = 1, n_gocart_var
             i_chem(iv)=i_skt+(iv-1)*lm+1
          end do
       endif
  
       if ( wrf_pm2_5 ) then
          iv=1
          i_chem(iv)=i_skt+(iv-1)*lm+1
       endif
  
  
    endif
    
    allocate(temp1(im*jm),temp1u((im+1)*jm),temp1v(im*(jm+1)))
    if(mype==0) then
       allocate(landmask(im*jm),snow(im*jm),seaice(im*jm))
       allocate(t1st2d(im*jm),pt2t(im*jm))
       allocate(qs1st2d(im*jm,nlvl_qcheck))
       allocate(sfcprs(im*jm))
    endif
  
    if(mype == 0) write(6,*)' at 2 in wrwrfmassa'
  
    if(mype == 0) then
       write(filename,'("sigf",i2.2)')ifilesig(ntguessig)
       print *,'update ',trim(filename)
       open (lendian_in,file=filename,form='unformatted')
       open (lendian_out,file='siganl',form='unformatted')
       rewind lendian_in ; rewind lendian_out
    end if
  
  ! Convert analysis variables to MASS variables
  ! get pointer to relevant instance of cloud-related backgroud
    if (n_actual_clouds>0) then
       ier=0
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'ql', ges_qc, istatus );ier=ier+istatus
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qi', ges_qi, istatus );ier=ier+istatus
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qr', ges_qr, istatus );ier=ier+istatus
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qs', ges_qs, istatus );ier=ier+istatus
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qg', ges_qg, istatus );ier=ier+istatus
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qnr',ges_qnr,istatus );ier=ier+istatus
       if ( .not.l_use_dbz_directDA) then ! direct reflectivity does not use qni and qnc
          call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qni',ges_qni,istatus );ier=ier+istatus
          call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qnc',ges_qnc,istatus );ier=ier+istatus
       end if
       if(dbz_exist) &
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'dbz',ges_dbz,istatus );ier=ier+istatus
       if (ier/=0) then
           write(6,*)'READ_WRF_MASS_BINARY_GUESS: getpointer failed, cannot do cloud analysis'
           if (l_hydrometeor_bkio .and. n_actual_clouds>0) call stop2(999)
       endif
    endif
  
    ier=0
    call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'ps', ges_ps, istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'u' , ges_u , istatus );ier=ier+istatus
    call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'v' , ges_v , istatus );ier=ier+istatus
    if(w_exist)then
      call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'w' , ges_w , istatus );ier=ier+istatus
    endif
    call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'q' , ges_q , istatus );ier=ier+istatus
    if (ier/=0) then ! doesn't have to die - code can be generalized to bypass missing vars
        write(6,*)'wrwrfmassa_binary: getpointer failed, cannot retrieve ps,u,v,q'
        call stop2(999)
    endif
    
  ! Create all_loc from ges_*
    if(mype == 0) write(6,*)' at 3 in wrwrfmassa'
    all_loc=zero_single
    kt=i_t-1
    kq=i_q-1
    ku=i_u-1
    kv=i_v-1
    if(w_exist)kw=i_w-1
  ! for hydrometeors
    if(l_hydrometeor_bkio .and. n_actual_clouds>0) then
       kqc=i_qc-1
       kqi=i_qi-1
       kqr=i_qr-1
       kqs=i_qs-1
       kqg=i_qg-1
       kqnr=i_qnr-1
       if (.not.l_use_dbz_directDA) then ! direct reflectivity DA does not use qni and qnc
          kqni=i_qni-1
          kqnc=i_qnc-1
       end if
       if(dbz_exist.and.if_model_dbz)kdbz=i_dbz-1
       ktt=i_tt-1
    endif
    if ( laeroana_gocart ) then
       ier = 0
       call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'sulf',  ges_sulf,  istatus );ier=ier+istatus 
       call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'bc1',   ges_bc1,   istatus );ier=ier+istatus   
       call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'bc2',   ges_bc2,   istatus );ier=ier+istatus 
       call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'oc1',   ges_oc1,   istatus );ier=ier+istatus      
       call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'oc2',   ges_oc2,   istatus );ier=ier+istatus    
       call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'dust1', ges_dust1, istatus );ier=ier+istatus  
       call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'dust2', ges_dust2, istatus );ier=ier+istatus 
       call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'dust3', ges_dust3, istatus );ier=ier+istatus  
       call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'dust4', ges_dust4, istatus );ier=ier+istatus  
       call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'dust5', ges_dust5, istatus );ier=ier+istatus     
       call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'seas1', ges_seas1, istatus );ier=ier+istatus
       call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'seas2', ges_seas2, istatus );ier=ier+istatus
       call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'seas3', ges_seas3, istatus );ier=ier+istatus 
       call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'seas4', ges_seas4, istatus );ier=ier+istatus
       call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'p25',   ges_p25,   istatus );ier=ier+istatus
       if (ier/=0 .and. mype == 0) then
           write(6,*)'WRWRFMASSA_NETCDF: getpointer failed for gocart species'
       endif
       do iv = 1, n_gocart_var
          kchem(iv) = i_chem(iv)-1
       end do
    endif
  
    if ( wrf_pm2_5 ) then
       ier = 0
       call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it), 'pm2_5',  ges_pm2_5,  istatus )
       ier=ier+istatus
       if (ier/=0 .and. mype == 0) then
           write(6,*)'WRWRFMASSA_NETCDF: getpointer failed for gocart pm2_5'
       endif
       iv=1
       kchem(iv) = i_chem(iv)-1
    endif
  
    q_integral=one
    q_integralc4h=zero_single
    do k=1,nsig
       deltasigma=eta1_ll(k)-eta1_ll(k+1)
       deltasigmac4h=eta2_ll(k)-eta2_ll(k+1)
       kt=kt+1
       kq=kq+1
       ku=ku+1
       kv=kv+1
  ! for hydrometeors
       if(l_hydrometeor_bkio .and. n_actual_clouds>0) then
          kqc=kqc+1
          kqi=kqi+1
          kqr=kqr+1
          kqs=kqs+1
          kqg=kqg+1
          kqnr=kqnr+1
          if ( .not. l_use_dbz_directDA) then ! diret reflectivity DA does not use qni and qnc
             kqni=kqni+1
             kqnc=kqnc+1
          end if
          if(dbz_exist.and.if_model_dbz)kdbz=kdbz+1
          ktt=ktt+1
       endif
       if ( laeroana_gocart ) then
          do iv = 1, n_gocart_var
             kchem(iv) = kchem(iv)+1
          end do
       endif
  
       if (wrf_pm2_5) then
          iv=1
          kchem(iv) = kchem(iv)+1
       endif
  
  
       do i=1,lon2
          do j=1,lat2
             all_loc(j,i,ku)=ges_u(j,i,k)
             all_loc(j,i,kv)=ges_v(j,i,k)
  
  !          Convert sensible temperature to potential temperature
             work_prsl  = one_tenth*(aeta1_ll(k)*(r10*ges_ps(j,i)-pt_ll)+aeta2_ll(k)+pt_ll)
             work_prslk = (work_prsl/r100)**rd_over_cp_mass
             all_loc(j,i,kt) = ges_tsen(j,i,k,it)/work_prslk
  
  !          Convert specific humidity to mixing ratio
             all_loc(j,i,kq)= ges_q(j,i,k)/(one-ges_q(j,i,k))
             	
  ! for hydrometeors      
             if(l_hydrometeor_bkio .and. n_actual_clouds>0) then
                all_loc(j,i,kqc)=ges_qc(j,i,k)
                all_loc(j,i,kqi)=ges_qi(j,i,k)
                all_loc(j,i,kqr)=ges_qr(j,i,k)
                all_loc(j,i,kqs)=ges_qs(j,i,k)
                all_loc(j,i,kqg)=ges_qg(j,i,k)
                all_loc(j,i,kqnr)=ges_qnr(j,i,k)
                all_loc(j,i,ktt)=ges_tten(j,i,k,it)
                if(dbz_exist.and.if_model_dbz)all_loc(j,i,kdbz)=ges_dbz(j,i,k)
                if ( .not. l_use_dbz_directDA) then ! dirZDA does not update qni, qnc
                   all_loc(j,i,kqni)=ges_qni(j,i,k)
                   all_loc(j,i,kqnc)=ges_qnc(j,i,k)
                endif

             end if

             if ( laeroana_gocart ) then
                all_loc(j,i,kchem(1))=ges_sulf(j,i,k)
                all_loc(j,i,kchem(2))=ges_bc1(j,i,k)
                all_loc(j,i,kchem(3))=ges_bc2(j,i,k)
                all_loc(j,i,kchem(4))=ges_oc1(j,i,k)
                all_loc(j,i,kchem(5))=ges_oc2(j,i,k)
                all_loc(j,i,kchem(6))=ges_dust1(j,i,k)
                all_loc(j,i,kchem(7))=ges_dust2(j,i,k)
                all_loc(j,i,kchem(8))=ges_dust3(j,i,k)
                all_loc(j,i,kchem(9))=ges_dust4(j,i,k)
                all_loc(j,i,kchem(10))=ges_dust5(j,i,k)
                all_loc(j,i,kchem(11))=ges_seas1(j,i,k)
                all_loc(j,i,kchem(12))=ges_seas2(j,i,k)
                all_loc(j,i,kchem(13))=ges_seas3(j,i,k)
                all_loc(j,i,kchem(14))=ges_seas4(j,i,k)
                if ( n_gocart_var>=15 ) all_loc(j,i,kchem(15))=ges_p25(j,i,k)
             endif
  
             if ( wrf_pm2_5 ) then
                all_loc(j,i,kchem(1))=ges_pm2_5(j,i,k)
             endif
             
  
             q_integral(j,i)=q_integral(j,i)+deltasigma* &
                  ges_q(j,i,k)/(one-ges_q(j,i,k))
             q_integralc4h(j,i)=q_integralc4h(j,i)+deltasigmac4h* &
                  ges_q(j,i,k)/(one-ges_q(j,i,k))
          end do
       end do
    end do

    if(w_exist) then
     kw=kw+1
     do i=1,lon2
        do j=1,lat2
          all_loc(j,i,kw)=ges_w_btlev(j,i,1,it) ! for w on the bottom not changed
        enddo
     enddo

     do k=1,nsig-1
       kw=kw+1
       do i=1,lon2
          do j=1,lat2
           all_loc(j,i,kw)=0.5*(ges_w(j,i,k)+ges_w(j,i,k+1))
          enddo
       enddo
     enddo

     kw=kw+1
     do i=1,lon2
        do j=1,lat2
          all_loc(j,i,kw)=ges_w_btlev(j,i,2,it) ! for w on the top not changed
        enddo
     enddo
    endif


    do i=1,lon2
       do j=1,lat2
          psfc_this=r10*ges_ps(j,i)   ! convert from cb to mb
          psfc_this_dry=pt_ll+((psfc_this-pt_ll)-q_integralc4h(j,i))/q_integral(j,i)
          all_loc(j,i,i_psfc)=r100*psfc_this_dry
       end do
    end do

! directZDA - restore cvpqx to qx
    if ( l_use_dbz_directDA ) then
       call convert_cvpqx_to_qx(all_loc(:,:,i_qr:i_qr+nsig), & 
                                all_loc(:,:,i_qs:i_qs+nsig), &
                                all_loc(:,:,i_qg:i_qg+nsig), &
                                l_use_cvpqx, cvpqx_pval)  ! Convert Qx back
       call convert_cvpnx_to_nx(all_loc(:,:,i_qnr:i_qnr+nsig), &
                                l_cvpnr, cvpnr_pval, cld_nt_updt, &
                                ges_q, all_loc(:,:,i_qr:i_qr+nsig), ges_ps)       ! psfc/ Convert Nx back 
    end if
  
    
    if(mype == 0) then
       read(lendian_in) regional_time0,nlon_regional0,nlat_regional0,nsig0,pt0,nsoil
       write(lendian_out) regional_time0,nlon_regional0,nlat_regional0,nsig0,pt0,nsoil
       read(lendian_in) aeta10,aeta20
       write(lendian_out) aeta10,aeta20
       read(lendian_in) eta10,eta20
       write(lendian_out) eta10,eta20
       read(lendian_in) glat0,dx_mc0
       write(lendian_out) glat0,dx_mc0
       read(lendian_in) glon0,dy_mc0
       write(lendian_out) glon0,dy_mc0
    end if
    
  ! Update psfc
    if(mype == 0) write(6,*)' at 6 in wrwrfmassa'
  
    allocate(tempa(itotsub),tempb(itotsub))
    if(mype == 0) read(lendian_in)temp1
    call strip(all_loc(:,:,i_psfc),strp)
    call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
         tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
    if(mype == 0) then
       call fill_mass_grid2t(temp1,im,jm,tempb,2)
       do i=1,iglobal
          tempa(i)=tempa(i)-tempb(i)
       end do
       call unfill_mass_grid2t(tempa,im,jm,temp1)
       write(lendian_out)temp1
       do i=1,im*jm
          sfcprs(i) = temp1(i)
       enddo
    end if
  
  !  FIS read/write
    if(mype == 0) then
       read(lendian_in)temp1
       write(lendian_out)temp1
    end if
  
  ! Update t
    kt=i_t-1
    do k=1,nsig
       kt=kt+1
       if(mype == 0) read(lendian_in)temp1
       call strip(all_loc(:,:,kt),strp)
       call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
            tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
       if(mype == 0) then
          call fill_mass_grid2t(temp1,im,jm,tempb,2)
          do i=1,iglobal
             tempa(i)=tempa(i)-tempb(i)
          end do
          call unfill_mass_grid2t(tempa,im,jm,temp1)
          write(lendian_out)temp1
          if(k>=1 .and. k<=nlvl_qcheck) then
          ! calculate moisture saturation at model level k 
             allocate(tmp2da(im,jm),tmp2db(im,jm),tmp2dc(im,jm))
             ij=0
             do j=1,jm
                do i=1,im
                   ij=ij+1
                   work_prsl = one_tenth*(aeta1_ll(k)*(sfcprs(ij)/r100-pt_ll)+aeta2_ll(k)+pt_ll)
                   tmp2dc(i,j)=work_prsl                                       ! pressure
                   tmp2db(i,j)=temp1(ij)*(work_prsl/r100)**rd_over_cp_mass     ! T in K
                   if(k==1) t1st2d(ij)  = tmp2db(i,j)
                enddo
             enddo
             call genqsat(tmp2da,tmp2db,tmp2dc,im,jm,1,.false.,0)
             ij=0
             do j=1,jm
                do i=1,im
                   ij=ij+1
                   qs1st2d(ij,k)=tmp2da(i,j) ! moisture saturation 
                enddo
             enddo
             deallocate(tmp2da,tmp2db,tmp2dc)
             if(k==1) pt2t=t1st2d   ! save a copy
          endif
       end if
    end do
  
  ! Update q
    kq=i_q-1
    do k=1,nsig
       kq=kq+1
       if(mype == 0) read(lendian_in)temp1
       call strip(all_loc(:,:,kq),strp)
       call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
            tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
       if(mype == 0) then
          call fill_mass_grid2t(temp1,im,jm,tempb,2)
          do i=1,iglobal
             tempa(i)=tempa(i)-tempb(i)
          end do
          if(k>=1 .and. k<=nlvl_qcheck) then
             call unfill_mass_grid2t_drycheck(tempa,im,jm,temp1,qs1st2d(:,k))
          else
             call unfill_mass_grid2t(tempa,im,jm,temp1)
          endif
          write(lendian_out)temp1
       end if
    end do
  
  ! Update u
    ku=i_u-1
    do k=1,nsig
       ku=ku+1
       if(mype == 0) read(lendian_in)temp1u
       call strip(all_loc(:,:,ku),strp)
       call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
            tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
       if(mype == 0) then
          call fill_mass_grid2u(temp1u,im,jm,tempb,2)
          do i=1,iglobal
             tempa(i)=tempa(i)-tempb(i)
          end do
          call unfill_mass_grid2u(tempa,im,jm,temp1u)
          write(lendian_out)temp1u
       end if
    end do
  
  ! Update v
    kv=i_v-1
    do k=1,nsig
       kv=kv+1
       if(mype == 0) read(lendian_in)temp1v
       call strip(all_loc(:,:,kv),strp)
       call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
            tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
       if(mype == 0) then
          call fill_mass_grid2v(temp1v,im,jm,tempb,2)
          do i=1,iglobal
             tempa(i)=tempa(i)-tempb(i)
          end do
          call unfill_mass_grid2v(tempa,im,jm,temp1v)
          write(lendian_out)temp1v
       end if
    end do

    if(w_exist) then
    ! Update w
      kw=i_w-1
      do k=1,nsig+1
        kw=kw+1
        if(mype == 0) read(lendian_in)temp1
        call strip(all_loc(:,:,kw),strp)
        call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
             tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
        if(mype == 0) then
           call fill_mass_grid2t(temp1,im,jm,tempb,2)
           do i=1,iglobal
              tempa(i)=tempa(i)-tempb(i)
           end do
           call unfill_mass_grid2t(tempa,im,jm,temp1)
           write(lendian_out)temp1
        end if
      end do
    endif ! for w_exist

    
  ! Load updated skin temperature array if writing out to analysis file
    if (update_regsfc) then
       do i=1,lon2
          do j=1,lat2
             all_loc(j,i,i_sst)=dsfct(j,i,ntguessfc)
             all_loc(j,i,i_skt)=dsfct(j,i,ntguessfc)
          end do
       end do
    end if
  
  ! for soil nudging
    if(l_gsd_soilTQ_nudge) then
       ier=0
       call GSI_BundleGetPointer (GSI_MetGuess_Bundle(it),'smoist',ges_smois_it,istatus)
       ier=ier+istatus
       call GSI_BundleGetPointer (GSI_MetGuess_Bundle(it),'tslb' ,ges_tslb_it ,istatus)
       ier=ier+istatus
       if (ier/=0) call die(trim(myname),'cannot get pointers for tslb/smois, ier =',ier)
       ktslb=i_tslb-1
       ksmois=i_smois-1
       do k=1,nsig_soil
          ktslb=ktslb+1
          ksmois=ksmois+1
          do i=1,lon2
             do j=1,lat2
                all_loc(j,i,ktslb)=ges_tslb_it(j,i,k)
                all_loc(j,i,ksmois)=ges_smois_it(j,i,k)
             end do
          end do
       end do
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'tsoil', ges_soilt1, istatus );ier=ier+istatus
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'tskn' , ges_tsk , istatus );ier=ier+istatus
       if (ier/=0) then ! doesn't have to die - code can be generalized to bypass missing vars
           write(6,*)'wrwrfmassa_netcdf: getpointer failed, cannot retrieve soil stuff'
           call stop2(999)
       endif
       do i=1,lon2
          do j=1,lat2
             all_loc(j,i,i_skt)=ges_tsk(j,i)
             all_loc(j,i,i_soilt1)=ges_soilt1(j,i)
          end do
       end do
    endif ! l_gsd_soilTQ_nudge

    if(i_use_2mt4b > 0 ) then
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 't2m', ges_t2m,istatus );ier=ier+istatus
       if (ier/=0) then ! doesn't have to die - code can be generalized to bypass
           write(6,*)'wrwrfmassa_netcdf: getpointer failed, cannot retrieve th2'
           call stop2(999)
       endif
! convert to potential T
       do i=1,lon2
          do j=1,lat2
             all_loc(j,i,i_th2)=ges_t2m(j,i)*(r100/ges_ps(j,i))**rd_over_cp_mass
          end do
       end do
    endif
    if(i_use_2mt4b > 0 .or. i_use_2mq4b > 0) then
       call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'q2m',   ges_q2, istatus );ier=ier+istatus
       if (ier/=0) then ! doesn't have to die - code can be generalized to bypass missing vars          
           write(6,*)'wrwrfmassa_netcdf: getpointer failed, cannot retrieve q2m'
           call stop2(999)
       endif
       do i=1,lon2
          do j=1,lat2
  ! Convert 2m specific humidity to mixing ratio
             all_loc(j,i,i_q2)=ges_q2(j,i)/(one-ges_q2(j,i))
          end do
       end do
    endif
  
    if(mype == 0) then
  ! SM   This is landmask
       read(lendian_in)temp1
       landmask=temp1
       write(lendian_out)temp1
  ! SICE
       read(lendian_in)temp1
       seaice=temp1
       write(lendian_out)temp1
    end if
  
  ! SST
    if(update_regsfc) then
       if(mype == 0) read(lendian_in)temp1
       if (mype==0)write(6,*)' at 9.1 in wrwrfmassa,max,min(temp1)=',maxval(temp1),minval(temp1)
       call strip(all_loc(:,:,i_sst),strp)
       call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
            tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
       if(mype == 0) then
          if(mype == 0) write(6,*)' at 9.2 in wrwrfmassa,max,min(tempa)=',maxval(tempa),minval(tempa)
          call fill_mass_grid2t(temp1,im,jm,tempb,2)
          do i=1,iglobal
             if(tempb(i) < (r225)) then
                tempa(i)=zero_single
             else
                tempa(i)=tempa(i)-tempb(i)
             end if
          end do
          if(mype == 0) write(6,*)' at 9.4 in wrwrfmassa,max,min(tempa)=',maxval(tempa),minval(tempa)
          call unfill_mass_grid2t(tempa,im,jm,temp1)
          write(6,*)' at 9.6 in wrwrfmassa,max,min(temp1)=',maxval(temp1),minval(temp1)
          write(lendian_out)temp1
       end if     !endif mype==0
    else
       if(mype==0) then
          read(lendian_in)temp1
          write(lendian_out)temp1
       end if
    end if   !end if check updatesfc
    
  ! REST OF FIELDS
    if(l_gsd_soilTQ_nudge) then
       if (mype == 0) then
          do k=4,6
             read(lendian_in)temp1
             write(lendian_out)temp1
          end do
  ! SM   This is snow
          read(lendian_in)temp1
          write(lendian_out)temp1
          snow=temp1
          do k=8,9
             read(lendian_in)temp1
             write(lendian_out)temp1
          end do
       end if
  ! Update smois
       ksmois=i_smois-1
       do k=1,nsig_soil
          ksmois=ksmois+1
          if(mype == 0) read(lendian_in)temp1
          call strip(all_loc(:,:,ksmois),strp)
          call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
               tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
          if(mype == 0) then
             call fill_mass_grid2t(temp1,im,jm,tempb,2)
             do i=1,iglobal
                tempa(i)=tempa(i)-tempb(i)
             end do
             i_snowT_check=2
             call unfill_mass_grid2t_ldmk(tempa,im,jm,temp1,landmask,&
                                          snow,seaice,t1st2d,i_snowT_check)
  ! make sure soil moisture is larger than soilmoistmin (0.002 (sand)).
             do i=1,im*jm
                temp1(i) = max(temp1(i),soilmoistmin)
             enddo
             write(lendian_out)temp1
          end if
       end do
  ! update TSLB
       ktslb=i_tslb-1
       do k=1,nsig_soil
          ktslb=ktslb+1
          if(mype == 0) read(lendian_in)temp1
          call strip(all_loc(:,:,ktslb),strp)
          call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
               tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
          if(mype == 0) then
             call fill_mass_grid2t(temp1,im,jm,tempb,2)
             do i=1,iglobal
                tempa(i)=tempa(i)-tempb(i)
             end do
             i_snowT_check=3
             if(k==1) t1st2d=temp1-t1st2d
             call unfill_mass_grid2t_ldmk(tempa,im,jm,temp1,landmask, &
                                          snow,seaice,t1st2d,i_snowT_check)
             write(lendian_out)temp1
          end if
       end do
    else
       if (mype == 0) then
          do k=4,11
             read(lendian_in)temp1
             write(lendian_out)temp1
          end do
          snow=0
       end if
    endif !  l_gsd_soilTQ_nudge
    
  ! Update SKIN TEMP
    if(update_regsfc .or. l_gsd_soilTQ_nudge) then
       if(mype == 0) read(lendian_in)temp1
       if (mype==0)write(6,*)' at 10.0 in wrwrfmassa,max,min(temp1)=',maxval(temp1),minval(temp1)
       call strip(all_loc(:,:,i_skt),strp)
       call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
            tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
       if(mype == 0) then
          call fill_mass_grid2t(temp1,im,jm,tempb,2)
          do i=1,iglobal
             if(tempb(i) < (r225)) then
                tempa(i)=zero_single
             else 
                tempa(i)=tempa(i)-tempb(i)
             end if
          end do
          i_snowT_check=1
          t1st2d=temp1-pt2t
          call unfill_mass_grid2t_ldmk(tempa,im,jm,temp1,landmask, &
                                       snow,seaice,t1st2d,i_snowT_check)
          write(lendian_out)temp1
       end if
    else
       if (mype == 0) then
          read(lendian_in)temp1
          write(lendian_out)temp1
       end if
    end if
  
  ! Update Q2
    if( i_use_2mq4b>0 .or. i_use_2mt4b > 0) then
       if(mype == 0) read(lendian_in)temp1
       if (mype==0)write(6,*)' at 10.11 in wrwrfmassa,max,min(temp1)=', &
                                           maxval(temp1),minval(temp1)
       call strip(all_loc(:,:,i_q2),strp)
       tempa=zero_single
       call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
            tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
       if(mype == 0) then
          write(6,*)' at 10.12 in wrwrfmassa,max,min(tempa)=', &
                            maxval(tempa),minval(tempa)
          call fill_mass_grid2t(temp1,im,jm,tempb,2)
          do i=1,iglobal
             tempa(i)=tempa(i)-tempb(i)
          end do
          write(6,*)' at 10.13 in wrwrfmassa,max,min(tempa)=', &
                             maxval(tempa),minval(tempa)
          call unfill_mass_grid2t_drycheck(tempa,im,jm,temp1,qs1st2d(:,1))
          write(6,*)' at 10.14 in wrwrfmassa,max,min(temp1)=', &
                             maxval(temp1),minval(temp1)
          write(lendian_out)temp1
       end if     !endif mype==0
    endif
  
    if(l_gsd_soilTQ_nudge) then
  ! update soilt1
       if(mype == 0) read(lendian_in)temp1
       if (mype==0)write(6,*)' at 10.1 in wrwrfmassa,max,min(temp1)=',maxval(temp1),minval(temp1)
       call strip(all_loc(:,:,i_soilt1),strp)
       tempa=zero_single
       call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
            tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
       if(mype == 0) then
           write(6,*)' at 10.2 in wrwrfmassa,max,min(tempa)=',maxval(tempa),minval(tempa)
          call fill_mass_grid2t(temp1,im,jm,tempb,2)
          do i=1,iglobal
             tempa(i)=tempa(i)-tempb(i)
          end do
          write(6,*)' at 10.3 in wrwrfmassa,max,min(tempa)=',maxval(tempa),minval(tempa)
          i_snowT_check=4
          call unfill_mass_grid2t_ldmk(tempa,im,jm,temp1,landmask, &
                                      snow,seaice,t1st2d,i_snowT_check)
          write(6,*)' at 10.4 in wrwrfmassa,max,min(temp1)=',maxval(temp1),minval(temp1)
          write(lendian_out)temp1
       end if     !endif mype==0
    endif  !  l_gsd_soilTQ_nudge
  ! update TH2
    if( i_use_2mt4b>0 ) then
       if(mype == 0) read(lendian_in)temp1
       if (mype==0)write(6,*)' at 10.5 in wrwrfmassa,max,min(temp1)=',maxval(temp1),minval(temp1)
       call strip(all_loc(:,:,i_th2),strp)
       call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
            tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
       if(mype == 0) then
           write(6,*)' at 10.6 in wrwrfmassa,max,min(tempa)=',maxval(tempa),minval(tempa)
          call fill_mass_grid2t(temp1,im,jm,tempb,2)
          do i=1,iglobal
             if(tempb(i) < (r100)) then
                tempa(i)=zero_single
             else
                tempa(i)=tempa(i)-tempb(i)
             end if
          end do
          write(6,*)' at 10.7 in wrwrfmassa,max,min(tempa)=',maxval(tempa),minval(tempa)
          call unfill_mass_grid2t(tempa,im,jm,temp1)
          write(6,*)' at 10.8 in wrwrfmassa,max,min(temp1)=',maxval(temp1),minval(temp1)
          write(lendian_out)temp1
       end if     !endif mype==0
    endif  ! i_use_2mt4b>0
  !
  ! for saving cloud analysis results
    if(l_hydrometeor_bkio .and. n_actual_clouds>0) then
  ! Update qc
       kqc=i_qc-1
       do k=1,nsig
          kqc=kqc+1
          if(mype == 0) read(lendian_in)temp1
          call strip(all_loc(:,:,kqc),strp)
          call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
               tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
          if(mype == 0) then
             call fill_mass_grid2t(temp1,im,jm,tempb,2)
             do i=1,iglobal
                tempa(i)=tempa(i)-tempb(i)
             end do
             call unfill_mass_grid2t(tempa,im,jm,temp1)
             write(lendian_out)temp1
          end if
       end do
  
  ! Update qr     
       kqr=i_qr-1
       do k=1,nsig
          kqr=kqr+1
          if(mype == 0) read(lendian_in)temp1
          call strip(all_loc(:,:,kqr),strp)
          call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
               tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
          if(mype == 0) then
             call fill_mass_grid2t(temp1,im,jm,tempb,2)
             do i=1,iglobal
                tempa(i)=tempa(i)-tempb(i)
             end do
             call unfill_mass_grid2t(tempa,im,jm,temp1)
             write(lendian_out)temp1
          end if
       end do
  
  ! Update qs     
       kqs=i_qs-1
       do k=1,nsig
          kqs=kqs+1
          if(mype == 0) read(lendian_in)temp1
          call strip(all_loc(:,:,kqs),strp)
          call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
               tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
          if(mype == 0) then
             call fill_mass_grid2t(temp1,im,jm,tempb,2)
             do i=1,iglobal
                tempa(i)=tempa(i)-tempb(i)
             end do
             call unfill_mass_grid2t(tempa,im,jm,temp1)
             write(lendian_out)temp1
          end if
       end do
  
  ! Update qi     
       kqi=i_qi-1
       do k=1,nsig
          kqi=kqi+1
          if(mype == 0) read(lendian_in)temp1
          call strip(all_loc(:,:,kqi),strp)
          call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
               tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
          if(mype == 0) then
             call fill_mass_grid2t(temp1,im,jm,tempb,2)
             do i=1,iglobal
                tempa(i)=tempa(i)-tempb(i)
             end do
             call unfill_mass_grid2t(tempa,im,jm,temp1)
             write(lendian_out)temp1
          end if
       end do
  
  ! Update qg     
       kqg=i_qg-1
       do k=1,nsig
          kqg=kqg+1
          if(mype == 0) read(lendian_in)temp1
          call strip(all_loc(:,:,kqg),strp)
          call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
               tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
          if(mype == 0) then
             call fill_mass_grid2t(temp1,im,jm,tempb,2)
             do i=1,iglobal
                tempa(i)=tempa(i)-tempb(i)
             end do
             call unfill_mass_grid2t(tempa,im,jm,temp1)
             write(lendian_out)temp1
          end if
       end do
  
  ! Update qnr     
       kqnr=i_qnr-1
       do k=1,nsig
          kqnr=kqnr+1
          if(mype == 0) read(lendian_in)temp1
          call strip(all_loc(:,:,kqnr),strp)
          call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
               tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
          if(mype == 0) then
             call fill_mass_grid2t(temp1,im,jm,tempb,2)
             do i=1,iglobal
                tempa(i)=tempa(i)-tempb(i)
             end do
             call unfill_mass_grid2t(tempa,im,jm,temp1)
             write(lendian_out)temp1
          end if
       end do
       
       if ( .not. l_use_dbz_directDA) then ! direct reflectivity DA does not use qni and qnc
       ! Update qni     
            kqni=i_qni-1
            do k=1,nsig
               kqni=kqni+1
               if(mype == 0) read(lendian_in)temp1
               call strip(all_loc(:,:,kqni),strp)
               call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
                    tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
               if(mype == 0) then
                  call fill_mass_grid2t(temp1,im,jm,tempb,2)
                  do i=1,iglobal
                     tempa(i)=tempa(i)-tempb(i)
                  end do
                  call unfill_mass_grid2t(tempa,im,jm,temp1)
                  write(lendian_out)temp1
               end if
            end do
   
       ! Update qnc     
            kqnc=i_qnc-1
            do k=1,nsig
               kqnc=kqnc+1
               if(mype == 0) read(lendian_in)temp1
               call strip(all_loc(:,:,kqnc),strp)
               call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
                    tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
               if(mype == 0) then
                  call fill_mass_grid2t(temp1,im,jm,tempb,2)
                  do i=1,iglobal
                     tempa(i)=tempa(i)-tempb(i)
                  end do
                  call unfill_mass_grid2t(tempa,im,jm,temp1)
                  write(lendian_out)temp1
               end if
            end do
       end if
   
       if(dbz_exist.and.if_model_dbz)then
       ! Update refl_10cm
            kdbz=i_dbz-1
            do k=1,nsig
               kdbz=kdbz+1
               if(mype == 0) read(lendian_in)temp1
               call strip(all_loc(:,:,kdbz),strp)
               call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
                    tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
               if(mype == 0) then
                  call fill_mass_grid2t(temp1,im,jm,tempb,2)
                  do i=1,iglobal
                     tempa(i)=tempa(i)-tempb(i)
                  end do
                  call unfill_mass_grid2t(tempa,im,jm,temp1)
                  write(lendian_out)temp1
               end if
            end do
       end if
   
  ! Update tten     
       ktt=i_tt-1
       do k=1,nsig
          ktt=ktt+1
          if(mype == 0) read(lendian_in)temp1
          call strip(all_loc(:,:,ktt),strp)
          call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
               tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
          if(mype == 0) then
             call fill_mass_grid2t(temp1,im,jm,tempb,2)
             do i=1,iglobal
                tempa(i)=tempa(i)-tempb(i)
             end do
             call unfill_mass_grid2t(tempa,im,jm,temp1)
             write(lendian_out)temp1
          end if
       end do
  
    endif    ! l_hydrometeor_bkio
  
    if ( laeroana_gocart ) then
       do iv = 1, n_gocart_var
          kchem(iv)=i_chem(iv)-1
          do k=1,nsig
             tempa=0.0
             kchem(iv)=kchem(iv)+1
             if(mype == 0) read(lendian_in)temp1
             call strip(all_loc(:,:,kchem(iv)),strp)
             call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
                  tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
             if(mype == 0) then
                call fill_mass_grid2t(temp1,im,jm,tempb,2)
                do i=1,iglobal
                   tempa(i)=tempa(i)-tempb(i)
                end do
                call unfill_mass_grid2t(tempa,im,jm,temp1)
                write(lendian_out)temp1
             end if
          end do
       end do
       deallocate(i_chem)
       deallocate(kchem)
    endif
  
    if ( wrf_pm2_5 ) then
       iv=1
       kchem(iv)=i_chem(iv)-1
       do k=1,nsig
          tempa=0.0
          kchem(iv)=kchem(iv)+1
          if(mype == 0) read(lendian_in)temp1
          call strip(all_loc(:,:,kchem(iv)),strp)
          call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
               tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
          if(mype == 0) then
             call fill_mass_grid2t(temp1,im,jm,tempb,2)
             do i=1,iglobal
                tempa(i)=tempa(i)-tempb(i)
             end do
             call unfill_mass_grid2t(tempa,im,jm,temp1)
             write(lendian_out)temp1
          end if
       end do
       deallocate(i_chem)
       deallocate(kchem)
    endif
  
    if (mype==0) then
       close(lendian_in)
       close(lendian_out)
    endif
  
    deallocate(all_loc)
    deallocate(strp)
    deallocate(temp1)
    deallocate(temp1u)
    deallocate(temp1v)
    deallocate(tempa)
    deallocate(tempb)
    if(mype==0) then
       deallocate(landmask)
       deallocate(snow)
       deallocate(seaice)
       deallocate(t1st2d)
       deallocate(pt2t)
       deallocate(qs1st2d)
       deallocate(sfcprs)
    endif
    
  end subroutine wrwrfmassa_netcdf_wrf

  subroutine update_start_date(chdrbuf,iyear,imonth,iday,ihour,iminute,isecond)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    update_start_date  update start date on wrf file
  !   prgmmr: parrish          org: np22                date: 2004-11-29
  !
  ! abstract: update date record in START_DATE header record
  !
  ! program history log:
  !   2004-11-29  parrish
  !   2006-04-24  middlecoff - Put error handling in search for START-TIME
  !                            and changed search loop increment from 4 to one
  !
  !   input argument list:
  !     chdrbuf  - 2048 byte wrf header record containing "START_DATE"
  !     iyear    - analysis time year
  !     imonth   - analysis time month
  !     iday     - analysis time day
  !     ihour    - analysis time hour
  !     iminute  - analysis time minute
  !     isecond  - analysis time second
  !
  !   output argument list:
  !     chdrbuf  - output header record with new analysis date
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$
  
  !    update date record in START_DATE header record
  
    use kinds, only: i_kind
    implicit none
  
    character(1)   ,intent(inout) :: chdrbuf(2048)
    integer(i_kind),intent(in   ) :: iyear,imonth,iday,ihour,iminute,isecond
  
    character(2) c_two
    character(4) c_four
    character(1) c2(2),d2(2),c4(4),d4(4)
    equivalence (c2(1),c_two),(c4(1),c_four)
    integer(i_kind) i,ibegin,j
  
    ibegin=0
    do i=1,1932
       if(chdrbuf(i)=='S'.and.chdrbuf(i+4)=='T'.and.chdrbuf(i+8)=='A' &
         .and.chdrbuf(i+12)=='R'.and.chdrbuf(i+16)=='T'.and.chdrbuf(i+20)=='_' &
         .and.chdrbuf(i+24)=='D'.and.chdrbuf(i+28)=='A'.and.chdrbuf(i+32)=='T' &
         .and.chdrbuf(i+36)=='E') then
          ibegin=i+44
          exit
       end if
    end do
    if(ibegin==0) then
       write(6,*)'UPDATE_START_DATE:  ***ERROR***  ibegin = ',ibegin
       write(6,*)'chdrbuf=',chdrbuf
       call stop2(24)
    endif
  
    i=ibegin-4
    write(c_four,'(i4.4)')iyear
    do j=1,4
       i=i+4
       d4(j)=chdrbuf(i)
       chdrbuf(i)=c4(j)
    end do
    write(6,*) 'UPDATE_START_DATE:  old year, new year =',d4,' , ',c4
  
  !          skip "-"
    i=i+4
  
    write(c_two,'(i2.2)')imonth
    do j=1,2
       i=i+4
       d2(j)=chdrbuf(i)
       chdrbuf(i)=c2(j)
    end do
    write(6,*) 'UPDATE_START_DATE:  old month, new month =',d2,' , ',c2
  
  !          skip "-"
    i=i+4
  
    write(c_two,'(i2.2)')iday
    do j=1,2
       i=i+4
       d2(j)=chdrbuf(i)
       chdrbuf(i)=c2(j)
    end do
    write(6,*) 'UPDATE_START_DATE:  old day, new day =',d2,' , ',c2
  
  !          skip "_"
    i=i+4
  
    write(c_two,'(i2.2)')ihour
    do j=1,2
       i=i+4
       d2(j)=chdrbuf(i)
       chdrbuf(i)=c2(j)
    end do
    write(6,*) 'UPDATE_START_DATE:  old hour, new hour =',d2,' , ',c2
  
  !          skip ":"
    i=i+4
  
    write(c_two,'(i2.2)')iminute
    do j=1,2
       i=i+4
       d2(j)=chdrbuf(i)
       chdrbuf(i)=c2(j)
    end do
    write(6,*) 'UPDATE_START_DATE:  old minute, new minute =',d2,' , ',c2
  
  !          skip ":"
    i=i+4
  
    write(c_two,'(i2.2)')isecond
    do j=1,2
       i=i+4
       d2(j)=chdrbuf(i)
       chdrbuf(i)=c2(j)
    end do
    write(6,*) 'UPDATE_START_DATE:  old second, new second =',d2,' , ',c2
  
  end subroutine update_start_date

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
    use kinds, only: r_kind,i_kind,r_single
    use gridmod, only: lat2,lon2,nsig
    use guess_grids, only: ges_tsen
    use mpimod, only: mype
    use guess_grids, only: ntguessig
    use constants, only: zero, one_tenth,r0_01

    implicit none
    real(r_single), intent(inout  ) :: qr_arr(lat2,lon2,nsig)
    real(r_single), intent(inout  ) :: qs_arr(lat2,lon2,nsig)
    real(r_single), intent(inout  ) :: qg_arr(lat2,lon2,nsig)
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
              write(6,*)'wrwrfmassa_netcdf_wrf: convert log(qr/qs/qg) back to qr/qs/qg.'
              write(6,*)'wrwrfmassa_netcdf_wrf: then reset (qr/qs/qg)to 0.0 for some cases.'
          else if ( cvpqx_pvalue > 0._r_kind ) then   ! CVpq
              write(6,*)'wrwrfmassa_netcdf_wrf: convert power transformed (qr/qs/qg) back to qr/qs/qg.'
              write(6,*)'wrwrfmassa_netcdf_wrf: then reset (qr/qs/qg)to 0.0 for some cases.'
          end if
       else                                         ! CVq
          write(6,*)'wrwrfmassa_netcdf_wrf: only reset (qr/qs/qg) to 0.0 for negative analysis value. (regular qx)'
       end if
    end if

! Initialized temp arrays with ges. Will be recalculated later if log(q) is used
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

                 !set upper limits to hydrometeors to disable overshooting
                 qr_tmp=min(qr_tmp,1E-2_r_kind)
                 qs_tmp=min(qs_tmp,1.5E-2_r_kind)
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
!     cloud_nt_updt  - integer flag to use re-initialisation of QNRAIN with
!     analyzed qr and n0r
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
    use kinds, only: r_kind,i_kind,r_single
    use gridmod, only: lat2,lon2,nsig
    use mpimod, only: mype
    use constants, only: zero, one, one_tenth
    use directDA_radaruse_mod, only: init_mm_qnr
    use guess_grids, only: ges_tsen
    use guess_grids, only: ntguessig
    use gridmod, only: pt_ll, aeta1_ll
    use constants, only: r10, r100, rd


    implicit none
    real(r_single), intent(inout  )    :: qnx_arr(lat2,lon2,nsig)
    logical,      intent(in     )    :: cvpnr
    real(r_kind), intent(in     )    :: cvpnr_pvalue
    integer(i_kind), intent(in     ) :: cloud_nt_updt
    real(r_kind), intent(in     )    :: q_arr(lat2,lon2,nsig)
    real(r_single), intent(in     )  :: qr_arr(lat2,lon2,nsig)
    real(r_kind), intent(in     )    :: ps_arr(lat2,lon2)

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
       write(6,*)'wrwrfmassa_netcdf_wrf: convert power transformed(qnx) back to qnx.'
    end if

! Initialized temp arrays with ges.
    tmparr_qnr=qnx_arr

    do k=1,nsig
      do i=1,lon2
        do j=1,lat2

!          re-initialisation of QNRAIN with analyzed qr and N0r(which is
!          single-moment parameter)
!          equation is used in subroutine init_MM of initlib3d.f90 in arps
!          package
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

end module wrwrfmassa_mod

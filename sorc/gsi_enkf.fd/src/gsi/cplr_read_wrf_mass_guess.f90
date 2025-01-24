module read_wrf_mass_guess_mod
use abstract_read_wrf_mass_guess_mod
use kinds, only: i_kind
  type, extends(abstract_read_wrf_mass_guess_class) :: read_wrf_mass_guess_class 
  contains
    procedure, pass(this) :: read_wrf_mass_netcdf_guess => read_wrf_mass_netcdf_guess_wrf
    procedure, pass(this) :: read_wrf_mass_binary_guess => read_wrf_mass_binary_guess_wrf 
    procedure, nopass :: transfer_jbuf2ibuf
    procedure, pass(this) :: generic_grid2sub
    procedure, nopass :: expand_ibuf
    procedure, nopass :: move_ibuf_hg
    procedure, nopass :: move_ibuf_ihg
    procedure, nopass :: reorder2_s
  end type read_wrf_mass_guess_class 
contains
  subroutine read_wrf_mass_binary_guess_wrf(this,mype)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    read_wrf_mass_guess      read wrf_mass interface file
  !   prgmmr: parrish          org: np22                date: 2003-09-05
  !
  ! abstract: in place of read_guess for global application, read guess
  !             from regional model, in this case the wrf mass core model.
  !             This version reads a binary file created
  !             in a previous step that interfaces with the wrf infrastructure.
  !             A later version will read directly from the wrf restart file.
  !             The guess is read in by complete horizontal fields, one field
  !             per processor, in parallel.  Each horizontal input field is 
  !             converted from the staggered c-grid to an unstaggered a-grid.
  !             On the c-grid, u is shifted 1/2 point in the negative x direction
  !             and v 1/2 point in the negative y direction, but otherwise the
  !             three grids are regular.  When the fields are read in, nothing
  !             is done to mass variables, but wind variables are interpolated to
  !             mass points.
  !
  ! program history log:
  !   2004--7-15  parrish
  !   2004-08-02  treadon - add only to module use, add intent in/out
  !   2004-09-10  parrish - correct error in land-sea mask interpretation
  !   2004-11-08  parrish - change to mpi-io for binary file format
  !   2004-12-15  treadon - remove variable mype from call load_geop_hgt, 
  !                         rename variable wrf_ges_filename as wrfges
  !   2005-02-17  todling - ifdef'ed wrf code out
  !   2005-02-23  wu - setup for qoption=2 and output stats of RH
  !   2005-04-01  treadon - add initialization of ges_oz, prsi_oz; comestic format changes
  !   2005-05-27  parrish - add call get_derivatives
  !   2005-11-21  kleist - new call to genqsat
  !   2005-11-21  derber - make qoption=1 work same as qoption=2
  !   2005-11-29  derber - remove external iteration dependent calculations
  !   2005-12-15  treadon - remove initialization of certain guess arrays (done elsewhere)
  !   2006-02-02  treadon - remove load_prsges,load_geop_hgt,prsl,prslk (not used)
  !   2006-02-15  treadon - convert moisture mixing ratio to specific humidity
  !   2006-03-07  treadon - convert guess potential temperature to vritual temperature
  !   2006-04-06  middlecoff - changed nfcst from 11 to lendian_in
  !   2006-07-28  derber  - include sensible temperatures
  !   2006-07-31  kleist - change to use ges_ps instead of lnps
  !   2007-03-13  derber - remove unused qsinv2 from jfunc use list
  !   2007-04-12  parrish - add modifications to allow any combination of ikj or ijk
  !                          grid ordering for input 3D fields
  !   2008-04-16  safford - rm unused uses
  !   2010-06-24  hu     - add code to read in cloud/hydrometeor fields
  !                             and distributed them to all processors
  !   2011-04-29  todling - introduce MetGuess and wrf_mass_guess_mod
  !   2012-10-11  parrish - add option to swap bytes immediately after every call to mpi_file_read_at.
  !                           (to handle cases of big-endian file/little-endian machine and vice-versa)
  !   2012-11-26  hu     - add code to read in soil fields
  !   2013-10-19  todling - metguess now holds background
  !   2014-03-12  hu     - add code to read ges_q2 (2m Q), 
  !                               Qnr(rain number concentration), 
  !                               and nsoil (number of soil levels)
  !   2014-12-12  hu     - change l_use_2mq4b to i_use_2mq4b
  !   2015-01-13  ladwig - add code to read Qni and Qnc (cloud ice and water
  !                               number concentration)
  !   2017-03-23  Hu     - add code to read hybrid vertical coodinate in WRF MASS
  !                          core
  !   2022-03-15  Hu  change all th2 to t2m and convert 2m temperature 
  !                        from potentionl to senseible temperature
  !
  !   input argument list:
  !     mype     - pe number
  !
  !     NOTES:  need to pay special attention to various surface fields to make sure
  !             they are correct (check units).  fact10 needs to be computed from 
  !             10m wind, which is included in wrf mass interface file.
  !
  !             Cloud water currently set to zero.  Need to fix before doing precip
  !             assimilation--wait until global adopts Brad Ferrier's multi-species 
  !             scheme??
  !
  !             Ozone currently set to zero.  No ozone variable in wrf mass core--climatology
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
    use mpimod, only: mpi_sum,mpi_integer,mpi_comm_world,npe,ierror, &
         mpi_offset_kind,mpi_info_null,mpi_mode_rdonly,mpi_status_size
    use guess_grids, only: &
         fact10,soil_type,veg_frac,veg_type,sfct,sno,soil_temp,soil_moi,&
         isli,nfldsig,ifilesig,ges_tsen,sfc_rough,ntguessig
    use gridmod, only: lat2,lon2,nlat_regional,nlon_regional,&
         nsig,nsig_soil,eta1_ll,pt_ll,itotsub,aeta1_ll,eta2_ll,aeta2_ll
    use constants, only: zero,one,grav,fv,zero_single,rd_over_cp_mass,one_tenth,h300,r10,r100
    use constants, only: r0_01
    use gsi_io, only: lendian_in,verbose
    use rapidrefresh_cldsurf_mod, only: l_hydrometeor_bkio,l_gsd_soilTQ_nudge,i_use_2mq4b,i_use_2mt4b
    use wrf_mass_guess_mod, only: soil_temp_cld,isli_cld,ges_xlon,ges_xlat,ges_tten,create_cld_grids
    use gsi_bundlemod, only: GSI_BundleGetPointer
    use gsi_metguess_mod, only: gsi_metguess_get,GSI_MetGuess_Bundle
    use native_endianness, only: byte_swap
    use mpeu_util, only: die
    implicit none
  
  ! Declare passed variables
    class(read_wrf_mass_guess_class),intent(inout) :: this
    integer(i_kind),intent(in):: mype
  
  ! Declare local parameters
  
    character(len=*),parameter::myname='read_wrf_mass_binary_guess::'
    real(r_kind),parameter:: rough_default=0.05_r_kind
  
  ! Declare local variables
    integer(i_kind) kt,kq,ku,kv
    real(r_single) rad2deg_single
  
  ! MASS variable names stuck in here
    integer(i_kind) mfcst
  
  ! other internal variables
    real(r_single),allocatable::tempa(:,:)
    real(r_single),allocatable::temp1(:,:),temp1u(:,:),temp1v(:,:)
    real(r_single),allocatable::all_loc(:,:,:)
    integer(i_kind),allocatable::igtype(:),kdim(:),kord(:)
    integer(kind=mpi_offset_kind),allocatable::offset(:)
    integer(kind=mpi_offset_kind) this_offset
    integer(i_kind),allocatable::length(:)
    integer(i_kind) this_length
    integer(i_llong) num_swap
    character(6) filename 
    character(9) wrfges
    integer(i_kind) ifld,im,jm,lm,num_mass_fields
    integer(i_kind) num_loc_groups,num_j_groups
    integer(i_kind) i,it,j,k
    integer(i_kind) i_mub,i_mu,i_fis,i_t,i_q,i_u,i_v,i_sno,i_u10,i_v10,i_smois,i_tslb
    integer(i_kind) i_sm,i_xice,i_sst,i_tsk,i_ivgtyp,i_isltyp,i_vegfrac
    integer(i_kind) isli_this
    real(r_kind) psfc_this,psfc_this_dry,sm_this,xice_this
    real(r_kind),dimension(lat2,lon2):: q_integral,q_integralc4h
    real(r_kind),dimension(lat2,lon2,nsig):: ges_pot
    integer(i_kind) num_doubtful_sfct,num_doubtful_sfct_all
    real(r_kind) deltasigma,deltasigmac4h
    integer(i_llong) n_position
    integer(i_kind) iskip,ksize,jextra,nextra
    integer(i_kind) status(mpi_status_size)
    integer(i_kind) jbegin(0:npe),jend(0:npe-1),jend2(0:npe-1)
    integer(i_kind) kbegin(0:npe),kend(0:npe-1)
    integer(i_long),allocatable:: ibuf(:,:)
    integer(i_long),allocatable:: jbuf(:,:,:)
    integer(i_long) dummy9(9)
    real(r_single) pt_regional_single
    real(r_kind):: work_prsl,work_prslk
    integer(i_kind) i_qc,i_qi,i_qr,i_qs,i_qg,i_qnr,i_qni,i_qnc
    integer(i_kind) kqc,kqi,kqr,kqs,kqg,kqnr,kqni,kqnc,i_xlon,i_xlat,i_tt,ktt
    integer(i_kind) i_th2,i_q2,i_soilt1,ksmois,ktslb
    integer(i_kind) ier, istatus
    integer(i_kind) n_actual_clouds
  
    real(r_kind), pointer :: ges_ps_it (:,:  )=>NULL()
    real(r_kind), pointer :: ges_t2m_it(:,:  )=>NULL()
    real(r_kind), pointer :: ges_q2_it (:,:  )=>NULL()
    real(r_kind), pointer :: ges_tsk_it(:,:  )=>NULL()
    real(r_kind), pointer :: ges_soilt1_it(:,:)=>NULL()
    real(r_kind), pointer :: ges_tslb_it(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_smois_it(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_z_it  (:,:  )=>NULL()
    real(r_kind), pointer :: ges_u_it  (:,:,:)=>NULL()
    real(r_kind), pointer :: ges_v_it  (:,:,:)=>NULL()
    real(r_kind), pointer :: ges_tv_it (:,:,:)=>NULL()
    real(r_kind), pointer :: ges_q_it  (:,:,:)=>NULL()
  
    real(r_kind), pointer :: ges_qc (:,:,:)=>NULL()
    real(r_kind), pointer :: ges_qi (:,:,:)=>NULL()
    real(r_kind), pointer :: ges_qr (:,:,:)=>NULL()
    real(r_kind), pointer :: ges_qs (:,:,:)=>NULL()
    real(r_kind), pointer :: ges_qg (:,:,:)=>NULL()
    real(r_kind), pointer :: ges_qnr(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_qni(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_qnc(:,:,:)=>NULL()
  
    integer(i_kind) iadd
    character(132) memoryorder
    logical print_verbose
  
  !  WRF MASS input grid dimensions in module gridmod
  !      These are the following:
  !          im -- number of x-points on C-grid
  !          jm -- number of y-points on C-grid
  !          lm -- number of vertical levels ( = nsig for now)
  
       print_verbose = .false. ! want to turn to true only on mype == 0 for printout
       if(verbose .and. mype == 0)print_verbose=.true.
       num_doubtful_sfct=0
  
       rad2deg_single=45.0_r_single/atan(1.0_r_single)
  
       im=nlon_regional
       jm=nlat_regional
       lm=nsig
       if(jm<=npe)then
          write(6,*)' in read_wrf_mass_binary_guess, jm <= npe, ',&
                     'so program will end.'
          call stop2(1)
       endif
  
       if(print_verbose) write(6,*)' in read_wrf_mass_binary_guess, im,jm,lm=',im,jm,lm
  
  !    Inquire about cloud guess fields
       call gsi_metguess_get('clouds::3d',n_actual_clouds,istatus)
       if (n_actual_clouds>0) then
  !       Get pointer for each of the hydrometeors from guess at time index "it"
          ier=0
          it=ntguessig
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
  
  !    Following is for convenient WRF MASS input
       num_mass_fields=15+5*lm+2*nsig_soil
!    The 9 3D cloud analysis fields are: ql,qi,qr,qs,qg,qnr,qni,qnc,tt
       if(l_hydrometeor_bkio .and. n_actual_clouds>0) num_mass_fields=num_mass_fields+9*lm+2    
       if(l_gsd_soilTQ_nudge) num_mass_fields=num_mass_fields+2
       num_loc_groups=num_mass_fields/npe
       if(print_verbose) then
          write(6,'(" read_wrf_mass_guess: lm            =",i6)')lm
          write(6,'(" read_wrf_mass_guess: nsig_soil     =",i6)')nsig_soil
          write(6,'(" read_wrf_mass_guess: num_mass_fields=",i6)')num_mass_fields
          write(6,'(" read_wrf_mass_guess: nfldsig       =",i6)')nfldsig
          write(6,'(" read_wrf_mass_guess: npe           =",i6)')npe
          write(6,'(" read_wrf_mass_guess: num_loc_groups=",i6)')num_loc_groups
       end if
  
       allocate(offset(num_mass_fields))
       allocate(igtype(num_mass_fields),kdim(num_mass_fields),kord(num_mass_fields))
       allocate(length(num_mass_fields))
  
  !    initialize GSD specific guess fields
       call create_cld_grids()
       
  !    igtype is a flag indicating whether each input MASS field is h-, u-, or v-grid
  !    and whether integer or real
  !     abs(igtype)=1 for h-grid
  !                =2 for u-grid
  !                =3 for v-grid
  !               
  !     igtype < 0 for integer field
  !     igtype = 0  for dummy (nothing to read)
  
  !    offset is the byte count preceding each record to be read from the wrf binary file.
  !       used as individual file pointers by mpi_file_read_at
  
       do it=1,nfldsig
          write(filename,'("sigf",i2.2)')ifilesig(it)
          open(lendian_in,file=filename,form='unformatted') ; rewind lendian_in
          if(print_verbose)then
             write(6,*)'READ_WRF_MASS_BINARY_GUESS:  open lendian_in=',lendian_in,&
               ' to filename=',filename,' on it=',it
             write(6,*)'READ_WRF_MASS_OFFSET_FILE:  open lendian_in=',lendian_in,' to file=',filename
          end if
          read(lendian_in) dummy9,pt_regional_single
          if(print_verbose)write(6,*)'READ_WRF_MASS_BINARY_GUESS:  dummy9=',dummy9
  
  ! get pointers for typical meteorological fields
          ier=0
          call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'ps',ges_ps_it,istatus );ier=ier+istatus
          call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'z', ges_z_it, istatus );ier=ier+istatus
          call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'u', ges_u_it, istatus );ier=ier+istatus
          call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'v', ges_v_it, istatus );ier=ier+istatus
          call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'tv',ges_tv_it,istatus );ier=ier+istatus
          call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'q' ,ges_q_it, istatus );ier=ier+istatus
          if (ier/=0) call die(trim(myname),'cannot get pointers for met-fields, ier =',ier)
  
          if (l_gsd_soilTQ_nudge .or.i_use_2mt4b>0) then
             call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 't2m', ges_t2m_it,istatus );ier=ier+istatus 
             if (ier/=0) call die(trim(myname),'cannot get pointers for t2m, ier=',ier)
          endif

          if (l_gsd_soilTQ_nudge) then
             call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'tskn', ges_tsk_it,istatus );ier=ier+istatus
             call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'tsoil',ges_soilt1_it,istatus );ier=ier+istatus
             if (ier/=0) call die(trim(myname),'cannot get pointers for met-fields, ier =',ier)
          endif
          if (i_use_2mq4b>0) then
             call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'q2m'  ,ges_q2_it,istatus);ier=ier+istatus
             if (ier/=0) call die(trim(myname),'cannot get pointers for q2m, ier =',ier)
          endif
  
  ! for cloud analysis
          if(l_hydrometeor_bkio .and. n_actual_clouds>0) then
  
  ! get pointer to relevant instance of cloud-related background
             ier=0
             call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'ql', ges_qc, istatus );ier=ier+istatus
             call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qi', ges_qi, istatus );ier=ier+istatus
             call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qr', ges_qr, istatus );ier=ier+istatus
             call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qs', ges_qs, istatus );ier=ier+istatus
             call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qg', ges_qg, istatus );ier=ier+istatus
             call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qnr',ges_qnr,istatus );ier=ier+istatus
             call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qni',ges_qni,istatus );ier=ier+istatus
             call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qnc',ges_qnc,istatus );ier=ier+istatus
             if (ier/=0 .and. mype == 0) then
                 write(6,*)'READ_WRF_MASS_BINARY_GUESS: getpointer failed, cannot do cloud analysis'
                 l_hydrometeor_bkio=.false.
             endif
  
             i=0
             allocate(tempa(nlon_regional,nlat_regional))
             do iskip=2,3
                read(lendian_in)
             end do
             i=i+1 ; i_xlat=i                                                ! xlat
             read(lendian_in) tempa,tempa,n_position
             offset(i)=n_position ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=1
             if(print_verbose) write(6,*)' xlat, i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
   
             i=i+1 ; i_xlon=i                                                ! xlon
             read(lendian_in) tempa,tempa,n_position
             offset(i)=n_position ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=1
             if(print_verbose) write(6,*)' xlon, i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
             deallocate(tempa)
          else
             do iskip=2,5
                read(lendian_in)
             end do
             i=0
          endif
          read(lendian_in) wrfges
          read(lendian_in) ! n_position          !  offset for START_DATE record
  !       if(mype == 0) write(6,*)'READ_WRF_MASS_BINARY_GUESS:  read wrfges,n_position= ',wrfges,' ',n_position
          if(print_verbose) write(6,*)'READ_WRF_MASS_BINARY_GUESS:  read wrfges= ',wrfges
          
          i=i+1 ; i_mub=i                                                ! mub
          read(lendian_in) n_position
          offset(i)=n_position ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=1
          if(print_verbose) write(6,*)' mub, i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
          
          i=i+1 ; i_mu =i                                                ! mu
          read(lendian_in) n_position
          offset(i)=n_position ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=1
          if(print_verbose) write(6,*)' mu, i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
          
          i_fis=i+1                                               ! sfc geopotential
          read(lendian_in) n_position,memoryorder
          do k=1,lm+1
             i=i+1
             if(trim(memoryorder)=='XZY') then
                iadd=0
                kord(i)=lm+1
             else
                iadd=(k-1)*im*jm*4
                kord(i)=1
             end if
             offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=lm+1
             if(print_verbose.and.k==1) write(6,*)' sfc geopot i,igtype(i),offset(i),kord(i) = ', &
                                                                     i,igtype(i),offset(i),kord(i)
          end do
          
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
             if(print_verbose.and.k==1) write(6,*)' temp i,igtype(i),offset(i),kord(i) = ', &
                                                               i,igtype(i),offset(i),kord(i)
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
             if(print_verbose.and.k==1) write(6,*)' q i,igtype(i),offset(i),kord(i) = ', &
                                                               i,igtype(i),offset(i),kord(i)
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
             if(print_verbose.and.k==1) write(6,*)' u i,igtype(i),offset(i),kord(i) = ', &
                                                               i,igtype(i),offset(i),kord(i)
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
             if(print_verbose.and.k==1) write(6,*)' v i,igtype(i),offset(i),kord(i) = ', &
                                                               i,igtype(i),offset(i),kord(i)
          end do
          
          i=i+1   ; i_sm=i                                              ! landmask
          read(lendian_in) n_position
          offset(i)=n_position ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=1
          if(print_verbose) write(6,*)' landmask i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
          
          i=i+1 ; i_xice=i                                              ! xice
          read(lendian_in) n_position
          offset(i)=n_position ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=1
          if(print_verbose) write(6,*)' xice i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
          
          i=i+1 ; i_sst=i                                               ! sst
          read(lendian_in) n_position
          offset(i)=n_position ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=1
          if(print_verbose) write(6,*)' sst i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
          
          i=i+1 ; i_ivgtyp=i                                            ! ivgtyp
          read(lendian_in) n_position
          offset(i)=n_position ; length(i)=im*jm ; igtype(i)=-1 ; kdim(i)=1
          if(print_verbose) write(6,*)' ivgtyp i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
          
          i=i+1 ; i_isltyp=i                                            ! isltyp
          read(lendian_in) n_position
          offset(i)=n_position ; length(i)=im*jm ; igtype(i)=-1 ; kdim(i)=1
          if(print_verbose) write(6,*)' isltyp i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
          
          i=i+1 ; i_vegfrac=i                                           ! vegfrac
          read(lendian_in) n_position
          offset(i)=n_position ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=1
          if(print_verbose) write(6,*)' vegfrac i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
          
          i=i+1 ; i_sno=i                                               ! sno
          read(lendian_in) n_position
          offset(i)=n_position ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=1
          if(print_verbose) write(6,*)' sno i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
          
          i=i+1 ; i_u10=i                                               ! u10
          read(lendian_in) n_position
          offset(i)=n_position ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=1
          if(print_verbose) write(6,*)' u10 i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
          
          i=i+1 ; i_v10=i                                               ! v10
          read(lendian_in) n_position
          offset(i)=n_position ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=1
          if(print_verbose) write(6,*)' v10 i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
          
          i=i+1 ; i_smois=i                                             ! smois
          read(lendian_in) n_position,ksize,memoryorder
          if(trim(memoryorder)=='XZY') then
             kord(i)=ksize
          else
             kord(i)=1
          end if
          offset(i)=n_position ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=ksize
          if(print_verbose) write(6,*)' smois i,igtype(i),offset(i),kord(i) = ', &
                                                               i,igtype(i),offset(i),kord(i)
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
          
          i=i+1 ; i_tslb=i                                              ! tslb
          read(lendian_in) n_position,ksize,memoryorder
          if(trim(memoryorder)=='XZY') then
             kord(i)=ksize
          else
             kord(i)=1
          end if
  
          offset(i)=n_position ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=ksize
          if(print_verbose) write(6,*)' tslb i,igtype(i),offset(i),kord(i) = ', &
                                                               i,igtype(i),offset(i),kord(i)
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
             if(print_verbose) write(6,*)' i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
          end do
  
          i=i+1 ; i_tsk=i                                               ! tsk
  
          read(lendian_in) n_position
          offset(i)=n_position ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=1
          if(print_verbose) write(6,*)' tsk i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
  
          i=i+1 ; i_q2=i                                               ! q2 
          read(lendian_in) n_position
          offset(i)=n_position ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=1
          if(print_verbose) write(6,*)' q2 i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
  
          if(l_gsd_soilTQ_nudge) then
             i=i+1 ; i_soilt1=i                                               ! soilt1
             read(lendian_in) n_position
             offset(i)=n_position ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=1
             if(print_verbose) write(6,*)' soilt1 i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
          endif
  
          i=i+1 ; i_th2=i                                               ! th2
          read(lendian_in) n_position
          offset(i)=n_position ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=1
          if(print_verbose) write(6,*)' th2 i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
  
  ! for cloud array
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
                if(print_verbose.and.k==1) write(6,*)' qc i,igtype(i),offset(i),kord(i) = ', &
                                                                i,igtype(i),offset(i),kord(i)
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
                if(mype == 0.and.k==1) write(6,*)' qr i,igtype(i),offset(i),kord(i) = ', &
                                                                i,igtype(i),offset(i),kord(i)
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
                if(print_verbose.and.k==1) write(6,*)' qi i,igtype(i),offset(i),kord(i) = ', &
                                                                i,igtype(i),offset(i),kord(i)
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
                if(print_verbose.and.k==1) write(6,*)' qs i,igtype(i),offset(i),kord(i) = ', &
                                                                i,igtype(i),offset(i),kord(i)
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
                if(print_verbose.and.k==1) write(6,*)' qg i,igtype(i),offset(i),kord(i) = ', &
                                                                i,igtype(i),offset(i),kord(i)
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
                if(print_verbose.and.k==1) write(6,*)' qnr i,igtype(i),offset(i),kord(i) = ', &
                                                                i,igtype(i),offset(i),kord(i)
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
                offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=1 ;kdim(i)=lm
                if(print_verbose.and.k==1) write(6,*)' qni i,igtype(i),offset(i),kord(i) = ', &
                                                              i,igtype(i),offset(i),kord(i)
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
                offset(i)=n_position+iadd ; length(i)=im*jm ; igtype(i)=1;kdim(i)=lm
                if(print_verbose.and.k==1) write(6,*)' qnc i,igtype(i),offset(i),kord(i) = ', &
                                                              i,igtype(i),offset(i),kord(i)
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
                if(print_verbose.and.k==1) write(6,*)' tt i,igtype(i),offset(i),kord(i) = ', &
                                                                i,igtype(i),offset(i),kord(i)
             end do
    
          endif
  
          close(lendian_in)
          
  !    End of stuff from MASS restart file
  
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
          
          allocate(ibuf((im+1)*(jm+1),kbegin(mype):kend(mype)))
          call mpi_file_open(mpi_comm_world,trim(wrfges),mpi_mode_rdonly,mpi_info_null,mfcst,ierror)
          
  !                                    read geopotential
          if(kord(i_fis)/=1) then
             allocate(jbuf(im,lm+1,jbegin(mype):jend(mype)))
             this_offset=offset(i_fis)+(jbegin(mype)-1)*4*im*(lm+1)
             this_length=(jend(mype)-jbegin(mype)+1)*im*(lm+1)
             call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length, &
                                   mpi_integer,status,ierror)
             if(byte_swap) then
                num_swap=this_length
                call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
             end if
             call this%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                  jbegin,jend,kbegin,kend,mype,npe,im,jm,lm+1,im+1,jm+1,i_fis,i_fis+lm)
             deallocate(jbuf)
          end if
          
  !                                    read temps
          if(kord(i_t)/=1) then
             allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
             this_offset=offset(i_t)+(jbegin(mype)-1)*4*im*lm
             this_length=(jend(mype)-jbegin(mype)+1)*im*lm
             call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length, &
                                   mpi_integer,status,ierror)
             if(byte_swap) then
                num_swap=this_length
                call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
             end if
             call this%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                  jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im+1,jm+1,i_t,i_t+lm-1)
             deallocate(jbuf)
          end if
  
  !                                    read q
          if(kord(i_q)/=1) then
             allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
             this_offset=offset(i_q)+(jbegin(mype)-1)*4*im*lm
             this_length=(jend(mype)-jbegin(mype)+1)*im*lm
             call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length, &
                                   mpi_integer,status,ierror)
             if(byte_swap) then
                num_swap=this_length
                call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
             end if
             call this%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                  jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im+1,jm+1,i_q,i_q+lm-1)
             deallocate(jbuf)
          end if
          
  !                                    read u
          if(kord(i_u)/=1) then
             allocate(jbuf(im+1,lm,jbegin(mype):jend(mype)))
             this_offset=offset(i_u)+(jbegin(mype)-1)*4*(im+1)*lm
             this_length=(jend(mype)-jbegin(mype)+1)*(im+1)*lm
             call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length, &
                                   mpi_integer,status,ierror)
             if(byte_swap) then
                num_swap=this_length
                call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
             end if
             call this%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                  jbegin,jend,kbegin,kend,mype,npe,im+1,jm,lm,im+1,jm+1,i_u,i_u+lm-1)
             deallocate(jbuf)
          end if
          
  !                                    read v
          if(kord(i_v)/=1) then
             jend2=jend
  !  Account for extra lat for v
             jend2(npe-1)=jend2(npe-1)+1
             allocate(jbuf(im,lm,jbegin(mype):jend2(mype)))
             this_offset=offset(i_v)+(jbegin(mype)-1)*4*im*lm
             this_length=(jend2(mype)-jbegin(mype)+1)*im*lm
             call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length, &
                                   mpi_integer,status,ierror)
             if(byte_swap) then
                num_swap=this_length
                call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
             end if
             call this%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend2(mype),ibuf,kbegin(mype),kend(mype), &
                  jbegin,jend2,kbegin,kend,mype,npe,im,jm+1,lm,im+1,jm+1,i_v,i_v+lm-1)
             deallocate(jbuf)
          end if
          
  !                                    read smois
          if(kord(i_smois)/=1) then
             allocate(jbuf(im,ksize,jbegin(mype):jend(mype)))
             this_offset=offset(i_smois)+(jbegin(mype)-1)*4*im*ksize
             this_length=(jend(mype)-jbegin(mype)+1)*im*ksize
             call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length, &
                                   mpi_integer,status,ierror)
             if(byte_swap) then
                num_swap=this_length
                call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
             end if
             call this%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                  jbegin,jend,kbegin,kend,mype,npe,im,jm,ksize,im+1,jm+1,i_smois,i_smois+ksize-1)
             deallocate(jbuf)
          end if
  
  !                                    read tslb
          if(kord(i_tslb)/=1) then
             allocate(jbuf(im,ksize,jbegin(mype):jend(mype)))
             this_offset=offset(i_tslb)+(jbegin(mype)-1)*4*im*ksize
             this_length=(jend(mype)-jbegin(mype)+1)*im*ksize
             call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length, &
                                   mpi_integer,status,ierror)
             if(byte_swap) then
                num_swap=this_length
                call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
             end if
             call this%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                  jbegin,jend,kbegin,kend,mype,npe,im,jm,ksize,im+1,jm+1,i_tslb,i_tslb+ksize-1)
             deallocate(jbuf)
          end if
  
  ! for cloud analysis
          if(l_hydrometeor_bkio .and. n_actual_clouds>0) then
  !                                    read qc
             if(kord(i_qc)/=1) then
                allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
                this_offset=offset(i_qc)+(jbegin(mype)-1)*4*im*lm
                this_length=(jend(mype)-jbegin(mype)+1)*im*lm
                call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length, &
                                    mpi_integer,status,ierror)
             if(byte_swap) then
                num_swap=this_length
                call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
             end if
                call this%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                   jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im+1,jm+1,i_qc,i_qc+lm-1)
                deallocate(jbuf)
             end if
  
  !                                    read qr
             if(kord(i_qr)/=1) then
                allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
                this_offset=offset(i_qr)+(jbegin(mype)-1)*4*im*lm
                this_length=(jend(mype)-jbegin(mype)+1)*im*lm
                call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length, &
                                    mpi_integer,status,ierror)
             if(byte_swap) then
                num_swap=this_length
                call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
             end if
                call this%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                   jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im+1,jm+1,i_qr,i_qr+lm-1)
                deallocate(jbuf)
             end if
  
  !                                    read qi
             if(kord(i_qi)/=1) then
                allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
                this_offset=offset(i_qi)+(jbegin(mype)-1)*4*im*lm
                this_length=(jend(mype)-jbegin(mype)+1)*im*lm
                call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length, &
                                    mpi_integer,status,ierror)
             if(byte_swap) then
                num_swap=this_length
                call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
             end if
                call this%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                   jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im+1,jm+1,i_qi,i_qi+lm-1)
                deallocate(jbuf)
             end if
  
  !                                    read qs
             if(kord(i_qs)/=1) then
                allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
                this_offset=offset(i_qs)+(jbegin(mype)-1)*4*im*lm
                this_length=(jend(mype)-jbegin(mype)+1)*im*lm
                call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length, &
                                    mpi_integer,status,ierror)
             if(byte_swap) then
                num_swap=this_length
                call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
             end if
                call this%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                   jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im+1,jm+1,i_qs,i_qs+lm-1)
                deallocate(jbuf)
             end if
  
  !                                    read qg
             if(kord(i_qg)/=1) then
                allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
                this_offset=offset(i_qg)+(jbegin(mype)-1)*4*im*lm
                this_length=(jend(mype)-jbegin(mype)+1)*im*lm
                call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length, &
                                    mpi_integer,status,ierror)
             if(byte_swap) then
                num_swap=this_length
                call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
             end if
                call this%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                   jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im+1,jm+1,i_qg,i_qg+lm-1)
                deallocate(jbuf)
             end if
  
  !                                    read qnr
             if(kord(i_qnr)/=1) then
                allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
                this_offset=offset(i_qnr)+(jbegin(mype)-1)*4*im*lm
                this_length=(jend(mype)-jbegin(mype)+1)*im*lm
                call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length, &
                                    mpi_integer,status,ierror)
             if(byte_swap) then
                num_swap=this_length
                call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
             end if
                call this%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                   jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im+1,jm+1,i_qnr,i_qnr+lm-1)
                deallocate(jbuf)
             end if
  
!                                    read qni
             if(kord(i_qni)/=1) then
                allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
                this_offset=offset(i_qni)+(jbegin(mype)-1)*4*im*lm
                this_length=(jend(mype)-jbegin(mype)+1)*im*lm
                call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length, &
                                  mpi_integer,status,ierror)
             if(byte_swap) then
                num_swap=this_length
                call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
             end if
                call this%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                   jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im+1,jm+1,i_qni,i_qni+lm-1)
                deallocate(jbuf)
             end if

!                                    read qnc
             if(kord(i_qnc)/=1) then
                allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
                this_offset=offset(i_qnc)+(jbegin(mype)-1)*4*im*lm
                this_length=(jend(mype)-jbegin(mype)+1)*im*lm
                call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length, &
                                    mpi_integer,status,ierror)
             if(byte_swap) then
                num_swap=this_length
                call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
             end if
                call this%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                   jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im+1,jm+1,i_qnc,i_qnc+lm-1)
                deallocate(jbuf)
             end if


  !                                    read tt  radar temperature tendency
             if(kord(i_tt)/=1) then
                allocate(jbuf(im,lm,jbegin(mype):jend(mype)))
                this_offset=offset(i_tt)+(jbegin(mype)-1)*4*im*lm
                this_length=(jend(mype)-jbegin(mype)+1)*im*lm
                call mpi_file_read_at(mfcst,this_offset,jbuf(1,1,jbegin(mype)),this_length, &
                                    mpi_integer,status,ierror)
             if(byte_swap) then
                num_swap=this_length
                call to_native_endianness_i4(jbuf(1,1,jbegin(mype)),num_swap)
             end if
                call this%transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                   jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im+1,jm+1,i_tt,i_tt+lm-1)
                deallocate(jbuf)
             end if
  
          endif  ! l_hydrometeor_bkio
  
  !---------------------- read surface files last
          do k=kbegin(mype),kend(mype)
             if(kdim(k)==1.or.kord(k)==1) then
                call mpi_file_read_at(mfcst,offset(k),ibuf(1,k),length(k),mpi_integer,status,ierror)
             if(byte_swap) then
                num_swap=length(k)
                call to_native_endianness_i4(ibuf(1,k),num_swap)
             end if
                if(igtype(k)==1) call this%expand_ibuf(ibuf(1,k),im  ,jm  ,im+1,jm+1)
                if(igtype(k)==-1)call this%expand_ibuf(ibuf(1,k),im  ,jm  ,im+1,jm+1)
                if(igtype(k)==2) call this%expand_ibuf(ibuf(1,k),im+1,jm  ,im+1,jm+1)
                if(igtype(k)==3) call this%expand_ibuf(ibuf(1,k),im  ,jm+1,im+1,jm+1)
             end if
          end do
  
          call mpi_file_close(mfcst,ierror)
      
  !   next interpolate to analysis grid, then distribute to subdomains
          
          allocate(temp1(im,jm),temp1u(im+1,jm),temp1v(im,jm+1))
          allocate(tempa(itotsub,kbegin(mype):kend(mype)))
          do ifld=kbegin(mype),kend(mype)
             if(igtype(ifld) ==  1) then
                call this%move_ibuf_hg(ibuf(1,ifld),temp1,im+1,jm+1,im,jm)
                call fill_mass_grid2t(temp1,im,jm,tempa(1,ifld),1)
             else if(igtype(ifld) == -1) then
                call this%move_ibuf_ihg(ibuf(1,ifld),temp1,im+1,jm+1,im,jm)
                call fill_mass_grid2t(temp1,im,jm,tempa(1,ifld),1)
             else if(igtype(ifld) == 2) then
                call this%move_ibuf_hg(ibuf(1,ifld),temp1u,im+1,jm+1,im+1,jm)
                call fill_mass_grid2u(temp1u,im,jm,tempa(1,ifld),1)
             else if(igtype(ifld) == 3) then
                call this%move_ibuf_hg(ibuf(1,ifld),temp1v,im+1,jm+1,im,jm+1)
                call fill_mass_grid2v(temp1v,im,jm,tempa(1,ifld),1)
             end if
          end do
          deallocate(ibuf)
          deallocate(temp1,temp1u,temp1v)
          allocate(all_loc(lat2,lon2,num_mass_fields))
          call this%generic_grid2sub(tempa,all_loc,kbegin(mype),kend(mype),kbegin,kend,mype,num_mass_fields)
          
  
  !    Next do conversion of units as necessary and
  !    reorganize into WeiYu's format--
  
          kt=i_t-1
          kq=i_q-1
          ku=i_u-1
          kv=i_v-1
  ! hydrometeors
          if(l_hydrometeor_bkio .and. n_actual_clouds>0) then
             kqc=i_qc-1
             kqr=i_qr-1
             kqs=i_qs-1
             kqi=i_qi-1
             kqg=i_qg-1
             kqnr=i_qnr-1
             kqni=i_qni-1
             kqnc=i_qnc-1
             ktt=i_tt-1
          endif
  !             wrf pressure variable is dry air partial pressure--need to add water vapor contribution
  !              so accumulate 1 + total water vapor to use as correction factor
  
          q_integral=one
          q_integralc4h=zero
          do k=1,nsig
             deltasigma=eta1_ll(k)-eta1_ll(k+1)
             deltasigmac4h=eta2_ll(k)-eta2_ll(k+1)
             kt=kt+1
             kq=kq+1
             ku=ku+1
             kv=kv+1
  ! hydrometeors
             if(l_hydrometeor_bkio .and. n_actual_clouds>0) then
                kqc=kqc+1
                kqr=kqr+1
                kqs=kqs+1
                kqi=kqi+1
                kqg=kqg+1
                kqnr=kqnr+1
                kqni=kqni+1
                kqnc=kqnc+1
                ktt=ktt+1
             endif
             do i=1,lon2
                do j=1,lat2
                   ges_u_it(j,i,k) = real(all_loc(j,i,ku),r_kind)
                   ges_v_it(j,i,k) = real(all_loc(j,i,kv),r_kind)
                   ges_q_it(j,i,k) = real(all_loc(j,i,kq),r_kind)
                   q_integral(j,i) = q_integral(j,i)+deltasigma*ges_q_it(j,i,k)
                   q_integralc4h(j,i) = q_integralc4h(j,i)+deltasigmac4h*ges_q_it(j,i,k)
  
  !                Convert guess mixing ratio to specific humidity
                   ges_q_it(j,i,k) = ges_q_it(j,i,k)/(one+ges_q_it(j,i,k))
  
  !                Add offset to get guess potential temperature
                   ges_pot(j,i,k)  = real(all_loc(j,i,kt),r_kind) + h300
  ! hydrometeors
                   if(l_hydrometeor_bkio .and. n_actual_clouds>0) then
                      ges_qc(j,i,k) = real(all_loc(j,i,kqc),r_kind)
                      ges_qi(j,i,k) = real(all_loc(j,i,kqi),r_kind)
                      ges_qr(j,i,k) = real(all_loc(j,i,kqr),r_kind)
                      ges_qs(j,i,k) = real(all_loc(j,i,kqs),r_kind)
                      ges_qg(j,i,k) = real(all_loc(j,i,kqg),r_kind)
                      ges_qnr(j,i,k)= real(all_loc(j,i,kqnr),r_kind)
                      ges_qni(j,i,k)= real(all_loc(j,i,kqni),r_kind)
                      ges_qnc(j,i,k)= real(all_loc(j,i,kqnc),r_kind)
  !                    ges_tten(j,i,k,it) = real(all_loc(j,i,ktt),r_kind)
                      ges_tten(j,i,k,it) = -20.0_r_single
                      if(k==nsig) ges_tten(j,i,k,it) = -10.0_r_single
  
                   endif
  
                end do
             end do
          end do
  
          if(l_gsd_soilTQ_nudge) then
             ier=0
             call GSI_BundleGetPointer (GSI_MetGuess_Bundle(it),'smoist',ges_smois_it,istatus)
             ier=ier+istatus
             call GSI_BundleGetPointer (GSI_MetGuess_Bundle(it),'tslb'  ,ges_tslb_it ,istatus)
             ier=ier+istatus
             if (ier/=0) call die(trim(myname),'cannot get pointers for tslb/smois, ier =',ier)
             ksmois=i_smois-1
             ktslb=i_tslb-1
             do k=1,nsig_soil
                ksmois=ksmois+1
                ktslb=ktslb+1
                do i=1,lon2
                   do j=1,lat2
                      ges_smois_it(j,i,k) = real(all_loc(j,i,ksmois),r_kind)
                      ges_tslb_it(j,i,k) = real(all_loc(j,i,ktslb),r_kind)
                   enddo
                enddo
             enddo  ! k
             do i=1,lon2
                do j=1,lat2
                   soil_moi(j,i,it)=ges_smois_it(j,i,1)
                   soil_temp(j,i,it)=ges_tslb_it(j,i,1)
                enddo
             enddo
          else
             do i=1,lon2
                do j=1,lat2
                   soil_moi(j,i,it)=real(all_loc(j,i,i_smois),r_kind)
                   soil_temp(j,i,it)=real(all_loc(j,i,i_tslb),r_kind)
                enddo
             enddo
          endif
  
          do i=1,lon2
             do j=1,lat2
  
  !             NOTE:  MASS surface elevation is multiplied by g, so divide by g below
                ges_z_it(j,i) = all_loc(j,i,i_fis)/grav
  
  !             Convert psfc units of mb and then convert to log(psfc) in cb
                psfc_this_dry=r0_01*real(all_loc(j,i,i_mub)+all_loc(j,i,i_mu)+pt_regional_single,r_kind)
                psfc_this=(psfc_this_dry-pt_ll)*q_integral(j,i)+pt_ll+q_integralc4h(j,i)
                ges_ps_it(j,i)=one_tenth*psfc_this   ! convert from mb to cb
                sno(j,i,it)=real(all_loc(j,i,i_sno),r_kind)
  !GSD              soil_moi(j,i,it)=real(all_loc(j,i,i_smois),r_kind)
  !GSD              soil_temp(j,i,it)=real(all_loc(j,i,i_tslb),r_kind)
  ! for cloud analysis
                if(l_hydrometeor_bkio .and. n_actual_clouds>0) then
                   soil_temp_cld(j,i,it)=soil_temp(j,i,it)
                   ges_xlon(j,i,it)=real(all_loc(j,i,i_xlon),r_kind)/rad2deg_single
                   ges_xlat(j,i,it)=real(all_loc(j,i,i_xlat),r_kind)/rad2deg_single
                endif
                if(l_gsd_soilTQ_nudge) then
                   ges_tsk_it(j,i)=real(all_loc(j,i,i_tsk),r_kind)
                   ges_soilt1_it(j,i)=real(all_loc(j,i,i_soilt1),r_kind)
                endif
                if(i_use_2mt4b > 0 ) then
                   ges_t2m_it(j,i)=real(all_loc(j,i,i_th2),r_kind)
  ! convert from potential to sensible temperature
                   ges_t2m_it(j,i)=ges_t2m_it(j,i)*(ges_ps_it(j,i)/r100)**rd_over_cp_mass
                endif
                if(i_use_2mq4b>0) then
                  ges_q2_it(j,i)=real(all_loc(j,i,i_q2),r_kind)
  ! Convert 2m guess mixing ratio to specific humidity
                  ges_q2_it(j,i) = ges_q2_it(j,i)/(one+ges_q2_it(j,i))
                endif
  
             end do
          end do
  
  
  !       Convert potenital temperature to temperature.  Then convert
  !       sensible to virtual temperature
          do k=1,nsig
             do i=1,lon2
                do j=1,lat2
                   work_prsl  = one_tenth*(aeta1_ll(k)*(r10*ges_ps_it(j,i)-pt_ll)+aeta2_ll(k)+pt_ll)
                   work_prslk = (work_prsl/r100)**rd_over_cp_mass
                   ges_tsen(j,i,k,it)= ges_pot(j,i,k)*work_prslk
                   ges_tv_it(j,i,k) = ges_tsen(j,i,k,it) * (one+fv*ges_q_it(j,i,k))
                end do
             end do
          end do
  
  
  !    Transfer surface fields
          do i=1,lon2
             do j=1,lat2
                fact10(j,i,it)=one    !  later fix this by using correct w10/w(1)
                veg_type(j,i,it)=real(all_loc(j,i,i_ivgtyp),r_kind)
                veg_frac(j,i,it)=r0_01*real(all_loc(j,i,i_vegfrac),r_kind)
                soil_type(j,i,it)=real(all_loc(j,i,i_isltyp),r_kind)
                sm_this=zero
                if(all_loc(j,i,i_sm) /= zero_single) sm_this=one
                xice_this=zero
                if(all_loc(j,i,i_xice) /= zero_single) xice_this=one
                
                isli_this=0
                if(xice_this==one) isli_this=2
                if(xice_this==zero.and.sm_this==one) isli_this=1
                isli(j,i,it)=isli_this
  
  !?????????????????????????????????check to see if land skin temp is pot temp--if so, need to convert
                sfct(j,i,it)=real(all_loc(j,i,i_sst),r_kind)
                if(isli(j,i,it) /= 0) sfct(j,i,it)=real(all_loc(j,i,i_tsk),r_kind)
                if(sfct(j,i,it) < one) then
  
  !             For now, replace missing skin temps with 1st sigma level temp
                   sfct(j,i,it)=ges_tsen(j,i,1,it)
                   num_doubtful_sfct=num_doubtful_sfct+1
                   if(num_doubtful_sfct <= 100) &
                        write(6,*)'READ_WRF_MASS_BINARY_GUESS:  ',&
                        'doubtful skint replaced with 1st sigma level t, j,i,mype,sfct=',&
                        j,i,mype,sfct(j,i,it)
                end if
                sfc_rough(j,i,it)=rough_default
                if(l_hydrometeor_bkio .and. n_actual_clouds>0) then
                   isli_cld(j,i,it)=isli(j,i,it)
                endif
             end do
          end do
       end do
  
       
       call mpi_reduce(num_doubtful_sfct,num_doubtful_sfct_all,1,mpi_integer,mpi_sum,&
            0,mpi_comm_world,ierror)
       if(print_verbose)then
          write(6,*)' in read_wrf_mass_guess, num_doubtful_sfct_all = ',num_doubtful_sfct_all
          write(6,*)' in read_wrf_mass_guess, num_doubtful_sfct_all = ',num_doubtful_sfct_all
          write(6,*)' in read_wrf_mass_guess, min,max(sfct)=', &
            minval(sfct),maxval(sfct)
       end if
       
       deallocate(all_loc,igtype,kdim,kord)
  
  
       return
  end subroutine read_wrf_mass_binary_guess_wrf
  
  subroutine read_wrf_mass_netcdf_guess_wrf(this,mype)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    read_wrf_mass_guess      read wrf_mass interface file
  !   prgmmr: parrish          org: np22                date: 2003-09-05
  !
  ! abstract: in place of read_guess for global application, read guess
  !             from regional model, in this case the wrf mass core model.
  !             This version reads a binary file created
  !             in a previous step that interfaces with the wrf infrastructure.
  !             A later version will read directly from the wrf restart file.
  !             The guess is read in by complete horizontal fields, one field
  !             per processor, in parallel.  Each horizontal input field is 
  !             converted from the staggered c-grid to an unstaggered a-grid.
  !             On the c-grid, u is shifted 1/2 point in the negative x direction
  !             and v 1/2 point in the negative y direction, but otherwise the
  !             three grids are regular.  When the fields are read in, nothing
  !             is done to mass variables, but wind variables are interpolated to
  !             mass points.
  !
  ! program history log:
  !   2004--7-15  parrish
  !   2004-08-02  treadon - add only to module use, add intent in/out
  !   2004-09-10  parrish - correct error in land-sea mask interpretation
  !   2005-02-23  wu - setup for qoption=2 and output stats of RH
  !   2005-04-01  treadon - add initialization of ges_oz, prsi_oz; comestic format changes
  !   2005-05-27  parrish - add call get_derivatives
  !   2005-11-21  derber - make qoption=1 work same as qoption=2
  !   2005-11-29  derber - remove external iteration dependent calculations
  !   2005-12-15  treadon - remove initialization of certain guess arrays (done elsewhere)
  !   2006-02-02  treadon - remove load_prsges,load_geop_hgt,prsl,prslk (not used)
  !   2006-02-15  treadon - convert moisture mixing ratio to specific humidity
  !   2006-03-07  treadon - convert guess potential temperature to vritual temperature
  !   2006-04-06  middlecoff - changed nfcst from 11 to lendian_in
  !   2006-07-28  derber  - include sensible temperatures
  !   2006-07-31  kleist - change to use ges_ps instead of lnps
  !   2007-03-13  derber - remove unused qsinv2 from jfunc use list
  !   2008-04-16  safford - rm unused uses
  !   2010-03-29  hu     - add code to read in cloud/hydrometeor fields 
  !                             and distributed them to all processors 
  !   2011-04-29  todling - introduce MetGuess and wrf_mass_guess_mod
  !   2011-09-20  hclin   - added 15 wrfchem/gocart fields for aod
  !   2012-11-26  hu     - add code to read in soil fields
  !   2013-10-19  todling - metguess now holds background
  !   2014-03-12  hu     - add code to read ges_q2 (2m Q), 
  !                               Qnr(rain number concentration), 
  !                               and nsoil (number of soil levels)
  !   2015-01-13  ladwig - add code to read Qni and Qnc (cloud ice and water
  !                               number concentration)
  !   2017-03-23  Hu     - add code to read hybrid vertical coodinate in WRF MASS
  !
  !   2016-02-14 Johnson, Y. Wang, X. Wang  - add code to read vertical velocity (W) and
  !                                           Reflectivity (REFL_10CM) for radar
  !                                           DA, POC: xuguang.wang@ou.edu
  !   2016-09    CAPS(G. Zhao) - add checking and print-out for hydrometers reading-in
  !                            - convert hydrometers variables(qr,qs and qg) to log variables
  !                            - checking up for zero value of qr/qs/qg and reset to a tiny value
  !                              since reflectivity algorithm requires non-zero
  !                              hydrometer mixing ratio.
  !   2017-03    CAPS(G. Zhao) - tuning different tiny non-zero values assigned to qr/qs/qg
  !                              and considering the temperature effect on rain, snow and graupel
  !                              if T > 274.15
  !                                  qr_min=2.9E-6
  !                                  qs_min=0.0E-6
  !                                  qg_min=3.1E-7
  !                              if T < 274.15  and  T > 272.15
  !                                  qr_min=2.0E-6
  !                                  qs_min=1.3E-7
  !                                  qg_min=3.1E-7
  !                              if T < 272.15
  !                                  qr_min=0.0E-6
  !                                  qs_min=6.3E-6
  !                                  qg_min=3.1E-7
  !   2018-02-xx CAPS(G. Zhao) - only reset the value for log transformed qx
  !                              for regular qx, the non-zero tiny value is applied to
  !                              qx on obs point in setupdbz
  !   2020-09-13 CAPS(C. Liu, L. Chen, and H. Li) 
  !                            - add code for hydrometer variables needed for
  !                            direct reflectivity DA
  !                            - add CV transform option on hydrometer variables
  !   2022-03-15  Hu  change all th2 to t2m and convert 2m temperature 
  !                        from potentionl to senseible temperature
  !
  !   input argument list:
  !     mype     - pe number
  !
  !     NOTES:  need to pay special attention to various surface fields to make sure
  !             they are correct (check units).  fact10 needs to be computed from 
  !             10m wind, which is included in wrf mass interface file.
  !
  !             Cloud water currently set to zero.  Need to fix before doing precip
  !             assimilation--wait until global adopts Brad Ferrier's multi-species 
  !             scheme??
  !
  !             Ozone currently set to zero.  No ozone variable in wrf mass core--climatology
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
    use mpimod, only: mpi_sum,mpi_integer,mpi_real4,mpi_comm_world,npe,ierror
    use guess_grids, only: &
         fact10,soil_type,veg_frac,veg_type,sfct,sno,soil_temp,soil_moi,&
         isli,nfldsig,ifilesig,ges_tsen,sfc_rough,ntguessig
    use gridmod, only: lat2,lon2,nlat_regional,nlon_regional,&
         nsig,nsig_soil,ijn_s,displs_s,eta1_ll,pt_ll,itotsub,aeta1_ll,eta2_ll,aeta2_ll
    use constants, only: zero,one,grav,fv,zero_single,rd_over_cp_mass,one_tenth,r10,r100
    use constants, only: r0_01, tiny_r_kind,rd,r1000
    use gsi_io, only: lendian_in, verbose
    use chemmod, only: laeroana_gocart,nh4_mfac,oc_mfac,&
         aerotot_guess,init_aerotot_guess,wrf_pm2_5,aero_ratios
    use rapidrefresh_cldsurf_mod, only: l_hydrometeor_bkio,l_gsd_soiltq_nudge
    use rapidrefresh_cldsurf_mod, only: i_use_2mq4b,i_use_2mt4b
    use wrf_mass_guess_mod, only: soil_temp_cld,isli_cld,ges_xlon,ges_xlat,ges_tten,create_cld_grids
    use gsi_bundlemod, only: GSI_BundleGetPointer
    use gsi_metguess_mod, only: gsi_metguess_get,GSI_MetGuess_Bundle
    use gsi_chemguess_mod, only: GSI_ChemGuess_Bundle, gsi_chemguess_get
    use mpeu_util, only: die
    use guess_grids, only: ges_w_btlev
    use wrf_vars_mod, only : w_exist, dbz_exist
    use setupdbz_lib,only: hx_dart
    use obsmod,only: if_model_dbz
    use directDA_radaruse_mod, only: l_use_cvpqx, cvpqx_pval, l_use_dbz_directDA
    use directDA_radaruse_mod, only: l_cvpnr, cvpnr_pval                                          

    implicit none
    class(read_wrf_mass_guess_class),intent(inout) :: this
  
  ! Declare passed variables
    integer(i_kind),intent(in):: mype
  
  ! Declare local parameters
    character(len=*),parameter::myname='read_wrf_mass_netcdf_guess::'
    real(r_kind),parameter:: rough_default=0.05_r_kind
  
  ! Declare local variables
    integer(i_kind) kt,kq,ku,kv,kw,kw0,kdbz
  
  ! MASS variable names stuck in here
  
  ! other internal variables
    real(r_single) tempa(itotsub)
    real(r_single),allocatable::temp1(:,:),temp1u(:,:),temp1v(:,:)
    real(r_single),allocatable::all_loc(:,:,:)
    integer(i_kind),allocatable::itemp1(:,:)
    integer(i_kind),allocatable::igtype(:),jsig_skip(:)
    character(60),allocatable::identity(:)
    character(6) filename 
    integer(i_kind) irc_s_reg(npe),ird_s_reg(npe)
    integer(i_kind) ifld,im,jm,lm,num_mass_fields,num_mass_fields_base
    integer(i_kind) num_all_fields,num_loc_groups,num_all_pad
    integer(i_kind) i,icount,icount_prev,it,j,k
    integer(i_kind) i_0,i_psfc,i_fis,i_t,i_q,i_u,i_v,i_sno,i_u10,i_v10,i_smois,i_tslb
    integer(i_kind) i_sm,i_xice,i_sst,i_tsk,i_ivgtyp,i_isltyp,i_vegfrac
    integer(i_kind) isli_this
    real(r_kind) psfc_this,psfc_this_dry,sm_this,xice_this
    real(r_kind),dimension(lat2,lon2):: q_integral,q_integralc4h
    real(r_kind),dimension(lat2,lon2,nsig):: ges_pot
    integer(i_kind) num_doubtful_sfct,num_doubtful_sfct_all
    real(r_kind) deltasigma,deltasigmac4h
    real(r_kind):: work_prsl,work_prslk
    integer(i_kind),allocatable :: i_chem(:),kchem(:)
    integer(i_kind) i_qc,i_qi,i_qr,i_qs,i_qg,i_qnr,i_qni,i_qnc,i_w,i_dbz
    integer(i_kind) kqc,kqi,kqr,kqs,kqg,kqnr,kqni,kqnc,i_xlon,i_xlat,i_tt,ktt
    integer(i_kind) i_th2,i_q2,i_soilt1,ksmois,ktslb
    integer(i_kind) ier, istatus
    integer(i_kind) n_actual_clouds
    integer(i_kind) iv,n_gocart_var
    integer(i_kind) :: indx_sulf, indx_bc1, indx_bc2,  &
                       indx_oc1, indx_oc2, indx_dust1, indx_dust2, &
                       indx_dust3, indx_dust4, indx_dust5, &
                       indx_seas1, indx_seas2, indx_seas3, indx_seas4,indx_p25
    character(len=5),allocatable :: cvar(:)
    real(r_kind)   :: ges_rho, tsn

    real(r_kind), pointer :: ges_ps_it (:,:  )=>NULL()
    real(r_kind), pointer :: ges_t2m_it(:,:  )=>NULL()
    real(r_kind), pointer :: ges_q2_it (:,:  )=>NULL()
    real(r_kind), pointer :: ges_tsk_it(:,:  )=>NULL()
    real(r_kind), pointer :: ges_soilt1_it(:,:)=>NULL()
    real(r_kind), pointer :: ges_tslb_it(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_smois_it(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_z_it  (:,:  )=>NULL()
    real(r_kind), pointer :: ges_u_it  (:,:,:)=>NULL()
    real(r_kind), pointer :: ges_v_it  (:,:,:)=>NULL()
    real(r_kind), pointer :: ges_tv_it (:,:,:)=>NULL()
    real(r_kind), pointer :: ges_q_it  (:,:,:)=>NULL()
    real(r_kind), pointer :: ges_w_it  (:,:,:)=>NULL()
  
    real(r_kind), pointer :: ges_qc (:,:,:)=>NULL()
    real(r_kind), pointer :: ges_qi (:,:,:)=>NULL()
    real(r_kind), pointer :: ges_qr (:,:,:)=>NULL()
    real(r_kind), pointer :: ges_iqr (:,:,:)=>NULL()
    real(r_kind), pointer :: ges_qs (:,:,:)=>NULL()
    real(r_kind), pointer :: ges_qg (:,:,:)=>NULL()
    real(r_kind), pointer :: ges_qnr(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_qni(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_qnc(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_dbz(:,:,:)=>NULL()
  
    real(r_kind), pointer :: ges_sulf(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_bc1(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_bc2(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_oc1(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_oc2(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_dust1(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_dust2(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_dust3(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_dust4(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_dust5(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_seas1(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_seas2(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_seas3(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_seas4(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_p25(:,:,:)=>NULL()
    real(r_kind), pointer :: ges_pm2_5(:,:,:)=>NULL()
    logical print_verbose
    associate( this => this ) ! eliminates warning for unused dummy argument needed for binding
    end associate
  !  WRF MASS input grid dimensions in module gridmod
  !      These are the following:
  !          im -- number of x-points on C-grid
  !          jm -- number of y-points on C-grid
  !          lm -- number of vertical levels ( = nsig for now)
  
  
       print_verbose=.false. .and. mype == 0
       if(verbose .and. mype == 0)print_verbose=.true.
       num_doubtful_sfct=0
       if(print_verbose) write(6,*)' at 0 in read_wrf_mass_guess'
  
  
  ! Big section of operations done only on first outer iteration
  
  !    Inquire about cloud guess fields
       call gsi_metguess_get('clouds::3d',n_actual_clouds,istatus)
       if (n_actual_clouds>0) then
  !       Get pointer for each of the hydrometeors from guess at time index "it"
          it=ntguessig
          ier=0
          call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'ql', ges_qc, istatus );ier=ier+istatus
          call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qi', ges_qi, istatus );ier=ier+istatus
          call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qr', ges_qr, istatus );ier=ier+istatus
          if ( l_use_dbz_directDA ) call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'iqr', ges_iqr, istatus );ier=ier+istatus
          call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qs', ges_qs, istatus );ier=ier+istatus
          call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qg', ges_qg, istatus );ier=ier+istatus
          call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qnr',ges_qnr,istatus );ier=ier+istatus
          if ( .not.l_use_dbz_directDA) then
             call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qni',ges_qni,istatus );ier=ier+istatus
             call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qnc',ges_qnc,istatus );ier=ier+istatus
          end if
          if (ier/=0) n_actual_clouds=0
       end if
       if( dbz_exist )then
         call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'dbz',ges_dbz,istatus );ier=ier+istatus
       end if
       if (l_gsd_soilTQ_nudge) then
          ier=0
          call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'tskn', ges_tsk_it, istatus );ier=ier+istatus
          call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'tsoil',ges_soilt1_it,istatus);ier=ier+istatus
          if (ier/=0) call die(trim(myname),'cannot get pointers for met-fields, ier =',ier)
       endif
  
       im=nlon_regional
       jm=nlat_regional
       lm=nsig
  
  !    Following is for convenient WRF MASS input
       num_mass_fields_base=14+4*lm
       num_mass_fields=num_mass_fields_base
!    The 9 3D cloud analysis fields are: ql,qi,qr,qs,qg,qnr,qni,qnc,tt
       if ( l_use_dbz_directDA ) then ! direct reflectivity DA does not use qni and qnc
          if(l_hydrometeor_bkio .and. n_actual_clouds>0) num_mass_fields=num_mass_fields+7*lm+2
       else
          if(l_hydrometeor_bkio .and.n_actual_clouds>0) num_mass_fields=num_mass_fields+9*lm+2
       end if
       if(l_gsd_soilTQ_nudge) num_mass_fields=num_mass_fields+2*(nsig_soil-1)+1
       if(i_use_2mt4b > 0 ) num_mass_fields=num_mass_fields + 2
       if(i_use_2mq4b > 0 .and. i_use_2mt4b <=0 ) num_mass_fields=num_mass_fields + 1

       if (laeroana_gocart .and. wrf_pm2_5 ) then
          if(mype==0) write(6,*)'laeroana_gocart canoot be both true'
          call stop2(2)
       endif
  
  
       if ( laeroana_gocart ) then
          call gsi_chemguess_get ('aerosols::3d',n_gocart_var,istatus)
          if ( n_gocart_var > 0 ) then
             num_mass_fields = num_mass_fields + n_gocart_var*lm
          else
             laeroana_gocart = .false.
          endif
       endif
  
       if ( wrf_pm2_5 ) then
          num_mass_fields = num_mass_fields + lm
       endif

       if( w_exist) num_mass_fields = num_mass_fields + lm + 1
       if( dbz_exist.and.if_model_dbz ) num_mass_fields = num_mass_fields + lm
  
  
       num_all_fields=num_mass_fields*nfldsig
       num_loc_groups=num_all_fields/npe
       if(print_verbose)then
          write(6,'(" read_wrf_mass_guess: lm            =",i6)')lm
          write(6,'(" read_wrf_mass_guess: num_mass_fields=",i6)')num_mass_fields
          write(6,'(" read_wrf_mass_guess: nfldsig       =",i6)')nfldsig
          write(6,'(" read_wrf_mass_guess: num_all_fields=",i6)')num_all_fields
          write(6,'(" read_wrf_mass_guess: npe           =",i6)')npe
          write(6,'(" read_wrf_mass_guess: num_loc_groups=",i6)')num_loc_groups
       end if
       do 
          num_all_pad=num_loc_groups*npe
          if(num_all_pad >= num_all_fields) exit
          num_loc_groups=num_loc_groups+1
       end do
       if(print_verbose) then
          write(6,'(" read_wrf_mass_guess, num_all_pad   =",i6)')num_all_pad
          write(6,'(" read_wrf_mass_guess, num_loc_groups=",i6)')num_loc_groups
       end if
  
       allocate(all_loc(lat2,lon2,num_all_pad))
       allocate(jsig_skip(num_mass_fields))
       allocate(igtype(num_mass_fields))
       allocate(identity(num_mass_fields))
  
  !    initialize GSD specific guess fields
       call create_cld_grids()
  
  !    igtype is a flag indicating whether each input MASS field is h-, u-, or v-grid
  !    and whether integer or real
  !     abs(igtype)=1 for h-grid
  !                =2 for u-grid
  !                =3 for u-grid
  !     igtype < 0 for integer field
  
       i=0
  ! for cloud analysis
       if(l_hydrometeor_bkio .and. n_actual_clouds>0) then
          i=i+1 ; i_xlat=i                                                ! xlat
          write(identity(i),'("record ",i3,"--xlat")')i
          jsig_skip(i)=3     ! number of files to skip before getting to xlat
          igtype(i)=1
          i=i+1 ; i_xlon=i                                                ! xlon
          write(identity(i),'("record ",i3,"--xlon")')i
          jsig_skip(i)=0     ! 
          igtype(i)=1
       endif
  
       i=i+1 ; i_psfc=i                                                ! psfc
       write(identity(i),'("record ",i3,"--psfc")')i
       jsig_skip(i)=5     ! number of files to skip before getting to psfc
       if(l_hydrometeor_bkio .and. n_actual_clouds>0) jsig_skip(i)=0 ! number of files to skip before getting to psfc
       igtype(i)=1
       i=i+1 ; i_fis=i                                               ! sfc geopotential
       write(identity(i),'("record ",i3,"--fis")')i
       jsig_skip(i)=0
       igtype(i)=1
       i_t=i+1
       do k=1,lm
          i=i+1                                                       ! theta(k)  (pot temp)
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
          jsig_skip(i)=0 ; igtype(i)=3
       end do
       if(w_exist) then
         i_w=i+1
         do k=1,lm+1
           i=i+1                                                       ! w(k)
           write(identity(i),'("record ",i3,"--w(",i2,")")')i,k
           jsig_skip(i)=0 ; igtype(i)=1
         end do
       endif
       i=i+1   ; i_sm=i                                              ! landmask
       write(identity(i),'("record ",i3,"--sm")')i
       jsig_skip(i)=0 ; igtype(i)=1
       i=i+1 ; i_xice=i                                              ! xice
       write(identity(i),'("record ",i3,"--xice")')i
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
       if(l_gsd_soilTQ_nudge) then
          i_smois=i+1
          do k=1,nsig_soil
             i=i+1                                                      ! smois
             write(identity(i),'("record ",i3,"--smois(",i2,")")')i,k
             jsig_skip(i)=0 ; igtype(i)=1
          end do
          i_tslb=i + 1
          do k=1,nsig_soil
             i=i+1                                                       ! tslb
             write(identity(i),'("record ",i3,"--tslb(",i2,")")')i,k
             jsig_skip(i)=0 ; igtype(i)=1
          end do
       else
          i=i+1 ; i_smois=i                                             ! smois
          write(identity(i),'("record ",i3,"--smois(",i2,")")')i,k
          jsig_skip(i)=0 ; igtype(i)=1
          i=i+1 ; i_tslb=i                                              ! tslb
          write(identity(i),'("record ",i3,"--tslb(",i2,")")')i,k
          jsig_skip(i)=0 ; igtype(i)=1
       endif
       i=i+1 ; i_tsk=i                                               ! tsk
       write(identity(i),'("record ",i3,"--sst")')i
       jsig_skip(i)=0 ; igtype(i)=1
       if(i_use_2mq4b > 0 .or. i_use_2mt4b >0 ) then
          i=i+1 ; i_q2=i                                                ! q2
          write(identity(i),'("record ",i3,"--q2")')i
          jsig_skip(i)=0 ; igtype(i)=1
       endif
       if(l_gsd_soilTQ_nudge) then
          i=i+1 ; i_soilt1=i                                         ! soilt1
          write(identity(i),'("record ",i3,"--soilt1(",i2,")")')i,k
          jsig_skip(i)=0 ; igtype(i)=1
       endif
       if(i_use_2mt4b > 0 ) then
          i=i+1 ; i_th2=i                                            ! th2 
          write(identity(i),'("record ",i3,"--th2(",i2,")")')i,k
          jsig_skip(i)=0 ; igtype(i)=1
       endif
  ! for cloud array
       if(l_hydrometeor_bkio .and. n_actual_clouds>0) then
          i_qc=i+1
          do k=1,lm
             i=i+1                                                      ! qc(k)
             write(identity(i),'("record ",i3,"--qc(",i2,")")')i,k
             jsig_skip(i)=0 ; igtype(i)=1
          end do
          i_qr=i+1
          do k=1,lm
             i=i+1                                                    ! qi(k)
             write(identity(i),'("record ",i3,"--qr(",i2,")")')i,k
             jsig_skip(i)=0 ; igtype(i)=1
          end do
          i_qs=i+1
          do k=1,lm
             i=i+1                                                    ! qr(k)
             write(identity(i),'("record ",i3,"--qs(",i2,")")')i,k
             jsig_skip(i)=0 ; igtype(i)=1
          end do
          i_qi=i+1
          do k=1,lm
             i=i+1                                                    ! qs(k)
             write(identity(i),'("record ",i3,"--qi(",i2,")")')i,k
             jsig_skip(i)=0 ; igtype(i)=1
          end do
          i_qg=i+1
          do k=1,lm
             i=i+1                                                    ! qg(k)
             write(identity(i),'("record ",i3,"--qg(",i2,")")')i,k
             jsig_skip(i)=0 ; igtype(i)=1
          end do
          i_qnr=i+1
          do k=1,lm
             i=i+1                                                    !  qnr(k)
             write(identity(i),'("record ",i3,"--qnr(",i2,")")')i,k
             jsig_skip(i)=0 ; igtype(i)=1
          end do
          if ( .not. l_use_dbz_directDA) then ! direct reflectivity DA does not use qni and qnc
             i_qni=i+1
             do k=1,lm
                i=i+1                                                    ! qni(k)
                write(identity(i),'("record ",i3,"--qni(",i2,")")')i,k
                jsig_skip(i)=0 ; igtype(i)=1
             end do
             i_qnc=i+1
             do k=1,lm
                i=i+1                                                    ! qnc(k)
                write(identity(i),'("record ",i3,"--qnc(",i2,")")')i,k
                jsig_skip(i)=0 ; igtype(i)=1
             end do
          end if
          if( dbz_exist.and.if_model_dbz )then
            i_dbz=i+1
            do k=1,lm
             i=i+1                                                    ! dbz(k)
             write(identity(i),'("record ",i3,"--tt(",i2,")")')i,k
             jsig_skip(i)=0 ; igtype(i)=1
           end do
          end if
          i_tt=i+1
          do k=1,lm
             i=i+1                                                    ! tt(k)
             write(identity(i),'("record ",i3,"--tt(",i2,")")')i,k
             jsig_skip(i)=0 ; igtype(i)=1
          end do
       endif
  
       if ( laeroana_gocart ) then
          if (n_gocart_var >0) then
             allocate(cvar(n_gocart_var))
             call gsi_chemguess_get ('aerosols::3d',cvar,ier)
             allocate(i_chem(n_gocart_var))
             allocate(kchem(n_gocart_var))
             do iv = 1, n_gocart_var
                i_chem(iv)=i+1
                do k=1,lm
                  i=i+1
                  jsig_skip(i)=0 ; igtype(i)=1
                end do
             end do
          endif
       endif ! laeroana_gocart
  
       if ( wrf_pm2_5 ) then
          allocate(cvar(1))
          allocate(i_chem(1))
          allocate(kchem(1))
          iv=1
          i_chem(iv)=i+1
          do k=1,lm
             i=i+1
             jsig_skip(i)=0 ; igtype(i)=1
          end do
       endif
  
  
  !    End of stuff from MASS restart file
  
       allocate(temp1(im,jm),itemp1(im,jm),temp1u(im+1,jm),temp1v(im,jm+1))
       
       do i=1,npe
          irc_s_reg(i)=ijn_s(mype+1)
       end do
       ird_s_reg(1)=0
       do i=1,npe
          if(i /= 1) ird_s_reg(i)=ird_s_reg(i-1)+irc_s_reg(i-1)
       end do
       
  !    Read wrf MASS fixed format file created from external interface
  !    This is done by reading in parallel from every pe, and redistributing
  !    to local domains once for every npe fields read in, using 
  !    mpi_all_to_allv
  
       icount=0
       icount_prev=1
       do it=1,nfldsig
          write(filename,'("sigf",i2.2)')ifilesig(it)
          open(lendian_in,file=filename,form='unformatted') ; rewind lendian_in
          if(print_verbose)write(6,*)'READ_WRF_MASS_GUESS:  open lendian_in=',lendian_in,' to file=',filename
  
  !       Read, interpolate, and distribute MASS restart fields
          do ifld=1,num_mass_fields
             icount=icount+1
             if(jsig_skip(ifld) > 0) then
                do i=1,jsig_skip(ifld)
                   read(lendian_in)
                end do
             end if
             if(mype==mod(icount-1,npe)) then
                if(igtype(ifld)==1) then
                   read(lendian_in)((temp1(i,j),i=1,im),j=1,jm)
  !                write(6,'(" ifld, temp1(im/2,jm/2)=",i6,e15.5)')ifld,temp1(im/2,jm/2)
                   call fill_mass_grid2t(temp1,im,jm,tempa,1)
                end if
                if(igtype(ifld)==2) then
                   read(lendian_in)((temp1u(i,j),i=1,im+1),j=1,jm)
  !                write(6,'(" ifld, temp1u(im/2,jm/2)=",i6,e15.5)')ifld,temp1u(im/2,jm/2)
                   call fill_mass_grid2u(temp1u,im,jm,tempa,1)
                end if
                if(igtype(ifld)==3) then
                   read(lendian_in)((temp1v(i,j),i=1,im),j=1,jm+1)
  !                write(6,'(" ifld, temp1v(im/2,jm/2)=",i6,e15.5)')ifld,temp1v(im/2,jm/2)
                   call fill_mass_grid2v(temp1v,im,jm,tempa,1)
                end if
                if(igtype(ifld) < 0) then
                   read(lendian_in)((itemp1(i,j),i=1,im),j=1,jm)
                   do j=1,jm
                      do i=1,im
                         temp1(i,j)=itemp1(i,j)
                      end do
                   end do
  !                write(6,'(" ifld, temp1(im/2,jm/2)=",i6,e15.5)')ifld,temp1(im/2,jm/2)
                   call fill_mass_grid2t(temp1,im,jm,tempa,1)
                end if
             else
                read(lendian_in)
             end if
  
  !          Distribute to local domains everytime we have npe fields
             if(mod(icount,npe) == 0.or.icount==num_all_fields) then
                call mpi_alltoallv(tempa,ijn_s,displs_s,mpi_real4, &
                     all_loc(1,1,icount_prev),irc_s_reg,ird_s_reg,mpi_real4,mpi_comm_world,ierror)
                icount_prev=icount+1
             end if
          end do
          close(lendian_in)
       end do
  !    do kv=i_v,i_v+nsig-1
  !       if(mype==0) write(6,*)' at 1.15, kv,mype,j,i,v=', &
  !          kv,mype,2,1,all_loc(2,1,kv)
  !    end do
  
  
  !    Next do conversion of units as necessary and
  !    reorganize into WeiYu's format--
  
       do it=1,nfldsig
          i_0=(it-1)*num_mass_fields
          kt=i_0+i_t-1
          kq=i_0+i_q-1
          ku=i_0+i_u-1
          kv=i_0+i_v-1
          if(w_exist) kw=i_0+i_w-1
  
  ! typical meteorological fields
          ier=0
          call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'ps',ges_ps_it,istatus );ier=ier+istatus
          call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'z', ges_z_it, istatus );ier=ier+istatus
          call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'u', ges_u_it, istatus );ier=ier+istatus
          call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'v', ges_v_it, istatus );ier=ier+istatus
          if (w_exist) &
          call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'w', ges_w_it, istatus );ier=ier+istatus
          call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'tv',ges_tv_it,istatus );ier=ier+istatus
          call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'q' ,ges_q_it, istatus );ier=ier+istatus
          if (ier/=0) call die(trim(myname),'cannot get pointers for met-fields, ier =',ier)
          if(i_use_2mt4b > 0 .or. i_use_2mq4b > 0) then
             call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it),'q2m',ges_q2_it,istatus ); ier=ier+istatus
             if (ier/=0) call die(trim(myname),'cannot get pointers for q2m, ier =',ier)
          endif
          if (i_use_2mt4b > 0) then
             call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 't2m',ges_t2m_it, istatus );ier=ier+istatus
             if (ier/=0) call die(trim(myname),'cannot get pointers for t2m,ier =',ier)
          endif
          if (l_gsd_soilTQ_nudge) then
             call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'tskn',ges_tsk_it, istatus );ier=ier+istatus 
             call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'tsoil',ges_soilt1_it,istatus);ier=ier+istatus
             if (ier/=0) call die(trim(myname),'cannot get pointers for met-fields, ier =',ier)
          endif
  
  ! hydrometeors
          if(l_hydrometeor_bkio .and. n_actual_clouds>0) then
  !          Get pointer for each of the hydrometeors from guess at time index "it"
             call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'ql', ges_qc, istatus );ier=ier+istatus
             call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qi', ges_qi, istatus );ier=ier+istatus
             call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qr', ges_qr, istatus );ier=ier+istatus
             if ( l_use_dbz_directDA) call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'iqr', ges_iqr, istatus );ier=ier+istatus
             call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qs', ges_qs, istatus );ier=ier+istatus
             call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qg', ges_qg, istatus );ier=ier+istatus
             call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qnr',ges_qnr,istatus );ier=ier+istatus
             if ( .not.l_use_dbz_directDA ) then ! direct reflectivity DA  does not use qni and qnc
                call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qni',ges_qni,istatus );ier=ier+istatus
                call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qnc',ges_qnc,istatus );ier=ier+istatus
             end if
             kqc=i_0+i_qc-1
             kqr=i_0+i_qr-1
             kqs=i_0+i_qs-1
             kqi=i_0+i_qi-1
             kqg=i_0+i_qg-1
             kqnr=i_0+i_qnr-1
             if ( .not.l_use_dbz_directDA ) then ! direct reflectivity DA does not use qni and qnc
                kqni=i_0+i_qni-1
                kqnc=i_0+i_qnc-1
             end if
             ktt=i_0+i_tt-1
          endif
          if( dbz_exist ) then
            call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'dbz',ges_dbz,istatus );ier=ier+istatus
            if( if_model_dbz )kdbz=i_0+i_dbz-1
          end if
          if ( laeroana_gocart ) then
  
             if (aero_ratios) then 
                IF (mype==0) write(6,*) 'aero_ratios = .true. disabled - reset aero_ratios = .false. Aborting'
                call stop2(3)
             endif
  
             if (aero_ratios .and. it==1) call init_aerotot_guess()
  
             ier = 0
             indx_sulf=-1; indx_bc1=-1; indx_bc2=-1; indx_oc1=-1; indx_oc2=-1
             indx_dust1=-1;indx_dust2=-1;indx_dust3=-1; indx_dust4=-1;
             indx_dust5=-1;indx_seas1=-1;indx_seas2=-1; indx_seas3=-1
             indx_seas4=-1; indx_p25=-1
             do iv = 1, n_gocart_var
                if ( ier == 0 ) then
                   select case ( trim(cvar(iv)) )
                   case ( 'sulf' )
                      call GSI_BundleGetPointer(GSI_ChemGuess_Bundle(it),cvar(iv),ges_sulf,istatus);ier=ier+istatus                                                              
                      indx_sulf = iv
                   case ( 'bc1' )
                      call GSI_BundleGetPointer(GSI_ChemGuess_Bundle(it),cvar(iv),ges_bc1,istatus);ier=ier+istatus                                                              
                      indx_bc1 = iv
                   case ( 'bc2' )
                      call GSI_BundleGetPointer(GSI_ChemGuess_Bundle(it),cvar(iv),ges_bc2,istatus);ier=ier+istatus                                                              
                      indx_bc2 = iv
                   case ( 'oc1' )
                      call GSI_BundleGetPointer(GSI_ChemGuess_Bundle(it),cvar(iv),ges_oc1,istatus);ier=ier+istatus                                                              
                      indx_oc1 = iv
                   case ( 'oc2' )
                      call GSI_BundleGetPointer(GSI_ChemGuess_Bundle(it),cvar(iv),ges_oc2,istatus);ier=ier+istatus                                                              
                      indx_oc2 = iv
                   case ( 'dust1' )
                      call GSI_BundleGetPointer(GSI_ChemGuess_Bundle(it),cvar(iv),ges_dust1,istatus);ier=ier+istatus                                                              
                      indx_dust1 = iv
                   case ( 'dust2' )
                      call GSI_BundleGetPointer(GSI_ChemGuess_Bundle(it),cvar(iv),ges_dust2,istatus);ier=ier+istatus                                                              
                      indx_dust2 = iv
                   case ( 'dust3' )
                      call GSI_BundleGetPointer(GSI_ChemGuess_Bundle(it),cvar(iv),ges_dust3,istatus);ier=ier+istatus                                                              
                      indx_dust3 = iv
                   case ( 'dust4' )
                      call GSI_BundleGetPointer(GSI_ChemGuess_Bundle(it),cvar(iv),ges_dust4,istatus);ier=ier+istatus                                                              
                      indx_dust4 = iv
                   case ( 'dust5' )
                      call GSI_BundleGetPointer(GSI_ChemGuess_Bundle(it),cvar(iv),ges_dust5,istatus);ier=ier+istatus                                                              
                      indx_dust5 = iv
                   case ( 'seas1' )
                      call GSI_BundleGetPointer(GSI_ChemGuess_Bundle(it),cvar(iv),ges_seas1,istatus);ier=ier+istatus                                                              
                      indx_seas1 = iv
                   case ( 'seas2' )
                      call GSI_BundleGetPointer(GSI_ChemGuess_Bundle(it),cvar(iv),ges_seas2,istatus);ier=ier+istatus                                                              
                      indx_seas2 = iv
                   case ( 'seas3' )
                      call GSI_BundleGetPointer(GSI_ChemGuess_Bundle(it),cvar(iv),ges_seas3,istatus);ier=ier+istatus                                                              
                      indx_seas3 = iv
                   case ( 'seas4' )
                      call GSI_BundleGetPointer(GSI_ChemGuess_Bundle(it),cvar(iv),ges_seas4,istatus);ier=ier+istatus                                                              
                      indx_seas4 = iv
                   case ( 'p25' )
                      call GSI_BundleGetPointer(GSI_ChemGuess_Bundle(it),cvar(iv),ges_p25,istatus);ier=ier+istatus                                                              
                      indx_p25 = iv
                   end select
                endif
                if (ier/=0 .and. mype == 0) then
                    write(6,*)'READ_WRF_MASS_NETCDF_GUESS: getpointer failed ',  &
                              'for gocart species'
                endif
             enddo
             if ( n_gocart_var > 0 ) then
                do iv = 1, n_gocart_var
                   kchem(iv) = i_0+i_chem(iv)-1
                end do
             endif
          endif
  
          if ( wrf_pm2_5 ) then
             ier = 0
             iv=1
             cvar(1)='pm2_5'
  
             call GSI_BundleGetPointer(GSI_ChemGuess_Bundle(it),cvar(iv),ges_pm2_5,istatus)
             ier=ier+istatus
             if (ier/=0 .and. mype == 0) then
                write(6,*)'READ_WRF_MASS_NETCDF_GUESS: getpointer failed ',  &
                     'for pm2_5 species ',cvar(iv)
             endif
             kchem(iv) = i_0+i_chem(iv)-1
          endif
  
  
  !             wrf pressure variable is dry air partial pressure--need to add water vapor contribution
  !              so accumulate 1 + total water vapor to use as correction factor
  
          q_integral=one
          q_integralc4h=zero
          if(w_exist) kw0 = kw + 1
          do k=1,nsig
             deltasigma=eta1_ll(k)-eta1_ll(k+1)
             deltasigmac4h=eta2_ll(k)-eta2_ll(k+1)
             kt=kt+1
             kq=kq+1
             ku=ku+1
             kv=kv+1
             if(w_exist)  kw=kw+1
  ! hydrometeors
             if(l_hydrometeor_bkio .and. n_actual_clouds>0) then
                kqc=kqc+1
                kqr=kqr+1
                kqs=kqs+1
                kqi=kqi+1
                kqg=kqg+1
                kqnr=kqnr+1
                if ( .not.l_use_dbz_directDA ) then ! direct reflectivity DA does not use qni and qnc
                   kqni=kqni+1
                   kqnc=kqnc+1
                end if
                ktt=ktt+1
             endif
             if(dbz_exist.and.if_model_dbz) kdbz=kdbz+1
             if ( laeroana_gocart ) then
                if ( n_gocart_var > 0 ) then
                   do iv = 1, n_gocart_var
                      kchem(iv) = kchem(iv)+1
                   end do
                endif
             endif
  
             if ( wrf_pm2_5 ) then
                iv = 1
                kchem(iv) = kchem(iv)+1
             endif
  
             do i=1,lon2
                do j=1,lat2
                   ges_u_it(j,i,k) = real(all_loc(j,i,ku),r_kind)
                   ges_v_it(j,i,k) = real(all_loc(j,i,kv),r_kind)
                   ges_pot(j,i,k)  = real(all_loc(j,i,kt),r_kind)
                   ges_q_it(j,i,k) = real(all_loc(j,i,kq),r_kind)
                   q_integral(j,i) = q_integral(j,i)+deltasigma*ges_q_it(j,i,k)
                   q_integralc4h(j,i) = q_integralc4h(j,i)+deltasigmac4h*ges_q_it(j,i,k)
                   if(w_exist) then
                     ges_w_it(j,i,k) = 0.5*real((all_loc(j,i,kw)+all_loc(j,i,kw+1)),r_kind)
                   end if
  
  !                Convert guess mixing ratio to specific humidity
                   ges_q_it(j,i,k) = ges_q_it(j,i,k)/(one+ges_q_it(j,i,k))
  ! hydrometeors
                   if(l_hydrometeor_bkio .and. n_actual_clouds>0) then
                      ges_qc(j,i,k) = real(all_loc(j,i,kqc),r_kind)
                      ges_qi(j,i,k) = real(all_loc(j,i,kqi),r_kind)
                      ges_qr(j,i,k) = real(all_loc(j,i,kqr),r_kind)
                      if ( l_use_dbz_directDA) ges_iqr(j,i,k) = real(all_loc(j,i,kqr),r_kind)
                      ges_qs(j,i,k) = real(all_loc(j,i,kqs),r_kind)
                      ges_qg(j,i,k) = real(all_loc(j,i,kqg),r_kind)
                      ges_qnr(j,i,k)= real(all_loc(j,i,kqnr),r_kind)
                      if ( .not.l_use_dbz_directDA ) then ! direct reflectivity DA does not use qni and qnc
                         ges_qni(j,i,k)= real(all_loc(j,i,kqni),r_kind)
                         ges_qnc(j,i,k)= real(all_loc(j,i,kqnc),r_kind)
                      end if

  !                    ges_tten(j,i,k,it) = real(all_loc(j,i,ktt),r_kind)
                      ges_tten(j,i,k,it) = -20.0_r_single
                      if(k==nsig) ges_tten(j,i,k,it) = -10.0_r_single

                   endif
                   if(dbz_exist.and.if_model_dbz) ges_dbz(j,i,k) = real(all_loc(j,i,kdbz),r_kind)
                   if ( laeroana_gocart ) then
                      if (indx_sulf>0)  ges_sulf(j,i,k)  = real(all_loc(j,i,kchem(indx_sulf)),r_kind)
                      if (indx_bc1>0)   ges_bc1(j,i,k)   = real(all_loc(j,i,kchem(indx_bc1)),r_kind)
                      if (indx_bc2>0)   ges_bc2(j,i,k)   = real(all_loc(j,i,kchem(indx_bc2)),r_kind)
                      if (indx_oc1>0)   ges_oc1(j,i,k)   = real(all_loc(j,i,kchem(indx_oc1)),r_kind)
                      if (indx_oc2>0)   ges_oc2(j,i,k)   = real(all_loc(j,i,kchem(indx_oc2)),r_kind)
                      if (indx_dust1>0) ges_dust1(j,i,k) = real(all_loc(j,i,kchem(indx_dust1)),r_kind)
                      if (indx_dust2>0) ges_dust2(j,i,k) = real(all_loc(j,i,kchem(indx_dust2)),r_kind)
                      if (indx_dust3>0) ges_dust3(j,i,k) = real(all_loc(j,i,kchem(indx_dust3)),r_kind)
                      if (indx_dust4>0) ges_dust4(j,i,k) = real(all_loc(j,i,kchem(indx_dust4)),r_kind)
                      if (indx_dust5>0) ges_dust5(j,i,k) = real(all_loc(j,i,kchem(indx_dust5)),r_kind)
                      if (indx_seas1>0) ges_seas1(j,i,k) = real(all_loc(j,i,kchem(indx_seas1)),r_kind) 
                      if (indx_seas2>0) ges_seas2(j,i,k) = real(all_loc(j,i,kchem(indx_seas2)),r_kind) 
                      if (indx_seas3>0) ges_seas3(j,i,k) = real(all_loc(j,i,kchem(indx_seas3)),r_kind) 
                      if (indx_seas4>0) ges_seas4(j,i,k) = real(all_loc(j,i,kchem(indx_seas4)),r_kind) 
                      if (indx_p25>0)   ges_p25(j,i,k)   = real(all_loc(j,i,kchem(indx_p25)),r_kind)   
                      if (aero_ratios .and. it==1) then
                         aerotot_guess(j,i,k)=max(tiny_r_kind,&
                         ges_sulf(j,i,k)*nh4_mfac+&
                         ges_bc1(j,i,k)+&
                         ges_bc2(j,i,k)+&
                         ges_oc1(j,i,k)*oc_mfac+&
                         ges_oc2(j,i,k)*oc_mfac+&
                         ges_p25(j,i,k)+&
                         ges_dust1(j,i,k)+&
                         ges_dust2(j,i,k)+&
                         ges_dust3(j,i,k)+&
                         ges_dust4(j,i,k)+&
                         ges_dust5(j,i,k)+&
                         ges_seas1(j,i,k)+&
                         ges_seas2(j,i,k)+&
                         ges_seas3(j,i,k)+&
                         ges_seas4(j,i,k))
                      endif
  
                   end if
  
                   if ( wrf_pm2_5 ) then
                      iv=1
                      ges_pm2_5(j,i,k)  = real(all_loc(j,i,kchem(iv)),r_kind)
                   end if
                end do
             end do
          end do

          if ( laeroana_gocart ) then
             deallocate(i_chem)
             deallocate(kchem)
          endif
  
          if ( wrf_pm2_5 ) then
             deallocate(i_chem)
             deallocate(kchem)
          endif
  
  
          if(l_gsd_soilTQ_nudge) then
             ier=0
             call GSI_BundleGetPointer (GSI_MetGuess_Bundle(it),'smoist',ges_smois_it,istatus)
             ier=ier+istatus
             call GSI_BundleGetPointer (GSI_MetGuess_Bundle(it),'tslb'  ,ges_tslb_it ,istatus)
             ier=ier+istatus
             if (ier/=0) call die(trim(myname),'cannot get pointers for tslb/smois, ier =',ier)
             ksmois=i_0+i_smois-1
             ktslb=i_0+i_tslb-1
             do k=1,nsig_soil
                ksmois=ksmois+1
                ktslb=ktslb+1
                do i=1,lon2
                   do j=1,lat2
                      ges_smois_it(j,i,k) = real(all_loc(j,i,ksmois),r_kind)
                      ges_tslb_it(j,i,k) = real(all_loc(j,i,ktslb),r_kind)
                   enddo
                enddo
             enddo  ! k
             do i=1,lon2
                do j=1,lat2
                   soil_moi(j,i,it)=ges_smois_it(j,i,1)
                   soil_temp(j,i,it)=ges_tslb_it(j,i,1)
                enddo
             enddo
          else
             do i=1,lon2
                do j=1,lat2
                   soil_moi(j,i,it)=real(all_loc(j,i,i_0+i_smois),r_kind)
                   soil_temp(j,i,it)=real(all_loc(j,i,i_0+i_tslb),r_kind)
                enddo
             enddo
          endif

          if(w_exist) then
            do i=1,lon2
              do j=1,lat2
                 ges_w_btlev(j,i,1,it) = all_loc(j,i,kw0)
                 ges_w_btlev(j,i,2,it) = all_loc(j,i,kw+1)
              enddo
             enddo
          endif
  
          do i=1,lon2
             do j=1,lat2
  
  !             NOTE:  MASS surface elevation is multiplied by g, so divide by g below
                ges_z_it(j,i)    = real(all_loc(j,i,i_0+i_fis),r_kind)/grav
  
  !             Convert psfc units of mb and then convert to log(psfc) in cb
                psfc_this_dry=r0_01*real(all_loc(j,i,i_0+i_psfc),r_kind)
                psfc_this=(psfc_this_dry-pt_ll)*q_integral(j,i)+pt_ll+q_integralc4h(j,i)
                ges_ps_it(j,i)=one_tenth*psfc_this   ! convert from mb to cb
                sno(j,i,it)=real(all_loc(j,i,i_0+i_sno),r_kind)
                sfc_rough(j,i,it)=rough_default
                if(i_use_2mt4b > 0 ) then
                   ges_t2m_it(j,i)=real(all_loc(j,i,i_0+i_th2),r_kind)
  ! convert from potential to sensible temperature
                   ges_t2m_it(j,i)=ges_t2m_it(j,i)*(ges_ps_it(j,i)/r100)**rd_over_cp_mass
                endif
  ! for GSD soil nudging
                if(l_gsd_soilTQ_nudge) then
                   ges_tsk_it(j,i)=real(all_loc(j,i,i_0+i_tsk),r_kind)
                   ges_soilt1_it(j,i)=real(all_loc(j,i,i_0+i_soilt1),r_kind)
                endif
  ! Convert 2m guess mixing ratio to specific humidity
                if(i_use_2mt4b > 0 .or. i_use_2mq4b>0) then
                   ges_q2_it(j,i)=real(all_loc(j,i,i_0+i_q2),r_kind)
                   ges_q2_it(j,i)=ges_q2_it(j,i)/(one+ges_q2_it(j,i))
                endif
  ! for cloud analysis
                if(l_hydrometeor_bkio .and. n_actual_clouds>0) then
                   soil_temp_cld(j,i,it)=soil_temp(j,i,it)
                   ges_xlon(j,i,it)=real(all_loc(j,i,i_0+i_xlon),r_kind)
                   ges_xlat(j,i,it)=real(all_loc(j,i,i_0+i_xlat),r_kind)
                endif
  
             end do
          end do
          
          if(print_verbose) write(6,*)' in read_wrf_mass_guess, min,max(soil_moi)=', &
               minval(soil_moi),maxval(soil_moi)
          if(print_verbose) write(6,*)' in read_wrf_mass_guess, min,max(soil_temp)=', &
               minval(soil_temp),maxval(soil_temp)
  
  !       Convert potenital temperature to temperature
          do k=1,nsig
             do i=1,lon2
                do j=1,lat2
                   work_prsl  = one_tenth*(aeta1_ll(k)*(r10*ges_ps_it(j,i)-pt_ll)+aeta2_ll(k)+pt_ll)
                   work_prslk = (work_prsl/r100)**rd_over_cp_mass
                   ges_tsen(j,i,k,it)     = ges_pot(j,i,k)*work_prslk
                   ges_tv_it(j,i,k) = ges_tsen(j,i,k,it) * (one+fv*ges_q_it(j,i,k))
                   if( dbz_exist.and.(.not. if_model_dbz) )then
                     ges_rho = (work_prsl/(ges_tv_it(j,i,k)*rd))*r1000
                     tsn=ges_tv_it(j,i,k)/(one+fv*max(zero,ges_q_it(j,i,k)))
                     call hx_dart(ges_qr(j,i,k),ges_qg(j,i,k),ges_qs(j,i,k),ges_rho,tsn,ges_dbz(j,i,k),.false.)
                   end if
                end do
             end do
          end do

! --- direct reflectivity DA --- CV transform on hydrometeors
         if(l_hydrometeor_bkio .and. n_actual_clouds>0 .and. l_use_dbz_directDA) then
           call convert_qx_to_cvpqx(ges_qr, ges_qs, ges_qg, l_use_cvpqx, cvpqx_pval, it) ! convert Qx
           call convert_nx_to_cvpnx(ges_qnr, l_cvpnr, cvpnr_pval)                        ! convert Qnx
         end if
  
       end do  ! end of it loop
 
  !    Transfer surface fields
       do it=1,nfldsig
          i_0=(it-1)*num_mass_fields
          do i=1,lon2
             do j=1,lat2
                fact10(j,i,it)=one    !  later fix this by using correct w10/w(1)
                veg_type(j,i,it)=real(all_loc(j,i,i_0+i_ivgtyp),r_kind)
                veg_frac(j,i,it)=r0_01*real(all_loc(j,i,i_0+i_vegfrac),r_kind)
                soil_type(j,i,it)=real(all_loc(j,i,i_0+i_isltyp),r_kind)
                sm_this=zero
                if(all_loc(j,i,i_0+i_sm) /= zero_single) sm_this=one
                xice_this=zero
                if(all_loc(j,i,i_0+i_xice) /= zero_single) xice_this=one
                
                isli_this=0
                if(xice_this==one) isli_this=2
                if(xice_this==zero.and.sm_this==one) isli_this=1
                isli(j,i,it)=isli_this
                
  !?????????????????????????????????check to see if land skin temp is pot temp--if so, need to convert
                sfct(j,i,it)=real(all_loc(j,i,i_0+i_sst),r_kind)
                if(isli(j,i,it) /= 0) sfct(j,i,it)=real(all_loc(j,i,i_0+i_tsk),r_kind)
                if(sfct(j,i,it) < one) then
  
  !             For now, replace missing skin temps with 1st sigma level temp
                   sfct(j,i,it)=real(all_loc(j,i,i_0+i_t),r_kind) 
                   write(6,*)' doubtful skint replaced with 1st sigma level t, j,i,mype,sfct=',&
                        j,i,mype,sfct(j,i,it)
                   num_doubtful_sfct=num_doubtful_sfct+1
                end if
                if(l_hydrometeor_bkio .and. n_actual_clouds>0) then
                   isli_cld(j,i,it)=isli(j,i,it)
                endif
             end do
          end do
       end do
       
       call mpi_reduce(num_doubtful_sfct,num_doubtful_sfct_all,1,mpi_integer,mpi_sum,&
            0,mpi_comm_world,ierror)
       if(print_verbose) then
          write(6,*)' in read_wrf_mass_guess, num_doubtful_sfct_all = ',num_doubtful_sfct_all
          write(6,*)' in read_wrf_mass_guess, num_doubtful_sfct_all = ',num_doubtful_sfct_all
       else if(mype==10) then
          write(6,*)' in read_wrf_mass_guess, min,max(sfct)=', &
            minval(sfct),maxval(sfct)
          write(6,*)' in read_wrf_mass_guess, min,max(veg_type)=', &
            minval(veg_type),maxval(veg_type)
          write(6,*)' in read_wrf_mass_guess, min,max(veg_frac)=', &
            minval(veg_frac),maxval(veg_frac)
          write(6,*)' in read_wrf_mass_guess, min,max(soil_type)=', &
            minval(soil_type),maxval(soil_type)
          write(6,*)' in read_wrf_mass_guess, min,max(isli)=', &
            minval(isli),maxval(isli)
       end if
       
       deallocate(all_loc,jsig_skip,igtype,identity)
       deallocate(temp1,itemp1,temp1u,temp1v)
  
  
       return
  end subroutine read_wrf_mass_netcdf_guess_wrf

  subroutine generic_grid2sub(this,tempa,all_loc,kbegin_loc,kend_loc,kbegin,kend,mype,num_fields)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    generic_grid2sub   converts from full horizontal grid to subdomains
  !   prgmmr: parrish          org: np22                date: 2004-11-29
  !
  ! abstract: variation on subroutine grid2sub, with more general distribution of variables
  !              along the k index.
  !
  ! program history log:
  !   2004-02-03  kleist, new mpi strategy
  !   2004-05-06  derber
  !   2004-07-15  treadon - handle periodic subdomains
  !   2004-07-28  treadon - add only on use declarations; add intent in/out
  !   2004-10-26  kleist - u,v removed; periodicity accounted for only in
  !               sub2grid routine if necessary
  !   2004-11-29  parrish - adapt grid2sub for related use with mpi io.
  !   2013-01-26  parrish - WCOSS debug compile error -- change tempa from intent(in) to intent(inout)
  !
  !   input argument list:
  !     tempa    - input grid values in horizontal slab mode.
  !     kbegin_loc - starting k index for tempa on local processor
  !     kend_loc   - ending k index for tempa on local processor
  !     kbegin     - starting k indices for tempa for all processors
  !     kend       - ending k indices for tempa for all processors
  !     mype       - local processor number
  !     num_fields - total range of k index (1 <= k <= num_fields)
  !
  !   output argument list:
  !     all_loc  - output grid values in vertical subdomain mode
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$
  
    use mpimod, only: ierror,mpi_comm_world,mpi_real4,npe
    use gridmod, only: ijn_s,itotsub,lat2,lon2
    use kinds, only: r_single,i_kind
    implicit none
    
    class(read_wrf_mass_guess_class),intent(inout) :: this
    integer(i_kind),intent(in   ) :: kbegin_loc,kend_loc,mype,num_fields
    integer(i_kind),intent(in   ) :: kbegin(0:npe),kend(0:npe-1)
    real(r_single) ,intent(inout) :: tempa(itotsub,kbegin_loc:kend_loc)
    real(r_single) ,intent(  out) :: all_loc(lat2*lon2*num_fields)
    
    integer(i_kind) k
    integer(i_kind) sendcounts(0:npe-1),sdispls(0:npe),recvcounts(0:npe-1),rdispls(0:npe)
  
  ! first get alltoallv indices
    
    sdispls(0)=0
    do k=0,npe-1
       sendcounts(k)=ijn_s(k+1)*(kend_loc-kbegin_loc+1) 
       sdispls(k+1)=sdispls(k)+sendcounts(k)
    end do
    rdispls(0)=0
    do k=0,npe-1
       recvcounts(k)=ijn_s(mype+1)*(kend(k)-kbegin(k)+1)
       rdispls(k+1)=rdispls(k)+recvcounts(k)
    end do
    
  ! then call reorder2
  
    call this%reorder2_s(tempa,kend_loc-kbegin_loc+1)
  
  ! then alltoallv and i think we are done??
  
    call mpi_alltoallv(tempa,sendcounts,sdispls,mpi_real4, &
         all_loc,recvcounts,rdispls,mpi_real4,mpi_comm_world,ierror)
  
  end subroutine generic_grid2sub
  subroutine reorder2_s(work,k_in)
  !$$$  subprogram documentation block
  !                .      .    .
  ! subprogram:    reorder2_s
  !
  !   prgrmmr:  kleist           org: np20                date: 2004-01-25
  !
  ! abstract:  adapt reorder2 to single precision
  !
  ! program history log:
  !   2004-01-25  kleist
  !   2004-05-14  kleist, documentation
  !   2004-07-15  todling, protex-complaint prologue
  !   2004-11-29  parrish, adapt reorder2 to single precision
  !   2008-04-16  safford -- add subprogram doc block
  !
  !   input argument list:
  !     k_in    ! number of levs in work array
  !     work
  !
  !   output argument list:
  !     work
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm rs/6000 sp; sgi origin 2000; compaq/hp
  !
  !$$$
  
  ! !USES:
  
    use constants, only: zero_single
    use mpimod, only: npe
    use gridmod, only: ijn_s,itotsub
    use kinds, only: r_single,i_kind
    implicit none
    
  
  ! !INPUT PARAMETERS:
  
    integer(i_kind)                       ,intent(in   ) :: k_in    ! number of levs in work array
  
  ! !INPUT/OUTPUT PARAMETERS:
  
    real(r_single),dimension(itotsub,k_in),intent(inout) :: work
  
  
    integer(i_kind) iloc,iskip,i,k,n
    real(r_single),dimension(itotsub*k_in):: temp
  
  ! Zero out temp array
    do k=1,itotsub*k_in
       temp(k)=zero_single
    end do
    
  ! Load temp array in order of subdomains
    iloc=0
    iskip=0
    do n=1,npe
       if (n/=1) then
          iskip=iskip+ijn_s(n-1)
       end if
       
       do k=1,k_in
          do i=1,ijn_s(n)
             iloc=iloc+1
             temp(iloc)=work(iskip+i,k)
          end do
       end do
    end do
  
  ! Now load the tmp array back into work
    iloc=0
    do k=1,k_in
       do i=1,itotsub
          iloc=iloc+1
          work(i,k)=temp(iloc)
       end do
    end do
    
    return
  end subroutine reorder2_s

  subroutine expand_ibuf(ibuf,im,jm,imp,jmp)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    expand_ibuf    expand array in place
  !   prgmmr: parrish          org: np22                date: 2004-11-29
  !
  ! abstract: expand array in place from im,jm to imp,jmp
  !
  ! program history log:
  !   2004-11-29  parrish
  !   2007-04-12  parrish - replace im+1, jm+1 with inputs imp, jmp to allow
  !                           for use with u and v fields, where im=imp or jm=jmp
  !
  !   input argument list:
  !     ibuf     - input grid values in im,jm
  !     im       - first grid index
  !     jm       - second grid index
  !
  !   output argument list:
  !     ibuf     - output grid values in im+1,jm+1
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$
  
  !   field of dim im*jm read into array of dim imp*jmp--need to readjust
  
    use kinds, only: i_long,i_kind
    implicit none
    
    integer(i_kind),intent(in   ) :: im,jm,imp,jmp
    integer(i_long),intent(inout) :: ibuf(imp*jmp)
    
    integer(i_long) itemp(imp,jmp)
    integer(i_kind) i,ii,j
    
  
    do j=1,jmp
       do i=1,imp
          itemp(i,j)=0_i_long
       end do
    end do
    ii=0
    do j=1,jm
       do i=1,im
          ii=ii+1
          itemp(i,j)=ibuf(ii)
       end do
    end do
    
    ii=0
    do j=1,jmp
       do i=1,imp
          ii=ii+1
          ibuf(ii)=itemp(i,j)
       end do
    end do
  end subroutine expand_ibuf

  subroutine transfer_jbuf2ibuf(jbuf,jbegin_loc,jend_loc,ibuf,kbegin_loc,kend_loc, &
       jbegin,jend,kbegin,kend,mype,npe,im_jbuf,jm_jbuf,lm_jbuf, &
       im_ibuf,jm_ibuf,k_start,k_end)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    transfer_jbuf2ibuf   flip from ikj to ijk 
  !   prgmmr: parrish          org: np22                date: 2004-11-29
  !
  ! abstract: redistribute 3-d field from ikj order to ijk order across processors
  !
  ! program history log:
  !   2004-11-29  parrish
  !   2005-02-16  todling - replaced "use mpi" by use of mpimod
  !
  !   input argument list:
  !     jbuf     - input grid values distributed as all ik and a range of j on each processor
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
  !     ibuf     - output grid redistributed to ijk order (full i, full j, k_start <= k <= k_end)
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$
  
  !  flip around from ikj to ijk, moving result from jbuf to ibuf
  
    use mpimod, only: mpi_comm_world,mpi_integer
    use kinds, only: i_long,i_kind
    implicit none
    
    integer(i_kind),intent(in   ) :: jbegin_loc,jend_loc,kbegin_loc,kend_loc,mype,npe,im_jbuf,jm_jbuf,lm_jbuf
    integer(i_kind),intent(in   ) :: im_ibuf,jm_ibuf,k_start,k_end
    
    integer(i_long),intent(in   ) :: jbuf(im_jbuf,lm_jbuf,jbegin_loc:jend_loc)
    integer(i_long),intent(  out) :: ibuf(im_ibuf,jm_ibuf,kbegin_loc:kend_loc)
    integer(i_kind),intent(in   ) :: jbegin(0:npe),jend(0:npe-1)
    integer(i_kind),intent(in   ) :: kbegin(0:npe),kend(0:npe-1)
    
    integer(i_long) sendbuf(im_jbuf*lm_jbuf*(jend_loc-jbegin_loc+2))
    integer(i_long) recvbuf(im_jbuf*jm_jbuf*(kend_loc-kbegin_loc+1))
    integer(i_long) recvcounts(0:npe-1),displs(0:npe)
    integer(i_kind) i,ipe,j,ierror,k,n,ii,k_t_start,k_t_end,sendcount
    
    do ipe=0,npe-1
       k_t_start=max(k_start,kbegin(ipe))
       k_t_end=  min(k_end,kend(ipe))
       if(k_t_end < k_t_start) cycle
       
       displs(0)=0_i_long
       do i=0,npe-1
          recvcounts(i)=im_jbuf*(k_t_end-k_t_start+1)*(jend(i)-jbegin(i)+1)
          displs(i+1)=displs(i)+recvcounts(i)
       end do
       
  !   gather everything to ipe
       
       ii=0
       do k=k_t_start,k_t_end
          do j=jbegin_loc,jend_loc
             do i=1,im_jbuf
                ii=ii+1
                sendbuf(ii)=jbuf(i,k-k_start+1,j)
             end do
          end do
       end do
       sendcount=ii
       call mpi_gatherv(sendbuf,sendcount,mpi_integer,recvbuf,recvcounts, &
            displs,mpi_integer,ipe,mpi_comm_world,ierror)
       if(ipe==mype) then
          ii=0
          do n=0,npe-1
             do k=k_t_start,k_t_end
                do j=jbegin(n),jend(n)
                   do i=1,im_jbuf
                      ii=ii+1
                      ibuf(i,j,k)=recvbuf(ii)
                   end do
                end do
             end do
          end do
       end if
       
    end do
    
  end subroutine transfer_jbuf2ibuf
  subroutine move_ibuf_hg(ibuf,temp1,im_buf,jm_buf,im_out,jm_out)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    move_ibuf_hg  copy from one array to another
  !   prgmmr: parrish          org: np22                date: 2004-11-29
  !
  ! abstract: copy from one array to another
  !
  ! program history log:
  !   2004-11-29  parrish
  !
  !   input argument list:
  !     ibuf     - input grid values
  !     im_buf   - first index of input array buf
  !     jm_buf   - second index of input array buf
  !     im_out   - first index of output array temp1
  !     jm_out   - second index of output array temp1
  !
  !   output argument list:
  !     temp1    - output grid values
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$
  
  !        cp buf to temp1
  
    use kinds, only: r_single,i_kind,i_long
    use constants, only: zero_single
    implicit none
    
    integer(i_kind),intent(in   ) :: im_buf,jm_buf,im_out,jm_out
    integer(i_long),intent(in   ) :: ibuf(im_buf,jm_buf)
    real(r_single) ,intent(  out) :: temp1(im_out,jm_out)
  
    integer(i_kind) i,j
  
    do j=1,jm_out
       do i=1,im_out
          temp1(i,j)=transfer(ibuf(i,j),zero_single)
       end do
    end do
    
  end subroutine move_ibuf_hg
  
  subroutine move_ibuf_ihg(ibuf,temp1,im_buf,jm_buf,im_out,jm_out)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    move_ibuf_hg  copy from one array to another
  !   prgmmr: parrish          org: np22                date: 2004-11-29
  !
  ! abstract: copy from one array to another, converting from int to real
  !
  ! program history log:
  !   2004-11-29  parrish
  !
  !   input argument list:
  !     ibuf     - input grid values
  !     im_buf   - first index of input array buf
  !     jm_buf   - second index of input array buf
  !     im_out   - first index of output array temp1
  !     jm_out   - second index of output array temp1
  !
  !   output argument list:
  !     temp1    - output grid values
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$ end documentation block
  
  !        cp buf to temp1
  
    use kinds, only: i_long,r_single,i_kind
    implicit none
    
    integer(i_kind),intent(in   ) :: im_buf,jm_buf,im_out,jm_out
    integer(i_long),intent(in   ) :: ibuf(im_buf,jm_buf)
    real(r_single) ,intent(  out) :: temp1(im_out,jm_out)
    
    integer(i_kind) i,j
    
    do j=1,jm_out
       do i=1,im_out
          temp1(i,j)=ibuf(i,j)
       end do
    end do
    
  end subroutine move_ibuf_ihg

subroutine convert_qx_to_cvpqx(qr_arr,qs_arr,qg_arr,use_cvpqx,cvpqx_pvalue,it_val)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    convert_qx_to_cvpqx
!   prgmmr: J. Park(CAPS)                     date: 2021-05-05
!
! abstract: convert qx(mixing ratio) to cvpqx using power transform for qr, qs,
! qg
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
!     cvpqx_pvalue   - value to be used for power transform
!     it_val         - value from it loop
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
    use constants, only: zero, one_tenth

    implicit none
    real(r_kind), intent(inout  ) :: qr_arr(lat2,lon2,nsig)
    real(r_kind), intent(inout  ) :: qs_arr(lat2,lon2,nsig)
    real(r_kind), intent(inout  ) :: qg_arr(lat2,lon2,nsig)
    logical,      intent(in     ) :: use_cvpqx
    real(r_kind), intent(in     ) :: cvpqx_pvalue
    integer(i_kind), intent(in    ) :: it_val

    integer(i_kind)                   :: i, j, k, it

    real(r_kind) :: qr_min, qs_min, qg_min
    real(r_kind) :: qr_thrshd, qs_thrshd, qg_thrshd
!
    it=it_val
!

!   print info message: CVq, CVlogq, and CVpq
    if(mype==0)then
       if (use_cvpqx) then
          if ( cvpqx_pvalue == 0._r_kind ) then        ! CVlogq
              write(6,*)'read_wrf_mass_netcdf_guess_wrf: ',     &
                        ' reset zero of qr/qs/qg to specified values (~0dbz)', &
                        'before log transformation. (for dbz assimilation)'
              write(6,*)'read_wrf_mass_netcdf_guess_wrf: convert qr/qs/qg to log transform.'
          else if ( cvpqx_pvalue > 0._r_kind ) then   ! CVpq
              write(6,*)'read_wrf_mass_netcdf_guess_wrf: convert qr/qs/qg with power transform .'
          end if
       else                                         ! CVq
          write(6,*)'read_wrf_mass_netcdf_guess_wrf: only reset (qr/qs/qg) to &
                     0.0 for negative analysis value. (regular qx)'
       end if
    end if


    do k=1,nsig
      do i=1,lon2
        do j=1,lat2
!         Apply power transform if option is ON 
          if (use_cvpqx) then
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
       write(6,*)'read_wrf_mass_netcdf_guess_wrf: convert qnx with power transform .'
    end if

    do k=1,nsig
      do i=1,lon2
        do j=1,lat2

!       Treatment on qnx ; power transform
        if (cvpnr) then
           qnx_arr(j,i,k)=((max(qnx_arr(j,i,k),1.0E-2_r_kind)**cvpnr_pvalue)-1)/cvpnr_pvalue
        endif

        end do
      end do
    end do
end subroutine convert_nx_to_cvpnx

end module read_wrf_mass_guess_mod

#ifdef WRF
subroutine read_wrf_mass_binary_guess(mype)
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
       nsig,nsig_soil,eta1_ll,pt_ll,itotsub,aeta1_ll
  use constants, only: zero,one,grav,fv,zero_single,rd_over_cp_mass,one_tenth,h300,r10,r100
  use constants, only: r0_01
  use gsi_io, only: lendian_in
  use rapidrefresh_cldsurf_mod, only: l_cloud_analysis,l_gsd_soilTQ_nudge,i_use_2mq4b
  use wrf_mass_guess_mod, only: soil_temp_cld,isli_cld,ges_xlon,ges_xlat,ges_tten,create_cld_grids
  use gsi_bundlemod, only: GSI_BundleGetPointer
  use gsi_metguess_mod, only: gsi_metguess_get,GSI_MetGuess_Bundle
  use native_endianness, only: byte_swap
  use mpeu_util, only: die
  implicit none

! Declare passed variables
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
  real(r_kind),dimension(lat2,lon2):: q_integral
  real(r_kind),dimension(lat2,lon2,nsig):: ges_pot
  integer(i_kind) num_doubtful_sfct,num_doubtful_sfct_all
  real(r_kind) deltasigma
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
  integer(i_kind) i_qc,i_qi,i_qr,i_qs,i_qg,i_qnr
  integer(i_kind) kqc,kqi,kqr,kqs,kqg,kqnr,i_xlon,i_xlat,i_tt,ktt
  integer(i_kind) i_th2,i_q2,i_soilt1,ksmois,ktslb
  integer(i_kind) ier, istatus
  integer(i_kind) n_actual_clouds

  real(r_kind), pointer :: ges_ps_it (:,:  )=>NULL()
  real(r_kind), pointer :: ges_th2_it(:,:  )=>NULL()
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

  integer(i_kind) iadd
  character(132) memoryorder

!  WRF MASS input grid dimensions in module gridmod
!      These are the following:
!          im -- number of x-points on C-grid
!          jm -- number of y-points on C-grid
!          lm -- number of vertical levels ( = nsig for now)

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

     if(mype==0) write(6,*)' in read_wrf_mass_binary_guess, im,jm,lm=',im,jm,lm

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
        if (ier/=0) n_actual_clouds=0
     end if

!    Following is for convenient WRF MASS input
     num_mass_fields=14+5*lm+2*nsig_soil
     if(l_cloud_analysis .or. n_actual_clouds>0) num_mass_fields=num_mass_fields+7*lm+2
     if(l_gsd_soilTQ_nudge) num_mass_fields=num_mass_fields+2
     num_loc_groups=num_mass_fields/npe
     if(mype==0) write(6,'(" at 1 in read_wrf_mass_guess, lm            =",i6)')lm
     if(mype==0) write(6,'(" at 1 in read_wrf_mass_guess, nsig_soil     =",i6)')nsig_soil
     if(mype==0) write(6,'(" at 1 in read_wrf_mass_guess, num_mass_fields=",i6)')num_mass_fields
     if(mype==0) write(6,'(" at 1 in read_wrf_mass_guess, nfldsig       =",i6)')nfldsig
     if(mype==0) write(6,'(" at 1 in read_wrf_mass_guess, npe           =",i6)')npe
     if(mype==0) write(6,'(" at 1 in read_wrf_mass_guess, num_loc_groups=",i6)')num_loc_groups

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
        write(6,*)'READ_WRF_MASS_BINARY_GUESS:  open lendian_in=',lendian_in,&
             ' to filename=',filename,' on it=',it
        if(mype == 0) write(6,*)'READ_WRF_MASS_OFFSET_FILE:  open lendian_in=',lendian_in,' to file=',filename
        read(lendian_in) dummy9,pt_regional_single
        write(6,*)'READ_WRF_MASS_BINARY_GUESS:  dummy9=',dummy9

! get pointers for typical meteorological fields
        ier=0
        call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'ps',ges_ps_it,istatus );ier=ier+istatus
        call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'z', ges_z_it, istatus );ier=ier+istatus
        call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'u', ges_u_it, istatus );ier=ier+istatus
        call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'v', ges_v_it, istatus );ier=ier+istatus
        call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'tv',ges_tv_it,istatus );ier=ier+istatus
        call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'q' ,ges_q_it, istatus );ier=ier+istatus
        if (ier/=0) call die(trim(myname),'cannot get pointers for met-fields, ier =',ier)

        if (l_gsd_soilTQ_nudge) then
           call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'tskn', ges_tsk_it,istatus );ier=ier+istatus
           call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'th2m', ges_th2_it,istatus );ier=ier+istatus
           call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'tsoil',ges_soilt1_it,istatus );ier=ier+istatus
           if (ier/=0) call die(trim(myname),'cannot get pointers for met-fields, ier =',ier)
        endif
        if (i_use_2mq4b>0) then
           call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'q2m'  ,ges_q2_it,istatus);ier=ier+istatus
           if (ier/=0) call die(trim(myname),'cannot get pointers for q2m, ier =',ier)
        endif

! for cloud analysis
        if(l_cloud_analysis .or. n_actual_clouds>0) then

! get pointer to relevant instance of cloud-related background
           ier=0
           call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'ql', ges_qc, istatus );ier=ier+istatus
           call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qi', ges_qi, istatus );ier=ier+istatus
           call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qr', ges_qr, istatus );ier=ier+istatus
           call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qs', ges_qs, istatus );ier=ier+istatus
           call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qg', ges_qg, istatus );ier=ier+istatus
           call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qnr',ges_qnr,istatus );ier=ier+istatus
           if (ier/=0 .and. mype == 0) then
               write(6,*)'READ_WRF_MASS_BINARY_GUESS: getpointer failed, cannot do cloud analysis'
               l_cloud_analysis=.false.
           endif

           i=0
           allocate(tempa(nlon_regional,nlat_regional))
           do iskip=2,3
              read(lendian_in)
           end do
           i=i+1 ; i_xlat=i                                                ! xlat
           read(lendian_in) tempa,tempa,n_position
           offset(i)=n_position ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=1
           if(mype == 0) write(6,*)' xlat, i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
 
           i=i+1 ; i_xlon=i                                                ! xlon
           read(lendian_in) tempa,tempa,n_position
           offset(i)=n_position ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=1
           if(mype == 0) write(6,*)' xlon, i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
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
        if(mype == 0) write(6,*)'READ_WRF_MASS_BINARY_GUESS:  read wrfges= ',wrfges
        
        i=i+1 ; i_mub=i                                                ! mub
        read(lendian_in) n_position
        offset(i)=n_position ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=1
        if(mype == 0) write(6,*)' mub, i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
        
        i=i+1 ; i_mu =i                                                ! mu
        read(lendian_in) n_position
        offset(i)=n_position ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=1
        if(mype == 0) write(6,*)' mu, i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
        
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
           if(mype == 0.and.k==1) write(6,*)' sfc geopot i,igtype(i),offset(i),kord(i) = ', &
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
           if(mype == 0.and.k==1) write(6,*)' temp i,igtype(i),offset(i),kord(i) = ', &
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
           if(mype == 0.and.k==1) write(6,*)' q i,igtype(i),offset(i),kord(i) = ', &
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
           if(mype == 0.and.k==1) write(6,*)' u i,igtype(i),offset(i),kord(i) = ', &
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
           if(mype == 0.and.k==1) write(6,*)' v i,igtype(i),offset(i),kord(i) = ', &
                                                             i,igtype(i),offset(i),kord(i)
        end do
        
        i=i+1   ; i_sm=i                                              ! landmask
        read(lendian_in) n_position
        offset(i)=n_position ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=1
        if(mype == 0) write(6,*)' landmask i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
        
        i=i+1 ; i_xice=i                                              ! xice
        read(lendian_in) n_position
        offset(i)=n_position ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=1
        if(mype == 0) write(6,*)' xice i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
        
        i=i+1 ; i_sst=i                                               ! sst
        read(lendian_in) n_position
        offset(i)=n_position ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=1
        if(mype == 0) write(6,*)' sst i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
        
        i=i+1 ; i_ivgtyp=i                                            ! ivgtyp
        read(lendian_in) n_position
        offset(i)=n_position ; length(i)=im*jm ; igtype(i)=-1 ; kdim(i)=1
        if(mype == 0) write(6,*)' ivgtyp i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
        
        i=i+1 ; i_isltyp=i                                            ! isltyp
        read(lendian_in) n_position
        offset(i)=n_position ; length(i)=im*jm ; igtype(i)=-1 ; kdim(i)=1
        if(mype == 0) write(6,*)' isltyp i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
        
        i=i+1 ; i_vegfrac=i                                           ! vegfrac
        read(lendian_in) n_position
        offset(i)=n_position ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=1
        if(mype == 0) write(6,*)' vegfrac i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
        
        i=i+1 ; i_sno=i                                               ! sno
        read(lendian_in) n_position
        offset(i)=n_position ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=1
        if(mype == 0) write(6,*)' sno i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
        
        i=i+1 ; i_u10=i                                               ! u10
        read(lendian_in) n_position
        offset(i)=n_position ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=1
        if(mype == 0) write(6,*)' u10 i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
        
        i=i+1 ; i_v10=i                                               ! v10
        read(lendian_in) n_position
        offset(i)=n_position ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=1
        if(mype == 0) write(6,*)' v10 i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
        
        i=i+1 ; i_smois=i                                             ! smois
        read(lendian_in) n_position,ksize,memoryorder
        if(trim(memoryorder)=='XZY') then
           kord(i)=ksize
        else
           kord(i)=1
        end if
        offset(i)=n_position ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=ksize
        if(mype == 0) write(6,*)' smois i,igtype(i),offset(i),kord(i) = ', &
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
        if(mype == 0) write(6,*)' tslb i,igtype(i),offset(i),kord(i) = ', &
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
           if(mype == 0) write(6,*)' i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
        end do

        i=i+1 ; i_tsk=i                                               ! tsk

        read(lendian_in) n_position
        offset(i)=n_position ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=1
        if(mype == 0) write(6,*)' tsk i,igtype(i),offset(i) = ',i,igtype(i),offset(i)

        i=i+1 ; i_q2=i                                               ! q2 
        read(lendian_in) n_position
        offset(i)=n_position ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=1
        if(mype == 0) write(6,*)' q2 i,igtype(i),offset(i) = ',i,igtype(i),offset(i)

        if(l_gsd_soilTQ_nudge) then
           i=i+1 ; i_soilt1=i                                               ! soilt1
           read(lendian_in) n_position
           offset(i)=n_position ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=1
           if(mype == 0) write(6,*)' soilt1 i,igtype(i),offset(i) = ',i,igtype(i),offset(i)

           i=i+1 ; i_th2=i                                               ! th2
           read(lendian_in) n_position
           offset(i)=n_position ; length(i)=im*jm ; igtype(i)=1 ; kdim(i)=1
           if(mype == 0) write(6,*)' th2 i,igtype(i),offset(i) = ',i,igtype(i),offset(i)
        endif

! for cloud array
        if(l_cloud_analysis .or. n_actual_clouds>0) then

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
              if(mype == 0.and.k==1) write(6,*)' qc i,igtype(i),offset(i),kord(i) = ', &
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
              if(mype == 0.and.k==1) write(6,*)' qi i,igtype(i),offset(i),kord(i) = ', &
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
              if(mype == 0.and.k==1) write(6,*)' qs i,igtype(i),offset(i),kord(i) = ', &
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
              if(mype == 0.and.k==1) write(6,*)' qg i,igtype(i),offset(i),kord(i) = ', &
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
              if(mype == 0.and.k==1) write(6,*)' qr i,igtype(i),offset(i),kord(i) = ', &
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
              if(mype == 0.and.k==1) write(6,*)' tt i,igtype(i),offset(i),kord(i) = ', &
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
        if(mype == 0) then
           write(6,*)' kbegin=',kbegin
           write(6,*)' kend= ',kend
        end if
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
        if(mype == 0) then
           write(6,*)' jbegin=',jbegin
           write(6,*)' jend= ',jend
        end if
        
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
           call transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
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
           call transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
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
           call transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
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
           call transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
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
           call transfer_jbuf2ibuf(jbuf,jbegin(mype),jend2(mype),ibuf,kbegin(mype),kend(mype), &
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
           call transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
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
           call transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                jbegin,jend,kbegin,kend,mype,npe,im,jm,ksize,im+1,jm+1,i_tslb,i_tslb+ksize-1)
           deallocate(jbuf)
        end if

! for cloud analysis
        if(l_cloud_analysis .or. n_actual_clouds>0) then
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
              call transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
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
              call transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
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
              call transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
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
              call transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
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
              call transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
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
              call transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                 jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im+1,jm+1,i_qnr,i_qnr+lm-1)
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
              call transfer_jbuf2ibuf(jbuf,jbegin(mype),jend(mype),ibuf,kbegin(mype),kend(mype), &
                 jbegin,jend,kbegin,kend,mype,npe,im,jm,lm,im+1,jm+1,i_tt,i_tt+lm-1)
              deallocate(jbuf)
           end if

        endif  ! l_cloud_analysis

!---------------------- read surface files last
        do k=kbegin(mype),kend(mype)
           if(kdim(k)==1.or.kord(k)==1) then
              call mpi_file_read_at(mfcst,offset(k),ibuf(1,k),length(k),mpi_integer,status,ierror)
           if(byte_swap) then
              num_swap=length(k)
              call to_native_endianness_i4(ibuf(1,k),num_swap)
           end if
              if(igtype(k)==1) call expand_ibuf(ibuf(1,k),im  ,jm  ,im+1,jm+1)
              if(igtype(k)==-1)call expand_ibuf(ibuf(1,k),im  ,jm  ,im+1,jm+1)
              if(igtype(k)==2) call expand_ibuf(ibuf(1,k),im+1,jm  ,im+1,jm+1)
              if(igtype(k)==3) call expand_ibuf(ibuf(1,k),im  ,jm+1,im+1,jm+1)
           end if
        end do

        call mpi_file_close(mfcst,ierror)
    
!   next interpolate to analysis grid, then distribute to subdomains
        
        allocate(temp1(im,jm),temp1u(im+1,jm),temp1v(im,jm+1))
        allocate(tempa(itotsub,kbegin(mype):kend(mype)))
        do ifld=kbegin(mype),kend(mype)
           if(igtype(ifld) ==  1) then
              call move_ibuf_hg(ibuf(1,ifld),temp1,im+1,jm+1,im,jm)
              call fill_mass_grid2t(temp1,im,jm,tempa(1,ifld),1)
           else if(igtype(ifld) == -1) then
              call move_ibuf_ihg(ibuf(1,ifld),temp1,im+1,jm+1,im,jm)
              call fill_mass_grid2t(temp1,im,jm,tempa(1,ifld),1)
           else if(igtype(ifld) == 2) then
              call move_ibuf_hg(ibuf(1,ifld),temp1u,im+1,jm+1,im+1,jm)
              call fill_mass_grid2u(temp1u,im,jm,tempa(1,ifld),1)
           else if(igtype(ifld) == 3) then
              call move_ibuf_hg(ibuf(1,ifld),temp1v,im+1,jm+1,im,jm+1)
              call fill_mass_grid2v(temp1v,im,jm,tempa(1,ifld),1)
           end if
        end do
        deallocate(ibuf)
        deallocate(temp1,temp1u,temp1v)
        allocate(all_loc(lat2,lon2,num_mass_fields))
        call generic_grid2sub(tempa,all_loc,kbegin(mype),kend(mype),kbegin,kend,mype,num_mass_fields)
        

!    Next do conversion of units as necessary and
!    reorganize into WeiYu's format--

        kt=i_t-1
        kq=i_q-1
        ku=i_u-1
        kv=i_v-1
! hydrometeors
        if(l_cloud_analysis .or. n_actual_clouds>0) then
           kqc=i_qc-1
           kqr=i_qr-1
           kqs=i_qs-1
           kqi=i_qi-1
           kqg=i_qg-1
           kqnr=i_qnr-1
           ktt=i_tt-1
        endif
!             wrf pressure variable is dry air partial pressure--need to add water vapor contribution
!              so accumulate 1 + total water vapor to use as correction factor

        q_integral=one
        do k=1,nsig
           deltasigma=eta1_ll(k)-eta1_ll(k+1)
           kt=kt+1
           kq=kq+1
           ku=ku+1
           kv=kv+1
! hydrometeors
           if(l_cloud_analysis .or. n_actual_clouds>0) then
              kqc=kqc+1
              kqr=kqr+1
              kqs=kqs+1
              kqi=kqi+1
              kqg=kqg+1
              kqnr=kqnr+1
              ktt=ktt+1
           endif
           do i=1,lon2
              do j=1,lat2
                 ges_u_it(j,i,k) = all_loc(j,i,ku)
                 ges_v_it(j,i,k) = all_loc(j,i,kv)
                 ges_q_it(j,i,k) = all_loc(j,i,kq)
                 q_integral(j,i) = q_integral(j,i)+deltasigma*ges_q_it(j,i,k)

!                Convert guess mixing ratio to specific humidity
                 ges_q_it(j,i,k) = ges_q_it(j,i,k)/(one+ges_q_it(j,i,k))

!                Add offset to get guess potential temperature
                 ges_pot(j,i,k)  = all_loc(j,i,kt) + h300
! hydrometeors
                 if(l_cloud_analysis .or. n_actual_clouds>0) then
                    ges_qc(j,i,k) = all_loc(j,i,kqc)
                    ges_qi(j,i,k) = all_loc(j,i,kqi)
                    ges_qr(j,i,k) = all_loc(j,i,kqr)
                    ges_qs(j,i,k) = all_loc(j,i,kqs)
                    ges_qg(j,i,k) = all_loc(j,i,kqg)
                    ges_qnr(j,i,k)= all_loc(j,i,kqnr)
!                    ges_tten(j,i,k,it) = all_loc(j,i,ktt)
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
                    ges_smois_it(j,i,k) = all_loc(j,i,ksmois)
                    ges_tslb_it(j,i,k) = all_loc(j,i,ktslb)
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
                 soil_moi(j,i,it)=all_loc(j,i,i_smois)
                 soil_temp(j,i,it)=all_loc(j,i,i_tslb)
              enddo
           enddo
        endif

        do i=1,lon2
           do j=1,lat2

!             NOTE:  MASS surface elevation is multiplied by g, so divide by g below
              ges_z_it(j,i) = all_loc(j,i,i_fis)/grav

!             Convert psfc units of mb and then convert to log(psfc) in cb
              psfc_this_dry=r0_01*(all_loc(j,i,i_mub)+all_loc(j,i,i_mu)+pt_regional_single)
              psfc_this=(psfc_this_dry-pt_ll)*q_integral(j,i)+pt_ll
              ges_ps_it(j,i)=one_tenth*psfc_this   ! convert from mb to cb
              sno(j,i,it)=all_loc(j,i,i_sno)
!GSD              soil_moi(j,i,it)=all_loc(j,i,i_smois)
!GSD              soil_temp(j,i,it)=all_loc(j,i,i_tslb)
! for cloud analysis
              if(l_cloud_analysis .or. n_actual_clouds>0) then
                 soil_temp_cld(j,i,it)=soil_temp(j,i,it)
                 ges_xlon(j,i,it)=all_loc(j,i,i_xlon)/rad2deg_single
                 ges_xlat(j,i,it)=all_loc(j,i,i_xlat)/rad2deg_single
              endif
              if(l_gsd_soilTQ_nudge) then
                 ges_th2_it(j,i)=all_loc(j,i,i_th2)
                 ges_tsk_it(j,i)=all_loc(j,i,i_tsk)
                 ges_soilt1_it(j,i)=all_loc(j,i,i_soilt1)
              endif
              if(i_use_2mq4b>0) then
                ges_q2_it(j,i)=all_loc(j,i,i_q2)
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
                 work_prsl  = one_tenth*(aeta1_ll(k)*(r10*ges_ps_it(j,i)-pt_ll)+pt_ll)
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
              veg_type(j,i,it)=all_loc(j,i,i_ivgtyp)
              veg_frac(j,i,it)=r0_01*all_loc(j,i,i_vegfrac)
              soil_type(j,i,it)=all_loc(j,i,i_isltyp)
              sm_this=zero
              if(all_loc(j,i,i_sm) /= zero_single) sm_this=one
              xice_this=zero
              if(all_loc(j,i,i_xice) /= zero_single) xice_this=one
              
              isli_this=0
              if(xice_this==one) isli_this=2
              if(xice_this==zero.and.sm_this==one) isli_this=1
              isli(j,i,it)=isli_this

!?????????????????????????????????check to see if land skin temp is pot temp--if so, need to convert
              sfct(j,i,it)=all_loc(j,i,i_sst)
              if(isli(j,i,it) /= 0) sfct(j,i,it)=all_loc(j,i,i_tsk)
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
              if(l_cloud_analysis .or. n_actual_clouds>0) then
                 isli_cld(j,i,it)=isli(j,i,it)
              endif
           end do
        end do
     end do

     
     call mpi_reduce(num_doubtful_sfct,num_doubtful_sfct_all,1,mpi_integer,mpi_sum,&
          0,mpi_comm_world,ierror)
     if(mype==0) write(6,*)' in read_wrf_mass_guess, num_doubtful_sfct_all = ',num_doubtful_sfct_all
     if(mype==0) write(6,*)' in read_wrf_mass_guess, num_doubtful_sfct_all = ',num_doubtful_sfct_all
     if(mype==10) write(6,*)' in read_wrf_mass_guess, min,max(sfct)=', &
          minval(sfct),maxval(sfct)
     
     deallocate(all_loc,igtype,kdim,kord)


     return
end subroutine read_wrf_mass_binary_guess

subroutine read_wrf_mass_netcdf_guess(mype)
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
       nsig,nsig_soil,ijn_s,displs_s,eta1_ll,pt_ll,itotsub,aeta1_ll
  use constants, only: zero,one,grav,fv,zero_single,rd_over_cp_mass,one_tenth,r10,r100
  use constants, only: r0_01, tiny_r_kind
  use gsi_io, only: lendian_in
  use chemmod, only: laeroana_gocart,nh4_mfac,oc_mfac,&
       aerotot_guess,init_aerotot_guess,wrf_pm2_5,aero_ratios
  use rapidrefresh_cldsurf_mod, only: l_cloud_analysis,l_gsd_soiltq_nudge,i_use_2mq4b
  use wrf_mass_guess_mod, only: soil_temp_cld,isli_cld,ges_xlon,ges_xlat,ges_tten,create_cld_grids
  use gsi_bundlemod, only: GSI_BundleGetPointer
  use gsi_metguess_mod, only: gsi_metguess_get,GSI_MetGuess_Bundle
  use gsi_chemguess_mod, only: GSI_ChemGuess_Bundle, gsi_chemguess_get
  use mpeu_util, only: die
  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: mype

! Declare local parameters
  character(len=*),parameter::myname='read_wrf_mass_netcdf_guess::'
  real(r_kind),parameter:: rough_default=0.05_r_kind

! Declare local variables
  integer(i_kind) kt,kq,ku,kv

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
  integer(i_kind) ifld,im,jm,lm,num_mass_fields
  integer(i_kind) num_all_fields,num_loc_groups,num_all_pad
  integer(i_kind) i,icount,icount_prev,it,j,k
  integer(i_kind) i_0,i_psfc,i_fis,i_t,i_q,i_u,i_v,i_sno,i_u10,i_v10,i_smois,i_tslb
  integer(i_kind) i_sm,i_xice,i_sst,i_tsk,i_ivgtyp,i_isltyp,i_vegfrac
  integer(i_kind) isli_this
  real(r_kind) psfc_this,psfc_this_dry,sm_this,xice_this
  real(r_kind),dimension(lat2,lon2):: q_integral
  real(r_kind),dimension(lat2,lon2,nsig):: ges_pot
  integer(i_kind) num_doubtful_sfct,num_doubtful_sfct_all
  real(r_kind) deltasigma
  real(r_kind):: work_prsl,work_prslk
  integer(i_kind),allocatable :: i_chem(:),kchem(:)
  integer(i_kind) i_qc,i_qi,i_qr,i_qs,i_qg,i_qnr
  integer(i_kind) kqc,kqi,kqr,kqs,kqg,kqnr,i_xlon,i_xlat,i_tt,ktt
  integer(i_kind) i_th2,i_q2,i_soilt1,ksmois,ktslb
  integer(i_kind) ier, istatus
  integer(i_kind) n_actual_clouds
  integer(i_kind) iv,n_gocart_var
  integer(i_kind) :: indx_sulf, indx_bc1, indx_bc2,  &
                     indx_oc1, indx_oc2, indx_dust1, indx_dust2, &
                     indx_dust3, indx_dust4, indx_dust5, &
                     indx_seas1, indx_seas2, indx_seas3, indx_seas4,indx_p25
  character(len=5),allocatable :: cvar(:)

  real(r_kind), pointer :: ges_ps_it (:,:  )=>NULL()
  real(r_kind), pointer :: ges_th2_it(:,:  )=>NULL()
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

!  WRF MASS input grid dimensions in module gridmod
!      These are the following:
!          im -- number of x-points on C-grid
!          jm -- number of y-points on C-grid
!          lm -- number of vertical levels ( = nsig for now)


     num_doubtful_sfct=0
     if(mype==0) write(6,*)' at 0 in read_wrf_mass_guess'


! Big section of operations done only on first outer iteration

     if(mype==0) write(6,*)' at 0.1 in read_wrf_mass_guess'

!    Inquire about cloud guess fields
     call gsi_metguess_get('clouds::3d',n_actual_clouds,istatus)
     if (n_actual_clouds>0) then
!       Get pointer for each of the hydrometeors from guess at time index "it"
        it=ntguessig
        ier=0
        call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'ql', ges_qc, istatus );ier=ier+istatus
        call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qi', ges_qi, istatus );ier=ier+istatus
        call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qr', ges_qr, istatus );ier=ier+istatus
        call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qs', ges_qs, istatus );ier=ier+istatus
        call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qg', ges_qg, istatus );ier=ier+istatus
        call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qnr',ges_qnr,istatus );ier=ier+istatus
        if (ier/=0) n_actual_clouds=0
     end if
     if (l_gsd_soilTQ_nudge) then
        ier=0
        call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'th2m', ges_th2_it, istatus );ier=ier+istatus
        call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'tskn', ges_tsk_it, istatus );ier=ier+istatus
        call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'tsoil',ges_soilt1_it,istatus);ier=ier+istatus
        if (ier/=0) call die(trim(myname),'cannot get pointers for met-fields, ier =',ier)
     endif

     im=nlon_regional
     jm=nlat_regional
     lm=nsig

!    Following is for convenient WRF MASS input
     num_mass_fields=15+4*lm
     if(l_cloud_analysis .or.n_actual_clouds>0) num_mass_fields=15+4*lm+7*lm+2
     if(l_gsd_soilTQ_nudge) num_mass_fields=15+4*lm+2*nsig_soil
     if(l_gsd_soilTQ_nudge .and. l_cloud_analysis) &
                          num_mass_fields=15+4*lm+7*lm+2+2*nsig_soil

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


     num_all_fields=num_mass_fields*nfldsig
     num_loc_groups=num_all_fields/npe
     if(mype==0) write(6,'(" at 1 in read_wrf_mass_guess, lm            =",i6)')lm
     if(mype==0) write(6,'(" at 1 in read_wrf_mass_guess, num_mass_fields=",i6)')num_mass_fields
     if(mype==0) write(6,'(" at 1 in read_wrf_mass_guess, nfldsig       =",i6)')nfldsig
     if(mype==0) write(6,'(" at 1 in read_wrf_mass_guess, num_all_fields=",i6)')num_all_fields
     if(mype==0) write(6,'(" at 1 in read_wrf_mass_guess, npe           =",i6)')npe
     if(mype==0) write(6,'(" at 1 in read_wrf_mass_guess, num_loc_groups=",i6)')num_loc_groups
     do 
        num_all_pad=num_loc_groups*npe
        if(num_all_pad >= num_all_fields) exit
        num_loc_groups=num_loc_groups+1
     end do
     if(mype==0) write(6,'(" at 1 in read_wrf_mass_guess, num_all_pad   =",i6)')num_all_pad
     if(mype==0) write(6,'(" at 1 in read_wrf_mass_guess, num_loc_groups=",i6)')num_loc_groups

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
     if(l_cloud_analysis .or. n_actual_clouds>0) then
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
     if(l_cloud_analysis .or. n_actual_clouds>0) jsig_skip(i)=0 ! number of files to skip before getting to psfc
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
     i=i+1 ; i_q2=i                                                ! q2
     write(identity(i),'("record ",i3,"--q2")')i
     jsig_skip(i)=0 ; igtype(i)=1
     if(l_gsd_soilTQ_nudge) then
        i=i+1 ; i_soilt1=i                                         ! soilt1
        write(identity(i),'("record ",i3,"--soilt1(",i2,")")')i,k
        jsig_skip(i)=0 ; igtype(i)=1
        i=i+1 ; i_th2=i                                            ! th2 
        write(identity(i),'("record ",i3,"--th2(",i2,")")')i,k
        jsig_skip(i)=0 ; igtype(i)=1
     endif
! for cloud array
     if(l_cloud_analysis .or. n_actual_clouds>0) then
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
           write(identity(i),'("record ",i3,"--qr(",i2,")")')i,k
           jsig_skip(i)=0 ; igtype(i)=1
        end do
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
        write(6,*)'READ_WRF_MASS_GUESS:  open lendian_in=',lendian_in,' to file=',filename

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

! typical meteorological fields
        ier=0
        call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'ps',ges_ps_it,istatus );ier=ier+istatus
        call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'z', ges_z_it, istatus );ier=ier+istatus
        call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'u', ges_u_it, istatus );ier=ier+istatus
        call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'v', ges_v_it, istatus );ier=ier+istatus
        call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'tv',ges_tv_it,istatus );ier=ier+istatus
        call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'q' ,ges_q_it, istatus );ier=ier+istatus
        if (ier/=0) call die(trim(myname),'cannot get pointers for met-fields, ier =',ier)
        if(i_use_2mq4b>0) then
           call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it),'q2m',ges_q2_it,istatus ); ier=ier+istatus
           if (ier/=0) call die(trim(myname),'cannot get pointers for q2m, ier =',ier)
        endif
        if (l_gsd_soilTQ_nudge) then
           call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'th2m',ges_th2_it, istatus );ier=ier+istatus
           call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'tskn',ges_tsk_it, istatus );ier=ier+istatus 
           call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'tsoil',ges_soilt1_it,istatus);ier=ier+istatus
           if (ier/=0) call die(trim(myname),'cannot get pointers for met-fields, ier =',ier)
        endif

! hydrometeors
        if(l_cloud_analysis .or. n_actual_clouds>0) then
!          Get pointer for each of the hydrometeors from guess at time index "it"
           call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'ql', ges_qc, istatus );ier=ier+istatus
           call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qi', ges_qi, istatus );ier=ier+istatus
           call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qr', ges_qr, istatus );ier=ier+istatus
           call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qs', ges_qs, istatus );ier=ier+istatus
           call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qg', ges_qg, istatus );ier=ier+istatus
           call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qnr',ges_qnr,istatus );ier=ier+istatus
           kqc=i_0+i_qc-1
           kqr=i_0+i_qr-1
           kqs=i_0+i_qs-1
           kqi=i_0+i_qi-1
           kqg=i_0+i_qg-1
           kqnr=i_0+i_qnr-1
           ktt=i_0+i_tt-1
        endif
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
        do k=1,nsig
           deltasigma=eta1_ll(k)-eta1_ll(k+1)
           kt=kt+1
           kq=kq+1
           ku=ku+1
           kv=kv+1
! hydrometeors
           if(l_cloud_analysis .or. n_actual_clouds>0) then
              kqc=kqc+1
              kqr=kqr+1
              kqs=kqs+1
              kqi=kqi+1
              kqg=kqg+1
              kqnr=kqnr+1
              ktt=ktt+1
           endif
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
                 ges_u_it(j,i,k) = all_loc(j,i,ku)
                 ges_v_it(j,i,k) = all_loc(j,i,kv)
                 ges_pot(j,i,k)  = all_loc(j,i,kt)
                 ges_q_it(j,i,k) = all_loc(j,i,kq)
                 q_integral(j,i) = q_integral(j,i)+deltasigma*ges_q_it(j,i,k)

!                Convert guess mixing ratio to specific humidity
                 ges_q_it(j,i,k) = ges_q_it(j,i,k)/(one+ges_q_it(j,i,k))
! hydrometeors
                 if(l_cloud_analysis .or. n_actual_clouds>0) then
                    ges_qc(j,i,k) = all_loc(j,i,kqc)
                    ges_qi(j,i,k) = all_loc(j,i,kqi)
                    ges_qr(j,i,k) = all_loc(j,i,kqr)
                    ges_qs(j,i,k) = all_loc(j,i,kqs)
                    ges_qg(j,i,k) = all_loc(j,i,kqg)
                    ges_qnr(j,i,k)= all_loc(j,i,kqnr)
!                    ges_tten(j,i,k,it) = all_loc(j,i,ktt)
                    ges_tten(j,i,k,it) = -20.0_r_single
                    if(k==nsig) ges_tten(j,i,k,it) = -10.0_r_single

                 endif
                 if ( laeroana_gocart ) then
                    if (indx_sulf>0)  ges_sulf(j,i,k)  = all_loc(j,i,kchem(indx_sulf))
                    if (indx_bc1>0)   ges_bc1(j,i,k)   = all_loc(j,i,kchem(indx_bc1))  
                    if (indx_bc2>0)   ges_bc2(j,i,k)   = all_loc(j,i,kchem(indx_bc2))  
                    if (indx_oc1>0)   ges_oc1(j,i,k)   = all_loc(j,i,kchem(indx_oc1))  
                    if (indx_oc2>0)   ges_oc2(j,i,k)   = all_loc(j,i,kchem(indx_oc2))  
                    if (indx_dust1>0) ges_dust1(j,i,k) = all_loc(j,i,kchem(indx_dust1))
                    if (indx_dust2>0) ges_dust2(j,i,k) = all_loc(j,i,kchem(indx_dust2))
                    if (indx_dust3>0) ges_dust3(j,i,k) = all_loc(j,i,kchem(indx_dust3))
                    if (indx_dust4>0) ges_dust4(j,i,k) = all_loc(j,i,kchem(indx_dust4))
                    if (indx_dust5>0) ges_dust5(j,i,k) = all_loc(j,i,kchem(indx_dust5))
                    if (indx_seas1>0) ges_seas1(j,i,k) = all_loc(j,i,kchem(indx_seas1)) 
                    if (indx_seas2>0) ges_seas2(j,i,k) = all_loc(j,i,kchem(indx_seas2)) 
                    if (indx_seas3>0) ges_seas3(j,i,k) = all_loc(j,i,kchem(indx_seas3)) 
                    if (indx_seas4>0) ges_seas4(j,i,k) = all_loc(j,i,kchem(indx_seas4)) 
                    if (indx_p25>0)   ges_p25(j,i,k)   = all_loc(j,i,kchem(indx_p25))   
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
                    ges_pm2_5(j,i,k)  = all_loc(j,i,kchem(iv))
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
                    ges_smois_it(j,i,k) = all_loc(j,i,ksmois)
                    ges_tslb_it(j,i,k) = all_loc(j,i,ktslb)
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
                 soil_moi(j,i,it)=all_loc(j,i,i_0+i_smois)
                 soil_temp(j,i,it)=all_loc(j,i,i_0+i_tslb)
              enddo
           enddo
        endif

        do i=1,lon2
           do j=1,lat2

!             NOTE:  MASS surface elevation is multiplied by g, so divide by g below
              ges_z_it(j,i)    = all_loc(j,i,i_0+i_fis)/grav

!             Convert psfc units of mb and then convert to log(psfc) in cb
              psfc_this_dry=r0_01*all_loc(j,i,i_0+i_psfc)
              psfc_this=(psfc_this_dry-pt_ll)*q_integral(j,i)+pt_ll
              ges_ps_it(j,i)=one_tenth*psfc_this   ! convert from mb to cb
              sno(j,i,it)=all_loc(j,i,i_0+i_sno)
              sfc_rough(j,i,it)=rough_default
! for GSD soil nudging
              if(l_gsd_soilTQ_nudge) then
                 ges_th2_it(j,i)=all_loc(j,i,i_0+i_th2)
                 ges_tsk_it(j,i)=all_loc(j,i,i_0+i_tsk)
                 ges_soilt1_it(j,i)=all_loc(j,i,i_0+i_soilt1)
              endif
! Convert 2m guess mixing ratio to specific humidity
              if(i_use_2mq4b>0) then
                 ges_q2_it(j,i)=all_loc(j,i,i_0+i_q2)
                 ges_q2_it(j,i)=ges_q2_it(j,i)/(one+ges_q2_it(j,i))
              endif
! for cloud analysis
              if(l_cloud_analysis .or. n_actual_clouds>0) then
                 soil_temp_cld(j,i,it)=soil_temp(j,i,it)
                 ges_xlon(j,i,it)=all_loc(j,i,i_0+i_xlon)
                 ges_xlat(j,i,it)=all_loc(j,i,i_0+i_xlat)
              endif

           end do
        end do
        
        if(mype==10) write(6,*)' in read_wrf_mass_guess, min,max(soil_moi)=', &
             minval(soil_moi),maxval(soil_moi)
        if(mype==10) write(6,*)' in read_wrf_mass_guess, min,max(soil_temp)=', &
             minval(soil_temp),maxval(soil_temp)

!       Convert potenital temperature to temperature
        do k=1,nsig
           do i=1,lon2
              do j=1,lat2
                 work_prsl  = one_tenth*(aeta1_ll(k)*(r10*ges_ps_it(j,i)-pt_ll)+pt_ll)
                 work_prslk = (work_prsl/r100)**rd_over_cp_mass
                 ges_tsen(j,i,k,it)     = ges_pot(j,i,k)*work_prslk
                 ges_tv_it(j,i,k) = ges_tsen(j,i,k,it) * (one+fv*ges_q_it(j,i,k))
              end do
           end do
        end do
     end do

!    Transfer surface fields
     do it=1,nfldsig
        i_0=(it-1)*num_mass_fields
        do i=1,lon2
           do j=1,lat2
              fact10(j,i,it)=one    !  later fix this by using correct w10/w(1)
              veg_type(j,i,it)=all_loc(j,i,i_0+i_ivgtyp)
              veg_frac(j,i,it)=r0_01*all_loc(j,i,i_0+i_vegfrac)
              soil_type(j,i,it)=all_loc(j,i,i_0+i_isltyp)
              sm_this=zero
              if(all_loc(j,i,i_0+i_sm) /= zero_single) sm_this=one
              xice_this=zero
              if(all_loc(j,i,i_0+i_xice) /= zero_single) xice_this=one
              
              isli_this=0
              if(xice_this==one) isli_this=2
              if(xice_this==zero.and.sm_this==one) isli_this=1
              isli(j,i,it)=isli_this
              
!?????????????????????????????????check to see if land skin temp is pot temp--if so, need to convert
              sfct(j,i,it)=all_loc(j,i,i_0+i_sst)
              if(isli(j,i,it) /= 0) sfct(j,i,it)=all_loc(j,i,i_0+i_tsk)
              if(sfct(j,i,it) < one) then

!             For now, replace missing skin temps with 1st sigma level temp
                 sfct(j,i,it)=all_loc(j,i,i_0+i_t) 
                 write(6,*)' doubtful skint replaced with 1st sigma level t, j,i,mype,sfct=',&
                      j,i,mype,sfct(j,i,it)
                 num_doubtful_sfct=num_doubtful_sfct+1
              end if
              if(l_cloud_analysis .or. n_actual_clouds>0) then
                 isli_cld(j,i,it)=isli(j,i,it)
              endif
           end do
        end do
     end do
     
     call mpi_reduce(num_doubtful_sfct,num_doubtful_sfct_all,1,mpi_integer,mpi_sum,&
          0,mpi_comm_world,ierror)
     if(mype==0) write(6,*)' in read_wrf_mass_guess, num_doubtful_sfct_all = ',num_doubtful_sfct_all
     if(mype==0) write(6,*)' in read_wrf_mass_guess, num_doubtful_sfct_all = ',num_doubtful_sfct_all
     if(mype==10) write(6,*)' in read_wrf_mass_guess, min,max(sfct)=', &
          minval(sfct),maxval(sfct)
     if(mype==10) write(6,*)' in read_wrf_mass_guess, min,max(veg_type)=', &
          minval(veg_type),maxval(veg_type)
     if(mype==10) write(6,*)' in read_wrf_mass_guess, min,max(veg_frac)=', &
          minval(veg_frac),maxval(veg_frac)
     if(mype==10) write(6,*)' in read_wrf_mass_guess, min,max(soil_type)=', &
          minval(soil_type),maxval(soil_type)
     if(mype==10) write(6,*)' in read_wrf_mass_guess, min,max(isli)=', &
          minval(isli),maxval(isli)
     
     deallocate(all_loc,jsig_skip,igtype,identity)
     deallocate(temp1,itemp1,temp1u,temp1v)


     return
end subroutine read_wrf_mass_netcdf_guess

subroutine generic_grid2sub(tempa,all_loc,kbegin_loc,kend_loc,kbegin,kend,mype,num_fields)
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

  call reorder2_s(tempa,kend_loc-kbegin_loc+1)

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
#else /* Start no WRF-library block */
subroutine read_wrf_mass_binary_guess(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_wrf_mass_binary_guess
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-12-07  lueken - added subprogram doc block and implicit none
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use kinds,only: i_kind
  implicit none
  integer(i_kind),intent(in)::mype
  write(6,*)'READ_WRF_MASS_BINARY_GUESS:  dummy routine, does nothing!'
end subroutine read_wrf_mass_binary_guess
subroutine read_wrf_mass_netcdf_guess(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_wrf_mass_netcdf_guess
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-12-07  lueken - added subprogram doc block and implicit none
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use kinds,only: i_kind
  implicit none
  integer(i_kind),intent(in)::mype
  write(6,*)'READ_WRF_MASS_NETCDF_GUESS:  dummy routine, does nothing!'
end subroutine read_wrf_mass_netcdf_guess
#endif /* End no WRF-library block */

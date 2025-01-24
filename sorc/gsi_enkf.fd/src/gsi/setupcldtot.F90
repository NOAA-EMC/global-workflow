module cldtot_setup
  implicit none
  private
  public:: setup
        interface setup; module procedure setupcldtot; end interface

  character(len=*),parameter:: myname="cldtot_setup"
contains

subroutine setupcldtot(obsLL,odiagLL,lunin,mype,bwork,awork,nele,nobs,is,conv_diagsave)
!$$$  subprogram documentation block
!!                .      .    .                                       .
! subprogram:    setupcldtot      compute rhs of oi for pseudo moisture observations from
!                                 METAR and Satellite cloud observations
!   prgmmr: Ladwig          org: GSD                date: 2019-06-01
!
! abstract:  For moisture observations, this routine
!              a) reads obs assigned to given mpi task (geographic region),
!              b) simulates obs from guess,
!              c) apply some quality control to obs,
!              d) load weight and innovation arrays used in minimization
!              e) collects statistics for runtime diagnostic output
!              f) writes additional diagnostic information to output file
!
! program history log:
!   2016-04-06  Ladwig new setup routine for METAR ceilometer obs
!
!   input argument list:
!     lunin    - unit from which to read observations
!     mype     - mpi task id
!     nele     - number of data elements per observation
!     nobs     - number of observations
!
!   output argument list:
!     bwork    - array containing information about obs-ges statistics
!     awork    - array containing information for data counts and gross checks
!
! attributes:
!   language: f90
!   machine:  
!
!
!
!$$$
  use mpeu_util, only: die,perr
  use kinds, only: r_kind,r_single,r_double,i_kind

  use constants, only: zero,one,r1000,r10,r100
  use constants, only: huge_single,wgtlim,three
  use constants, only: tiny_r_kind,five,half,two,r0_01
  use constants, only: zero,one, h1000

  use obsmod, only: rmiss_single,time_offset
  use obsmod, only: netcdf_diag, binary_diag, dirname, ianldate
  use obsmod, only: luse_obsdiag
  use m_obsLList, only: obsLList
  use m_obsdiagNode, only: obs_diags
  use m_obsNode, only: obsNode
  use m_qNode, only: qNode
  use m_qNode, only: qNode_appendto
  use m_dtime, only: dtime_setup, dtime_check, dtime_show
  use gsi_4dvar, only: nobs_bins,hr_obsbin

  use nc_diag_write_mod,only: nc_diag_init, nc_diag_header,nc_diag_metadata, &
                              nc_diag_write, nc_diag_data2d
  use nc_diag_read_mod, only: nc_diag_read_init,nc_diag_read_get_dim, &
                              nc_diag_read_close
  use state_vectors, only: nsdim

  use guess_grids, only: geop_hgtl,hrdifsig,nfldsig,ges_tsen,ges_prsl
  use gridmod, only: nsig,get_ijk
  use qcmod, only: npres_print
  use jfunc, only: jiter
  use convinfo, only: nconvtype
  use convinfo, only: icsubtype
  use rapidrefresh_cldsurf_mod, only: i_cloud_q_innovation, &
                                      cld_bld_hgt,i_ens_mean
  use gsi_bundlemod, only : gsi_bundlegetpointer
  use gsi_metguess_mod, only : gsi_metguess_get,gsi_metguess_bundle

  use mpimod, only: mpi_comm_world
  use gsdcloudlib_pseudoq_mod, only: cloudLWC_pseudo,cloudCover_Surface_col


  implicit none

! Declare passed variables
  type(obsLList ),target,dimension(:),intent(in):: obsLL
  type(obs_diags),target,dimension(:),intent(in):: odiagLL

  integer(i_kind)                                  ,intent(in   ) :: lunin,mype,nele,nobs
  real(r_kind),dimension(100+7*nsig)               ,intent(inout) :: awork
  real(r_kind),dimension(npres_print,nconvtype,5,3),intent(inout) :: bwork
  integer(i_kind)                                  ,intent(in   ) :: is     ! ndat index
  logical                                          ,intent(in   ) :: conv_diagsave

#ifdef RR_CLOUDANALYSIS
! Declare local parameters
  real(r_single) :: cloudqvis

  real(r_kind),parameter:: small1=0.0001_r_kind
  real(r_kind),parameter:: small2=0.0002_r_kind
  real(r_kind),parameter:: r0_7=0.7_r_kind
  real(r_kind),parameter:: r8=8.0_r_kind
  real(r_kind),parameter:: r0_001 = 0.001_r_kind
  real(r_kind),parameter:: r1e16=1.e16_r_kind
  character(len=*),parameter:: myname='setupcldtot'

! Declare external calls for code analysis
  external:: tintrp2a1,tintrp2a11
  external:: tintrp31,tintrp3
  external:: grdcrd1
  external:: stop2

! Declare local variables  
  
  real(r_double) rstation_id
  real(r_kind) qob,qges,qv_ob
  real(r_kind) ratio_errors,dlat,dlon,dtime,dpres,error
  real(r_kind) ddiff
  real(r_kind) scale
  real(r_kind) val,rwgt
  real(r_kind) errinv_input,errinv_adjst,errinv_final
  real(r_kind) err_input,err_adjst,err_final
  real(r_kind),dimension(nele,nobs):: data
  real(r_single),allocatable,dimension(:,:)::rdiagbuf
  real(r_single),allocatable,dimension(:,:)::rdiagbufp

  integer(i_kind) i,j,nchar,nreal,nrealcld,ii,iip,mm1
  integer(i_kind) itype,k,ibin,ioff0
  integer(i_kind) ikx
  integer(i_kind) ilate,ilone,iobshgt
  integer(i_kind) id,ilon,ilat,istnelv,ivis,icldhgt,icldamt,iwthr,itime,iuse,iddp
  integer(i_kind) :: startwx, endwx


  character(8) station_id
  character(8),allocatable,dimension(:):: cdiagbuf
  character(8),allocatable,dimension(:):: cdiagbufp
  character(8),allocatable,dimension(:):: stationbuf

  logical proceed
  logical,dimension(nobs):: luse,muse

  logical:: in_curbin, in_anybin
  type(qNode),pointer:: my_headq

  equivalence(rstation_id,station_id)

  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_ql
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_qi
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_ps
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_z
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_tv
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_q

  real(r_kind),allocatable:: dpres1d(:)
  real(r_kind),allocatable:: t_bk(:)
  real(r_kind),allocatable:: h_bk(:)
  real(r_kind),allocatable:: p_bk(:)
  real(r_kind),allocatable:: ql_bk(:)
  real(r_kind),allocatable:: qi_bk(:)
  real(r_kind) z_bk
  real(r_kind),allocatable:: q_bk(:)

  !surface obs
  integer(i_kind),allocatable  :: ocld(:)
  character*10    :: owx
  real(r_single)  :: oelvtn
  real(r_single)  :: ovis
  character*3     :: mwx
  integer(i_kind) :: nvarcld_p
  parameter (nvarcld_p=13)
  real(r_kind)    ::  cldamt,awx,cldhgt

  integer(i_kind),allocatable :: pcp_type_obs(:)   ! precipitation type
  integer(i_kind) :: wthr_type
  !integer(i_kind),allocatable :: cloudlayers_i(:) ! 5 different layers 
                                                      ! 1= the number of layers
                                                      ! 2,4,... bottom
                                                      ! 3,5,... top
  real(r_single) :: vis2qc           ! fog
  real(r_single),external :: ruc_saturation     ! an external function accesseed through
                                                ! an implicit interface from GSD

  real(r_single), allocatable :: cld_cover_obs(:)  ! cloud cover obs

  real(r_single),allocatable :: cldwater_obs(:)    ! cloud water
  real(r_single),allocatable :: cldice_obs(:)      ! cloud ice

  real(r_single),allocatable :: all_qv_obs(:,:)      ! to save obs from mean

  integer(i_kind) :: miss_obs_int
  real(r_kind)    :: miss_obs_real
  real(r_single)    :: miss_obs_single
  real(r_single)    :: pressure
  parameter ( miss_obs_int = 99999999  )
  parameter ( miss_obs_real = 99999999.0_r_kind )
  parameter ( miss_obs_single = -9999.0_r_single )
  real(r_kind)    ::     spval_p
  parameter (spval_p = 99999999._r_kind)
  integer(i_kind) :: obzero,obcount,dontobcount
  integer(i_kind) :: q_obcount,q_clear_count,q_build_count
  integer(i_kind) :: q_clear0_count,q_build0_count
  real(r_kind) :: zlev_clr
  real(r_kind) :: qobmax,qobmin
  integer(i_kind) :: firstob
  real(r_kind) :: var_jb
  real(r_kind) :: rh_clear_p
  parameter (rh_clear_p = 0.8_r_kind)
  character(len=14) :: myfile
  logical:: lhere
  integer(i_kind):: istat1,istat2,istat3

  type(obsLList),pointer,dimension(:):: qhead

  if(luse_obsdiag) call die(myname,'not implemented for luse_obsdiag =',luse_obsdiag)

  qhead => obsLL(:)
!
  awork=0.0_r_kind
  bwork=0.0_r_kind
!
  obcount=0
  q_obcount=0
  q_clear_count=0
  q_clear0_count=0
  q_build_count=0
  q_build0_count=0
  dontobcount=0
  qobmax = 0.0_r_kind
  qobmin = 100.0_r_kind
  firstob=0
  var_jb=zero
  qv_ob=-7777.7_r_kind
  cloudqvis=0._r_single

! Check to see if cloud ob DA should be done
  if (i_cloud_q_innovation == 0) return

! Check to see if required guess fields are available
  call check_vars_(proceed)
  if(.not.proceed) return  ! not all vars available, simply return

! If require guess vars available, extract from bundle ...
  call init_vars_

  allocate(h_bk(nsig))
  allocate(t_bk(nsig))
  allocate(q_bk(nsig))
  allocate(p_bk(nsig))
  allocate(ql_bk(nsig))
  allocate(qi_bk(nsig))
  allocate(dpres1d(nsig))
  p_bk=miss_obs_real
  q_bk=miss_obs_real
  t_bk=miss_obs_real
  qi_bk=miss_obs_real
  ql_bk=miss_obs_real
  h_bk=miss_obs_real
  z_bk=miss_obs_real

  do k=1,nsig
     dpres1d(k)=k
  enddo

  ! If requested, save select data for output to diagnostic file
  if(conv_diagsave)then
     nchar=1
     nreal=23
     if (i_cloud_q_innovation == 1 .or. i_cloud_q_innovation == 3) then
         ii=0
         allocate(cdiagbuf(nobs*nsig),rdiagbuf(nreal,nobs*nsig))
         rdiagbuf=zero
     endif
     if (i_cloud_q_innovation >= 20 .or. i_cloud_q_innovation == 3) then
         iip=0
         allocate(cdiagbufp(nobs*nsig),rdiagbufp(nreal,nobs*nsig))
         cdiagbufp="EMPTY"
         rdiagbufp=zero
         if (i_ens_mean == 1) then
            nrealcld=nreal+10
            allocate(all_qv_obs(nrealcld,nobs*nsig))
            all_qv_obs=miss_obs_real
         endif
         if (netcdf_diag) call init_netcdf_diag_
     endif
  endif

  if (i_ens_mean == 2) then
!    will read the observations from saved file for diag file of each ensemble member.
!    so ship the moisture observation generation.
  else 
!*******************************************************************************
! Read and reformat observations in work arrays.
     read(lunin)data,luse
   
     id=1        ! index of station id
     ilon=2      ! index of grid relative obs location (x)
     ilat=3      ! index of grid relative obs location (y)
     istnelv=4   ! index of station elevation (m)
     ivis=5      ! index of visibility observation
     icldamt=6   ! index of cloud amount from 6-11 
     icldhgt=12  ! index of cloud base height from 12-17
     iwthr=18    ! index of weather 18-20
     itime=21    ! index of observation time in data array
     iuse=22     ! index of use parameter
     iddp=24     ! index of dewpoint depression from surface obs
     itype=25    ! index of ob type
     ilone=26    ! index of longitude (degrees)
     ilate=27    ! index of latitude (degrees)

     allocate(ocld(nvarcld_p))
     allocate(cld_cover_obs(nsig))
     allocate(pcp_type_obs(nsig))
     zlev_clr = 3650._r_kind
     allocate(cldwater_obs(nsig))
     allocate(cldice_obs(nsig))
   
     scale=one
   
! Prepare data
     call dtime_setup()
     do i=1,nobs
         dtime=data(itime,i)
         call dtime_check(dtime, in_curbin, in_anybin)
         if(.not.in_anybin)then
            write(*,*) "NOT_in_anybin"
            cycle
         endif
   
         oelvtn  = data(istnelv,i)
         dlat=data(ilat,i)
         dlon=data(ilon,i)
         
         ikx=nint(data(itype,i))
   
!    Link observation to appropriate observation bin
         if (nobs_bins>1) then
            ibin = NINT( dtime/hr_obsbin ) + 1
         else
            ibin = 1
         endif
         IF (ibin<1.OR.ibin>nobs_bins) write(6,*)mype,'Error nobs_bins,ibin=',nobs_bins,ibin
   
! Check Haze and Dust station
         data(iuse,i)=0 
         
         if(data(iuse,i) > 50 ) cycle   ! do not use this data
         ovis   = data(ivis,i)
   
         ocld=miss_obs_int
         do j=1,3
             cldamt =  data(icldamt+j-1,i)         ! cloud amount
             cldhgt =  int(data(icldhgt+j-1,i))   ! cloud bottom height
             if(cldamt < spval_p .and. cldhgt < spval_p) then
               if(abs(cldamt-0._r_kind) < 0.0001_r_kind) then
                 ocld(j)=0                 !msky='CLR'
                 cldhgt=spval_p
               elseif(abs(cldamt-13._r_kind) < 0.0001_r_kind) then
                 ocld(j)=1                 !msky='FEW'
               elseif(abs(cldamt-11._r_kind) < 0.0001_r_kind) then
                 ocld(j)=2                 !msky='SCT'
               elseif(abs(cldamt-12._r_kind) < 0.0001_r_kind) then
                 ocld(j)=3                 !msky='BKN'
               elseif((abs(cldamt-8._r_kind) < 0.0001_r_kind) .or. &
                      (abs(cldamt-9._r_kind) < 0.0001_r_kind)) then
                 ocld(j)=4                 !   msky='OVC'   msky='VV '
               elseif(abs(cldamt-1._r_kind) < 0.0001_r_kind) then
                 ocld(j)=1
               elseif(abs(cldamt-2._r_kind) < 0.0001_r_kind .or.   &
                      abs(cldamt-3._r_kind) < 0.0001_r_kind  ) then
                 ocld(j)=2
               elseif(cldamt > 3.5_r_kind .and. cldamt < 6.5_r_kind  ) then
                 ocld(j)=3
               elseif(abs(cldamt-7._r_kind) < 0.0001_r_kind ) then
                 ocld(j)=4
               else
                 ocld(j) = miss_obs_int          ! wrong cloud observation type
                 cldhgt = spval_p
               endif
               if(cldhgt > 0.0_r_kind ) then
                 ocld(6+j) = cldhgt
               else
                 ocld(j) =  miss_obs_int
                 ocld(6+j) = miss_obs_int 
               endif
             endif
         enddo   ! j
   
         owx=''
         do j=1,3
             awx    =  data(iwthr+j-1,i)        ! weather
             mwx='   '
             if(awx>=10._r_kind .and.awx<=12._r_kind ) mwx='BR '
             if(awx>=110._r_kind.and.awx<=112._r_kind) mwx='BR '
             if(awx==5._r_kind  .or. awx==105._r_kind) mwx='HZ '
             if(awx>=40._r_kind .and.awx<=49._r_kind ) mwx='FG '
             if(awx>=130._r_kind.and.awx<=135._r_kind) mwx='FG '
             if(awx>=50._r_kind .and.awx<=59._r_kind ) mwx='DZ '
             if(awx>=150._r_kind.and.awx<=159._r_kind) mwx='DZ '
             if(awx>=60._r_kind .and.awx<=69._r_kind ) mwx='RA '
             if(awx>=160._r_kind.and.awx<=169._r_kind) mwx='RA '
             if(awx>=70._r_kind .and.awx<=78._r_kind ) mwx='SN '
             if(awx>=170._r_kind.and.awx<=178._r_kind) mwx='SN '
             if(awx==79._r_kind .or. awx==179._r_kind) mwx='PE '
   
             if(awx>=80._r_kind .and.awx<=90._r_kind ) mwx='SH '
             if(awx>=180._r_kind.and.awx<=187._r_kind) mwx='SH '
             if(awx>=91._r_kind .and.awx<=99._r_kind ) mwx='TH '
             if(awx>=190._r_kind.and.awx<=196._r_kind) mwx='TH '
   
             if (j==1) startwx=1
             if (j==2) startwx=4
             if (j==3) startwx=7
             endwx=startwx+2
             owx(startwx:endwx)=mwx
         enddo
   
         wthr_type=miss_obs_int
         if ( owx=='SH'  ) wthr_type=16
         if ( owx=='TH'  ) wthr_type=1
         if ( owx=='RA'  ) wthr_type=11
         if ( owx=='SN'  ) wthr_type=12
         if ( owx=='PL'  ) wthr_type=13
         if ( owx=='DZ'  ) wthr_type=14
         if ( owx=='UP'  ) wthr_type=15
         if ( owx=='BR'  ) wthr_type=21
         if ( owx=='FG'  ) wthr_type=22
   
         if(data(ivis,i) >= spval_P) then
             ocld(13)=miss_obs_int
         else
             if(data(ivis,i) > 100.0_r_kind ) then
                 ocld(13)=int(data(ivis,i))
             elseif(data(ivis,i) <=100.0_r_kind .and. data(ivis,i) > 0.0_r_kind ) then
                 ocld(13)=100
                 write(6,*) 'setupcldtot, Warning: change visibility to 100 m !!!'
             endif
         endif
   
         ! background profiles in observation location and time
         call tintrp3(ges_prsl,p_bk,dlat,dlon,dpres1d,dtime, &
            hrdifsig,nsig,mype,nfldsig)
         call tintrp3(ges_ql,ql_bk,dlat,dlon,dpres1d,dtime, &
            hrdifsig,nsig,mype,nfldsig)
         call tintrp3(ges_qi,qi_bk,dlat,dlon,dpres1d,dtime, &
            hrdifsig,nsig,mype,nfldsig)
         call tintrp3(ges_tsen,t_bk,dlat,dlon,dpres1d,dtime, &
            hrdifsig,nsig,mype,nfldsig)
         call tintrp3(ges_q,q_bk,dlat,dlon,dpres1d,dtime, &
            hrdifsig,nsig,mype,nfldsig)
         call tintrp2a11(ges_z,z_bk,dlat,dlon,dtime, &
            hrdifsig,mype,nfldsig)
         call tintrp2a1(geop_hgtl,h_bk,dlat,dlon,dtime, &
            hrdifsig,nsig,mype,nfldsig)
   
         cld_cover_obs=miss_obs_single
         pcp_type_obs=miss_obs_int
         if (ocld(1) > 999) then
             cycle
         endif
   
         call cloudCover_surface_col(mype,nsig,i_cloud_q_innovation,cld_bld_hgt,h_bk,z_bk, &
                 nvarcld_p,ocld,oelvtn,wthr_type,pcp_type_obs,vis2qc,cld_cover_obs)
   
   
         cldwater_obs=miss_obs_single
         cldice_obs=miss_obs_single
   
          
         call cloudLWC_pseudo(nsig,q_bk,t_bk,p_bk,      &
                  cld_cover_obs,cldwater_obs,cldice_obs)
   
         obzero =0
         do k=1,nsig
            qob=miss_obs_real
            if (cldwater_obs(k) > -0.000001_r_single) then
                if (cldice_obs(k) > -0.000001_r_single) then
                   qob=cldwater_obs(k)+cldice_obs(k)
                else
                   qob=cldwater_obs(k)
                endif
            else
                if (cldice_obs(k) > -0.000001_r_single) then
                   qob=cldice_obs(k)
                endif
            endif
   
            ! make sure very small background values are set to 0
            if (ql_bk(k) < 0.000001_r_single) ql_bk(k)=0.0_r_single
            if (qi_bk(k) < 0.000001_r_single) qi_bk(k)=0.0_r_single
   
            if (qob < 99._r_single) then
   
                qges=(ql_bk(k)+qi_bk(k))*1000._r_single
   
                if (qob > 0.0_r_single .and. qges > 0.0_r_single) then
                    if (qob < qges) then
                        dontobcount=dontobcount+1
                        qob = qges
                    endif
                endif
                ! these are just for error checking
                obcount=obcount+1
                if (qob < qobmin) qobmin=qob
                if (qob > qobmax) qobmax=qob
   
   
                ! Compute innovations
                ddiff=(qob-qges)
                !write(*,'(3I,5f15.4)') mype,i,k,cld_cover_obs(k),cldwater_obs(k),cldice_obs(k),qob,ddiff
   
   
                luse(i)=.true.
                muse(i)=.true.
   
           !*******************************************************************************
               if (i_cloud_q_innovation < 20 .or. i_cloud_q_innovation > 22 ) then
                   write(*,*) "Warning - setupcldtot: this code version is only designed for i_cloud_q_innovation == 20,21,22"
                   return
               else
   
!!!!!Warning you hard coded q values here
               warning_your_hard_coded_values_here: associate(is=>4,ibin=>1)
                     !ibin = 1 ! q ob bin
                     !is = 4   ! q ob type number, these come from list in gsiparm

                   ! Within an association construct, "is" and "ibin" as
                   ! associats, would be purely local, i.e. significant
                   ! only in this construct.
                   !
                   ! On the other hand, %(idv,iob,ich) are sequential indices
                   ! referencing to the input observation stream.  While in
                   ! principle, one can hard-wire these values as long as
                   ! values are unique, variable "is" itself is a higher level
                   ! looping index with an intent(in) attribute, thus should
                   ! not be modified within this routine.
       
                   allocate(my_headq)
                   call qNode_appendto(my_headq,qhead(ibin))
       
                   my_headq%idv = is
                   my_headq%iob = i
                   my_headq%ich0= 0
                   my_headq%elat= data(ilat,i)
                   my_headq%elon= data(ilon,i)
                 end associate warning_your_hard_coded_values_here
       
                   ! Set (i,j,k) indices of guess gridpoint that bound obs location
                   mm1=mype+1
                   dpres=k
                   call get_ijk(mm1,dlat,dlon,dpres,my_headq%ij,my_headq%wij)
       
                   pressure=p_bk(k)*10.0_r_kind
                   cloudqvis= ruc_saturation(t_bk(k),pressure)
       
                   if (qob > 0._r_single) then
                    
                       if (q_bk(k) < cloudqvis) then
                           qv_ob=cloudqvis
                           ddiff=qv_ob-q_bk(k)
                           q_build_count=q_build_count+1
                       else
                           qv_ob=q_bk(k)
                           ddiff=qv_ob-q_bk(k)
                           q_build0_count=q_build0_count+1
                       endif
                       ! build error = 80%
                       error=one/(cloudqvis*8.E-01_r_kind)
       
                   elseif (qob > -0.000001_r_single) then
                       
                       if( q_bk(k) > cloudqvis*rh_clear_p) then
                           qv_ob=cloudqvis*rh_clear_p
                           ddiff=qv_ob-q_bk(k)
                           q_clear_count=q_clear_count+1
                       else
                           qv_ob=q_bk(k)
                           ddiff=qv_ob-q_bk(k) 
                           q_clear0_count=q_clear0_count+1
                       endif
                       ! clear error = 30%
                       error=one/(cloudqvis*3.E-01_r_kind)
                   else
                       cycle
                   endif
       
                   q_obcount=q_obcount+1
       
                   ! all obs errors = 30%
                   ratio_errors=1.0_r_kind
                   val = error*ddiff
       
                   my_headq%res    = ddiff
                   my_headq%err2   = error**2
                   my_headq%raterr2= ratio_errors**2
                   my_headq%time   = dtime
                   my_headq%b      = 10.0_r_single !cvar_b(ikx) 
                   my_headq%pg     = 0.0_r_single  !cvar_pg(ikx)
                   my_headq%jb     = var_jb 
                   my_headq%luse   = luse(i)
       
       
                   ! Save select output for diagnostic file
                   if(conv_diagsave .and. luse(i))then
                      iip=iip+1

                      rstation_id     = data(id,i)

                      err_input = error   ! data(ier2,i)
                      err_adjst = error   ! data(ier,i)
       
                      if (ratio_errors*error>tiny_r_kind) then
                         err_final = one/(ratio_errors*error)
                      else
                         err_final = huge_single
                      endif
       
                      errinv_input = huge_single
                      errinv_adjst = huge_single
                      errinv_final = huge_single
                      if (err_input>tiny_r_kind) errinv_input = one/err_input
                      if (err_adjst>tiny_r_kind) errinv_adjst = one/err_adjst
                      if (err_final>tiny_r_kind) errinv_final = one/err_final

                      if (binary_diag) call contents_binary_diag_
                      if (netcdf_diag) call contents_netcdf_diag_

                      if (i_ens_mean == 1) then
       
                          all_qv_obs(1:20,iip)=rdiagbufp(1:20,iip)
                          all_qv_obs(24,iip)=dlat
                          all_qv_obs(25,iip)=dlon
                          all_qv_obs(26,iip)=k
                          all_qv_obs(27,iip)=dtime
                          all_qv_obs(28,iip)=rstation_id
                          all_qv_obs(29,iip)=cloudqvis
       
                      endif
                   endif    !conv_diagsave .and. luse(i))
       
               endif !i_cloud_q_innovation
   
           endif !end valid ob
         enddo !end k loop
     enddo ! end loop over obs
  
     write(*,'(A,7I12)') 'qobcount',mype,q_obcount,obcount,q_build_count, &
                                  q_build0_count,q_clear_count,q_clear0_count

     deallocate(cld_cover_obs,pcp_type_obs)
     deallocate(ocld)
     deallocate(cldwater_obs,cldice_obs)
  endif !i_ens_mem .ne. 2

  write(myfile, "(A11,I3.3)") myname,mype

  if (i_ens_mean == 1) then

     open(33,file=myfile,form='UNFORMATTED')
     write(33) q_obcount,iip,nrealcld
     write(33) all_qv_obs(:,1:iip) 
     write(33) cdiagbufp(1:iip) 
     close(33)

     deallocate(all_qv_obs)

  elseif (i_ens_mean == 2) then
     inquire(file=myfile,exist=lhere)
     if (.not.lhere) then
        write(*,*)'SETUPCLDTOT:  **Warning** file ',&
             trim(myfile),' does NOT exist.'
        return
     endif

     open(33,file=myfile,form='unformatted')
     read(33,iostat=istat1) q_obcount,iip,nrealcld
     allocate(all_qv_obs(nrealcld,q_obcount))
     allocate(stationbuf(q_obcount))
     read(33,iostat=istat2) all_qv_obs
     read(33,iostat=istat3) stationbuf
     if (istat1/=0 .or. istat2/=0 .or. istat3/=0) then
        write(*,*)'SETUPCLDTOT:  ***ERROR*** reading file ',&
             trim(myfile),' istat1,istat2,istat3=',istat1,istat2,istat3,'  Terminate execution'
        call stop2(329)
     endif
     close(33)

     do i=1,q_obcount

        qv_ob=all_qv_obs(17,i)
        dlat=all_qv_obs(24,i)
        dlon=all_qv_obs(25,i)
        k=all_qv_obs(26,i)
        dtime=all_qv_obs(27,i)
        rstation_id=all_qv_obs(28,i)
        station_id=stationbuf(i)

        ! background profiles in observation location and time
        call tintrp3(ges_q,q_bk,dlat,dlon,dpres1d,dtime, &
          hrdifsig,nsig,mype,nfldsig)

        ddiff=qv_ob-q_bk(k)

        if(conv_diagsave)then
           if (binary_diag) call contents_binary_diag_mem_
           if (netcdf_diag) call contents_netcdf_diag_mem_
        endif
     enddo

     deallocate(all_qv_obs)
     deallocate(stationbuf)
     
  endif !i_ens_mem 

  !! Write information to diagnostic file
  if(conv_diagsave)then
     if (i_cloud_q_innovation >= 20 .and. iip>0) then
        if(netcdf_diag) call nc_diag_write
        if(binary_diag)then
           write(7)'  q',nchar,nreal,iip,mype,ioff0
           write(7)cdiagbufp(1:iip),rdiagbufp(:,1:iip)
        endif
        deallocate(cdiagbufp,rdiagbufp)
     elseif (i_cloud_q_innovation == 1 .and. ii>0) then
         deallocate(cdiagbuf,rdiagbuf)
         write(*,*) "Setupcldtot: DIAG not setup for i_cloud_q_innovation == 1!!!"
     elseif (i_cloud_q_innovation == 3) then   
         deallocate(cdiagbuf,rdiagbuf)
         deallocate(cdiagbufp,rdiagbufp)
         write(*,*) "Setupcldtot: DIAG not setup for i_cloud_q_innovation == 3!!!"
     endif
  endif

  ! Release memory of local guess arrays
  call final_vars_

  deallocate(h_bk,t_bk,q_bk)
  deallocate(p_bk,ql_bk,qi_bk,dpres1d)


!*******************************************************************************
! End of routine
  return
  contains

  subroutine check_vars_ (proceed)
  logical,intent(inout) :: proceed
  integer(i_kind) ivar, istatus
! Check to see if required guess fields are available
  call gsi_metguess_get ('var::ps', ivar, istatus )
  proceed=ivar>0
  call gsi_metguess_get ('var::ql', ivar, istatus )
  proceed=proceed.and.ivar>0
  call gsi_metguess_get ('var::qi', ivar, istatus )
  proceed=proceed.and.ivar>0
  call gsi_metguess_get ('var::z', ivar, istatus )
  proceed=proceed.and.ivar>0
  call gsi_metguess_get ('var::tv', ivar, istatus )
  proceed=proceed.and.ivar>0
  call gsi_metguess_get ('var::q', ivar, istatus )
  proceed=proceed.and.ivar>0
  end subroutine check_vars_ 


  subroutine init_vars_

  real(r_kind),dimension(:,:  ),pointer:: rank2=>NULL()
  real(r_kind),dimension(:,:,:),pointer:: rank3=>NULL()
  character(len=5) :: varname
  integer(i_kind) ifld, istatus

! If require guess vars available, extract from bundle ...
  if(size(gsi_metguess_bundle)==nfldsig) then
!    get ps ...
     varname='ps'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank2,istatus)
     if (istatus==0) then
         if(allocated(ges_ps))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_ps(size(rank2,1),size(rank2,2),nfldsig))
         ges_ps(:,:,1)=rank2
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank2,istatus)
            ges_ps(:,:,ifld)=rank2
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
!    get ql ...
     varname='ql'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(ges_ql))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_ql(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
         ges_ql(:,:,:,1)=rank3
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            ges_ql(:,:,:,ifld)=rank3
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
!    get qi ...
     varname='qi'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(ges_qi))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_qi(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
         ges_qi(:,:,:,1)=rank3
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            ges_qi(:,:,:,ifld)=rank3
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
!    get z ...
     varname='z'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank2,istatus)
     if (istatus==0) then
         if(allocated(ges_z))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_z(size(rank2,1),size(rank2,2),nfldsig))
         ges_z(:,:,1)=rank2
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank2,istatus)
            ges_z(:,:,ifld)=rank2
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
!    get tv ...
     varname='tv'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(ges_tv))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_tv(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
         ges_tv(:,:,:,1)=rank3
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            ges_tv(:,:,:,ifld)=rank3
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
!    get q ...
     varname='q'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(ges_q))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_q(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
         ges_q(:,:,:,1)=rank3
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            ges_q(:,:,:,ifld)=rank3
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
  else
     write(6,*) trim(myname), ': inconsistent vector sizes (nfldsig,size(metguess_bundle) ',&
                 nfldsig,size(gsi_metguess_bundle)
     call stop2(999)
  endif

  end subroutine init_vars_

  subroutine final_vars_
    if(allocated(ges_ps)) deallocate(ges_ps)
    if(allocated(ges_ql )) deallocate(ges_ql )
    if(allocated(ges_qi )) deallocate(ges_qi )
    if(allocated(ges_z)) deallocate(ges_z)
    if(allocated(ges_tv)) deallocate(ges_tv)
    if(allocated(ges_q)) deallocate(ges_q)
  end subroutine final_vars_

  subroutine init_netcdf_diag_
  
  character(len=80) string
  character(len=128) diag_conv_file
  integer(i_kind) ncd_fileid,ncd_nobs
  logical append_diag
  logical,parameter::verbose=.false.

! open netcdf diag file
     write(string,900) jiter
900  format('conv_cldtot_',i2.2,'.nc4')
     diag_conv_file=trim(dirname) // trim(string)

     inquire(file=diag_conv_file, exist=append_diag)

     if (append_diag) then
        call nc_diag_read_init(diag_conv_file,ncd_fileid)
        ncd_nobs = nc_diag_read_get_dim(ncd_fileid,'nobs')
        call nc_diag_read_close(diag_conv_file)

        if (ncd_nobs > 0) then
           if(verbose) print *,'file ' // trim(diag_conv_file) // ' exists.  Appending.  nobs,mype=',ncd_nobs,mype
        else
           if(verbose) print *,'file ' // trim(diag_conv_file) // ' exists but contains no obs.  Not appending. nobs,mype=',ncd_nobs,mype
           append_diag = .false. ! if there are no obs in existing file, then do not try to append
        endif
     end if

     call nc_diag_init(diag_conv_file, append=append_diag)

     if (.not. append_diag) then ! don't write headers on append - the module will break?
        call nc_diag_header("date_time",ianldate )
        call nc_diag_header("Number_of_state_vars", nsdim          )
     endif

  end subroutine init_netcdf_diag_

  subroutine contents_binary_diag_

      cdiagbufp(iip)  = station_id           ! station id
! force new ob type 
      rdiagbufp(1,iip)  = 199 
      rdiagbufp(2,iip)  = icsubtype(ikx)     ! observation subtype
    
      rdiagbufp(3,iip)  = data(ilate,i)      ! observation latitude (degrees)
      rdiagbufp(4,iip)  = data(ilone,i)      ! observation longitude (degrees)
      rdiagbufp(5,iip)  = data(istnelv,i)    ! station elevation (meters)
      rdiagbufp(6,iip)  = pressure           ! observation pressure
      rdiagbufp(7,iip)  = data(icldhgt,i)    ! observation height (meters)
      rdiagbufp(8,iip)  = dtime-time_offset  ! obs time (hours relative to analysis time)
      rdiagbufp(9,iip)  = 1._r_single        ! qc
      rdiagbufp(10,iip) = var_jb             ! non linear qc b parameter
      rdiagbufp(11,iip) = data(iuse,i)       ! read_prepbufr data usage flag
     
      if(muse(i)) then
         rdiagbufp(12,iip) = one             ! analysis usage flag (1=use, -1=not used)
      else
         rdiagbufp(12,iip) = -one
      endif
     
      rdiagbufp(13,iip) = rwgt               ! nonlinear qc relative weight
      rdiagbufp(14,iip) = errinv_input       ! prepbufr inverse observation error
      rdiagbufp(15,iip) = errinv_adjst       ! read_prepbufr inverse obs error
      rdiagbufp(16,iip) = errinv_final       ! final inverse observation error
     
      rdiagbufp(17,iip) = qv_ob              ! observation
      rdiagbufp(18,iip) = ddiff              ! obs-ges used in analysis
      rdiagbufp(19,iip) = ddiff              !qob-qges  !obs-ges w/o bias correction (future slot)

      rdiagbufp(20,iip) = q_bk(k) !qsges              ! guess saturation specific humidity

  end subroutine contents_binary_diag_

  subroutine contents_binary_diag_mem_

      cdiagbufp(i)    = station_id 
      rdiagbufp(1:17,i)=all_qv_obs(1:17,i)
      rdiagbufp(18,i) = ddiff  
      rdiagbufp(19,i) = ddiff     
      rdiagbufp(20,i) = q_bk(k)  
!      rdiagbufp(21:29,i)=all_qv_obs(21:29,i)
! could be a bug, 1st index max is 23 
  end subroutine contents_binary_diag_mem_

  subroutine contents_netcdf_diag_

    call nc_diag_metadata("Station_ID",              station_id             )
    call nc_diag_metadata("Observation_Type",        icsubtype(ikx)         )
    call nc_diag_metadata("Observation_Subtype",     int(icsubtype(ikx))    )
    call nc_diag_metadata("Latitude",                sngl(data(ilate,i))    )
    call nc_diag_metadata("Longitude",               sngl(data(ilone,i))    )
    call nc_diag_metadata("Station_Elevation",       sngl(data(istnelv,i))  )
    call nc_diag_metadata("Pressure",                sngl(pressure)         )
    call nc_diag_metadata("Height",                  sngl(data(iobshgt,i))  )
    call nc_diag_metadata("Time",                    sngl(dtime-time_offset))
    call nc_diag_metadata("Prep_QC_Mark",            sngl(1._r_single)      )
    call nc_diag_metadata("Setup_QC_Mark",           sngl(rmiss_single)     )
    call nc_diag_metadata("Prep_Use_Flag",           sngl(data(iuse,i))     )
    if(muse(i)) then
       call nc_diag_metadata("Analysis_Use_Flag",    sngl(one)              )
    else
       call nc_diag_metadata("Analysis_Use_Flag",    sngl(-one)             )
    endif

    call nc_diag_metadata("Nonlinear_QC_Rel_Wgt",    sngl(rwgt)             )
    call nc_diag_metadata("Errinv_Input",            sngl(errinv_input)     )
    call nc_diag_metadata("Errinv_Adjust",           sngl(errinv_adjst)     )
    call nc_diag_metadata("Errinv_Final",            sngl(errinv_final)     )
    call nc_diag_metadata("Observation",             sngl(qv_ob)            )
    call nc_diag_metadata("Obs_Minus_Forecast_adjusted",   sngl(ddiff)      )
    call nc_diag_metadata("Obs_Minus_Forecast_unadjusted", sngl(ddiff))

  end subroutine contents_netcdf_diag_

  subroutine contents_netcdf_diag_mem_

    call nc_diag_metadata("Station_ID",              station_id             )
    call nc_diag_metadata("Observation_Type",        int(all_qv_obs(1,iip))  )
    call nc_diag_metadata("Observation_Subtype",     int(all_qv_obs(2,iip))  )
    call nc_diag_metadata("Latitude",                sngl(all_qv_obs(3,iip)) )
    call nc_diag_metadata("Longitude",               sngl(all_qv_obs(4,iip)) )
    call nc_diag_metadata("Station_Elevation",       sngl(all_qv_obs(5,iip)) )
    call nc_diag_metadata("Pressure",                sngl(all_qv_obs(6,iip)) )
    call nc_diag_metadata("Height",                  sngl(all_qv_obs(7,iip)) )
    call nc_diag_metadata("Time",                    sngl(all_qv_obs(8,iip)) )
    call nc_diag_metadata("Prep_QC_Mark",            sngl(all_qv_obs(9,iip)) )
    call nc_diag_metadata("Setup_QC_Mark",           sngl(rmiss_single)     )
    call nc_diag_metadata("Prep_Use_Flag",           sngl(all_qv_obs(11,iip)))
    call nc_diag_metadata("Analysis_Use_Flag",       sngl(all_qv_obs(12,iip)))
    call nc_diag_metadata("Nonlinear_QC_Rel_Wgt",    sngl(all_qv_obs(13,iip)))
    call nc_diag_metadata("Errinv_Input",            sngl(all_qv_obs(14,iip)))
    call nc_diag_metadata("Errinv_Adjust",           sngl(all_qv_obs(15,iip)))
    call nc_diag_metadata("Errinv_Final",            sngl(all_qv_obs(16,iip)))
    call nc_diag_metadata("Observation",             sngl(all_qv_obs(17,iip)))
    call nc_diag_metadata("Obs_Minus_Forecast_adjusted",   sngl(ddiff)      )
    call nc_diag_metadata("Obs_Minus_Forecast_unadjusted", sngl(ddiff))

  end subroutine contents_netcdf_diag_mem_

#else

  character(len=*),parameter:: myname_=myname//"::setupcldtot"
  integer(i_kind):: ier

! Skip the record, and does nothing
  read(lunin,iostat=ier)
  if(ier/=0) call die(myname_,'unexpected empty block, iostat =',ier)
#endif

end subroutine setupcldtot
end module cldtot_setup

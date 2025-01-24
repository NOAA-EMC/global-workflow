module gfs_stratosphere
!$$$   module documentation block
!                .      .    .                                       .
! module:    gfs_stratosphere   routines for adding/blending high gfs levels in regional model
!   prgmmr: parrish          org: np22                date: 2012-02-07
!
! abstract: This module contains routines used to blend in and add additional levels from a
!             GFS model forecast valid at the same time as a regional guess.  The GFS and regional
!             model levels are blended together in the stratosphere continuing up to the top
!             of the GFS domain, which is usually much higher and/or has higher resolution compared
!             to typical regional model domains.  The reason for adding the extra detail from the
!             GFS model in the stratosphere is to allow direct utilization of satellite radiance
!             bias correction coefficients derived from the global data assimilation system (GDAS).
!             
! program history log:
!   2012-02-07  parrish, initial documentation
!   2013-02-08  zhu - add guess variables for hydrmeteros
!
! subroutines included:
!   sub init_gfs_stratosphere      - initialize module parameters to default values
!   sub mix_gfs_nmmb_vcoords       - create new vertical coordinate that smoothly blends nmmb with gfs.
!   sub broadcast_gfs_stratosphere_vars - broadcast new vertical vars to all processors
!   sub destroy_nmmb_vcoords       - deallocate arrays
!

! Variable Definitions:
!   def yyyy                       - description of yyyy
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ enddocumentation block

   use kinds, only: r_double,i_kind,i_long,r_single,r_kind
   use mpeu_util, only: getindex,die
   use gsi_io, only: verbose

   implicit none

   ! set default to private
   private
   ! set subroutines to public
   public :: init_gfs_stratosphere
   public :: add_gfs_stratosphere
   public :: mix_gfs_nmmb_vcoords
   public :: broadcast_gfs_stratosphere_vars
   public :: destroy_nmmb_vcoords
   public :: revert_to_nmmb
   public :: restore_nmmb_gfs
   ! set variables to public
   public :: use_gfs_stratosphere
   public :: nsig_max,nsig_save,k0m,k1m,k0r,k1g,k0rm,k1mp,k0rp
   public :: nsigg,ak5,bk5,nsigm
   public :: deta1_save,aeta1_save,eta1_save
   public :: deta2_save,aeta2_save,eta2_save
   public :: pblend0,pblend1
   public :: blend_rm,blend_gm
   public :: ges_tv_r,ges_q_r,ges_u_r,ges_v_r,ges_tsen_r,ges_oz_r
   public :: ges_cw_r,ges_ql_r,ges_qi_r,ges_qr_r,ges_qs_r,ges_qg_r,ges_qh_r
   public :: ges_tv_r_g,ges_q_r_g,ges_u_r_g,ges_v_r_g,ges_tsen_r_g,ges_oz_r_g
   public :: ges_cw_r_g,ges_ql_r_g,ges_qi_r_g,ges_qr_r_g,ges_qs_r_g,ges_qg_r_g,ges_qh_r_g
   public :: good_o3mr

   integer(i_kind) nsig_max,k0m,k1m,k0r,k1g,k0rm,k1mp,k0rp
   integer(i_kind) nsig_save,nsigg,nsigm
   real(r_kind) pblend0,pblend1
   real(r_kind),dimension(:),allocatable:: deta1_save,aeta1_save,eta1_save            
   real(r_kind),dimension(:),allocatable:: deta2_save,aeta2_save,eta2_save          
   real(r_kind),dimension(:),allocatable :: ak5,bk5
   real(r_kind),dimension(:),allocatable:: blend_rm,blend_gm
   real(r_kind),dimension(:,:,:,:),allocatable:: ges_tv_r_g,ges_u_r_g,ges_v_r_g, &
                                                 ges_tsen_r_g,ges_oz_r_g,ges_q_r_g
   real(r_kind),dimension(:,:,:,:),allocatable:: ges_cw_r_g,ges_ql_r_g,ges_qi_r_g,ges_qr_r_g,& 
                                                 ges_qs_r_g,ges_qg_r_g,ges_qh_r_g
   real(r_kind),dimension(:,:,:,:),allocatable:: ges_tv_r  ,gesq_r  ,ges_u_r  ,ges_v_r  , &
                                                 ges_tsen_r  ,ges_oz_r,ges_q_r
   real(r_kind),dimension(:,:,:,:),allocatable:: ges_cw_r,ges_ql_r,ges_qi_r,ges_qr_r,ges_qs_r,& 
                                                 ges_qg_r,ges_qh_r
   logical use_gfs_stratosphere
   logical good_o3mr
   logical zero_bkbridge

contains

subroutine init_gfs_stratosphere
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_gfs_stratosphere  initialize constants
!   prgmmr: parrish          org: np22                date: 2012-02-10
!
! abstract: initialize various constants.
!
! program history log:
!   2012-02-10  parrish, initial documentation
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

   implicit none

   use_gfs_stratosphere=.false.
   good_o3mr=.false.
   nsig_max=127
   k0m=0
   k1m=0
   k0r=0
   k1g=0
   nsig_save=0
   pblend0=152._r_kind
   pblend1=79._r_kind

   return

end subroutine init_gfs_stratosphere

subroutine mix_gfs_nmmb_vcoords(deta1 ,aeta1 ,eta1 ,deta2 ,aeta2 ,eta2 ,pdtop,pt,nsigr, &
                                deta1m,aeta1m,eta1m,deta2m,aeta2m,eta2m,nsigm_out)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    mix_gfs_nmmb_vcoords  make new vert coord from global and regional
!   prgmmr: parrish          org: np22                date: 2012-02-11
!
! abstract: Combine gfs vertical coordinate defined by ak5,bk5, nsigg with regional coordinate, defined
!            by eta1,eta2,pdtop,pt,nsigr, to create a new vertical coordinate in regional format, defined by
!            eta1m,eta2m,k0m,k1m,k0r,k1g,nsigm.  The new coordinate is designed so that
!            for pressures greater than pblend0, the coordinate is identical to the regional coordinate, and
!            for pressures less than pblend1, the coordinate is identical with the gfs vertical coordinate.
!            In the zone from pblend0 > p > pblend1, the coordinates are blended together, changing
!            smoothly from regional at bottom to global at top of blend zone.  This coordinate is created
!            so that global background fields can be combined smoothly with regional fields in the upper
!            atmosphere so that increased vertical resolution near top of global model can be made
!            available in the regional model for the purpose of allowing direct use of global
!            satellite radiance bias corrections in the regional model.
!
! program history log:
!   2012-02-11  parrish, initial documentation
!   2012-10-11  eliu -  modify to work for wrf_nmm_regional (HWRF) 
!   2013-02-15  parrish - change dimension of eta1, eta2, eta1m, eta2m to correct value.
!   2016-12-06  tong - add code to get gfs nemsio meta data, if use_gfs_nemsio=True
!   2019-09-24  martin - add code to get fv3gfs netCDF info, if use_gfs_ncio=True
!
!   input argument list:
!     deta1  - all of these are original nmmb vertical coordinate specifications.
!     aeta1  -
!     eta1   -
!     deta2  -
!     aeta2  -
!     eta2   -
!     pdtop  -
!     pt     -
!     nsigr  -
!
!   output argument list:
!     deta1m -  these are the new gfs/nmmb blended vertical coordinate specifications.
!     aeta1m -
!     eta1m  -
!     deta2m -
!     aeta2m -
!     eta2m  -
!     nsigm_out  -
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

   use sigio_module, only: sigio_intkind,sigio_head,sigio_srhead
   use constants, only: zero,one_tenth,half,one,ten,r0_01,r60,r3600
   use blendmod, only: init_blend,blend_f,blend_df
   use gridmod, only: use_gfs_nemsio,use_gfs_ncio
   use nemsio_module, only: nemsio_init,nemsio_open,nemsio_close
   use ncepnems_io, only: error_msg
   use nemsio_module, only: nemsio_gfile,nemsio_getfilehead
   use module_ncio, only: Dataset, Dimension, get_dim, read_vardata,&
                                 open_dataset, close_dataset, read_attribute,&
                                 get_idate_from_time_units

   implicit none

   ! Declare passed variables
   real(r_single),dimension(nsigr)        ,intent(in   ) :: deta1,aeta1,deta2,aeta2
   real(r_single),dimension(nsigr+1)      ,intent(in   ) :: eta1,eta2
   real(r_single)                         ,intent(in   ) :: pdtop,pt
   integer(i_kind)                        ,intent(in   ) :: nsigr
   real(r_single),dimension(nsig_max)     ,intent(  out) :: deta1m,aeta1m,deta2m,aeta2m
   real(r_single),dimension(nsig_max+1)   ,intent(  out) :: eta1m,eta2m
   integer(i_kind)                        ,intent(  out) :: nsigm_out

   ! Declare local variables
   real(r_kind) ak_r(nsigr+1),bk_r(nsigr+1),p_r(nsigr+1)
   real(r_kind),dimension(:),allocatable:: p_g,dp_g ! (nsigg+1)
   real(r_kind) psfc
   integer(i_kind) k,kr
   real(r_kind) dp_r(nsigr+1)
   real(r_kind) pref0,pref1
   real(r_kind) delpmin,delp
   real(r_kind) delp_m(nsig_max),wgt_m(nsig_max)
   integer(i_kind) nref
   real(r_kind),allocatable::pref(:),dpref_r(:),dpref_g(:),dpref_m(:)
   real(r_kind),allocatable:: p_m(:)
   integer(i_kind) iord,ierror,kk,knext,j,kkend
   real(r_kind) delta,gwgt,deltap,psum,sum,adjust,pthis,dummy
   real(r_kind),allocatable,dimension(:)::ak_m,bk_m,akm,bkm
   real(r_kind) ak_gthis,bk_gthis,ak_rthis,bk_rthis
   character(24) filename
   integer(sigio_intkind):: lunges = 11
   integer(i_kind) iret
   type(sigio_head):: sighead
   real(r_kind),parameter::  zero_001=0.001_r_kind
   real(r_kind) pdtop_ll,pt_ll

   real(r_single),allocatable:: plotp(:,:)
   real(r_kind) this_psfc
   character(len=120) :: my_name = 'MIX_GFS_NMMB_VCOORDS'   
   integer(i_kind) :: latb, lonb, levs, nframe
   integer(i_kind) :: nfhour, nfminute, nfsecondn, nfsecondd
   integer(i_kind) :: istop = 101
   integer(i_kind),dimension(7):: idate
   integer(i_kind),dimension(6):: idate6
   real(r_kind) :: fhour
   type(nemsio_gfile) :: gfile
   integer(i_kind) :: nvcoord
   real(r_single),allocatable:: nems_vcoord(:,:,:)
   real(r_single),allocatable:: vcoord(:,:)
   logical print_verbose
   type(Dataset) :: atmges
   type(Dimension) :: ncdim
   real(r_kind), allocatable, dimension(:) :: fhour2,aknc,bknc
 
   print_verbose=.false.
   if(verbose)print_verbose=.true.

   ! First, obtain gfs vertical coordinate information:
   filename='gfs_sigf03'  
   if ((.not. use_gfs_nemsio) .and. (.not. use_gfs_ncio))then
      open(lunges,file=trim(filename),form='unformatted')
      call sigio_srhead(lunges,sighead,iret)
      close(lunges)
      write(6,*) ' input filename=',filename  
      write(6,*) ' sighead%fhour,sighead%idate=',sighead%fhour,sighead%idate
      write(6,*) ' sighead%levs=',sighead%levs
      write(6,*) ' sighead%idvc,sighead%nvcoord=',sighead%idvc,sighead%nvcoord
      write(6,*) ' sighead%idsl=',sighead%idsl
      if(print_verbose)then
         do k=1,sighead%levs+1
            write(6,*)' k,vcoord=',k,sighead%vcoord(k,:)
         enddo
      end if
      if (sighead%nvcoord > 2) then
         write(6,*)' MIX_GFS_NMMB_VCOORDS: NOT READY YET FOR ak5,bk5,ck5 vert coordinate'
         call stop2(85)
      endif
   else if (use_gfs_ncio) then
      atmges = open_dataset(filename)
      ! get dimension sizes
      ncdim = get_dim(atmges, 'grid_xt'); lonb = ncdim%len
      ncdim = get_dim(atmges, 'grid_yt'); latb = ncdim%len
      ncdim = get_dim(atmges, 'pfull'); levs = ncdim%len

      ! get time information
      idate6 = get_idate_from_time_units(atmges)
      call read_vardata(atmges, 'time', fhour2) ! might need to change this to attribute later
                                               ! depends on model changes from Jeff Whitaker
      fhour = fhour2(1)
      write(6,*) ' input filename=',filename
      write(6,*) ' netcdf info: fhour,idate=',fhour,idate6
      write(6,*) ' netcdf info: levs=',levs

      nvcoord=2 ! ak and bk
      allocate(vcoord(levs+1,nvcoord))
      call read_attribute(atmges, 'ak', aknc)
      call read_attribute(atmges, 'bk', bknc)
      do k=1,levs+1
         kr = levs+2-k
         vcoord(k,1) = aknc(kr)
         vcoord(k,2) = bknc(kr)
      end do
      if(print_verbose)then
         write(6,*) ' netcdf : nvcoord=', nvcoord
         do k=1,levs+1
            write(6,*)' k,vcoord=',k,vcoord(k,:)
         enddo
      end if
      call close_dataset(atmges)

   else
      call nemsio_init(iret=iret)
      if (iret /= 0) call error_msg(trim(my_name),trim(filename),' ','init',istop,iret)

      call nemsio_open(gfile,filename,'READ',iret=iret)
      if (iret /= 0) call error_msg(trim(my_name),trim(filename),' ','open',istop,iret)

      call nemsio_getfilehead(gfile,iret=iret, nframe=nframe, &
           nfhour=nfhour, nfminute=nfminute, nfsecondn=nfsecondn, nfsecondd=nfsecondd, &
           idate=idate, dimx=lonb, dimy=latb,dimz=levs)

      
      if (nframe /= 0) call error_msg(trim(my_name),trim(filename),'nframe', &
                                         'getfilehead',istop,nframe)

      fhour = real(nfhour,r_kind) + real(nfminute,r_kind)/r60 + &
              real(nfsecondn,r_kind)/real(nfsecondd,r_kind)/r3600
      write(6,*) ' input filename=',filename
      write(6,*) ' nemsio head: fhour,idate=',fhour,idate
      write(6,*) ' nemsio head: levs=',levs

      allocate(nems_vcoord(levs+1,3,2))
      call nemsio_getfilehead(gfile,iret=iret,vcoord=nems_vcoord)
      if ( iret /= 0 ) call error_msg(trim(my_name),trim(filename),' ', &
                                      'getfilehead',istop,iret)

!     Determine the type of vertical coordinate used by model because that
!     gfshead%nvcoord is no longer part of NEMSIO header output.
      nvcoord=3
      if(maxval(nems_vcoord(:,3,1))==zero .and. &
         minval(nems_vcoord(:,3,1))==zero ) then
         nvcoord=2
         if(maxval(nems_vcoord(:,2,1))==zero .and. &
            minval(nems_vcoord(:,2,1))==zero ) then
            nvcoord=1
         end if
      end if
      if (nvcoord > 2) then
         write(6,*)' MIX_GFS_NMMB_VCOORDS: NOT READY YET FOR ak5,bk5,ck5 vert &
                     coordinate'
         call stop2(85)
      endif

      allocate(vcoord(levs+1,nvcoord))
      vcoord(:,1:nvcoord) = nems_vcoord(:,1:nvcoord,1)
      if(print_verbose)then
         write(6,*) ' nemsio : nvcoord=', nvcoord
         do k=1,levs+1
            write(6,*)' k,vcoord=',k,vcoord(k,:)
         enddo
      end if
      deallocate(nems_vcoord)

      call nemsio_close(gfile,iret=iret)
      if ( iret /= 0 ) call error_msg(trim(my_name),trim(filename),' ', &
                                      'close',istop,iret)
   end if
   if (allocated(p_m))        deallocate(p_m)
   if (allocated(p_g))        deallocate(p_g)
   if (allocated(dp_g))       deallocate(dp_g)
   if (allocated(pref))       deallocate(pref)
   if (allocated(dpref_r))    deallocate(dpref_r)
   if (allocated(dpref_g))    deallocate(dpref_g)
   if (allocated(dpref_m))    deallocate(dpref_m)
   if (allocated(ak_m))       deallocate(ak_m)
   if (allocated(bk_m))       deallocate(bk_m)
   if (allocated(aknc))       deallocate(aknc)
   if (allocated(bknc))       deallocate(bknc)
   if (allocated(akm))        deallocate(akm)
   if (allocated(bkm))        deallocate(bkm)
   if (allocated(plotp))      deallocate(plotp)
   if (allocated(blend_rm))   deallocate(blend_rm)
   if (allocated(blend_gm))   deallocate(blend_gm)
   if (allocated(deta1_save)) deallocate(deta1_save)
   if (allocated(aeta1_save)) deallocate(aeta1_save)
   if (allocated(eta1_save))  deallocate(eta1_save)
   if (allocated(deta2_save)) deallocate(deta2_save)
   if (allocated(aeta2_save)) deallocate(aeta2_save)
   if (allocated(eta2_save))  deallocate(eta2_save)
   if (allocated(ak5))        deallocate(ak5)
   if (allocated(bk5))        deallocate(bk5)

   if((.not. use_gfs_nemsio).and.(.not. use_gfs_ncio))then
       nsigg=sighead%levs
   else
       nsigg=levs
   end if
   if ( nsigg > nsig_max ) then
      write(6,*)' MIX_GFS_NMMB_VCOORDS: nsigg > nsig_max, nsigg,nsig_max=',nsigg,nsig_max
      call stop2(85)
   endif
   allocate(ak5(nsigg+1),bk5(nsigg+1))
   do k=1,nsigg+1
      ak5(k)=zero
      bk5(k)=zero
   end do
   if ((.not. use_gfs_nemsio).and.(.not. use_gfs_ncio))then
      do k = 1,nsigg+1
         ak5(k) = sighead%vcoord(k,1)*zero_001
         ! for purpose of this routine, convert to mb
         ak5(k)=ten*ak5(k)
         bk5(k) = sighead%vcoord(k,2)
      enddo
   else
      if (nvcoord == 1) then
         do k=1,nsigg+1
            bk5(k) = real(vcoord(k,1),r_kind)
         end do
      elseif (nvcoord == 2) then
         do k = 1,nsigg+1
            ak5(k) = real(vcoord(k,1),r_kind)*zero_001
            ak5(k)=ten*ak5(k)
            bk5(k) = real(vcoord(k,2),r_kind)
         end do
      end if
      deallocate(vcoord)
   end if

   !  save original deta1,aeta1, etc. as module public variables for later access.
   nsig_save=nsigr
   allocate(deta1_save(nsig_save),aeta1_save(nsig_save),eta1_save(nsig_save+1))
   allocate(deta2_save(nsig_save),aeta2_save(nsig_save),eta2_save(nsig_save+1))
   deta1_save=real(deta1,r_kind)
   aeta1_save=real(aeta1,r_kind)
   eta1_save=real(eta1,r_kind)
   deta2_save=real(deta2,r_kind)
   aeta2_save=real(aeta2,r_kind)
   eta2_save=real(eta2,r_kind)

   ! print out what I think deta1,2 and aeta1,2 might be as function of eta1, eta2
   if(print_verbose)then
      do k=1,nsigr
         write(6,'(" k,deta1,eta1(k)-eta1(k+1),diff=",i4,2f15.6,e11.3)') &
                     k,deta1(k),eta1(k)-eta1(k+1),abs(deta1(k)-eta1(k)+eta1(k+1))
      enddo
      do k=1,nsigr
         write(6,'(" k,deta2,eta2(k)-eta2(k+1),diff=",i4,2f15.6,e11.3)') &
                  k,deta2(k),eta2(k)-eta2(k+1),abs(deta2(k)-eta2(k)+eta2(k+1))
      enddo
      do k=1,nsigr
         write(6,'(" k,aeta1,half*(eta1(k)+eta1(k+1)),diff=",i4,2f15.6,e11.3)') &
                     k,aeta1(k),half*(eta1(k)+eta1(k+1)),abs(aeta1(k)-half*(eta1(k)+eta1(k+1)))
      enddo
      do k=1,nsigr
         write(6,'(" k,aeta2,half*(eta2(k)+eta2(k+1)),diff=",i4,2f15.6,e11.3)') &
                     k,aeta2(k),half*(eta2(k)+eta2(k+1)),abs(aeta2(k)-half*(eta2(k)+eta2(k+1)))
      enddo
   end if

   ! compute ak_r,bk_r from eta1,eta2,pdtop,pt for icase=1

   pdtop_ll=real(pdtop,r_kind)*r0_01
   pt_ll=real(pt,r_kind)*r0_01
   allocate(p_g(nsigg+1),dp_g(nsigg+1))
   psfc=1000._r_kind
   do k=1,nsigr+1
      ak_r(k)=real(eta1(k),r_kind)*pdtop_ll+pt_ll-real(eta2(k),r_kind)*(pdtop_ll+pt_ll)
      bk_r(k)=real(eta2(k),r_kind)
      p_r(k)=real(eta1(k),r_kind)*pdtop_ll+real(eta2(k),r_kind)*(psfc-pdtop_ll-pt_ll)+pt_ll
   enddo
   do k=1,nsigg+1
      p_g(k)=ak5(k)+bk5(k)*psfc
   enddo

   ! construct dp_r, dp_g

   do k=2,nsigr
      dp_r(k)=half*(p_r(k-1)-p_r(k+1))
   enddo
   dp_r(1)=p_r(1)-p_r(2)
   dp_r(nsigr+1)=p_r(nsigr)-p_r(nsigr+1)
   do k=2,nsigg
      dp_g(k)=half*(p_g(k-1)-p_g(k+1))
   enddo
   dp_g(1)=p_g(1)-p_g(2)
   dp_g(nsigg+1)=p_g(nsigg)-p_g(nsigg+1)

   !  construct reference pressures.
   !  first get range.

   pref0=p_r(1)
   k0r=1
   do k=nsigr,1,-1
      if (p_r(k)>=pblend0) then
         k0r=max(1,k-1)
         pref0=p_r(k0r)
         exit
      endif
   enddo

   pref1=pref0
   k1g=nsigg
   do k=1,nsigg+1
      if (p_g(k) <= pblend1) then
         k1g=min(k+1,nsigg)
         pref1=p_g(k1g)
         exit
      endif
   enddo
   write(6,*)' pref0,pref1=',pref0,pref1
   write(6,*)' p_r(k0r),p_g(k1g)=',p_r(k0r),p_g(k1g)
   write(6,*)' pblend0,pblend1=',pblend0,pblend1
   zero_bkbridge=bk_r(k0r)==zero.and.bk5(k1g)==zero
   write(6,*)' zero_bkbridge,k0r,k1g,bk_r(k0r),bk5(k1g)=',zero_bkbridge,k0r,k1g,bk_r(k0r),bk5(k1g)

   ! obtain min delp over range pref0,pref1
   delpmin=pref0-pref1
   do k=1,nsigr
      if (p_r(k) <= pref0 .and. p_r(k+1) >= pref1) delpmin=min(delpmin,p_r(k)-p_r(k+1))
   enddo
   do k=1,nsigg
      if (p_g(k) <= pref0 .and. p_g(k+1) >= pref1) delpmin=min(delpmin,p_g(k)-p_g(k+1))
   enddo
   write(6,*)' pref0-pref1,delpmin=',pref0-pref1,delpmin

   ! obtain ref pressure pref

   delp=delpmin/ten
   nref=one+(pref0-pref1)/delp
   delp=(pref0-pref1)/(nref-one)
   write(6,*)' nref,delp=',nref,delp
   allocate(pref(-10:nref+10))
   do k=-10,nref+10
      pref(k)=pref0-(k-one)*delp
   enddo
   if(print_verbose)then
      do k=-10,nref+10
         write(6,'(" k,pref(k),pref0,pref1=",i5,3f12.3)') k,pref(k),pref0,pref1
      enddo
   end if

   ! obtain dpref_g, dpref_r
   allocate(dpref_g(-10:nref+10))
   allocate(dpref_r(-10:nref+10))
   allocate(dpref_m(-10:nref+10))
   iord=4
   call init_blend(pblend0,pblend1,iord,ierror)

   do k=-10,nref+10
      if (pref(k) < p_g(nsigg+1)) then
         dpref_g(k)=dp_g(nsigg+1)
      elseif (pref(k) >= p_g(1)) then
         dpref_g(k)=dp_g(1)
      else
         do kk=1,nsigg
            if (pref(k) < p_g(kk) .and. pref(k) >= p_g(kk+1)) then
               delta=(dp_g(kk+1)-dp_g(kk))/(p_g(kk+1)-p_g(kk))
               dpref_g(k)=dp_g(kk)+delta*(pref(k)-p_g(kk))
               exit
            endif
         enddo
      endif
      if (pref(k) < p_r(nsigr+1)) then
         dpref_r(k)=dp_r(nsigr+1)
      else if (pref(k) >= p_r(1)) then
         dpref_r(k)=dp_r(1)
      else
         do kk=1,nsigr
            if (pref(k) < p_r(kk) .and. pref(k) >= p_r(kk+1)) then
               delta=(dp_r(kk+1)-dp_r(kk))/(p_r(kk+1)-p_r(kk))
               dpref_r(k)=dp_r(kk)+delta*(pref(k)-p_r(kk))
               exit
            endif
         enddo
      endif
      call blend_f(pref(k),gwgt)
      dpref_m(k)=gwgt*dpref_g(k)+(one-gwgt)*dpref_r(k)
      if(print_verbose)write(6,'(" k,pref,dpref_g,dpref_r,dpref_m=",i5,4f12.3)') &
                              k,pref(k),dpref_g(k),dpref_r(k),dpref_m(k)
   enddo

   ! do initial integration from p_r(k0r) to p_g(k1g) (pref0 to pref1) to get coordinate bridge from
   ! regional below to global above.
   allocate(p_m(nref))
   p_m=-999._r_kind
   p_m(1)=pref0
   knext=-10
   do kk=1,nref+9
      if ( p_m(kk) <= pref1 .or. knext >= nref+9 ) exit
      k=knext
      do j=k,nref+9
         if ( pref(j) >= p_m(kk) .and. pref(j+1) <= p_m(kk) ) then
            delta=(dpref_m(j+1)-dpref_m(j))/(pref(j+1)-pref(j))
            deltap=dpref_m(j)+delta*(p_m(kk)-pref(j))
            pthis=p_m(kk)-deltap
            p_m(kk+1)=pthis
            kkend=kk+1
            !write(6,'(" j,kkend,pref0,p_m(kkend-1:kkend),pref1=",2i4,4f9.4)') &
            !            j,kkend,pref0,p_m(kkend-1),p_m(kkend),pref1
            knext=j
            exit
         endif
      enddo
   enddo

   ! compute nsigm
   nsigm=k0r+kkend+nsigg-k1g-1
   k0m=k0r
   write(6,'(" nsigg,nsigr,nsigm=",3i4)')nsigg,nsigr,nsigm
   if (nsigm>=nsig_max) then
      write(6,*)' FAILURE IN MERGE_VCOORDS, NSIGM > NSIG_MAX.  ADJUST NSIG_MAX ACCORDINGLY'
      call stop2(99)
   endif

   ! make smooth correction so p_m(kkend) == pref1
   sum=zero
   psum=pref1-p_m(kkend)
   do k=1,kkend-1
      delp_m(k)=p_m(k+1)-p_m(k)
      pthis=p_m(k)+half*delp_m(k)
      call blend_df(pthis,dummy,wgt_m(k))
      sum=sum+delp_m(k)*wgt_m(k)
   enddo
   adjust=psum/sum
   do k=1,kkend-1
      p_m(k+1)=p_m(k)+delp_m(k)*(one+adjust*wgt_m(k))
   enddo
   if(print_verbose)then
      do k=1,kkend-1
         write(6,'(" j,k,pref0-p_m(k),p_m(k:k+1),p_m(k+1)-pref1=",2i4,4f9.4)') &
                         j,k,pref0-p_m(k),p_m(k),p_m(k+1),p_m(k+1)-pref1
      enddo
   end if

   ! interpolate ak_r, bk_r, ak5, bk5 to blended bridge pressure levels and construct
   !  ak_m, bk_m, a blended version of original.

   allocate(ak_m(kkend),bk_m(kkend))
   do k=1,kkend
      pthis=p_m(k)
      do kk=1,nsigg
         if (pthis < p_g(kk) .and. pthis >= p_g(kk+1)) then
            delta=(ak5(kk+1)-ak5(kk))/(p_g(kk+1)-p_g(kk))
            ak_gthis=ak5(kk)+delta*(pthis-p_g(kk))
            delta=(bk5(kk+1)-bk5(kk))/(p_g(kk+1)-p_g(kk))
            bk_gthis=bk5(kk)+delta*(pthis-p_g(kk))
            exit
         endif
      enddo
      do kk=1,nsigr
         if (pthis < p_r(kk) .and. pthis >= p_r(kk+1)) then
            delta=(ak_r(kk+1)-ak_r(kk))/(p_r(kk+1)-p_r(kk))
            ak_rthis=ak_r(kk)+delta*(pthis-p_r(kk))
            delta=(bk_r(kk+1)-bk_r(kk))/(p_r(kk+1)-p_r(kk))
            bk_rthis=bk_r(kk)+delta*(pthis-p_r(kk))
            exit
         endif
      enddo
      call blend_f(pthis,gwgt)
      ak_m(k)=gwgt*ak_gthis+(one-gwgt)*ak_rthis
      bk_m(k)=gwgt*bk_gthis+(one-gwgt)*bk_rthis
      if (zero_bkbridge) bk_m(k)=zero
      if(print_verbose) &
        write(6,'(" akgrm,bkgrm=",3f15.5,5x,3f15.5)')ak_gthis,ak_rthis,ak_m(k),bk_gthis,bk_rthis,bk_m(k)
   enddo

   ! create full profile of blended ak, bk

   allocate(akm(nsigm+1),bkm(nsigm+1))
   kk=0
   do k=1,k0r
      kk=kk+1
      akm(kk)=ak_r(k)
      bkm(kk)=bk_r(k)
   enddo
   do k=2,kkend-1
      kk=kk+1
      akm(kk)=ak_m(k)
      bkm(kk)=bk_m(k)
   enddo
   k1m=kk+1
   do k=k1g,nsigg+1
      kk=kk+1
      akm(kk)=ak5(k)
      bkm(kk)=bk5(k)
   enddo
   if(print_verbose)then
      write(6,'(" k0r,k0m,k1g,k1m,nsigg=",5i4)')k0r,k0m,k1g,k1m,nsigg
      write(6,'(" ak_r(k0r),ak_m(k0m),bk_r,bk_m=",4f15.5)') ak_r(k0r),akm(k0m),bk_r(k0r),bkm(k0m)
      write(6,'(" ak_g(k1g),ak_m(k1m),bk_g,bk_m=",4f15.5)') ak5 (k1g),akm(k1m),bk5 (k1g),bkm(k1m)
   end if
   ! plot pressure profiles as function of ps, from ps=1100 to ps=500 and see if anything strange appears.

   allocate(plotp(61,nsigm+1))
   do kk=1,61
      this_psfc=500+ten*(kk-one)
      do k=1,nsigm+1
         plotp(kk,k)=akm(k)+bkm(k)*this_psfc
      enddo
   enddo
   call outgrads1(plotp,61,nsigm+1,'pm')

   ! final step: derive eta1m, eta2m, deta1m, deta2m, aeta1m, aeta2m, blend_rm, blend_gm

   do k=1,nsigm+1
      eta2m(k)=bkm(k)
      eta1m(k)=(akm(k)-pt_ll+eta2m(k)*(pdtop_ll+pt_ll))/pdtop_ll
   enddo

   do k=1,nsigm
      deta1m(k)=eta1m(k)-eta1m(k+1)
      deta2m(k)=eta2m(k)-eta2m(k+1)
   enddo
   do k=1,nsigm
      aeta1m(k)=half*(eta1m(k)+eta1m(k+1))
      aeta2m(k)=half*(eta2m(k)+eta2m(k+1))
   enddo

   if(print_verbose)then
      do k=1,k0m
         write(6,'(" k,eta1,eta1m,diff=",i4,2f15.7,e11.3)')k,eta1(k),eta1m(k),eta1m(k)-eta1(k)
      enddo
      do k=1,k0m
         write(6,'(" k,eta2,eta2m,diff=",i4,2f15.7,e11.3)')k,eta2(k),eta2m(k),eta2m(k)-eta2(k)
      enddo

      do k=k1g,nsigg+1
         write(6,'(" k,km,ak5(k),akm(km),diff=",2i4,2f15.7,e11.3)') k,k-k1g+k1m,ak5(k),akm(k-k1g+k1m),&
                                                                 ak5(k)-akm(k-k1g+k1m)
      enddo
      do k=k1g,nsigg+1
         write(6,'(" k,km,bk5(k),bkm(km),diff=",2i4,2f15.7,e11.3)') k,k-k1g+k1m,bk5(k),bkm(k-k1g+k1m),&
                                                                 bk5(k)-bkm(k-k1g+k1m)
      enddo
   end if
   allocate(blend_rm(nsigm),blend_gm(nsigm))
   do k=1,nsigm
      pthis=aeta1m(k)*pdtop_ll+aeta2m(k)*(psfc-pdtop_ll-pt_ll)+pt_ll
      call blend_f(pthis,blend_gm(k))
      blend_rm(k)=one-blend_gm(k)
      write(6,'(" k,pthis,blend_gm,blend_rm=",i4,f15.7,2f13.7)') k,pthis,blend_gm(k),blend_rm(k)
   enddo
   nsigm_out=nsigm

   k0rm=max(k0r-2,1)
   k1mp=min(k1m+2,nsigm)
   k0rp=min(k0r+1,nsig_save)

   return

end subroutine mix_gfs_nmmb_vcoords

subroutine broadcast_gfs_stratosphere_vars
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    broadcast_gfs_stratosphere_vars
!   prgmmr: parrish          org: np22                date: 2012-02-11
!
! abstract:  Broadcast new vertical coordinate variables to all processors.
!
! program history log:
!   2012-09-13  parrish, initial documentation
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

   use mpimod, only: mype,mpi_integer4,mpi_rtype,mpi_comm_world,ierror

   implicit none

   call mpi_bcast(nsig_max,1,mpi_integer4,0,mpi_comm_world,ierror)
   call mpi_bcast(nsig_save,1,mpi_integer4,0,mpi_comm_world,ierror)
   call mpi_bcast(k0m,1,mpi_integer4,0,mpi_comm_world,ierror)
   call mpi_bcast(k1m,1,mpi_integer4,0,mpi_comm_world,ierror)
   call mpi_bcast(k0r,1,mpi_integer4,0,mpi_comm_world,ierror)
   call mpi_bcast(k1g,1,mpi_integer4,0,mpi_comm_world,ierror)
   call mpi_bcast(k0rm,1,mpi_integer4,0,mpi_comm_world,ierror)
   call mpi_bcast(k1mp,1,mpi_integer4,0,mpi_comm_world,ierror)
   call mpi_bcast(k0rp,1,mpi_integer4,0,mpi_comm_world,ierror)
   call mpi_bcast(nsigg,1,mpi_integer4,0,mpi_comm_world,ierror)
   call mpi_bcast(nsigm,1,mpi_integer4,0,mpi_comm_world,ierror)
   call mpi_bcast(pblend0,1,mpi_rtype,0,mpi_comm_world,ierror)
   call mpi_bcast(pblend1,1,mpi_rtype,0,mpi_comm_world,ierror)
   if ( mype /= 0 ) then
      allocate(deta1_save(nsig_save),aeta1_save(nsig_save),eta1_save(nsig_save+1))
      allocate(deta2_save(nsig_save),aeta2_save(nsig_save),eta2_save(nsig_save+1))
      allocate(blend_rm(nsigm),blend_gm(nsigm))
      allocate(ak5(nsigg+1),bk5(nsigg+1))
   endif
   call mpi_bcast(deta1_save,nsig_save,mpi_rtype,0,mpi_comm_world,ierror)
   call mpi_bcast(aeta1_save,nsig_save,mpi_rtype,0,mpi_comm_world,ierror)
   call mpi_bcast(eta1_save,nsig_save+1,mpi_rtype,0,mpi_comm_world,ierror)
   call mpi_bcast(deta2_save,nsig_save,mpi_rtype,0,mpi_comm_world,ierror)
   call mpi_bcast(aeta2_save,nsig_save,mpi_rtype,0,mpi_comm_world,ierror)
   call mpi_bcast(eta2_save,nsig_save+1,mpi_rtype,0,mpi_comm_world,ierror)
   call mpi_bcast(blend_rm,nsigm,mpi_rtype,0,mpi_comm_world,ierror)
   call mpi_bcast(blend_gm,nsigm,mpi_rtype,0,mpi_comm_world,ierror)
   call mpi_bcast(ak5,nsigg+1,mpi_rtype,0,mpi_comm_world,ierror)
   call mpi_bcast(bk5,nsigg+1,mpi_rtype,0,mpi_comm_world,ierror)

   return

end subroutine broadcast_gfs_stratosphere_vars

subroutine destroy_nmmb_vcoords

   implicit none

   ! deallocate arrays

   deallocate(deta1_save,aeta1_save,eta1_save)
   deallocate(deta2_save,aeta2_save,eta2_save)
   deallocate(ak5,bk5,blend_rm,blend_gm)
   deallocate(ges_tv_r_g,ges_q_r_g,ges_u_r_g,ges_v_r_g,ges_tsen_r_g,ges_oz_r_g)
   deallocate(ges_tv_r  ,ges_q_r  ,ges_u_r  ,ges_v_r  ,ges_tsen_r  ,ges_oz_r  )
   if (allocated(ges_cw_r)) deallocate(ges_cw_r)
   if (allocated(ges_ql_r)) deallocate(ges_ql_r)
   if (allocated(ges_qi_r)) deallocate(ges_qi_r)
   if (allocated(ges_qr_r)) deallocate(ges_qr_r)
   if (allocated(ges_qs_r)) deallocate(ges_qs_r)
   if (allocated(ges_qg_r)) deallocate(ges_qg_r)
   if (allocated(ges_qh_r)) deallocate(ges_qh_r)
   if (allocated(ges_cw_r_g)) deallocate(ges_cw_r_g)
   if (allocated(ges_ql_r_g)) deallocate(ges_ql_r_g)
   if (allocated(ges_qi_r_g)) deallocate(ges_qi_r_g)
   if (allocated(ges_qr_r_g)) deallocate(ges_qr_r_g)
   if (allocated(ges_qs_r_g)) deallocate(ges_qs_r_g)
   if (allocated(ges_qg_r_g)) deallocate(ges_qg_r_g)
   if (allocated(ges_qh_r_g)) deallocate(ges_qh_r_g)

   return

end subroutine destroy_nmmb_vcoords

subroutine add_gfs_stratosphere
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    add_gfs_stratosphere
!   prgmmr: parrish          org: np22                date: 2012-02-18
!
! abstract: This was created from a copy of get_gefs_for_regional.f90.  This subroutine
!             reads in the gfs guess, interpolates in horizontal to nmmb analysis grid,
!             then interpolates in the vertical to the new mixed nmmb/gfs vertical coordinate,
!             blending with nmmb fields in the blend zone pblend0 > p > pblend1.
!             Before the blending of gfs and nmmb can take place, the nmmb fields, which have
!             just been read in to module guess_grids, must be interpolated in the vertical from
!             the original nmmb vertical coordinate to the extended mixed nmmb/gfs vertical coordinate.
!
!
! program history log:
!   2012-02-18  parrish, initial documentation
!   2012-10-11  eliu - add FGAT capability for wrf_nmm_regional (HWRF) 
!   2013-02-08  zhu  - add blending capability for hydrometeros 
!   2013-10-19  todling - metguess now holds background
!   2014-08-18  tong    - modified to allow gfs/gdas spectral coefficients to be
!                         transformed to a coarser resolution grid
!   2014-11-30  todling - update interface to general_read_gfs routines
!   2014-12-03  derber  - modify call to general_read_gfsatm to reduce reading
!                         of unused variables
!   2016-12-10  tong - add code to gfs nemsio meta data, if use_gfs_nemsio=True
!   2019-09-24  martin - add support for when use_gfs_ncio is True
!   2020-05-04  wu   - no rotate_wind for fv3_regional
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ enddocumentation block

   use gridmod, only: regional,wrf_nmm_regional,use_gfs_nemsio,use_gfs_ncio
   use gridmod, only: region_lat,region_lon,aeta1_ll,aeta2_ll,pdtop_ll,pt_ll  
   use gridmod, only: nlon,nlat,lat2,lon2,nsig,rotate_wind_ll2xy,fv3_regional
   use gridmod, only: use_gfs_ozone,jcap_gfs,nlat_gfs,nlon_gfs
   use constants,only: zero,one_tenth,half,one,ten,fv,t0c,r0_05,r60,r3600
   use mpimod, only: mype
   use mpimod, only: mpi_comm_world
   use mpimod, only: npe
   use gsi_bundlemod, only: gsi_bundlegetpointer
   use gsi_bundlemod, only: gsi_bundlecreate
   use gsi_bundlemod, only: gsi_grid
   use gsi_bundlemod, only: gsi_gridcreate
   use gsi_bundlemod, only: gsi_bundle
   use gsi_bundlemod, only: gsi_bundledestroy
   use gsi_metguess_mod, only: gsi_metguess_bundle
   use general_sub2grid_mod, only: sub2grid_info,general_sub2grid_create_info
   use general_sub2grid_mod, only: general_grid2sub,general_sub2grid,general_sub2grid_destroy_info
   use general_specmod, only: spec_vars,general_init_spec_vars,general_destroy_spec_vars
   use egrid2agrid_mod, only: g_create_egrid2points_slow,egrid2agrid_parm,g_egrid2points_faster
   use sigio_module, only: sigio_intkind,sigio_head,sigio_srhead
   use guess_grids, only: ntguessig,nfldsig,ifilesig 
   use guess_grids, only: ges_tsen
   use aniso_ens_util, only: intp_spl
   use obsmod, only: iadate
!   use gfs_stratosphere, only: nsigg,nsig_save,ak5,bk5,aeta1_save,aeta2_save,eta1_save,eta2_save
!   use gfs_stratosphere, only: blend_rm,blend_gm
!   use gfs_stratosphere, only: ges_tv_r,ges_q_r,ges_u_r,ges_v_r,ges_tsen_r,ges_oz_r
!   use gfs_stratosphere, only: ges_cw_r,ges_ql_r,ges_qi_r,ges_qr_r,ges_qs_r,ges_qg_r,ges_qh_r
!   use gfs_stratosphere, only: ges_tv_r_g,ges_q_r_g,ges_u_r_g,ges_v_r_g,ges_tsen_r_g,ges_oz_r_g
!   use gfs_stratosphere, only: ges_cw_r_g,ges_ql_r_g,ges_qi_r_g,ges_qr_r_g,ges_qs_r_g,ges_qg_r_g,ges_qh_r_g
!   use gfs_stratosphere, only: good_o3mr
   use gsi_metguess_mod, only: gsi_metguess_get,gsi_metguess_bundle
   use gsi_bundlemod, only: gsi_bundlegetpointer
   use control_vectors, only: cvars3d
   use nemsio_module, only: nemsio_init,nemsio_open,nemsio_close
   use ncepnems_io, only: error_msg
   use nemsio_module, only: nemsio_gfile,nemsio_getfilehead
   use module_ncio, only: Dataset,Dimension,open_dataset,close_dataset,&
                                 read_attribute,get_dim,read_vardata,&
                                 get_idate_from_time_units

   implicit none
  
   type(sub2grid_info) grd_gfs,grd_mix,grd_gfst
   type(spec_vars) sp_gfs,sp_b
   real(r_kind),allocatable,dimension(:,:,:) :: pri_g,pri_r,prsl_g,prsl_r,prsl_m
   real(r_kind),pointer,dimension(:,:,:) :: vor =>null()
   real(r_kind),pointer,dimension(:,:,:) :: div =>null()
   real(r_kind),pointer,dimension(:,:,:) :: u   =>null()
   real(r_kind),pointer,dimension(:,:,:) :: v   =>null()
   real(r_kind),pointer,dimension(:,:,:) :: tv  =>null()
   real(r_kind),pointer,dimension(:,:,:) :: q   =>null()
   real(r_kind),pointer,dimension(:,:,:) :: cwmr=>null()
   real(r_kind),pointer,dimension(:,:,:) :: oz  =>null()
   real(r_kind),pointer,dimension(:,:)   :: z =>null()
   real(r_kind),pointer,dimension(:,:)   :: ps=>null()
   real(r_kind),allocatable :: work_sub(:,:,:,:),work(:,:,:,:),work_reg(:,:,:,:)
   real(r_kind),allocatable,dimension(:,:,:)::ut,vt,tt,qt,ozt,ttsen,qlt,qit,qrt,qst,qgt,qht
  
   character(len=*),parameter::myname='add_gfs_stratosphere'
   integer(i_kind) it_beg,it_end 
   integer(i_kind) iret,i,j,k,k2,mm1
   integer(i_kind) ku,kv,kt,kq,koz,kcw,kz,kps
   character(255) filename
   character(255),allocatable, dimension(:)::infiles  
   integer(sigio_intkind):: lunges = 11
   type(sigio_head):: sighead
   type(egrid2agrid_parm) :: p_g2r
   integer(i_kind) inner_vars,num_fields,num_fieldst,nsig_gfs,jcap_gfs_test
   integer(i_kind) nord_g2r,jcap_org,nlon_b
   logical,allocatable :: vector(:)
   logical hires
   real(r_kind),allocatable,dimension(:) :: xspli_r,yspliu_r,yspliv_r,xsplo,xsplo_r,ysplou_r,ysplov_r
   real(r_kind),allocatable,dimension(:) :: xspli_g,yspliu_g,yspliv_g,ysplou_g,ysplov_g
   integer(i_kind) iyr,ihourg
   integer(i_kind),dimension(4):: idate4
   integer(i_kind),dimension(8) :: ida,jda 
   integer(i_kind),dimension(5) :: iadate_gfs
   real(r_kind) hourg
   real(r_kind),dimension(5):: fha
   real(r_kind),allocatable,dimension(:):: blend_rm_oz,blend_gm_oz
  
   type(gsi_bundle) :: atm_bundle
   type(gsi_grid)   :: atm_grid
   integer(i_kind),parameter :: n2d=2
   integer(i_kind),parameter :: n3d=8
   character(len=4), parameter :: vars2d(n2d) = (/ 'z   ', 'ps  ' /)
   character(len=4), parameter :: vars3d(n3d) = (/ 'u   ', 'v   ', &
                                                   'vor ', 'div ', &
                                                   'tv  ', 'q   ', &
                                                   'cw  ', 'oz  '  /)
  
  
   real(r_kind) dlon,dlat,uob,vob
   integer(i_kind) ii,jj,it,ier,istatus

   character(len=120) :: my_name = 'ADD_GFS_STRATOSPHERE'
   integer(i_kind) :: latb, lonb, levs, nframe, njcap
   integer(i_kind) :: nfhour, nfminute, nfsecondn, nfsecondd
   integer(i_kind) :: istop = 101
   integer(i_kind),dimension(7):: idate
   real(r_kind) :: fhour
   type(nemsio_gfile) :: gfile
   
   real(r_kind),dimension(:,:  ),pointer:: ges_ps =>NULL()
   real(r_kind),dimension(:,:,:),pointer:: ges_u  =>NULL()
   real(r_kind),dimension(:,:,:),pointer:: ges_v  =>NULL()
   real(r_kind),dimension(:,:,:),pointer:: ges_tv =>NULL()
   real(r_kind),dimension(:,:,:),pointer:: ges_q  =>NULL()
   real(r_kind),dimension(:,:,:),pointer:: ges_oz =>NULL()
  
   ! variables for cloud info
   integer(i_kind) nguess,ier_cw,ier_ql,ier_qi,ier_qr,ier_qs,ier_qg,ier_qh
   integer(i_kind) iqtotal,icw4crtm
   real(r_kind) worktmp
   real(r_kind),pointer,dimension(:,:,:):: ges_cwmr
   real(r_kind),pointer,dimension(:,:,:):: ges_ql
   real(r_kind),pointer,dimension(:,:,:):: ges_qi
   real(r_kind),pointer,dimension(:,:,:):: ges_qr
   real(r_kind),pointer,dimension(:,:,:):: ges_qs
   real(r_kind),pointer,dimension(:,:,:):: ges_qg
   real(r_kind),pointer,dimension(:,:,:):: ges_qh
   logical print_verbose

   ! variables for netcdf io
   type(Dataset) :: atmges
   type(Dimension) :: ncdim
   integer(i_kind),dimension(6):: idate6
   real(r_kind),allocatable,dimension(:) :: fhour2
   

   ! allocate space for saving original regional model guess and original blended regional-global guess:

   allocate(ges_tv_r_g(lat2,lon2,nsig,nfldsig))
   allocate(ges_q_r_g (lat2,lon2,nsig,nfldsig))
   allocate(ges_u_r_g (lat2,lon2,nsig,nfldsig))
   allocate(ges_v_r_g (lat2,lon2,nsig,nfldsig))
   allocate(ges_tsen_r_g (lat2,lon2,nsig,nfldsig))
   allocate(ges_oz_r_g(lat2,lon2,nsig,nfldsig))
   allocate(ges_tv_r  (lat2,lon2,nsig_save,nfldsig))
   allocate(ges_q_r   (lat2,lon2,nsig_save,nfldsig))
   allocate(ges_u_r   (lat2,lon2,nsig_save,nfldsig))
   allocate(ges_v_r   (lat2,lon2,nsig_save,nfldsig))
   allocate(ges_tsen_r   (lat2,lon2,nsig_save,nfldsig))
   allocate(ges_oz_r  (lat2,lon2,nsig_save,nfldsig))

   print_verbose=.false.
   if(verbose)print_verbose=.true.
   ! Inquire about cloud guess fields
   call gsi_metguess_get('dim',nguess,istatus)

   ! Determine whether or not cloud-condensate is the control variable (ges_cw=ges_ql+ges_qi)
   icw4crtm=getindex(cvars3d,'cw')

   ! Determine whether or not total moisture (water vapor+total cloud condensate) is the control variable
   iqtotal=getindex(cvars3d,'qt')

   ! Get pointer to cloud water mixing ratio
   if ( nguess>0 .and. ( (icw4crtm /= 0) .or. (iqtotal /= 0) ) ) then
      allocate(ges_cw_r_g(lat2,lon2,nsig,nfldsig))
      allocate(ges_ql_r_g(lat2,lon2,nsig,nfldsig))
      allocate(ges_qi_r_g(lat2,lon2,nsig,nfldsig))
      allocate(ges_qr_r_g(lat2,lon2,nsig,nfldsig))
      allocate(ges_qs_r_g(lat2,lon2,nsig,nfldsig))
      allocate(ges_qg_r_g(lat2,lon2,nsig,nfldsig))
      allocate(ges_qh_r_g(lat2,lon2,nsig,nfldsig))
      allocate(ges_cw_r  (lat2,lon2,nsig_save,nfldsig))
      allocate(ges_ql_r  (lat2,lon2,nsig_save,nfldsig))
      allocate(ges_qi_r  (lat2,lon2,nsig_save,nfldsig))
      allocate(ges_qr_r  (lat2,lon2,nsig_save,nfldsig))
      allocate(ges_qs_r  (lat2,lon2,nsig_save,nfldsig))
      allocate(ges_qg_r  (lat2,lon2,nsig_save,nfldsig))
      allocate(ges_qh_r  (lat2,lon2,nsig_save,nfldsig))
   endif

   ! first, save current contents of ges_tv, etc  (later, consider how to save only from bottom of blend zone
   !                                               to regional model top, for computational savings)
   do it=1,nfldsig
      ier=0
      call gsi_bundlegetpointer(gsi_metguess_bundle(it),'u'  ,ges_u ,istatus) 
      ier=ier+istatus
      call gsi_bundlegetpointer(gsi_metguess_bundle(it),'v'  ,ges_v ,istatus) 
      ier=ier+istatus
      call gsi_bundlegetpointer(gsi_metguess_bundle(it),'tv' ,ges_tv,istatus) 
      ier=ier+istatus
      call gsi_bundlegetpointer(gsi_metguess_bundle(it),'q'  ,ges_q ,istatus) 
      ier=ier+istatus
      call gsi_bundlegetpointer(gsi_metguess_bundle(it),'oz' ,ges_oz,istatus) 
      ier=ier+istatus
      if ( ier /= 0 ) call die(myname,': missing guess vars, aborting ...',ier)
      do k=1,nsig_save
         do j=1,lon2
            do i=1,lat2
               ges_tv_r(i,j,k,it)=ges_tv(i,j,k)
               ges_q_r (i,j,k,it)=ges_q (i,j,k)
               ges_u_r (i,j,k,it)=ges_u (i,j,k)
               ges_v_r (i,j,k,it)=ges_v (i,j,k)
               ges_tsen_r(i,j,k,it)=ges_tsen(i,j,k,it)
               ges_oz_r(i,j,k,it)=ges_oz(i,j,k)
            enddo
         enddo
      enddo

      if (nguess>0 .and. ((icw4crtm /= 0) .or. (iqtotal /= 0))) then
         call gsi_bundlegetpointer (gsi_metguess_bundle(it),'ql',ges_ql,iret); ier_ql=iret
         call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qi',ges_qi,iret); ier_qi=iret
         call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qr',ges_qr,iret); ier_qr=iret
         call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qs',ges_qs,iret); ier_qs=iret
         call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qg',ges_qg,iret); ier_qg=iret
         call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qh',ges_qh,iret); ier_qh=iret
  
         do k=1,nsig_save
            do j=1,lon2
               do i=1,lat2
                  if (ier_ql==0) ges_ql_r(i,j,k,it)=ges_ql(i,j,k)
                  if (ier_qi==0) ges_qi_r(i,j,k,it)=ges_qi(i,j,k)
                  if (ier_qr==0) ges_qr_r(i,j,k,it)=ges_qr(i,j,k)
                  if (ier_qs==0) ges_qs_r(i,j,k,it)=ges_qs(i,j,k)
                  if (ier_qg==0) ges_qg_r(i,j,k,it)=ges_qg(i,j,k)
                  if (ier_qh==0) ges_qh_r(i,j,k,it)=ges_qh(i,j,k)
               enddo
            enddo
         enddo
  
         call gsi_bundlegetpointer (gsi_metguess_bundle(it),'cw',ges_cwmr,iret); ier_cw=iret
         if (ier_cw==0 .and. ier_ql==0 .and. ier_qi==0) then
            do k=1,nsig_save
               do j=1,lon2
                  do i=1,lat2
                     ges_cwmr(i,j,k)=ges_ql(i,j,k)+ges_qi(i,j,k)
                     ges_cw_r(i,j,k,it)=ges_cwmr(i,j,k)
                  enddo
               enddo
            enddo
         endif
      endif
   enddo ! do it=1,nfldsig

   ! figure out what are acceptable dimensions for global grid, based on resolution of input spectral coefs
   ! need to inquire from file what is spectral truncation, then setup general spectral structure variable

   ! Determine input GFS filenames
   it_beg=1
   it_end=nfldsig
   allocate(infiles(nfldsig))
   do it=it_beg,it_end
      write(filename,'("gfs_sigf",i2.2)')ifilesig(it)
      infiles(it)=filename
      if (mype==0) then
         write(6,*) 'add_gfs_stratosphere: gfs file required: nfldsig     = ',nfldsig              
         write(6,*) 'add_gfs_stratosphere: gfs file required: ifilesig(it)= ',ifilesig(it)
         write(6,*) 'add_gfs_stratosphere: gfs file required: infiles(it) = ',trim(infiles(it))                      
         write(6,*) 'add_gfs_stratosphere: gfs file required: ntguessig   = ',ntguessig                      
      endif
   enddo

   ! Loop through input GFS files
   it_loop: do it = it_beg,it_end  

      ier=0
      call gsi_bundlegetpointer(gsi_metguess_bundle(it),'ps' ,ges_ps,istatus)
      ier=ier+istatus
      call gsi_bundlegetpointer(gsi_metguess_bundle(it),'u'  ,ges_u ,istatus) 
      ier=ier+istatus
      call gsi_bundlegetpointer(gsi_metguess_bundle(it),'v'  ,ges_v ,istatus) 
      ier=ier+istatus
      call gsi_bundlegetpointer(gsi_metguess_bundle(it),'tv' ,ges_tv,istatus) 
      ier=ier+istatus
      call gsi_bundlegetpointer(gsi_metguess_bundle(it),'q'  ,ges_q ,istatus) 
      ier=ier+istatus
      call gsi_bundlegetpointer(gsi_metguess_bundle(it),'oz' ,ges_oz,istatus) 
      ier=ier+istatus
      if ( ier /=0 ) call die(myname,': missing guess vars, aborting ...',ier)
     
      filename=infiles(it)    
      if (mype==0) write(6,*)'add_gfs_stratosphere: reading in gfs file: ',trim(filename)                       
      if ( (.not. use_gfs_nemsio) .and. (.not. use_gfs_ncio) ) then
         open(lunges,file=trim(filename),form='unformatted')
         call sigio_srhead(lunges,sighead,iret)
         close(lunges)
         if ( mype == 0 ) then
            write(6,*) ' sighead%fhour,sighead%idate=',sighead%fhour,sighead%idate
            write(6,*) ' iadate(y,m,d,hr,min)=',iadate
            write(6,*) ' sighead%latf,sighead%lonf=',sighead%latf,sighead%lonf
         endif
         jcap_org=sighead%jcap
      else if (use_gfs_ncio) then
         atmges = open_dataset(filename)
         ncdim = get_dim(atmges, 'grid_xt'); lonb = ncdim%len
         ncdim = get_dim(atmges, 'grid_yt'); latb = ncdim%len
         ncdim = get_dim(atmges, 'pfull'); levs = ncdim%len
         if ( mype == 0 ) then
            write(6,*) 'ncio lonb latb lev =',lonb, latb, levs
         endif
         njcap = -9999
         ! FV3GFS write component does not include JCAP, infer from DIMY-2
         if (njcap<0) njcap=latb-2
         jcap_org = njcap
         idate6 = get_idate_from_time_units(atmges)
         call read_vardata(atmges, 'time', fhour2)
         fhour = fhour2(1)
         call close_dataset(atmges)
         
      else
         call nemsio_init(iret=iret)
         if (iret /= 0) call error_msg(trim(my_name),trim(filename),' ','init',istop,iret)
   
         call nemsio_open(gfile,filename,'READ',iret=iret)
         if (iret /= 0) call error_msg(trim(my_name),trim(filename),' ','open',istop,iret)

         call nemsio_getfilehead(gfile,iret=iret,nframe=nframe, &
              nfhour=nfhour,nfminute=nfminute,nfsecondn=nfsecondn,nfsecondd=nfsecondd, &
              idate=idate,dimx=lonb,dimy=latb,dimz=levs,jcap=njcap)

         ! FV3GFS write component does not include JCAP, infer from DIMY-2
         if (njcap<0) njcap=latb-2

         if (  nframe /= 0 ) call error_msg(trim(my_name),trim(filename),'nframe', &
                                            'getfilehead',istop,nframe)

         fhour = real(nfhour,r_kind) + real(nfminute,r_kind)/r60 + &
                 real(nfsecondn,r_kind)/real(nfsecondd,r_kind)/r3600
         if ( mype == 0 ) then
            write(6,*) ' input filename=',filename
            write(6,*) ' nemsio head: fhour,idate=',fhour,idate
            write(6,*) ' iadate(y,m,d,hr,min)=',iadate
            write(6,*) ' nemsio head: latb, lonb=', latb, lonb,njcap
         end if
   
         call nemsio_close(gfile,iret=iret)
         if ( iret /= 0 ) call error_msg(trim(my_name),trim(filename),' ', &
                                         'close',istop,iret)
         jcap_org=njcap
      end if 
   
      ! Extract header information
      if((.not. use_gfs_nemsio) .and. (.not. use_gfs_ncio))then
         hourg    = sighead%fhour
         idate4(1)= sighead%idate(1)
         idate4(2)= sighead%idate(2)
         idate4(3)= sighead%idate(3)
         idate4(4)= sighead%idate(4)
      else if (use_gfs_ncio) then
         hourg = fhour
         idate4(1) = idate6(4)  !hour
         idate4(2) = idate6(2)  !month
         idate4(3) = idate6(3)  !day
         idate4(4) = idate6(1)  !year
      else 
         hourg = fhour
         idate4(1) = idate(4)  !hour
         idate4(2) = idate(2)  !month
         idate4(3) = idate(3)  !day
         idate4(4) = idate(1)  !year
      end if

      ! Compute valid time from ensemble date and forecast length and compare to iadate, the analysis time
      iyr=idate4(4)
      ihourg=hourg
      if (iyr>=0.and.iyr<=99) then
         if (iyr>51) then
            iyr=iyr+1900
         else
            iyr=iyr+2000
         endif
      endif
      fha=zero ; ida=0; jda=0
      fha(2)=ihourg    ! relative time interval in hours
      ida(1)=iyr       ! year
      ida(2)=idate4(2) ! month
      ida(3)=idate4(3) ! day
      ida(4)=0         ! time zone
      ida(5)=idate4(1) ! hour
      call w3movdat(fha,ida,jda)
      iadate_gfs(1)=jda(1) ! year
      iadate_gfs(2)=jda(2) ! mon
      iadate_gfs(3)=jda(3) ! day
      iadate_gfs(4)=jda(5) ! hour
      iadate_gfs(5)=0      ! minute
      if (mype == 0) then
         write(6,*)' in add_gfs_stratosphere, iadate_gefs=',iadate_gfs
         write(6,*)' in add_gfs_stratosphere, iadate    =',iadate
      endif
      if (.not. wrf_nmm_regional) then
         if (iadate_gfs(1)/=iadate(1).or.iadate_gfs(2)/=iadate(2).or.iadate_gfs(3)/=iadate(3).or.&
                                        iadate_gfs(4)/=iadate(4).or.iadate_gfs(5)/=iadate(5) ) then
            if (mype == 0) write(6,*)' IN ADD_GFS_STRATOSPHERE, GFS DATE NOT EQUAL TO ANALYSIS DATE, PROGRAM STOPS'
            call stop2(85)
         endif
      endif

      inner_vars=1
      nsig_gfs=nsigg
      num_fields=6*nsig_gfs+2      !  want to transfer u,v,t,q,oz,cw,ps,z from gfs subdomain to slab
      num_fieldst=min(num_fields,npe)!  want to transfer u,v,t,q,oz,cw,ps,z from gfs subdomain to slab
                                !  later go through this code, adapting gsibundlemod, since currently 
                                !   hardwired.
     
      hires=.false.
      nlon_b=((2*jcap_org+1)/nlon_gfs+1)*nlon_gfs
      if ( nlon_b > nlon_gfs ) then
         hires=.true.
      else
         hires=.false.
         if((.not. use_gfs_nemsio).and.(.not. use_gfs_ncio))then
            jcap_gfs=sighead%jcap
            nlat_gfs=sighead%latf+2
            nlon_gfs=sighead%lonf
         else
            jcap_gfs=njcap
            nlat_gfs=latb+2
            nlon_gfs=lonb
         endif 
      end if
     
      if (mype==0) then
         write(6,*)' in add_gfs_stratosphere before general_sub2grid_create_info'                                                
         write(6,*)' in add_gfs_stratosphere: num_fields = ', num_fields,num_fieldst  
         write(6,*)' in add_gfs_stratosphere: jcap_org, jcap_gfs= ', &
                  jcap_org, jcap_gfs
         write(6,*)' in add_gfs_stratosphere: nlon_b, nlon_gfs, hires=', &
                              nlon_b, nlon_gfs, hires
      end if
     
      call general_sub2grid_create_info(grd_gfst,inner_vars,nlat_gfs,nlon_gfs,nsig_gfs,num_fieldst, &
                                      .not.regional)
      allocate(vector(num_fields))
      vector=.false.
      vector(1:2*nsig_gfs)=.true.
      call general_sub2grid_create_info(grd_gfs,inner_vars,nlat_gfs,nlon_gfs,nsig_gfs,num_fields, &
                                      .not.regional,vector)
      jcap_gfs_test=jcap_gfs
      call general_init_spec_vars(sp_gfs,jcap_gfs,jcap_gfs_test,grd_gfs%nlat,grd_gfs%nlon)
      if ( hires .and. .not. use_gfs_nemsio .and. .not. use_gfs_ncio) call general_init_spec_vars(sp_b,jcap_org,jcap_org,nlat_gfs,nlon_b)

      !  also want to set up regional grid structure variable grd_mix, which still has number of
      !   vertical levels set to nsig_gfs, but horizontal dimensions set to regional domain.
      call general_sub2grid_create_info(grd_mix,inner_vars,nlat,nlon,nsig_gfs, &
                                        num_fields,regional,vector)

      ! create interpolation information for global grid to regional ensemble grid
      nord_g2r=4
      call g_create_egrid2points_slow(nlat*nlon,region_lat,region_lon, &
                       grd_gfs%nlat,sp_gfs%rlats,grd_gfs%nlon,sp_gfs%rlons,nord_g2r,p_g2r)

      ! Allocate bundle on global grid
      call gsi_gridcreate(atm_grid,grd_gfs%lat2,grd_gfs%lon2,grd_gfs%nsig)
      call gsi_bundlecreate(atm_bundle,atm_grid,'aux-atm-read',istatus,names2d=vars2d,names3d=vars3d)
      if ( istatus /= 0 ) call die(myname,': trouble create atm_bundle ... ',istatus)

      if ( use_gfs_nemsio ) then
         call general_read_gfsatm_nems(grd_gfst,sp_gfs,filename,.true.,.false.,.true., &
                                       atm_bundle,.true.,iret)
      else if (use_gfs_ncio) then
         call general_read_gfsatm_nc(grd_gfst,sp_gfs,filename,.true.,.false.,.true., &
                                       atm_bundle,.true.,iret)
      else
         if ( hires ) then
            call general_read_gfsatm(grd_gfst,sp_gfs,sp_b,filename,.true.,.false.,.true., &
                                     atm_bundle,.true.,iret)
         else
            call general_read_gfsatm(grd_gfst,sp_gfs,sp_gfs,filename,.true.,.false.,.true., &
                                     atm_bundle,.true.,iret)
         endif
      endif

      ier = 0
      call gsi_bundlegetpointer(atm_bundle,'vor' ,vor ,istatus) ; ier = ier + istatus
      call gsi_bundlegetpointer(atm_bundle,'div' ,div ,istatus) ; ier = ier + istatus
      call gsi_bundlegetpointer(atm_bundle,'u'   ,u   ,istatus) ; ier = ier + istatus
      call gsi_bundlegetpointer(atm_bundle,'v'   ,v   ,istatus) ; ier = ier + istatus
      call gsi_bundlegetpointer(atm_bundle,'tv'  ,tv  ,istatus) ; ier = ier + istatus
      call gsi_bundlegetpointer(atm_bundle,'q'   ,q   ,istatus) ; ier = ier + istatus
      call gsi_bundlegetpointer(atm_bundle,'oz'  ,oz  ,istatus) ; ier = ier + istatus
      call gsi_bundlegetpointer(atm_bundle,'cw'  ,cwmr,istatus) ; ier = ier + istatus
      call gsi_bundlegetpointer(atm_bundle,'z'   ,z   ,istatus) ; ier = ier + istatus
      call gsi_bundlegetpointer(atm_bundle,'ps'  ,ps  ,istatus) ; ier = ier + istatus
      if ( ier /= 0 ) call die(myname,': missing atm_bundle vars, aborting ...',ier)

      allocate(work_sub(grd_gfs%inner_vars,grd_gfs%lat2,grd_gfs%lon2,num_fields))
      do k=1,grd_gfs%nsig
         ku=k ; kv=k+grd_gfs%nsig ; kt=k+2*grd_gfs%nsig ; kq=k+3*grd_gfs%nsig ; koz=k+4*grd_gfs%nsig
         kcw=k+5*grd_gfs%nsig
         do j=1,grd_gfs%lon2
            do i=1,grd_gfs%lat2
               work_sub(1,i,j,ku)=u(i,j,k)
               work_sub(1,i,j,kv)=v(i,j,k)
               work_sub(1,i,j,kt)=tv(i,j,k)
               work_sub(1,i,j,kq)=q(i,j,k)
               work_sub(1,i,j,koz)=oz(i,j,k)
               work_sub(1,i,j,kcw)=cwmr(i,j,k)
            enddo
         enddo
      enddo

      kz=num_fields ; kps=kz-1
      do j=1,grd_gfs%lon2
         do i=1,grd_gfs%lat2
            work_sub(1,i,j,kz)=z(i,j)
            work_sub(1,i,j,kps)=ps(i,j)
         enddo
      enddo

      call gsi_bundledestroy(atm_bundle,istatus)

      allocate(work(grd_gfs%inner_vars,grd_gfs%nlat,grd_gfs%nlon,grd_gfs%kbegin_loc:grd_gfs%kend_alloc))
      call general_sub2grid(grd_gfs,work_sub,work)
      deallocate(work_sub)

      ! then interpolate to regional analysis grid
      allocate(work_reg(grd_mix%inner_vars,grd_mix%nlat,grd_mix%nlon,grd_gfs%kbegin_loc:grd_gfs%kend_alloc))
      do k=grd_gfs%kbegin_loc,grd_gfs%kend_loc
         call g_egrid2points_faster(p_g2r,work(1,1,1,k),work_reg(1,1,1,k),vector(k))
      enddo
      deallocate(work)

      ! next general_grid2sub to go to regional grid subdomains.
      allocate(work_sub(grd_mix%inner_vars,grd_mix%lat2,grd_mix%lon2,num_fields))
      call general_grid2sub(grd_mix,work_reg,work_sub)
      deallocate(work_reg)
      allocate(pri_g(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig+1))

      ! IN FOLLOWING SECTION GET 3D PRESSURE FOR GFS.

      ! compute 3d pressure on interfaces
      pri_g=zero
      k=1
      k2=nsigg+1

      ! FOR NOW, ONLY CONSIDER pressure defined by ak5,bk5
      do j=1,lon2
         do i=1,lat2
            pri_g(i,j,k)=work_sub(1,i,j,kps)
            pri_g(i,j,k2)=zero
         enddo
      enddo
      do k=2,nsigg
         do j=1,lon2
            do i=1,lat2
               pri_g(i,j,k)=one_tenth*ak5(k)+bk5(k)*work_sub(1,i,j,kps)
               if ( mype==0 .and. i==10 .and. j==10 .and. print_verbose) &
               write(6,'(" k, pri_g,ak5,bk5,ps=",i3,4f18.3)') k,pri_g(i,j,k),ak5(k),bk5(k),work_sub(1,i,j,kps)
            enddo
         enddo
      enddo

      ! Get 3d pressure field now on layers
      ! FOR NOW, ASSUME IDSL==2, so layer value is just average of interface values
      ! (also apparently the definition used by nmmb)

      allocate(prsl_g(lat2,lon2,nsigg))
      do k=1,nsigg
         do j=1,lon2
            do i=1,lat2
               prsl_g(i,j,k)=(pri_g(i,j,k)+pri_g(i,j,k+1))*half
               if ( mype==0 .and. i==10 .and. j==10 .and. print_verbose ) &
               write(6,'(" k, prsl_g=",i3,f18.3)') k,prsl_g(i,j,k)
            enddo
         enddo
      enddo
      deallocate(pri_g)

      ! IN THIS SECTION GET 3D PRESSURE FOR INPUT NMMB.

      allocate(pri_r(lat2,lon2,nsig_save+1))
      do k=1,nsig_save+1
         do j=1,lon2
            do i=1,lat2
               pri_r(i,j,k)=one_tenth*(eta1_save(k)*pdtop_ll + &
                            eta2_save(k)*(ten*ges_ps(i,j)-pdtop_ll-pt_ll) + & !                         
                            pt_ll)
            enddo
         enddo
      enddo
      deallocate(pri_r)
      allocate(prsl_r(lat2,lon2,nsig_save))
      do k=1,nsig_save
         do j=1,lon2
            do i=1,lat2
               prsl_r(i,j,k)=one_tenth*(aeta1_save(k)*pdtop_ll + &
                             aeta2_save(k)*(ten*ges_ps(i,j)-pdtop_ll-pt_ll) + & 
                             pt_ll)
            enddo
         enddo
      enddo

      ! IF THERE IS NO REGIONAL GES OZONE (good_o3mr=F), THEN INTERPOLATE GLOBAL OZONE TO NMMB VERT COORD:

      if ( .not.good_o3mr ) then
         allocate(xsplo_r(nsig_save),ysplou_r(nsig_save))
         allocate(xspli_g(nsigg),yspliu_g(nsigg))
         do j=1,lon2
            do i=1,lat2
               do k=1,nsigg
                  xspli_g(k)=log(prsl_g(i,j,k)*ten)
               enddo
               do k=1,nsig_save
                  xsplo_r(k)=log(prsl_r(i,j,k)*ten)
               enddo
     
               ! global ozone:
               do k=1,nsigg
                  koz=k+4*grd_gfs%nsig
                  yspliu_g(k)=work_sub(1,i,j,koz)
               enddo
               call intp_spl(xspli_g,yspliu_g,xsplo_r,ysplou_r,nsigg,nsig_save)
               ! following is to correct for bug in intp_spl
               do k=1,nsig_save
                  if (xsplo_r(k) < xspli_g(nsigg)) ysplou_r(k)=yspliu_g(nsigg)
                  if (xsplo_r(k) > xspli_g(1)) ysplou_r(k)=yspliu_g(1)
               enddo
               do k=1,nsig_save
                  ges_oz_r(i,j,k,it)=ysplou_r(k)         
               enddo
            enddo
         enddo
         deallocate(xsplo_r,ysplou_r,xspli_g,yspliu_g)
      endif

      ! IN THIS SECTION GET 3D PRESSURE FOR MERGED (TARGET) PRESSURE

      ! allocate(pri_m(lat2,lon2,nsig+1))
      ! do k=1,nsig+1
      !    do j=1,lon2
      !       do i=1,lat2
      !          pri_m(i,j,k)=one_tenth*(eta1_ll(k)*pdtop_ll + &
      !                       eta2_ll(k)*(ten*ges_ps(i,j)-pdtop_ll-pt_ll) + &        
      !                       pt_ll)
      !       enddo
      !    enddo
      ! enddo
      allocate(prsl_m(lat2,lon2,nsig))
      do k=1,nsig
         do j=1,lon2
            do i=1,lat2
               prsl_m(i,j,k)=one_tenth*(aeta1_ll(k)*pdtop_ll + &
                             aeta2_ll(k)*(ten*ges_ps(i,j)-pdtop_ll-pt_ll) + &       
                             pt_ll)
            enddo
         enddo
      enddo

      allocate(xspli_r(nsig_save),yspliu_r(nsig_save),yspliv_r(nsig_save),xsplo(nsig))
      allocate(ysplou_r(nsig),ysplov_r(nsig),ysplou_g(nsig),ysplov_g(nsig))
      allocate(xspli_g(nsigg),yspliu_g(nsigg),yspliv_g(nsigg))
     
      allocate(ut(lat2,lon2,nsig))
      allocate(vt(lat2,lon2,nsig))
      allocate(tt(lat2,lon2,nsig))
      allocate(ttsen(lat2,lon2,nsig))
      allocate(qt(lat2,lon2,nsig))
      allocate(ozt(lat2,lon2,nsig))
      if (nguess>0 .and. ((icw4crtm /= 0) .or. (iqtotal /= 0))) then
         allocate(qlt(lat2,lon2,nsig))
         allocate(qit(lat2,lon2,nsig))
         allocate(qrt(lat2,lon2,nsig))
         allocate(qst(lat2,lon2,nsig))
         allocate(qgt(lat2,lon2,nsig))
         allocate(qht(lat2,lon2,nsig))
      endif
      mm1=mype+1
      allocate(blend_rm_oz(nsig))
      allocate(blend_gm_oz(nsig))
      blend_rm_oz=zero
      blend_gm_oz=zero
      if ( use_gfs_ozone ) then
         if (good_o3mr) then
            blend_rm_oz=blend_rm
            blend_gm_oz=blend_gm
         else
            blend_rm_oz=zero
            blend_gm_oz=one
         endif
      endif

      do j=1,lon2
         do i=1,lat2

            ii=i+grd_mix%istart(mm1)-2
            jj=j+grd_mix%jstart(mm1)-2
            ii=min(grd_mix%nlat,max(1,ii))
            jj=min(grd_mix%nlon,max(1,jj))
            dlon=real(jj,r_kind)
            dlat=real(ii,r_kind)
            do k=1,nsig_save
               xspli_r(k)=log(prsl_r(i,j,k)*ten)
            enddo
            do k=1,nsigg
               xspli_g(k)=log(prsl_g(i,j,k)*ten)
            enddo
            do k=1,nsig
               xsplo(k)=log(prsl_m(i,j,k)*ten)
            enddo
     
            ! u,v -- regional contribution
            do k=1,nsig_save
               yspliu_r(k)=ges_u(i,j,k) 
               yspliv_r(k)=ges_v(i,j,k)  
            enddo
            call intp_spl(xspli_r,yspliu_r,xsplo,ysplou_r,nsig_save,nsig)
            call intp_spl(xspli_r,yspliv_r,xsplo,ysplov_r,nsig_save,nsig)
            ! following is to correct for bug in intp_spl
            do k=1,nsig
               if (xsplo(k) < xspli_r(nsig_save)) ysplou_r(k)=yspliu_r(nsig_save)
               if (xsplo(k) > xspli_r(1)) ysplou_r(k)=yspliu_r(1)
               if (xsplo(k) < xspli_r(nsig_save)) ysplov_r(k)=yspliv_r(nsig_save)
               if (xsplo(k) > xspli_r(1)) ysplov_r(k)=yspliv_r(1)
            enddo
            ! u,v -- global contribution
            do k=1,nsigg
               ku=k ; kv=k+grd_gfs%nsig
               yspliu_g(k)=work_sub(1,i,j,ku)
               yspliv_g(k)=work_sub(1,i,j,kv)
            enddo
            call intp_spl(xspli_g,yspliu_g,xsplo,ysplou_g,nsigg,nsig)
            call intp_spl(xspli_g,yspliv_g,xsplo,ysplov_g,nsigg,nsig)
            ! following is to correct for bug in intp_spl
            do k=1,nsig
               if (xsplo(k) < xspli_g(nsigg)) ysplou_g(k)=yspliu_g(nsigg)
               if (xsplo(k) > xspli_g(1)) ysplou_g(k)=yspliu_g(1)
               if (xsplo(k) < xspli_g(nsigg)) ysplov_g(k)=yspliv_g(nsigg)
               if (xsplo(k) > xspli_g(1)) ysplov_g(k)=yspliv_g(1)
            enddo
            ! blend contributions from regional and global:
            if(fv3_regional)then
               do k=1,nsig
                  ut(i,j,k)=blend_rm(k)*ysplou_r(k)+blend_gm(k)*ysplou_g(k)
                  vt(i,j,k)=blend_rm(k)*ysplov_r(k)+blend_gm(k)*ysplov_g(k)
               enddo
            else
               do k=1,nsig
                  ! rotate gfs wind to nmmb coordinate:
                  call rotate_wind_ll2xy(ysplou_g(k),ysplov_g(k), &
                                         uob,vob,region_lon(ii,jj),dlon,dlat)
                  ut(i,j,k)=blend_rm(k)*ysplou_r(k)+blend_gm(k)*uob
                  vt(i,j,k)=blend_rm(k)*ysplov_r(k)+blend_gm(k)*vob
               enddo
            endif

            ! t -- regional contribution
            do k=1,nsig_save
               yspliu_r(k)=ges_tv(i,j,k) 
            enddo
            call intp_spl(xspli_r,yspliu_r,xsplo,ysplou_r,nsig_save,nsig) ! try replacing this with
                                                                          ! linear interpolation and compare result
            !do k=1,nsig
            !   pthis=xsplo(k)
            !   do kk=1,nsig_save-1
            !      if (pthis < xspli_r(kk) .and. pthis >= xspli_r(kk+1)) then
            !         delta=(yspliu_r(kk+1)-yspliu_r(kk))/(xspli_r(kk+1)-xspli_r(kk))
            !         ysplou_r(k)=yspliu_r(kk)+delta*(pthis-xspli_r(kk))
            !         exit
            !      endif
            !   enddo
            !enddo
            ! following is to correct for bug in intp_spl
            do k=1,nsig
               if (xsplo(k) < xspli_r(nsig_save)) ysplou_r(k)=yspliu_r(nsig_save)
               if (xsplo(k) > xspli_r(1)) ysplou_r(k)=yspliu_r(1)
            enddo
            ! t -- global contribution
            do k=1,nsigg
               kt=k+2*grd_gfs%nsig
               yspliu_g(k)=work_sub(1,i,j,kt)
            enddo
            call intp_spl(xspli_g,yspliu_g,xsplo,ysplou_g,nsigg,nsig)
             
            ! following is to correct for bug in intp_spl
            do k=1,nsig
               if (xsplo(k) < xspli_g(nsigg)) ysplou_g(k)=yspliu_g(nsigg)
               if (xsplo(k) > xspli_g(1)) ysplou_g(k)=yspliu_g(1)
            enddo
            ! blend contributions from regional and global:
            do k=1,nsig
               tt(i,j,k)=blend_rm(k)*ysplou_r(k)+blend_gm(k)*ysplou_g(k)
            enddo
            ! q -- regional contribution
            do k=1,nsig_save
               yspliu_r(k)=ges_q(i,j,k)
            enddo
            call intp_spl(xspli_r,yspliu_r,xsplo,ysplou_r,nsig_save,nsig)
            ! following is to correct for bug in intp_spl
            do k=1,nsig
               if (xsplo(k) < xspli_r(nsig_save)) ysplou_r(k)=yspliu_r(nsig_save)
               if (xsplo(k) > xspli_r(1)) ysplou_r(k)=yspliu_r(1)
            enddo
            ! q -- global contribution
            do k=1,nsigg
               kq=k+3*grd_gfs%nsig
               yspliu_g(k)=work_sub(1,i,j,kq)
            enddo
            call intp_spl(xspli_g,yspliu_g,xsplo,ysplou_g,nsigg,nsig)
            ! following is to correct for bug in intp_spl
            do k=1,nsig
               if (xsplo(k) < xspli_g(nsigg)) ysplou_g(k)=yspliu_g(nsigg)
               if (xsplo(k) > xspli_g(1)) ysplou_g(k)=yspliu_g(1)
            enddo
            ! blend contributions from regional and global:
            do k=1,nsig
               qt(i,j,k)=blend_rm(k)*ysplou_r(k)+blend_gm(k)*ysplou_g(k)
               ttsen(i,j,k)=tt(i,j,k)/(one+fv*qt(i,j,k))
            enddo
            ! oz -- regional contribution
            do k=1,nsig_save
               yspliu_r(k)=ges_oz(i,j,k) 
            enddo
            call intp_spl(xspli_r,yspliu_r,xsplo,ysplou_r,nsig_save,nsig)
            ! following is to correct for bug in intp_spl
            do k=1,nsig
               if (xsplo(k) < xspli_r(nsig_save)) ysplou_r(k)=yspliu_r(nsig_save)
               if (xsplo(k) > xspli_r(1)) ysplou_r(k)=yspliu_r(1)
            enddo
            ! oz -- global contribution
            do k=1,nsigg
               koz=k+4*grd_gfs%nsig
               yspliu_g(k)=work_sub(1,i,j,koz)
            enddo
            call intp_spl(xspli_g,yspliu_g,xsplo,ysplou_g,nsigg,nsig)
            ! following is to correct for bug in intp_spl
            do k=1,nsig
               if (xsplo(k) < xspli_g(nsigg)) ysplou_g(k)=yspliu_g(nsigg)
               if (xsplo(k) > xspli_g(1)) ysplou_g(k)=yspliu_g(1)
            enddo
            ! blend contributions from regional and global:
            do k=1,nsig
               ozt(i,j,k)=blend_rm_oz(k)*ysplou_r(k)+blend_gm_oz(k)*ysplou_g(k)
            enddo
         
     
            if ( nguess>0 .and. ((icw4crtm /= 0) .or. (iqtotal /= 0)) ) then
     
               if (ier_ql==0) then
                  ! ql -- regional contribution
                  do k=1,nsig_save
                     yspliu_r(k)=ges_ql_r(i,j,k,it)
                  enddo
                  call intp_spl(xspli_r,yspliu_r,xsplo,ysplou_r,nsig_save,nsig)
                  ! following is to correct for bug in intp_spl
                  do k=1,nsig
                     if (xsplo(k) < xspli_r(nsig_save)) ysplou_r(k)=yspliu_r(nsig_save)
                     if (xsplo(k) > xspli_r(1)) ysplou_r(k)=yspliu_r(1)
                  enddo
                  ! ql -- global contribution
                  do k=1,nsigg
                     kt=k+2*grd_gfs%nsig
                     kq=k+3*grd_gfs%nsig
                     kcw=k+5*grd_gfs%nsig
                     worktmp = -r0_05*(work_sub(1,i,j,kt)/(one+fv*work_sub(1,i,j,kq))-t0c) 
                     worktmp = max(zero,worktmp)
                     worktmp = min(one,worktmp)
                     yspliu_g(k)=work_sub(1,i,j,kcw)*(one-worktmp)
                  enddo
                  call intp_spl(xspli_g,yspliu_g,xsplo,ysplou_g,nsigg,nsig)
                  ! following is to correct for bug in intp_spl
                  do k=1,nsig
                     if (xsplo(k) < xspli_g(nsigg)) ysplou_g(k)=yspliu_g(nsigg)
                     if (xsplo(k) > xspli_g(1)) ysplou_g(k)=yspliu_g(1)
                  enddo
                  ! blend contributions from regional and global:
                  do k=1,nsig
                     qlt(i,j,k)=blend_rm(k)*ysplou_r(k)+blend_gm(k)*ysplou_g(k)
                  enddo 
               endif
     
               if (ier_qi==0) then
                  ! qi -- regional contribution
                  do k=1,nsig_save
                     yspliu_r(k)=ges_qi_r(i,j,k,it)
                  enddo
                  call intp_spl(xspli_r,yspliu_r,xsplo,ysplou_r,nsig_save,nsig)
                  ! following is to correct for bug in intp_spl
                  do k=1,nsig
                     if (xsplo(k) < xspli_r(nsig_save)) ysplou_r(k)=yspliu_r(nsig_save)
                     if (xsplo(k) > xspli_r(1)) ysplou_r(k)=yspliu_r(1)
                  enddo
                  ! qi -- global contribution
                  do k=1,nsigg
                     kt=k+2*grd_gfs%nsig
                     kq=k+3*grd_gfs%nsig
                     kcw=k+5*grd_gfs%nsig
                     worktmp = -r0_05*(work_sub(1,i,j,kt)/(one+fv*work_sub(1,i,j,kq))-t0c)
                     worktmp = max(zero,worktmp)
                     worktmp = min(one,worktmp)
                     yspliu_g(k)=work_sub(1,i,j,kcw)*worktmp
                  enddo
                  call intp_spl(xspli_g,yspliu_g,xsplo,ysplou_g,nsigg,nsig)
                  ! following is to correct for bug in intp_spl
                  do k=1,nsig
                     if (xsplo(k) < xspli_g(nsigg)) ysplou_g(k)=yspliu_g(nsigg)
                     if (xsplo(k) > xspli_g(1)) ysplou_g(k)=yspliu_g(1)
                  enddo
                  ! blend contributions from regional and global:
                  do k=1,nsig
                     qit(i,j,k)=blend_rm(k)*ysplou_r(k)+blend_gm(k)*ysplou_g(k)
                  enddo
               endif
     
               if (ier_qr==0) then
                  ! qr  -- regional contribution
                  do k=1,nsig_save
                     yspliu_r(k)=ges_qr_r(i,j,k,it)
                  enddo
                  call intp_spl(xspli_r,yspliu_r,xsplo,ysplou_r,nsig_save,nsig)
                  ! following is to correct for bug in intp_spl
                  do k=1,nsig
                     if (xsplo(k) < xspli_r(nsig_save)) ysplou_r(k)=yspliu_r(nsig_save)
                     if (xsplo(k) > xspli_r(1)) ysplou_r(k)=yspliu_r(1)
                  enddo
                  ! blend contributions from regional and global:
                  do k=1,nsig
                     qrt(i,j,k)=ysplou_r(k)
                  enddo
               endif
     
               if (ier_qs==0) then
                  ! qs  -- regional contribution
                  do k=1,nsig_save
                     yspliu_r(k)=ges_qs_r(i,j,k,it)
                  enddo
                  call intp_spl(xspli_r,yspliu_r,xsplo,ysplou_r,nsig_save,nsig)
                  ! following is to correct for bug in intp_spl
                  do k=1,nsig
                     if (xsplo(k) < xspli_r(nsig_save)) ysplou_r(k)=yspliu_r(nsig_save)
                     if (xsplo(k) > xspli_r(1)) ysplou_r(k)=yspliu_r(1)
                  enddo
                  ! blend contributions from regional and global:
                  do k=1,nsig
                     qst(i,j,k)=ysplou_r(k)
                  enddo
               endif
     
               if (ier_qg==0) then
                  ! qg  -- regional contribution
                  do k=1,nsig_save
                     yspliu_r(k)=ges_qg_r(i,j,k,it)
                  enddo
                  call intp_spl(xspli_r,yspliu_r,xsplo,ysplou_r,nsig_save,nsig)
                  ! following is to correct for bug in intp_spl
                  do k=1,nsig
                     if (xsplo(k) < xspli_r(nsig_save)) ysplou_r(k)=yspliu_r(nsig_save)
                     if (xsplo(k) > xspli_r(1)) ysplou_r(k)=yspliu_r(1)
                  enddo
                  ! blend contributions from regional and global:
                  do k=1,nsig
                     qgt(i,j,k)=ysplou_r(k)
                  enddo
               endif
     
               if (ier_qh==0) then
                  ! qh  -- regional contribution
                  do k=1,nsig_save
                     yspliu_r(k)=ges_qh_r(i,j,k,it)
                  enddo
                  call intp_spl(xspli_r,yspliu_r,xsplo,ysplou_r,nsig_save,nsig)
                  ! following is to correct for bug in intp_spl
                  do k=1,nsig
                     if (xsplo(k) < xspli_r(nsig_save)) ysplou_r(k)=yspliu_r(nsig_save)
                     if (xsplo(k) > xspli_r(1)) ysplou_r(k)=yspliu_r(1)
                  enddo
                  ! blend contributions from regional and global:
                  do k=1,nsig
                     qht(i,j,k)=ysplou_r(k)
                  enddo
               endif
     
            endif ! end nguess>0 loop
     
         enddo ! do i=1,lat2
      enddo ! do j=1,lon2
     
      deallocate(blend_rm_oz,blend_gm_oz)

      !call grads1a(ut,nsig,mype,'u')
      !call grads1a(vt,nsig,mype,'v')
      !call grads1a(tt,nsig,mype,'t')
      !call grads1a(ttsen,nsig,mype,'tsen')
      !call grads1a(qt,nsig,mype,'q')
      !call grads1a(ozt,nsig,mype,'oz')

      do k=1,nsig
         do j=1,lon2
            do i=1,lat2
               ges_tv(i,j,k)=tt(i,j,k)      
               ges_tsen(i,j,k,it)=ttsen(i,j,k)
               ges_u(i,j,k)=ut(i,j,k)       
               ges_v(i,j,k)=vt(i,j,k)       
               ges_q(i,j,k)=qt(i,j,k)       
               ges_oz(i,j,k)=ozt(i,j,k)    
            enddo
         enddo
      enddo
      ! save blended fields for use at end of analysis
      do k=1,nsig
         do j=1,lon2
            do i=1,lat2
               ges_tv_r_g(i,j,k,it)=ges_tv(i,j,k)     
               ges_tsen_r_g(i,j,k,it)=ges_tsen(i,j,k,it) 
               ges_u_r_g(i,j,k,it)=ges_u(i,j,k)       
               ges_v_r_g(i,j,k,it)=ges_v(i,j,k)       
               ges_q_r_g(i,j,k,it)=ges_q(i,j,k)      
               ges_oz_r_g(i,j,k,it)=ges_oz(i,j,k)    
            enddo
         enddo
      enddo
     
      call general_sub2grid_destroy_info(grd_gfs)
      call general_sub2grid_destroy_info(grd_gfst)
      call general_sub2grid_destroy_info(grd_mix)
      deallocate(ut,vt,tt,ttsen,qt,ozt)
      if ( nguess>0 .and. ((icw4crtm /= 0) .or. (iqtotal /= 0)) ) then
         call gsi_bundlegetpointer (gsi_metguess_bundle(it),'ql',ges_ql,iret); ier_ql=iret
         call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qi',ges_qi,iret); ier_qi=iret
         call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qr',ges_qr,iret); ier_qr=iret
         call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qs',ges_qs,iret); ier_qs=iret
         call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qg',ges_qg,iret); ier_qg=iret
         call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qh',ges_qh,iret); ier_qh=iret
         do k=1,nsig
            do j=1,lon2
               do i=1,lat2
                  if (ier_ql==0) then 
                     ges_ql(i,j,k)=qlt(i,j,k)
                     ges_ql_r_g(i,j,k,it)=ges_ql(i,j,k)
                  endif
                  if (ier_ql==0) then
                     ges_qi(i,j,k)=qit(i,j,k)
                     ges_qi_r_g(i,j,k,it)=ges_qi(i,j,k)
                  endif
                  if (ier_ql==0) then 
                     ges_qr(i,j,k)=qrt(i,j,k)
                     ges_qr_r_g(i,j,k,it)=ges_qr(i,j,k)
                  endif
                  if (ier_ql==0) then 
                     ges_qs(i,j,k)=qst(i,j,k)
                     ges_qs_r_g(i,j,k,it)=ges_qs(i,j,k)
                  endif
                  if (ier_ql==0) then 
                     ges_qg(i,j,k)=qgt(i,j,k)
                     ges_qg_r_g(i,j,k,it)=ges_qg(i,j,k)
                  endif
                  if (ier_ql==0) then 
                     ges_qh(i,j,k)=qht(i,j,k)
                     ges_qh_r_g(i,j,k,it)=ges_qh(i,j,k)
                  endif
               
               enddo
            enddo
         enddo
     
         call gsi_bundlegetpointer (gsi_metguess_bundle(it),'cw',ges_cwmr,iret); ier_cw=iret
         if (ier_cw==0 .and. ier_ql==0 .and. ier_qi==0) then
            do k=1,nsig
               do j=1,lon2
                  do i=1,lat2
                     ges_cwmr(i,j,k)=ges_ql(i,j,k)+ges_qi(i,j,k)
                     ges_cw_r_g(i,j,k,it)=ges_cwmr(i,j,k)
                  enddo
               enddo
            enddo
         endif
      endif
     
      call general_destroy_spec_vars(sp_gfs)
      if ( hires .and. .not. use_gfs_nemsio .and. .not. use_gfs_ncio) call general_destroy_spec_vars(sp_b)
      deallocate(xspli_r,yspliu_r,yspliv_r,xsplo)
      deallocate(ysplou_r,ysplov_r,ysplou_g,ysplov_g)
      deallocate(xspli_g,yspliu_g,yspliv_g)
      deallocate(prsl_m,prsl_r,prsl_g,work_sub)
      !deallocate(pri_m)  
      deallocate(vector)
      if ( nguess>0 .and. ((icw4crtm /= 0) .or. (iqtotal /= 0)) ) &
         deallocate(qlt,qit,qrt,qst,qgt,qht)
     
   enddo it_loop 
   deallocate(infiles)

   return

end subroutine add_gfs_stratosphere

subroutine revert_to_nmmb
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    revert_to_nmmb
!   prgmmr: parrish          org: np22                date: 2012-02-18
!
! abstract: Convert variables from mixed nmmb-gfs variables back to original nmmb in vertical.
!             To do this, subtract mixed nmmb-gfs guess to get analysis increment in mixed nmmb-gfs
!             vertical coordinate.  Then interpolate to original nmmb coordinate.  Finally add original
!             nmmb coordinate guess on to end up with nmmb analysis.
!
! program history log:
!   2012-09-06  parrish, initial documentation
!   2013-02-08  zhu  - add cloud hydrometeros
!   2013-10-19  todling - metguess now holds background
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ enddocumentation block

   use gridmod, only: aeta1_ll,aeta2_ll,pdtop_ll,pt_ll
   use gridmod, only: lat2,lon2,nsig
   use constants,only: zero,one_tenth,one,ten
   use mpimod, only: mpi_comm_world
   use guess_grids, only: ntguessig
   use guess_grids, only: ges_tsen
   use aniso_ens_util, only: intp_spl
!   use gfs_stratosphere, only: nsigg,nsig_save,ak5,bk5,aeta1_save,aeta2_save,eta1_save,eta2_save
!   use gfs_stratosphere, only: blend_rm,blend_gm
!   use gfs_stratosphere, only: ges_tv_r,ges_q_r,ges_u_r,ges_v_r,ges_tsen_r,ges_oz_r
!   use gfs_stratosphere, only: ges_cw_r,ges_ql_r,ges_qi_r,ges_qr_r,ges_qs_r,ges_qg_r,ges_qh_r
!   use gfs_stratosphere, only: ges_tv_r_g,ges_q_r_g,ges_u_r_g,ges_v_r_g,ges_tsen_r_g,ges_oz_r_g
!   use gfs_stratosphere, only: ges_cw_r_g,ges_ql_r_g,ges_qi_r_g,ges_qr_r_g,ges_qs_r_g,ges_qg_r_g,ges_qh_r_g
   use gsi_metguess_mod, only: gsi_metguess_get,gsi_metguess_bundle
   use gsi_bundlemod, only: gsi_bundlegetpointer
   use control_vectors, only: cvars3d

   implicit none
  
   character(len=*),parameter::myname='revert_to_nmmb'
   integer(i_kind) i,j,k,num_i,num_o,ier,istatus
   real(r_kind) xspli(nsig),xsplo(nsig_save)
   real(r_kind) yspli(nsig),ysplo(nsig_save)
   real(r_kind) prsl_r(lat2,lon2,nsig_save),prsl_m(lat2,lon2,nsig)
  
   ! variables for cloud info
   integer(i_kind) nguess,ier_cw,ier_ql,ier_qi,ier_qr,ier_qs,ier_qg,ier_qh,iret
   integer(i_kind) iqtotal,icw4crtm
   real(r_kind),pointer,dimension(:,:,:):: ges_cwmr =>NULL()
   real(r_kind),pointer,dimension(:,:,:):: ges_ql =>NULL()
   real(r_kind),pointer,dimension(:,:,:):: ges_qi =>NULL()
   real(r_kind),pointer,dimension(:,:,:):: ges_qr =>NULL()
   real(r_kind),pointer,dimension(:,:,:):: ges_qs =>NULL()
   real(r_kind),pointer,dimension(:,:,:):: ges_qg =>NULL()
   real(r_kind),pointer,dimension(:,:,:):: ges_qh =>NULL()
  
   real(r_kind),dimension(:,:  ),pointer:: ges_ps =>NULL()
   real(r_kind),dimension(:,:,:),pointer:: ges_u  =>NULL()
   real(r_kind),dimension(:,:,:),pointer:: ges_v  =>NULL()
   real(r_kind),dimension(:,:,:),pointer:: ges_tv =>NULL()
   real(r_kind),dimension(:,:,:),pointer:: ges_q  =>NULL()
   real(r_kind),dimension(:,:,:),pointer:: ges_oz =>NULL()
  
   ! GET 3D PRESSURE FOR ORIGINAL NMMB COORDINATE:
  
   ier=0
   call gsi_bundlegetpointer(gsi_metguess_bundle(ntguessig),'ps' ,ges_ps,istatus)
   ier=ier+istatus
   call gsi_bundlegetpointer(gsi_metguess_bundle(ntguessig),'u'  ,ges_u ,istatus) 
   ier=ier+istatus
   call gsi_bundlegetpointer(gsi_metguess_bundle(ntguessig),'v'  ,ges_v ,istatus) 
   ier=ier+istatus
   call gsi_bundlegetpointer(gsi_metguess_bundle(ntguessig),'tv' ,ges_tv,istatus) 
   ier=ier+istatus
   call gsi_bundlegetpointer(gsi_metguess_bundle(ntguessig),'q'  ,ges_q ,istatus) 
   ier=ier+istatus
   call gsi_bundlegetpointer(gsi_metguess_bundle(ntguessig),'oz' ,ges_oz,istatus) 
   ier=ier+istatus
   if (ier/=0) call die(myname,': missing guess vars, aborting ...',ier)
  
   num_i=nsig
   num_o=nsig_save
   do k=1,nsig_save
      do j=1,lon2
         do i=1,lat2
            prsl_r(i,j,k)=one_tenth*(aeta1_save(k)*pdtop_ll + &
                          aeta2_save(k)*(ten*ges_ps(i,j)-pdtop_ll-pt_ll) + &
                          pt_ll)
  
         enddo
      enddo
   enddo
  
   ! REPEAT FOR NMMB-GFS COORDINATE
  
   do k=1,nsig
      do j=1,lon2
         do i=1,lat2
            prsl_m(i,j,k)=one_tenth*(aeta1_ll(k)*pdtop_ll + &
                            aeta2_ll(k)*(ten*ges_ps(i,j)-pdtop_ll-pt_ll) + &
                            pt_ll)
         enddo
      enddo
   enddo
  
   ! In following, for nmmb-gfs, replace saved guess with current analysis.  Then fields in guess_grids
   ! will be updated to nmmb analysis.
    
   ! NOTE:  only need to interpolate from k0m+1 to nsig_save
     
   ! Inquire about cloud guess fields
   call gsi_metguess_get('dim',nguess,istatus)
  
   ! Determine whether or not cloud-condensate is the control variable (ges_cw=ges_ql+ges_qi)
   icw4crtm=getindex(cvars3d,'cw')
  
   ! Determine whether or not total moisture (water vapor+total cloud condensate) is the control variable
   iqtotal=getindex(cvars3d,'qt')
  
   if (nguess>0 .and. ((icw4crtm /= 0) .or. (iqtotal /= 0))) then
      call gsi_bundlegetpointer (gsi_metguess_bundle(ntguessig),'ql',ges_ql,iret); ier_ql=iret
      call gsi_bundlegetpointer (gsi_metguess_bundle(ntguessig),'qi',ges_qi,iret); ier_qi=iret
      call gsi_bundlegetpointer (gsi_metguess_bundle(ntguessig),'qr',ges_qr,iret); ier_qr=iret
      call gsi_bundlegetpointer (gsi_metguess_bundle(ntguessig),'qs',ges_qs,iret); ier_qs=iret
      call gsi_bundlegetpointer (gsi_metguess_bundle(ntguessig),'qg',ges_qg,iret); ier_qg=iret
      call gsi_bundlegetpointer (gsi_metguess_bundle(ntguessig),'qh',ges_qh,iret); ier_qh=iret
      call gsi_bundlegetpointer (gsi_metguess_bundle(ntguessig),'cw',ges_cwmr,iret); ier_cw=iret
   endif
  
   do j=1,lon2
      do i=1,lat2
         do k=1,nsig
            xspli(k)=log(prsl_m(i,j,k)*ten)
         enddo
         do k=1,nsig_save
            xsplo(k)=log(prsl_r(i,j,k)*ten)
         enddo
         ! u:
         do k=1,nsig
            yspli(k)=ges_u(i,j,k)-ges_u_r_g(i,j,k,ntguessig)
            ges_u_r_g(i,j,k,ntguessig)=ges_u(i,j,k) ! keep original for after write analysis
         enddo
         call intp_spl(xspli,yspli,xsplo,ysplo,num_i,num_o)
         ! following is to correct for bug in intp_spl
         do k=1,nsig_save
            if (xsplo(k) < xspli(nsig)) ysplo(k)=yspli(nsig)
            if (xsplo(k) > xspli(1)) ysplo(k)=yspli(1)
         enddo
         do k=1,nsig_save
            ges_u(i,j,k)=ysplo(k)+ges_u_r(i,j,k,ntguessig)
         enddo
         ! v:
         do k=1,nsig
            yspli(k)=ges_v(i,j,k)-ges_v_r_g(i,j,k,ntguessig)
            ges_v_r_g(i,j,k,ntguessig)=ges_v(i,j,k) ! keep original for after write analysis
         enddo
         call intp_spl(xspli,yspli,xsplo,ysplo,num_i,num_o)
         ! following is to correct for bug in intp_spl
         do k=1,nsig_save
            if (xsplo(k) < xspli(nsig)) ysplo(k)=yspli(nsig)
            if (xsplo(k) > xspli(1)   ) ysplo(k)=yspli(1)
         enddo
         do k=1,nsig_save
            ges_v(i,j,k)=ysplo(k)+ges_v_r(i,j,k,ntguessig)
         enddo
         ! q:
         do k=1,nsig
            yspli(k)=ges_q(i,j,k)-ges_q_r_g(i,j,k,ntguessig)
            ges_q_r_g(i,j,k,ntguessig)=ges_q(i,j,k) ! keep original for after write analysis
         enddo
         call intp_spl(xspli,yspli,xsplo,ysplo,num_i,num_o)
         ! following is to correct for bug in intp_spl
         do k=1,nsig_save
            if (xsplo(k) < xspli(nsig)) ysplo(k)=yspli(nsig)
            if (xsplo(k) > xspli(1)   ) ysplo(k)=yspli(1)
         enddo
         do k=1,nsig_save
            ges_q(i,j,k)=ysplo(k)+ges_q_r(i,j,k,ntguessig)
         enddo
         ! tsen:
         do k=1,nsig
            yspli(k)=ges_tsen(i,j,k,ntguessig)-ges_tsen_r_g(i,j,k,ntguessig)
            ges_tsen_r_g(i,j,k,ntguessig)=ges_tsen(i,j,k,ntguessig) ! keep original for after write analysis
         enddo
         call intp_spl(xspli,yspli,xsplo,ysplo,num_i,num_o)
         ! following is to correct for bug in intp_spl
         do k=1,nsig_save
            if (xsplo(k) < xspli(nsig)) ysplo(k)=yspli(nsig)
            if (xsplo(k) > xspli(1)   ) ysplo(k)=yspli(1)
         enddo
         do k=1,nsig_save
            ges_tsen(i,j,k,ntguessig)=ysplo(k)+ges_tsen_r(i,j,k,ntguessig)
         enddo
         ! oz:  
         do k=1,nsig
            yspli(k)=ges_oz(i,j,k)-ges_oz_r_g(i,j,k,ntguessig)
            ges_oz_r_g(i,j,k,ntguessig)=ges_oz(i,j,k) ! keep original for after write analysis
         enddo
         call intp_spl(xspli,yspli,xsplo,ysplo,num_i,num_o)
         ! following is to correct for bug in intp_spl
         do k=1,nsig_save
            if (xsplo(k) < xspli(nsig)) ysplo(k)=yspli(nsig)
            if (xsplo(k) > xspli(1)   ) ysplo(k)=yspli(1)
         enddo
         do k=1,nsig_save
            ges_oz(i,j,k)=ysplo(k)+ges_oz_r(i,j,k,ntguessig)
         enddo
  
  
         if ( nguess>0 .and. ((icw4crtm /= 0) .or. (iqtotal /= 0)) ) then
            ! ql:
            if (ier_ql==0) then
               do k=1,nsig
                  yspli(k)=ges_ql(i,j,k)-ges_ql_r_g(i,j,k,ntguessig)
                  ges_ql_r_g(i,j,k,ntguessig)=ges_ql(i,j,k) ! keep original for after write analysis
               enddo
               call intp_spl(xspli,yspli,xsplo,ysplo,num_i,num_o)
               ! following is to correct for bug in intp_spl
               do k=1,nsig_save
                  if (xsplo(k) < xspli(nsig)) ysplo(k)=yspli(nsig)
                  if (xsplo(k) > xspli(1)   ) ysplo(k)=yspli(1)
               enddo
               do k=1,nsig_save
                  ges_ql(i,j,k)=max(ysplo(k)+ges_ql_r(i,j,k,ntguessig), zero)
               enddo
            endif
  
            ! qi:
            if (ier_qi==0) then
               do k=1,nsig
                  yspli(k)=ges_qi(i,j,k)-ges_qi_r_g(i,j,k,ntguessig)
                  ges_qi_r_g(i,j,k,ntguessig)=ges_qi(i,j,k) ! keep original for after write analysis
               enddo
               call intp_spl(xspli,yspli,xsplo,ysplo,num_i,num_o)
               ! following is to correct for bug in intp_spl
               do k=1,nsig_save
                  if (xsplo(k) < xspli(nsig)) ysplo(k)=yspli(nsig)
                  if (xsplo(k) > xspli(1)   ) ysplo(k)=yspli(1)
               enddo
               do k=1,nsig_save
                  ges_qi(i,j,k)=max(ysplo(k)+ges_qi_r(i,j,k,ntguessig), zero)
               enddo
            endif
  
            ! qr:
            if (ier_qr==0) then
               do k=1,nsig
                  yspli(k)=ges_qr(i,j,k)-ges_qr_r_g(i,j,k,ntguessig)
                  ges_qr_r_g(i,j,k,ntguessig)=ges_qr(i,j,k) ! keep original for after write analysis
               enddo
               call intp_spl(xspli,yspli,xsplo,ysplo,num_i,num_o)
               ! following is to correct for bug in intp_spl
               do k=1,nsig_save
                  if (xsplo(k) < xspli(nsig)) ysplo(k)=yspli(nsig)
                  if (xsplo(k) > xspli(1)   ) ysplo(k)=yspli(1)
               enddo
               do k=1,nsig_save
                  ges_qr(i,j,k)=max(ysplo(k)+ges_qr_r(i,j,k,ntguessig), zero)
               enddo
            endif
  
            ! qs:
            if (ier_qs==0) then
               do k=1,nsig
                  yspli(k)=ges_qs(i,j,k)-ges_qs_r_g(i,j,k,ntguessig)
                  ges_qs_r_g(i,j,k,ntguessig)=ges_qs(i,j,k) ! keep original for after write analysis
               enddo
               call intp_spl(xspli,yspli,xsplo,ysplo,num_i,num_o)
               ! following is to correct for bug in intp_spl
               do k=1,nsig_save
                  if (xsplo(k) < xspli(nsig)) ysplo(k)=yspli(nsig)
                  if (xsplo(k) > xspli(1)   ) ysplo(k)=yspli(1)
               enddo
               do k=1,nsig_save
                  ges_qs(i,j,k)=max(ysplo(k)+ges_qs_r(i,j,k,ntguessig), zero)
               enddo
            endif
  
            ! qg:
            if (ier_qg==0) then
               do k=1,nsig
                  yspli(k)=ges_qg(i,j,k)-ges_qg_r_g(i,j,k,ntguessig)
                  ges_qg_r_g(i,j,k,ntguessig)=ges_qg(i,j,k) ! keep original for after write analysis
               enddo
               call intp_spl(xspli,yspli,xsplo,ysplo,num_i,num_o)
               ! following is to correct for bug in intp_spl
               do k=1,nsig_save
                  if (xsplo(k) < xspli(nsig)) ysplo(k)=yspli(nsig)
                  if (xsplo(k) > xspli(1)   ) ysplo(k)=yspli(1)
               enddo
               do k=1,nsig_save
                  ges_qg(i,j,k)=max(ysplo(k)+ges_qg_r(i,j,k,ntguessig), zero)
               enddo
            endif
  
            ! qh:
            if (ier_qh==0) then
               do k=1,nsig
                  yspli(k)=ges_qh(i,j,k)-ges_qh_r_g(i,j,k,ntguessig)
                  ges_qh_r_g(i,j,k,ntguessig)=ges_qh(i,j,k) ! keep original for after write analysis
               enddo
               call intp_spl(xspli,yspli,xsplo,ysplo,num_i,num_o)
               ! following is to correct for bug in intp_spl
               do k=1,nsig_save
                  if (xsplo(k) < xspli(nsig)) ysplo(k)=yspli(nsig)
                  if (xsplo(k) > xspli(1)   ) ysplo(k)=yspli(1)
               enddo
               do k=1,nsig_save
                  ges_qh(i,j,k)=max(ysplo(k)+ges_qh_r(i,j,k,ntguessig), zero)
               enddo
            endif
  
            if (ier_cw==0 .and. ier_ql==0 .and. ier_qi==0) then 
               do k=1,nsig_save
                  ges_cwmr(i,j,k)=ges_ql(i,j,k)+ges_qi(i,j,k)
               enddo
            endif
         endif ! if (nguess>0 .and. ((icw4crtm /= 0) .or. (iqtotal /= 0)))
  
      enddo
   enddo

   return
  
end subroutine revert_to_nmmb

subroutine restore_nmmb_gfs
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    restore_nmmb_gfs
!   prgmmr: parrish          org: np22                date: 2012-02-18
!
! abstract: recreate mixed nmmb-gfs variables for use in getting final fit of data to analysis.
!
! program history log:
!   2012-09-06  parrish, initial documentation
!   2013-02-09  zhu - add cloud hydrometeros
!   2013-10-19  todling - metguess now holds background
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ enddocumentation block

   use gridmod, only: lat2,lon2,nsig
   use mpimod, only: mpi_comm_world
   use guess_grids, only: ntguessig
   use guess_grids, only: ges_tsen
!   use gfs_stratosphere, only: ges_tv_r_g,ges_q_r_g,ges_u_r_g,ges_v_r_g,ges_tsen_r_g,ges_oz_r_g
!   use gfs_stratosphere, only: ges_cw_r_g,ges_ql_r_g,ges_qi_r_g,ges_qr_r_g,ges_qs_r_g,ges_qg_r_g,ges_qh_r_g
   use gsi_metguess_mod, only: gsi_metguess_get,gsi_metguess_bundle
   use gsi_bundlemod, only: gsi_bundlegetpointer
   use control_vectors, only: cvars3d

   implicit none

   character(len=*),parameter::myname='restore_nmmb_gfs'
   integer(i_kind) i,j,k,ier,istatus
   real(r_kind),dimension(:,:,:),pointer:: ges_u  =>NULL()
   real(r_kind),dimension(:,:,:),pointer:: ges_v  =>NULL()
   real(r_kind),dimension(:,:,:),pointer:: ges_q  =>NULL()
   real(r_kind),dimension(:,:,:),pointer:: ges_oz =>NULL()

   ! variables for cloud info
   integer(i_kind) nguess,ier_cw,ier_ql,ier_qi,ier_qr,ier_qs,ier_qg,ier_qh,iret
   integer(i_kind) iqtotal,icw4crtm
   real(r_kind),pointer,dimension(:,:,:):: ges_cwmr =>NULL()
   real(r_kind),pointer,dimension(:,:,:):: ges_ql =>NULL()
   real(r_kind),pointer,dimension(:,:,:):: ges_qi =>NULL()
   real(r_kind),pointer,dimension(:,:,:):: ges_qr =>NULL()
   real(r_kind),pointer,dimension(:,:,:):: ges_qs =>NULL()
   real(r_kind),pointer,dimension(:,:,:):: ges_qg =>NULL()
   real(r_kind),pointer,dimension(:,:,:):: ges_qh =>NULL()

   ier=0
   call gsi_bundlegetpointer(gsi_metguess_bundle(ntguessig),'u'  ,ges_u ,istatus) 
   ier=ier+istatus
   call gsi_bundlegetpointer(gsi_metguess_bundle(ntguessig),'v'  ,ges_v ,istatus) 
   ier=ier+istatus
   call gsi_bundlegetpointer(gsi_metguess_bundle(ntguessig),'q'  ,ges_q ,istatus) 
   ier=ier+istatus
   call gsi_bundlegetpointer(gsi_metguess_bundle(ntguessig),'oz' ,ges_oz,istatus) 
   ier=ier+istatus
   if (ier/=0) call die(myname,': missing guess vars, aborting ...',ier)

   ! restore nmmb-gfs analysis variable
   do k=1,nsig
      do j=1,lon2
         do i=1,lat2
            ges_u(i,j,k)=ges_u_r_g(i,j,k,ntguessig)
            ges_v(i,j,k)=ges_v_r_g(i,j,k,ntguessig)
            ges_q(i,j,k)=ges_q_r_g(i,j,k,ntguessig)
            ges_tsen(i,j,k,ntguessig)=ges_tsen_r_g(i,j,k,ntguessig)
            ges_oz(i,j,k)=ges_oz_r_g(i,j,k,ntguessig)
         enddo
      enddo
   enddo

   ! Inquire about cloud guess fields
   call gsi_metguess_get('dim',nguess,istatus)

   ! Determine whether or not cloud-condensate is the control variable (ges_cw=ges_ql+ges_qi)
   icw4crtm=getindex(cvars3d,'cw')

   ! Determine whether or not total moisture (water vapor+total cloud condensate) is the control variable
   iqtotal=getindex(cvars3d,'qt')

   if ( nguess>0 .and. ((icw4crtm /= 0) .or. (iqtotal /= 0)) ) then
      call gsi_bundlegetpointer (gsi_metguess_bundle(ntguessig),'ql',ges_ql,iret); ier_ql=iret
      call gsi_bundlegetpointer (gsi_metguess_bundle(ntguessig),'qi',ges_qi,iret); ier_qi=iret
      call gsi_bundlegetpointer (gsi_metguess_bundle(ntguessig),'qr',ges_qr,iret); ier_qr=iret
      call gsi_bundlegetpointer (gsi_metguess_bundle(ntguessig),'qs',ges_qs,iret); ier_qs=iret
      call gsi_bundlegetpointer (gsi_metguess_bundle(ntguessig),'qg',ges_qg,iret); ier_qg=iret
      call gsi_bundlegetpointer (gsi_metguess_bundle(ntguessig),'qh',ges_qh,iret); ier_qh=iret
      call gsi_bundlegetpointer (gsi_metguess_bundle(ntguessig),'cw',ges_cwmr,iret); ier_cw=iret
      do k=1,nsig
         do j=1,lon2
            do i=1,lat2
               if (ier_ql==0) ges_ql(i,j,k)=ges_ql_r_g(i,j,k,ntguessig)
               if (ier_qi==0) ges_qi(i,j,k)=ges_qi_r_g(i,j,k,ntguessig)
               if (ier_qr==0) ges_qr(i,j,k)=ges_qr_r_g(i,j,k,ntguessig)
               if (ier_qs==0) ges_qs(i,j,k)=ges_qs_r_g(i,j,k,ntguessig)
               if (ier_qg==0) ges_qg(i,j,k)=ges_qg_r_g(i,j,k,ntguessig)
               if (ier_qh==0) ges_qh(i,j,k)=ges_qh_r_g(i,j,k,ntguessig)
               if (ier_cw==0 .and. ier_ql==0 .and. ier_qi==0) ges_cwmr(i,j,k)=ges_ql(i,j,k)+ges_qi(i,j,k)
            enddo
         enddo
      enddo
   endif

   return

end subroutine restore_nmmb_gfs

end module gfs_stratosphere

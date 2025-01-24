subroutine general_read_fv3atm_nems(grd,sp_a,filename,uvflag,vordivflag,zflag, &
           gfs_bundle,init_head,iret_read)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_read_fv3atm  adaptation of read_fv3atm for general resolutions
!   prgmmr: parrish          org: np22                date: 1990-10-10
!
! abstract: copied from read_gfsatm, primarily for reading in gefs sigma files, where the
!            input resolution and the grid that variables are reconstructed on can be
!            different from the analysis grid/resolution.
!
! program history log:
!   2018-04-15  eliu   - copied from general_read_gfsatm.f90 to handle multiple fv3 physic
!                            schemes except for Zhao-Carr scheme (imp_physics=99)
!                            ** imp_physics: 11=GFDL 10=MG 8=Thompson 6=WSM6
!                            ** set fv3_full_hydro=.true. and imp_physics in the setup namelist
!                            (currently working for 11-GFDL; will be generalized for other schemes) 
!
!   input argument list:
!     grd      - structure variable containing information about grid
!                    (initialized by general_sub2grid_create_info, located in general_sub2grid_mod.f90)
!     sp_a     - structure variable containing spectral information for analysis
!                    (initialized by general_init_spec_vars, located in general_specmod.f90)
!     sp_b     - structure variable containing spectral information for input
!                     fields
!                    (initialized by general_init_spec_vars, located in general_specmod.f90)
!     filename - input sigma file name
!     uvflag   - logical to use u,v (.true.) or st,vp (.false.) perturbations
!     vordivflag - logical to determine if routine should output vorticity and
!                  divergence
!     zflag    - logical to determine if surface height field should be output
!     init_head- flag to read header record.  Usually .true. unless repeatedly
!                reading similar files (ensembles)
!
!   output argument list:
!     gfs_bundle  - bundle carrying guess fields
!     iret_read - return code, 0 for successful read.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
   use kinds, only: r_kind,r_single,i_kind
   use mpimod, only: mype
   use general_sub2grid_mod, only: sub2grid_info
   use general_specmod, only: spec_vars
   use mpimod, only: npe
   use constants, only: zero,one,fv,r0_01
   use nemsio_module, only: nemsio_init,nemsio_open,nemsio_close
   use ncepnems_io, only: error_msg
   use nemsio_module, only: nemsio_gfile,nemsio_getfilehead,nemsio_readrecv
   use egrid2agrid_mod,only: g_egrid2agrid,g_create_egrid2agrid,egrid2agrid_parm,destroy_egrid2agrid
   use general_commvars_mod, only: fill2_ns,filluv2_ns
   use constants, only: two,pi,half,deg2rad,r60,r3600
   use gsi_bundlemod, only: gsi_bundle
   use gsi_bundlemod, only: gsi_bundlegetpointer
   use gsi_metguess_mod, only: gsi_metguess_get   
   use ncepnems_io, only: imp_physics


   implicit none

   ! Declare local parameters
   real(r_kind),parameter:: r0_001 = 0.001_r_kind

   ! Declare passed variables
   type(sub2grid_info)                   ,intent(in   ) :: grd
   type(spec_vars)                       ,intent(in   ) :: sp_a
   character(*)                          ,intent(in   ) :: filename
   logical                               ,intent(in   ) :: uvflag,zflag,vordivflag,init_head
   integer(i_kind)                       ,intent(  out) :: iret_read
   type(gsi_bundle)                      ,intent(inout) :: gfs_bundle

   real(r_kind),pointer,dimension(:,:)       :: ptr2d
   real(r_kind),pointer,dimension(:,:,:)     :: ptr3d
   real(r_kind),pointer,dimension(:,:)       :: g_ps
   real(r_kind),pointer,dimension(:,:,:)     :: g_vor,g_div,&
                                                g_q,g_oz,g_tv
   real(r_kind),pointer,dimension(:,:,:)     :: g_ql,g_qi,g_qr,g_qs,g_qg,g_cf
   real(r_kind),allocatable,dimension(:,:)   :: g_z
   real(r_kind),allocatable,dimension(:,:,:) :: g_u,g_v

   ! Declare local variables
   character(len=120) :: my_name = 'GENERAL_READ_FV3ATM_NEMS'
   character(len=1)   :: null = ' '
   integer(i_kind):: iret,nlatm2,nlevs,icm,nord_int
   integer(i_kind):: i,j,k,icount,kk
   integer(i_kind) :: ier,istatus,istatus1,iredundant
   integer(i_kind) :: latb, lonb, levs, nframe
   integer(i_kind) :: nfhour, nfminute, nfsecondn, nfsecondd
   integer(i_kind) :: istop = 101
   integer(i_kind),dimension(npe)::ilev,iflag,mype_use
   integer(i_kind),dimension(7):: idate
   integer(i_kind),dimension(4):: odate
   real(r_kind) :: fhour

   real(r_kind),allocatable,dimension(:):: spec_div,spec_vor
   real(r_kind),allocatable,dimension(:,:) :: grid, grid_v, &
        grid_vor, grid_div, grid_b, grid_b2
   real(r_kind),allocatable,dimension(:,:,:) :: grid_c, grid2, grid_c2
   real(r_kind),allocatable,dimension(:)   :: work, work_v
   real(r_kind),allocatable,dimension(:) :: rwork1d0, rwork1d1
   real(r_kind),allocatable,dimension(:) :: rlats,rlons,clons,slons
   real(4),allocatable,dimension(:) :: r4lats,r4lons

   logical :: procuse,diff_res,eqspace
   type(nemsio_gfile) :: gfile
   type(egrid2agrid_parm) :: p_high
   logical,dimension(1) :: vector

   !******************************************************************************
   ! Initialize variables used below
   iret_read=0
   iret=0
   nlatm2=grd%nlat-2
   iflag = 0
   ilev = 0

   nlevs=grd%nsig
   mype_use=-1
   icount=0
   procuse=.false.
   if ( mype == 0 ) procuse = .true.
   do i=1,npe
      if ( grd%recvcounts_s(i-1) > 0 ) then
         icount = icount+1
         mype_use(icount)=i-1
         if ( i-1 == mype ) procuse=.true.
      endif
   enddo
   icm=icount
   allocate( work(grd%itotsub),work_v(grd%itotsub) )
   work=zero
   work_v=zero

   if ( procuse ) then

      if ( init_head)call nemsio_init(iret=iret)
      if (iret /= 0) call error_msg(trim(my_name),trim(filename),null,'init',istop,iret)

      call nemsio_open(gfile,filename,'READ',iret=iret)
      if (iret /= 0) call error_msg(trim(my_name),trim(filename),null,'open',istop+1,iret)

      call nemsio_getfilehead(gfile,iret=iret, nframe=nframe, &
           nfhour=nfhour, nfminute=nfminute, nfsecondn=nfsecondn, nfsecondd=nfsecondd, &
           idate=idate, dimx=lonb, dimy=latb,dimz=levs)

      if (  nframe /= 0 ) then
         if ( mype == 0 ) &
            write(6,*)trim(my_name),': ***ERROR***  nframe /= 0 for global model read, nframe = ', nframe
         call stop2(101)
      endif

      fhour = real(nfhour,r_kind) + real(nfminute,r_kind)/r60 + &
              real(nfsecondn,r_kind)/real(nfsecondd,r_kind)/r3600
      odate(1) = idate(4)  !hour
      odate(2) = idate(2)  !month
      odate(3) = idate(3)  !day
      odate(4) = idate(1)  !year

      diff_res=.false.
      if ( latb /= nlatm2 ) then
         diff_res=.true.
         if ( mype == 0 ) write(6, &
            '(a,'': different spatial dimension nlatm2 = '',i4,tr1,''latb = '',i4)') &
            trim(my_name),nlatm2,latb
         !call stop2(101)
      endif
      if ( lonb /= grd%nlon ) then
         diff_res=.true.
         if ( mype == 0 ) write(6, &
            '(a,'': different spatial dimension nlon   = '',i4,tr1,''lonb = '',i4)') &
            trim(my_name),grd%nlon,lonb
         !call stop2(101)
      endif
      if ( levs /= grd%nsig ) then
         if ( mype == 0 ) write(6, &
            '(a,'': inconsistent spatial dimension nsig   = '',i4,tr1,''levs = '',i4)') &
            trim(my_name),grd%nsig,levs
         call stop2(101)
      endif

      allocate( spec_vor(sp_a%nc), spec_div(sp_a%nc) )
      allocate( grid(grd%nlon,nlatm2), grid_v(grd%nlon,nlatm2) )
      if ( diff_res ) then
         allocate(grid_b(lonb,latb),grid_c(latb+2,lonb,1),grid2(grd%nlat,grd%nlon,1))
         allocate(grid_b2(lonb,latb),grid_c2(latb+2,lonb,1))
      endif
      allocate(rwork1d0(latb*lonb))
      allocate(rlats(latb+2),rlons(lonb),clons(lonb),slons(lonb),r4lats(lonb*latb),r4lons(lonb*latb))
      allocate(rwork1d1(latb*lonb))
      call nemsio_getfilehead(gfile,lat=r4lats,iret=iret)
      call nemsio_getfilehead(gfile,lon=r4lons,iret=iret)
      do j=1,latb
         rlats(latb+2-j)=deg2rad*r4lats(lonb/2+(j-1)*lonb)
      enddo
      do j=1,lonb
         rlons(j)=deg2rad*r4lons(j)
      enddo
      deallocate(r4lats,r4lons)
      rlats(1)=-half*pi
      rlats(latb+2)=half*pi
      do j=1,lonb
         clons(j)=cos(rlons(j))
         slons(j)=sin(rlons(j))
      enddo

      nord_int=4
      eqspace=.false.
      call g_create_egrid2agrid(grd%nlat,sp_a%rlats,grd%nlon,sp_a%rlons, &
                              latb+2,rlats,lonb,rlons,&
                              nord_int,p_high,.true.,eqspace=eqspace)
      deallocate(rlats,rlons)

   endif ! if ( procuse )

   ! Get pointer to relevant variables (this should be made flexible and general)
   iredundant=0
   call gsi_bundlegetpointer(gfs_bundle,'sf',g_div ,ier)
   if ( ier == 0 ) iredundant = iredundant + 1
   call gsi_bundlegetpointer(gfs_bundle,'div',g_div ,ier)
   if ( ier == 0 ) iredundant = iredundant + 1
   if ( iredundant==2 ) then
      if ( mype == 0 ) then
         write(6,*) 'general_read_fv3atm_nems: ERROR'
         write(6,*) 'cannot handle having both sf and div'
         write(6,*) 'Aborting ... '
      endif
      call stop2(999)
   endif
   iredundant=0
   call gsi_bundlegetpointer(gfs_bundle,'vp',g_vor ,ier)
   if ( ier == 0 ) iredundant = iredundant + 1
   call gsi_bundlegetpointer(gfs_bundle,'vor',g_vor ,ier)
   if ( ier == 0 ) iredundant = iredundant + 1
   if ( iredundant==2 ) then
      if ( mype == 0 ) then
         write(6,*) 'general_read_fv3atm_nems: ERROR'
         write(6,*) 'cannot handle having both vp and vor'
         write(6,*) 'Aborting ... '
      endif
      call stop2(999)
   endif
   iredundant=0
   call gsi_bundlegetpointer(gfs_bundle,'t' ,g_tv  ,ier)
   if ( ier == 0 ) iredundant = iredundant + 1
   call gsi_bundlegetpointer(gfs_bundle,'tv',g_tv  ,ier)
   if ( ier == 0 ) iredundant = iredundant + 1
   if ( iredundant==2 ) then
      if ( mype == 0 ) then
         write(6,*) 'general_read_fv3atm_nems: ERROR'
         write(6,*) 'cannot handle having both t and tv'
         write(6,*) 'Aborting ... '
      endif
      call stop2(999)
   endif
   istatus=0
   istatus1=0
   call gsi_bundlegetpointer(gfs_bundle,'ps',g_ps  ,ier);istatus = istatus + ier
   call gsi_bundlegetpointer(gfs_bundle,'q' ,g_q   ,ier);istatus = istatus + ier
   call gsi_bundlegetpointer(gfs_bundle,'oz',g_oz  ,ier);istatus = istatus + ier
   call gsi_bundlegetpointer(gfs_bundle,'ql',g_ql  ,ier);istatus1= istatus + ier
   call gsi_bundlegetpointer(gfs_bundle,'qi',g_qi  ,ier);istatus1= istatus1+ ier
   call gsi_bundlegetpointer(gfs_bundle,'qr',g_qr  ,ier);istatus1= istatus1+ ier
   call gsi_bundlegetpointer(gfs_bundle,'qs',g_qs  ,ier);istatus1= istatus1+ ier
   call gsi_bundlegetpointer(gfs_bundle,'qg',g_qg  ,ier);istatus1= istatus1+ ier
   call gsi_bundlegetpointer(gfs_bundle,'cf',g_cf  ,ier);istatus1= istatus1+ ier
   if ( istatus /= 0 ) then
      if ( mype == 0 ) then
         write(6,*) 'general_read_fv3atm_nems: ERROR'
         write(6,*) 'Missing some of the required fields'
         write(6,*) 'Aborting ... '
      endif
      call stop2(999)
   endif
   if ( istatus1 /= 0 ) then
      if ( mype == 0 ) then
         write(6,*) 'general_read_fv3atm_nems: ERROR'
         write(6,*) 'Missing some of the required hydrometeor fields for imp_physics = ', imp_physics
         write(6,*) 'Aborting ... '
      endif
      call stop2(999)
   endif

   allocate(g_u(grd%lat2,grd%lon2,grd%nsig),g_v(grd%lat2,grd%lon2,grd%nsig))
   allocate(g_z(grd%lat2,grd%lon2))

   icount=0

   !   Process guess fields according to type of input file.   NCEP_SIGIO files
   !   are spectral coefficient files and need to be transformed to the grid.
   !   Once on the grid, fields need to be scattered from the full domain to
   !   sub-domains.

   !  Only read Terrain when zflag is true.
   if ( zflag ) then

      icount=icount+1
      iflag(icount)=1
      ilev(icount)=1

      ! Terrain:  spectral --> grid transform, scatter to all mpi tasks
      if (mype==mype_use(icount)) then
         ! read hs
         call nemsio_readrecv(gfile,'hgt', 'sfc',1,rwork1d0,iret=iret)
         if (iret /= 0) call error_msg(trim(my_name),trim(filename),'hgt','read',istop+2,iret)
         if ( diff_res ) then
            grid_b=reshape(rwork1d0,(/size(grid_b,1),size(grid_b,2)/))
            vector(1)=.false.
            call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
            call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
            do kk=1,grd%itotsub
               i=grd%ltosi_s(kk)
               j=grd%ltosj_s(kk)
               work(kk)=grid2(i,j,1)
            enddo
         else
            grid=reshape(rwork1d0,(/size(grid,1),size(grid,2)/))
            call general_fill_ns(grd,grid,work)
         endif
      endif
      if ( icount == icm ) then
         call general_reload2(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz, &
              g_ql,g_qi,g_qr,g_qs,g_qg,g_cf,icount,iflag,ilev,work,uvflag,vordivflag)
      endif
   endif

   icount=icount+1
   iflag(icount)=2
   ilev(icount)=1

   ! Surface pressure:  same procedure as terrain
   if (mype==mype_use(icount)) then
      ! read ps
      call nemsio_readrecv(gfile,'pres','sfc',1,rwork1d0,iret=iret)
      if (iret /= 0) call error_msg(trim(my_name),trim(filename),'pres','read',istop+3,iret)
      rwork1d1 = r0_001*rwork1d0 ! convert Pa to cb
      if ( diff_res ) then
         vector(1)=.false.
         grid_b=reshape(rwork1d1,(/size(grid_b,1),size(grid_b,2)/))
         call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
         call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
         do kk=1,grd%itotsub
            i=grd%ltosi_s(kk)
            j=grd%ltosj_s(kk)
            work(kk)=grid2(i,j,1)
         enddo
      else
         grid=reshape(rwork1d1,(/size(grid,1),size(grid,2)/))
         call general_fill_ns(grd,grid,work)
      endif
   endif
   if ( icount == icm ) then
      call general_reload2(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz, &
           g_ql,g_qi,g_qr,g_qs,g_qg,g_cf,icount,iflag,ilev,work,uvflag,vordivflag)
   endif

   !   Thermodynamic variable:  s-->g transform, communicate to all tasks
   !   For multilevel fields, each task handles a given level.  Periodic
   !   mpi_alltoallv calls communicate the grids to all mpi tasks.
   !   Finally, the grids are loaded into guess arrays used later in the
   !   code.

   do k=1,nlevs

      icount=icount+1
      iflag(icount)=3
      ilev(icount)=k

      if (mype==mype_use(icount)) then
         ! read T/Tv/etc.
         call nemsio_readrecv(gfile,'tmp','mid layer',k,rwork1d0,iret=iret)
         if (iret /= 0) call error_msg(trim(my_name),trim(filename),'tmp','read',istop+7,iret)
         call nemsio_readrecv(gfile,'spfh','mid layer',k,rwork1d1,iret=iret)
         if (iret /= 0) call error_msg(trim(my_name),trim(filename),'spfh','read',istop+7,iret)
         rwork1d0=rwork1d0*(one+fv*rwork1d1)
         if ( diff_res ) then
            grid_b=reshape(rwork1d0,(/size(grid_b,1),size(grid_b,2)/))
            vector(1)=.false.
            call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
            call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
            do kk=1,grd%itotsub
               i=grd%ltosi_s(kk)
               j=grd%ltosj_s(kk)
               work(kk)=grid2(i,j,1)
            enddo
         else
            grid=reshape(rwork1d0,(/size(grid,1),size(grid,2)/))
            call general_fill_ns(grd,grid,work)
         endif
      endif
      if ( icount == icm ) then
         call general_reload2(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz, &
              g_ql,g_qi,g_qr,g_qs,g_qg,g_cf,icount,iflag,ilev,work,uvflag,vordivflag)
      endif

      if ( vordivflag .or. .not. uvflag ) then

         icount=icount+1
         iflag(icount)=4
         ilev(icount)=k

         if (mype==mype_use(icount)) then
            ! Vorticity
            ! Convert grid u,v to div and vor
            call nemsio_readrecv(gfile,'ugrd','mid layer',k,rwork1d0,iret=iret)
            if (iret /= 0) call error_msg(trim(my_name),trim(filename),'ugrd','read',istop+4,iret)
            call nemsio_readrecv(gfile,'vgrd','mid layer',k,rwork1d1,iret=iret)
            if (iret /= 0) call error_msg(trim(my_name),trim(filename),'vgrd','read',istop+5,iret)
            if ( diff_res ) then
               grid_b=reshape(rwork1d0,(/size(grid_b,1),size(grid_b,2)/))
               grid_b2=reshape(rwork1d1,(/size(grid_b2,1),size(grid_b2,2)/))
               vector(1)=.true.
               call filluv2_ns(grid_b,grid_b2,grid_c(:,:,1),grid_c2(:,:,1),latb+2,lonb,slons,clons)
               call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
               do kk=1,grd%itotsub
                  i=grd%ltosi_s(kk)
                  j=grd%ltosj_s(kk)
                  work(kk)=grid2(i,j,1)
               enddo
               do j=1,grd%nlon
                  do i=2,grd%nlat-1
                     grid(j,grd%nlat-i)=grid2(i,j,1)
                  enddo
               enddo
               call g_egrid2agrid(p_high,grid_c2,grid2,1,1,vector)
               do kk=1,grd%itotsub
                  i=grd%ltosi_s(kk)
                  j=grd%ltosj_s(kk)
                  work_v(kk)=grid2(i,j,1)
               enddo
               do j=1,grd%nlon
                  do i=2,grd%nlat-1
                     grid_v(j,grd%nlat-i)=grid2(i,j,1)
                  enddo
               enddo
            else
               grid=reshape(rwork1d0,(/size(grid,1),size(grid,2)/))
               grid_v=reshape(rwork1d1,(/size(grid_v,1),size(grid_v,2)/))
               call general_filluv_ns(grd,slons,clons,grid,grid_v,work,work_v)
            endif
            allocate( grid_vor(grd%nlon,nlatm2))
            call general_sptez_v(sp_a,spec_div,spec_vor,grid,grid_v,-1)
            call general_sptez_s_b(sp_a,sp_a,spec_vor,grid_vor,1)
            ! Load values into rows for south and north pole
            call general_fill_ns(grd,grid_vor,work)
            deallocate(grid_vor)
         endif
         if ( icount == icm ) then
            call general_reload2(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz, &
                 g_ql,g_qi,g_qr,g_qs,g_qg,g_cf,icount,iflag,ilev,work,uvflag,vordivflag)
         endif

         icount=icount+1
         iflag(icount)=5
         ilev(icount)=k

         if (mype==mype_use(icount)) then
            ! Divergence
            ! Convert grid u,v to div and vor
            call nemsio_readrecv(gfile,'ugrd','mid layer',k,rwork1d0,iret=iret)
            if (iret /= 0) call error_msg(trim(my_name),trim(filename),'ugrd','read',istop+4,iret)
            call nemsio_readrecv(gfile,'vgrd','mid layer',k,rwork1d1,iret=iret)
            if (iret /= 0) call error_msg(trim(my_name),trim(filename),'vgrd','read',istop+5,iret)
            if ( diff_res ) then
               grid_b=reshape(rwork1d0,(/size(grid_b,1),size(grid_b,2)/))
               grid_b2=reshape(rwork1d1,(/size(grid_b,1),size(grid_b,2)/))
               vector(1)=.true.
               call filluv2_ns(grid_b,grid_b2,grid_c(:,:,1),grid_c2(:,:,1),latb+2,lonb,slons,clons)
               call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
               do kk=1,grd%itotsub
                  i=grd%ltosi_s(kk)
                  j=grd%ltosj_s(kk)
                  work(kk)=grid2(i,j,1)
               enddo
               do j=1,grd%nlon
                  do i=2,grd%nlat-1
                     grid(j,grd%nlat-i)=grid2(i,j,1)
                  enddo
               enddo
               call g_egrid2agrid(p_high,grid_c2,grid2,1,1,vector)
               do kk=1,grd%itotsub
                  i=grd%ltosi_s(kk)
                  j=grd%ltosj_s(kk)
                  work_v(kk)=grid2(i,j,1)
               enddo
               do j=1,grd%nlon
                  do i=2,grd%nlat-1
                     grid_v(j,grd%nlat-i)=grid2(i,j,1)
                  enddo
               enddo
            else
               grid=reshape(rwork1d0,(/size(grid,1),size(grid,2)/))
               grid_v=reshape(rwork1d1,(/size(grid_v,1),size(grid_v,2)/))
               call general_filluv_ns(grd,slons,clons,grid,grid_v,work,work_v)
            endif
            allocate( grid_div(grd%nlon,nlatm2) )
            call general_sptez_v(sp_a,spec_div,spec_vor,grid,grid_v,-1)
            call general_sptez_s_b(sp_a,sp_a,spec_div,grid_div,1)
            ! Load values into rows for south and north pole
            call general_fill_ns(grd,grid_div,work)
            deallocate(grid_div)
         endif
         if ( icount == icm ) then
            call general_reload2(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz, &
                 g_ql,g_qi,g_qr,g_qs,g_qg,g_cf,icount,iflag,ilev,work,uvflag,vordivflag)
         endif

      endif ! if ( vordivflag .or. .not. uvflag )

      if ( uvflag ) then

         icount=icount+1
         iflag(icount)=6
         ilev(icount)=k

         if (mype==mype_use(icount)) then

            ! U
            call nemsio_readrecv(gfile,'ugrd','mid layer',k,rwork1d0,iret=iret)
            if (iret /= 0) call error_msg(trim(my_name),trim(filename),'ugrd','read',istop+4,iret)
            call nemsio_readrecv(gfile,'vgrd','mid layer',k,rwork1d1,iret=iret)
            if (iret /= 0) call error_msg(trim(my_name),trim(filename),'vgrd','read',istop+5,iret)
            if ( diff_res ) then
               grid_b=reshape(rwork1d0,(/size(grid_b,1),size(grid_b,2)/))
               grid_b2=reshape(rwork1d1,(/size(grid_b2,1),size(grid_b2,2)/))
               vector(1)=.true.
               call filluv2_ns(grid_b,grid_b2,grid_c(:,:,1),grid_c2(:,:,1),latb+2,lonb,slons,clons)
               call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
               do kk=1,grd%itotsub
                  i=grd%ltosi_s(kk)
                  j=grd%ltosj_s(kk)
                  work(kk)=grid2(i,j,1)
               enddo
            else
               grid=reshape(rwork1d0,(/size(grid,1),size(grid,2)/))
               grid_v=reshape(rwork1d1,(/size(grid_v,1),size(grid_v,2)/))
               call general_filluv_ns(grd,slons,clons,grid,grid_v,work,work_v)
            endif
         endif
         if ( icount == icm ) then
            call general_reload2(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz, &
                 g_ql,g_qi,g_qr,g_qs,g_qg,g_cf,icount,iflag,ilev,work,uvflag,vordivflag)
         endif

         icount=icount+1
         iflag(icount)=7
         ilev(icount)=k

         if (mype==mype_use(icount)) then
            ! V
            call nemsio_readrecv(gfile,'ugrd','mid layer',k,rwork1d0,iret=iret)
            if (iret /= 0) call error_msg(trim(my_name),trim(filename),'ugrd','read',istop+4,iret)
            call nemsio_readrecv(gfile,'vgrd','mid layer',k,rwork1d1,iret=iret)
            if (iret /= 0) call error_msg(trim(my_name),trim(filename),'vgrd','read',istop+5,iret)
            if ( diff_res ) then
               grid_b=reshape(rwork1d0,(/size(grid_b,1),size(grid_b,2)/))
               grid_b2=reshape(rwork1d1,(/size(grid_b2,1),size(grid_b2,2)/))
               vector(1)=.true.
               call filluv2_ns(grid_b,grid_b2,grid_c(:,:,1),grid_c2(:,:,1),latb+2,lonb,slons,clons)
               call g_egrid2agrid(p_high,grid_c2,grid2,1,1,vector)
               do kk=1,grd%itotsub
                  i=grd%ltosi_s(kk)
                  j=grd%ltosj_s(kk)
                  work(kk)=grid2(i,j,1)
               enddo
            else
               grid=reshape(rwork1d0,(/size(grid,1),size(grid,2)/))
               grid_v=reshape(rwork1d1,(/size(grid_v,1),size(grid_v,2)/))
               ! Note work_v and work are switched because output must be in work.
               call general_filluv_ns(grd,slons,clons,grid,grid_v,work_v,work)
            endif
         endif
         if ( icount == icm ) then
            call general_reload2(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz, &
                 g_ql,g_qi,g_qr,g_qs,g_qg,g_cf,icount,iflag,ilev,work,uvflag,vordivflag)
         endif

      endif ! if ( uvflag )

      icount=icount+1
      iflag(icount)=8
      ilev(icount)=k

      if (mype==mype_use(icount)) then
         ! Specific humidity
         call nemsio_readrecv(gfile,'spfh','mid layer',k,rwork1d0,iret=iret)
         if (iret /= 0) call error_msg(trim(my_name),trim(filename),'spfh','read',istop+6,iret)
         if ( diff_res ) then
            grid_b=reshape(rwork1d0,(/size(grid_b,1),size(grid_b,2)/))
            vector(1)=.false.
            call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
            call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
            do kk=1,grd%itotsub
               i=grd%ltosi_s(kk)
               j=grd%ltosj_s(kk)
               work(kk)=grid2(i,j,1)
            enddo
         else
            grid=reshape(rwork1d0,(/size(grid,1),size(grid,2)/))
            call general_fill_ns(grd,grid,work)
         endif
      endif
      if ( icount == icm ) then
         call general_reload2(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz, &
              g_ql,g_qi,g_qr,g_qs,g_qg,g_cf,icount,iflag,ilev,work,uvflag,vordivflag)
      endif

      icount=icount+1
      iflag(icount)=9
      ilev(icount)=k

      if (mype==mype_use(icount)) then
         ! Ozone mixing ratio
         call nemsio_readrecv(gfile,'o3mr','mid layer',k,rwork1d0,iret=iret)
         if (iret /= 0) call error_msg(trim(my_name),trim(filename),'o3mr','read',istop+8,iret)
         if ( diff_res ) then
            grid_b=reshape(rwork1d0,(/size(grid_b,1),size(grid_b,2)/))
            vector(1)=.false.
            call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
            call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
            do kk=1,grd%itotsub
               i=grd%ltosi_s(kk)
               j=grd%ltosj_s(kk)
               work(kk)=grid2(i,j,1)
            enddo
         else
            grid=reshape(rwork1d0,(/size(grid,1),size(grid,2)/))
            call general_fill_ns(grd,grid,work)
         endif
      endif
      if ( icount == icm ) then
         call general_reload2(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz, &
              g_ql,g_qi,g_qr,g_qs,g_qg,g_cf,icount,iflag,ilev,work,uvflag,vordivflag)
      endif

      icount=icount+1
      iflag(icount)=10
      ilev(icount)=k

      if (mype==mype_use(icount)) then
         ! Cloud liquid water mixing ratio
         call nemsio_readrecv(gfile,'clwmr','mid layer',k,rwork1d0,iret=iret)
         if (iret /= 0) call error_msg(trim(my_name),trim(filename),'clwmr','read',istop+9,iret)
         if ( diff_res ) then
            grid_b=reshape(rwork1d0,(/size(grid_b,1),size(grid_b,2)/))
            vector(1)=.false.
            call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
            call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
            do kk=1,grd%itotsub
               i=grd%ltosi_s(kk)
               j=grd%ltosj_s(kk)
               work(kk)=grid2(i,j,1)
            enddo
         else
            grid=reshape(rwork1d0,(/size(grid,1),size(grid,2)/))
            call general_fill_ns(grd,grid,work)
         endif
      endif
      if ( icount == icm ) then
         call general_reload2(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz, &
              g_ql,g_qi,g_qr,g_qs,g_qg,g_cf,icount,iflag,ilev,work,uvflag,vordivflag)
      endif

      icount=icount+1
      iflag(icount)=11
      ilev(icount)=k

      if (mype==mype_use(icount)) then
         ! Cloud ice water mixing ratio
         call nemsio_readrecv(gfile,'icmr','mid layer',k,rwork1d0,iret=iret)
         if (iret /= 0) call error_msg(trim(my_name),trim(filename),'icmr','read',istop+10,iret)
         if ( diff_res ) then
            grid_b=reshape(rwork1d0,(/size(grid_b,1),size(grid_b,2)/))
            vector(1)=.false.
            call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
            call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
            do kk=1,grd%itotsub
               i=grd%ltosi_s(kk)
               j=grd%ltosj_s(kk)
               work(kk)=grid2(i,j,1)
            enddo
         else
            grid=reshape(rwork1d0,(/size(grid,1),size(grid,2)/))
            call general_fill_ns(grd,grid,work)
         endif
      endif
      if ( icount == icm ) then
         call general_reload2(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz, &
              g_ql,g_qi,g_qr,g_qs,g_qg,g_cf,icount,iflag,ilev,work,uvflag,vordivflag)
      endif

      icount=icount+1
      iflag(icount)=12
      ilev(icount)=k

      if (mype==mype_use(icount)) then
         ! rain water mixing ratio
         call nemsio_readrecv(gfile,'rwmr','mid layer',k,rwork1d0,iret=iret)
         if (iret /= 0) call error_msg(trim(my_name),trim(filename),'rwmr','read',istop+11,iret)
         if ( diff_res ) then
            grid_b=reshape(rwork1d0,(/size(grid_b,1),size(grid_b,2)/))
            vector(1)=.false.
            call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
            call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
            do kk=1,grd%itotsub
               i=grd%ltosi_s(kk)
               j=grd%ltosj_s(kk)
               work(kk)=grid2(i,j,1)
            enddo
         else
            grid=reshape(rwork1d0,(/size(grid,1),size(grid,2)/))
            call general_fill_ns(grd,grid,work)
         endif
      endif
      if ( icount == icm ) then
         call general_reload2(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz, &
              g_ql,g_qi,g_qr,g_qs,g_qg,g_cf,icount,iflag,ilev,work,uvflag,vordivflag)
      endif

      icount=icount+1
      iflag(icount)=13
      ilev(icount)=k

      if (mype==mype_use(icount)) then
         ! snow water mixing ratio
         call nemsio_readrecv(gfile,'snmr','mid layer',k,rwork1d0,iret=iret)
         if (iret /= 0) call error_msg(trim(my_name),trim(filename),'snmr','read',istop+12,iret)
         if ( diff_res ) then
            grid_b=reshape(rwork1d0,(/size(grid_b,1),size(grid_b,2)/))
            vector(1)=.false.
            call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
            call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
            do kk=1,grd%itotsub
               i=grd%ltosi_s(kk)
               j=grd%ltosj_s(kk)
               work(kk)=grid2(i,j,1)
            enddo
         else
            grid=reshape(rwork1d0,(/size(grid,1),size(grid,2)/))
            call general_fill_ns(grd,grid,work)
         endif
      endif
      if ( icount == icm ) then
         call general_reload2(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz, &
              g_ql,g_qi,g_qr,g_qs,g_qg,g_cf,icount,iflag,ilev,work,uvflag,vordivflag)
      endif

      icount=icount+1
      iflag(icount)=14
      ilev(icount)=k

      if (mype==mype_use(icount)) then
         ! snow water mixing ratio
         call nemsio_readrecv(gfile,'grle','mid layer',k,rwork1d0,iret=iret)
         if (iret /= 0) call error_msg(trim(my_name),trim(filename),'grle','read',istop+13,iret)
         if ( diff_res ) then
            grid_b=reshape(rwork1d0,(/size(grid_b,1),size(grid_b,2)/))
            vector(1)=.false.
            call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
            call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
            do kk=1,grd%itotsub
               i=grd%ltosi_s(kk)
               j=grd%ltosj_s(kk)
               work(kk)=grid2(i,j,1)
            enddo
         else
            grid=reshape(rwork1d0,(/size(grid,1),size(grid,2)/))
            call general_fill_ns(grd,grid,work)
         endif
      endif
      if ( icount == icm ) then
         call general_reload2(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz, &
              g_ql,g_qi,g_qr,g_qs,g_qg,g_cf,icount,iflag,ilev,work,uvflag,vordivflag)
      endif

      icount=icount+1
      iflag(icount)=15
      ilev(icount)=k

      if (mype==mype_use(icount)) then
         ! cloud amount 
         call nemsio_readrecv(gfile,'cld_amt','mid layer',k,rwork1d0,iret=iret)
         if (iret /= 0) call error_msg(trim(my_name),trim(filename),'cld_amt','read',istop+14,iret)
         if ( diff_res ) then
            grid_b=reshape(rwork1d0,(/size(grid_b,1),size(grid_b,2)/))
            vector(1)=.false.
            call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
            call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
            do kk=1,grd%itotsub
               i=grd%ltosi_s(kk)
               j=grd%ltosj_s(kk)
               work(kk)=grid2(i,j,1)
            enddo
         else
            grid=reshape(rwork1d0,(/size(grid,1),size(grid,2)/))
            call general_fill_ns(grd,grid,work)
         endif
      endif
      if ( icount == icm .or. k==nlevs ) then
         call general_reload2(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz, &
              g_ql,g_qi,g_qr,g_qs,g_qg,g_cf,icount,iflag,ilev,work,uvflag,vordivflag)
      endif

   enddo ! do k=1,nlevs

   if ( procuse ) then
      if ( diff_res) deallocate(grid_b,grid_b2,grid_c,grid_c2,grid2)
      call destroy_egrid2agrid(p_high)
      deallocate(spec_div,spec_vor)
      deallocate(rwork1d1,clons,slons)
      deallocate(rwork1d0)
      deallocate(grid,grid_v)
      call nemsio_close(gfile,iret=iret)
      if (iret /= 0) call error_msg(trim(my_name),trim(filename),null,'close',istop+9,iret)
   endif
   deallocate(work,work_v)

   ! Convert dry temperature to virtual temperature
   !do k=1,grd%nsig
   !   do j=1,grd%lon2
   !      do i=1,grd%lat2
   !         g_tv(i,j,k) = g_tv(i,j,k)*(one+fv*g_q(i,j,k))
   !      enddo
   !   enddo
   !enddo

   ! Load u->div and v->vor slot when uv are used instead
   if ( uvflag ) then
      call gsi_bundlegetpointer(gfs_bundle,'u' ,ptr3d,ier)
      if ( ier == 0 ) then
         ptr3d=g_u
         call gsi_bundlegetpointer(gfs_bundle,'v' ,ptr3d,ier)
         if ( ier == 0 ) ptr3d=g_v
      else ! in this case, overload: return u/v in sf/vp slot
         call gsi_bundlegetpointer(gfs_bundle,'sf' ,ptr3d,ier)
         if ( ier == 0 ) then
            ptr3d=g_u
            call gsi_bundlegetpointer(gfs_bundle,'vp' ,ptr3d,ier)
            if ( ier == 0 ) ptr3d=g_v
         endif
      endif
   else ! in this case, overload: return u/v in sf/vp slot
      call gsi_bundlegetpointer(gfs_bundle,'sf' ,ptr3d,ier)
      if ( ier == 0 ) ptr3d=g_u
      call gsi_bundlegetpointer(gfs_bundle,'vp' ,ptr3d,ier)
      if ( ier == 0 ) ptr3d=g_v
   endif
   if (zflag) then
      call gsi_bundlegetpointer(gfs_bundle,'z' ,ptr2d,ier)
      if ( ier == 0 ) ptr2d=g_z
   endif

   ! Clean up
   deallocate(g_z)
   deallocate(g_u,g_v)

   ! Print date/time stamp
   if ( mype == 0 ) then
      write(6,700) lonb,latb,nlevs,grd%nlon,nlatm2,&
            fhour,odate,trim(filename)
700   format('GENERAL_READ_FV3ATM_NEMS: read lonb,latb,levs=',&
            3i6,', scatter nlon,nlat=',2i6,', hour=',f6.1,', idate=',4i5,1x,a)
   endif

   return

   ! ERROR detected while reading file
1000 continue
   write(6,*)'GENERAL_READ_FV3ATM_NEMS:  ***ERROR*** reading ',&
       trim(filename),' mype,iret_read=',mype,iret_read,grd%nsig,nlevs
   return

   ! End of routine.  Return

   return
end subroutine general_read_fv3atm_nems

subroutine general_reload2(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz, &
           g_ql,g_qi,g_qr,g_qs,g_qg,g_cf,icount,iflag,ilev,work,uvflag,vdflag)

! !USES:

  use kinds, only: r_kind,i_kind
  use mpimod, only: npe,mpi_comm_world,ierror,mpi_rtype
  use general_sub2grid_mod, only: sub2grid_info
  implicit none

! !INPUT PARAMETERS:

  type(sub2grid_info),                intent(in   ) :: grd
  integer(i_kind),                    intent(inout) :: icount
  integer(i_kind),dimension(npe),     intent(inout) :: ilev,iflag
  real(r_kind),dimension(grd%itotsub),intent(in   ) :: work
  logical,                            intent(in   ) :: uvflag,vdflag

! !OUTPUT PARAMETERS:

  real(r_kind),dimension(grd%lat2,grd%lon2),         intent(  out) :: g_ps
  real(r_kind),dimension(grd%lat2,grd%lon2),         intent(inout) :: g_z
  real(r_kind),dimension(grd%lat2,grd%lon2,grd%nsig),intent(  out) :: g_u,g_v,&
       g_vor,g_div,g_q,g_oz,g_tv,g_ql,g_qi,g_qr,g_qs,g_qg,g_cf


! !DESCRIPTION: Transfer contents of 2-d array global to 3-d subdomain array
!
! !REVISION HISTORY:
!   2004-05-14  treadon
!   2004-07-15  todling, protex-compliant prologue
!   2014-12-03  derber     - introduce vdflag and optimize routines
!
! !REMARKS:
!
!   language: f90
!   machine:  ibm rs/6000 sp; sgi origin 2000; compaq/hp
!
! !AUTHOR:
!   treadon          org: np23                date: 2004-05-14
!
!EOP
!-------------------------------------------------------------------------

   integer(i_kind) i,j,k,ij,klev
   real(r_kind),dimension(grd%lat2*grd%lon2,npe):: sub

   call mpi_alltoallv(work,grd%sendcounts_s,grd%sdispls_s,mpi_rtype,&
        sub,grd%recvcounts_s,grd%rdispls_s,mpi_rtype,&
        mpi_comm_world,ierror)

!$omp parallel do  schedule(dynamic,1) private(k,i,j,ij,klev)
   do k=1,icount
      if ( iflag(k) == 1 ) then
         ij=0
         do j=1,grd%lon2
            do i=1,grd%lat2
               ij=ij+1
               g_z(i,j)=sub(ij,k)
            enddo
         enddo
      elseif ( iflag(k) == 2 ) then
         ij=0
         do j=1,grd%lon2
            do i=1,grd%lat2
               ij=ij+1
               g_ps(i,j)=sub(ij,k)
            enddo
         enddo
      elseif ( iflag(k) == 3 ) then
         klev=ilev(k)
         ij=0
         do j=1,grd%lon2
            do i=1,grd%lat2
               ij=ij+1
               g_tv(i,j,klev)=sub(ij,k)
            enddo
         enddo
      elseif ( iflag(k) == 4 ) then
         klev=ilev(k)
         if ( vdflag ) then
           ij=0
           do j=1,grd%lon2
              do i=1,grd%lat2
                 ij=ij+1
                 g_vor(i,j,klev)=sub(ij,k)
              enddo
           enddo
         endif
         if ( .not. uvflag ) then
           ij=0
           do j=1,grd%lon2
              do i=1,grd%lat2
                 ij=ij+1
                 g_u(i,j,klev)=sub(ij,k)
              enddo
           enddo
         endif
      elseif ( iflag(k) == 5 ) then
         klev=ilev(k)
         if ( vdflag ) then
           ij=0
           do j=1,grd%lon2
              do i=1,grd%lat2
                 ij=ij+1
                 g_div(i,j,klev)=sub(ij,k)
              enddo
           enddo
         endif
         if ( .not. uvflag ) then
           ij=0
           do j=1,grd%lon2
              do i=1,grd%lat2
                 ij=ij+1
                 g_v(i,j,klev)=sub(ij,k)
              enddo
           enddo
         endif
      elseif ( iflag(k) == 6 ) then
         if ( .not. uvflag) then
           write(6,*) 'error in general_reload  u '
         endif
         klev=ilev(k)
         ij=0
         do j=1,grd%lon2
            do i=1,grd%lat2
               ij=ij+1
               g_u(i,j,klev)=sub(ij,k)
            enddo
         enddo
      elseif ( iflag(k) == 7 ) then
         if ( .not. uvflag) then
           write(6,*) 'error in general_reload  v '
         endif
         klev=ilev(k)
         ij=0
         do j=1,grd%lon2
            do i=1,grd%lat2
               ij=ij+1
               g_v(i,j,klev)=sub(ij,k)
            enddo
         enddo
      elseif ( iflag(k) == 8 ) then
         klev=ilev(k)
         ij=0
         do j=1,grd%lon2
            do i=1,grd%lat2
               ij=ij+1
               g_q(i,j,klev)=sub(ij,k)
            enddo
         enddo
      elseif ( iflag(k) == 9 ) then
         klev=ilev(k)
         ij=0
         do j=1,grd%lon2
            do i=1,grd%lat2
               ij=ij+1
               g_oz(i,j,klev)=sub(ij,k)
            enddo
         enddo
      elseif ( iflag(k) == 10 ) then
         klev=ilev(k)
         ij=0
         do j=1,grd%lon2
            do i=1,grd%lat2
               ij=ij+1
               g_ql(i,j,klev)=sub(ij,k)
            enddo
         enddo
      elseif ( iflag(k) == 11 ) then
         klev=ilev(k)
         ij=0
         do j=1,grd%lon2
            do i=1,grd%lat2
               ij=ij+1
               g_qi(i,j,klev)=sub(ij,k)
            enddo
         enddo
      elseif ( iflag(k) == 12 ) then
         klev=ilev(k)
         ij=0
         do j=1,grd%lon2
            do i=1,grd%lat2
               ij=ij+1
               g_qr(i,j,klev)=sub(ij,k)
            enddo
         enddo
      elseif ( iflag(k) == 13 ) then
         klev=ilev(k)
         ij=0
         do j=1,grd%lon2
            do i=1,grd%lat2
               ij=ij+1
               g_qs(i,j,klev)=sub(ij,k)
            enddo
         enddo
      elseif ( iflag(k) == 14 ) then
         klev=ilev(k)
         ij=0
         do j=1,grd%lon2
            do i=1,grd%lat2
               ij=ij+1
               g_qg(i,j,klev)=sub(ij,k)
            enddo
         enddo
      elseif ( iflag(k) == 15 ) then
         klev=ilev(k)
         ij=0
         do j=1,grd%lon2
            do i=1,grd%lat2
               ij=ij+1
               g_cf(i,j,klev)=sub(ij,k)
            enddo
         enddo
      endif
   enddo ! do k=1,icount

   icount=0
   ilev=0
   iflag=0

   return

end subroutine general_reload2

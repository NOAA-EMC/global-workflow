      module constant_cc

      use machine, only: kind_phys

      use physcons

      end module constant_cc
!
!***********************************************************************
!
      module atm_cc

      use cmp_comm, only:
     >   mpi_comm_atmos => comm_local,
     >   coupler_id,
     >   component_master_rank_local,
     >   process_rank_local,
!          note: the latter two are only to compare with each
!          other and thus determine if the process is the local
!          master (root) process. (comparison of
!          component_master_rank_global with process_rank_global
!          would not work because the former is known only to
!          coupler process and the local master process itself.)
     >   component_nprocs,
     >   kind_real,mpi_kind_real,
     >   mpi_integer,mpi_status_size,
     >   ibuffer
      use mpi_def, only: comm_tiles => mc_comp
      use layout1, only: tiles_nprocs => nodes_comp

      implicit none

      integer latg,latr,lonf,lonr
      integer latd
      integer lats_node_r,ipt_lats_node_r

      integer n2d

      integer, allocatable:: global_lats_r(:),lonsperlar(:)

      logical comp /.false./

!controls:
!     integer nunit_announce_cc /6/, verblev /5/
!     integer nunit_announce_cc /6/, verblev /2/
      integer nunit_announce_cc /6/, verblev /1/

      save

      end module atm_cc
!
!***********************************************************************
!
      module surface_cc

      use constant_cc, only:
     >           hvap_cc=>con_hvap,         ! - this is l, to use in le
                                            ! check: if l in le must
                                            ! rather be either evap.
                                            ! heat or evap.+melt. heat
     >           jcal_cc=>con_jcal,         ! - j in cal
     >           kind_phys_cc=>kind_phys

      implicit none

      integer, parameter:: 
     >           kind_sfcflux=8,
     >           kind_sst=8,
     >           kind_slmsk=8,
     >           kind_dt_cc=8,              !-->cpl insertion: add model vars precision here <--
     >           kind_modelvar=8

      integer,allocatable:: islm_rg(:,:),islm_fg(:,:)
      real (kind=kind_sfcflux),allocatable:: 
     >dusfc_cc(:,:),dvsfc_cc(:,:),
     >dtsfc_cc(:,:),dqsfc_cc(:,:),precr_cc(:,:),
     >dlwsfc_cc(:,:),ulwsfc_cc(:,:),swsfc_cc(:,:),
!-->cpl insertion
     >xmu_cc(:,:),dsw_cc(:,:),dlw_cc(:,:),ffmm_cc(:,:),ffhh_cc(:,:),
     >snw_cc(:,:),lprec_cc(:,:),sst_ave(:,:)
!<--cpl insertion

      real (kind=kind_sst),allocatable:: sst_cc(:,:)

      real (kind=kind_dt_cc) dt_cc,dto_cc         !-->cpl insertion: add dto_cc

!--> cpl insertion: add model vars here:
      real (kind=kind_modelvar),allocatable::
     >   t_bot_cc(:,:),u_bot_cc(:,:),v_bot_cc(:,:), q_bot_cc(:,:), 
     >   p_bot_cc(:,:),p_surf_cc(:,:),z_bot_cc(:,:),t_sfc_cc(:,:)
     &,  fice_sfc_cc(:,:), hice_sfc_cc(:,:)
!<-- cpl insertion

!     logical lsout_cc
      logical lssav_cc,lgetsstice_cc,l_df_cc
!--> cpl insertion
      logical lsout_cc_momice,lsout_cc_momocn
      integer i_dto2dta_cc
!<-- cpl insertion
      integer i_dtc2dta_cc
!     parameter (i_dtc2dta_cc=3) ! <- ratio of time steps in om and am
      real (kind=kind_dt_cc) dta2dtc_cc,dta2dto_cc

!     real(kind=kind_phys_cc) convrad_cc
!     parameter (convrad_cc=jcal_cc*1.e4/60.) ! - see progtmr.f,
!                                             ! subr. progtm

!     integer kdtmax_cc/0/
      integer n_do_tstep_cc /0/

      character*180 s_cc

      integer islm_os_value
      integer islm_si_value,islm_l_value
      parameter (islm_os_value=0)
                  !<- must be integer open sea value in am sea/land mask
!     parameter( islm_l_value=1,
!                 !<- must be integer land value in am sea/land mask
!    >           islm_si_value=2)
!                 !<- must be integer sea ice value in am sea/land mask

      real slm_os_value,unrealistically_low_sst,unrealistically_low_sv
!     real unrealistically_low_svp,unrealistically_low_sf
      parameter (unrealistically_low_sst=0.01,
                           ! <- must be unreal low but >=0., see
                           ! subr. o2a --- check!
     >     unrealistically_low_sv=-1.e30)
                           ! <- must be negative unreal low surface flux
                           ! or other surface value to be sent
                           ! to coupler, see coupler code
      parameter (slm_os_value=real(islm_os_value))
                   ! <- must be real open sea value in am
                   ! sea/land mask array 
!     parameter (unrealistically_low_svp=0.99*unrealistically_low_sv,
!    >               unrealistically_low_sf=unrealistically_low_sv)
                        !<- this used to be the name of the value; it
                        ! is not used any more but may be referred to
                        ! in comments

      save

      end module surface_cc
!
!***********************************************************************
!
      subroutine atm_cmp_start

      use atm_cc, only: component_nprocs,verblev,ibuffer,coupler_id

      implicit none

      integer atmos_id /1/, atmos_master_rank_local /0/
      character*20 s
!

!        print*,'am: to call cmp_init'
                      !<-id of am as a component of the coupled system
      call cmp_init(atmos_id,1)
                             !<-"flexibility level"
!        print*,'am: back from cmp_init'
!      if (coupler_id.ge.0) verblev=min(verblev,ibuffer(4))
      if (coupler_id.ge.0) verblev=min(verblev,2)

        atmos_master_rank_local=component_nprocs-1
                               !<- this redefinition is to meet the
                               ! requirement of subr. split2d_r used
                               ! in disassemble_cc for disassembling
                               ! 2d fields. the requirement seems
                               ! to be that the input argument
                               ! representing a whole grid array be
                               ! defined in process of the largest rank
                               ! which seems to be considered i/o
                               ! process. to use a different value,
                               ! e.g. the conventional 0, split2d_r
                               ! (or disassemble_cc) must be rewritten.
                     ! (strangely, unsplit2d_r does not pose this
                     ! requirement and uses a dummy arg. ioproc to
                     ! identify the process where the whole grid array
                     ! is to be defined. seemingly.)

      atmos_master_rank_local=0  ! see above for modifications needed
                                 ! to support this change

      call cmp_intro(atmos_master_rank_local)

      write(s,'(i2)') verblev
      call atm_announce('back from cmp_intro, verblev='//s,2)

      return
      end
!
!***********************************************************************
!
      subroutine atm_cmp_start1

      use atm_cc, only: process_rank_local,verblev,ibuffer,coupler_id

      implicit none

      integer atmos_id /1/
!

                      !<-id of am as a component of the coupling system
      call cmp_init(atmos_id,1)
                             !<-"flexibility level"

!      if (coupler_id.ge.0) verblev=min(verblev,ibuffer(4))
      if (coupler_id.ge.0) verblev=min(verblev,2)

!           print*,'am: back from cmp_init, process_rank_local=',
!    >      process_rank_local

      return
      end
!
!***********************************************************************
!
      subroutine atm_cmp_start2(me)      

      use atm_cc, only: verblev

      implicit none

      integer me

      character*20 s
!

      if (me .eq. 0) then
        call cmp_intro_m
      else
        call cmp_intro_s
      end if

      write(s,'(i2)') verblev
      call atm_announce('back from cmp_intro_m, verblev='//s,1)

      return
      end
!
!***********************************************************************
!
      subroutine atm_tiles_init(lonr_dummy,latr_dummy,lonf_dummy,
     >latg_dummy,latd_dummy,ipt_lats_node_r_dummy,
     >global_lats_r_dummy,lonsperlar_dummy)

      use atm_cc

      implicit none

      integer lonr_dummy,latr_dummy,lonf_dummy,latg_dummy,latd_dummy
      integer ipt_lats_node_r_dummy
      integer global_lats_r_dummy(latr_dummy),
     >        lonsperlar_dummy(latr_dummy)

      character*10 s
!

      lonr=lonr_dummy
      latr=latr_dummy
      lonf=lonf_dummy
      latg=latg_dummy
      latd=latd_dummy
      lats_node_r=latd
      ipt_lats_node_r=ipt_lats_node_r_dummy

      n2d=lonf*latg

      write(s,'(i5)') lonr
      call atm_announce('atm_tiles_init: lonr='//s,2)
      write(s,'(i5)') latr
      call atm_announce('atm_tiles_init: latr='//s,2)
      write(s,'(i5)') lonf
      call atm_announce('atm_tiles_init: lonf='//s,2)
      write(s,'(i5)') latg
      call atm_announce('atm_tiles_init: latg='//s,2)
      write(s,'(i5)') latd
      call atm_announce('atm_tiles_init: latd='//s,2)

      call glob_abort(abs(lonr-lonf)+abs(latr-latg),
     >'unexpected: lonr, lonf or latr, latg differ. aborting',1)

      if (.not. allocated(global_lats_r)) allocate(global_lats_r(latr))
      if (.not. allocated(lonsperlar))    allocate(lonsperlar(latr))
      global_lats_r=global_lats_r_dummy
      lonsperlar=lonsperlar_dummy

      call atm_announce(
     >'atm_tiles_init: global_lats_r, lonsperlar assigned',2)
      if (verblev.ge.2) then
        print*,'am: atm_tiles_init',component_master_rank_local,
     >  ' ipt_lats_node_r=',ipt_lats_node_r,' latd=',latd
        print*,'am: atm_tiles_init',component_master_rank_local,
     >  ' global_lats_r: ',global_lats_r
        print*,'am: atm_tiles_init',component_master_rank_local,
     >  ' lonsperlar: ',lonsperlar
      end if

      call initialize_tiling

      return
      end
!
!***********************************************************************
!
      subroutine atm_surf_init

      use atm_cc, only: lonr,latd,lonf,latg

      use surface_cc

      implicit none

      integer rc
c

      write(s_cc,'(4i5)') lonr,latd,lonf,latg
      call atm_announce(
     >'atm_surf_init: lonr,latd,lonf,latg: '//s_cc,2)
!--> cpl insertion
      if (.not. allocated(t_bot_cc))    allocate(t_bot_cc(lonr,latd))
      if (.not. allocated(u_bot_cc))    allocate(u_bot_cc(lonr,latd))
      if (.not. allocated(v_bot_cc))    allocate(v_bot_cc(lonr,latd))
      if (.not. allocated(q_bot_cc))    allocate(q_bot_cc(lonr,latd))
      if (.not. allocated(p_bot_cc))    allocate(p_bot_cc(lonr,latd))
      if (.not. allocated(z_bot_cc))    allocate(z_bot_cc(lonr,latd))
      if (.not. allocated(p_surf_cc))   allocate(p_surf_cc(lonr,latd))
      if (.not. allocated(t_sfc_cc))    allocate(t_sfc_cc(lonr,latd))
      if (.not. allocated(fice_sfc_cc)) allocate(fice_sfc_cc(lonr,latd))
      if (.not. allocated(hice_sfc_cc)) allocate(hice_sfc_cc(lonr,latd))
      if (.not. allocated(xmu_cc))      allocate(xmu_cc(lonr,latd))
      if (.not. allocated(dsw_cc))      allocate(dsw_cc(lonr,latd))
      if (.not. allocated(dlw_cc))      allocate(dlw_cc(lonr,latd))
      if (.not. allocated(ffmm_cc))     allocate(ffmm_cc(lonr,latd))
      if (.not. allocated(ffhh_cc))     allocate(ffhh_cc(lonr,latd))
!
!     allocate(t_bot_cc(lonr,latd),u_bot_cc(lonr,latd),
!    >     v_bot_cc (lonr,latd),q_bot_cc(lonr,latd),
!    >     p_bot_cc (lonr,latd),p_surf_cc(lonr,latd),
!    >     z_bot_cc (lonr,latd),
!    >     t_sfc_cc (lonr,latd),
!    >     fice_sfc_cc (lonr,latd),hice_sfc_cc (lonr,latd),
!    >     xmu_cc(lonr,latd),
!    >     dsw_cc(lonr,latd), dlw_cc(lonr,latd),
!    >     ffmm_cc(lonr,latd), ffhh_cc(lonr,latd) )
!
      t_bot_cc    = 0.
      u_bot_cc    = 0.
      v_bot_cc    = 0.
      q_bot_cc    = 0.
      p_bot_cc    = 0.
      p_surf_cc   = 0.
      z_bot_cc    = 0.
      t_sfc_cc    = 0.
      fice_sfc_cc = 0.
      hice_sfc_cc = 0.
      xmu_cc      = 0.
      dsw_cc      = 0.
      dlw_cc      = 0.
      ffmm_cc     = 0.
      ffhh_cc     = 0.
!<-- cpl insertion

      if (.not. allocated(dusfc_cc))  allocate(dusfc_cc(lonr,latd))
      if (.not. allocated(dvsfc_cc))  allocate(dvsfc_cc(lonr,latd))
      if (.not. allocated(dtsfc_cc))  allocate(dtsfc_cc(lonr,latd))
      if (.not. allocated(dqsfc_cc))  allocate(dqsfc_cc(lonr,latd))
      if (.not. allocated(precr_cc))  allocate(precr_cc(lonr,latd))
      if (.not. allocated(sst_cc))    allocate(sst_cc(lonr,latd))
      if (.not. allocated(dlwsfc_cc)) allocate(dlwsfc_cc(lonr,latd))
      if (.not. allocated(ulwsfc_cc)) allocate(ulwsfc_cc(lonr,latd))
      if (.not. allocated(swsfc_cc))  allocate(swsfc_cc(lonr,latd))
      if (.not. allocated(sst_ave))   allocate(sst_ave(lonr,latd))
      if (.not. allocated(snw_cc))    allocate(snw_cc(lonr,latd))
      if (.not. allocated(lprec_cc))  allocate(lprec_cc(lonr,latd))
!
!     allocate(dusfc_cc(lonr,latd),dvsfc_cc(lonr,latd),
!    >     dtsfc_cc (lonr,latd),dqsfc_cc(lonr,latd),
!    >     precr_cc(lonr,latd),sst_cc(lonr,latd),
!    >     dlwsfc_cc(lonr,latd),ulwsfc_cc(lonr,latd),
!    >     swsfc_cc(lonr,latd) ,sst_ave(lonr,latd),
!    >     snw_cc(lonr,latd), lprec_cc(lonr,latd) )     

      dusfc_cc  = 0.
      dvsfc_cc  = 0.
      dtsfc_cc  = 0.
      dqsfc_cc  = 0.
      precr_cc  = 0.
      snw_cc    = 0.
      lprec_cc  = 0.
      dlwsfc_cc = 0.
      ulwsfc_cc = 0.
      swsfc_cc  = 0.
      sst_ave   = 0.

      if (.not. allocated(islm_rg)) allocate(islm_rg(lonr,latd))
      if (.not. allocated(islm_fg)) allocate(islm_fg(lonr,latd))
!
!     allocate(islm_rg(lonr,latd),islm_fg(lonr,latd))

      call atm_announce('atm_surf_init: islm_rg, islm_fg allocated',1)

      if (kind_sfcflux.ne.kind_phys_cc) then
        print*,'atm_surf_init: kind_sfcflux, kind_phys: ',
     >  kind_sfcflux, kind_phys_cc
        call glob_abort(1,'kind_sfcflux.ne.kind_phys_cc, gbphys args'//
     >  ' must be redeclared and code adjustments made',rc)
      end if

      return
      end
!
!***********************************************************************
!
      subroutine atm_recvdtc(dta)

      use atm_cc, only:
     >   mpi_comm_atmos,
     >   coupler_id,
     >   component_master_rank_local,
     >   kind_real,mpi_kind_real

      use surface_cc, only:
     >dt_cc,dta2dtc_cc,i_dtc2dta_cc,i_dto2dta_cc,
     >s_cc , dto_cc,dta2dto_cc                  !--> cpl insertion: add dto_cc, dta2dto_cc

      implicit none

      real dta
      real (kind=kind_real) buf(2)
      integer rc,sizebuf
      character*40 s

      call atm_announce('atm_recvdtc: to receive c time step',2)
      buf=0.
      sizebuf=size(buf)
      call cmp_recv(buf,sizebuf)
      if (coupler_id.lt.0) then
        dt_cc=0.
        dto_cc=0.
        call atm_announce(
     >  'atm_recvdtc: c time step assigned 0, as it is standalone mode'
     >  ,2)
      else
        write(s,'(e20.12,e20.12)') buf(1),buf(2)
        call atm_announce(
     >  'atm_recvdtc: c time step ='//trim(s)//' received',2)
        call mpi_bcast(buf,2,mpi_kind_real,
     >  component_master_rank_local,mpi_comm_atmos,rc)
        call atm_announce('atm_recvdtc: c time step broadcast',2)
        dt_cc=buf(1)
        dto_cc=buf(2)
      end if

      i_dtc2dta_cc = dt_cc/dta  + 0.001
      i_dto2dta_cc = dto_cc/dta + 0.001

      print *,'am: dto_cc=',dto_cc,' dta=',dta,' i_dto2dta_cc=',
     & i_dto2dta_cc,' dt_cc=',dt_cc,' i_dtc2dta_cc=',i_dtc2dta_cc

      if (i_dtc2dta_cc.eq.0) then
        i_dtc2dta_cc=4
        call atm_announce('ratio of om/am time steps =0, assigned 4 .'//
     >  ' this should only occur if it is standalone mode',2)
      else
        write(s_cc,'(i2,i2)') i_dtc2dta_cc,i_dto2dta_cc
!       print *,' s_cc=',s_cc
        call atm_announce('ratio of om/am time steps: '//trim(s_cc),2)
      end if

      if (i_dtc2dta_cc > 0) then
        dta2dtc_cc = 1./i_dtc2dta_cc
      else
        dta2dtc_cc = 0.0
      endif

      if (i_dto2dta_cc > 0) then
        dta2dto_cc = 1./i_dto2dta_cc
      else
        dta2dto_cc = 0.0
      endif

      return
      end
!
!***********************************************************************
!
      subroutine atm_sendgrid(xlon,xlat)

      use atm_cc

      implicit none

      real (kind=kind_real) xlon(lonr,latd),xlat(lonr,latd),x(lonf,latg)
!     real (kind=kind_real) alon(lonf),alat(latg),
!    >y(lonf,latg)

      integer buf(2)
!     integer i,j

!     logical fg

!     character*50 s
      
      if (coupler_id.lt.0) return    !   <- standalone mode

      buf(1)=lonf
      buf(2)=latg
      call atm_announce('to send grid dimensions',1)
      call cmp_integer_send(buf,2)
      call atm_announce('grid dimensions sent',1)

      call assemble_cc(x,xlon)

!-->cpl deletion, mom4, do not need laon, alat
!      if (component_master_rank_local.eq.process_rank_local) then
!
!c       alon=x(:,1)
!        alon=x(:,latg/2) ! assigns closest to equator lat. circle,
!                         ! where in reduced grid numb. of longitudes
!                         ! is maximal and = that in full grid
!
!        fg=.true.
!        do j=1,latg
!        do i=1,lonf
!          if (alon(i).ne.x(i,j)) then
!            fg=.false.
!            write(s,'(2i5,1p2e16.7)') j,i,alon(i),x(i,j)
!c           call glob_abort(1,
!            call atm_announce(
!     >      'atm_sendgrid: inhomogeneous longitudes'//s,2)
!            exit
!          end if
!        end do
!        end do
!        if (fg) then
!          call atm_announce('atm_sendgrid: full grid',1)
!        else
!          call atm_announce('atm_sendgrid: reduced grid',1)
!        end if
!
!        call atm_announce('to send array of longitudes',1)
!        call cmp_send(alon,lonf)
!        call atm_announce('array of longitudes sent',1)
!
!      end if
!<-- cpl deletion
 
      call assemble_cc(x,xlat)

!-->cpl deletion, mom4, do not need laon, alat
!      if (component_master_rank_local.eq.process_rank_local) then
!
!        alat=x(1,:)
!
!        do j=1,latg
!          if (alat(j).ne.x(2,j)) then
!            write(s,'(i5,1p2e16.7)') j,alat(j),x(2,j)
!            call glob_abort(1,
!     >      'atm_sendgrid: inhomogenous latitudes, aborting'//s,1)
!          end if
!        end do
!
!        call atm_announce('to send array of latitudes',1)
!        call cmp_send(alat,latg)
!        call atm_announce('array of latitudes sent',1)
!
!      end if
!<-- cpl deletion

      return
      end
!
!***********************************************************************
!
      subroutine atm_sendslm(slmsk)
!
!        this is to send sea/land mask with 0. on sea (either open sea
!        or sea ice) and 1. on land. for the assumptions about slmsk
!        argument, see code/comments below

      use atm_cc

      use surface_cc, only: islm_rg,islm_fg,kind_slmsk

      implicit none

      real (kind=kind_slmsk) slmsk(lonr,latd)

      real(kind=kind_real), dimension(lonr,latd):: slm1,slm2,slm0
      real slm(lonf,latg)
      integer i,j,lat,lons
      character*80 s
      logical bad_slm /.false./

      if (coupler_id.lt.0) return    !   <- standalone mode

      if (verblev.ge.2) then
         print*,'atmsendslm entered, lonr,latd,lonf,latg: ',
     >   lonr,latd,lonf,latg
      end if

      do j=1,latd
      do i=1,lonr
        if (abs(slmsk(i,j)-2.).lt.1.e-5              ! sea ice
     >      .or. abs(slmsk(i,j)).lt.1.e-5) then      ! open sea
          slm1(i,j)=0.
        else if (abs(slmsk(i,j)-1.).lt.1.e-5) then   ! land
          slm1(i,j)=1.
        else
          slm1(i,j)=666.
        end if
      end do
      end do

      islm_rg=nint(slm1)
            !<- store reduced grid integer mask array for future
            ! communications; it will only be needed for uninterpred_cc

!        print*,'atmsendslm to call uninterpred_cc'

      call uninterpred_cc(1,islm_rg,slm1,slm2)
                  ! <- interpolation from reduced grid (i.e. with # of
                  ! longitudes varying from lat. circle to lat. circle)
                  ! to full grid. 

!        print*,'atmsendslm back from uninterpred_cc'

        ! because 1st arg. iord=1, islm_rg values do not matter here, it
        ! is just a dummy input argument with proper type/dimensions.
        ! reduced grid mask slm1 is interpolated to full grid mask
        ! slm2 (both arrays are local (per process)) by taking the
        ! nearest value on the lat. circle. this procedure should be
        ! reversible.
! reversibility test:->

!        print*,'atmsendslm to call interpred_cc'

      call interpred_cc(1,islm_fg,slm2,slm0)
                           !<- same thing: islm_fg values don't matter.
                           ! and they are undefined here.

!        print*,'atmsendslm back from interpred_cc'

      do j=1,latd
        lat=global_lats_r(ipt_lats_node_r-1+j)
        lons=lonsperlar(lat)
        do i=1,lons
          if (slm0(i,j).ne.slm1(i,j)) then
            write(s,'("slm: r2f irreversible",2i6,2pe17.9)')
     >      i,j,slm1(i,j),slm0(i,j)
            bad_slm=.true.
            exit
          end if
        end do
      end do
! <-: reversibility test

!        print*,'atmsendslm finished reversibility test'

! value test:->
      do j=1,latd
      do i=1,lonr
        if (slm2(i,j).ne.0. .and. slm2(i,j).ne.1.) then
          write(s,'("bad slm value",2i6,1pe20.12)') i,j,slm2(i,j)
          bad_slm=.true.
          exit
        end if
      end do
      end do
! <-: value test

!        print*,'atmsendslm finished value test'

      if (bad_slm) then
        call glob_abort(1,'atm_sendslm: '//s,1)
      end if

!        print*,'atmsendslm to assign islm_fg=nint(slm2)'


      islm_fg=nint(slm2)
            !<- store full grid integer mask array for future
            ! communications; it will only be needed for interpred_cc

!        print*,'atmsendslm to call assemble_cc'


      call assemble_cc(slm,slm2)

!        print*,'atmsendslm back from assemble_cc'

!--> cpl deletion
!d      call cmp_send(slm,n2d)
!<-- cpl deletion

!        print*,'atmsendslm to return'


      return
      end
!
!***********************************************************************
!
      subroutine atm_getsstice
     >(tsea,tisfc,fice,hice,sheleg,slmsk,kdt)

      use atm_cc, only: kind_real,lonr,latd,coupler_id,n2d,latg,lonf

      use surface_cc, only:
     > lgetsstice_cc,kind_sst,kind_slmsk,islm_fg,
     >sst_cc, slm_os_value,unrealistically_low_sst,
     >sst_ave,lsout_cc_momocn,dta2dto_cc,i_dto2dta_cc

      implicit none

      integer kdt
      real (kind=kind_sst),dimension(lonr,latd),intent(inout) :: tsea,
     >   tisfc, fice, hice, sheleg
      real,dimension(:,:),allocatable :: fice_cc,hice_cc,
     >    hsno_cc
      real (kind=kind_slmsk) slmsk(lonr,latd)

      logical recv

!     real, parameter:: rlapse=0.65e-2
      real, parameter:: cimin=0.15, himin=0.10, himax=8.0, tfw=271.2
      real, parameter:: ds=330.0

      integer i,j
!

      recv=lgetsstice_cc

      allocate(fice_cc(lonr,latd),hice_cc(lonr,latd),
     >  hsno_cc(lonr,latd) )

!     print *,' am: in atm_getsst recv=',recv,' coupler_id=',coupler_id

      if (recv) then
        call atm_announce('atm_getsstice: to receive sst',2)
        call atm_tiles_recv(sst_cc,fval=unrealistically_low_sst,iord=2)
        call atm_announce('atm_getsstice: sst received',2)

!
        call atm_announce('atm_getsstice: to receive fice',2)
        call atm_tiles_recv(fice_cc,iord=2)
        call atm_announce('atm_getsstice: fice received',2)

        call atm_announce('atm_getsstice: to receive hice',2)
        call atm_tiles_recv(hice_cc,iord=2)
        call atm_announce('atm_getsstice: hice received',2)

        call atm_announce('atm_getsstice: to receive hsno',2)
        call atm_tiles_recv(hsno_cc,iord=2)
        call atm_announce('atm_getsstice: hsno received',2)

      end if
      
      if (coupler_id.lt.0) return    !   <- standalone mode

      if (recv .and. kdt > 1) then

        sst_ave=sst_ave+sst_cc
        do j=1,latd
          do i=1,lonr
            if (abs(slmsk(i,j)-slm_os_value).lt.0.01) then 
            if (fice_cc(i,j).ge.cimin) then
              slmsk(i,j)=2.0
              fice(i,j)=fice_cc(i,j)
              hice(i,j)=max(min(hice_cc(i,j)/fice_cc(i,j),himax),himin)
              sheleg(i,j)=hsno_cc(i,j)*ds
              tisfc(i,j)=(tsea(i,j)-(1.-fice_cc(i,j))*tfw)/fice_cc(i,j)
            end if
            else if (slmsk(i,j).gt.1.5) then
            if (fice_cc(i,j).ge.cimin) then
              fice(i,j)=fice_cc(i,j)
              hice(i,j)=max(min(hice_cc(i,j)/fice_cc(i,j),himax),himin)
              sheleg(i,j)=hsno_cc(i,j)*ds
              tsea(i,j)=tisfc(i,j)*fice_cc(i,j)+tfw*(1.-fice_cc(i,j))
            else
              fice(i,j)   = 0.0
              hice(i,j)   = 0.0
              sheleg(i,j) = 0.0
              tsea(i,j)   = tfw
              tisfc(i,j)  = tfw
              slmsk(i,j)  = 0.0
            end if
            else
              fice(i,j) = 0.0
              hice(i,j) = 0.0
            end if
          end do
        end do

      endif

      if (lsout_cc_momocn) then
        if(kdt > i_dto2dta_cc) then
!     print *,'am:  sst_ave=',sst_ave(1,1),' dta2dto_cc=',dta2dto_cc
          sst_ave=sst_ave*dta2dto_cc
          do j=1,latd
            do i=1,lonr
              if (abs(slmsk(i,j)-slm_os_value).lt.0.01) then
                if (sst_ave(i,j).gt.unrealistically_low_sst)
     &          tsea(i,j) = sst_ave(i,j)
              end if
            end do
          end do
          sst_ave=0.
        else
          sst_ave=0.
        endif
      endif

      deallocate(fice_cc)
      deallocate(hice_cc)
      deallocate(hsno_cc)

      contains

      subroutine atm_tiles_recv(f,fval,iord)

      implicit none
      real (kind=kind_real) f(lonr,latd)
      real,optional,intent(in) ::  fval
      integer,optional,intent(in) :: iord

      real (kind=kind_real) f1(lonr,latd)
      real (kind=kind_real) x(lonf,latg)
      integer kmsk(lonr,latd),i,j,iiord,ik
!

      if (coupler_id.lt.0) return    !   <- standalone mode

!     print *,' am: in atm_tiles_recv f=',f(1,1),' lonr=',lonr,latd

      call cmp_recv(x,n2d)

!     print *,' am: after cmp_recv in atm_tiles_recv n2d=',n2d
!    &,' x=',x(1,1)

      call disassemble_cc(x,f1)

      kmsk=islm_fg
      ik=0
      if ( present(fval) )then
       do j=1,latd
        do i=1,lonr
          if (f1(i,j).le.fval) kmsk(i,j)=1
          if (f1(i,j).le.fval) ik=ik+1
        end do
       end do
      endif
      if ( present(iord) ) then
          iiord=iord
      else
          iiord=2
      endif
      call interpred_cc(iiord,kmsk,f1,f)
                ! <- interpolation to reduced grid (i.e. with # of
                ! longitudes varying from lat. circle to lat. circle)
                ! from full grid

      end subroutine atm_tiles_recv

      end subroutine
!
!***********************************************************************
!
      subroutine atm_announce(s,dbglev)

      use atm_cc, only: nunit_announce_cc,verblev

      implicit none

      character*(*) s
      integer dbglev
!
      if (dbglev.le.verblev)
     >  call cmp_announce(nunit_announce_cc,'am: '//s)

      return
      end
!
!***********************************************************************
!
      subroutine atm_dbg1(kdt,s,dbglev)

      use atm_cc, only: nunit_announce_cc,verblev
      use surface_cc

      implicit none

      integer kdt
      character*(*) s
      integer dbglev
!
      if (dbglev.gt.verblev) return

!--> cpl change: write lsout_cc_momice and lsout_cc_momocn  <--
      write(s_cc,'("'//trim(s)//
     >': kdt=",i8," lsout_cc_momice=",l1, 
     >" lsout_cc_momocn=",l1," lgetsstice_cc=",l1)'
     >) kdt,lsout_cc_momice,lsout_cc_momocn,lgetsstice_cc

      call cmp_announce(nunit_announce_cc,'am: dbg1: '//s_cc)

      return
      end
!
!***********************************************************************
!
      subroutine atm_dbg2(kdt,phour,zhour,shour,dbglev)

      use atm_cc, only: nunit_announce_cc,verblev
      use surface_cc

      implicit none

      integer kdt
      real phour,zhour,shour
      integer dbglev
!
!           print*,'am: atm_dbg2 entered'

      if (dbglev.gt.verblev) return

!           print*,'am: atm_dbg2 to do write(s_cc, ...'

!--> cpl change: write lsout_cc_momice and lsout_cc_momocn  <--
!
      write(s_cc,'("do_tstep entry",i6," kdt=",i8,'//
     >'" phour,zhour,shour: ",1p,3e15.7,0p," lsout_cc_momice=",l1,'//
     >'" lsout_cc_momocn=",l1,'//
     >'" lgetsstice_cc=",l1)') n_do_tstep_cc,kdt,phour,zhour,shour,
     > lsout_cc_momice,lsout_cc_momocn,lgetsstice_cc

      call cmp_announce(nunit_announce_cc,'am: dbg2: '//s_cc)

      return
      end
!
!***********************************************************************
!
      subroutine atm_tstep_init(kdt)

      use namelist_def, only: lssav
      use surface_cc

      implicit none

      integer kdt
!

      call atm_announce('dotstep entered, in atm_tstep_init',3)
      n_do_tstep_cc=n_do_tstep_cc+1
      lssav_cc=lssav
      l_df_cc=.not.lssav   ! - double-check
!--> cpl deletion 
!d      lsout_cc=(mod(kdt,i_dtc2dta_cc).eq.0)  ! <- still double-check
!d     > .and. .not. l_df_cc
!<-- cpl deletion 
!--> cpl insertion
!moor  if (i_dtc2dta_cc > 0) then
      if (i_dtc2dta_cc > 0 .and. i_dto2dta_cc > 0) then
        lsout_cc_momice = (mod(kdt,max(1,i_dtc2dta_cc)).eq.0)  ! <- still double-check
     >                   .and. .not. l_df_cc                   ! <- instantaneous vars
      else
        lsout_cc_momice = .false.
      endif
      if (i_dto2dta_cc > 0) then
       lsout_cc_momocn = (mod(kdt,max(1,i_dto2dta_cc)).eq.0)  ! <- still double-check
     >                   .and. .not. l_df_cc
      else
!       i_dto2dta_cc = .false.
        lsout_cc_momocn = .false.
      endif
!<-- cpl insertion
      lgetsstice_cc=mod(kdt,max(1,i_dtc2dta_cc)).eq.0                !-check!
     > .and. .not. l_df_cc

      if (kdt == 1) then
        print *,'in atm initial,kdt=',kdt,'dtc/dta=', i_dtc2dta_cc,
     >    'dto/dta=',i_dto2dta_cc,'lsout_cc_momice=',
     >    lsout_cc_momice, 
     >   'lsout_cc_momocn=',lsout_cc_momocn,'lgetsstice_cc=',
     &    lgetsstice_cc,'lssav=',lssav,mod(kdt,max(1,i_dtc2dta_cc))
     >                  ,mod(kdt,max(1,i_dto2dta_cc))
      endif
      return
      end
!
!***********************************************************************
!
      subroutine atm_sendfluxes(slmsk)

      use atm_cc, only: lonr,latd

      use surface_cc

      implicit none

      real (kind=kind_slmsk) slmsk(lonr,latd)
!     integer i,j
!

!--> cpl insertion: send model vars first to coupler
       if (lsout_cc_momice) then
        call atm_announce('to send t_sfc',2)
        call atm_sendflux(t_sfc_cc)
        call atm_announce('to send t_bot',2)
!       print *,'send fluxes, t_bot(1:10)=',t_bot_cc(1:10,1)
        call atm_sendflux(t_bot_cc)
        call atm_announce('to send u_bot',2)
        call atm_sendflux(u_bot_cc)
        call atm_announce('to send v_bot',2)
        call atm_sendflux(v_bot_cc)
        call atm_announce('to send q_bot',2)
        call atm_sendflux(q_bot_cc)
        call atm_announce('to send p_bot',2)
        call atm_sendflux(p_bot_cc)
        call atm_announce('to send p_surf',2)
        call atm_sendflux(p_surf_cc)
        call atm_announce('to send z_bot',2)
        call atm_sendflux(z_bot_cc)
        call atm_announce('to send xmu',2)
        call atm_sendflux(xmu_cc)
        call atm_announce('to send dlw',2)
        call atm_sendflux(dlw_cc)
        call atm_announce('to send dsw',2)
        call atm_sendflux(dsw_cc)
        call atm_announce('to send ffmm',2)
        call atm_sendflux(ffmm_cc)
        call atm_announce('to send ffhh',2)
        call atm_sendflux(ffhh_cc)
        call atm_announce('end of send variables',2)

        call atm_maxmin(lonr,latd,snw_cc,'in atm, snw_cc')

        snw_cc(:,:)=snw_cc(:,:)/dt_cc*1.e3
        call atm_maxmin(lonr,latd,snw_cc,'in atm,2 snw_cc')

        call atm_sendflux(snw_cc)
        call atm_announce('precip snw sent',2)

        lprec_cc(:,:)=lprec_cc(:,:)/dt_cc*1.e3
        call atm_maxmin(lonr,latd,lprec_cc,'in atm,2 lprec_cc')
        call atm_sendflux(lprec_cc)
        call atm_announce('liquid precip sent',2)

!       sending original hice and fice
!
        call atm_sendflux(fice_sfc_cc)
        call atm_announce('to send fice',2)
        call atm_sendflux(hice_sfc_cc)
        call atm_announce('to send hice',2)
!

        t_bot_cc  = 0.
        u_bot_cc  = 0.
        v_bot_cc  = 0.
        q_bot_cc  = 0.
        p_bot_cc  = 0.
        p_surf_cc = 0.
        z_bot_cc  = 0.
        t_sfc_cc  = 0.
        xmu_cc    = 0.
        dsw_cc    = 0.
        dlw_cc    = 0.
        ffmm_cc   = 0.
        ffhh_cc   = 0.
        snw_cc    = 0.
        lprec_cc  = 0.
       endif
!<-- cpl insertion

      if (lsout_cc_momocn) then
        dusfc_cc  = -dusfc_cc*dta2dto_cc  !chk units, *const*ps may be needed
        dvsfc_cc  = -dvsfc_cc*dta2dto_cc  !chk units, *const*ps may be needed
        dtsfc_cc  =  dtsfc_cc*dta2dto_cc  !chk units, *const*ps may be needed
        dqsfc_cc  =  dqsfc_cc*dta2dto_cc  !chk units, *const*ps may be needed
        dlwsfc_cc =  dlwsfc_cc*dta2dto_cc !-------, *const*ps may be needed
        ulwsfc_cc =  ulwsfc_cc*dta2dto_cc !-------, *const*ps may be needed
        swsfc_cc  = -swsfc_cc*dta2dto_cc  !chk units, *const*ps may be needed
        precr_cc  =  precr_cc/dto_cc      ! assign dt_cc -- om time step
                                ! <- (above, it was "am" instead of
                                ! om in the commentary - apparently
                                ! by mistake or misprint, but it
                                ! resulted in actual assignment of
                                ! am time step to dt_cc)
     &            *1.e3   ! <- don't know why. see treatment of
                         ! geshem in wrtsfc.f, wrtsfc_comm.f (7/16/04)

        call atm_announce('to send fluxes',2)
        call atm_sendflux(dusfc_cc,slmsk=slmsk)
        call atm_announce('x-stress sent',2)
        call atm_sendflux(dvsfc_cc,slmsk=slmsk)
        call atm_announce('y-stress sent',2)

!       dtsfc_cc = dtsfc_cc+dqsfc_cc-dlwsfc_cc+ulwsfc_cc+swsfc_cc
        dtsfc_cc = dtsfc_cc

        call atm_sendflux(dtsfc_cc,slmsk=slmsk)
        call atm_announce('q (net heat flux) sent',2)

!       dqsfc_cc = dqsfc_cc/hvap_cc-precr_cc
        dqsfc_cc = dqsfc_cc/hvap_cc

        call atm_sendflux(dqsfc_cc,slmsk=slmsk)
        call atm_announce('e-p sent',2)
!
        dlwsfc_cc = dlwsfc_cc-ulwsfc_cc

        call atm_sendflux(dlwsfc_cc,slmsk=slmsk)
        call atm_announce('net lwr sent',2)
        call atm_sendflux(swsfc_cc,slmsk=slmsk)
        call atm_announce('net swr sent',2)

!xw     call atm_sendflux(precr_cc,slmsk=slmsk)
!xw     call atm_announce('precip sent',2)
!
        call atm_announce('fluxes sent',2)
        dusfc_cc  = 0.
        dvsfc_cc  = 0.
        dtsfc_cc  = 0.
        dqsfc_cc  = 0.
        precr_cc  = 0.
        dlwsfc_cc = 0.
        ulwsfc_cc = 0.
        swsfc_cc  = 0.
      end if

      contains
!===
      subroutine atm_sendflux(f,slmsk)

      use atm_cc

      use surface_cc, only: islm_rg,
     >kind_sfcflux,kind_slmsk,slm_os_value,
     >unrealistically_low_sv

      implicit none

      real (kind=kind_sfcflux),intent(in) :: f(lonr,latd)
!--> cpl deletion
!      real (kind=kind_slmsk) slmsk(lonr,latd)
!<-- cpl deletion
      real (kind=kind_slmsk),optional,intent(in)  :: slmsk(lonr,latd)

      real(kind=kind_real), dimension(lonr,latd):: f1,f2
      real (kind=kind_real) x(lonf,latg)
      integer kmsk(lonr,latd)
      integer iord /2/
      integer i,j
!     character*40 s
!

      if (coupler_id.lt.0) return    !   <- standalone mode

      f1(:,:)=f(:,:)
      kmsk=islm_rg

!      islm_rg is local (per process) mask array that is
!      constant in time. it contains 0 for either open sea (os) or
!      sea ice (si) and 1 for land (l). keep in mind: it's on reduced g.

!--> cpl insertion
      if ( present(slmsk) ) then
!<-- cpl insertion
      do j=1,latd
      do i=1,lonr
!        if (abs(slmsk(i,j)-slm_os_value).lt.0.01) then
!                                  ! i.e. if it is os (open sea) amgp
        if (abs(slmsk(i,j)-2.).lt.1.e-5              ! sea ice
     >      .or. abs(slmsk(i,j)).lt.1.e-5) then      ! open sea  am
          kmsk(i,j)=0
        else
          kmsk(i,j)=1
        end if
      end do
      end do

      endif

!       slmsk is (per-process-) local mask array regularly updated
!       with sea ice data

      call uninterpred_cc(iord,kmsk,f1,f2)
                ! <- interpolation from reduced grid (i.e. with # of
                ! longitudes varying from lat. circle to lat. circle)
                ! to full grid
!
!      print *,'in send_flux, before assemble_cc'
      call  assemble_cc(x,f2)

!      print *,'in send_flux, testing, x=',x(1:5,1),'f=',f(1:5,1),
!     >   'f1=',f1(1:5,1),'f2=',f2(1:5,1)
      call cmp_send(x,n2d)

      end subroutine atm_sendflux

      end subroutine
! ******************************************************************
       subroutine atm_maxmin(xdim,ydim,x,s)
  
       use atm_cc

       implicit none

       integer xdim,ydim,i,j
       real(kind=kind_real) x(xdim,ydim),xmax,xmin
       character(*) s

      xmax=x(1,1)
      xmin=x(1,1)
      do j=1,ydim
      do i=1,xdim
       if ( xmax .lt. x(i,j) ) xmax=x(i,j)
       if ( xmin .gt. x(i,j) ) xmin=x(i,j)
      enddo
      enddo
!     print *,s//' in atm_maxmin,xdim=',xdim,'ydim=',ydim,
!    >   'xmax=',xmax,'xmin=',xmin

      return
      end
! ******************************************************************
       subroutine atm_maxmin_int(xdim,ydim,x,s)

       use atm_cc

       implicit none

       integer xdim,ydim,i,j
       integer x(xdim,ydim),xmax,xmin
       character(*) s

      xmax=x(1,1)
      xmin=x(1,1)
      do j=1,ydim
      do i=1,xdim
       if ( xmax .lt. x(i,j) ) xmax=x(i,j)
       if ( xmin .gt. x(i,j) ) xmin=x(i,j)
      enddo
      enddo
      print *,s//' in atm_maxmin,xdim=',xdim,'ydim=',ydim,
     >   'xmax=',xmax,'xmin=',xmin

      return
      end


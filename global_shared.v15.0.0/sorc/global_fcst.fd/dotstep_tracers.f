      subroutine do_tstep(deltim,kdt,phour,
     &                    trie_ls,trio_ls,
     &                    ls_node,ls_nodes,max_ls_nodes,
     &                    lats_nodes_a,global_lats_a,
     &                    lonsperlat,
     &                    lats_nodes_r,global_lats_r,
     &                    lonsperlar,
!    &                    lats_nodes_ext,global_lats_ext,
     &                    epse,epso,epsedn,epsodn,
     &                    snnp1ev,snnp1od,ndexev,ndexod,
     &                    plnev_a,plnod_a,pddev_a,pddod_a,
     &                    plnew_a,plnow_a,
     &                    plnev_r,plnod_r,pddev_r,pddod_r,
     &                    plnew_r,plnow_r,
!    &                    syn_gr_a_1,dyn_gr_a_1,anl_gr_a_1,
!    &                    syn_gr_a_2,dyn_gr_a_2,anl_gr_a_2,
     &                    xlon,xlat,coszdg, sfc_fld, flx_fld, nst_fld,
     &                    hprime,swh,hlw,fluxr,sfalb,slag,sdec,cdec,
     &                    ozplin,jindx1,jindx2,ddy,pdryini,
     &                    phy_f3d,  phy_f2d, dyn_f3d, dyn_f2d,
     &                    zhour,n1,n4,lsout,colat1,cfhour1,fscav,
     &                    sps,total_member)
!!
!   program modification log:
!     jan 2013     y-t hou       - added variable deltim in gloopr calling interface
!                        to support better astronomy calculations (coszen) that
!                        enables radiation calling interval in shorter period than
!                        the one-hour limit in early model.
!
      use machine             , only : kind_evod,kind_phys,kind_rad
      use resol_def           , only : latg,latg2,latr,latr2,levh,levs,
     &                                 lonr,lotd,lots,lsoil,nfxr,nmtvr,
     &                                 ntoz,ntrac,ncld,
     &                                 num_p2d,num_p3d,num_a3d,num_a2d,
     &                                 p_di,p_dim,p_q,p_qm,p_rm,p_rq,
     &                                 p_rt,p_te,p_tem,p_uln,p_vln,
     &                                 p_w,p_x,p_y,p_ze,p_zem,p_zq,lonf
      use layout1             , only : ipt_lats_node_r,lats_node_r,
     &                                 len_trie_ls,len_trio_ls,
     &                                 ls_dim,ls_max_node,
     &                                 me,me_l_0,nodes,lats_dim_a,
     .                                 ipt_lats_node_a,lats_node_a
      use vert_def            , only : am,bm,si,sl,sv,tov
      use date_def            , only : fhour,idate,shour,spdmax
      use namelist_def        , only : adiab,ens_nam,fhcyc,filta,
     &                                 gen_coord_hybrid,gg_tracers,
     &                                 hybrid, igen,explicit,mom4ice,
     &                                 ldiag3d,lsfwd,lslwr,lsswr,k2o,
     &                                 ialb,nst_fcst,
     &                                 ngptc,nscyc,nsres,nszer,semilag,
     &                                 sl_epsln,nsout,fhstoch,n3dfercld
      use mpi_def             , only : icolor,kind_mpi,liope,
     &                                 mc_comp,mpi_r_mpi,comp_task
      use ozne_def            , only : latsozp,levozp,pl_coeff,timeoz

!     use layout_grid_tracers , only : rgt_a


      use sfc_flx_esmfmod
      use nst_var_esmfmod
      use d3d_def

      use atm_cc              , only : coupler_id
      use stoch_data          , only : dump_patterns

      implicit none
!!     
      type(sfc_var_data)        :: sfc_fld
      type(flx_var_data)        :: flx_fld
      type(nst_var_data)        :: nst_fld
      character(16)             :: cfhour1
      integer,intent(in)        :: lonsperlat(latg),n1,n4,total_member
!!     
      real(kind=kind_evod),intent(in)    :: deltim,phour
      real(kind=kind_evod),intent(inout) :: zhour

      integer ifirst
      data ifirst /1/
      save ifirst
!
      real, allocatable   :: gzie_ln(:,:),gzio_ln(:,:),factor_b2t_ref(:)
      save gzie_ln,gzio_ln,factor_b2t_ref

      real(kind=kind_evod) trie_ls(len_trie_ls,2,11*levs+3*levh+6)
      real(kind=kind_evod) trio_ls(len_trio_ls,2,11*levs+3*levh+6)
!!
      integer              ls_node(ls_dim,3)
!!
!     ls_node(1,1) ... ls_node(ls_max_node,1) : values of l
!     ls_node(1,2) ... ls_node(ls_max_node,2) : values of jbasev
!     ls_node(1,3) ... ls_node(ls_max_node,3) : values of jbasod
!!
      integer              ls_nodes(ls_dim,nodes)
      integer          max_ls_nodes(nodes)
      integer               lats_nodes_a(nodes)
!     integer               lats_nodes_ext(nodes)
      integer              global_lats_a(latg)
!     integer        global_lats_ext(latg+2*jintmx+2*nypt*(nodes-1))
      integer               lats_nodes_r(nodes)
      integer              global_lats_r(latr)
      integer                 lonsperlar(latr)
!
      real(kind=kind_evod) colat1
      real(kind=kind_evod)    epse(len_trie_ls)
      real(kind=kind_evod)    epso(len_trio_ls)
      real(kind=kind_evod)  epsedn(len_trie_ls)
      real(kind=kind_evod)  epsodn(len_trio_ls)
      real(kind=kind_evod) snnp1ev(len_trie_ls)
      real(kind=kind_evod) snnp1od(len_trio_ls)
      integer               ndexev(len_trie_ls)
      integer               ndexod(len_trio_ls)
      real(kind=kind_evod)   plnev_a(len_trie_ls,latg2)
      real(kind=kind_evod)   plnod_a(len_trio_ls,latg2)
      real(kind=kind_evod)   pddev_a(len_trie_ls,latg2)
      real(kind=kind_evod)   pddod_a(len_trio_ls,latg2)
      real(kind=kind_evod)   plnew_a(len_trie_ls,latg2)
      real(kind=kind_evod)   plnow_a(len_trio_ls,latg2)
      real(kind=kind_evod)   plnev_r(len_trie_ls,latr2)
      real(kind=kind_evod)   plnod_r(len_trio_ls,latr2)
      real(kind=kind_evod)   pddev_r(len_trie_ls,latr2)
      real(kind=kind_evod)   pddod_r(len_trio_ls,latr2)
      real(kind=kind_evod)   plnew_r(len_trie_ls,latr2)
      real(kind=kind_evod)   plnow_r(len_trio_ls,latr2)

!     real(kind=kind_evod) syn_gr_a_1(lonfx*lots,lats_dim_ext)
!     real(kind=kind_evod) dyn_gr_a_1(lonfx*lotd,lats_dim_ext)
!     real(kind=kind_evod) anl_gr_a_1(lonfx*lota,lats_dim_ext)
!     real(kind=kind_evod) syn_gr_a_2(lonfx*lots,lats_dim_ext)
!     real(kind=kind_evod) dyn_gr_a_2(lonfx*lotd,lats_dim_ext)
!     real(kind=kind_evod) anl_gr_a_2(lonfx*lota,lats_dim_ext)
!!     
      real (kind=kind_rad) xlon(lonr,lats_node_r),
     &                     xlat(lonr,lats_node_r),
     &                     coszdg(lonr,lats_node_r),
     &                     hprime(lonr,nmtvr,lats_node_r),
     &                     fluxr(lonr,nfxr,lats_node_r),
     &                     sfalb(lonr,lats_node_r),
     &                     swh(lonr,levs,lats_node_r),
     &                     hlw(lonr,levs,lats_node_r),
     &                     swhc(lonr,levs,lats_node_r),
     &                     hlwc(lonr,levs,lats_node_r)

      real (kind=kind_phys)
     &     phy_f3d(lonr,levs,num_p3d,lats_node_r),
     &     phy_f2d(lonr,num_p2d,lats_node_r),
     &     ddy(lats_node_r), fscav(ntrac-ncld-1)
      real (kind=kind_evod)
     &     dyn_f3d(lonf,levs,num_a3d,lats_node_a),
     &     dyn_f2d(lonf,num_a2d,lats_node_a)
!

      integer jindx1(lats_node_r),jindx2(lats_node_r)
!!     
      real ozplin(latsozp,levozp,pl_coeff,timeoz) !ozone pl coeff
      real (kind=kind_phys) pdryini
      real(kind=kind_evod) slag,sdec,cdec
!
!****************************************************************************
!$$$      integer   p_gz,p_zem,p_dim,p_tem,p_rm,p_qm
!$$$      integer   p_ze,p_di,p_te,p_rq,p_q,p_dlam,p_dphi,p_uln,p_vln
!$$$      integer   p_w,p_x,p_y,p_rt,p_zq
!$$$      parameter(p_gz  = 0*levs+0*levh+1,  !      gze/o(lnte/od,2),
!$$$     x          p_zem = 0*levs+0*levh+2,  !     zeme/o(lnte/od,2,levs),
!$$$     x          p_dim = 1*levs+0*levh+2,  !     dime/o(lnte/od,2,levs),
!$$$     x          p_tem = 2*levs+0*levh+2,  !     teme/o(lnte/od,2,levs),
!$$$     x          p_rm  = 3*levs+0*levh+2,  !      rme/o(lnte/od,2,levh),
!$$$     x          p_qm  = 3*levs+1*levh+2,  !      qme/o(lnte/od,2),
!$$$     x          p_ze  = 3*levs+1*levh+3,  !      zee/o(lnte/od,2,levs),
!$$$     x          p_di  = 4*levs+1*levh+3,  !      die/o(lnte/od,2,levs),
!$$$     x          p_te  = 5*levs+1*levh+3,  !      tee/o(lnte/od,2,levs),
!$$$     x          p_rq  = 6*levs+1*levh+3,  !      rqe/o(lnte/od,2,levh),
!$$$     x          p_q   = 6*levs+2*levh+3,  !       qe/o(lnte/od,2),
!$$$     x          p_dlam= 6*levs+2*levh+4,  !  dpdlame/o(lnte/od,2),
!$$$     x          p_dphi= 6*levs+2*levh+5,  !  dpdphie/o(lnte/od,2),
!$$$     x          p_uln = 6*levs+2*levh+6,  !     ulne/o(lnte/od,2,levs),
!$$$     x          p_vln = 7*levs+2*levh+6,  !     vlne/o(lnte/od,2,levs),
!$$$     x          p_w   = 8*levs+2*levh+6,  !       we/o(lnte/od,2,levs),
!$$$     x          p_x   = 9*levs+2*levh+6,  !       xe/o(lnte/od,2,levs),
!$$$     x          p_y   =10*levs+2*levh+6,  !       ye/o(lnte/od,2,levs),
!$$$     x          p_rt  =11*levs+2*levh+6,  !      rte/o(lnte/od,2,levh),
!$$$     x          p_zq  =11*levs+3*levh+6)  !      zqe/o(lnte/od,2)
!****************************************************************************
      real(kind=kind_evod) batah
      real(kind=kind_mpi)  coef00m(levs,ntrac)! temp. ozone clwater  
      real(kind=kind_evod) coef00(levs,ntrac) ! temp. ozone clwater  
      integer              kdt, ierr,i,j,k,l,locl,n,kp_t,kp_y,
     &                     kp_d,kp_z,kp_x,kp_w,iprint

      integer              indlsev,jbasev
      include 'function_indlsev'

      logical lsout, sps, wrt_g3d
!
      real, parameter:: rlapse=0.65e-2, omz1=10.0
!
! timings
      real(kind=kind_evod) global_times_a(latg,nodes)
     &,                    global_times_b(latr,nodes)
     &,                    global_times_r(latr,nodes)
      real*8               rtc, timer1, timer2, dt_warm, tem1, tem2
!
!     if(ifirst == 1)then
!      allocate ( factor_b2t_ref(levs), gzie_ln(len_trie_ls,2),
!    &            gzio_ln(len_trio_ls,2) )
!      ifirst=0
!     endif
!
      shour = shour + deltim

!-> coupling insertion
      if (comp_task) then
        call atm_dbg2(kdt,phour,zhour,shour,3)
        call atm_tstep_init(kdt)
      endif
!<- coupling insertion

      if (comp_task) then

!     if (me == 0) print *,' in do_tstep semilag=',semilag,' kdt=',kdt

        if (semilag) then    ! joe sela's semi-lagrangian code

!         batah = 0.
!         batah = 1.                   ! commented by moorthi 11/23/2010
          batah = 1.0 + sl_epsln       ! moorthi

          if(ifirst == 1) then
            allocate ( factor_b2t_ref(levs), gzie_ln(len_trie_ls,2),
     &                 gzio_ln(len_trio_ls,2) )
            call get_cd_hyb_slg(deltim,batah)

            k = 0
            call deldifs(
     &                trie_ls(1,1,p_rt+k-1), trie_ls(1,1,p_w+k-1),
     &                trie_ls(1,1,p_qm    ), trie_ls(1,1,p_x+k-1),
     &                trie_ls(1,1,p_y +k-1), trie_ls(1,1,p_tem+k-1),    ! hmhj
     &                trio_ls(1,1,p_rt+k-1), trio_ls(1,1,p_w+k-1),
     &                trio_ls(1,1,p_qm    ), trio_ls(1,1,p_x+k-1),
     &                trio_ls(1,1,p_y +k-1), trio_ls(1,1,p_tem+k-1),    ! hmhj
     &                deltim,sl,ls_node,coef00,k,hybrid,
     &                gen_coord_hybrid)
            if (gg_tracers .and. me == 0) then
              write(0,*)' tracers, including moisture are not diffused'
            endif
            ifirst = 0
          endif
          global_times_a = 0.
          timer1         = rtc()

          call gloopa_hyb_slg
     &      (deltim,trie_ls,trio_ls,gzie_ln,gzio_ln,
     &       ls_node,ls_nodes,max_ls_nodes,
     &       lats_nodes_a,global_lats_a,
     &       lonsperlat,
     &       epse,epso,epsedn,epsodn,
     &       snnp1ev,snnp1od,ndexev,ndexod,
     &       plnev_a,plnod_a,pddev_a,pddod_a,plnew_a,plnow_a,
     &       global_times_a,kdt,batah,lsout)

          timer2 = rtc()

!         if (kdt.lt.4)then
!           print*,' gloopa timer = ',timer2-timer1,' kdt=',kdt
!         endif

          if(.not. adiab) then ! first if.not.adiab
            if (nscyc > 0 .and. mod(kdt,nscyc) == 1) then
!             if (me == 0) print*,' calling gcycle at kdt=',kdt
                call gcycle(me,lats_node_r,lonsperlar,global_lats_r,
     &                      ipt_lats_node_r,idate,phour,fhcyc,
     &                      xlon ,xlat  , sfc_fld, ialb)
            endif
!
            if (n3dfercld >0) then  ! ferrier microphysics initialization
              call init_micro(deltim,lonr,levs,n3dfercld,lats_node_r,
     &                        phy_f3d(:,:,1:n3dfercld,:),   fhour, me)
            endif
          endif              ! first if.not.adiab

!
!-> coupling insertion

! lgetsstice_cc must be defined by this moment. it used to be an argument
! to atm_getsst, accessible here via use surface_cc. now it is defined in
! atm_tstep_init called above, and the use is removed. (even in the earlier
! version lgetsstice_cc did not have to be an actual argumnent, since
! it is in the module surface_cc used by atm_getsst.)

          if (coupler_id >= 0) then
!$omp parallel do private(j,i)
            do j = 1, lats_node_r
              do i = 1, lonr
                if (sfc_fld%slmsk(i,j) == 0 ) then
                  sfc_fld%tsea(i,j) = sfc_fld%tsea(i,j)
     &                 + (sfc_fld%oro(i,j)-sfc_fld%oro_uf(i,j))*rlapse
                endif
              enddo
            enddo
          endif

          call atm_getsstice(sfc_fld%tsea,sfc_fld%tisfc,sfc_fld%fice,
     &                       sfc_fld%hice,sfc_fld%sheleg,sfc_fld%slmsk,
     &                       kdt)

          if (coupler_id >= 0) then
!$omp parallel do private(j,i)
            do j = 1, lats_node_r
              do i = 1, lonr
                if (sfc_fld%slmsk(i,j) == 0 ) then
                  sfc_fld%tsea(i,j) = sfc_fld%tsea(i,j)
     &                 - (sfc_fld%oro(i,j)-sfc_fld%oro_uf(i,j))*rlapse
                endif
              enddo
            enddo
          endif

!<- coupling insertion

!
          if (nst_fcst > 1) then                         ! update tsea
            if (coupler_id < 0 .or. .not. mom4ice) then  ! standalone mode
!$omp parallel do private(j,i,dt_warm)
              do j = 1, lats_node_r
                do i = 1, lonr
                  if (sfc_fld%slmsk(i,j) == 0 ) then
                    dt_warm = (nst_fld%xt(i,j)+nst_fld%xt(i,j))
     &                      /  nst_fld%xz(i,j)
                    nst_fld%tref(i,j) = sfc_fld%tsea(i,j)
     &                   - dt_warm + nst_fld%dt_cool(i,j)
     &                   + (sfc_fld%oro(i,j)-sfc_fld%oro_uf(i,j))*rlapse
                  endif
                enddo
              enddo
            else                                         ! coupled to mom4 om
              tem1 = 0.5 / omz1
!$omp parallel do private(j,i,tem2,dt_warm)
              do j = 1, lats_node_r
                do i = 1, lonr
                  if (sfc_fld%slmsk(i,j) == 0 ) then
                    tem2 = 1.0 / nst_fld%xz(i,j)
                    sfc_fld%tsea(i,j) = sfc_fld%tsea(i,j)
     &                   + (sfc_fld%oro(i,j)-sfc_fld%oro_uf(i,j))*rlapse
                    dt_warm = (nst_fld%xt(i,j)+nst_fld%xt(i,j)) * tem2

                    if ( nst_fld%xz(i,j) > omz1) then
                      nst_fld%tref(i,j) = sfc_fld%tsea(i,j)
     &                 - (1.0-0.5*omz1*tem2) * dt_warm
     &                 + nst_fld%z_c(i,j)*nst_fld%dt_cool(i,j)*tem1
                    else
                     nst_fld%tref(i,j) = sfc_fld%tsea(i,j)
     &                 - (nst_fld%xz(i,j)*dt_warm
     &                 -  nst_fld%z_c(i,j)*nst_fld%dt_cool(i,j))*tem1
                    endif
                    sfc_fld%tsea(i,j) = nst_fld%tref(i,j)
     &                  + dt_warm - nst_fld%dt_cool(i,j)
     &                  - (sfc_fld%oro(i,j)-sfc_fld%oro_uf(i,j))*rlapse
                  endif
                enddo
              enddo
            endif
          endif

          global_times_r = 0.0

          if(.not. adiab) then             ! second  if.not.adiab
            if (lsswr .or. lslwr) then     ! radiation call!
              call gloopr
     &          (trie_ls,trio_ls,
     &           ls_node,ls_nodes,max_ls_nodes,
     &           lats_nodes_a,global_lats_a,
     &           lats_nodes_r,global_lats_r,
     &           lonsperlar,
     &           epse,epso,epsedn,epsodn,
     &           snnp1ev,snnp1od,plnev_r,plnod_r,
     &           pddev_r,pddod_r,
     &           phour,deltim,
     &           xlon,xlat,coszdg,flx_fld%coszen,
     &           sfc_fld%slmsk,sfc_fld%sheleg,sfc_fld%sncovr,
     &           sfc_fld%snoalb,sfc_fld%zorl,sfc_fld%tsea,
     &           hprime,sfalb,sfc_fld%alvsf,sfc_fld%alnsf,
     &           sfc_fld%alvwf,sfc_fld%alnwf,sfc_fld%facsf,
     &           sfc_fld%facwf,sfc_fld%cv,sfc_fld%cvt ,
     &           sfc_fld%cvb,swh,swhc,hlw,hlwc,flx_fld%sfcnsw,
     &           flx_fld%sfcdlw,
     &           sfc_fld%fice,sfc_fld%tisfc,flx_fld%sfcdsw,
     &           flx_fld%sfcemis,                                    ! yth 4/09
     &           flx_fld%tsflw,fluxr,phy_f3d,phy_f2d,
     &           slag,sdec,cdec,kdt,
     &           global_times_r)
!                if (iprint == 1) print*,' me = fin gloopr ',me
            endif                          ! sswr .or. lslwr
          endif                            ! second  if.not.adiab

!     write(0,*)' after gloopr kdt=',kdt
!         if (iprint .eq. 1) print*,' me = beg gloopb ',me
!         if(kdt < 4)then
!           print*,' deltim in if(kdt.lt.4)=',deltim
!         endif

!$omp parallel do private(locl)
          do locl=1,ls_max_node
            call sicdife_hyb_slg(trie_ls(1,1,p_x  ), trie_ls(1,1,p_y ),
     x                         trie_ls(1,1,p_zq ), deltim/2.,
     x                         trie_ls(1,1,p_uln), trie_ls(1,1,p_vln),
     x                         ls_node,snnp1ev,ndexev,locl,batah)
            call sicdifo_hyb_slg(trio_ls(1,1,p_x  ), trio_ls(1,1,p_y ),
     x                         trio_ls(1,1,p_zq ), deltim/2.,
     x                         trio_ls(1,1,p_uln), trio_ls(1,1,p_vln),
     x                         ls_node,snnp1od,ndexod,locl,batah)
          enddo

          do j=1,len_trie_ls
            trie_ls(j,1,p_zq ) = trie_ls(j,1,p_zq)-gzie_ln(j,1)
            trie_ls(j,2,p_zq ) = trie_ls(j,2,p_zq)-gzie_ln(j,2)
!save n-1 values for diffusion, not really part of samilag scheme
            trie_ls(j,1,p_qm ) = trie_ls(j,1,p_zq)
            trie_ls(j,2,p_qm ) = trie_ls(j,2,p_zq)
          enddo
          do j=1,len_trio_ls
            trio_ls(j,1,p_zq ) = trio_ls(j,1,p_zq)-gzio_ln(j,1)
            trio_ls(j,2,p_zq ) = trio_ls(j,2,p_zq)-gzio_ln(j,2)
!save n-1 values for diffusion, not really part of samilag scheme
            trio_ls(j,1,p_qm ) = trio_ls(j,1,p_zq)
            trio_ls(j,2,p_qm ) = trio_ls(j,2,p_zq)
          enddo

!$omp parallel do private(k,kp_t,kp_y,j)
          do k=1,levs
            kp_t = p_tem + k - 1
            kp_y = p_y   + k - 1
            do j=1,len_trie_ls
              trie_ls(j,1,kp_t) = trie_ls(j,1,kp_y)
              trie_ls(j,2,kp_t) = trie_ls(j,2,kp_y)
            enddo
            do j=1,len_trio_ls
              trio_ls(j,1,kp_t) = trio_ls(j,1,kp_y)
              trio_ls(j,2,kp_t) = trio_ls(j,2,kp_y)
            enddo
          enddo
!--------------------------------------------------------
          if ( me  == me_l_0 ) then
            coef00(:,:) = 0.0
            do locl=1,ls_max_node
              l      = ls_node(locl,1)
              jbasev = ls_node(locl,2)
              if ( l  ==  0 ) then
                n = 0
! 1 corresponds to temperature,  2 corresponds to ozon, 3 to clwater
                do k=1,levs
                  coef00(k,1) = trie_ls(indlsev(n,l),1,p_y +k-1)
                enddo
              endif
            end do
            coef00m = coef00
          end if

          call mpi_bcast(coef00m,levs*ntrac,mpi_r_mpi,me_l_0,mc_comp,
     &                                                       ierr)
          coef00 = coef00m
          if( gen_coord_hybrid ) then                                    ! hmhj
            call updown_gc(sl,coef00(1,1))                               ! hmhj
          else                                                           ! hmhj
            call updown(sl,coef00(1,1))
          endif                                                          ! hmhj
          if (gg_tracers) then
!$omp parallel do shared(trie_ls,trio_ls)
!$omp+shared(deltim,sl,ls_node,coef00,hybrid,gen_coord_hybrid)
!$omp+private(k)
            do k=1,levs
              call deldifs_tracers(
     &                trie_ls(1,1,p_rt+k-1), trie_ls(1,1,p_w+k-1),
     &                trie_ls(1,1,p_qm    ), trie_ls(1,1,p_x+k-1),
     &                trie_ls(1,1,p_y +k-1), trie_ls(1,1,p_tem+k-1),
     &                trio_ls(1,1,p_rt+k-1), trio_ls(1,1,p_w+k-1),
     &                trio_ls(1,1,p_qm    ), trio_ls(1,1,p_x+k-1),
     &                trio_ls(1,1,p_y +k-1), trio_ls(1,1,p_tem+k-1),
     &                deltim,sl,ls_node,coef00,k,hybrid,
     &                gen_coord_hybrid)
            enddo
          else
!
!$omp parallel do shared(trie_ls,trio_ls)
!$omp+shared(deltim,sl,ls_node,coef00,hybrid,gen_coord_hybrid)
!$omp+private(k)
            do k=1,levs
              call deldifs(
     &                trie_ls(1,1,p_rt+k-1), trie_ls(1,1,p_w+k-1),
     &                trie_ls(1,1,p_qm    ), trie_ls(1,1,p_x+k-1),
     &                trie_ls(1,1,p_y +k-1), trie_ls(1,1,p_tem+k-1),
     &                trio_ls(1,1,p_rt+k-1), trio_ls(1,1,p_w+k-1),
     &                trio_ls(1,1,p_qm    ), trio_ls(1,1,p_x+k-1),
     &                trio_ls(1,1,p_y +k-1), trio_ls(1,1,p_tem+k-1),
     &                deltim,sl,ls_node,coef00,k,hybrid,
     &                gen_coord_hybrid)
            enddo
          endif
!--------------------------------------------------------
          do j=1,len_trie_ls
            trie_ls(j,1,p_q ) = trie_ls(j,1,p_zq)
            trie_ls(j,2,p_q ) = trie_ls(j,2,p_zq)
          enddo
          do j=1,len_trio_ls
            trio_ls(j,1,p_q ) = trio_ls(j,1,p_zq)
            trio_ls(j,2,p_q ) = trio_ls(j,2,p_zq)
          enddo
!         if (iprint .eq. 1) print*,' me = beg gloopb ',me
          timer1 = rtc()
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! add impadj_slg to gloopb with batah, and set timetsteps to deltim
! add impadj_slg to gloopb with batah, and set timetsteps to deltim
! add impadj_slg to gloopb with batah, and set timetsteps to deltim
! add impadj_slg to gloopb with batah, and set timetsteps to deltim
! add impadj_slg to gloopb with batah, and set timetsteps to deltim
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!     write(0,*)' bef gloopb kdt=',kdt
          global_times_b = 0.0
          if(.not. adiab) then  ! third if.not.adiab
            call gloopb
     &        (trie_ls,trio_ls,
     &         ls_node,ls_nodes,max_ls_nodes,
     &         lats_nodes_a,global_lats_a,
     &         lats_nodes_r,global_lats_r,
     &         lonsperlar,lonsperlat,
     &         epse,epso,epsedn,epsodn,
     &         snnp1ev,snnp1od,ndexev,ndexod,
     &         plnev_r,plnod_r,pddev_r,pddod_r,plnew_r,plnow_r,
     &         deltim,phour,sfc_fld, flx_fld, nst_fld, sfalb,
     &         xlon,
     &         swh,swhc,hlw,hlwc,hprime,slag,sdec,cdec,
     &         ozplin,jindx1,jindx2,ddy,pdryini,
     &         phy_f3d,phy_f2d,
     &         xlat,kdt,global_times_b,batah,lsout,fscav)
          endif            !  third if.not.adiab

!     write(0,*)' aft gloopb kdt=',kdt
!!$omp parallel do shared(trie_ls,ndexev,trio_ls,ndexod)
!!$omp+shared(sl,spdmax,deltim,ls_node)
!         do k=1,levs
!sela       call damp_speed(trie_ls(1,1,p_x+k-1), trie_ls(1,1,p_w +k-1),
!selax                  trie_ls(1,1,p_y+k-1), trie_ls(1,1,p_rt+k-1),
!selax                  ndexev,
!selax                  trio_ls(1,1,p_x+k-1), trio_ls(1,1,p_w +k-1),
!selax                  trio_ls(1,1,p_y+k-1), trio_ls(1,1,p_rt+k-1),
!selax                  ndexod,
!selax                  sl,spdmax(k),deltim,ls_node)
!         enddo

!$omp parallel do private(k,kp_d,kp_z,kp_t,kp_x,kp_w,kp_y,j)
          do k=1,levs
            kp_d = p_di + k - 1
            kp_z = p_ze + k - 1
            kp_t = p_te + k - 1
            kp_x = p_x  + k - 1
            kp_w = p_w  + k - 1
            kp_y = p_y  + k - 1
            do j=1,len_trie_ls
              trie_ls(j,1,kp_d) = trie_ls(j,1,kp_x)
              trie_ls(j,2,kp_d) = trie_ls(j,2,kp_x)
              trie_ls(j,1,kp_z) = trie_ls(j,1,kp_w)
              trie_ls(j,2,kp_z) = trie_ls(j,2,kp_w)
              trie_ls(j,1,kp_t) = trie_ls(j,1,kp_y)
              trie_ls(j,2,kp_t) = trie_ls(j,2,kp_y)
            enddo
            do j=1,len_trio_ls
              trio_ls(j,1,kp_d) = trio_ls(j,1,kp_x)
              trio_ls(j,2,kp_d) = trio_ls(j,2,kp_x)
              trio_ls(j,1,kp_z) = trio_ls(j,1,kp_w)
              trio_ls(j,2,kp_z) = trio_ls(j,2,kp_w)
              trio_ls(j,1,kp_t) = trio_ls(j,1,kp_y)
              trio_ls(j,2,kp_t) = trio_ls(j,2,kp_y)
            enddo
          enddo
          if(.not. gg_tracers)then
!$omp parallel do private(k,kp_x,kp_t,j)
            do k=1,levh
              kp_x = p_rq + k - 1
              kp_t = p_rt + k - 1
              do j=1,len_trie_ls
                trie_ls(j,1,kp_x) = trie_ls(j,1,kp_t)
                trie_ls(j,2,kp_x) = trie_ls(j,2,kp_t)
              enddo
              do j=1,len_trio_ls
                trio_ls(j,1,kp_x) = trio_ls(j,1,kp_t)
                trio_ls(j,2,kp_x) = trio_ls(j,2,kp_t)
              enddo
            enddo
          endif !  if(.not.gg_tracers)
!
!----------------------------------------------------------
        else                          ! eulerian dynamics
!----------------------------------------------------------
!!
          if(ifirst == 1) then
            k = 0
            call deldifs(
     &                trie_ls(1,1,p_rt+k-1), trie_ls(1,1,p_w+k-1),
     &                trie_ls(1,1,p_qm    ), trie_ls(1,1,p_x+k-1),
     &                trie_ls(1,1,p_y +k-1), trie_ls(1,1,p_tem+k-1),    ! hmhj
     &                trio_ls(1,1,p_rt+k-1), trio_ls(1,1,p_w+k-1),
     &                trio_ls(1,1,p_qm    ), trio_ls(1,1,p_x+k-1),
     &                trio_ls(1,1,p_y +k-1), trio_ls(1,1,p_tem+k-1),    ! hmhj
     &                deltim,sl,ls_node,coef00,k,hybrid,
     &                gen_coord_hybrid)

            ifirst = 0
          endif
          global_times_a=0.

!         if(me == 0) then
!           print *,' in do_tstep - eulerian dynamics callling gloopa', &
!    &              ' for kdt=',kdt,', deltim=',deltim,                 &
!    &              ', fhour,phour =',fhour,phour
!         endif

          call gloopa
     &      (deltim,trie_ls,trio_ls,
     &       ls_node,ls_nodes,max_ls_nodes,
     &       lats_nodes_a,global_lats_a,
     &       lonsperlat,
     &       epse,epso,epsedn,epsodn,
     &       snnp1ev,snnp1od,ndexev,ndexod,
     &       plnev_a,plnod_a,pddev_a,pddod_a,plnew_a,plnow_a,
     &       dyn_f3d,dyn_f2d,
     &       global_times_a,kdt)
       
!
!
          iprint = 0
!         if (iprint .eq. 1) print*,' fin gloopa kdt = ',kdt
!
!my gather lat timings for load balancing
!sela     if (reshuff_lats_a .and. kdt .eq. 5) then
!sela       call redist_lats_a(kdt,global_times_a,
!selax                lats_nodes_a,global_lats_a,
!selax                lonsperlat,
!selax                lats_nodes_ext,global_lats_ext,iprint)
!sela     endif
!----------------------------------------------------------

!
          if(.not. adiab) then
            if (nscyc > 0 .and. mod(kdt,nscyc) == 1) then
             call gcycle(me,lats_node_r,lonsperlar,global_lats_r,
     &                  ipt_lats_node_r,idate,phour,fhcyc,
     &                  xlon ,xlat  , sfc_fld, ialb)
            endif
!
            if (n3dfercld >0) then  ! ferrier microphysics initialization
              call init_micro(deltim,lonr,levs,n3dfercld,lats_node_r,
     &                        phy_f3d(:,:,1:n3dfercld,:),   fhour, me)
            endif
          endif
!
!-> coupling insertion

! lgetsstice_cc must be defined by this moment. it used to be an argument
! to atm_getsst, accessible here via use surface_cc. now it is defined in
! atm_tstep_init called above, and the use is removed. (even in the earlier
! version lgetsstice_cc did not have to be an actual argumnent, since
! it is in the module surface_cc used by atm_getsst.)

          if (coupler_id >= 0) then
!$omp parallel do private(j,i)
            do j = 1, lats_node_r
              do i = 1, lonr
                if (sfc_fld%slmsk(i,j) == 0 ) then
                  sfc_fld%tsea(i,j) = sfc_fld%tsea(i,j)
     &                 + (sfc_fld%oro(i,j)-sfc_fld%oro_uf(i,j))*rlapse
                endif
              enddo
            enddo
          endif

!     print *,' am: before atm_getsstice coupler_id=',coupler_id,
!    &' tsea=',sfc_fld%tsea(1,1),' me=',me
          call atm_getsstice(sfc_fld%tsea,sfc_fld%tisfc,sfc_fld%fice,
     &                       sfc_fld%hice,sfc_fld%sheleg,sfc_fld%slmsk,
     &                       kdt)
!     print *,' am: after atm_getsstice coupler_id=',coupler_id,
!    & ' me=',me

          if (coupler_id >= 0) then
!$omp parallel do private(j,i)
            do j = 1, lats_node_r
              do i = 1, lonr
                if (sfc_fld%slmsk(i,j) == 0 ) then
                  sfc_fld%tsea(i,j) = sfc_fld%tsea(i,j)
     &                 - (sfc_fld%oro(i,j)-sfc_fld%oro_uf(i,j))*rlapse
                endif
              enddo
            enddo
          endif

!<- coupling insertion

!
          if (nst_fcst > 1) then                         ! update tsea
            if (coupler_id < 0 .or. .not. mom4ice) then  ! standalone mode
!$omp parallel do private(j,i,dt_warm)
              do j = 1, lats_node_r
                do i = 1, lonr
                  if (sfc_fld%slmsk(i,j) == 0 ) then
                    dt_warm = (nst_fld%xt(i,j)+nst_fld%xt(i,j))
     &                      /  nst_fld%xz(i,j)
                    sfc_fld%tsea(i,j) = nst_fld%tref(i,j)
     &                  + dt_warm - nst_fld%dt_cool(i,j)
     &                  - (sfc_fld%oro(i,j)-sfc_fld%oro_uf(i,j))*rlapse
                  endif
                enddo
              enddo
            else                                         ! coupled to mom4 om
              tem1 = 0.5 / omz1
!$omp parallel do private(j,i,tem2,dt_warm)
              do j = 1, lats_node_r
                do i = 1, lonr
                  if (sfc_fld%slmsk(i,j) == 0 ) then
                    tem2 = 1.0 / nst_fld%xz(i,j)
                    sfc_fld%tsea(i,j) = sfc_fld%tsea(i,j)
     &                   + (sfc_fld%oro(i,j)-sfc_fld%oro_uf(i,j))*rlapse
                    dt_warm = (nst_fld%xt(i,j)+nst_fld%xt(i,j)) * tem2

                    if ( nst_fld%xz(i,j) > omz1) then
                      nst_fld%tref(i,j) = sfc_fld%tsea(i,j)
     &                 - (1.0-0.5*omz1*tem2) * dt_warm
     &                 + nst_fld%z_c(i,j)*nst_fld%dt_cool(i,j)*tem1
                    else
                     nst_fld%tref(i,j) = sfc_fld%tsea(i,j)
     &                 - (nst_fld%xz(i,j)*dt_warm
     &                 -  nst_fld%z_c(i,j)*nst_fld%dt_cool(i,j))*tem1
                    endif
                    sfc_fld%tsea(i,j) = nst_fld%tref(i,j)
     &                  + dt_warm - nst_fld%dt_cool(i,j)
     &                  - (sfc_fld%oro(i,j)-sfc_fld%oro_uf(i,j))*rlapse
                  endif
                enddo
              enddo
            endif
          endif

!           do j = 1, lats_node_r
!             do i = 1, lonr
!               if (sfc_fld%slmsk(i,j) == 0 ) then
!                 dt_warm = (nst_fld%xt(i,j)+nst_fld%xt(i,j))
!    &                    /  nst_fld%xz(i,j)
!                 sfc_fld%tsea(i,j) = sfc_fld%tsea(i,j)
!    &                    + dt_warm - nst_fld%dt_cool(i,j)
!               endif
!             enddo
!           enddo
!         endif


!sela     if (me.eq.0) print*,'completed gloopa in do_tstep'

          global_times_r = 0.0               !my set to zero for every timestep

          if (lsswr .or. lslwr) then         ! radiation call!
            if(.not. adiab) then

!           if(.not.adiab .and. kdt > 1) then
!       if (me == 0) print *,' before calling gloopr kdt=',kdt

              call gloopr
     &          (trie_ls,trio_ls,
     &           ls_node,ls_nodes,max_ls_nodes,
     &           lats_nodes_a,global_lats_a,
     &           lats_nodes_r,global_lats_r,
     &           lonsperlar,
     &           epse,epso,epsedn,epsodn,
     &           snnp1ev,snnp1od,plnev_r,plnod_r,
     &           pddev_r,pddod_r,
     &           phour,deltim,
     &           xlon,xlat,coszdg,flx_fld%coszen,
     &           sfc_fld%slmsk,sfc_fld%sheleg,sfc_fld%sncovr,
     &           sfc_fld%snoalb,sfc_fld%zorl,sfc_fld%tsea,
     &           hprime,sfalb,sfc_fld%alvsf,sfc_fld%alnsf,
     &           sfc_fld%alvwf,sfc_fld%alnwf,sfc_fld%facsf,
     &           sfc_fld%facwf,sfc_fld%cv,sfc_fld%cvt ,
     &           sfc_fld%cvb,swh,swhc,hlw,hlwc,
     &           flx_fld%sfcnsw,flx_fld%sfcdlw,
     &           sfc_fld%fice,sfc_fld%tisfc,flx_fld%sfcdsw,
     &           flx_fld%sfcemis,                                    ! yth 4/09
     &           flx_fld%tsflw,fluxr,phy_f3d,phy_f2d,
     &           slag,sdec,cdec,kdt,
     &           global_times_r)
            endif               ! second  if.not.adiab
          endif  !sswr .or. lslwr

!     if (me == 0) then
!     print *,' aft gloopr hlw45=',hlw(1,:,45)
!     print *,' aft gloopr swh45=',swh(1,:,45)
!     endif
!!
!         if (me ==0)  print *,' finished gloopr at kdt=',kdt
!         call mpi_quit(1111)

          if( .not. explicit ) then                                    ! hmhj
!
            if( gen_coord_hybrid ) then                                 ! hmhj

!$omp parallel do private(locl)
              do locl=1,ls_max_node                                     ! hmhj
                call sicdife_hyb_gc(
     &                       trie_ls(1,1,p_dim), trie_ls(1,1,p_tem),    ! hmhj
     &                       trie_ls(1,1,p_qm ), trie_ls(1,1,p_x  ),    ! hmhj
     &                       trie_ls(1,1,p_y  ), trie_ls(1,1,p_zq ),    ! hmhj
     &                       trie_ls(1,1,p_di ), trie_ls(1,1,p_te ),    ! hmhj
     &                       trie_ls(1,1,p_q  ),deltim,                 ! hmhj
     &                       trie_ls(1,1,p_uln), trie_ls(1,1,p_vln),    ! hmhj
     &                       ls_node,snnp1ev,ndexev,locl)               ! hmhj

                call sicdifo_hyb_gc(
     &                       trio_ls(1,1,p_dim), trio_ls(1,1,p_tem),    ! hmhj
     &                       trio_ls(1,1,p_qm ), trio_ls(1,1,p_x  ),    ! hmhj
     &                       trio_ls(1,1,p_y  ), trio_ls(1,1,p_zq ),    ! hmhj
     &                       trio_ls(1,1,p_di ), trio_ls(1,1,p_te ),    ! hmhj
     &                       trio_ls(1,1,p_q  ),deltim,                 ! hmhj
     &                       trio_ls(1,1,p_uln), trio_ls(1,1,p_vln),    ! hmhj
     &                       ls_node,snnp1od,ndexod,locl)               ! hmhj
              enddo                                                     ! hmhj

            else if(hybrid)then                                         ! hmhj

!         print *,' calling sicdife_hyb at kdt=',kdt
!$omp parallel do private(locl)
              do locl=1,ls_max_node
                call sicdife_hyb(
     &                    trie_ls(1,1,p_dim), trie_ls(1,1,p_tem),
     &                    trie_ls(1,1,p_qm ), trie_ls(1,1,p_x  ),
     &                    trie_ls(1,1,p_y  ), trie_ls(1,1,p_zq ),
     &                    trie_ls(1,1,p_di ), trie_ls(1,1,p_te ),
     &                    trie_ls(1,1,p_q  ),deltim,
     &                    trie_ls(1,1,p_uln), trie_ls(1,1,p_vln),
     &                    ls_node,snnp1ev,ndexev,locl)

                 call sicdifo_hyb(
     &                    trio_ls(1,1,p_dim), trio_ls(1,1,p_tem),
     &                    trio_ls(1,1,p_qm ), trio_ls(1,1,p_x  ),
     &                    trio_ls(1,1,p_y  ), trio_ls(1,1,p_zq ),
     &                    trio_ls(1,1,p_di ), trio_ls(1,1,p_te ),
     &                    trio_ls(1,1,p_q  ),deltim,
     &                    trio_ls(1,1,p_uln), trio_ls(1,1,p_vln),
     &                    ls_node,snnp1od,ndexod,locl)
              enddo

!         print *,' after calling sicdife_hyb at kdt=',kdt
            endif

          endif                 ! not explicit	                 ! hmhj

!
!----------------------------------------------------------
!sela     if (.not.liope.or.icolor.ne.2) then
!sela       print*,'liope=',liope,' icolor=',icolor,' after loopa'
!sela       call rms_spect(trie_ls(1,1,p_zq ), trie_ls(1,1,p_x  ),
!selax             trie_ls(1,1,p_y  ), trie_ls(1,1,p_w  ),
!selax             trie_ls(1,1,p_rt ),
!selax             trio_ls(1,1,p_zq ), trio_ls(1,1,p_x  ),
!selax             trio_ls(1,1,p_y  ), trio_ls(1,1,p_w  ),
!selax             trio_ls(1,1,p_rt ),
!selax             ls_nodes,max_ls_nodes)
!sela     endif
!----------------------------------------------------------

! hmhj compute coef00 for all, even for hybrid mode

          if ( me .eq. me_l_0 ) then
            coef00(:,:) = 0.0
            do locl=1,ls_max_node
              l      = ls_node(locl,1)
              jbasev = ls_node(locl,2)
              if ( l  ==  0 ) then
                n = 0
! 1 corresponds to temperature,  2 corresponds to ozone, 3 to cloud condensate
                do k=1,levs
                  coef00(k,1) = trie_ls(indlsev(n,l),1,p_y +k-1)
                enddo
              endif
            end do
            coef00m = coef00
          end if
          call mpi_bcast(coef00m,levs*ntrac,mpi_r_mpi,me_l_0,mc_comp,
     &                                                       ierr)
          coef00=coef00m
          if( gen_coord_hybrid ) then                                     ! hmhj
            call updown_gc(sl,coef00(1,1))                                ! hmhj
          else                                                            ! hmhj
            call updown(sl,coef00(1,1))
          endif                                                           ! hmhj

!         print *,' calling deldifs at kdt=',kdt
!
!$omp parallel do shared(trie_ls,trio_ls)
!$omp+shared(deltim,sl,ls_node,coef00,hybrid,gen_coord_hybrid)
!$omp+private(k)
          do k=1,levs
            call deldifs(trie_ls(1,1,p_rt+k-1), trie_ls(1,1,p_w+k-1),
     &                   trie_ls(1,1,p_qm    ), trie_ls(1,1,p_x+k-1),
     &                   trie_ls(1,1,p_y +k-1), trie_ls(1,1,p_tem+k-1),   ! hmhj
     &                   trio_ls(1,1,p_rt+k-1), trio_ls(1,1,p_w+k-1),
     &                   trio_ls(1,1,p_qm    ), trio_ls(1,1,p_x+k-1),
     &                   trio_ls(1,1,p_y +k-1), trio_ls(1,1,p_tem+k-1),   ! hmhj
     &                   deltim,sl,ls_node,coef00,k,hybrid,               ! hmhj
     &                   gen_coord_hybrid)                                ! hmhj
          enddo
!         print *,' after calling deldifs at kdt=',kdt
!
!
!-------------------------------------------
          if(.not.lsfwd)then
!-------------------------------------------
            call filtr1eo(trie_ls(1,1,p_tem), trie_ls(1,1,p_te ),
     &                    trie_ls(1,1,p_y  ), trie_ls(1,1,p_dim),
     &                    trie_ls(1,1,p_di ), trie_ls(1,1,p_x  ),
     &                    trie_ls(1,1,p_zem), trie_ls(1,1,p_ze ),
     &                    trie_ls(1,1,p_w  ), trie_ls(1,1,p_rm ),
     &                    trie_ls(1,1,p_rq ), trie_ls(1,1,p_rt ),
     &                    trio_ls(1,1,p_tem), trio_ls(1,1,p_te ),
     &                    trio_ls(1,1,p_y  ), trio_ls(1,1,p_dim),
     &                    trio_ls(1,1,p_di ), trio_ls(1,1,p_x  ),
     &                    trio_ls(1,1,p_zem), trio_ls(1,1,p_ze ),
     &                    trio_ls(1,1,p_w  ), trio_ls(1,1,p_rm ),
     &                    trio_ls(1,1,p_rq ), trio_ls(1,1,p_rt ),
     &                    filta,ls_node)


            do j=1,len_trie_ls
              trie_ls(j,1,p_qm) = trie_ls(j,1,p_q )
              trie_ls(j,2,p_qm) = trie_ls(j,2,p_q )
              trie_ls(j,1,p_q ) = trie_ls(j,1,p_zq)
              trie_ls(j,2,p_q ) = trie_ls(j,2,p_zq)
            enddo
            do j=1,len_trio_ls
              trio_ls(j,1,p_qm) = trio_ls(j,1,p_q )
              trio_ls(j,2,p_qm) = trio_ls(j,2,p_q )
              trio_ls(j,1,p_q ) = trio_ls(j,1,p_zq)
              trio_ls(j,2,p_q ) = trio_ls(j,2,p_zq)
            enddo

!-------------------------------------------
          else
!-------------------------------------------
            do j=1,len_trie_ls
              trie_ls(j,1,p_q) = trie_ls(j,1,p_zq)
              trie_ls(j,2,p_q) = trie_ls(j,2,p_zq)
            enddo
            do j=1,len_trio_ls
              trio_ls(j,1,p_q) = trio_ls(j,1,p_zq)
              trio_ls(j,2,p_q) = trio_ls(j,2,p_zq)
            enddo
!-------------------------------------------
          endif
!
!-------------------------------------------
!         if (iprint .eq. 1) print*,' me = beg gloopb ',me
!my set to zero for every timestep
          global_times_b = 0.0

          if(.not. adiab) then

!        print *,' before calling gloopb kdt=',kdt
            call gloopb
     &        (trie_ls,trio_ls,
     &         ls_node,ls_nodes,max_ls_nodes,
     &         lats_nodes_a,global_lats_a,
     &         lats_nodes_r,global_lats_r,
     &         lonsperlar,lonsperlat,
     &         epse,epso,epsedn,epsodn,
     &         snnp1ev,snnp1od,ndexev,ndexod,
     &         plnev_r,plnod_r,pddev_r,pddod_r,plnew_r,plnow_r,
     &         deltim,phour,sfc_fld, flx_fld, nst_fld, sfalb,
     &         xlon,
     &         swh,swhc,hlw,hlwc,hprime,slag,sdec,cdec,
     &         ozplin,jindx1,jindx2,ddy,pdryini,
     &         phy_f3d,phy_f2d,
     &         xlat,kdt,global_times_b,batah,lsout,fscav)
!
!           if (kdt .eq. 1) call mpi_quit(222)
          endif ! not.adiab

!        print *,' after calling gloopb kdt=',kdt
!
!!$omp parallel do shared(trie_ls,ndexev,trio_ls,ndexod)
!!$omp+shared(sl,spdmax,deltim,ls_node)
!$omp parallel do private(k)
          do k=1,levs
            call damp_speed(trie_ls(1,1,p_x+k-1), trie_ls(1,1,p_w +k-1),
     &                      trie_ls(1,1,p_y+k-1), trie_ls(1,1,p_rt+k-1),
     &                      ndexev,
     &                      trio_ls(1,1,p_x+k-1), trio_ls(1,1,p_w +k-1),
     &                      trio_ls(1,1,p_y+k-1), trio_ls(1,1,p_rt+k-1),
     &                      ndexod,
     &                      sl,spdmax(k),deltim,ls_node)
          enddo
!
!--------------------------------------------
          if(.not. lsfwd)then
!--------------------------------------------
            call filtr2eo(trie_ls(1,1,p_tem), trie_ls(1,1,p_te ),
     &                    trie_ls(1,1,p_y  ), trie_ls(1,1,p_dim),
     &                    trie_ls(1,1,p_di ), trie_ls(1,1,p_x  ),
     &                    trie_ls(1,1,p_zem), trie_ls(1,1,p_ze ),
     &                    trie_ls(1,1,p_w  ), trie_ls(1,1,p_rm ),
     &                    trie_ls(1,1,p_rq ), trie_ls(1,1,p_rt ),
     &                    trio_ls(1,1,p_tem), trio_ls(1,1,p_te ),
     &                    trio_ls(1,1,p_y  ), trio_ls(1,1,p_dim),
     &                    trio_ls(1,1,p_di ), trio_ls(1,1,p_x  ),
     &                    trio_ls(1,1,p_zem), trio_ls(1,1,p_ze ),
     &                    trio_ls(1,1,p_w  ), trio_ls(1,1,p_rm ),
     &                    trio_ls(1,1,p_rq ), trio_ls(1,1,p_rt ),
     &                    filta,ls_node)
!--------------------------------------------
          else
!--------------------------------------------

!$omp parallel do private(k,kp_d,kp_z,kp_t,kp_x,kp_w,kp_y,j)
            do k=1,levs
              kp_d = p_di + k - 1
              kp_z = p_ze + k - 1
              kp_t = p_te + k - 1
              kp_x = p_x  + k - 1
              kp_w = p_w  + k - 1
              kp_y = p_y  + k - 1
              do j=1,len_trie_ls
                trie_ls(j,1,kp_d) = trie_ls(j,1,kp_x)
                trie_ls(j,2,kp_d) = trie_ls(j,2,kp_x)
                trie_ls(j,1,kp_z) = trie_ls(j,1,kp_w)
                trie_ls(j,2,kp_z) = trie_ls(j,2,kp_w)
                trie_ls(j,1,kp_t) = trie_ls(j,1,kp_y)
                trie_ls(j,2,kp_t) = trie_ls(j,2,kp_y)
              enddo
              do j=1,len_trio_ls
                trio_ls(j,1,kp_d) = trio_ls(j,1,kp_x)
                trio_ls(j,2,kp_d) = trio_ls(j,2,kp_x)
                trio_ls(j,1,kp_z) = trio_ls(j,1,kp_w)
                trio_ls(j,2,kp_z) = trio_ls(j,2,kp_w)
                trio_ls(j,1,kp_t) = trio_ls(j,1,kp_y)
                trio_ls(j,2,kp_t) = trio_ls(j,2,kp_y)
              enddo
            enddo
!$omp parallel do private(k,kp_x,kp_t,j)
            do k=1,levh
              kp_x = p_rq + k - 1
              kp_t = p_rt + k - 1
              do j=1,len_trie_ls
                trie_ls(j,1,kp_x) = trie_ls(j,1,kp_t)
                trie_ls(j,2,kp_x) = trie_ls(j,2,kp_t)
              enddo
              do j=1,len_trio_ls
                trio_ls(j,1,kp_x) = trio_ls(j,1,kp_t)
                trio_ls(j,2,kp_x) = trio_ls(j,2,kp_t)
              enddo
            enddo
!--------------------------------------------
          endif
!         if (kdt .eq. 2) call mpi_quit(444)
!!
        endif                   ! if (semilag) then loop
        if (fhour .eq. fhstoch) then   
! time to dump stochastic patterns for next cycle (used in da, typically
! 6 default is -999
           call dump_patterns('stoch_out')
        endif

      endif                     ! if(comp_task) then loop
!
!--------------------------------------------
!--------------------------------------------
      if (lsout) then
!!
!       if (me == 0) then
!         print *,'  in do_tstep - calling wrtout at kdt =',kdt
!         print *,'    fhour, phour, zhour =',fhour,phour,zhour
!       endif

        wrt_g3d = mod(kdt ,nsout) == 0 .or. phour == 0.0
        call wrtout(phour,fhour,zhour,idate,
     &              trie_ls,trio_ls,
     &              sl,si,
     &              ls_node,ls_nodes,max_ls_nodes,
     &              sfc_fld, flx_fld, nst_fld,
     &              fluxr,pdryini,
     &              lats_nodes_r,global_lats_r,lonsperlar,
     &              colat1,cfhour1,pl_coeff,
     &              epsedn,epsodn,snnp1ev,snnp1od,plnev_r,plnod_r,
     &              plnew_r,plnow_r,'SIG.F','SFC.F','FLX.F',wrt_g3d)

!
        if (mod(kdt,nsres) == 0 .and. (.not. sps)) then
!!
          call wrt_restart(trie_ls,trio_ls,
     &         sfc_fld, nst_fld,
     &         si,sl,fhour,idate,
     &         igen,pdryini,
     x         ls_node,ls_nodes,max_ls_nodes,
     &         global_lats_r,lonsperlar,snnp1ev,snnp1od,
     &         global_lats_a,lonsperlat,
     &         phy_f3d, phy_f2d, dyn_f3d, dyn_f2d,
     &         ngptc, adiab, ens_nam,
     &         nst_fcst,'SIGR1','SIGR2','SFCR','NSTR')
!
        endif
!
        if (mod(kdt,nszer) == 0 .and. lsout) then
          call flx_init(flx_fld,ierr)
          zhour = fhour
          fluxr = 0.
!
          if (ldiag3d) then
            call d3d_zero(ldiag3d)
          endif
        endif
      endif
!
! coupling insertion->
      if(total_member == 1) then
        if (comp_task) then
          call atm_sendfluxes(sfc_fld%slmsk)
        endif
      endif
!<- coupling insertion
!     write(0,*)' returning from  do kdt=',kdt

      return
      end

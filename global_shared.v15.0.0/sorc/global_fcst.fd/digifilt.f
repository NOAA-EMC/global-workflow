      subroutine tldfi(deltim,kdt,phour,
     &                 trie_ls,trio_ls,
     &                 ls_node,ls_nodes,max_ls_nodes,
     &                 lats_nodes_a,global_lats_a,
     &                 lonsperlat,
     &                 lats_nodes_r,global_lats_r,
     &                 lonsperlar,
     &                 epse,epso,epsedn,epsodn,
     &                 snnp1ev,snnp1od,ndexev,ndexod,
     &                 plnev_a,plnod_a,pddev_a,pddod_a,
     &                 plnew_a,plnow_a,
     &                 plnev_r,plnod_r,pddev_r,pddod_r,
     &                 plnew_r,plnow_r,
     &                 xlon,xlat,coszdg,sfc_fld,flx_fld,nst_fld,
     &                 hprime,swh,hlw,fluxr,
     &                 sfalb,slag,sdec,cdec,
     &                 ozplin,jindx1,jindx2,
     &                 ddy,pdryini,
     &                 phy_f3d,  phy_f2d,
     &                 dyn_f3d,  dyn_f2d,
     &                 zhour,n1,n4,lsout,colat1,
     &                 cfhour1,fscav,sps,total_member)
       
      use machine         , only : kind_evod,kind_phys,kind_rad
      use resol_def       , only : latg,latg2,latr,latr2,levh,levs,
     &                             lnte,lnto,lonf,lonr,lotd,lots,
     &                             lsoil,nfxr,nmtvr,ntrac,ncld,
     &                             num_p2d,num_p3d,num_a2d,num_a3d,
     &                             p_di,p_dim,p_q,p_qm,p_rm,p_rq,
     &                             p_te,p_tem,p_ze,p_zem
      use layout1         , only : lats_dim_a,lats_node_r,lats_node_a,
     &                             len_trie_ls,len_trio_ls,
     &                             ls_dim,me,nodes
      use layout_grid_tracers , only : rgt_a
      use vert_def            , only : am,bm,si,sl,sv,tov
      use date_def            , only : fhour, idate
      use namelist_def        , only : fhdfi,gen_coord_hybrid,
     &                                 gg_tracers,hybrid,
     &                                 lscca,lsfwd,lslwr,lssav,lsswr,
     &                                 nsdfi,nslwr,nsout,nsswr,nszer,
     &                                 semilag,nsout_hf,fhmax_hf,
     &                                 ldiag3d
      use mpi_def             , only : comp_task
      use ozne_def            , only : latsozp,levozp,pl_coeff,timeoz

!     use resol_def
!     use layout1
      use gg_def
!     use vert_def
!     use date_def
!     use namelist_def
!     use ozne_def
      use sfc_flx_esmfmod
      use nst_var_esmfmod
      use d3d_def
      use stoch_data           , only : dump_patterns,restore_patterns
!
      implicit none
!     include 'mpif.h'      

!!     
      type(sfc_var_data)        :: sfc_fld
      type(flx_var_data)        :: flx_fld
      type(nst_var_data)        :: nst_fld

      character(16)                     :: cfhour1
      integer,intent(in):: lonsperlat(latg),n1,n4
!!     
      real(kind=kind_evod),intent(inout):: deltim,phour,zhour
!!     
      integer i
      real(kind=kind_evod) trie_ls(len_trie_ls,2,11*levs+3*levh+6)
      real(kind=kind_evod) trio_ls(len_trio_ls,2,11*levs+3*levh+6)
      integer              ls_node (ls_dim)
      integer              ls_nodes(ls_dim,nodes)
      integer          max_ls_nodes(nodes)
      integer               lats_nodes_a(nodes)
!     integer               lats_nodes_ext(nodes)
      integer              global_lats_a(latg)
!     integer        global_lats_ext(latg+2*jintmx+2*nypt*(nodes-1))
      integer               lats_nodes_r(nodes)
      integer              global_lats_r(latr)
      integer                 lonsperlar(latr)
!!
      integer, dimension(len_trie_ls) :: ndexev
      integer, dimension(len_trio_ls) :: ndexod

      real(kind=kind_evod) colat1
      real (kind=kind_evod), dimension(len_trie_ls) :: epse, epso,
     &                       epsedn, epsodn, snnp1ev, snnp1od
      real (kind=kind_evod), dimension(len_trie_ls,latg2) ::
     &                       plnev_a, plnod_a, pddev_a, pddod_a,
     &                       plnew_a, plnow_a
      real (kind=kind_evod), dimension(len_trie_ls,latg2) ::
     &                       plnev_r, plnod_r, pddev_r, pddod_r,
     &                       plnew_r, plnow_r

!$$$      integer                lots,lotd,lota
!$$$      parameter            ( lots = 5*levs+1*levh+3 )
!$$$      parameter            ( lotd = 6*levs+2*levh+0 )
!$$$      parameter            ( lota = 3*levs+1*levh+1 )

      real (kind=kind_rad), dimension(lonr,lats_node_r) :: xlon, xlat,
     &                                                     coszdg, sfalb
      real (kind=kind_rad), dimension(lonr,nmtvr,lats_node_r) :: hprime
      real (kind=kind_rad), dimension(lonr,levs,lats_node_r)  :: swh,hlw
      real (kind=kind_rad), dimension(lonr,nfxr,lats_node_r)  :: fluxr

      real (kind=kind_phys)
     &     phy_f3d(lonr,levs,num_p3d,lats_node_r),
     &     phy_f2d(lonr,num_p2d,lats_node_r),
     &     ddy(lats_node_r), fscav(ntrac-ncld-1)
      real (kind=kind_evod)
     &     dyn_f3d(lonf,levs,num_a3d,lats_node_a),
     &     dyn_f2d(lonf,num_a2d,lats_node_a)

      integer, dimension(lats_node_r) ::  jindx1, jindx2
!
      real ozplin(latsozp,levozp,pl_coeff,timeoz) !ozone coeff
      real (kind=kind_phys) pdryini
      real(kind=kind_evod) slag,sdec,cdec

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

      integer   kdt,ierr,j,k,l,n
      integer  idt,mdt,jdt,kdtdfi,total_member
      logical lsout,sps
!
      real ,allocatable ::  qse(:,:)
      real ,allocatable :: dise(:,:,:)
      real ,allocatable ::  zes(:,:,:)
      real ,allocatable ::  tes(:,:,:)
      real ,allocatable :: rqse(:,:,:)
!
      real ,allocatable ::  qso(:,:)
      real ,allocatable :: diso(:,:,:)
      real ,allocatable ::  zos(:,:,:)
      real ,allocatable ::  tos(:,:,:)
      real ,allocatable :: rqso(:,:,:)


      real totsum
      real deltim_loc
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!   following allocs. are for gridded tracers
!
      real ,allocatable :: rgt_s(:,:,:,:)
!
      if (comp_task) then

        allocate( rgt_s(lonf,levs,lats_dim_a,ntrac))

        if (.not.allocated(rgt_a))
     &           allocate (rgt_a(lonf,levs,lats_dim_a,ntrac))

        allocate (  qse(len_trie_ls,2)      )
        allocate ( dise(len_trie_ls,2,levs) )
        allocate (  zes(len_trie_ls,2,levs) )
        allocate (  tes(len_trie_ls,2,levs) )
        allocate ( rqse(len_trie_ls,2,levh) )
!
        allocate (  qso(len_trio_ls,2)      )
        allocate ( diso(len_trio_ls,2,levs) )
        allocate (  zos(len_trio_ls,2,levs) )
        allocate (  tos(len_trio_ls,2,levs) )
        allocate ( rqso(len_trio_ls,2,levh) )

        rgt_a(:,:,:,:) = 0.0
      endif

      if (semilag) then
        deltim_loc = deltim
      else
        deltim_loc = deltim*0.5
      endif

!     write(0,*)' beg tldfi- semilag,deltim_loc = ',semilag,deltim_loc,
!    . 'gg_tracers=',gg_tracers


!     write(0,*)' enter tldfi '
!
!  include first two time levels
!!
      kdtdfi = kdt + nsdfi
      if (comp_task) then
        call dfini(-nsdfi-1  ,nsdfi,
     &   trie_ls(1,1,p_q),trie_ls(1,1,p_di),
     &   trie_ls(1,1,p_ze),trie_ls(1,1,p_te),trie_ls(1,1,p_rq),
     &   trio_ls(1,1,p_q),trio_ls(1,1,p_di),
     &   trio_ls(1,1,p_ze),trio_ls(1,1,p_te),trio_ls(1,1,p_rq),
     &   totsum,qse,dise,zes,tes,rqse,qso,diso,zos,tos,rqso,
     &   rgt_s,gg_tracers)
!!
      call dfini(kdt-kdtdfi,nsdfi,
     &   trie_ls(1,1,p_q),trie_ls(1,1,p_di),
     &   trie_ls(1,1,p_ze),trie_ls(1,1,p_te),trie_ls(1,1,p_rq),
     &   trio_ls(1,1,p_q),trio_ls(1,1,p_di),
     &   trio_ls(1,1,p_ze),trio_ls(1,1,p_te),trio_ls(1,1,p_rq),
     &   totsum,qse,dise,zes,tes,rqse,qso,diso,zos,tos,rqso,
     &   rgt_s,gg_tracers)
      endif

      kdt   = kdt + 1
      fhour = kdt*deltim/3600
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      lssav = .true. !always true, except in digital filter
      lsswr = .true. !ex short wave radaition, used in gloopr(astronomy)
      lslwr = .true. !ex long  wave radaition, used in gloopr(astronomy)
      lsfwd = .true. !true only during forward step
      lscca = .false.!get clouds from precp.(first step use fixio_r clds)
      lsout = mod(kdt,nsout).eq.0 .or. phour.eq.0.
      if (nsout_hf > 0 .and. phour <= fhmax_hf)                         &
     &   lsout = mod(kdt ,nsout_hf) == 0 .or. lsout
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
      if(hybrid)then
        call get_cd_hyb(deltim/2.)
      else if( gen_coord_hybrid ) then
        call get_cd_hyb_gc(deltim/2.)
      endif
!!
      if (me == 0) write(0,*)'kdt before forward step in tldfi=',kdt
      call do_tstep(deltim_loc,kdt,phour,
     &              trie_ls,trio_ls,
     &              ls_node,ls_nodes,max_ls_nodes,
     &              lats_nodes_a,global_lats_a,
     &              lonsperlat,
     &              lats_nodes_r,global_lats_r,
     &              lonsperlar,
     &              epse,epso,epsedn,epsodn,
     &              snnp1ev,snnp1od,ndexev,ndexod,
     &              plnev_a,plnod_a,pddev_a,pddod_a,
     &              plnew_a,plnow_a,
     &              plnev_r,plnod_r,pddev_r,pddod_r,
     &              plnew_r,plnow_r,
     &              xlon,xlat,coszdg,sfc_fld,flx_fld,nst_fld,
     &              hprime,swh,hlw,fluxr,
     &              sfalb,slag,sdec,cdec,
     &              ozplin,jindx1,jindx2,
     &              ddy,pdryini,
     &              phy_f3d,  phy_f2d,
     &              dyn_f3d,  dyn_f2d,
     &              zhour,n1,n4,lsout,colat1,
     &              cfhour1,fscav,.false.,total_member)
!!
      if (me == 0) write(0,*)'kdt after forward step in digifilter=',kdt
!!
      phour = fhour
      if (comp_task) then
        call dfini(kdt-kdtdfi,nsdfi
     &,            trie_ls(1,1,p_q),trie_ls(1,1,p_di)
     &,            trie_ls(1,1,p_ze),trie_ls(1,1,p_te),trie_ls(1,1,p_rq)
     &,            trio_ls(1,1,p_q),trio_ls(1,1,p_di)
     &,            trio_ls(1,1,p_ze),trio_ls(1,1,p_te),trio_ls(1,1,p_rq)
     &,            totsum,qse,dise,zes,tes,rqse,qso,diso,zos,tos,rqso
     &,            rgt_s,gg_tracers)
      endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  include time levels up to hour fhour+fhdfi
!.....
      if(hybrid)then
        call get_cd_hyb(deltim)
      else if( gen_coord_hybrid ) then
        call get_cd_hyb_gc(deltim)
      endif
!.....

      lsfwd = .false.
      lssav = .true.
      idt   = kdt + 1
      mdt   = kdtdfi
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      do jdt=idt,mdt
        kdt=kdt+1
        fhour = kdt*deltim/3600
        lsout = mod(kdt,nsout) == 0
        if (nsout_hf > 0 .and. fhour <= fhmax_hf)                         &
     &  lsout = mod(kdt ,nsout_hf) == 0
        lscca = mod(kdt,nsswr) == 0
        if ( jdt == mdt ) lsout = .false. ! do not write out unfiltered state
        lsswr = mod(kdt,nsswr) == 1
        lslwr = mod(kdt,nslwr) == 1
!
!     if (me == 0) write(0,*)'jdt before do_tstep in digifilter=',jdt
!    &,' kdt=',kdt
        call do_tstep(deltim,kdt,phour,
     &                trie_ls,trio_ls,
     &                ls_node,ls_nodes,max_ls_nodes,
     &                lats_nodes_a,global_lats_a,
     &                lonsperlat,
     &                lats_nodes_r,global_lats_r,
     &                lonsperlar,
     &                epse,epso,epsedn,epsodn,
     &                snnp1ev,snnp1od,ndexev,ndexod,
     &                plnev_a,plnod_a,pddev_a,pddod_a,
     &                plnew_a,plnow_a,
     &                plnev_r,plnod_r,pddev_r,pddod_r,
     &                plnew_r,plnow_r,
     &                xlon,xlat,coszdg,sfc_fld,flx_fld,nst_fld,
     &                hprime,swh,hlw,fluxr,
     &                sfalb,slag,sdec,cdec,
     &                ozplin,jindx1,jindx2,
     &                ddy,pdryini,
     &                phy_f3d,  phy_f2d,
     &                dyn_f3d,  dyn_f2d,
     &                zhour,n1,n4,lsout,colat1,
     &                cfhour1,fscav,.false.,total_member)

!     if (me == 0) write(0,*)'jdt after do_tstep in digifilter=',jdt
!    &,' kdt=',kdt
!!
        if (comp_task) then
          call dfini(kdt-kdtdfi,nsdfi
     &,            trie_ls(1,1,p_q),trie_ls(1,1,p_di)
     &,            trie_ls(1,1,p_ze),trie_ls(1,1,p_te),trie_ls(1,1,p_rq)
     &,            trio_ls(1,1,p_q),trio_ls(1,1,p_di)
     &,            trio_ls(1,1,p_ze),trio_ls(1,1,p_te),trio_ls(1,1,p_rq)
     &,            totsum,qse,dise,zes,tes,rqse,qso,diso,zos,tos,rqso
     &,            rgt_s,gg_tracers)
        endif
        phour = fhour
      enddo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  save surface conditions, nst, and flx files
      if (comp_task) then
       call fixwr(1, nst_fld, sfc_fld, flx_fld)

!  write out state of the stochastic random patterns...
       call dump_patterns('stoch_dfi')
      endif
!......................................................................
!  include time levels up to hour fhour+2*fhdfi
!  but do not save diagnostics for this time
      lssav = .false.
      lsout = .false.
      idt   = kdt + 1
      mdt   = kdt + nsdfi
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      do jdt=idt,mdt
        kdt=kdt+1
        fhour = kdt*deltim/3600
        lscca = mod(kdt,nsswr) == 0
        lsswr = mod(kdt,nsswr) == 1
        lslwr = mod(kdt,nslwr) == 1
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        if (me == 0)
     &  write(0,*) ' calling do_tstep in second loop kdt=',kdt,
     &              ' me=',me,' jdt2=',jdt

        call do_tstep(deltim,kdt,phour,
     &                trie_ls,trio_ls,
     &                ls_node,ls_nodes,max_ls_nodes,
     &                lats_nodes_a,global_lats_a,
     &                lonsperlat,
     &                lats_nodes_r,global_lats_r,
     &                lonsperlar,
     &                epse,epso,epsedn,epsodn,
     &                snnp1ev,snnp1od,ndexev,ndexod,
     &                plnev_a,plnod_a,pddev_a,pddod_a,
     &                plnew_a,plnow_a,
     &                plnev_r,plnod_r,pddev_r,pddod_r,
     &                plnew_r,plnow_r,
     &                xlon,xlat,coszdg,sfc_fld,flx_fld,nst_fld,
     &                hprime,swh,hlw,fluxr,
     &                sfalb,slag,sdec,cdec,
     &                ozplin,jindx1,jindx2,
     &                ddy,pdryini,
     &                phy_f3d,  phy_f2d,
     &                dyn_f3d,  dyn_f2d,
     &                zhour,n1,n4,lsout,colat1,
     &                cfhour1,fscav,.false.,total_member)
!!
!!
        if (comp_task) then
          call dfini(kdt-kdtdfi,nsdfi
     &,            trie_ls(1,1,p_q),trie_ls(1,1,p_di)
     &,            trie_ls(1,1,p_ze),trie_ls(1,1,p_te),trie_ls(1,1,p_rq)
     &,            trio_ls(1,1,p_q),trio_ls(1,1,p_di)
     &,            trio_ls(1,1,p_ze),trio_ls(1,1,p_te),trio_ls(1,1,p_rq)
     &,            totsum,qse,dise,zes,tes,rqse,qso,diso,zos,tos,rqso
     &,            rgt_s,gg_tracers)
         endif
         phour = fhour
!        if (me == 0)  write(0,*)' fhour in second loop of tldfi kdt=',
!    &                        fhour,kdt
      enddo
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!  do final digital filter, set (n-1)=(n), and run forward step in main

      if (comp_task) then
        call dfini(nsdfi+1,nsdfi
     &,            trie_ls(1,1,p_q),trie_ls(1,1,p_di)
     &,            trie_ls(1,1,p_ze),trie_ls(1,1,p_te),trie_ls(1,1,p_rq)
     &,            trio_ls(1,1,p_q),trio_ls(1,1,p_di)
     &,            trio_ls(1,1,p_ze),trio_ls(1,1,p_te),trio_ls(1,1,p_rq)
     &,            totsum,qse,dise,zes,tes,rqse,qso,diso,zos,tos,rqso
     &,            rgt_s,gg_tracers)

        if (me == 0) write(0,*)'kdt after last dfini in digifilter=',kdt
!!
        do i=1,len_trie_ls
          trie_ls(i,1,p_qm) = trie_ls(i,1,p_q)
          trie_ls(i,2,p_qm) = trie_ls(i,2,p_q)
        enddo
        do i=1,len_trio_ls
          trio_ls(i,1,p_qm) = trio_ls(i,1,p_q)
          trio_ls(i,2,p_qm) = trio_ls(i,2,p_q)
        enddo
!
!$omp parallel do private(k,i)
        do k=1,levs
          do i=1,len_trie_ls
            trie_ls(i,1,p_tem+k-1) = trie_ls(i,1,p_te+k-1)
            trie_ls(i,1,p_dim+k-1) = trie_ls(i,1,p_di+k-1)
            trie_ls(i,1,p_zem+k-1) = trie_ls(i,1,p_ze+k-1)
            trie_ls(i,2,p_tem+k-1) = trie_ls(i,2,p_te+k-1)
            trie_ls(i,2,p_dim+k-1) = trie_ls(i,2,p_di+k-1)
            trie_ls(i,2,p_zem+k-1) = trie_ls(i,2,p_ze+k-1)
          enddo
          do i=1,len_trio_ls
            trio_ls(i,1,p_tem+k-1) = trio_ls(i,1,p_te+k-1)
            trio_ls(i,1,p_dim+k-1) = trio_ls(i,1,p_di+k-1)
            trio_ls(i,1,p_zem+k-1) = trio_ls(i,1,p_ze+k-1)
            trio_ls(i,2,p_tem+k-1) = trio_ls(i,2,p_te+k-1)
            trio_ls(i,2,p_dim+k-1) = trio_ls(i,2,p_di+k-1)
            trio_ls(i,2,p_zem+k-1) = trio_ls(i,2,p_ze+k-1)
          enddo
        enddo
!
        if(gg_tracers)then
!         in case tracers need to be moved to other holding area
          write(0,*)' digital filter for grid point tracer not done'
        else
!$omp parallel do private(k,i)
          do k=1,levh
            do i=1,len_trie_ls
              trie_ls(i,1,p_rm+k-1) = trie_ls(i,1,p_rq+k-1)
              trie_ls(i,2,p_rm+k-1) = trie_ls(i,2,p_rq+k-1)
            enddo
            do i=1,len_trio_ls
              trio_ls(i,1,p_rm+k-1) = trio_ls(i,1,p_rq+k-1)
              trio_ls(i,2,p_rm+k-1) = trio_ls(i,2,p_rq+k-1)
            enddo
          enddo
        endif
!!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  replace surface conditions and flx with conditions written at mid ini segment.
!  forward step in main begins with values in the middle of the filter span
!  forward step in main begins with values at time fhdfi
       call fixwr(2, nst_fld, sfc_fld, flx_fld)
       call restore_patterns('stoch_dfi')
      endif
!!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  reset clock and output initialized fields
      kdt   = kdtdfi
      fhour = kdt*deltim/3600 ! note that fhour also comes from last fixio
      if(me == 0) write(0,*)'fhour after reset clock digifilter=',fhour,
     &                       kdt
      lsout = mod(kdt,nsout) == 0
      if (nsout_hf > 0 .and. fhour <= fhmax_hf)                         &
     &  lsout = mod(kdt ,nsout_hf) == 0
!!
!     if (me == 0) then
!       write(*,*)'initialized values in digifilter'
!       write(*,*)'************'
!$$$        call bar3(trie_ls(1,1,p_ze),trio_ls(1,1,p_ze),'ze ',levs)
!$$$        call bar3(trie_ls(1,1,p_di),trio_ls(1,1,p_di),'di ',levs)
!$$$        call bar3(trie_ls(1,1,p_te),trio_ls(1,1,p_te),'te ',levs)
!$$$        call bar3(trie_ls(1,1,p_rq),trio_ls(1,1,p_rq),'rq ',levs)
!$$$        call bar3(trie_ls(1,1,p_rq+levs),trio_ls(1,1,p_rq+levs),
!$$$     &            'oz1 ',levs)
!$$$        call bar3(trie_ls(1,1,p_rq+2*levs),trio_ls(1,1,p_rq+2*levs),
!$$$     &            'oz2 ',levs)
!$$$        call bar3(trie_ls(1,1,p_q),trio_ls(1,1,p_q),'q ',1)
!$$$        call bar3(trie_ls(1,1,p_gz),trio_ls(1,1,p_gz),'gz ',1)
!       print*,'p_qm =',p_qm ,' p_rm =',p_rm 
!sela if (.not.liope.or.icolor.ne.2) then
!$$$        call rms_spect(trie_ls(1,1,p_qm ), trie_ls(1,1,p_dim),
!$$$     x             trie_ls(1,1,p_tem), trie_ls(1,1,p_zem),
!$$$     x             trie_ls(1,1,p_rm ),
!$$$     x             trio_ls(1,1,p_qm ), trio_ls(1,1,p_dim),
!$$$     x             trio_ls(1,1,p_tem), trio_ls(1,1,p_zem),
!$$$     x             trio_ls(1,1,p_rm ),
!$$$     x             ls_nodes,max_ls_nodes)
!sela endif
!---------------------------------------------------------------

!     endif
!!
      phour = fhour

      if (lsout) then
!                                             write out filtered state
!
        call wrtout(phour,fhour,zhour,idate,
     &              trie_ls,trio_ls,
     &              sl,si,
     &              ls_node,ls_nodes,max_ls_nodes,
     &              sfc_fld, flx_fld, nst_fld,
     &              fluxr,pdryini,
     &              lats_nodes_r,global_lats_r,lonsperlar,
     &              colat1,cfhour1,pl_coeff,
     &              epsedn,epsodn,snnp1ev,snnp1od,plnev_r,plnod_r,
     &              plnew_r,plnow_r,'SIG.F','SFC.F','FLX.F',.false.)


        if (mod(kdt,nszer) == 0) then
          call flx_init(flx_fld,ierr)
          zhour = fhour
          fluxr = 0.
!
          if (ldiag3d) then
            call d3d_zero(ldiag3d)
          endif
        endif
      endif

!--------------------------------------------
!my reset digifilter switch to zero for activiation of reshuffling lats loopa
      fhdfi = 0
!
      if (comp_task) then
        deallocate( rgt_s)

        deallocate ( qse  )
        deallocate ( dise )
        deallocate ( zes  )
        deallocate ( tes  )
        deallocate ( rqse )
!
        deallocate ( qso  )
        deallocate ( diso )
        deallocate ( zos  )
        deallocate ( tos  )
        deallocate ( rqso )
      endif
!
      if (me == 0) write(0,*)' exit tldif kdt=',kdt

      return
      end
!!
      subroutine dfini(kstep,nstep,qe,die,ze,te,rqe,
     &                 qo,dio,zo,to,rqo,totsum,
     &                 qse,dise,zes,tes,rqse,qso,diso,zos,tos,rqso,
     &                 rgt_s,gg_tracers)
!
! digital filter initialization (dfini) for model prognostic variables.
! when initial digital filter is first applied, time step counter is set 
! from -nstep to nstep. at -nstep, all saved prognostic variables are
! initialized with a values of zero; while kstep <= nstep, progonstic 
! variable from each time step are accumulated and weigthed by digital 
! filter coefficients. after summing over 2nsteps+1 steps, the saved 
! prognoostic variables are therfore time filtered without high freqiency 
! modes. at the last step, the saved prognostic variables are moved back 
! to normal prognostic variable which should represent value at kstep=0.
!
! kstep		current step count
! nstep		half step count for digital filter period
! *e *o		before totsum are prognostic variables as input
! totsum	current total summation of filter coefficient
! *s* *s	are current saved sum prognostic variables with filter
! gg_tracers	logical to define tracer in grid space or not
!
! henry juang	oroginal code developer (1994?)
!
!
      use resol_def           , only : levh,levs,lnte,lnto,lonf,ntrac
      use layout1             , only : lats_dim_a,lats_node_a,
     &                                 len_trie_ls,len_trio_ls,me
      use layout_grid_tracers , only : rgt_a
      implicit none
!
       real rgt_s(lonf,levs,lats_dim_a,ntrac)
!

!
      real qe(len_trie_ls,2),die(len_trie_ls,2,levs),
     &     ze(len_trie_ls,2,levs),
     &     te(len_trie_ls,2,levs),rqe(len_trie_ls,2,levh)
      real qo(len_trio_ls,2),dio(len_trio_ls,2,levs),
     &     zo(len_trio_ls,2,levs),
     &     to(len_trio_ls,2,levs),rqo(len_trio_ls,2,levh)
!!
      integer len_trie,len_trio
      real digfil,sx,wx,totsumi
      real qse(len_trie_ls,2),dise(len_trie_ls,2,levs),
     &     zes(len_trie_ls,2,levs),
     &     tes(len_trie_ls,2,levs),rqse(len_trie_ls,2,levh)
      real qso(len_trio_ls,2),diso(len_trio_ls,2,levs),
     &     zos(len_trio_ls,2,levs),
     &     tos(len_trio_ls,2,levs),rqso(len_trio_ls,2,levh)
      integer levl
!     save totsum,qse,dise,zes,tes,rqse,qso,diso,zos,tos,rqso
!
      real totsum
      integer i,k,nstep,kstep,lan,nt
      logical gg_tracers

      if (me == 0) write(0,*)' enter dfini ','kstep=',kstep,
     &                     ' nstep=',nstep

      if(kstep < -nstep) then !++++++++++++++++++++++++++++++++++
        totsum = 0
        do i=1,len_trie_ls
          qse(i,1)    = 0.
          qse(i,2)    = 0.
        enddo
        do i=1,len_trio_ls
          qso(i,1)    = 0.
          qso(i,2)    = 0.
        enddo
!$omp parallel do private(k,i)
        do k=1,levs
          do i=1,len_trie_ls
            zes(i,1,k)    = 0.
            zes(i,2,k)    = 0.
            dise(i,1,k)   = 0.
            dise(i,2,k)   = 0.
            tes(i,1,k)    = 0.
            tes(i,2,k)    = 0.
          enddo
          do i=1,len_trio_ls
            zos(i,1,k)    = 0.
            zos(i,2,k)    = 0.
            diso(i,1,k)   = 0.
            diso(i,2,k)   = 0.
            tos(i,1,k)    = 0.
            tos(i,2,k)    = 0.
          enddo
        enddo
!$omp parallel do private(k,i)
        do k=1,levh
          do i=1,len_trie_ls
            rqse(i,1,k)   = 0.
            rqse(i,2,k)   = 0.
          enddo
          do i=1,len_trio_ls
            rqso(i,1,k)   = 0.
            rqso(i,2,k)   = 0.
          enddo
        enddo
        rgt_s  = 0.
        return
      endif
!
      if(kstep <= nstep) then !++++++++++++++++++++++++++++++
!sela  print*,'arrived at elseif(kstep.le.nstep)',ktstep
        if(kstep /= 0) then  !--------------------------------
          sx     = acos(-1.)*kstep/nstep
          wx     = acos(-1.)*kstep/(nstep+1)
          digfil = sin(wx)/wx*sin(sx)/sx
          if(me == 0)then
            write(0,*)'in dfini sx=',sx,'wx=',wx,'digfil=',digfil,
     &      'at kstep=',kstep
          endif
        else                 !--------------------------------
!sela     print*,'arrived at if(kstep.ne.0) in elseif(kstep.le.nstep),
!sela                 ktstep= ntstep=',ktstep,ntstep
          sx     = acos(-1.)*kstep/nstep
          wx     = acos(-1.)*kstep/(nstep+1)
          digfil=1
          if(me == 0)then
            write(0,*)'in dfini sx=',sx,'wx=',wx,'digfil=',digfil,
     &      'at kstep=',kstep
          endif
        endif                !--------------------------------

        totsum = totsum + digfil
        do i=1,len_trie_ls
          qse(i,1) = qse(i,1) + digfil*qe(i,1)
          qse(i,2) = qse(i,2) + digfil*qe(i,2)
        enddo
        do i=1,len_trio_ls
          qso(i,1) = qso(i,1) + digfil*qo(i,1)
          qso(i,2) = qso(i,2) + digfil*qo(i,2)
        enddo
!$omp parallel do private(k,i)
        do k=1,levs
          do i=1,len_trie_ls
            dise(i,1,k) = dise(i,1,k) + digfil*die(i,1,k)
            dise(i,2,k) = dise(i,2,k) + digfil*die(i,2,k)
            zes(i,1,k)  = zes(i,1,k)  + digfil*ze(i,1,k)
            zes(i,2,k)  = zes(i,2,k)  + digfil*ze(i,2,k)
            tes(i,1,k)  = tes(i,1,k)  + digfil*te(i,1,k)
            tes(i,2,k)  = tes(i,2,k)  + digfil*te(i,2,k)
          enddo
          do i=1,len_trio_ls
            diso(i,1,k) = diso(i,1,k) + digfil*dio(i,1,k)
            diso(i,2,k) = diso(i,2,k) + digfil*dio(i,2,k)
            zos(i,1,k)  = zos(i,1,k)  + digfil*zo(i,1,k)
            zos(i,2,k)  = zos(i,2,k)  + digfil*zo(i,2,k)
            tos(i,1,k)  = tos(i,1,k)  + digfil*to(i,1,k)
            tos(i,2,k)  = tos(i,2,k)  + digfil*to(i,2,k)
          enddo
        enddo

        if(gg_tracers)then

          if (me == 0) write(0,*)' kstep=',kstep,' digfil=',digfil,
     & ' totsum=',totsum,' in dfini for gg_tracers'
          do nt=1,ntrac
            do lan=1,lats_node_a   !sela begin lan loop
!$omp parallel do private(k,i)
              do k=1,levs
                do i=1,lonf
                  rgt_s(i,k,lan,nt) = rgt_s(i,k,lan,nt)
     &                              + digfil*rgt_a(i,k,lan,nt)
                enddo
              enddo
            enddo  ! lan loop
          enddo  ! nt loop

        else

!$omp parallel do private(k,i)
          do k=1,levh
            do i=1,len_trie_ls
              rqse(i,1,k) = rqse(i,1,k) + digfil*rqe(i,1,k)
              rqse(i,2,k) = rqse(i,2,k) + digfil*rqe(i,2,k)
            enddo
            do i=1,len_trio_ls
              rqso(i,1,k) = rqso(i,1,k) + digfil*rqo(i,1,k)
              rqso(i,2,k) = rqso(i,2,k) + digfil*rqo(i,2,k)
            enddo
          enddo
!
        endif   ! if(gg_tracers)then
        return
      endif !++++++++++++++++++++++++++++++++++++++++++++++++++++

!sela  print*,'arrived at (kstep.lt.-nstep) in dfini
!sela& ktstep= ntstep=',ktstep,ntstep

      totsumi = 1.0 / totsum

      if (me == 0) write(0,*)' totsum=',totsum,' totsumi=',totsumi

      do i=1,len_trie_ls
        qe(i,1)  = qse(i,1) * totsumi
        qe(i,2)  = qse(i,2) * totsumi
      enddo
      do i=1,len_trio_ls
        qo(i,1)  = qso(i,1) * totsumi
        qo(i,2)  = qso(i,2) * totsumi
      enddo
!$omp parallel do private(k,i)
      do k=1,levs
        do i=1,len_trie_ls
          die(i,1,k) = dise(i,1,k) * totsumi
          die(i,2,k) = dise(i,2,k) * totsumi
           ze(i,1,k) =  zes(i,1,k)  * totsumi
           ze(i,2,k) =  zes(i,2,k)  * totsumi
           te(i,1,k) =  tes(i,1,k)  * totsumi
           te(i,2,k) =  tes(i,2,k)  * totsumi
        enddo
        do i=1,len_trio_ls
          dio(i,1,k) = diso(i,1,k) * totsumi
          dio(i,2,k) = diso(i,2,k) * totsumi
           zo(i,1,k) =  zos(i,1,k)  * totsumi
           zo(i,2,k) =  zos(i,2,k)  * totsumi
           to(i,1,k) =  tos(i,1,k)  * totsumi
           to(i,2,k) =  tos(i,2,k)  * totsumi
        enddo
      enddo

      if(gg_tracers)then
        do nt=1,ntrac
          do lan=1,lats_node_a   !sela begin lan loop
!$omp parallel do private(k,i)
            do k=1,levs
              do i=1,lonf
                rgt_a(i,k,lan,nt) = rgt_s(i,k,lan,nt)*totsumi
              enddo
            enddo
          enddo  ! lan loop
        enddo  ! nt loop
        if (me == 0) then
          write(0,*)' rgt_a=',rgt_a(1,:,1,1)
        endif
      else
!$omp parallel do private(k,i)
        do k=1,levh
          do i=1,len_trie_ls
            rqe(i,1,k) = rqse(i,1,k) * totsumi
            rqe(i,2,k) = rqse(i,2,k) * totsumi
          enddo
          do i=1,len_trio_ls
            rqo(i,1,k) = rqso(i,1,k) * totsumi
            rqo(i,2,k) = rqso(i,2,k) * totsumi
          enddo
        enddo
      endif ! if(gg_tracers)then

      if (me == 0) write(0,*)' leave dfini '                            ! hmhj
      end

!***********************************************************************
      subroutine fixwr(iflag,nst_fld,sfc_fld,flx_fld)
!***********************************************************************
!     purpose: save or retrieve fixed fields in digifilt
!     fanglin yang, march 2014: added flx, simplified using derived type fields.
!***********************************************************************
      use resol_def
      use layout1
      use sfc_flx_esmfmod
      use nst_var_esmfmod
      implicit none
      integer iflag, ierr
      type(sfc_var_data)        :: sfc_fld
      type(flx_var_data)        :: flx_fld
      type(nst_var_data)        :: nst_fld
      type(sfc_var_data)        :: sfc_flds
      type(flx_var_data)        :: flx_flds
      type(nst_var_data)        :: nst_flds

      logical first
      data first/.true./
      save  first, sfc_flds, flx_flds, nst_flds
      if (first) then
        call sfcvar_aldata(lonr,lats_node_r,lsoil,sfc_flds,ierr)
        call flxvar_aldata(lonr,lats_node_r,flx_flds,ierr)
        call nstvar_aldata(lonr,lats_node_r,nst_flds,ierr)
        call flx_init(flx_flds,ierr)
        first = .false.
      endif
      if(iflag == 1) then
        sfc_flds = sfc_fld
        nst_flds = nst_fld
        flx_flds = flx_fld
      elseif(iflag == 2) then
        sfc_fld = sfc_flds
        nst_fld = nst_flds
        flx_fld = flx_flds
      endif

      return
      end

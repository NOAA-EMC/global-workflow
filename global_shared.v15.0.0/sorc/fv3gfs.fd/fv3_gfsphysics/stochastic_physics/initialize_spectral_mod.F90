! !module: stochy_initialize_spectral
!          --- initialize module of the
!              gridded component of the stochastic physics patteern
!              generator, which is in spectral space
!
! !description: gfs dynamics gridded component initialize module.
!
! !revision history:
!
!  oct 11  2016 P.Pegion   copy of gsm/dynamics to create stand alone version
!
! !interface:
!
      module initialize_spectral_mod
!
!!uses:
!
      use machine
      use spectral_layout,      only : ipt_lats_node_a, lats_node_a_max,lon_dim_a,len_trie_ls,len_trio_ls &
                                      ,nodes,ls_max_node,lats_dim_a,ls_dim,nodes_comp,lat1s_a
      use stochy_layout_lag, only : lat1s_h
      use stochy_internal_state_mod
      use spectral_layout,only:lon_dims_a
      use stochy_resol_def
      use stochy_namelist_def
      use fv_mp_mod, only : is_master
      use stochy_gg_def, only : wgt_a,sinlat_a,coslat_a,colrad_a,wgtcs_a,rcs2_a,lats_nodes_h,global_lats_h
      use mpp_mod
#ifndef IBM
      USE omp_lib
#endif

      implicit none

      contains

      subroutine initialize_spectral(gis_stochy, rc)

! this subroutine set up the internal state variables,
! allocate internal state arrays for initializing the gfs system.
!----------------------------------------------------------------
!
      implicit none
!
!      type(stochy_internal_state), pointer, intent(inout) :: gis_stochy
      type(stochy_internal_state), intent(inout) :: gis_stochy
      integer,                                    intent(out)   :: rc
      integer           :: ierr, npe_single_member, iret,latghf
      integer           :: num_parthds_stochy
      integer           :: i, j, k, l, n, locl
      logical           :: file_exists=.false.
      integer, parameter :: iunit=101

!-------------------------------------------------------------------

! set up gfs internal state dimension and values for dynamics etc
!-------------------------------------------------------------------
      print*,'before allocate lonsperlat,',&
                   allocated(gis_stochy%lonsperlat),'latg=',latg
!
      gis_stochy%nodes=mpp_npes()
      print*,'mpp_npes=',mpp_npes()
      nodes  = gis_stochy%nodes
      npe_single_member = gis_stochy%npe_single_member


      lon_dim_a = lon_s + 2
      jcap=ntrunc
      jcap1  = jcap+1 
      jcap2  = jcap+2 
      latg   = lat_s
      latg2  = latg/2 
      lonf   = lon_s
      lnt    = jcap2*jcap1/2 
      lnuv   = jcap2*jcap1 
      lnt2   = lnt  + lnt 
      lnt22  = lnt2 + 1 
      lnte   = (jcap2/2)*((jcap2/2)+1)-1 
      lnto   = (jcap2/2)*((jcap2/2)+1)-(jcap2/2) 
      lnted  = lnte 
      lntod  = lnto 

      gis_stochy%lnt2 = lnt2

      allocate(lat1s_a(0:jcap))
      allocate(lon_dims_a(latg))

      allocate(wgt_a(latg2))
      allocate(wgtcs_a(latg2))
      allocate(rcs2_a(latg2))

!!      create io communicator and comp communicator
!!
      nodes_comp=nodes
!
      if (is_master()) then
        print*,'number of threads is',num_parthds_stochy()
        print*,'number of mpi procs is',nodes
      endif
!
      ls_dim = (jcap1-1)/nodes+1
      print*,'allocating lonsperlat',latg
      allocate(gis_stochy%lonsperlat(latg))
      print*,'size=',size(gis_stochy%lonsperlat)
      

      inquire (file="lonsperlat.dat", exist=file_exists)
      if ( .not. file_exists ) then
        !call mpp_error(FATAL,'Requested lonsperlat.dat  data file does not exist')
         gis_stochy%lonsperlat(:)=lonf
      else
        open (iunit,file='lonsperlat.dat',status='old',form='formatted',      &
                                          action='read',iostat=iret)
        if (iret /= 0) call mpp_error(FATAL,'error while reading lonsperlat.dat')
        rewind iunit
        read (iunit,*,iostat=iret) latghf,(gis_stochy%lonsperlat(i),i=1,latghf)
        if (latghf+latghf /= latg) then
           write(0,*)' latghf=',latghf,' not equal to latg/2=',latg/2
           if (iret /= 0) call mpp_error(FATAL,'lonsperlat file has wrong size')
        endif
        do i=1,latghf
          gis_stochy%lonsperlat(latg-i+1) = gis_stochy%lonsperlat(i)
        enddo
        close(iunit)
      endif
!!
!cxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
!
       write(0,*)'before allocate ls_nodes,',allocated(gis_stochy%ls_nodes),&
       'ls_dim=', ls_dim,'nodes=',nodes
      allocate (      gis_stochy%ls_node (ls_dim*3) )
      allocate (      gis_stochy%ls_nodes(ls_dim,nodes) )
      allocate (  gis_stochy%max_ls_nodes(nodes) )
!
      allocate (  gis_stochy%lats_nodes_a_fix(nodes))     ! added for mGrid
!
      allocate (  gis_stochy%lats_nodes_a(nodes) )
      allocate ( gis_stochy%global_lats_a(latg) )
!
      allocate (   gis_stochy%lats_nodes_ext(nodes) )
      allocate ( gis_stochy%global_lats_ext(latg+2*jintmx+2*nypt*(nodes-1)) )

! internal parallel structure.   Weiyu.
!---------------------------------------------------
      ALLOCATE(gis_stochy%TRIE_LS_SIZE      (npe_single_member))
      ALLOCATE(gis_stochy%TRIO_LS_SIZE      (npe_single_member))
      ALLOCATE(gis_stochy%TRIEO_LS_SIZE     (npe_single_member))
      ALLOCATE(gis_stochy%LS_MAX_NODE_GLOBAL(npe_single_member))
      ALLOCATE(gis_stochy%LS_NODE_GLOBAL    (LS_DIM*3, npe_single_member))

      gis_stochy%LS_NODE_GLOBAL     = 0
      gis_stochy%LS_MAX_NODE_GLOBAL = 0
      gis_stochy%TRIEO_TOTAL_SIZE   = 0

      DO i = 1, npe_single_member
          CALL GET_LS_NODE_STOCHY(i-1, gis_stochy%LS_NODE_GLOBAL(1, i),               &
                            gis_stochy%LS_MAX_NODE_GLOBAL(i), gis_stochy%IPRINT)
          gis_stochy%TRIE_LS_SIZE(i) = 0
          gis_stochy%TRIO_LS_SIZE(i) = 0
          DO LOCL = 1, gis_stochy%LS_MAX_NODE_GLOBAL(i)
              gis_stochy%LS_NODE_GLOBAL(LOCL+  LS_DIM, i)   = gis_stochy%TRIE_LS_SIZE(i)
              gis_stochy%LS_NODE_GLOBAL(LOCL+  2*LS_DIM, i) = gis_stochy%TRIO_LS_SIZE(i)

              L = gis_stochy%LS_NODE_GLOBAL(LOCL, i)

              gis_stochy%TRIE_LS_SIZE(i) = gis_stochy%TRIE_LS_SIZE(i) + (JCAP+3-L)/2
              gis_stochy%TRIO_LS_SIZE(i) = gis_stochy%TRIO_LS_SIZE(i) + (JCAP+2-L)/2
          END DO
          gis_stochy%TRIEO_LS_SIZE(i) = gis_stochy%TRIE_LS_SIZE(i)  + gis_stochy%TRIO_LS_SIZE(i) + 3
          gis_stochy%TRIEO_TOTAL_SIZE = gis_stochy%TRIEO_TOTAL_SIZE + gis_stochy%TRIEO_LS_SIZE(i)
      END DO


!---------------------------------------------------
!
      gis_stochy%iprint = 0
      call get_ls_node_stochy( gis_stochy%me, gis_stochy%ls_node, ls_max_node, gis_stochy%iprint )
!
!
      len_trie_ls = 0
      len_trio_ls = 0
      do locl=1,ls_max_node
         gis_stochy%ls_node(locl+  ls_dim) = len_trie_ls
         gis_stochy%ls_node(locl+2*ls_dim) = len_trio_ls
         l = gis_stochy%ls_node(locl)
         len_trie_ls = len_trie_ls+(jcap+3-l)/2
         len_trio_ls = len_trio_ls+(jcap+2-l)/2
      enddo
      if (gis_stochy%me == 0) print *,'ls_node=',gis_stochy%ls_node(1:ls_dim),'2dim=',  &
         gis_stochy%ls_node(ls_dim+1:2*ls_dim),'3dim=',  &
         gis_stochy%ls_node(2*ls_dim+1:3*ls_dim)
!
!
      allocate ( gis_stochy%epse  (len_trie_ls) )
      allocate ( gis_stochy%epso  (len_trio_ls) )
      allocate ( gis_stochy%epsedn(len_trie_ls) )
      allocate ( gis_stochy%epsodn(len_trio_ls) )
!
      allocate ( gis_stochy%snnp1ev(len_trie_ls) )
      allocate ( gis_stochy%snnp1od(len_trio_ls) )
!
      allocate ( gis_stochy%plnev_a(len_trie_ls,latg2) )
      allocate ( gis_stochy%plnod_a(len_trio_ls,latg2) )
      allocate ( gis_stochy%pddev_a(len_trie_ls,latg2) )
      allocate ( gis_stochy%pddod_a(len_trio_ls,latg2) )
      allocate ( gis_stochy%plnew_a(len_trie_ls,latg2) )
      allocate ( gis_stochy%plnow_a(len_trio_ls,latg2) )

      allocate(colrad_a(latg2))
      allocate(sinlat_a(latg))
      allocate(coslat_a(latg))
      allocate(lat1s_h(0:jcap))
!
      if(gis_stochy%iret/=0) call mpp_error(FATAL,'incompatible namelist - aborted in stochy')
!!
      gis_stochy%lats_nodes_ext = 0
      call getcon_spectral(gis_stochy%ls_node,         gis_stochy%ls_nodes,           &
                           gis_stochy%max_ls_nodes,    gis_stochy%lats_nodes_a,       &
                           gis_stochy%global_lats_a,   gis_stochy%lonsperlat,         &
                           gis_stochy%lats_node_a_max, gis_stochy%lats_nodes_ext,     &
                           gis_stochy%global_lats_ext, gis_stochy%epse,               &
                           gis_stochy%epso,            gis_stochy%epsedn,             &
                           gis_stochy%epsodn,          gis_stochy%snnp1ev,            &
                           gis_stochy%snnp1od,         gis_stochy%plnev_a,            &
                           gis_stochy%plnod_a,         gis_stochy%pddev_a,            &
                           gis_stochy%pddod_a,         gis_stochy%plnew_a,            &
                           gis_stochy%plnow_a,         gis_stochy%colat1)
!
      gis_stochy%lats_node_a     = gis_stochy%lats_nodes_a(gis_stochy%me+1)
      gis_stochy%ipt_lats_node_a = ipt_lats_node_a

      if (gis_stochy%me == 0)                                                        &
       write(0,*)'after getcon_spectral lats_node_a=',gis_stochy%lats_node_a &
         ,'ipt_lats_node_a=',gis_stochy%ipt_lats_node_a
!
        if (.not. allocated(lats_nodes_h))  allocate (lats_nodes_h(nodes))
        if (.not. allocated(global_lats_h)) allocate (global_lats_h(latg+2*gis_stochy%yhalo*nodes))
        call getcon_lag_stochy(gis_stochy%lats_nodes_a,gis_stochy%global_lats_a,        &
                        lats_nodes_h, global_lats_h,                       &
                        gis_stochy%lonsperlat,gis_stochy%xhalo,gis_stochy%yhalo)

!
!
      allocate ( gis_stochy%trie_ls (len_trie_ls,2,lotls) )
      allocate ( gis_stochy%trio_ls (len_trio_ls,2,lotls) )

      if (gis_stochy%me == 0) then
        print*, ' lats_dim_a=', lats_dim_a, ' lats_node_a=', gis_stochy%lats_node_a
      endif  
      rc=0

      end subroutine initialize_spectral
end module initialize_spectral_mod

!!!!!  ==========================================================  !!!!!
!!!!!                        file  descriptions                    !!!!!
!!!!!  ==========================================================  !!!!!
!                                                                      !
!   a collection of subprograms and a module for model data output.    !
!                                                                      !
!   contents:                                                          !
!      module: mod_state                                               !
!                                                                      !
!      subroutine: wrtout, wrt_restart, wrtlog, shapeset,              !
!                  sfc_collect, nst_collect, sfc_only_move,            !
!                  nst_only_move, sfc_wrt, nst_wrt, wrtflx_a,          !
!                  wrtflx_w, grids_move, spect_collect.                !
!                                                                      !
!   program history log:                                               !
!     mmm-yyyy  g. vandengerghe - created program wrtout               !
!      -   --   joe sela     - modified, set lfnhr to false for        !
!                 writing one step output etc.                         !
!      -   --   sarah lu     - added smc(3:4), stc(3:4), slc(1:4),     !
!                 snwdph, canopy, changed 10-200cm to 10-40cm for      !
!                 smc(2), and changed 10-200 to 10-40 for stc(2).      !
!      -   --   jun wang     - modified to add spfhmax/spfhmin.        !
!     nov 2004  xingren wu   - modified to add sea-ice fields.         !
!     oct 2006  helin wei    - modified to add 30 records for land mdl.!
!                 monitoring and analysis purpose.                     !
!     nov 2007  ho-chun huang- code change for gocart, added lggfs3d   !
!                 and wrt3gd_hyb for gocart only g3d files output.     !
!                 also confirmed by helin, correct bug, zorl unit in   !
!                 cm not mm, when callng subuninterprez, array glolal  !
!                 is assign to buff_mult_piecea at the ngrid location, !
!                 then ngrid advanced by 1.  before assign the modified!
!                 value (buffo) to buff_mult_piecea again dial ngrid   !
!                 back by 1 for the correct ngrid index.               !
!     sep 2008  yu-tai hou   - add sunshine duration time to flux file.!
!         2009  sarah lu     - added 7 clear-sky radiation racords.    !
!      -   --   hc huang     - added control flag wrt_g3d for output   !
!                 g3d calls.                                           !
!      -   --   s. moorthi   - a multi-year continuation to improve    !
!                 the support of requests for adding output fields and !
!                 model upgrades, including changing fields, logical   !
!                 controls, grib data conversions, and incorporating   !
!                 new developments, etc.                               !
!     apr 2012  yu-tai hou   - added 4 sw sfc flux components vis/nir  !
!                 beam/diffused. in subprograms wrtflx_a and wrtflx_w, !
!                 the written out fields have been re-organized that   !
!                 related fields are closer together, and reduced      !
!                 duplications of field labels.                        !
!     jan 2013  s. moorthi   - modified scale factor for spfhmax and   !
!                 spfhmin fields with additional variable ids_iq to    !
!                 avoid conflict with fields dnwd sw/lw (idswf/idlwf). !
!     mar 2013  yu-tai hou   - modified scale factor for sfc-uvb and   !
!                 sfc-csuvb fields with additional variable ids_uvb to !
!                 avoid conflict with field canopy water evap (ievcw). !
!     jul 2013  ruiyu sun    - modified for pdf cloud                  !
!     sep 2013  fanglin yang - added restart fields for dynamics and   !
!                 restructured physics restart fiels.                  !
!                                                                      !
!!!!!  ==========================================================  !!!!!
!!!!!                       end descriptions                       !!!!!
!!!!!  ==========================================================  !!!!!


!========================================!
      module mod_state
!........................................!
! -------------------------------------------------------------------- !
!    new module to supply domain information to the gfs output routines!
!    called by wrtout.                                                 !
! -------------------------------------------------------------------- !
!
      use machine
      use resol_def
      use gfsio_module
      use gfsio_def
      use mpi_def, only: liope
!
      implicit none
!
      real (kind=kind_io4), allocatable :: buff_mult_pieceg(:,:,:),     &
     &                                     buff_mult_piecesg(:,:,:,:)
      real (kind=kind_io4), allocatable :: buff_mult_piece(:,:,:),      &
     &                                     buff_mult_pieces(:,:,:,:)
      real (kind=kind_io4), allocatable :: buff_mult_piecef(:,:,:),     &
     &                                     buff_mult_piecesf(:,:,:,:)
      real (kind=kind_io4), allocatable :: buff_mult_pieceo(:,:,:),     &
     &                                     buff_mult_pieceso(:,:,:,:)
      real (kind=kind_io4), allocatable :: buff_mult_piecea(:,:,:),     &
     &                                     buff_mult_piecesa(:,:,:,:)

      integer, allocatable :: ivar_global(:), ivar_global_a(:,:),       &
     &                        ivarg_global(:), ivarg_global_a(:,:)
      integer, allocatable :: maskss(:,:,:)
      integer :: ngrid ,ngrida, ngridg

      save ngrid,ngrida,buff_mult_piece,buff_mult_pieces,ivar_global,   &
     &     ngridg,buff_mult_pieceg,buff_mult_piecesg,ivarg_global,      &
     &     buff_mult_pieceo
      save maskss
!
!........................................!
      end module mod_state
!========================================!


!-----------------------------------
      subroutine wrtout                                                 &
!...................................
!  ---  inputs/outputs:
     &     ( phour,fhour,zhour,idate,trie_ls,trio_ls,                   &
     &       sl,si,ls_node,ls_nodes,max_ls_nodes,                       &
     &       sfc_fld,flx_fld,nst_fld,fluxr,pdryini,                     &
     &       lats_nodes_r,global_lats_r,lonsperlar,                     &
     &       colat1,cfhour1,pl_coeff,epsedn,epsodn,                     &
     &       snnp1ev,snnp1od,plnev_r,plnod_r,plnew_r,plnow_r,           &
     &       sigf,sfcf,flxf,wrt_g3d )

! =================   subprogram documentation block   ================ !
!                                                                       !
!    this program writes out surface flux file in grib form.            !
!                                                                       !
!    usage:        call wrtout                                          !
!                                                                       !
!    subprograms called:                                                !
!      spect_tv_enthalpy_ps, shapeset, bar3, spect_collect,             !
!      sfc_collect, nst_collect, grid_collect, spect_to_grid,           !
!      atmgg_move, grids_move, sfc_only_move, nst_only_move,            !
!      wrt3d_hyb, wrt3d, wrtlog, wrtsfc,                                !
!      wrtflx_a, wrtflx_w, spect_write, atmgg_wrt, sfc_wrt, nst_wrt,    !
!      sigio_rwopen, sigio_rclose, baopenwt, baclose,                   !
!      mpi_barrier, mpi_finalize, mpi_gather.                           !
!                                                                       !
!    attributes:                                                        !
!      language:   fortran 90                                           !
!                                                                       !
!    external modules referenced:                                       !
!      'module resol_def'            in 'resol_def.f'                   !
!      'module layout1'              in 'layout1.f'                     !
!      'module sig_io'               in 'sig_io.f'                      !
!      'module namelist_def'         in 'namelist_def.f'                !
!      'module mpi_def'              in 'mpi_def.f.f'                   !
!      'module sigio_module'         in 'sigio_module.f.                !
!      'module sigio_r_module'       in 'sigio_r_module.f'              !
!      'module nstio_module'         in 'nstio_module.f'                !
!      'module sfc_flx_esmfmod'      in 'sfc_var_esmfmod.f'             !
!      'module nst_var_esmfmod'      in 'nst_var_esmfmod.f'             !
!                                                                       !
!  ====================  defination of variables  ====================  !
!                                                                       !
!    input variables:                                            size   !
!      phour         : kind_evod, time for previous time step      1    !
!      fhour         : kind_evod, forecast time in hr              1    !
!      zhour         : kind_evod, accumulator reset time in hr     1    !
!      idate         : integer, init cond time (h,m,d,y)           4    !
!      sl/si         : kind_evod, sigma layer/interface      levs/levp1 !
!      ls_node       : integer,                               (ls_dim,3)!
!      ls_nodes      : integer,                           (ls_dim,nodes)!
!      max_ls_nodes  : integer,                                  nodes  !
!      sfc_fld       : sfc_var_data type, surface related fields        !
!      flx_fld       : flx_var_data type, rediation related fields      !
!      nst_fld       : nst_var_data type, nst related fields            !
!      fluxr         : real, time accum radiation related fields        !
!                                                (nfxr,lonr,lats_node_r)!
!      pdryini       : real,                                       1    !
!      lats_nodes_r  : integer,                                  nodes  !
!      global_lats_r : integer, index for global latitudes        latr  !
!      lonsperlar    : integer, num of pts on a given lat circle  latr  !
!      colat1        : real, colatitude                            1    !
!      pl_coeff      : integer,                                    1    !
!      epsedn        : kind_evod,                           len_trie_ls !
!      epsodn        : kind_evod,                           len_trio_ls !
!      snnp1ev       : kind_evod,                           len_trie_ls !
!      snnp1od       : kind_evod,                           len_trio_ls !
!      plnev_r       : kind_evod,                    (len_trie_ls,latr2)!
!      plnov_r       : kind_evod,                    (len_trio_ls,latr2)!
!      plnew_r       : kind_evod,                    (len_trie_ls,latr2)!
!      plnow_r       : kind_evod,                    (len_trio_ls,latr2)!
!      sigf/sfcf/flxf: character,                                  5    !
!      wrt_g3d       : logical,                                    1    !
!                                                                       !
!    input/output variables:                                     size   !
!      trie_ls/trio_ls:kind_evod,                                       !
!                                       (len_trio_ls,2,11*levs+3*levh+6)!
!                                                                       !
!    output variables:                                           size   !
!      cfhour1       : character, for esmf export state creation  16    !
!                                                                       !
!  ======================  end of definations  =======================  !
!
      use resol_def
      use layout1
      use sig_io
      use namelist_def
      use mpi_def
      use sigio_module
      use sigio_r_module
      use nstio_module
      use sfc_flx_esmfmod
      use nst_var_esmfmod
!
      implicit none
!
!  ---  inputs:
      integer, intent(in) :: idate(4), pl_coeff
      integer, intent(in) :: ls_node(ls_dim,3), ls_nodes(ls_dim,nodes)

      integer, dimension(nodes), intent(in) :: max_ls_nodes,lats_nodes_r
      integer, dimension(latr),  intent(in) :: global_lats_r, lonsperlar

      real (kind=kind_evod), intent(in) :: phour, fhour, zhour
      real (kind=kind_evod), intent(in) :: sl(levs), si(levp1)

      real (kind=kind_evod), dimension(len_trie_ls) :: epsedn, snnp1ev
      real (kind=kind_evod), dimension(len_trio_ls) :: epsodn, snnp1od

      real (kind=kind_evod), dimension(len_trie_ls,latr2), intent(in):: &
     &                       plnev_r, plnew_r
      real (kind=kind_evod), dimension(len_trio_ls,latr2), intent(in):: &
     &                       plnod_r, plnow_r

      real (kind=kind_io8), intent(in) :: pdryini, colat1
      real (kind=kind_io8), intent(in) :: fluxr(lonr,nfxr,lats_node_r)

      type (sfc_var_data), intent(in) :: sfc_fld
      type (flx_var_data), intent(in) :: flx_fld
      type (nst_var_data), intent(in) :: nst_fld

      character*5,  intent(in) :: sigf, sfcf, flxf
      logical, intent(in) :: wrt_g3d

!  ---  inputs/outputs:
      real (kind=kind_evod), intent(inout) ::                           &
     &                       trie_ls(len_trie_ls,2,11*levs+3*levh+6),   &
     &                       trio_ls(len_trio_ls,2,11*levs+3*levh+6)

!  ---  outputs: (none)
      character, intent(out) :: cfhour1(16)

!  ---  locals:
      integer :: ixgr, k, kh, kk, km, ks, iostat, no3d, nog3d, lenrec,  &
     &           ierr, direction, ndig, ndigyr, ioproc, iret, nosig,    &
     &           nosfc, noflx, nonst, nfill, jdate(4), ixga, i

!  ---  locals for iau:
      integer :: jdate_iau(4),idat(8),jdat(8)
      real (kind=kind_evod) :: zhour_iau,fhour_iau
      real :: rinc(5)

      logical :: lfnhr, convert_temp_ps

      character*16 :: cosfc, const
      character*40 :: cfhour, cform

      real (kind=kind_evod) :: secphy, secswr, seclwr
      real (kind=kind_rad), dimension(lonr,lats_node_r)      :: psg
      real (kind=kind_rad), dimension(lonr,lats_node_r,levs) :: uug,vvg,&
     &                                                          teg,dpg
      real (kind=kind_rad), dimension(lonr,lats_node_r,levh) :: rqg

      real (kind=kind_mpi),allocatable :: trieo_ls_nodes_buf(:,:,:,:,:)
      real (kind=kind_mpi),allocatable :: trieo_ls_node(:,:,:)
!
      real (kind=kind_evod), allocatable :: trie_te(:,:,:),             &
     &                                      trio_te(:,:,:),             &
     &                                      trie_q(:,:),                &
     &                                      trio_q(:,:)

      save trieo_ls_nodes_buf, trieo_ls_node

!     real (kind=kind_io8), dimension(lonr,lats_node_r) :: geshem

      real (kind=kind_io8) :: t1, t2, t3, t4, ta, tb, tc, td, te, tf,   &
     &                        tba, tbb, tbd, rtc, timesum
      data timesum / 0. /
      integer nfillnum
!
!===> ...  begin here
!
!     print *,' in wrtout me=',me

      nosig = 61
      nosfc = 62
      noflx = 63
      nonst = 65

!  --- ...  convert to virtual temperature and lnps if need

      convert_temp_ps = .false.

      if ( gen_coord_hybrid .and. out_virttemp
     &                      .and. .not. gfsio_out) then

!       t3 = rtc()
        convert_temp_ps = .true.

        if (comp_task) then
!   --- ...  keep enthalpy and ps variables before write
           allocate (trie_q(len_trie_ls,2), trio_q(len_trio_ls,2))
           allocate (trie_te(len_trie_ls,2,levs),                       &
     &               trio_te(len_trio_ls,2,levs))

!$omp parallel do private(k,kk)
          do k = 1, levs
            kk = p_te + k - 1
            do i=1,len_trie_ls
              trie_te(i,1,k) = trie_ls(i,1,kk)
              trie_te(i,2,k) = trie_ls(i,2,kk)
            enddo
            do i=1,len_trio_ls
              trio_te(i,1,k) = trio_ls(i,1,kk)
              trio_te(i,2,k) = trio_ls(i,2,kk)
            enddo
          enddo
          do i=1,len_trie_ls
            trie_q(i,1) = trie_ls(i,1,p_q)
            trie_q(i,2) = trie_ls(i,2,p_q)
          enddo
          do i=1,len_trio_ls
            trio_q(i,1) = trio_ls(i,1,p_q)
            trio_q(i,2) = trio_ls(i,2,p_q)
          enddo

          direction = -1          ! from (enthalpy,ps) to (virttemp,lnps)
          call spect_tv_enthalpy_ps( direction,
     &        trie_ls(1,1,p_q ), trio_ls(1,1,p_q ),
     &        trie_ls(1,1,p_te), trio_ls(1,1,p_te),
     &        trie_ls(1,1,p_rq), trio_ls(1,1,p_rq),
     &        ls_node,ls_nodes,max_ls_nodes,
     &        lats_nodes_r,global_lats_r,lonsperlar,
     &        plnev_r,plnod_r,plnew_r,plnow_r )

        endif     ! end if com_task block
!
        thermodyn_id = 1
        sfcpress_id  = 1

!       t4 = rtc()
!       print *,' time spent on spect_tv_enthalpy is ',t4-t3

      endif     ! ( gen_coord_hybrid... )

!     t3 = rtc()
      call mpi_barrier(mpi_comm_all,ierr)

!     t4 = rtc()
!     tba = t4 - t3
      if (nodes_comp<1 .or. nodes_comp>nodes) then
        print *, '  nodes_comp undefined, cannot do i.o '

        call mpi_finalize()
        stop 333
      endif

      ioproc = nodes_comp - 1
      if (liope) ioproc = nodes_comp

      if (.not. allocated(trieo_ls_node)) then
        allocate ( trieo_ls_node (len_trie_ls_max+len_trio_ls_max,
     &                            2, 3*levs+1*levh+1 ) )
      endif

!     t3 = rtc()
      call shapeset (ls_nodes,max_ls_nodes,pdryini)
      call mpi_barrier(mpi_comm_all,ierr)

!     t4 = rtc()
!     tbb = t4 - t3
       
      if (.not. allocated(trieo_ls_nodes_buf))then
        allocate( trieo_ls_nodes_buf (len_trie_ls_max+len_trio_ls_max,
     &                                2, 3*levs+1*levh+1, nodes,1 ) )
      endif
!     t1 = rtc()

      if (me == 0) then
        write(*,*)'in wrtout fhour=',fhour

        call bar3(trie_ls(1,1,p_q),trio_ls(1,1,p_q),'q  ',1)
      endif

!  --- ...  create cfhour

      jdate = idate
      ndigyr = 4
      if (ndigyr == 2) then
        jdate(4) = mod(idate(4)-1,100) + 1
      endif
      if ( iau .and. fhour >= 6.0 ) then
        idat = 0
        idat(1) = idate(4)
        idat(2) = idate(2)
        idat(3) = idate(3)
        idat(5) = idate(1)
        rinc = 0.
        rinc(2) = 6.
        call w3movdat(rinc,idat,jdat)
        jdate_iau(4) = jdat(1)
        jdate_iau(2) = jdat(2)
        jdate_iau(3) = jdat(3)
        jdate_iau(1) = jdat(5)
        fhour_iau = fhour - 6.0
        zhour_iau = zhour - 6.0
        if ( zhour_iau < 0.0 ) zhour_iau = 0.0
      else
        jdate_iau = jdate
        fhour_iau = fhour
        zhour_iau = zhour
      endif

!  --- ...  set lfnhr to false for writing one step output etc.

      lfnhr = .true.    ! no output
      lfnhr = (3600*abs(fhour-nint(fhour))<=.1 .or. phour==0.)
      if (lfnhr) then
        kh = nint(fhour)
        ndig = max(log10(kh+0.5)+1.,2.)
        write(cform,'("(i",i1,".",i1,")")') ndig,ndig
        write(cfhour,cform) kh
      else
        ks = nint(fhour*3600)
        kh = ks / 3600
        km = (ks - kh*3600) / 60
        ks = ks - kh*3600 - km*60
        ndig = max(log10(kh+0.5)+1.,2.)
        write(cform,'("(i",i1,".",i1,",a1,i2.2,a1,i2.2)")') ndig,ndig
        write(cfhour,cform) kh,':',km,':',ks
      endif

      nfillnum=nfill(ens_nam)      
      cfhour = cfhour(1:nfill(cfhour)) // ens_nam(1:nfill(ens_nam))
      if (me == 0) then
         if(nfillnum > 0) then
           print *,' in wrtout cfhour=',cfhour,
     &                     ' ens_nam=',ens_nam
         else
           print *,' in wrtout cfhour=',cfhour
         end if
      end if

!     t3 = rtc()
      call mpi_barrier(mpi_comm_all,ierr)

!     t4  = rtc()
!     tbd = t4 - t3
!     t3  = rtc()

      secphy = (fhour - zhour) * 3600.
!     secswr = max(secphy,fhswr*3600.)
!     seclwr = max(secphy,fhlwr*3600.)
      secswr = max(secphy,fhswr)
      seclwr = max(secphy,fhlwr)

!  --- ...  build state on each node.   comp tasks only assemble spectral
!           state first then sfc state, then (only if liope) flux state.

!     t3 = rtc()
      if (mc_comp /= mpi_comm_null) then
        if (.not. gfsio_out) then
          call spect_collect(
     &         trie_ls(1,1,p_zq), trie_ls(1,1,p_q ), trie_ls(1,1,p_te),
     &         trie_ls(1,1,p_di), trie_ls(1,1,p_ze), trie_ls(1,1,p_rq),
     &         trie_ls(1,1,p_gz),
     &         trio_ls(1,1,p_zq), trio_ls(1,1,p_q ), trio_ls(1,1,p_te),
     &         trio_ls(1,1,p_di), trio_ls(1,1,p_ze), trio_ls(1,1,p_rq),
     &         trio_ls(1,1,p_gz), trieo_ls_node )
        else
          call spect_to_grid(
     &         trie_ls(1,1,p_q ), trio_ls(1,1,p_q ), trie_ls(1,1,p_di),
     &         trio_ls(1,1,p_di), trie_ls(1,1,p_ze), trio_ls(1,1,p_ze),
     &         trie_ls(1,1,p_te), trio_ls(1,1,p_te), trie_ls(1,1,p_rq),
     &         trio_ls(1,1,p_rq), sfc_fld%oro,psg,uug,vvg,teg,rqg,dpg,
     &         ls_node, ls_nodes, max_ls_nodes, lats_nodes_r,
     &         global_lats_r, lonsperlar, epsedn, epsodn,
     &         snnp1ev, snnp1od, plnev_r, plnod_r )
        endif

        if (.not. adiab) then
          call sfc_collect(sfc_fld,global_lats_r,lonsperlar)

          if ( nst_fcst > 0 ) then
            call nst_collect(nst_fld,global_lats_r,lonsperlar)
          endif

          if (liope) then
!  --- ...  collect flux grids as was done with sfc grids above, but only if
!           liope is true.  if false, the fluxes are handled by the original
!           wrtsfc predating the i/o task updates.

            call wrtflx_a(ioproc,noflx,zhour_iau,fhour_iau,jdate_iau,
     &                    colat1,secswr,seclwr,sfc_fld,flx_fld,fluxr,
     &                    global_lats_r,lonsperlar )
          endif   ! liope
        endif     ! not adiab
      endif       ! comp node

!     t4 = rtc()
!     td = t4 - t3

!  --- ...  done with state build
!           now state is assembled on each node.  get everything off the
!           compute nodes (currently done with a send to the i/o task_
!           send state to i/o task.  all tasks

      if (.not. gfsio_out) then
!       t3 = rtc()
        lenrec = (len_trie_ls_max+len_trio_ls_max)*2*(3*levs+1*levh+1)

!       call mpi_gather( trieo_ls_node, lenrec, mpi_r_mpi,
        call mpi_gathe4( trieo_ls_node, lenrec, mpi_r_mpi,
     &                   trieo_ls_nodes_buf(1,1,1,1,1), lenrec,
     &                   mpi_r_mpi, ioproc, mpi_comm_all, ierr )
        call mpi_barrier(mpi_comm_all,ierr)
      else
        call grid_collect( sfc_fld%oro, psg,uug,vvg,teg,rqg,dpg,
     &                     global_lats_r,lonsperlar)
        call atmgg_move(ioproc)
      endif

      if (.not. adiab) then
        if (liope) then    ! move all grids (fluxes and sfc)
          call grids_move(ioproc )
        else               ! move sfc grids only,  handle fluxes in original wrtsfc
          call sfc_only_move(ioproc)

          if ( nst_fcst > 0 ) then
            call nst_only_move(ioproc)

            write(*,*) ' nst_only_move done'
          endif

          if (me == ioproc) then
            call baopenwt(noflx,flxf//cfhour,iostat)
          endif

          call wrtsfc
     &     (ioproc,noflx,zhour_iau,fhour_iau,jdate_iau,colat1,secswr,
     &      seclwr,sfc_fld,flx_fld,fluxr,global_lats_r,lonsperlar)
        endif   !  liope

!       t4 = rtc()
!       te = t4 - t3

        if (ldiag3d) then
          no3d = 64
          if (icolor==2 .and. me==ioproc) then
            call baopenwt(no3d,'d3d.f'//cfhour,iostat)
          endif

!         print *,' pl_coeff bef call wrt3d_hyb=',pl_coeff
          call wrt3d_hyb(ioproc,no3d,zhour_iau,fhour_iau,jdate_iau,
     &                   colat1,global_lats_r,lonsperlar,pl_coeff,
     &                   secswr,seclwr,sfc_fld%slmsk,flx_fld%psurf)
        endif    ! ldiag3d

      endif     ! not.adiab

!  --- ...  ioproc only

      cfhour1 = cfhour         ! for the esmf export state creation
!     ta = rtc()

      if (me == ioproc) then
        cform = sigf//cfhour

        if (.not. gfsio_out) then           ! write using sigio
          call sigio_rwopen(nosig,cform,iret)

          ixgr = 0
          ixga = 0
          if (me == 0) print *,' calling spect_write fhour=',fhour

          call spect_write(nosig,ioproc,fhour_iau,jdate_iau,sl,si,
     &                     pdryini,ls_nodes,max_ls_nodes,global_lats_r,
     &                     lonsperlar,trieo_ls_nodes_buf,ixgr)

          call sigio_rclose(nosig,iret)

          if (me == 0) print *,' return from spect_write fhour=',
     &      fhour,' iret=',iret
        else                               ! write using gfsio
          if (me == 0) print *,' calling atmgg_wrt fhour=',fhour,
     &      ' cform=',cform,' idate=',idate

          call atmgg_wrt(ioproc,cform,fhour_iau,jdate_iau,
     &                   global_lats_r,lonsperlar,pdryini)

          if (me == 0) print *,' returning fromatmgg_wrt=',fhour
        endif

        if (.not. adiab) then
          if (liope) call baopenwt(noflx,flxf//cfhour,iostat)

!  --- ...  now write the surface file

          cosfc = sfcf//cfhour
          call sfc_wrt(ioproc,nosfc,cosfc,fhour_iau,jdate_iau,
     &                 global_lats_r,lonsperlar)

          if ( nst_fcst > 0 ) then            !  now write the ocean file
            const = 'nst.f'//cfhour

            call nst_wrt(ioproc,nonst,const,fhour_iau,jdate_iau,
     &                   global_lats_r,lonsperlar)
!                                           !  ocean write done
          endif    ! if ( nst_fcst > 0 )
        endif      ! not.adiab
      endif        ! if_me==ioproc
!
!     tc = rtc()
!     if (me == 0) t2 = rtc()
!     t3 = rtc()

      if (mc_comp /= mpi_comm_null) then
        call mpi_barrier(mc_comp,info)
      endif

!     t4 = rtc()
      if (.not. adiab) then
        if (liope) then                     !  write the fluxes
          if (me == ioproc) then
            call wrtflx_w(ioproc,noflx,zhour_iau,fhour_iau,jdate_iau,
     &                    colat1,secswr,seclwr,sfc_fld%slmsk,
     &                    global_lats_r,lonsperlar )
          endif
        endif
      endif
!  --- ...  flux write done

      if (me == ioproc) then
        if (.not. adiab) then
          call baclose(noflx,iostat)

          print *,' iostat after baclose of noflx ',iostat,noflx
          close(nonst)
        endif
      endif

      if (me == ioproc)  call wrtlog(phour,fhour,idate)
!     tb = rtc()
!     tf = tb - ta
!     t2 = rtc()

!     timesum = timesum + (t2 - t1)
!     if (me == 0) print 1012, timesum,t2-t1,td,te,tf,t4-t3,tba,tbb,tbd
!1012 format(' wrtout time all tasks  ',f10.4,f10.4,
!    &       ' state, send, io  iobarr, (beginbarr), spectbarr,open,',
!    &       ' openbarr )  ',8f9.4)

!  --- ...  restore temperature for continuing computation

      if ( convert_temp_ps ) then
        if ( run_enthalpy ) then
          thermodyn_id = 3
          sfcpress_id  = 2
        else
          thermodyn_id = 1
          sfcpress_id  = 2
        endif

        if (comp_task) then
! te
!$omp parallel do private(k,kk)
          do k = 1, levs
            kk = p_te + k - 1
            do i=1,len_trie_ls
              trie_ls(i,1,kk) = trie_te(i,1,k)
              trie_ls(i,2,kk) = trie_te(i,2,k)
            enddo
            do i=1,len_trio_ls
              trio_ls(i,1,kk) = trio_te(i,1,k)
              trio_ls(i,2,kk) = trio_te(i,2,k)
            enddo
          enddo
! ps
          do i=1,len_trie_ls
            trie_ls(i,1,p_q) = trie_q (i,1)
            trie_ls(i,2,p_q) = trie_q (i,2)
          enddo
          do i=1,len_trio_ls
            trio_ls(i,1,p_q) = trio_q (i,1)
            trio_ls(i,2,p_q) = trio_q (i,2)
          enddo
          deallocate (trie_q, trio_q,trie_te,trio_te)
        endif    ! end if comp_task block
      endif
!
      return
!...................................
      end subroutine wrtout
!-----------------------------------


!-----------------------------------
      subroutine wrt_restart                                            &
!...................................
     &     ( trie_ls,trio_ls,sfc_fld,nst_fld,si,sl,fhour,               &
     &       idate,igen,pdryini,ls_node,ls_nodes,max_ls_nodes,          &
     &       global_lats_r,lonsperlar,snnp1ev,snnp1od,                  &
     &       global_lats_a,lonsperlat,                                  &
     &       phy_f3d,phy_f2d, dyn_f3d, dyn_f2d,                         &
     &       ngptc,adiab,ens_nam,nst_fcst,                              &
     &       sigr1,sigr2,sfcr,cnstr )

! =================   subprogram documentation block   ================ !
!                                                                       !
!    this program writing restart files                                 !
!                                                                       !
!    usage:        call wrt_restart                                     !
!                                                                       !
!    subprograms called:                                                !
!                  sigio_rwopen, twriteeo, para_fixio_w, para_nstio_w.  !
!                                                                       !
!    attributes:                                                        !
!      language:   fortran 90                                           !
!                                                                       !
!    external modules referenced:                                       !
!      'module resol_def'            in 'resol_def.f'                   !
!      'module layout1'              in 'layout1.f'                     !
!      'module mpi_def'              in 'mpi_def.f.f'                   !
!      'module namelist_def'         in 'namelist_def.f'                !
!      'module sigio_module'         in 'sigio_module.f.                !
!      'module sigio_r_module'       in 'sigio_r_module.f'              !
!      'module nstio_module'         in 'nstio_module.f'                !
!      'module sfc_flx_esmfmod'      in 'sfc_var_esmfmod.f'             !
!      'module nst_var_esmfmodl      in 'nst_var_esmfmod.f'             !
!                                                                       !
!  ====================  defination of variables  ====================  !
!                                                                       !
!    input/output variables:                                     size   !
!      trie_ls/trio_ls:kind_evod,                                       !
!                                       (len_trio_ls,2,11*levs+3*levh+6)!
!      sfc_fld       : sfc_var_data type, surface related fields        !
!      nst_fld       : nst_var_data type, nst related fields            !
!      sl/si         : kind_evod, sigma layer/interface      levs/levp1 !
!      fhour         : kind_evod, forecast time in hr              1    !
!      idate         : integer, init cond time (h,m,d,y)           4    !
!      igen          : integer,                                    1    !
!      pdryini       : real,                                       1    !
!      ls_node       : integer,                               (ls_dim,3)!
!      ls_nodes      : integer,                           (ls_dim,nodes)!
!      max_ls_nodes  : integer,                                  nodes  !
!      global_lats_r : integer, index for global latitudes        latr  !
!      lonsperlar    : integer, num of pts on a given lat circle  latr  !
!      global_lats_g : integer, index for global latitudes        latg  !
!      lonsperlat    : integer, num of pts on a given lat circle  latg  !
!      snnp1ev       : kind_evod,                           len_trie_ls !
!      snnp1od       : kind_evod,                           len_trio_ls !
!      phy_f3d       : real,                                            !
!                                        (lonr,levs,num_p3d,lats_node_r)!
!      phy_f2d       : real,                                            !
!                                             (lonr,num_p2d,lats_node_r)!
!      dyn_f3d       : real,                                            !
!                                        (lonf,levs,num_a3d,lats_node_a)!
!      dyn_f2d       : real,                                            !
!                                             (lonf,num_a2d,lats_node_a)!
!      ngptc         : integer,                                    1    !
!      adiab         : logical,                                    1    !
!      ens_nam       : character,                                  *    !
!      nst_fcst      : integer,                                    1    !
!      sigr1         : character,                                  5    !
!      sigr2         : character,                                  5    !
!      sfcr          : character,                                  4    !
!      cnstr         : character,                                  4    !
!                                                                       !
!  ======================  end of definations  =======================  !
!
      use resol_def
      use layout1
      use mpi_def
      use sigio_module
      use sigio_r_module
      use nstio_module
      use sfc_flx_esmfmod
      use nst_var_esmfmod
      use namelist_def, only: n3dzhaocld, n3dfercld,                    &
     &                        n3dcldpdf, n3dflxtvd
!
      implicit none
!
!  ---  inputs/outputs:
      integer :: idate(4), igen, ngptc, nst_fcst
      integer :: ls_node(ls_dim*3), ls_nodes(ls_dim,nodes),             &
     &     max_ls_nodes(nodes), global_lats_r(latr), lonsperlar(latr),  &
     &     global_lats_a(latg), lonsperlat(latg)

      real (kind=kind_evod) :: sl(levs), si(levp1), pdryini, fhour
      real (kind=kind_evod) :: trie_ls(len_trie_ls,2,11*levs+3*levh+6), &
     &                         trio_ls(len_trio_ls,2,11*levs+3*levh+6)
      real (kind=kind_evod) :: snnp1ev(len_trie_ls),                    &
     &                         snnp1od(len_trio_ls)
      real (kind=kind_phys) :: phy_f3d(lonr,levs,num_p3d,lats_node_r),  &
     &                         phy_f2d(lonr,num_p2d,lats_node_r)
      real (kind=kind_evod) :: dyn_f3d(lonf,levs,num_a3d,lats_node_a),  &
     &                         dyn_f2d(lonf,num_a2d,lats_node_a)

      type (sfc_var_data)   :: sfc_fld
      type (nst_var_data)   :: nst_fld

      logical          :: adiab
      character(len=*) :: ens_nam
      character        :: sigr1*5, sigr2*5, sfcr*4, cnstr*4

!  ---  locals:
      integer          :: ioproc, iret, ixgr, ixga, n3, n4, nflop, nstr
      integer          :: nfill                 ! external function
      integer          :: a, b, c, d

      character*20 :: cflop, sigr51, sigr52, nstr54
!
!===> ...  begin here
!
      if (me == 0) then
        write(0,*)'writing restart files fhour in wrt_res=',fhour
      endif

      sigr51 = sigr1  // ens_nam(1:nfill(ens_nam))
      sigr52 = sigr2  // ens_nam(1:nfill(ens_nam))
      cflop  = sfcr   // ens_nam(1:nfill(ens_nam))
      nstr54 = cnstr  // ens_nam(1:nfill(ens_nam))

      if (me == 0) write(0,*)' sigr51=',sigr51,' sigr52=',sigr52,
     &  ' cflop=',cflop,'ens_nam=',ens_nam(1:nfill(ens_nam)),
     &  ' nstr54=',nstr54

      n3 = 51
      call sigio_rwopen(n3,sigr51,iret)

!     write(0,*)' in rest bef rewind fhour=',fhour,' iret=',iret

      rewind(n3)

!     write(0,*)' in rest write fhour=',fhour

      if (icolor == 2) then
        ioproc = nodes - 1
      else
        ioproc = nodes
      endif

!--for physics
      ixgr = 0
      a = 0 ; b = 0 ; c = 0
      if (.not. adiab) then
        if (n3dzhaocld > 0) a = 3    !zhao micro, four 3d fields, two 2d fields
        if (n3dfercld  > 0) b = 2    !ferrier micro, three 3d fields, zero 2d field
        if (n3dcldpdf  > 0) c = 1    !pdf cloud, three 3d fields, zero 2d field
      endif
      ixgr = a + b*10 + c*100

!--for dynamics
      ixga = 0
      a = 0 ; b = 0 ; c=0
      if (n3dflxtvd > 0)  a = 1    !tvd for dynamics, one 3d fields, zero 2d field
      ixga = a + b*10 + c*100

      if (icolor==2 .and. me==ioproc) then
        write(0,*)' before twriteeo, ixgr ixga: ',ixgr, ixga
      endif

      call twriteeo(n3,ioproc,fhour,idate,
     &       trie_ls(1,1,p_zq ),trie_ls(1,1,p_qm ),trie_ls(1,1,p_tem),
     &       trie_ls(1,1,p_dim),trie_ls(1,1,p_zem),trie_ls(1,1,p_rm ),
     &       trie_ls(1,1,p_gz ),
     &       trio_ls(1,1,p_zq ),trio_ls(1,1,p_qm ),trio_ls(1,1,p_tem),
     &       trio_ls(1,1,p_dim),trio_ls(1,1,p_zem),trio_ls(1,1,p_rm ),
     &       trio_ls(1,1,p_gz ),
     &       sl,si,pdryini,ls_nodes,max_ls_nodes,ixgr,ixga,
     &       phy_f3d,phy_f2d,global_lats_r,lonsperlar,
     &       dyn_f3d,dyn_f2d,global_lats_a,lonsperlat)

      if (icolor==2 .and. me==ioproc) write(0,*)' closed ',n3

      n4 = 52
      call sigio_rwopen(n4,sigr52,iret)

      rewind(n4)

      if (icolor == 2) then
        ioproc = nodes - 1
      else
        ioproc = nodes
      endif
      ixgr = 0
      ixga = 0

      call twriteeo(n4,ioproc,fhour,idate,
     &         trie_ls(1,1,p_zq),trie_ls(1,1,p_q ),trie_ls(1,1,p_te),
     &         trie_ls(1,1,p_di),trie_ls(1,1,p_ze),trie_ls(1,1,p_rq),
     &         trie_ls(1,1,p_gz),
     &         trio_ls(1,1,p_zq),trio_ls(1,1,p_q ),trio_ls(1,1,p_te),
     &         trio_ls(1,1,p_di),trio_ls(1,1,p_ze),trio_ls(1,1,p_rq),
     &         trio_ls(1,1,p_gz),
     &         sl,si,pdryini,ls_nodes,max_ls_nodes,ixgr,ixga,
     &         phy_f3d,phy_f2d,global_lats_r,lonsperlar,
     &         dyn_f3d,dyn_f2d,global_lats_a,lonsperlat)

      if (icolor==2 .and. me==ioproc) write(0,*)' closed ',n4
!     if (icolor==2 .and. me==ioproc) close(n4)
!     print *,' finished writing restart files'

      nflop = 53
      if (icolor == 2) then
         ioproc = nodes - 1
      else
         ioproc = nodes
      endif

      if (.not. adiab) then
        call para_fixio_w(ioproc,sfc_fld, nflop,cflop,fhour,idate,
     &                    global_lats_r,lonsperlar)
        if (nst_fcst > 0) then
          nstr = 54
          call para_nstio_w(ioproc,nst_fld,nstr,nstr54,fhour,idate,
     &                      global_lats_r,lonsperlar)
        endif
      endif ! .not. adiab
!
      return
!...................................
      end subroutine wrt_restart
!-----------------------------------


!-----------------------------------
      subroutine wrtlog(phour,fhour,idate)
!...................................

      use resol_def
      use namelist_def
!
      implicit none
!
!  ---  inputs/outputs:
      integer :: idate(4)
      real (kind=kind_evod), intent(in) :: phour, fhour

!  ---  locals:
      integer   :: nolog, ks, kh, km, ndig
      integer   :: nfill                   ! external function
      logical   ::  lfnhr
      character :: cfhour*40, cform*40
!
!===> ...  begin here
!
!  ---  create cfhour

      lfnhr = .true.    ! no output
!     lfnhr = .false.   !    output

      lfnhr = (3600*abs(fhour-nint(fhour))<=1 .or. phour==0.)
      if ( lfnhr ) then
        kh = nint(fhour)
        ndig = max(log10(kh+0.5)+1., 2.)
        write(cform,'("(i",i1,".",i1,")")') ndig,ndig
        write(cfhour,cform) kh
        write(cform,'("(i",i1,".",i1,")")') ndig,ndig
        write(cfhour,cform) kh
      else
        ks = nint(fhour*3600)
        kh = ks / 3600
        km = (ks - kh*3600) / 60
        ks = ks - kh*3600 - km*60
        ndig = max(log10(kh+0.5)+1., 2.)
        write(cform,'("(i",i1,".",i1,",a1,i2.2,a1,i2.2)")') ndig,ndig
        write(cfhour,cform) kh,':',km,':',ks
      endif
      cfhour = cfhour(1:nfill(cfhour)) // ens_nam(1:nfill(ens_nam))

      nolog = 99
      open(nolog,file='LOG.F'//cfhour,form='formatted')
      write(nolog,100) fhour,idate
100   format(' completed mrf fhour=',f10.3,2x,4(i4,2x))
      close(nolog)
!
      return
!...................................
      end subroutine wrtlog
!-----------------------------------


!-----------------------------------
      subroutine shapeset(ls_nodes,max_ls_nodes,pdryini)
!...................................
!
      use resol_def
      use layout1
      use sig_io
      use namelist_def
      use mpi_def
!
      implicit none
!
!  ---  inputs/outputs:
      integer :: ls_nodes(ls_dim,nodes), max_ls_nodes(nodes)

      real (kind=kind_io8) :: pdryini

!  ---  locals:
      integer :: i, il, j, ilen, msgtag, ls_diml, ioproc, itmpr

      real (kind=kind_evod) :: gencode, order, ppid, rlatd,             &
     &       rlatp, rlatr, rlond, rlonp, rlonr, slid, subcen, tracers,  &
     &       trun, vcid, vmid, vtid, waves, xlayers

      real (kind=kind_io4) :: tmps(4+nodes+jcap1*nodes),                &
     &                        tmpr(3+nodes+jcap1*(nodes-1))
!
!===> ...  begin here
!

!  ---  now define shape of the coefficients array as a function of node.
!       this will define how to assemble the few wavenumbers on each node
!       into a full coefficient array.
!
      if (icolor == 2) then
        ioproc = nodes - 1
      else
        ioproc = nodes
      endif

      if (liope) then
        if (me==0 .or. me==ioproc) then
          tmps = 0.
          tmps(1) = pdryini
          tmps(2:nodes_comp+1) = max_ls_nodes(1:nodes_comp)
          tmps(nodes_comp+2) = ls_dim
          tmps(nodes_comp+3) = len_trie_ls_max
          tmps(nodes_comp+4) = len_trio_ls_max

          il = nodes_comp + 4
          do i = 1, nodes_comp
            do j = 1, ls_dim
              il = il + 1
              tmps(il) = ls_nodes(j,i)
            enddo
          enddo

          ilen = 4 + nodes_comp + jcap1*nodes_comp
          msgtag = 2345
          if (me == 0) then
            call mpi_send(tmps,ilen,mpi_r_io,ioproc,
     &                    msgtag,mpi_comm_all,info)
          endif
        endif

        if (me == ioproc) then
          ilen = 4 + nodes_comp + jcap1*(nodes_comp)
          msgtag = 2345
          call mpi_recv(tmpr,ilen,mpi_r_io,0,
     &                  msgtag,mpi_comm_all,stat,info)

          itmpr = 3 + nodes + jcap1*(nodes-1)
          tmps(1:itmpr) = tmpr(1:itmpr)
          ls_nodes = 0
          pdryini = tmps(1)

          max_ls_nodes(1:nodes_comp) = int(tmps(2:nodes_comp+1))
          ls_diml         = int(tmps(nodes_comp+2))
          len_trie_ls_max = int(tmps(nodes_comp+3))
          len_trio_ls_max = int(tmps(nodes_comp+4))
          il = nodes_comp + 3 + 1

          do i = 1, nodes_comp
            do j = 1, ls_diml
              il = il + 1
              ls_nodes(j,i) = int(tmps(il))
            enddo
          enddo
        endif
      endif    ! end_if_liope
!
      return
!...................................
      end subroutine shapeset
!-----------------------------------


!-----------------------------------
      subroutine sfc_collect (sfc_fld,global_lats_r,lonsperlar)
!...................................
!
      use resol_def
      use mod_state
      use layout1
      use mpi_def
      use sfc_flx_esmfmod
!
      implicit none
!
!  ---  inputs/outputs:
      type (sfc_var_data) :: sfc_fld

      integer :: global_lats_r(latr), lonsperlar(latr)

!  ---  locals:
      real (kind=kind_io8) :: buffo(lonr,lats_node_r),
     &                        buffi(lonr,lats_node_r)

      integer :: kmsk(lonr,lats_node_r), k,j,i
!     integer, save :: icount = 0
!     integer, save :: version = 200004
!
!===> ...  begin here
!
      ngrid = 1

      if (.not. allocated(buff_mult_piece)) then
         allocate(buff_mult_piece(lonr,ngrids_sfcc,lats_node_r))
      endif

      if (.not. allocated(buff_mult_piecef)) then
         allocate(buff_mult_piecef(lonr,0:ngrids_flx,lats_node_r))
      endif

      if (.not. allocated(buff_mult_piecea)) then
         allocate (buff_mult_piecea(lonr,
     &             1:ngrids_flx+ngrids_sfcc+ngrids_nst+1,lats_node_r))
      endif

      kmsk = nint(sfc_fld%slmsk)
      call uninterprez(1,kmsk,buffo,sfc_fld%tsea,
     &                 global_lats_r,lonsperlar)

      do k = 1, lsoil
!$omp parallel do private(i,j)
        do j=1,lats_node_r
          do i=1,lonr
            buffi(i,j) = sfc_fld%smc(i,k,j)
          enddo
        enddo
        call uninterprez(1,kmsk,buffo,buffi,global_lats_r,lonsperlar)
      enddo

      call uninterprez(1,kmsk,buffo,sfc_fld%sheleg,
     &                 global_lats_r,lonsperlar)

      do k = 1, lsoil
!$omp parallel do private(i,j)
        do j=1,lats_node_r
          do i=1,lonr
            buffi(i,j) = sfc_fld%stc(i,k,j)
          enddo
        enddo
        call uninterprez(1,kmsk,buffo,buffi,global_lats_r,lonsperlar)
      enddo

      call uninterprez(1,kmsk,buffo,sfc_fld%tg3,
     &                 global_lats_r,lonsperlar)

      call uninterprez(1,kmsk,buffo,sfc_fld%zorl,
     &                 global_lats_r,lonsperlar)

!  ---  alvsf
      call uninterprez(1,kmsk,buffo,sfc_fld%alvsf,
     &                 global_lats_r,lonsperlar)
!  ---  alvwf
      call uninterprez(1,kmsk,buffo,sfc_fld%alvwf,
     &                 global_lats_r,lonsperlar)
!  ---  alnsf
      call uninterprez(1,kmsk,buffo,sfc_fld%alnsf,
     &                 global_lats_r,lonsperlar)
!  ---  alnwf
      call uninterprez(1,kmsk,buffo,sfc_fld%alnwf,
     &                 global_lats_r,lonsperlar)
!  ---  slmsk
      call uninterprez(1,kmsk,buffo,sfc_fld%slmsk,
     &                 global_lats_r,lonsperlar)
!  ---  vfrac
      call uninterprez(1,kmsk,buffo,sfc_fld%vfrac,
     &                 global_lats_r,lonsperlar)
!  ---  canopy
      call uninterprez(1,kmsk,buffo,sfc_fld%canopy,
     &                 global_lats_r,lonsperlar)
!  ---  f10m
      call uninterprez(1,kmsk,buffo,sfc_fld%f10m,
     &                 global_lats_r,lonsperlar)
!  ---  t2m
      call uninterprez(1,kmsk,buffo,sfc_fld%t2m,
     &                 global_lats_r,lonsperlar)
!  ---  q2m
      call uninterprez(1,kmsk,buffo,sfc_fld%q2m,
     &                 global_lats_r,lonsperlar)
!  ---  vtype
      call uninterprez(1,kmsk,buffo,sfc_fld%vtype,
     &                 global_lats_r,lonsperlar)
!  ---  stype
      call uninterprez(1,kmsk,buffo,sfc_fld%stype,
     &                 global_lats_r,lonsperlar)
!  ---  facsf
      call uninterprez(1,kmsk,buffo,sfc_fld%facsf,
     &                 global_lats_r,lonsperlar)
!  ---  facwf
      call uninterprez(1,kmsk,buffo,sfc_fld%facwf,
     &                 global_lats_r,lonsperlar)
!  ---  uustar
      call uninterprez(1,kmsk,buffo,sfc_fld%uustar,
     &                 global_lats_r,lonsperlar)
!  ---  ffmm
      call uninterprez(1,kmsk,buffo,sfc_fld%ffmm,
     &                 global_lats_r,lonsperlar)
!  ---  ffhh
      call uninterprez(1,kmsk,buffo,sfc_fld%ffhh,
     &                 global_lats_r,lonsperlar)
!  ---  hice
      call uninterprez(1,kmsk,buffo,sfc_fld%hice,
     &                 global_lats_r,lonsperlar)
!  ---  fice
      call uninterprez(1,kmsk,buffo,sfc_fld%fice, 
     &                 global_lats_r,lonsperlar)
!  ---  tisfc
      call uninterprez(1,kmsk,buffo,sfc_fld%tisfc,
     &                 global_lats_r,lonsperlar)
!  ---  tprcp
      call uninterprez(1,kmsk,buffo,sfc_fld%tprcp,
     &                 global_lats_r,lonsperlar)
!  ---  srflag
      call uninterprez(1,kmsk,buffo,sfc_fld%srflag,
     &                 global_lats_r,lonsperlar)
!  ---  snwdph
      call uninterprez(1,kmsk,buffo,sfc_fld%snwdph,
     &                 global_lats_r,lonsperlar)
!  ---  slc
      do k = 1, lsoil
!$omp parallel do private(i,j)
        do j=1,lats_node_r
          do i=1,lonr
            buffi(i,j) = sfc_fld%slc(i,k,j)
          enddo
        enddo
        call uninterprez(1,kmsk,buffo,buffi,global_lats_r,lonsperlar)
      enddo
!  ---  shdmin
      call uninterprez(1,kmsk,buffo,sfc_fld%shdmin,
     &                 global_lats_r,lonsperlar)
!  ---  shdmax
      call uninterprez(1,kmsk,buffo,sfc_fld%shdmax,
     &                 global_lats_r,lonsperlar)
!  ---  slope
      call uninterprez(1,kmsk,buffo,sfc_fld%slope,
     &                 global_lats_r,lonsperlar)
!  ---  snoalb
      call uninterprez(1,kmsk,buffo,sfc_fld%snoalb,
     &                 global_lats_r,lonsperlar)
!  ---  oro
      call uninterprez(1,kmsk,buffo,sfc_fld%oro,
     &                 global_lats_r,lonsperlar)

!     print *,' finished sfc_collect for  ngrid=',ngrid
      ngrid = 1
!
      return
!...................................
      end subroutine sfc_collect
!-----------------------------------


!-----------------------------------
       subroutine nst_collect(nst_fld,global_lats_r,lonsperlar)
!...................................
!
      use resol_def
      use mod_state
      use layout1
      use mpi_def
      use nst_var_esmfmod
!
      implicit none
!
!  ---  inputs/outputs:
      type (nst_var_data) :: nst_fld

      integer :: global_lats_r(latr), lonsperlar(latr)

!  ---  locals:
      real(kind=kind_io8):: buffo(lonr,lats_node_r)

      integer :: kmsk(lonr,lats_node_r)
!
!===> ...  begin here
!
!     ngrid = ngrids_sfcc + ngrids_flx + 1 + 1
      ngrid = ngrids_sfcc + 1

      if (.not. allocated(buff_mult_piece)) then
        allocate(buff_mult_piece(lonr,  ngrids_sfcc,lats_node_r))
        allocate(buff_mult_piecef(lonr,0:ngrids_flx,lats_node_r))
        allocate(buff_mult_pieceo(lonr,  ngrids_nst,lats_node_r))
        allocate(buff_mult_piecea(lonr,
     &           1:ngrids_flx+ngrids_sfcc+ngrids_nst+1,lats_node_r))
      endif
      kmsk = nint(nst_fld%slmsk)

      call uninterprez(1,kmsk,buffo, nst_fld%slmsk,
     &                 global_lats_r,lonsperlar)
      call uninterprez(1,kmsk,buffo, nst_fld%xt,
     &                 global_lats_r,lonsperlar)
      call uninterprez(1,kmsk,buffo, nst_fld%xs,
     &                 global_lats_r,lonsperlar)
      call uninterprez(1,kmsk,buffo, nst_fld%xu,
     &                 global_lats_r,lonsperlar)
      call uninterprez(1,kmsk,buffo, nst_fld%xv,
     &                 global_lats_r,lonsperlar)
      call uninterprez(1,kmsk,buffo, nst_fld%xz,
     &                 global_lats_r,lonsperlar)
      call uninterprez(1,kmsk,buffo, nst_fld%zm,
     &                 global_lats_r,lonsperlar)
      call uninterprez(1,kmsk,buffo, nst_fld%xtts,
     &                 global_lats_r,lonsperlar)
      call uninterprez(1,kmsk,buffo, nst_fld%xzts,
     &                 global_lats_r,lonsperlar)
      call uninterprez(1,kmsk,buffo, nst_fld%dt_cool,
     &                 global_lats_r,lonsperlar)
      call uninterprez(1,kmsk,buffo, nst_fld%z_c,
     &                 global_lats_r,lonsperlar)
      call uninterprez(1,kmsk,buffo, nst_fld%c_0,
     &                 global_lats_r,lonsperlar)
      call uninterprez(1,kmsk,buffo, nst_fld%c_d,
     &                 global_lats_r,lonsperlar)
      call uninterprez(1,kmsk,buffo, nst_fld%w_0,
     &                 global_lats_r,lonsperlar)
      call uninterprez(1,kmsk,buffo, nst_fld%w_d,
     &                 global_lats_r,lonsperlar)
      call uninterprez(1,kmsk,buffo, nst_fld%d_conv,
     &                 global_lats_r,lonsperlar)
      call uninterprez(1,kmsk,buffo, nst_fld%ifd,
     &                 global_lats_r,lonsperlar)
      call uninterprez(1,kmsk,buffo, nst_fld%tref,
     &                 global_lats_r,lonsperlar)
      call uninterprez(1,kmsk,buffo, nst_fld%qrain,
     &                 global_lats_r,lonsperlar)

!     print *,' finished nst_collect for  ngrid=',ngrid
      ngrid = 1
!
      return
!...................................
      end subroutine nst_collect
!-----------------------------------


!-----------------------------------
       subroutine sfc_only_move(ioproc)
!...................................
!
      use resol_def
      use mod_state
      use layout1
      use mpi_def
!
      implicit none
!
!  ---  inputs/outputs:
      integer :: ioproc

!  ---  locals:
      integer :: ubound, proc, msgtag, maxlats_comp, illen, ierr
      integer, save :: icount
      data icount / 0 /
!
!===> ...  begin here
!
!  ---  allocate the data structures

      if (icount == 0) then
        allocate(ivar_global(10))
        allocate(ivar_global_a(10,nodes))

        ivar_global(1) = ipt_lats_node_r
        ivar_global(2) = lats_node_r
        ivar_global(3) = lats_node_r_max

        call mpi_gather(ivar_global,10,mpi_integer,
     &       ivar_global_a,10,mpi_integer,ioproc,mpi_comm_all,ierr)
        icount = icount + 1
      endif

      if (.not. allocated(buff_mult_pieces)) then
        maxlats_comp = lats_node_r_max

        if(liope .and. me==ioproc) then
          maxlats_comp = ivar_global_a(3,1)
        endif
!       print *,' index for maxlat set ',ioproc

        allocate(buff_mult_pieces (lonr,ngrids_sfcc,maxlats_comp,nodes))
        allocate(buff_mult_piecesf(lonr,0:ngrids_flx,
     &                                              maxlats_comp,nodes))
        allocate(buff_mult_pieceo (lonr,ngrids_nst, maxlats_comp))
        allocate(buff_mult_pieceso(lonr,ngrids_nst, maxlats_comp,nodes))
        illen = ngrids_flx + ngrids_sfcc + ngrids_nst + 1
        allocate(buff_mult_piecesa(lonr,1:illen,    maxlats_comp,nodes))
      endif

!  ---  sendloop of grids from comp processors to i/o task.  the i/o task
!       may or may not be a comp task also.  the send logic on that task
!       is different for these two cases
!
!  big send

       buff_mult_piece(:,1:ngrids_sfcc,:) =
     &                           buff_mult_piecea(:,1:ngrids_sfcc,:)

      if (me /= ioproc) then    !   sending the data
        msgtag = me
        illen = lats_node_r * lonr * ngrids_sfcc

!  ---  send the local grid domain
         call mpi_send(buff_mult_piece,illen,mpi_r_io,ioproc,msgtag,
     &                 mpi_comm_all,info)
      else
        if (mc_comp /= mpi_comm_null) then

!  ---  iotask is also a compute task.  send is replaced with direct array copy

          buff_mult_pieces(:,:,1:lats_node_r,ioproc+1) =
     &                  buff_mult_piece(:,:,1:lats_node_r)
        endif

!  ---  end compute tasks portion of logic
!       receiving part of i/o task

        do proc = 1, nodes_comp
          if (proc /= ioproc+1) then
            msgtag = proc - 1
            illen = ivar_global_a(2,proc) * lonr * ngrids_sfcc
!           print *,' pux target ',ubound(buff_mult_pieces)

            call mpi_recv(buff_mult_pieces(1,1,1,proc),illen,mpi_r_io,
     &                    proc-1,msgtag,mpi_comm_all,stat,info)
          endif
        enddo

        buff_mult_piecesa(:,1:ngrids_sfcc,:,:) =
     &                      buff_mult_pieces(:,1:ngrids_sfcc,:,:)
      endif
!
      return
!...................................
      end subroutine sfc_only_move
!-----------------------------------


!-----------------------------------
      subroutine nst_only_move(ioproc)
!...................................
!
      use resol_def
      use mod_state
      use layout1
      use mpi_def
!
      implicit none
!
!  ---  inputs/outputs:
      integer :: ioproc

!  ---  locals:
      integer :: nstart, nend, proc, msgtag, illen, ubound, ierr,
     &           maxlats_comp
      integer, save :: icount
      data icount / 0 /
!
!===> ...  begin here
!
!  ---  allocate the data structures

      if (icount == 0) then
        if (.not. allocated(ivar_global)) then
          allocate(ivar_global(10))
          allocate(ivar_global_a(10,nodes))

          ivar_global(1) = ipt_lats_node_r
          ivar_global(2) = lats_node_r
          ivar_global(3) = lats_node_r_max

          call mpi_gather(ivar_global,10,mpi_integer,ivar_global_a,
     &                    10,mpi_integer,ioproc,mpi_comm_all,ierr)
          icount = icount + 1
        endif
      endif

      if (.not. allocated(buff_mult_pieces)) then
        maxlats_comp=lats_node_r_max

        if(liope .and. me==ioproc) then
          maxlats_comp = ivar_global_a(3,1)
        endif
!       print *,' index for maxlat set ',ioproc

        allocate(buff_mult_pieces (lonr,ngrids_sfcc,maxlats_comp,nodes))
!       print *,' allocated', lonr,ngrids_sfcc,maxlats_comp,nodes

        allocate(buff_mult_piecesf(lonr,0:ngrids_flx,
     &                                              maxlats_comp,nodes))

        allocate(buff_mult_pieceo (lonr,ngrids_nst, maxlats_comp))

        allocate(buff_mult_pieceso(lonr,ngrids_nst, maxlats_comp,nodes))

        illen = ngrids_flx + 1 + ngrids_sfcc + ngrids_nst
        allocate(buff_mult_piecesa(lonr,1:illen,    maxlats_comp,nodes))
      endif

!  ---  sendloop of grids from comp processors to i/o task.  the i/o task
!       may or may not be a comp task also.  the send logic on that task
!       is different for these two cases

!  big send
      nstart = ngrids_sfcc + 1
      nend   = ngrids_sfcc + ngrids_nst

      buff_mult_pieceo(:,1:ngrids_nst,1:lats_node_r) =
     &                   buff_mult_piecea(:,nstart:nend,1:lats_node_r)

      if (me /= ioproc) then    !   sending the data
        msgtag = me
        illen = lats_node_r

!  ---  send the local grid domain

        call mpi_send(buff_mult_pieceo,illen*lonr*ngrids_nst,
     &                mpi_r_io,ioproc,msgtag,mpi_comm_all,info)
      else
        if (mc_comp /= mpi_comm_null) then

!  ---  iotask is also a compute task.  send is replaced with direct array copy

          buff_mult_pieceso(:,:,1:lats_node_r,ioproc+1) =
     &                        buff_mult_pieceo(:,:,1:lats_node_r)
        endif

!  ---  end compute tasks portion of logic receiving part of i/o task

        do proc = 1, nodes_comp
          if (proc /= ioproc+1) then
            msgtag = proc - 1
            illen = ivar_global_a(2,proc) * lonr * ngrids_nst
            print *,' pux target ',ubound(buff_mult_pieceso)

            call mpi_recv(buff_mult_pieceso(1,1,1,proc),illen,mpi_r_io,
     &                    proc-1,msgtag,mpi_comm_all,stat,info)
          endif
        enddo

        buff_mult_piecesa(:,nstart:nend,:,:) =
     &                      buff_mult_pieceso(:,1:ngrids_nst,:,:)
      endif
!
      return
!...................................
      end subroutine nst_only_move
!-----------------------------------


!-----------------------------------
      subroutine sfc_wrt(ioproc,nw,cfile,xhour,idate,                   &
     &                   global_lats_r,lonsperlar)
!...................................
!
      use sfcio_module
      use resol_def
      use mod_state
      use layout1
      use mpi_def
!
      implicit none
!
!  ---  inputs/outputs:
      integer :: nw, ioproc, idate(4)
      integer :: global_lats_r(latr), lonsperlar(latr)

      character*16 :: cfile
      real(kind=kind_io8) :: xhour

!  ---  locals:
      integer k, ngridss, iret
!     integer, save:: version = 200501

      type (sfcio_head), save :: head
      type (sfcio_data)       :: data

      logical, save :: first
      data first /.true./
!
!===> ...  begin here
!
!  ---  build surface fields in to buff_mult

      ngrid = 1
      do ngridss = 1, ngrids_sfcc
!       print *,' inside sfc_wrt calling unsp ngridss=',ngridss

        call unsplit2z(buff_mult(1,1,ngridss),global_lats_r)
      enddo

      if (me == ioproc) then
        if (first) then
          head%clabsfc = char(0)//char(0)//char(0)//char(0)//
     &                   char(0)//char(0)//char(0)//char(0)
          head%latb    = latr
          head%lonb    = lonr
          head%ivs     = ivssfc
!         head%irealf  = 1
          head%lsoil   = lsoil

          call sfcio_alhead(head,iret)

          head%lpl     = lonsperlar(1:latr/2)

          if (lsoil == 4) then
            head%zsoil = (/-0.1,-0.4,-1.0,-2.0/)
          elseif (lsoil == 2) then
            head%zsoil = (/-0.1,-2.0/)
          endif

          first = .false.
        endif

        head%fhour = xhour
        head%idate = idate

        print 99,nw,xhour,idate
 99     format(' in fixio nw=',i7,2x,'hour=',f8.2,3x,'idate=',
     &         4(1x,i4))

        ngrid = 1
        data%tsea => buff_mult(:,:,ngrid)
        ngrid = ngrid + 1
        data%smc  => buff_mult(:,:,ngrid:ngrid+lsoil-1)
        ngrid = ngrid + lsoil
        data%sheleg => buff_mult(:,:,ngrid)
        ngrid = ngrid + 1
        data%stc => buff_mult(:,:,ngrid:ngrid+lsoil-1)
        ngrid = ngrid + lsoil
        data%tg3 => buff_mult(:,:,ngrid)
        ngrid = ngrid + 1
        data%zorl => buff_mult(:,:,ngrid)
        ngrid = ngrid + 1
        data%alvsf => buff_mult(:,:,ngrid)
        ngrid = ngrid + 1
        data%alvwf => buff_mult(:,:,ngrid)
        ngrid = ngrid + 1
        data%alnsf => buff_mult(:,:,ngrid)
        ngrid = ngrid + 1
        data%alnwf => buff_mult(:,:,ngrid)
        ngrid = ngrid + 1
        data%slmsk => buff_mult(:,:,ngrid)
        ngrid = ngrid + 1
        data%vfrac => buff_mult(:,:,ngrid)
        ngrid = ngrid + 1
        data%canopy => buff_mult(:,:,ngrid)
        ngrid = ngrid + 1
        data%f10m => buff_mult(:,:,ngrid)
        ngrid = ngrid + 1
        data%t2m => buff_mult(:,:,ngrid)
        ngrid = ngrid + 1
        data%q2m => buff_mult(:,:,ngrid)
        ngrid = ngrid + 1
        data%vtype => buff_mult(:,:,ngrid)
        ngrid = ngrid + 1
        data%stype => buff_mult(:,:,ngrid)
        ngrid = ngrid + 1
        data%facsf => buff_mult(:,:,ngrid)
        ngrid = ngrid + 1
        data%facwf => buff_mult(:,:,ngrid)
        ngrid = ngrid + 1
        data%uustar => buff_mult(:,:,ngrid)
        ngrid = ngrid + 1
        data%ffmm => buff_mult(:,:,ngrid)
        ngrid = ngrid + 1
        data%ffhh => buff_mult(:,:,ngrid)
        ngrid = ngrid + 1
        data%hice => buff_mult(:,:,ngrid)
        ngrid = ngrid + 1
        data%fice => buff_mult(:,:,ngrid)
        ngrid = ngrid + 1
        data%tisfc => buff_mult(:,:,ngrid)
        ngrid = ngrid + 1
        data%tprcp => buff_mult(:,:,ngrid)
        ngrid = ngrid + 1
        data%srflag => buff_mult(:,:,ngrid)
        ngrid = ngrid + 1
        data%snwdph => buff_mult(:,:,ngrid)
        ngrid = ngrid + 1
        data%slc => buff_mult(:,:,ngrid:ngrid+lsoil-1)
        ngrid = ngrid + lsoil
        data%shdmin => buff_mult(:,:,ngrid)
        ngrid = ngrid + 1
        data%shdmax => buff_mult(:,:,ngrid)
        ngrid = ngrid + 1
        data%slope => buff_mult(:,:,ngrid)
        ngrid = ngrid + 1
        data%snoalb => buff_mult(:,:,ngrid)
        ngrid = ngrid + 1
        data%orog => buff_mult(:,:,ngrid)      ! orography

        call sfcio_swohdc(nw,cfile,head,data,iret)
      endif
!
      return
!...................................
      end subroutine sfc_wrt
!-----------------------------------


!-----------------------------------
      subroutine nst_wrt(ioproc,nw,cfile,xhour,idate,                   &
     &                   global_lats_r,lonsperlar)
!...................................
!
      use nstio_module
      use namelist_def
      use resol_def
      use mod_state
      use layout1
      use mpi_def
!
      implicit none
!
!  ---  inputs/outputs:
      integer :: nw, ioproc, idate(4)
      integer :: global_lats_r(latr), lonsperlar(latr)

      character*16 :: cfile
      real (kind=kind_io8) :: xhour

!  ---  locals:
      integer :: ngridss, iret

!     integer, save :: version = 200907

      type (nstio_head), save :: head
      type (nstio_data)       :: data

      logical, save :: first
      data first /.true./
!
!===> ...  begin here
!
!  ---  build ocean fields in to buff_mult

!     ngrid = ngrids_sfcc + ngrids_flx + 1 + 1
      ngrid = ngrids_sfcc + 1
      do ngridss = ngrid, ngrid+ngrids_nst-1
!       print *,' inside nst_wrt calling unsp ngridss=',ngridss

        call unsplit2z(buff_mult(1,1,ngridss),global_lats_r)
      enddo

      if (me == ioproc) then
        if (first) then
          head%clabnst = char(0)//char(0)//char(0)//char(0)//
     &                   char(0)//char(0)//char(0)//char(0)
          head%latb = latr
          head%lonb = lonr
          head%ivo  = ivsnst
          head%lsea = lsea

          call nstio_alhead(head,iret)

          head%lpl  = lonsperlar(1:latr/2)
          first = .false.
        endif
        head%fhour = xhour
        head%idate = idate

        print 99,nw,xhour,idate
 99     format(' in fixio nw=',i7,2x,'hour=',f8.2,3x,'idate=',
     &         4(1x,i4))

!       ngrid = 1
        ngrid = ngrids_sfcc + 1
        data%slmsk    => buff_mult(:,:,ngrid)
        ngrid = ngrid + 1
        data%xt       => buff_mult(:,:,ngrid)
        ngrid = ngrid + 1
        data%xs       => buff_mult(:,:,ngrid)
        ngrid = ngrid + 1
        data%xu       => buff_mult(:,:,ngrid)
        ngrid = ngrid + 1
        data%xv       => buff_mult(:,:,ngrid)
        ngrid = ngrid + 1
        data%xz       => buff_mult(:,:,ngrid)
        ngrid = ngrid + 1
        data%zm       => buff_mult(:,:,ngrid)
        ngrid = ngrid + 1
        data%xtts     => buff_mult(:,:,ngrid)
        ngrid = ngrid + 1
        data%xzts     => buff_mult(:,:,ngrid)
        ngrid = ngrid + 1
        data%dt_cool  => buff_mult(:,:,ngrid)
        ngrid = ngrid + 1
        data%z_c      => buff_mult(:,:,ngrid)
        ngrid = ngrid + 1
        data%c_0      => buff_mult(:,:,ngrid)
        ngrid = ngrid + 1
        data%c_d      => buff_mult(:,:,ngrid)
        ngrid = ngrid + 1
        data%w_0      => buff_mult(:,:,ngrid)
        ngrid = ngrid + 1
        data%w_d      => buff_mult(:,:,ngrid)
        ngrid = ngrid + 1
        data%d_conv   => buff_mult(:,:,ngrid)
        ngrid = ngrid + 1
        data%ifd      => buff_mult(:,:,ngrid)
        ngrid = ngrid + 1
        data%tref     => buff_mult(:,:,ngrid)
        ngrid = ngrid + 1
        data%qrain    => buff_mult(:,:,ngrid)
        ngrid = ngrid + 1

        call nstio_swohdc(nw,cfile,head,ngrids_nst,data,iret)
      endif
!
      return
!...................................
      end subroutine nst_wrt
!-----------------------------------


!-----------------------------------
      subroutine wrtflx_a(ioproc,noflx,zhour,fhour,idate,colat1,        &
     &                    secswr,seclwr,sfc_fld,flx_fld,fluxr,          &
     &                    global_lats_r,lonsperlar)
!...................................
!
      use resol_def
      use mod_state
      use layout1
      use sig_io
      use namelist_def
      use sfc_flx_esmfmod
!
      implicit none
!
!  ---  inputs/outputs:
      integer :: ioproc, noflx, idate(4)
      integer :: global_lats_r(latr), lonsperlar(latr)

      real (kind=kind_io8) :: zhour, fhour, colat1, secswr, seclwr
      real (kind=kind_io8) :: fluxr(lonr,nfxr,lats_node_r)

      type (sfc_var_data)  :: sfc_fld
      type (flx_var_data)  :: flx_fld

!  ---  locals:
      integer, parameter :: nfld = 29

!     integer, parameter ::        iprs  =  1, ihgt  =  7, itemp = 11,  &
!    &     itmx  = 15, itmn  = 16, iznlw = 33, imerw = 34, isphum= 51,  &
!    &     ipwat = 54, ipcpr = 59, isr= 194, isnowd= 65, isnod = 66,  &
!    &     icldf = 71, iccldf= 72, islmsk= 81, izorl = 83, ialbdo= 84,  &
!    &     istc  = 86, iveg  = 87, irnof = 90, icemsk= 91, isik  = 92,  &
!    &     ilhflx=121, ishflx=122, izws  =124, imws  =125, irst  =140,  &
!    &     isoilm=144, iep   =145, icldwk=146, izgw  =147, imgw  =148,  &
!    &     ighflx=155, icsusw=160, icsdsw=161, icsulw=162, csdlw=163,  &
!    &     iuswfc=160, idswfc=161, iulwfc=162, idlwfc=163, inswfc=164,  &
!    &     inlwfc=165, idswvb=166, idswvd=167, idswnb=168, idswnd=169,  &
!    &     icmm  =179, isuntm=191, isbs  =198, ievbs =199, ievcw =200,  &
!    &     iuvbf =200, iuvbfc=201, idswf =204, idlwf =205, iqmx  =204,  &
!    &     iqmn  =205, ichh  =208, itran =210, iuswf =211, iulwf =212,  &
!    &     icpcpr=214, ismcwlt=219,ismcref=220,ihpbl =221, islo  =222,  &
!    &     icnp  =223, istp  =224, ivtp  =225, isnohf=229, isrf  =235,  &
!    &     isnc  =238, iust  =253

!     integer, parameter ::        isfc  =  1, itoa  =  8, ielev =105,  &
!    &     isglev=109, idbls =111, i2dbls=112, islc  =160, icolmn=200,  &
!    &     iblbl =209, ibltl =210, ibllyr=211, ilcbl =212, ilctl =213,  &
!    &     ilclyr=214, imcbl =222, imctl =223, imclyr=224, ihcbl =232,  &
!    &     ihctl =233, ihclyr=234, icvbl =242, icvtl =243, icvlyr=244

!     integer, parameter ::        ifhour=  1, ifday =  2, inst  = 10,  &
!    &     iwin  =  2, iavg  =  3, iacc  =  4

      integer, dimension(lonr,lats_node_r) :: kmsk, kmsk0, kmskcv
      integer :: i, j, k, k4, l

      real (kind=kind_io8) :: rtime, rtimsw, rtimlw, rtimer(nfld)
      real (kind=kind_io8), dimension(lonr,lats_node_r) :: slmskloc,    &
     &                                                     glolal, buffo
      real (kind=kind_io8) :: rflux(lonr,lats_node_r,nfxr)

!     integer :: len, ierr
!     logical(1) :: lbm(len)

!     integer :: ipur(nfld), itlr(nfld)
!     data ipur /iulwf , iuswf , iuswf , idswf , icldf , iprs  , iprs  ,&
!    &   itemp , icldf , iprs  , iprs  , itemp , icldf , iprs  , iprs  ,&
!    &   itemp , iuvbf , iuvbfc, idswf , icsulw, icsusw, icsdlw, icsusw,&
!    &   icsdsw, icsulw, idswvb, idswvd, idswnb, idswnd /
!     data itlr /itoa  , itoa  , isfc  , isfc  , ihclyr, ihctl , ihcbl ,&
!    &   ihctl , imclyr, imctl , imcbl , imctl , ilclyr, ilctl , ilcbl ,&
!    &   ilctl , isfc  , isfc  , itoa  , itoa  , itoa  , isfc  , isfc  ,&
!    &   isfc  , isfc  , isfc  , isfc  , isfc  , isfc   /
!
!===> ...  begin here
!
      ngrid = ngrids_sfcc + 1 + ngrids_nst
!
!     initialize ibufm
      ibufm(1,1)=-9

      kmsk  = nint(sfc_fld%slmsk)
      kmsk0 = 0

      call uninterprez(1,kmsk,glolal,sfc_fld%slmsk,
     &                 global_lats_r,lonsperlar)

      slmskloc = glolal

!!!!!$omp parallel do private(k,j,i)    !!!not threading safe
      do k = 1, nfxr
        do j = 1, lats_node_r
          do i = 1, lonr
            rflux(i,j,k) = fluxr(i,k,j)
          enddo
        enddo
      enddo

      if (fhour > zhour) then
        rtime = 1. / (3600.*(fhour-zhour))
      else
        rtime = 0.
      endif

      if (secswr > 0.) then
        rtimsw = 1. / secswr
      else
        rtimsw = 1.
      endif

      if (seclwr > 0.) then
        rtimlw = 1. / seclwr
      else
        rtimlw = 1.
      endif

      rtimer     = rtimsw
      rtimer( 1) = rtimlw
      rtimer(20) = rtimlw       ! csulf_toa
      rtimer(22) = rtimlw       ! csdlf_sfc
      rtimer(25) = rtimlw       ! csulf_sfc
!..........................................................
!  - zonal component of momentum flux:
!
!$omp parallel do private(j,i)
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = flx_fld%dusfc(i,j)*rtime
        enddo
      enddo

      call uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)

!     if (ierr /=0 ) print*,'wrtsfc gribit ierr=',ierr,'  01) ',
!    & 'zonal compt of momentum flux (n/m**2) land and sea surface'
!..........................................................
!  - meridional component of momentum flux:
!
!$omp parallel do private(j,i)
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = flx_fld%dvsfc(i,j)*rtime
        enddo
      enddo

      call uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)

!     if (ierr /=0 ) print*,'wrtsfc gribit ierr=',ierr,'  02) ',
!    & 'merid compt of momentum flux (n/m**2) land and sea surface'
!..........................................................
!  - sensible heat flux:
!
!$omp parallel do private(j,i)
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = flx_fld%dtsfc(i,j)*rtime
        enddo
      enddo

      call uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)

!     if (ierr /=0 ) print*,'wrtsfc gribit ierr=',ierr,'   03) ',
!    & 'sensible heat flux (w/m**2) land and sea surface'
!..........................................................
!  - latent heat flux:
!
!$omp parallel do private(j,i)
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = flx_fld%dqsfc(i,j)*rtime
        enddo
      enddo

      call uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)

!     if (ierr /=0 ) print*,'wrtsfc gribit ierr=',ierr,'  04) ',
!    & 'latent heat flux (w/m**2) land and sea surface'
!..........................................................
!  - surface temperature:
!
      call uninterprez(2,kmsk0,buffo,sfc_fld%tsea,
     &                 global_lats_r,lonsperlar)

!     if (ierr /=0 ) print*,'wrtsfc gribsn ierr=',ierr,'  05) ',
!    & 'temperature (k) land and sea surface'
!..........................................................
!  - volumetric soil moist content at layer 10cm and 0cm:
!
!$omp parallel do private(j,i)
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = sfc_fld%smc(i,1,j)
        enddo
      enddo


      call uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribsn ierr=',ierr,'  06) ',
!    & 'volumetric soil moist content (frac) layer 10cm and 0cm'
!..........................................................
!  - volumetric soil moist content at layer 40cm and 10cm:
!
!$omp parallel do private(j,i)
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = sfc_fld%smc(i,2,j)
        enddo
      enddo

      call uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  07) ',
!    & 'volumetric soil moist content (frac) layer 40cm and 10cm'
!..........................................................
!  - temperature at layer 10cm and 0cm:
!
!$omp parallel do private(j,i)
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = sfc_fld%stc(i,1,j)
        enddo
      enddo

      call uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  08) ',
!    & 'temp (k) layer betw two depth below land sfc 10cm and 0cm'
!..........................................................
!  - temperature at layer 40cm and 10cm:
!
!$omp parallel do private(j,i)
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = sfc_fld%stc(i,2,j)
        enddo
      enddo

      call uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  09) ',
!    & 'temp (k) layer betw two depth below land sfc 40cm and 10cm'
!..........................................................
!  - water equivalent of accummulated snow depth:
!
      call uninterprez(2,kmsk,buffo,sfc_fld%sheleg,
     &                 global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  10) ',
!    & 'water equiv of accum snow depth (kg/m**2) at surface'
!..........................................................
!  - total sky radiation fluxes at toa and surface:
!
      do k = 1, 4
!$omp parallel do private(j,i)
        do j = 1, lats_node_r
          do i = 1, lonr
            glolal(i,j) = rflux(i,j,k)*rtimer(k)
          enddo
        enddo

        call uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)

!       if (ierr/=0 .and. k==1) print*,'wrtsfc gribit ierr=',ierr,'  ',
!    &   '11) upward long wave radiation flux (w/m**2) at toa'
!       if (ierr/=0 .and. k==2) print*,'wrtsfc gribit ierr=',ierr,'  ',
!    &   '12) upward solar radiation flux (w/m**2) at toa'
!       if (ierr/=0 .and. k==3) print*,'wrtsfc gribit ierr=',ierr,'  ',
!    &   '13) upward solar radiation flux (w/m**2) at surface'
!       if (ierr/=0 .and. k==4) print*,'wrtsfc gribit ierr=',ierr,'  ',
!    &   '14) downward solar radiation flux (w/m**2) at surface'
      enddo
!..........................................................
!  - for high, mid, low cloud (cover, pressure, temperature)
!
      lab_do_cloud : do k = 5, 7    ! (high, mid, low clouds)
        k4 = 4 + (k-5)*4

!  - cloud cover (h,m,l):
!
!$omp parallel do private(j,i)
        do j = 1, lats_node_r
          do i = 1, lonr
            glolal(i,j) = rflux(i,j,k)*100.*rtimsw
          enddo
        enddo
        where (glolal >= 0.5)
          kmskcv = 1
        elsewhere
          kmskcv = 0
        endwhere

        call uninterprez(2,kmskcv,buffo,glolal,global_lats_r,lonsperlar)

!       l = k4 + 1
!       if (ierr/=0 .and. k==5) print*,'wrtsfc gribit ierr=',ierr,'  ',
!    &   '15) total cloud cover (percent) high cloud layer'
!       if (ierr/=0 .and. k==6) print*,'wrtsfc gribit ierr=',ierr,'  ',
!    &   '19) total cloud cover (percent) middle cloud layer'
!       if (ierr/=0 .and. k==7) print*,'wrtsfc gribit ierr=',ierr,'  ',
!    &   '23) total cloud cover (percent) low cloud layer'

!  - pressure at cloud top:
!
!$omp parallel do private(j,i)
        do j = 1, lats_node_r
          do i = 1, lonr
            if (rflux(i,j,k) > 0.) then
              glolal(i,j) = rflux(i,j,k+3)*1000./rflux(i,j,k)
            else
              glolal(i,j) = 0.
            endif
          enddo
        enddo

        call uninterprez(2,kmskcv,buffo,glolal,global_lats_r,lonsperlar)

!       l = k4 + 2
!       if (ierr/=0 .and. k==5) print*,'wrtsfc gribit ierr=',ierr,'  ',
!    &   '16) pressure (pa) high cloud top level'
!       if (ierr/=0 .and. k==6) print*,'wrtsfc gribit ierr=',ierr,'  ',
!    &   '20) pressure (pa) middle cloud top level'
!       if (ierr/=0 .and. k==7) print*,'wrtsfc gribit ierr=',ierr,'  ',
!    &   '24) pressure (pa) low cloud top level'

!  - pressure at cloud base:
!
!$omp parallel do private(j,i)
        do j = 1, lats_node_r
          do i = 1, lonr
            if(rflux(i,j,k) > 0.) then
              glolal(i,j) = rflux(i,j,k+6)*1000./rflux(i,j,k)
            else
              glolal(i,j) = 0.
            endif
          enddo
        enddo

        call uninterprez(2,kmskcv,buffo,glolal,global_lats_r,lonsperlar)

!       l = k4 + 3
!       if (ierr/=0 .and. k==5) print*,'wrtsfc gribit ierr=',ierr,'  ',
!    &   '17) pressure (pa) high cloud bottom level'
!       if (ierr/=0 .and. k==6) print*,'wrtsfc gribit ierr=',ierr,'  ',
!    &   '21) pressure (pa) middle cloud bottom level'
!       if (ierr/=0 .and. k==7) print*,'wrtsfc gribit ierr=',ierr,'  ',
!    &   '25) pressure (pa) low cloud bottom level'

!  - temperature at cloud top:
!
!$omp parallel do private(j,i)
        do j = 1, lats_node_r
          do i = 1, lonr
            if (rflux(i,j,k) > 0.) then
              glolal(i,j) = rflux(i,j,k+9)/rflux(i,j,k)
            else
              glolal(i,j) = 0.
            endif
          enddo
        enddo

        call uninterprez(2,kmskcv,buffo,glolal,global_lats_r,lonsperlar)

!       l = k4 + 4
!       if (ierr/=0 .and. k==5) print*,'wrtsfc gribit ierr=',ierr,'  ',
!    &   '18) temperature (k) high cloud top level'
!       if (ierr/=0 .and. k==6) print*,'wrtsfc gribit ierr=',ierr,'  ',
!    &   '22) temperature (k) middle cloud top level'
!       if (ierr/=0 .and. k==7) print*,'wrtsfc gribit ierr=',ierr,'  ',
!    &   '26) temperature (k) low cloud top level'

      enddo  lab_do_cloud
!
!..........................................................
!  - total cloud amount:
!
!$omp parallel do private(j,i)
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = rflux(i,j,17)*100.*rtimsw
        enddo
      enddo

      call uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  27) ',
!    & 'total cloud cover (percent) total atmospheric column'
!..........................................................
!  - boundary layer cloud amount:
!
!$omp parallel do private(j,i)
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = rflux(i,j,18)*100.*rtimsw
        enddo
      enddo

      call uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  28) ',
!    & 'total cloud cover (percent) boundary layer cloud layer'
!..........................................................
!  - surface downeard lw fluxes: (use the surface temp adjusted quantities
!    to replace the original on in rec 19 of fluxr)
!
!$omp parallel do private(j,i)
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = flx_fld%dlwsfc(i,j)*rtimlw
        enddo
      enddo

      call uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  29) ',
!    & 'downward long wave radiation flux (w/m**2) land sea surface'
!..........................................................
!  - surface upward lw fluxes: (use the one recalc'ed from surface temp
!    to replace the original one in rec 20 of fluxr)
!
!$omp parallel do private(j,i)
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = flx_fld%ulwsfc(i,j)*rtimlw
        enddo
      enddo

      call uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  30) ',
!    & 'upward long wave radiation flux (w/m**2) land sea surface'
!..........................................................
!  - uv-b flux at surface for total sky:
!
!$omp parallel do private(j,i)
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = rflux(i,j,21)*rtimsw
        enddo
      enddo

      call uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  31) ',
!    & 'uv-b downward solar flux (w/m**2) land sea surface'
!..........................................................
!  - uv-b flux at surface for clear sky:
!
!$omp parallel do private(j,i)
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = rflux(i,j,22)*rtimsw
        enddo
      enddo

      call uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  32) ',
!    & 'clear sky uv-b downward solar flux (w/m**2) land sea surface'
!..........................................................
!  - incoming solar radiation at toa:
!
!$omp parallel do private(j,i)
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = rflux(i,j,23)*rtimsw
        enddo
      enddo

      call uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  33) ',
!    & 'downward solar radiation flux (w/m**2) at toa'
!..........................................................
!  - sw downward surface flux components:
!
      do l = 24, 27
        k = l + 2

!$omp parallel do private(j,i)
        do j = 1, lats_node_r
          do i = 1, lonr
            glolal(i,j) = rflux(i,j,l)*rtimer(k)
          enddo
        enddo

        call uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)

!       if (ierr/=0 .and. l==24) print*,'wrtsfc gribit ierr=',ierr,
!    &   '   34) downward sw uv+vis beam radiation flux (w/m**2) sfc '
!       if (ierr/=0 .and. l==25) print*,'wrtsfc gribit ierr=',ierr,
!    &   '   35) downward sw uv+vis diffuse radiation flux (w/m**2) sfc'
!       if (ierr/=0 .and. l==26) print*,'wrtsfc gribit ierr=',ierr,
!    &   '   36) downward sw nir beam radiation flux (w/m**2) sfc '
!       if (ierr/=0 .and. l==27) print*,'wrtsfc gribit ierr=',ierr,
!    &   '   37) downward sw nir diffuse radiation flux (w/m**2) sfc '
      enddo
!...................................................................
!  -  clear sky radiative fluxes at toa and surface:
!
      do l = 28,33
        k = l - 8

!$omp parallel do private(j,i)
        do j = 1, lats_node_r
          do i = 1, lonr
            glolal(i,j) = rflux(i,j,l)*rtimer(k)
          enddo
        enddo

        call uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)

!       if (ierr/=0 .and. l==28) print*,'wrtsfc gribit ierr=',ierr,
!    &   '  38) cs upward long wave radiation flux (w/m**2) at toa'
!       if (ierr/=0 .and. l==29) print*,'wrtsfc gribit ierr=',ierr,
!    &   '  39) cs upward solar radiation flux (w/m**2) at toa'
!       if (ierr/=0 .and. l==30) print*,'wrtsfc gribit ierr=',ierr,
!    &   '  40) cs downward long radiation flux (w/m**2) at surface'
!       if (ierr/=0 .and. l==31) print*,'wrtsfc gribit ierr=',ierr,
!    &   '  41) cs upward solar radiation flux (w/m**2) at surface'
!       if (ierr/=0 .and. l==32) print*,'wrtsfc gribit ierr=',ierr,
!    &   '  42) cs downward solar radiation flux (w/m**2) at surface'
!       if (ierr/=0 .and. l==33) print*,'wrtsfc gribit ierr=',ierr,
!    &   '  43) cs upward long wave radiation flux (w/m**2) at surface'
      enddo
!...................................................................
!  - surface albedo (derived from radiative fluxes at surface):
!
!$omp parallel do private(j,i)
      do j = 1, lats_node_r
        do i = 1, lonr
          if (rflux(i,j,4) > 0.) then
            glolal(i,j) = rflux(i,j,3)/rflux(i,j,4) * 100.
            if (glolal(i,j) > 100.) glolal(i,j) = 100.
          else
            glolal(i,j) = 0.
          endif
        enddo
      enddo

      call uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  45) ',
!    & 'albedo (percent) land and sea surface '
!...................................................................
!  - precipitation rate (geshem unit in m, final unit = mm/s = kg/m2/s)
!
!$omp parallel do private(j,i)
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = flx_fld%geshem(i,j)*1.e3*rtime
        enddo
      enddo

      call uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  46) ',
!    & 'precipitation rate (kg/m**2/s) land and sea surface'
!...................................................................
!  - convective precipitation rate:
!
!$omp parallel do private(j,i)
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = flx_fld%bengsh(i,j)*1.e3*rtime
        enddo
      enddo

      call uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  47) ',
!    & 'convective precipitation rate (kg/m**2/s) land sea surface'
!...................................................................
!  - ground heat flux:
!
!$omp parallel do private(j,i)
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = flx_fld%gflux(i,j)*rtime
        enddo
      enddo

      call uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  48) ',
!    & 'ground heat flux (w/m**2) land and sea surface'
!...................................................................
!  - land-sea mask:
!
!     buffo = mod(slmskloc,2._kind_io8)
!$omp parallel do private(j,i)
      do j = 1, lats_node_r
        do i = 1, lonr
!         buff_mult_piecea(i,ngrid,j) = buffo(i,j)
          buff_mult_piecea(i,ngrid,j) = mod(slmskloc(i,j),2._kind_io8)
        enddo
      enddo
      ngrid = ngrid + 1

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  49) ',
!    & 'land-sea mask (1=land; 0=sea) (integer) land sea surface'
!...................................................................
!  - sea-ice concentration:
!
      call uninterprez(2,kmsk0,buffo,sfc_fld%fice,
     &                 global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  50) ',
!    & 'ice concentration (ice>0; no ice=0) (1/0) land sea surface'
!...................................................................
!  - 10m u wind:
!
      call uninterprez(2,kmsk0,buffo,flx_fld%u10m,
     &                 global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  51) ',
!    & 'u wind (m/s) height above ground'
!...................................................................
!  - 10m v wind:
!
      call uninterprez(2,kmsk0,buffo,flx_fld%v10m,
     &                 global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  52) ',
!    & 'v wind (m/s) height above ground'
!...................................................................
!  - 2m temperature:
!
      call uninterprez(2,kmsk0,buffo,sfc_fld%t2m,
     &                 global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  53) ',
!    & 'temperature (k) height above ground'
!...................................................................
!  - 2m specific humidity:
!
      call uninterprez(2,kmsk0,buffo,sfc_fld%q2m,
     &                 global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  54) ',
!    & 'specific humidity (kg/kg) height above ground'
!...................................................................
!  - surface pressure:
!
!$omp parallel do private(j,i)
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = flx_fld%psurf(i,j)
        enddo
      enddo

      call uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  55) ',
!    & 'pressure (pa) land and sea surface'
!...................................................................
!  - maximum temperature:
!
      call uninterprez(2,kmsk0,buffo,flx_fld%tmpmax,
     &                 global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  56) ',
!    & 'maximum temperature (k) height above ground'
!...................................................................
!  - minimum temperature:
!
      call uninterprez(2,kmsk0,buffo,flx_fld%tmpmin,
     &                 global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  57) ',
!    & 'minimum temperature (k) height above ground'
!...................................................................
!  - maximum specific humidity
!
      call uninterprez(2,kmsk0,buffo,flx_fld%spfhmax,
     &                 global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  58) ',
!    & 'maximum specific humidity (kg/kg) height above ground'
!...................................................................
!  - minimum specific humidity
!
      call uninterprez(2,kmsk0,buffo,flx_fld%spfhmin,
     &                 global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  59) ',
!    & 'minimum specific humidity (kg/kg) height above ground'
!...................................................................
!  - runoff (accumulative value)
!
!$omp parallel do private(j,i)
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = flx_fld%runoff(i,j)*1.e3
        enddo
      enddo

      call uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  60) ',
!    & 'runoff (kg/m**2) land and sea surface'
!...................................................................
!  - potential evaporation rate
!
!$omp parallel do private(j,i)
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = flx_fld%ep(i,j)*rtime
        enddo
      enddo

      call uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  61) ',
!    & 'potential evaporation rate (w/m**/) land and sea surface'
!...................................................................
!  - cloud work function
!
!$omp parallel do private(j,i)
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = flx_fld%cldwrk(i,j)*rtime
        enddo
      enddo

      call uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  62) ',
!    & 'cloud work function (j/kg) total atmospheric column'
!...................................................................
!  - zonal gravity wave stress
!
!$omp parallel do private(j,i)
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = flx_fld%dugwd(i,j)*rtime
        enddo
      enddo

      call uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  63) ',
!    & 'zonal gravity wave stress (n/m**2) land and sea surface'
!...................................................................
!  - meridional gravity wave stress
!
!$omp parallel do private(j,i)
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = flx_fld%dvgwd(i,j)*rtime
        enddo
      enddo

      call uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  64) ',
!    & 'meridional gravity wave stress (n/m**2) land sea surface'
!...................................................................
!  - boundary layer height
!
      call uninterprez(2,kmsk0,buffo,flx_fld%hpbl,
     &                 global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  65) ',
!    & 'boundary layer height '
!...................................................................
!  - precipitable water
!
      call uninterprez(2,kmsk0,buffo,flx_fld%pwat,
     &                 global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  66) ',
!    & 'precipitable water (kg/m**2) total atmospheric column'
!...................................................................
!  - convective clouds
!    * labeled instantaneous but actually averaged over fhswr seconds
!
!$omp parallel do private(j,i)
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = sfc_fld%cv(i,j)*100.0
        enddo
      enddo
      where(glolal >= 0.5)
        kmskcv = 1
      elsewhere
        kmskcv = 0
      endwhere

      call uninterprez(2,kmskcv,buffo,glolal,global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  67) ',
!    & 'total cloud cover (percent) convective cloud layer'
!.................................................
!  - pressure at convective cloud top
!
!$omp parallel do private(j,i)
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = 0.
          if (sfc_fld%cv(i,j) > 0.) then
            glolal(i,j) = sfc_fld%cvt(i,j)*1.e3
          endif
        enddo
      enddo

      call uninterprez(2,kmskcv,buffo,glolal,global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  68) ',
!    & 'pressure (pa) convective cloud top level'
!.................................................
!  - pressure at convective cloud bottom
!
!$omp parallel do private(j,i)
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = 0.
          if (sfc_fld%cv(i,j) > 0.) then
            glolal(i,j) = sfc_fld%cvb(i,j)*1.e3
          endif
        enddo
      enddo

      call uninterprez(2,kmskcv,buffo,glolal,global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  69) ',
!    & 'pressure (pa) convective cloud bottom level'
!.................................................
!  - sea ice thickness

      call uninterprez(2,kmsk0,buffo,sfc_fld%hice,
     &                 global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  70) ',
!    & 'sea ice thickness (m) category 1'
!.................................................
!  - volumetric soil moist content (layer 100cm and 40cm)
!
      if (lsoil > 2) then
!$omp parallel do private(j,i)
        do j = 1, lats_node_r
          do i = 1, lonr
            glolal(i,j) = sfc_fld%smc(i,3,j)
          enddo
        enddo

        call uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)

!       if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  71) ',
!    &   'volumetric soil moist content (frac) layer 100cm and 40cm'
!..........................................................
!  - volumetric soil moist content (layer 200cm and 100cm)
!
!$omp parallel do private(j,i)
        do j = 1, lats_node_r
          do i = 1, lonr
            glolal(i,j) = sfc_fld%smc(i,4,j)
          enddo
        enddo

        call uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)

!       if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  72) ',
!    &   'volumetric soil moist content(frac) layer 200cm and 100cm'
!..........................................................
!  - temperature for layer 100cm and 40cm below sfc
!
!$omp parallel do private(j,i)
        do j = 1, lats_node_r
          do i = 1, lonr
            glolal(i,j) = sfc_fld%stc(i,3,j)
          enddo
        enddo

        call uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)

!       if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  73) ',
!    &   'temp (k) betw two depth below land sfc 100cm and 40cm'
!..........................................................
!  - temperature for layer 200cm and 100cm below sfc
!
!$omp parallel do private(j,i)
        do j = 1, lats_node_r
          do i = 1, lonr
            glolal(i,j) = sfc_fld%stc(i,4,j)
          enddo
        enddo

        call uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)

!       if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  74) ',
!    &   'temp (k) betw two depth below land sfc 200cm and 100cm'
      endif
!..........................................................
!  - liquid soil moist content layer 10cm and 0cm
!
!$omp parallel do private(j,i)
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = sfc_fld%slc(i,1,j)
        enddo
      enddo

      call uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  75) ',
!    & 'liquid soil moist content (frac) layer 10cm and 0cm'
!..........................................................
!  - liquid soil moist content layer 40cm and 10cm
!
!$omp parallel do private(j,i)
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = sfc_fld%slc(i,2,j)
        enddo
      enddo

      call uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  76) ',
!    & 'liquid soil moist content (frac) layer 40cm and 10cm'
!..........................................................
!  - liquid soil moist content layer 100cm and 40cm
!
      if (lsoil > 2) then
!$omp parallel do private(j,i)
        do j = 1, lats_node_r
          do i = 1, lonr
            glolal(i,j) = sfc_fld%slc(i,3,j)
          enddo
        enddo

        call uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)

!       if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  77) ',
!    &   'liquid soil moist content (frac) layer 100cm and 40cm'
!..........................................................
!  - liquid soil moist content layer 200cm and 100cm
!
!$omp parallel do private(j,i)
        do j = 1, lats_node_r
          do i = 1, lonr
            glolal(i,j) = sfc_fld%slc(i,4,j)
          enddo
        enddo

        call uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)

!       if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  78) ',
!    &   'liquid soil moist content (frac) layer 200cm and 100cm'
      endif
!..........................................................
!  - snow depth
!
!$omp parallel do private(j,i)
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = sfc_fld%snwdph(i,j) * 0.001  ! convert from mm to m
        enddo
      enddo

      call uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  79) ',
!    & 'snow depth (m) land surface'
!..........................................................
!  - canopy water content
!
!     lbm = (slmskful == 1._kind_io8)

      call uninterprez(2,kmsk,buffo,sfc_fld%canopy,
     &                 global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  80) ',
!    & 'canopy water content (kg/m^2) land surface'
!..........................................................
!  - the following 30 records are for land mdl use
!  - surface roughness
!
!$omp parallel do private(j,i)
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = sfc_fld%zorl(i,j) * 0.01  ! convert from cm to m
        enddo
      enddo

      call uninterprez(1,kmsk0,buffo,glolal,global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  81) ',
!    & 'surface roughness (m)'
!..........................................................
!  - vegetation fraction
!
!$omp parallel do private(j,i)
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = sfc_fld%vfrac(i,j) * 100.0
        enddo
      enddo

      call uninterprez(1,kmsk,buffo,glolal,global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'   82) ',
!    & 'vegetation fraction (fractional) land surface'
!..........................................................
!  - vegetation type
!
      call uninterprez(1,kmsk,glolal,sfc_fld%vtype,
     &                 global_lats_r,lonsperlar)

!     buffo = mod(glolal,2._kind_io8)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  83) ',
!    & 'vegetation type land surface'
!..........................................................
!  - soil type
!
      call uninterprez(1,kmsk,glolal,sfc_fld%stype,
     &                 global_lats_r,lonsperlar)

!     buffo = mod(glolal,2._kind_io8)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  84) ',
!    & 'soil type land surface'
!..........................................................
!  - slope type
!
      call uninterprez(1,kmsk,glolal,sfc_fld%slope,
     &                 global_lats_r,lonsperlar)

!     buffo = mod(glolal,2._kind_io8)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  85) ',
!    & 'slope type land surface'
!..........................................................
!  - frictional velocity
!
      call uninterprez(2,kmsk0,buffo,sfc_fld%uustar,
     &                 global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  86) ',
!    & 'frictional velocity (m/s)'
!..........................................................
!  - surface height
!
      call uninterprez(1,kmsk,buffo,sfc_fld%oro,
     &                 global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  87) ',
!    & 'surface height (m)'
!..........................................................
!  - freezing precip flag
!
      call uninterprez(1,kmsk,buffo,sfc_fld%srflag,
     &                 global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  88) ',
!    & 'freezing precip flag land surface'
!..........................................................
!  - exchange coefficient ch
!
      call uninterprez(2,kmsk0,buffo,flx_fld%chh,
     &                 global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  89) ',
!    & 'exchange coefficient ch(m/s)'
!..........................................................
!  - exchange coefficient cm
!
      call uninterprez(2,kmsk0,buffo,flx_fld%cmm,
     &                 global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  90) ',
!    & 'exchange coefficient cm(m/s)'
!..........................................................
!  - potential evaporation rate
!
      call uninterprez(2,kmsk,buffo,flx_fld%epi,
     &                 global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  91) ',
!    & 'potential evaporation rate (w/m**2) land and sea surface'
!..........................................................
!  - downward long wave radiation flux (instantaneous value)
!
      if (.not. climate) then         ! do not output those fields in climate mode

        call uninterprez(2,kmsk0,buffo,flx_fld%dlwsfci,
     &                   global_lats_r,lonsperlar)

!       if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  92) ',
!    & 'downward long wave radiation flux (w/m**2) '
!..........................................................
!  - upward long wave radiation flux (instantaneous value)
!
        call uninterprez(2,kmsk0,buffo,flx_fld%ulwsfci,
     &                   global_lats_r,lonsperlar)

!       if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  93) ',
!    & 'upward long wave radiation flux (w/m**2)'
!..........................................................
!  - upward short wave radiation flux (instantaneous value)
!
        call uninterprez(2,kmsk0,buffo,flx_fld%uswsfci,
     &                   global_lats_r,lonsperlar)

!       if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  94) ',
!    & 'upward short wave radiation flux (w/m**2)'
!..........................................................
!  - downward short wave radiation flux (instantaneous value)
!
        call uninterprez(2,kmsk0,buffo,flx_fld%dswsfci,
     &                   global_lats_r,lonsperlar)

!       if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  95) ',
!    & 'downward short wave radiation flux (w/m**2)'
!..........................................................
!  - sensible heat flux (instantaneous value)
!
        call uninterprez(2,kmsk0,buffo,flx_fld%dtsfci,
     &                   global_lats_r,lonsperlar)

!       if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  96) ',
!    & 'sensible heat flux (w/m**2) land and sea surface'
!..........................................................
!  - latent heat flux (instantaneous value)
!
        call uninterprez(2,kmsk0,buffo,flx_fld%dqsfci,
     &                   global_lats_r,lonsperlar)

!       if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  97) ',
!    & 'latent heat flux (w/m**2) land and sea surface'
!..........................................................
!  - ground heat flux (instantaneous value)
!
        call uninterprez(2,kmsk,buffo,flx_fld%gfluxi,
     &                   global_lats_r,lonsperlar)

!       if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  98) ',
!    & 'ground heat flux (w/m**2) land and sea surface'
      endif   ! if_.not._climate
!..........................................................
!  - surface runoff
!
!$omp parallel do private(j,i)
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = flx_fld%runoff(i,j) * 1000.0
        enddo
      enddo

      call uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  99) ',
!    & 'surface runoff (kg/m^2) land surface'
!..........................................................
!  - lowest model level temp
!
      call uninterprez(2,kmsk0,buffo,flx_fld%t1,
     &                 global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  100) ',
!    & 'lowest model level temp (k)'
!..........................................................
!  - lowest model specific humidity
!
      call uninterprez(2,kmsk0,buffo,flx_fld%q1,
     &                 global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  101) ',
!    & 'lowest model specific humidity (kg/kg)'
!..........................................................
!  - lowest model u wind
!
      call uninterprez(2,kmsk0,buffo,flx_fld%u1,
     &                 global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  102) ',
!    & 'lowest model u wind (m/s)'
!..........................................................
!  - lowest model v wind
!
      call uninterprez(2,kmsk0,buffo,flx_fld%v1,
     &                 global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  103) ',
!    & 'lowest model v wind (m/s)'
!..........................................................
!  - lowest model level height
!
      call uninterprez(2,kmsk,buffo,flx_fld%zlvl,
     &                 global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  104) ',
!    & 'lowest model level height (m) land surface'
!..........................................................
!  - direct evaporation from bare soil
!
!$omp parallel do private(j,i)
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = flx_fld%evbsa(i,j) * rtime
        enddo
      enddo

      call uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  105) ',
!    & 'direct evaporation from bare soil(w/m^2) land surface'
!..........................................................
!  - canopy water evaporation
!
!$omp parallel do private(j,i)
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = flx_fld%evcwa(i,j) * rtime
        enddo
      enddo

      call uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  106) ',
!    & 'canopy water evaporation(w/m^2) land surface'
!..........................................................
!  - transpiration
!
!$omp parallel do private(j,i)
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = flx_fld%transa(i,j) * rtime
        enddo
      enddo

      call uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  107) ',
!    & 'transpiration (w/m^2) land surface'
!..........................................................
!  - snow sublimation
!
!$omp parallel do private(j,i)
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = flx_fld%sbsnoa(i,j) * rtime
        enddo
      enddo

      call uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  108) ',
!    & 'snow sublimation (w/m^2) land surface'
!..........................................................
!  - snow cover
!
!$omp parallel do private(j,i)
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = flx_fld%snowca(i,j) * rtime * 100.0
        enddo
      enddo

      call uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  109) ',
!    & 'snow cover (fraction) land surface'
!..........................................................
!  - total column soil moisture
!
!$omp parallel do private(j,i)
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = flx_fld%soilm(i,j) * 1000.0 ! convert from m to (mm)kg/m^2
        enddo
      enddo

      call uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  110) ',
!    & 'total column soil moisture (kg/m^2) land surface'
!..........................................................
!  - snow phase-change heat flux
!
!$omp parallel do private(j,i)
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = flx_fld%snohfa(i,j) * rtime
        enddo
      enddo

      call uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  111) ',
!    & 'snow phase-change heat flux [w/m^2] land surface'
!..........................................................
!  - wilting point
!
!$omp parallel do private(j,i)
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = flx_fld%smcwlt2(i,j)
        enddo
      enddo

      call uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  112) ',
!    & 'wilting point [fraction] land surface'
!..........................................................
!  - field capacity
!
!$omp parallel do private(j,i)
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = flx_fld%smcref2(i,j)
        enddo
      enddo

      call uninterprez(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  113) ',
!    & 'field capacity [fraction] land surface'
!..........................................................
!  - accumulated sunshine duration time
!
!$omp parallel do private(j,i)
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = flx_fld%suntim(i,j)
        enddo
      enddo

      call uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  114) ',
!    & 'accumulated sunshine duration (sec)'
!..........................................................
!
!..........................................................
!! - frozen precipitation fraction
! 
!$omp parallel do private(j,i)
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = flx_fld%sr(i,j)
        enddo
      enddo

      call uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  123) ',
!    &  'frozen precipitation fraction '
!..........................................................
!! - aerosols optical depth at 550nm

!!$omp parallel do private(j,i)
!!    do j = 1, lats_node_r
!!      do i = 1, lonr
!!        glolal(i,j) = rflux(i,j,34)*rtimsw
!!      enddo
!!    enddo

!!    call uninterprez(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)

!     if (ierr /= 0) print*,'wrtsfc gribit ierr=',ierr,'  124) ',
!    &  'total aerosol optical depth at 550 nm wavelength'
!..........................................................

      if (me == ioproc) then
        print *,'grib flux file written ',fhour,idate,noflx
      endif
!
      return
!...................................
      end subroutine wrtflx_a
!-----------------------------------


!-----------------------------------
      subroutine wrtflx_w                                               &
!...................................
!  ---  inputs:
     &     ( ioproc,noflx,zhour,fhour,idate,colat1,secswr,seclwr,       &
     &       slmsk,global_lats_r,lonsperlar)
!  ---  outputs: ( none )
! =================   subprogram documentation block   ================ !
!                                                                       !
!    this program writes out surface flux file in grib form.            !
!                                                                       !
!    usage:        call wrtflx_w                                        !
!                                                                       !
!    subprograms called:                                                !
!                  uninterpred, unsplit2d, idsdef, gribit, wryte        !
!                                                                       !
!    attributes:                                                        !
!      language:   fortran 90                                           !
!                                                                       !
!    external modules referenced:                                       !
!      'module resol_def             in 'resol_def.f'                   !
!      'module mod_state             in 'wrtout.f'                      !
!      'module layout1               in 'layout1.f'                     !
!      'module sig_io                in 'sig_io.f'                      !
!      'module namelist_def          in 'namelist_def.f'                !
!                                                                       !
!  ====================  defination of variables  ====================  !
!                                                                       !
!    input variables:                                            size   !
!      ioproc        : integer, processor id num                   1    !
!      noflx         : integer,                                    1    !
!      zhour         : real, accumulator reset time in hr.         1    !
!      fhour         : real, forecast time in hr.                  1    !
!      idate         : integer, init cond time (h,m,d,y)           4    !
!      colat1        : real, colatitude                            1    !
!      secswr        : real, sw radiation call duration in sec     1    !
!      seclwr        : real, lw radiation call duration in sec     1    !
!      slmsk         : real, sea-lane mask            (lonr,lats_node_r)!
!      global_lats_r : integer, index for global latitudes        latr  !
!      lonsperlar    : integer, num of pts on a given lat circle  latr  !
!                                                                       !
!  ======================  end of definations  =======================  !
!
      use resol_def
      use mod_state
      use layout1
      use sig_io
      use namelist_def
!
      implicit none
!
!  ---  inputs:
      integer, intent(in) :: ioproc, noflx, idate(4),                   &
     &       global_lats_r(latr), lonsperlar(latr)

      real (kind=kind_io8), intent(in) :: zhour, fhour, colat1,         &
     &                                    secswr, seclwr
      real (kind=kind_io8), intent(in) :: slmsk(lonr,lats_node_r)

!  ---  outputs: (none)

!  ---  parameters:
      integer, parameter :: nfld = 29

      integer, parameter ::        iprs  =  1, ihgt  =  7, itemp = 11,  &
     &     itmx  = 15, itmn  = 16, iznlw = 33, imerw = 34, isphum= 51,  &
     &     ipwat = 54, ipcpr = 59, isr=   194, isnowd= 65, isnod = 66,  &
     &     icldf = 71, iccldf= 72, islmsk= 81, izorl = 83, ialbdo= 84,  &
     &     istc  = 86, iveg  = 87, irnof = 90, icemsk= 91, isik  = 92,  &
     &     ilhflx=121, ishflx=122, izws  =124, imws  =125, irst  =140,  &
     &     isoilm=144, iep   =145, icldwk=146, izgw  =147, imgw  =148,  &
     &     ighflx=155, icsusw=160, icsdsw=161, icsulw=162, icsdlw=163,  &
     &     iuswfc=160, idswfc=161, iulwfc=162, idlwfc=163, inswfc=164,  &
     &     inlwfc=165, idswvb=166, idswvd=167, idswnb=168, idswnd=169,  &
     &     icmm  =179, isuntm=191, isbs  =198, ievbs =199, ievcw =200,  &
     &     iuvbf =200, iuvbfc=201, idswf =204, idlwf =205, iqmx  =204,  &
     &     iqmn  =205, ichh  =208, itran =210, iuswf =211, iulwf =212,  &
     &     icpcpr=214, ismcwlt=219,ismcref=220,ihpbl =221, islo  =222,  &
     &     icnp  =223, istp  =224, ivtp  =225, isnohf=229, isrf  =235,  &
     &     isnc  =238, iust  =253

      integer, parameter ::        isfc  =  1, itoa  =  8, ielev =105,  &
     &     isglev=109, idbls =111, i2dbls=112, islc  =160, icolmn=200,  &
     &     iblbl =209, ibltl =210, ibllyr=211, ilcbl =212, ilctl =213,  &
     &     ilclyr=214, imcbl =222, imctl =223, imclyr=224, ihcbl =232,  &
     &     ihctl =233, ihclyr=234, icvbl =242, icvtl =243, icvlyr=244

      integer, parameter ::        ifhour=  1, ifday =  2, inst  = 10,  &
     &     iwin  =  2, iavg  =  3, iacc  =  4

!  ---  local variables:
      integer :: k, k4, l
      integer :: ilpds, iyr, imo, ida, ihr, ifhr, ithr, lg, ierr
      integer :: ids(255), ids_iq, ids_uvb
      integer, dimension(lonr,lats_node_r) :: kmsk, kmsk0, kmskcv

      real (kind=kind_io8) :: rtimer(nfld), rtime, rtimsw, rtimlw
      real (kind=kind_io8) :: cl1, si(levp1)

      real (kind=kind_io4), dimension(lonr*latr) :: buff1l, wrkga

      real (kind=kind_io8), dimension(lonr*latr) :: slmskful
      real (kind=kind_io8), dimension(lonr,lats_node_r) :: slmskloc

      logical(1) :: lbm(lonr*latr)
      character  :: g(200+lonr*latr*(16+1)/8)

!  ---  label indexes:
      integer, dimension(nfld) :: ipur, itlr
      data  ipur / iulwf , iuswf , iuswf , idswf , icldf , iprs  ,      &
     &             iprs  , itemp , icldf , iprs  , iprs  , itemp ,      &
     &             icldf , iprs  , iprs  , itemp , iuvbf , iuvbfc,      &
     &             idswf , icsulw, icsusw, icsdlw, icsusw, icsdsw,      &
     &             icsulw, idswvb, idswvd, idswnb, idswnd /

      data  itlr / itoa  , itoa  , isfc  , isfc  , ihclyr, ihctl ,      &
     &             ihcbl , ihctl , imclyr, imctl , imcbl , imctl ,      &
     &             ilclyr, ilctl , ilcbl , ilctl , isfc  , isfc  ,      &
     &             itoa  , itoa  , itoa  , isfc  , isfc  , isfc  ,      &
     &             isfc  , isfc  , isfc  , isfc  , isfc   /
!
!===>  begin here
!
!     ngrid = 0 + ngrids_sfcc + 1
      ngrid = 0 + ngrids_sfcc + 1 + ngrids_nst
      g = ' '

      kmsk  = nint(slmsk)
      kmsk0 = 0

      call unsplit2z(buff1l,global_lats_r)
      slmskful = buff1l

!     do k = 1, nfxr
!       do j = 1, lats_node_r
!         do i = 1, lonr
!           rflux(i,j,k) = fluxr(i,k,j)
!         enddo
!       enddo
!     enddo

!  ---  set defalt decimal scaling factor array
      ids = 0
      ids_iq      = 5      ! used for iqmx/iqmn due to conflict with idswf/idlwf
      ids_uvb     = 2      ! used for iuvbf/iuvbfc due to conflict with ievcw
      call idsdef(1,ids)

!  ---  make adjustment if diff from the defaults
      ids(ihgt)   = 3      ! (007) geopotential height            (def=1)
      ids(itemp)  = 3      ! (011) temperature                    (def=1)
      ids(iznlw)  = 2      ! (033) zonal wind                     (def=1)
      ids(imerw)  = 2      ! (034) meridional wind                (def=1)
      ids(isphum) = 6      ! (051) specific humidity              (def=5)
      ids(ipcpr)  = 6      ! (059) precipitation rate             (def=6)
      ids(isnowd) = 5      ! (065) water equivalent of snow depth (def=0)
      ids(isnod)  = 6      ! (066) snow depth/mixed-layer depth   (def=2)
      ids(izorl)  = 4      ! (083) roughness                      (def=5)
      ids(istc)   = 4      ! (086) soil wetness                   (def=0)
      ids(iveg)   = 2      ! (087) vegetation/salinity            (def=0)
      ids(irnof)  = 5      ! (090) runoff                         (def=1)
      ids(icemsk) = 3      ! (091) ice concentration              (def=2)
      ids(isik)   = 2      ! (092) ice thickness                  (def=2)
      ids(isoilm) = 4      ! (144) vol soil moisture content      (def=3)
      ids(icmm)   = 4      ! (179) exchange coefficient           (def not set) *table 130
      ids(ievcw)  = 2      ! (200) canopy water evaporation       (def not set) *table 2
!     ids(iuvbf)  = 0      ! (200) sfc dnwd uv-b flux tot sky     (def not set) *table 129
!     ids(iuvbfc) = 0      ! (201) sfc dnwd uv-b flux clr sky     (def not set) *table 129
!     ids(iqmx)   = 5      ! (204) max specific humidity          (def=0) *** table 133
!     ids(iqmn)   = 5      ! (205) min specific humidity          (def=0) *** table 133
      ids(ichh)   = 4      ! (208) exchange coefficient           (def not set)
      ids(icpcpr) = 6      ! (214) convective precipitation rate  (def=6) ***
      ids(ismcwlt)= 4      ! (219) wilting point                  (def=1) *** table 130
      ids(ismcref)= 4      ! (220) field capacity                 (def=4  *** table 130
      ids(icnp)   = 5      ! (223) plant canopy surface water     (def=1)
      ids(isrf)   = 5      ! (235) storm surface runoff           (def=1)
      ids(isnc)   = 3      ! (238) snow cover                     (def=0)
      ids(iust)   = 3      ! (253) friction velocity              (def not set)

      ilpds = 28
      if (icen2 == 2) ilpds = 45

      iens(1) = 1
      iens(2) = ienst
      iens(3) = iensi
      iens(4) = 1
      iens(5) = 255

      iyr     = idate(4)
      imo     = idate(2)
      ida     = idate(3)
      ihr     = idate(1)
      ifhr    = nint(zhour)
      ithr    = nint(fhour)

      if (fhour > zhour) then
        rtime = 1./(3600.*(fhour-zhour))
      else
        rtime = 0.
      endif

      if (secswr > 0.) then
        rtimsw = 1./secswr
      else
        rtimsw = 1.
      endif

      if (seclwr > 0.) then
        rtimlw = 1./seclwr
      else
        rtimlw = 1.
      endif

      rtimer    = rtimsw
      rtimer( 1)= rtimlw
      rtimer(20)= rtimlw       ! csulf_toa
      rtimer(22)= rtimlw       ! csdlf_sfc
      rtimer(25)= rtimlw       ! csulf_sfc
      cl1       = colat1

      if (me == ioproc) then
!..........................................................
!  - zonal component of momentum flux:
!
!     glolal = dusfc*rtime


        call unsplit2z(wrkga,global_lats_r)

        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,izws,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(izws),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  01)',
     &     'zonal compt of momentum flux (n/m**2) land and sea surface'
        endif

!..........................................................
!  - meridional component of momentum flux:
!
!     glolal = dvsfc*rtime

        call unsplit2z(wrkga,global_lats_r)

        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,imws,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(imws),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  02) ',
     &     'merid compt of momentum flux (n/m**2) land and sea surface'
        endif
!..........................................................
!  - sensible heat flux:
!
!     glolal = dtsfc*rtime

        call unsplit2z(wrkga,global_lats_r)

        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,ishflx,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(ishflx),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  03) ',
     &     'sensible heat flux (w/m**2) land and sea surface'
        endif
!..........................................................
!  - latent heat flux:
!
!     glolal = dqsfc*rtime

        call unsplit2z(wrkga,global_lats_r)

        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,ilhflx,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(ilhflx),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  04) ',
     &     'latent heat flux (w/m**2) land and sea surface'
        endif
!..........................................................
!  - surface temperature:
!
        call unsplit2z(wrkga,global_lats_r)

        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,itemp,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(itemp),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  05) ',
     &     'temperature (k) land and sea surface'
          stop
        endif
!..........................................................
!  - volumetric soil moist content at layer 10cm and 0cm:
!
!     glolal(:,:) = smc(:,1,:)

        call unsplit2z(wrkga,global_lats_r)

        lbm = (slmskful == 1._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              1,isoilm,i2dbls,0,10,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(isoilm),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  06) ',
     &     'volumetric soil moist content (frac) layer 10cm and 0cm'
        endif
!..........................................................
!  - volumetric soil moist content at layer 40cm and 10cm:
!
!     glolal(:,:) = smc(:,2,:)

        call unsplit2z(wrkga,global_lats_r)

        lbm = (slmskful == 1._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              1,isoilm,i2dbls,10,40,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(isoilm),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  07) ',
     &     'volumetric soil moist content (frac) layer 40cm and 10cm'
        endif
!..........................................................
!  - temperature at layer 10cm and 0cm:
!
!     glolal(:,:) = stc(:,1,:)

        call unsplit2z(wrkga,global_lats_r)

        lbm = (slmskful == 1._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              1,itemp,i2dbls,0,10,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(itemp),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  08) ',
     &    'temp (k) layer betw two depth below land sfc 10cm and 0cm'
        endif
!..........................................................
!  - temperature at layer 40cm and 10cm:
!
!     glolal(:,:) = stc(:,2,:)

        call unsplit2z(wrkga,global_lats_r)

        lbm = (slmskful == 1._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              1,itemp,i2dbls,10,40,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(itemp),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  09) ',
     &     'temp (k) layer betw 2 depth below land sfc 40cm and 10cm'
        endif
!..........................................................
!  - water equivalent of accummulated snow depth:
!
        call unsplit2z(wrkga,global_lats_r)

        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,isnowd,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(isnowd),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  10) ',
     &     'water equiv of accum snow depth (kg/m**2) land sea surface'
        endif
!..........................................................
!  - total sky radiation fluxes at toa and surface:
!
        do k = 1, 4
!       do j = 1, lats_node_r
!         do i = 1, lonr
!           glolal(i,j) = rflux(i,j,k)*rtimer(k)
!         enddo
!       enddo

          call unsplit2z(wrkga,global_lats_r)

          call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &                0,ipur(k),itlr(k),0,0,iyr,imo,ida,ihr,
     &                ifhour,ifhr,ithr,iavg,0,0,icen2,ids(ipur(k)),iens,
     &                0.,0.,0.,0.,0.,0.,g,lg,ierr)

          if (ierr == 0) then
            call wryte(noflx,lg,g)
          else
            if (k == 1) print*,'wrtsfc gribit ierr=',ierr,'  11) ',
     &       'upward long wave radiation flux (w/m**2) at toa'
            if (k == 2) print*,'wrtsfc gribit ierr=',ierr,'  12) ',
     &       'upward solar radiation flux (w/m**2) at toa'
            if (k == 3) print*,'wrtsfc gribit ierr=',ierr,'  13) ',
     &       'upward solar radiation flux (w/m**2) at surface'
            if (k == 4) print*,'wrtsfc gribit ierr=',ierr,'  14) ',
     &       'downward solar radiation flux (w/m**2) at surface'
          endif
      enddo
!..........................................................
!  - for high, mid, low cloud (cover, pressure, temperature)
!
        lab_do_cloud : do k = 5, 7    ! (high, mid, low clouds)
        k4 = 4 + (k-5)*4

!  - cloud cover (h,m,l):
!
!       do j = 1, lats_node_r
!         do i = 1, lonr
!           glolal(i,j) = rflux(i,j,k)*100.*rtimsw
!         enddo
!       enddo
!       where (glolal >= 0.5)
!         kmskcv=1
!       elsewhere
!         kmskcv=0
!       endwhere

          call unsplit2z(wrkga,global_lats_r)

          l = k4 + 1
          lbm = (wrkga >= 0.5_kind_io8)
          call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &                0,ipur(l),itlr(l),0,0,iyr,imo,ida,ihr,
     &                ifhour,ifhr,ithr,iavg,0,0,icen2,ids(ipur(l)),iens,
     &                0.,0.,0.,0.,0.,0.,g,lg,ierr)

          if (ierr == 0) then
            call wryte(noflx,lg,g)
          else
            if (k == 5) print*,'wrtsfc gribit ierr=',ierr,'  15) ',
     &       'total cloud cover (percent) high cloud layer'
            if (k == 6) print*,'wrtsfc gribit ierr=',ierr,'  19) ',
     &       'total cloud cover (percent) middle cloud layer'
            if (k == 7) print*,'wrtsfc gribit ierr=',ierr,'  23) ',
     &       'total cloud cover (percent) low cloud layer'
          endif

!  - pressure at cloud top:
!
!       do j = 1, lats_node_r
!         do i = 1, lonr
!           if (rflux(i,j,k) > 0.)then
!             glolal(i,j) = rflux(i,j,k+3)*1000./rflux(i,j,k)
!           else
!             glolal(i,j) = 0.
!           endif
!         enddo
!       enddo

          call unsplit2z(wrkga,global_lats_r)

          l = k4 + 2
          call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &                1,ipur(l),itlr(l),0,0,iyr,imo,ida,ihr,
     &                ifhour,ifhr,ithr,iavg,0,0,icen2,ids(ipur(l)),iens,
     &                0.,0.,0.,0.,0.,0.,g,lg,ierr)

          if (ierr == 0) then
            call wryte(noflx,lg,g)
          else
            if (k == 5) print*,'wrtsfc gribit ierr=',ierr,'  16) ',
     &       'pressure (pa) high cloud top level'
            if (k == 6) print*,'wrtsfc gribit ierr=',ierr,'  20) ',
     &       'pressure (pa) middle cloud top level'
            if (k == 7) print*,'wrtsfc gribit ierr=',ierr,'  24) ',
     &       'pressure (pa) low cloud top level'
          endif

!  - pressure at cloud base:
!
!       do j = 1, lats_node_r
!         do i = 1, lonr
!           if (rflux(i,j,k) > 0.) then
!             glolal(i,j) = rflux(i,j,k+6)*1000./rflux(i,j,k)
!           else
!             glolal(i,j) = 0.
!           endif
!         enddo
!       enddo

          call unsplit2z(wrkga,global_lats_r)

          l = k4 + 3
          call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &                1,ipur(l),itlr(l),0,0,iyr,imo,ida,ihr,
     &                ifhour,ifhr,ithr,iavg,0,0,icen2,ids(ipur(l)),iens,
     &                0.,0.,0.,0.,0.,0.,g,lg,ierr)

          if (ierr == 0) then
            call wryte(noflx,lg,g)
          else
            if (k == 5) print*,'wrtsfc gribit ierr=',ierr,'  17) ',
     &       'pressure (pa) high cloud bottom level'
            if (k == 6) print*,'wrtsfc gribit ierr=',ierr,'  21) ',
     &       'pressure (pa) middle cloud bottom level'
            if (k == 7) print*,'wrtsfc gribit ierr=',ierr,'  25) ',
     &       'pressure (pa) low cloud bottom level'
        endif

!  - temperature at cloud top:
!
!       do j = 1, lats_node_r
!         do i = 1, lonr
!           if (rflux(i,j,k) > 0.) then
!             glolal(i,j) = rflux(i,j,k+9)/rflux(i,j,k)
!           else
!             glolal(i,j) = 0.
!           endif
!         enddo
!       enddo

          call unsplit2z(wrkga,global_lats_r)

          l = k4 + 4
          call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &                1,ipur(l),itlr(l),0,0,iyr,imo,ida,ihr,
     &                ifhour,ifhr,ithr,iavg,0,0,icen2,ids(ipur(l)),iens,
     &                0.,0.,0.,0.,0.,0.,g,lg,ierr)

          if (ierr == 0) then
            call wryte(noflx,lg,g)
          else
            if (k == 5) print*,'wrtsfc gribit ierr=',ierr,'  18) ',
     &       'temperature (k) high cloud top level'
            if (k == 6) print*,'wrtsfc gribit ierr=',ierr,'  22) ',
     &       'temperature (k) middle cloud top level'
            if (k == 7) print*,'wrtsfc gribit ierr=',ierr,'  26) ',
     &       'temperature (k) low cloud top level'
          endif

        enddo  lab_do_cloud

!...................................................................
!  - total cloud amount:
!
!     do j = 1, lats_node_r
!       do i = 1, lonr
!         glolal(i,j) = rflux(i,j,17)*100.*rtimsw
!       enddo
!     enddo

        call unsplit2z(wrkga,global_lats_r)

        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,icldf,icolmn,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(icldf),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  27) ',
     &     'total cloud cover (percent) total atmospheric column'
        endif
!.................................................
!  - boundary layer cloud amount:
!
!     do j = 1, lats_node_r
!       do i = 1, lonr
!         glolal(i,j) = rflux(i,j,18)*100.*rtimsw
!       enddo
!     enddo

        call unsplit2z(wrkga,global_lats_r)

        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,icldf,ibllyr,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(icldf),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  28) ',
     &     'total cloud cover (percent) boundary layer cloud layer'
        endif
!..........................................................
!  - surface downeard lw fluxes: (use the surface temp adjusted quantities
!    to replace the original on in rec 19 of fluxr)
!
!     glolal = dlwsfc*rtime

        call unsplit2z(wrkga,global_lats_r)

        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,idlwf,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(idlwf),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  29) ',
     &     'downward long wave radiation flux (w/m**2) at surface'
        endif
!..........................................................
!  - surface upward lw fluxes: (use the one recalc'ed from surface temp
!    to replace the original one in rec 20 of fluxr)
!
!     glolal = ulwsfc*rtime

        call unsplit2z(wrkga,global_lats_r)

        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,iulwf,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(iulwf),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  30) ',
     &     'upward long wave radiation flux (w/m**2) at surface'
        endif
!..........................................................
!  - uv-b flux at surface for total sky:
!
!     do j = 1, lats_node_r
!       do i = 1, lonr
!         glolal(i,j) = rflux(i,j,21)*rtimsw
!       enddo
!     enddo

        call unsplit2z(wrkga,global_lats_r)

        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,129,icen,igen,
     &              0,iuvbf,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids_uvb,iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  31) ',
     &     'uv-b downward solar flux (w/m**2) at surface'
        endif
!..........................................................
!  - uv-b flux at surface for clear sky:
!
!     do j = 1, lats_node_r
!       do i = 1, lonr
!         glolal(i,j) = rflux(i,j,22)*rtimsw
!       enddo
!     enddo

        call unsplit2z(wrkga,global_lats_r)

        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,129,icen,igen,
     &              0,iuvbfc,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids_uvb,iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  32) ',
     &     'clear sky uv-b downward solar flux (w/m**2) at surface'
        endif
!..........................................................
!  - incoming solar radiation at toa:
!
!     do j = 1, lats_node_r
!       do i = 1, lonr
!         glolal(i,j) = rflux(i,j,23)*rtimsw
!       enddo
!     enddo

        call unsplit2z(wrkga,global_lats_r)

        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,ipur(19),itlr(19),0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(ipur(19)),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  32) ',                   &
     &     'downward solar radiation flux (w/m**2) at toa'
        endif
!..........................................................
!  - sw downward surface flux components:
!
        do l = 24, 27
          k = l + 2

!       do j = 1, lats_node_r
!         do i = 1, lonr
!           glolal(i,j) = rflux(i,j,l)*rtimsw
!         enddo
!       enddo

          call unsplit2z(wrkga,global_lats_r)

          call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &                0,ipur(k),itlr(k),0,0,iyr,imo,ida,ihr,
     &                ifhour,ifhr,ithr,iavg,0,0,icen2,ids(ipur(k)),iens,
     &                0.,0.,0.,0.,0.,0.,g,lg,ierr)

          if (ierr == 0) then
            call wryte(noflx,lg,g)
          else
            if (l == 24) print*,'wrtsfc gribit ierr=',ierr,'  34) ',
     &       'downward sw uv+vis beam radiation flux (w/m**2) sfc '
            if (l == 25) print*,'wrtsfc gribit ierr=',ierr,'  35) ',
     &       'downward sw uv+vis diffuse radiation flux (w/m**2) sfc'
            if (l == 26) print*,'wrtsfc gribit ierr=',ierr,'  36) ',
     &       'downward sw nir beam radiation flux (w/m**2) sfc '
            if (l == 27) print*,'wrtsfc gribit ierr=',ierr,'  37) ',
     &       'downward sw nir diffuse radiation flux (w/m**2) sfc '
         endif
      enddo
!..........................................................
!  -  clear sky radiative fluxes at toa and surface:
!
        do l = 28, 33
          k = l - 8

!       do j = 1, lats_node_r
!         do i = 1, lonr
!           glolal(i,j) = rflux(i,j,l)*rtimer(k)
!         enddo
!       enddo

          call unsplit2z(wrkga,global_lats_r)

          call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &                0,ipur(k),itlr(k),0,0,iyr,imo,ida,ihr,
     &                ifhour,ifhr,ithr,iavg,0,0,icen2,ids(ipur(k)),iens,
     &                0.,0.,0.,0.,0.,0.,g,lg,ierr)

          if (ierr == 0) then
            call wryte(noflx,lg,g)
          else
            if (l == 28) print*,'wrtsfc gribit ierr=',ierr,'  38) ',
     &       'cs upward long wave radiation flux (w/m**2) at toa'
            if (l == 29) print*,'wrtsfc gribit ierr=',ierr,'  39) ',
     &       'cs upward solar radiation flux (w/m**2) at toa'
            if (l == 30) print*,'wrtsfc gribit ierr=',ierr,'  40) ',
     &       'cs downward long radiation flux (w/m**2) at surface'
            if (l == 31) print*,'wrtsfc gribit ierr=',ierr,'  41) ',
     &       'cs upward solar radiation flux (w/m**2)  at surface'
            if (l == 32) print*,'wrtsfc gribit ierr=',ierr,'  42) ',
     &       'cs downward solar radiation flux (w/m**2) at surface'
            if (l == 33) print*,'wrtsfc gribit ierr=',ierr,'  43) ',
     &       'cs upward long wave radiation (w/m**2) at surface'
          endif
        enddo
!...................................................................
!  - surface albedo (derived from radiative fluxes at surface):
!
!     do j = 1, lats_node_r
!       do i = 1, lonr
!         if (rflux(i,j,4) > 0.) then
!           glolal(i,j) = rflux(i,j,3)/rflux(i,j,4) * 100.
!           if (glolal(i,j) > 100.) glolal(i,j) = 100.
!         else
!           glolal(i,j) = 0.
!         endif
!       enddo
!     enddo

        call unsplit2z(wrkga,global_lats_r)

        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,ialbdo,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(ialbdo),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  45) ',
     &     'aalbedo (percent) land and sea surface '
        endif
!...................................................................
!  - precipitation rate (geshem unit in m, final unit = mm/s = kg/m2/s)
!
!     glolal = geshem*1.e3*rtime

        call unsplit2z(wrkga,global_lats_r)

        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,ipcpr,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(ipcpr),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  46) ',
     &     'precipitation rate (kg/m**2/s) land and sea surface'
        endif
!...................................................................
!  - convective precipitation rate:
!
!     glolal = bengsh*1.e3*rtime

        call unsplit2z(wrkga,global_lats_r)

        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,icpcpr,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(icpcpr),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  47) ',
     &     'convective precipitation rate (kg/m**2/s) at surface'
        endif
!...................................................................
!  - ground heat flux:
!
!     glolal = gflux*rtime

      call unsplit2z(wrkga,global_lats_r)

        lbm = (slmskful /= 0._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              1,ighflx,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(ighflx),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  48) ',
     &     'ground heat flux (w/m**2) land and sea surface'
        endif
!...................................................................
!  - land-sea mask:
!
!     buffo = mod(slmskloc,2._kind_io8)

        call unsplit2z(wrkga,global_lats_r)

        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,islmsk,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(islmsk),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  49) ',
     &     'land-sea mask (1=land; 0=sea) (integer) land sea surface'
        endif
!...................................................................
!  - sea-ice concentration:
!
        call unsplit2z(wrkga,global_lats_r)

        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,icemsk,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(icemsk),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  50) ',
     &     'ice concentration (ice>0; no ice=0) (1/0) land sea surface'
        endif
!...................................................................
!  - 10m u wind:
!
        call unsplit2z(wrkga,global_lats_r)

        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,iznlw,ielev,0,10,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(iznlw),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  51) ',
     &     'u wind (m/s) height above ground'
        endif
!...................................................................
!  - 10m v wind:
!
        call unsplit2z(wrkga,global_lats_r)

        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,imerw,ielev,0,10,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(imerw),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  52) ',
     &     'v wind (m/s) height above ground'
        endif
!...................................................................
!  - 2m temperature:
!
        call unsplit2z(wrkga,global_lats_r)

        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,itemp,ielev,0,2,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(itemp),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  53) ',
     &     'temperature (k) height above ground'
        endif
!...................................................................
!  - 2m specific humidity:
!
        call unsplit2z(wrkga,global_lats_r)

        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,isphum,ielev,0,2,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(isphum),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  54) ',
     &     'specific humidity (kg/kg) height above ground'
        endif
!...................................................................
!  - surface pressure:
!
!     glolal = psurf

        call unsplit2z(wrkga,global_lats_r)

        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,iprs,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(iprs),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  55) ',
     &     'pressure (pa) land and sea surface'
        endif
!...................................................................
!  - maximum temperature:
!
        call unsplit2z(wrkga,global_lats_r)

        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,itmx,ielev,0,2,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iwin,0,0,icen2,ids(itmx),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  56) ',
     &     'maximum temperature (k) height above ground'
        endif
!...................................................................
!  - minimum temperature:
!
        call unsplit2z(wrkga,global_lats_r)

        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,itmn,ielev,0,2,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iwin,0,0,icen2,ids(itmn),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  57) ',
     &     'minimum temperature (k) height above ground'
        endif
!...................................................................
!  - maximum specific humidity
!
        call unsplit2z(wrkga,global_lats_r)

        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,133,icen,igen,
     &              0,iqmx,ielev,0,2,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iwin,0,0,icen2,ids_iq,iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  58) ',
     &     'maximum specific humidity (kg/kg) height above ground'
        endif
!...................................................................
!  - minimum specific humidity
!
        call unsplit2z(wrkga,global_lats_r)

        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,133,icen,igen,
     &              0,iqmn,ielev,0,2,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iwin,0,0,icen2,ids_iq,iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  59) ',
     &     'minimum specific humidity (kg/kg) height above ground'
        endif
!...................................................................
!  - runoff, the output unit of runoff is kg/m2 (accumulative value)
!
!     glolal = runoff * 1.e3

        call unsplit2z(wrkga,global_lats_r)

        lbm = (slmskful /= 0._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              1,irnof,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iacc,0,0,icen2,ids(irnof),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  60) ',
     &     'runoff (kg/m**2) land and sea surface'
        endif
!...................................................................
!  - potential evaporation rate
!
!     glolal = ep * rtime

        call unsplit2z(wrkga,global_lats_r)

        lbm = (slmskful /= 0._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              1,iep,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(iep),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  61) ',
     &     'potential evaporation rate (w/m**/) land and sea surface'
        endif
!...................................................................
!  - cloud work function
!
!     glolal = cldwrk * rtime

        call unsplit2z(wrkga,global_lats_r)

        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,icldwk,icolmn,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(icldwk),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  62) ',
     &     'cloud work function (j/kg) total atmospheric column'
        endif
!...................................................................
!  - zonal gravity wave stress
!
!     glolal = dugwd*rtime

        call unsplit2z(wrkga,global_lats_r)

        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,izgw,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(izgw),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  63) ',
     &     'zonal gravity wave stress (n/m**2) land and sea surface'
        endif
!...................................................................
!  - meridional gravity wave stress
!
!     glolal = dvgwd*rtime

        call unsplit2z(wrkga,global_lats_r)

        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,imgw,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(imgw),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  64) ',
     &     'meridional gravity wave stress (n/m**2) land sea surface'
        endif
!...................................................................
!  - boundary layer height
!
        call unsplit2z(wrkga,global_lats_r)

        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,ihpbl,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(ihpbl),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  65) ',
     &     'boundary layer height'
        endif
!...................................................................
!  - precipitable water
!
        call unsplit2z(wrkga,global_lats_r)

        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,ipwat,icolmn,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(ipwat),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  66) ',
     &     'precipitable water (kg/m**2) total atmospheric column'
        endif
!...................................................................
!  - convective clouds
!    * labeled instantaneous but actually averaged over fhswr seconds
!
!     glolal = cv*1.e2
!     where (glolal >= 0.5)
!       kmskcv = 1
!     elsewhere
!       kmskcv = 0
!     endwhere

        call unsplit2z(wrkga,global_lats_r)

        lbm = (wrkga >= 0.5_kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,icldf,icvlyr,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(icldf),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  67) ',
     &     'total cloud cover (percent) convective cloud layer'
        endif
!.................................................
!  - pressure at convective cloud top
!
!     do j = 1, lats_node_r
!       do i = 1, lonr
!         glolal(i,j) = 0.
!         if (cv(i,j) > 0.) then
!           glolal(i,j) = cvt(i,j)*1.e3
!         endif
!       enddo
!     enddo

        call unsplit2z(wrkga,global_lats_r)

        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              1,iprs,icvtl,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(iprs),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  68) ',
     &     'pressure (pa) convective cloud top level'
        endif
!.................................................
!  - pressure at convective cloud bottom
!
!     do j = 1, lats_node_r
!       do i = 1, lonr
!         glolal(i,j) = 0.
!         if (cv(i,j) > 0.) then
!           glolal(i,j) = cvb(i,j)*1.e3
!         endif
!       enddo
!     enddo

        call unsplit2z(wrkga,global_lats_r)

        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              1,iprs,icvbl,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(iprs),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  69) ',
     &     'pressure (pa) convective cloud bottom level'
        endif
!.................................................
!  - sea ice thickness
!
        call unsplit2z(wrkga,global_lats_r)

        lbm = (slmskful /= 1._kind_io8)
!       lbm = (slmskful == 2._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              1,isik,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(isik),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  70) ',
     &     'sea ice thickness (m) category 1'
        endif
!.................................................
!  - volumetric soil moist content (layer 100cm and 40cm)
!
        if (lsoil > 2) then
!         glolal(:,:) = smc(:,3,:)

          call unsplit2z(wrkga,global_lats_r)

          lbm = (slmskful == 1._kind_io8)
          call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &                1,isoilm,i2dbls,40,100,iyr,imo,ida,ihr,
     &                ifhour,ithr,0,inst,0,0,icen2,ids(isoilm),iens,
     &                0.,0.,0.,0.,0.,0.,g,lg,ierr)

          if (ierr == 0) then
            call wryte(noflx,lg,g)
          else
            print*,'wrtsfc gribit ierr=',ierr,'  71) ',
     &       '71) volumetric soil moist content (frac) 100cm and 40cm'
          endif
!..........................................................
!  - volumetric soil moist content (layer 200cm and 100cm)
!
!       glolal(:,:) = smc(:,4,:)

          call unsplit2z(wrkga,global_lats_r)

          lbm = (slmskful == 1._kind_io8)
          call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &                1,isoilm,i2dbls,100,200,iyr,imo,ida,ihr,
     &                ifhour,ithr,0,inst,0,0,icen2,ids(isoilm),iens,
     &                0.,0.,0.,0.,0.,0.,g,lg,ierr)

          if (ierr == 0) then
            call wryte(noflx,lg,g)
          else
            print*,'wrtsfc gribit ierr=',ierr,'  72) ',
     &       'volumetric soil moist content (frac) 200cm and 100cm'
          endif
!..........................................................
!  - temperature for layer 100cm and 40cm below sfc
!
!       glolal(:,:) = stc(:,3,:)

          call unsplit2z(wrkga,global_lats_r)

          lbm = (slmskful == 1._kind_io8)
          call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &                1,itemp,i2dbls,40,100,iyr,imo,ida,ihr,
     &                ifhour,ithr,0,inst,0,0,icen2,ids(itemp),iens,
     &                0.,0.,0.,0.,0.,0.,g,lg,ierr)

          if (ierr == 0) then
            call wryte(noflx,lg,g)
          else
            print*,'wrtsfc gribit ierr=',ierr,'  73) ',
     &       'temp (k) layer betw two depth below sfc 100cm and 40cm'
          endif
!..........................................................
!  - temperature for layer 200cm and 100cm below sfc
!
!       glolal(:,:) = stc(:,4,:)

          call unsplit2z(wrkga,global_lats_r)

          lbm = (slmskful == 1._kind_io8)
          call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &                1,itemp,i2dbls,100,200,iyr,imo,ida,ihr,
     &                ifhour,ithr,0,inst,0,0,icen2,ids(itemp),iens,
     &                0.,0.,0.,0.,0.,0.,g,lg,ierr)

          if (ierr == 0) then
            call wryte(noflx,lg,g)
          else
            print*,'wrtsfc gribit ierr=',ierr,'  74) ',
     &       'temp (k) layer betw two depth below sfc 200cm and 100cm'
          endif
      endif   ! end_if_lsoil
!..........................................................
!  - liquid soil moist content layer 40cm and 10cm
!
!     glolal(:,:) = slc(:,1,:)

        call unsplit2z(wrkga,global_lats_r)

        lbm = (slmskful == 1._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,130,icen,igen,
     &              1,islc,i2dbls,0,10,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(isoilm),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  75) ',
     &     'liquid soil moist content (frac) layer 10cm and 0cm'
        endif
!..........................................................
!  - liquid soil moist content layer 40cm and 10cm
!
!     glolal(:,:) = slc(:,2,:)

        call unsplit2z(wrkga,global_lats_r)

        lbm = (slmskful == 1._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,130,icen,igen,
     &              1,islc,i2dbls,10,40,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(isoilm),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  76) ',
     &     'liquid soil moist content (frac) layer 40cm and 10cm'
        endif
!..........................................................
!  - liquid soil moist content layer 100cm and 40cm
!
      if (lsoil > 2) then
!       glolal(:,:) = slc(:,3,:)

          call unsplit2z(wrkga,global_lats_r)

          lbm = (slmskful == 1._kind_io8)
          call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,130,icen,igen,
     &                1,islc,i2dbls,40,100,iyr,imo,ida,ihr,
     &                ifhour,ithr,0,inst,0,0,icen2,ids(isoilm),iens,
     &                0.,0.,0.,0.,0.,0.,g,lg,ierr)

          if (ierr == 0) then
            call wryte(noflx,lg,g)
          else
            print*,'wrtsfc gribit ierr=',ierr,'  77) ',
     &       'liquid soil moist content (frac) layer 100cm and 40cm'
          endif
!..........................................................
!  - liquid soil moist content layer 200cm and 100cm
!
!       glolal(:,:) = slc(:,4,:)

          call unsplit2z(wrkga,global_lats_r)

          lbm = (slmskful == 1._kind_io8)
          call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,130,icen,igen,
     &                1,islc,i2dbls,100,200,iyr,imo,ida,ihr,
     &                ifhour,ithr,0,inst,0,0,icen2,ids(isoilm),iens,
     &                0.,0.,0.,0.,0.,0.,g,lg,ierr)

          if (ierr == 0) then
            call wryte(noflx,lg,g)
          else
            print*,'wrtsfc gribit ierr=',ierr,'  78) ',
     &       'liquid soil moist content (frac) layer 200cm and 100cm'
          endif
      endif
!..........................................................
!  - snow depth
!
!     glolal = snwdph / 1.e3       !! convert from mm to m

        call unsplit2z(wrkga,global_lats_r)

!       lbm = (slmskful == 1._kind_io8)
        lbm = (slmskful == 1._kind_io8 .or. slmskful == 2._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              1,isnod,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(isnod),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  79) ',
     &     'snow depth (m) land surface'
        endif
!..........................................................
!  - canopy water content
!
        call unsplit2z(wrkga,global_lats_r)

        lbm = (slmskful == 1._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              1,icnp,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(icnp),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  80) ',
     &     'canopy water content (kg/m^2) land surface'
        endif
!..........................................................
!  - the following 30 records are for land mdl use
!  - surface roughness
!
!     glolal = zorl / 1.e2       !! convert from cm to m

        call unsplit2z(wrkga,global_lats_r)

        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,izorl,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(izorl),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  81) ',
     &     'surface roughness (m)'
        endif
!..........................................................
!  - vegetation fraction
!
!     glolal = vfrac*100.

        call unsplit2z(wrkga,global_lats_r)

        lbm = (slmskful == 1._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              1,iveg,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(iveg),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  82) ',
     &     'vegetation fraction (fractional) land surface'
        endif
!..........................................................
!  - vegetation type
!
        call unsplit2z(wrkga,global_lats_r)

        lbm = (slmskful == 1._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              1,ivtp,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(ivtp),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  83) ',
     &     'vegetation type land surface'
        endif
!..........................................................
!  - soil type
!
        call unsplit2z(wrkga,global_lats_r)

        lbm = (slmskful == 1._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              1,istp,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(istp),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  84) ',
     &     'soil type land surface'
        endif
!..........................................................
!  - slope type
!
        call unsplit2z(wrkga,global_lats_r)

        lbm = (slmskful == 1._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,130,icen,igen,
     &              1,islo,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(islo),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  85) ',
     &     'slope type land surface'
        endif
!..........................................................
!  - frictional velocity
!
      call unsplit2z(wrkga,global_lats_r)

        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,iust,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(iust),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  86) ',
     &     'frictional velocity (m/s)'
        endif
!..........................................................
!  - surface height
!
        call unsplit2z(wrkga,global_lats_r)

        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,ihgt,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(ihgt),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  87) ',
     &     'surface height (m)'
        endif
!..........................................................
!  - freezing precip flag
!
        call unsplit2z(wrkga,global_lats_r)

        lbm = (slmskful == 1._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              1,irst,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(irst),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  88) ',
     &     'freezing precip flag land surface'
        endif
!..........................................................
!  - exchange coefficient ch
!
        call unsplit2z(wrkga,global_lats_r)

        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,ichh,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(ichh),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  89) ',
     &     'exchange coefficient ch(m/s)'
        endif
!..........................................................
!  - exchange coefficient cm
!
        call unsplit2z(wrkga,global_lats_r)

        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,130,icen,igen,
     &              0,icmm,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(icmm),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  90) ',
     &     'exchange coefficient cm(m/s)'
        endif
!..........................................................
!  - potential evaporation rate
!
        call unsplit2z(wrkga,global_lats_r)

        lbm = (slmskful == 1._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              1,iep,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(iep),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  91) ',
     &     'potential evaporation rate (w/m**2) land and sea surface'
        endif
!..........................................................
!  - downward long wave radiation flux (instantaneous value)
!
        if (.not. climate) then         ! do not output those fields in climate mode

          call unsplit2z(wrkga,global_lats_r)

          call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &                0,idlwf,isfc,0,0,iyr,imo,ida,ihr,
     &                ifhour,ithr,0,inst,0,0,icen2,ids(idlwf),iens,
     &                0.,0.,0.,0.,0.,0.,g,lg,ierr)

          if (ierr == 0) then
            call wryte(noflx,lg,g)
          else
            print*,'wrtsfc gribit ierr=',ierr,'  92) ',
     &       'downward long wave radiation flux (w/m**2)'
          endif
!..........................................................
!  - upward long wave radiation flux (instantaneous value)
!
          call unsplit2z(wrkga,global_lats_r)

          call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &                0,iulwf,isfc,0,0,iyr,imo,ida,ihr,
     &                ifhour,ithr,0,inst,0,0,icen2,ids(iulwf),iens,
     &                0.,0.,0.,0.,0.,0.,g,lg,ierr)

          if (ierr == 0) then
            call wryte(noflx,lg,g)
          else
            print*,'wrtsfc gribit ierr=',ierr,'  93) ',
     &       'upward long wave radiation flux (w/m**2)'
          endif
!..........................................................
!  - upward short wave radiation flux (instantaneous value)
!
          call unsplit2z(wrkga,global_lats_r)

          call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &                0,iuswf,isfc,0,0,iyr,imo,ida,ihr,
     &                ifhour,ithr,0,inst,0,0,icen2,ids(iuswf),iens,
     &                0.,0.,0.,0.,0.,0.,g,lg,ierr)

          if (ierr == 0) then
            call wryte(noflx,lg,g)
          else
            print*,'wrtsfc gribit ierr=',ierr,'  94) ',
     &       'upward short wave radiation flux (w/m**2)'
          endif
!..........................................................
!  - downward short wave radiation flux (instantaneous value)
!
          call unsplit2z(wrkga,global_lats_r)

          call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &                0,idswf,isfc,0,0,iyr,imo,ida,ihr,
     &                ifhour,ithr,0,inst,0,0,icen2,ids(idswf),iens,
     &                0.,0.,0.,0.,0.,0.,g,lg,ierr)

          if (ierr == 0) then
            call wryte(noflx,lg,g)
          else
            print*,'wrtsfc gribit ierr=',ierr,'  95) ',
     &       'downward short wave radiation flux (w/m**2)'
          endif
!..........................................................
!  - sensible heat flux (instantaneous value)
!
          call unsplit2z(wrkga,global_lats_r)

          call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &                0,ishflx,isfc,0,0,iyr,imo,ida,ihr,
     &                ifhour,ithr,0,inst,0,0,icen2,ids(ishflx),iens,
     &                0.,0.,0.,0.,0.,0.,g,lg,ierr)

          if (ierr == 0) then
            call wryte(noflx,lg,g)
          else
            print*,'wrtsfc gribit ierr=',ierr,'  96) ',
     &       'sensible heat flux (w/m**2) land and sea surface'
          endif
!..........................................................
!  - latent heat flux (instantaneous value)
!
          call unsplit2z(wrkga,global_lats_r)

          call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &                0,ilhflx,isfc,0,0,iyr,imo,ida,ihr,
     &                ifhour,ithr,0,inst,0,0,icen2,ids(ilhflx),iens,
     &                0.,0.,0.,0.,0.,0.,g,lg,ierr)

          if (ierr == 0) then
            call wryte(noflx,lg,g)
          else
            print*,'wrtsfc gribit ierr=',ierr,'  97) ',
     &       'latent heat flux (w/m**2) land and sea surface'
          endif
!..........................................................
!  - ground heat flux (instantaneous value)
!
          call unsplit2z(wrkga,global_lats_r)

          lbm = (slmskful /= 0._kind_io8)
          call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &                1,ighflx,isfc,0,0,iyr,imo,ida,ihr,
     &                ifhour,ithr,0,inst,0,0,icen2,ids(ighflx),iens,
     &                0.,0.,0.,0.,0.,0.,g,lg,ierr)

          if (ierr == 0) then
            call wryte(noflx,lg,g)
          else
            print*,'wrtsfc gribit ierr=',ierr,'  98) ',
     &       'ground heat flux (w/m**2) land and sea surface'
          endif
        endif             ! end of if(.not climate)
!..........................................................
!  - surface runoff
!
!     glolal = srunoff * 1.e3

        call unsplit2z(wrkga,global_lats_r)

        lbm = (slmskful == 1._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              1,isrf,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iacc,0,0,icen2,ids(isrf),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  99) ',
     &     'surface runoff (kg/m^2) land surface'
        endif
!..........................................................
!  - lowest model level temp
!
        call unsplit2z(wrkga,global_lats_r)

        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,itemp,isglev,1,1,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(itemp),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  100) ',
     &     ' lowest model level temp (k)'
        endif
!..........................................................
!  - lowest model specific humidity
!
        call unsplit2z(wrkga,global_lats_r)

        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,isphum,isglev,1,1,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(isphum),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  101) ',
     &     'lowest model specific humidity (kg/kg)'
        endif
!..........................................................
!  - lowest model u wind
!
        call unsplit2z(wrkga,global_lats_r)

        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,iznlw,isglev,1,1,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(iznlw),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  102) ',
     &     'lowest model u wind (m/s)'
        endif
!..........................................................
!  - lowest model v wind
!
        call unsplit2z(wrkga,global_lats_r)

        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,imerw,isglev,1,1,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(imerw),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  103) ',
     &     'lowest model v wind (m/s)'
        endif
!..........................................................
!  - lowest model level height
!
        call unsplit2z(wrkga,global_lats_r)

        lbm = (slmskful == 1._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              1,ihgt,isglev,1,1,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(ihgt),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  104) ',
     &     'lowest model level height (m) land surface'
        endif
!..........................................................
!  - direct evaporation from bare soil
!
!     glolal = evbsa*rtime

        call unsplit2z(wrkga,global_lats_r)

        lbm = (slmskful == 1._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              1,ievbs,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(ievbs),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  105) ',
     &     'direct evaporation from bare soil(w/m^2) land surface'
        endif
!..........................................................
!  - canopy water evaporation
!
!     glolal = evcwa*rtime

        call unsplit2z(wrkga,global_lats_r)

        lbm = (slmskful == 1._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              1,ievcw,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(ievbs),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  106) ',
     &     'canopy water evaporation(w/m^2) land surface'
        endif
!..........................................................
!  - transpiration
!
!     glolal = transa*rtime

        call unsplit2z(wrkga,global_lats_r)

        lbm = (slmskful == 1._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              1,itran,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(itran),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  107) ',
     &     'transpiration (w/m^2) land surface'
        endif
!..........................................................
!  - snow sublimation
!
!     glolal = sbsnoa*rtime

      call unsplit2z(wrkga,global_lats_r)

        lbm = (slmskful == 1._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,130,icen,igen,
     &              1,isbs,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(isbs),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  108) ',
     &     'snow sublimation (w/m^2) land surface'
        endif
!..........................................................
!  - snow cover
!
!     glolal = snowca*rtime*100.

        call unsplit2z(wrkga,global_lats_r)

        lbm = (slmskful == 1._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              1,isnc,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(isnc),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  109) ',
     &     'snow cover (fraction) land surface'
        endif
!..........................................................
!  - total column soil moisture
!
!     glolal = soilm*1.e3       !! convert from m to (mm)kg/m^2

        call unsplit2z(wrkga,global_lats_r)

        lbm = (slmskful == 1._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              1,istc,i2dbls,0,200,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(istc),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  110) ',
     &     'total column soil moisture (kg/m^2) land surface'
        endif
!..........................................................
!  - snow phase-change heat flux
!
!     glolal = snohfa*rtime

        call unsplit2z(wrkga,global_lats_r)

        lbm = (slmskful == 1._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,130,icen,igen,
     &              1,isnohf,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(isnohf),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  111) ',
     &     'snow phase-change heat flux [w/m^2] land surface'
        endif
!..........................................................
!  - wilting point
!
!     glolal = smcwlt2

        call unsplit2z(wrkga,global_lats_r)

        lbm = (slmskful == 1._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,130,icen,igen,
     &              1,ismcwlt,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(ismcwlt),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  112) ',
     &     'wilting point [fraction] land surface'
        endif
!..........................................................
!  - field capacity
!
!     glolal = smcref2

        call unsplit2z(wrkga,global_lats_r)

        lbm = (slmskful == 1._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,130,icen,igen,
     &              1,ismcref,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(ismcref),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  113) ',
     &     'field capacity [fraction] land surface'
        endif
!..........................................................
!  - accumulated sunshine duration time
!
!     glolal = suntim

        call unsplit2z(wrkga,global_lats_r)

        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,133,icen,igen,
     &              0,isuntm,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iacc,0,0,icen2,ids(isuntm),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  114) ',
     &     'accumulated sunshine duration time (sec)'
        endif
!...................................................................
!
!..........................................................
!  - frozen precipitation fraction
!
!     glolal = geshem*1.e3*rtime*srflag

        call unsplit2z(wrkga,global_lats_r)

        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,   
     &              0,isr,isfc,0,0,iyr,imo,ida,ihr,                  
     &              ifhour,ithr,0,inst,0,0,icen2,1,iens,   
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  123) ', 
     &     'frozen precipitation (fraction)'
        endif
!..........................................................
!  - aerosols optical depth at 550nm (optional)

!     do j = 1, lats_node_r
!       do i = 1, lonr
!         glolal(i,j) = rflux(i,j,34)*rtimsw
!       enddo
!     enddo

!       call unsplit2z(wrkga,global_lats_r)

!       call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,130,icen,igen,   &
!    &              0,iaod,icolmn,0,0,iyr,imo,ida,ihr,                  &
!    &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(iaod),iens,     &
!    &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

!       if (ierr == 0) then
!         call wryte(noflx,lg,g)
!       else
!         print*,'wrtsfc gribit ierr=',ierr,'  124) ',
!    &     'total aerosol optical depth at 550 nm wavelength'
!       endif

      endif           ! if (me == ioproc)
!..........................................................

      if (me == ioproc) then
         print *,'grib flux file written ',fhour,idate,noflx
      endif
!
      return
!...................................
      end subroutine wrtflx_w
!-----------------------------------


!-----------------------------------
       subroutine grids_move(ioproc)
!...................................
!
      use resol_def
      use mod_state
      use layout1
      use mpi_def
!
      implicit none
!
!  ---  inputs/outputs:
      integer :: ioproc

!  ---  locals:
      integer :: proc, msgtag, ierr, illen, kllen

      integer, save :: icount, maxlats_comp
      data icount / 0 /
!
!===>  begin here
!
      illen = ngrids_flx + 1 + ngrids_sfcc + ngrids_nst

      if (icount == 0) then
        allocate(ivar_global(10))
        allocate(ivar_global_a(10,nodes))

        ivar_global(1) = ipt_lats_node_r
        ivar_global(2) = lats_node_r
        ivar_global(3) = lats_node_r_max

        call mpi_gather(ivar_global,10,mpi_integer,ivar_global_a,
     &                  10,mpi_integer,ioproc,mpi_comm_all,ierr)
        icount = icount + 1
      endif
!
      if (.not. allocated(buff_mult_pieces)) then
        maxlats_comp = lats_node_r_max

        if (liope .and. me==ioproc) then
!         maxlats_comp = ivar_global_a(3,ioproc)
          maxlats_comp = ivar_global_a(3,1)
        endif

        allocate(buff_mult_pieces(lonr,ngrids_sfcc,maxlats_comp,nodes))
        allocate(buff_mult_piecesf(lonr,0:ngrids_flx,maxlats_comp,
     &                             nodes))
        allocate(buff_mult_piecesa(lonr,1:illen,maxlats_comp,nodes))
      endif

!  ---  big send

      if (me /= ioproc) then

!  ---  sending the data for the local grid domain
        msgtag = me
        kllen = illen * lats_node_r * lonr

        call mpi_send(buff_mult_piecea,kllen,mpi_r_io,ioproc,
     &                msgtag,mpi_comm_all,info)
      else
        if (mc_comp /= mpi_comm_null) then

!  ---  iotask is also a compute task.  send is replaced with direct array copy

          buff_mult_piecesa(:,:,1:lats_node_r,ioproc+1) =
     &    buff_mult_piecea(:,:,1:lats_node_r)
        endif

!  ---  receiving part of i/o task
        do proc = 1, nodes_comp
          if (proc /= ioproc+1) then
            msgtag = proc - 1
            kllen = illen * lonr * ivar_global_a(2,proc)

            call mpi_recv(buff_mult_piecesa(1,1,1,proc),kllen,
     &                    mpi_r_io,proc-1,msgtag,mpi_comm_all,stat,info)
          endif
        enddo
      endif
!
      return
!...................................
      end subroutine grids_move
!-----------------------------------


!-----------------------------------
      subroutine spect_collect(
     &            zqe_ls,qe_ls,tee_ls,die_ls,zee_ls,rqe_ls,gze_ls,
     &            zqo_ls,qo_ls,teo_ls,dio_ls,zeo_ls,rqo_ls,gzo_ls,
     &            trieo_ls_node  )
!...................................
!
      use resol_def
      use layout1
      use mpi_def
!
      implicit none
!
!  ---  inputs/outputs:
      real (kind=kind_evod), dimension(len_trie_ls,2)      :: zqe_ls,
     &                                                qe_ls , gze_ls
      real (kind=kind_evod), dimension(len_trie_ls,2,levs) :: tee_ls,
     &                                                die_ls, zee_ls
      real (kind=kind_evod), dimension(len_trie_ls,2,levh) :: rqe_ls

      real (kind=kind_evod), dimension(len_trio_ls,2)      :: zqo_ls,
     &                                                qo_ls , gzo_ls
      real (kind=kind_evod), dimension(len_trio_ls,2,levs) :: teo_ls,
     &                                                dio_ls, zeo_ls
      real (kind=kind_evod), dimension(len_trio_ls,2,levh) :: rqo_ls

!  ---  locals:
      integer :: j, k, kw_d, kw_t, jj

!     real(kind=8) :: t4, rtc

      real (kind=kind_mpi) :: trieo_ls_node(
     &       len_trie_ls_max+len_trio_ls_max, 2, 3*levs+1*levh+1)
!
!===> ...  begin here
!
      if (comp_task) then

!  ---  build state in trieo_ls_node
        do j = 1, len_trie_ls
          trieo_ls_node(j,1,kwq) = qe_ls(j,1)
          trieo_ls_node(j,2,kwq) = qe_ls(j,2)
        enddo

        do j = 1, len_trio_ls
          trieo_ls_node(j+len_trie_ls_max,1,kwq) = qo_ls(j,1)
          trieo_ls_node(j+len_trie_ls_max,2,kwq) = qo_ls(j,2)
        enddo

!$omp parallel do private(k,j,kw_t,kw_d,jj)
        do k = 1, levs
          kw_t = kwte+  k-1
          kw_d = kwdz+2*k-2
          
          do j = 1, len_trie_ls
            trieo_ls_node(j,1,kw_t)   = tee_ls(j,1,k)
            trieo_ls_node(j,2,kw_t)   = tee_ls(j,2,k)

            trieo_ls_node(j,1,kw_d)   = die_ls(j,1,k)
            trieo_ls_node(j,2,kw_d)   = die_ls(j,2,k)

            trieo_ls_node(j,1,kw_d+1) = zee_ls(j,1,k)
            trieo_ls_node(j,2,kw_d+1) = zee_ls(j,2,k)
          enddo

          do j = 1, len_trio_ls
            jj = len_trie_ls_max + j
            trieo_ls_node(jj,1,kw_t)   = teo_ls(j,1,k)
            trieo_ls_node(jj,2,kw_t)   = teo_ls(j,2,k)

            trieo_ls_node(jj,1,kw_d)   = dio_ls(j,1,k)
            trieo_ls_node(jj,2,kw_d)   = dio_ls(j,2,k)

            trieo_ls_node(jj,1,kw_d+1) = zeo_ls(j,1,k)
            trieo_ls_node(jj,2,kw_d+1) = zeo_ls(j,2,k)
          enddo
        enddo

!$omp parallel do private(k,j)
        do k = 1, levh
          do j = 1, len_trie_ls
            trieo_ls_node(j,1,kwrq+k-1) = rqe_ls(j,1,k)
            trieo_ls_node(j,2,kwrq+k-1) = rqe_ls(j,2,k)
          enddo
          do j = 1, len_trio_ls
            trieo_ls_node(j+len_trie_ls_max,1,kwrq+k-1) = rqo_ls(j,1,k)
            trieo_ls_node(j+len_trie_ls_max,2,kwrq+k-1) = rqo_ls(j,2,k)
          enddo
        enddo

      endif  ! end if comp_task block

!     t4 = rtc()
!
      return
!...................................
      end subroutine spect_collect
!-----------------------------------

!-----------------------------------
      integer function nfill(c)
!...................................
!
      implicit none
!
!  ---  inputs:
      character*(*) :: c

!  ---  locals:
      integer :: j
!
!===> ...  begin here
!
      nfill = len(c)
      do j = 1, nfill
        if (c(j:j) == ' ') then
          nfill = j - 1
          return
        endif
      enddo
!
      return
!...................................
      end function nfill
!-----------------------------------
 

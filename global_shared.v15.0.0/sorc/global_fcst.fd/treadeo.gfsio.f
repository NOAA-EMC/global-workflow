      subroutine treadeo_gfsio(fhour,idate,
     x                   gze,qe,tee,die,zee,rqe,
     x                   gzo,qo,teo,dio,zeo,rqo,
     x                   ls_node,ls_nodes,max_ls_nodes,
     x                   snnp1ev,snnp1od,pdryini,iprint,
     &                   global_lats_r,lats_nodes_r,lonsperlar,cfile,
     &                   epse,epso,plnew_r,plnow_r)
 
      use resol_def
      use layout1
      use coordinate_def					! hmhj
      use sig_io
      use namelist_def
      use vert_def
      use mpi_def
      use physcons, rerth => con_rerth, grav => con_g, rkap => con_rocp
     &,             cpd => con_cp
      use gfsio_module
      use gfsio_def
!
      implicit none
      character*(*) cfile
      real(kind=kind_evod) fhour
      integer              idate(4),ntraci, ntozi, ntcwi, ncldi, 
     &                     ixgr, ixga, levsi, jcapi,
     &                     latgi, lonfi, latri, lonri, nt0
!!
      real(kind=kind_evod)  epse(len_trie_ls)
      real(kind=kind_evod)  epso(len_trio_ls)
!!
      real(kind=kind_evod)   plnew_r(len_trie_ls,latr2)
      real(kind=kind_evod)   plnow_r(len_trio_ls,latr2)
!!
      real(kind=kind_evod) gze(len_trie_ls,2)
     &,                     qe(len_trie_ls,2)
     &,                    tee(len_trie_ls,2,levs)
     &,                    die(len_trie_ls,2,levs)
     &,                    zee(len_trie_ls,2,levs)
     &,                    rqe(len_trie_ls,2,levs,ntrac)
     &,                    gzo(len_trio_ls,2)
     &,                     qo(len_trio_ls,2)
     &,                    teo(len_trio_ls,2,levs)
     &,                     dio(len_trio_ls,2,levs)
     &,                    zeo(len_trio_ls,2,levs)
     &,                    rqo(len_trio_ls,2,levs,ntrac)
 
!
      integer              ls_node(ls_dim,3)
!
!cmr  ls_node(1,1) ... ls_node(ls_max_node,1) : values of l
!cmr  ls_node(1,2) ... ls_node(ls_max_node,2) : values of jbasev
!cmr  ls_node(1,3) ... ls_node(ls_max_node,3) : values of jbasod
!
      integer              ls_nodes(ls_dim,nodes)
      integer              max_ls_nodes(nodes)
      integer              lats_nodes_r(nodes)
      real(kind=kind_evod) snnp1ev(len_trie_ls)
      real(kind=kind_evod) snnp1od(len_trio_ls)
      integer              iprint
      integer              k
!     real(kind=kind_evod) ga2,gencode,gzbar,order,realform
      real(kind=kind_evod) waves,xlayers
      real(kind=kind_evod) sikp1(levp1)
      real(kind=kind_io4)   fhour4,pdryini4
      real(kind=kind_io8)   pdryini
      real(kind=kind_io4), allocatable ::  vcoord4(:,:)
!     integer             idusr
!
!     type (gfsio_gfile) gfile
!
!     integer              idvc
!     integer              idsl, iret, num_dta
      integer              iret
      real(kind=kind_evod) psurfff
      real(kind=kind_evod) pressk, tem
      real(kind=kind_evod), parameter :: rkapi=1.0/rkap,
     &                                   rkapp1=1.0+rkap
!
      integer kmsk(lonr,latr), global_lats_r(latr), lonsperlar(latr)
      real(kind=kind_io8) buffo(lonr,lats_node_r)
      real(kind=kind_evod) teref(levp1),ck5p(levp1)			! hmhj
!    &,                    ttref(levp1)
!
      real (kind=kind_io4), allocatable ::  gfsio_data(:)
!!
      real(kind=kind_rad) zsg(lonr,lats_node_r)
      real(kind=kind_rad) psg(lonr,lats_node_r)
      real(kind=kind_rad) uug(lonr,lats_node_r,levs)
      real(kind=kind_rad) vvg(lonr,lats_node_r,levs)
      real(kind=kind_rad) teg(lonr,lats_node_r,levs)
      real(kind=kind_rad) rqg(lonr,lats_node_r,levh)
!
!       input file is in grid-point space - use gfs_io package
!
      call gfsio_open(gfile_in,trim(cfile),'read',iret)
      call gfsio_getfilehead(gfile_in,iret=iret,
     &  version=ivsupa,fhour=fhour4,idate=idate,
     &  latb=latb,lonb=lonb,levs=levsi,jcap=jcapi,itrun=itrun,
     &  iorder=iorder,irealf=irealf,igen=igen,latf=latgi,lonf=lonfi,
     &  latr=latri,lonr=lonri,ntrac=ntraci,icen2=icen2,iens=iens,
     &  idpp=idpp,idsl=idsl,idvc=idvc,idvm=idvm,idvt=idvt,idrun=idrun,
     &  idusr=idusr,pdryini=pdryini4,ncldt=ncldt,nvcoord=nvcoord)
!
      if (me == 0) then
        print *,' idvt=',idvt,' nvcoord=',nvcoord,' levsi=',levsi
        if(lonf .ne. lonfi .or. latg .ne. latgi .or.
     &     jcap .ne. jcapi .or. levs .ne. levsi) then
          print *,' input resolution and the model resolutions are'
     &,  ' different- run aborted'
          call mpi_quit(777)
        endif
      endif
!
      allocate (vcoord4(levsi+1,nvcoord))
      allocate (vcoord(levsi+1,nvcoord))
      call gfsio_getfilehead(gfile_in,iret=iret,vcoord=vcoord4)
!
!     if (me == 0) then
!     print *,' nvcoord=',nvcoord,' vcoord4=',vcoord4(:,1:nvcoord)
!    &,' iret=',iret
!     endif
!
!     usrid = idusr
!     runid - idrun
      vcoord(:,1:nvcoord) = vcoord4(:,1:nvcoord)
!     if (me .eq. 0) print *,' vcoord=',vcoord(:,1:nvcoord)
      deallocate (vcoord4)
!
      if (gen_coord_hybrid) then                                        ! hmhj

        sfcpress_id  = mod(idvm , 10)
        thermodyn_id = mod(idvm/10 , 10)
!   ak bk ck in file have the same order as model                       ! hmhj
        do k=1,levp1                                                    ! hmhj
          ak5(k) = vcoord(k,1)/1000.                                    ! hmhj
          bk5(k) = vcoord(k,2)                                          ! hmhj
          ck5(k) = vcoord(k,3)/1000.                                    ! hmhj
        enddo                                                           ! hmhj
        vertcoord_id=0                                                  ! hmhj
        do k=1,levp1                                                    ! hmhj
          if( ck5(k).ne.0.0 ) vertcoord_id=3                            ! hmhj
        enddo
! provide better estimated press                                        ! hmhj
        psurfff = 101.3                                                 ! hmhj
        if( thermodyn_id.eq.3 ) then                                    ! hmhj
          do k=1,levs                                                   ! hmhj
            thref(k) = 300.0*cpd                                        ! hmhj
            teref(k) = 255.0*cpd                                        ! hmhj
          enddo                                                         ! hmhj
        else                                                            ! hmhj
         do k=1,levp1                                                   ! hmhj
          thref(k) = 300.0                                              ! hmhj
          teref(k) = 255.0                                              ! hmhj
         enddo                                                          ! hmhj
        endif
        ck5p(levp1) = ck5(levp1)                                        ! hmhj
        do k=1,levs                                                     ! hmhj
          ck5p(k) = ck5(k)*(teref(k)/thref(k))**rkapi                   ! hmhj
        enddo
        if( me.eq.0 ) then                                              ! hmhj
          do k=1,levp1                                                  ! hmhj
            pressk=ak5(k)+bk5(k)*psurfff+ck5p(k)                        ! hmhj
            print 180,k,ak5(k),bk5(k),ck5(k),pressk                     ! hmhj
180         format('k=',i3,'  ak5=',f13.9,'  bk5=',e15.8,               ! hmhj
     &            '   ck5=',f13.9,'  closed pressk=',f10.6)             ! hmhj
          enddo                                                         ! hmhj
        endif                                                           ! hmhj
        do k=1,levp1                                                    ! hmhj
          si(k) = ak5(k)/psurfff + bk5(k) + ck5p(k)/psurfff             ! hmhj
        enddo                                                           ! hmhj
        do k=1,levs                                                     ! hmhj
          sl(k) = 0.5*(si(k)+si(k+1))                                   ! hmhj
        enddo                                                           ! hmhj

      else if (hybrid .and. idvc .eq. 2) then
!       idsl=slid  !=2,pk=0.5*(p(k+1/2)+p(k-1/2)) check alfa(1)  am_bm
!   ak bk order in "sigma" file is bottom to top !!!!!!!!!!!!!!!!!!
        psurfff = 101.3
        do k=1,levp1
          ak5(k) = vcoord(levp1+1-k,1)/1000.
          bk5(k) = vcoord(levp1+1-k,2)
          pressk = ak5(k) + bk5(k)*psurfff

          if(me.eq.0)print 190,k,ak5(k),bk5(k),pressk
190       format('k=',i3,'  ak5=',e14.6,'  bk5=',e15.8,
     &           '  pressk=',e14.6)

        enddo
        do k=1,levs
          dbk(k) = bk5(k+1)-bk5(k)
          bkl(k) = (bk5(k+1)+bk5(k))*0.5
          ck(k)  = ak5(k+1)*bk5(k)-ak5(k)*bk5(k+1)
          if(me.eq.0)print 200,k,dbk(k),ck(k)
200       format('k=',i3,'  dbk=',f9.6,'  ck=',e13.5)
        enddo
!
! hmhj give an estimated si and sl for dynamics
        do k=1,levs+1
          si(levs+2-k) = ak5(k)/psurfff + bk5(k) !ak(k) bk(k) go top to bottom
        enddo
        do k=1,levs
          sl(k) = 0.5*(si(k)+si(k+1))
        enddo
!
      elseif (idvc .le. 1) then
        si(:)    = vcoord(:,1)
        sik(:)   = si(:) ** rkap
        sikp1(:) = si(:) ** rkapp1
        do k=1,levs
          tem      = rkapp1 * (si(k) - si(k+1))
          slk(k)   = (sikp1(k)-sikp1(k+1))/tem
          sl(k)    = slk(k) ** rkapi
!         sl(k)    = ((sikp1(k)-sikp1(k+1))/tem)**rkapi
          if (me .eq. 0) print 250, k, si(k), sl(k)
250       format('k=',i2,'  si=',f9.6,'  sl=',e13.5)
        enddo
      else
        print *,' non compatible initial state idvc=',idvc
     &,' iret=',iret
        call mpi_quit(333)
      endif
!
      fhour       = fhour4
      idate       = idate
      waves       = jcap
      xlayers     = levs
      itrun       = itrun
!     order       = iorder
!     realform    = irealf
      icen        = 7
      icen2       = icen2
      igen        = igen
      ienst       = iens(1)
      iensi       = iens(2)
!     runid       = idrun
!     usrid       = idusr
      if (pdryini .eq. 0.0) pdryini = pdryini4
      ntraci = ntrac
      if (idvt .gt. 0.0) then
        if (nt0 > 0) then
          ntcwi = idvt / 10
          ntozi = idvt - ntcwi * 10 + 1
          ntcwi = ntcwi + 1
        else
          ntcwi = ntcw
          ntozi = ntoz
        endif
        ncldi = ncldt
      elseif(ntraci .eq. 2) then
        ntozi = 2
        ntcwi = 0
        ncldi = 0
      elseif(ntraci .eq. 3) then
        ntozi = 2
        ntcwi = 3
        ncldi = 1
      else
        ntozi = 0
        ntcwi = 0
        ncldi = 0
      endif
!     ixgr = ixgr
!
      if (ntrac <= 3 .and. idvt <= 0) then
        idvt = (ntcw-1)*10 + ntoz - 1
      endif
!
!
      if (me.eq.0) then
        write(*,*)'cfile,in treadeo fhour,idate=',cfile,fhour,idate
     &, ' ntozi=',ntozi,' ntcwi=',ntcwi,' ncldi=',ncldi
     &, ' ntraci=',ntraci,' tracers=',ntrac,' vtid=',idvt
     &,   ncldt,' idvc=',idvc,' jcap=',jcap
     &, ' ixgr=',ixgr,' ixga=',ixga,' pdryini=',pdryini
      endif
!
        allocate (gfsio_data(lonb*latb))
!  read orog
      call gfsio_readrecv(gfile_in,'hgt','sfc',1,gfsio_data,iret)
      call split2d(gfsio_data,buffo,global_lats_r)
      call interpred(1,kmsk,buffo,zsg,global_lats_r,lonsperlar)
!
!  read ps
      call gfsio_readrecv(gfile_in,'pres','sfc',1,gfsio_data,iret)
      call split2d(gfsio_data,buffo,global_lats_r)
      call interpred(1,kmsk,buffo,psg,global_lats_r,lonsperlar)
!
      do k=1,levs
!  read u
        call gfsio_readrecv(gfile_in,'ugrd','layer',k,gfsio_data,iret)
        call split2d(gfsio_data,buffo,global_lats_r)
        call interpred(1,kmsk,buffo,uug(1,1,k),global_lats_r,lonsperlar)
      enddo
!  read v
      do k=1,levs
        call gfsio_readrecv(gfile_in,'vgrd','layer',k,gfsio_data,iret)
        call split2d(gfsio_data,buffo,global_lats_r)
        call interpred(1,kmsk,buffo,vvg(1,1,k),global_lats_r,lonsperlar)
      enddo
!  read t   -- this is real temperature
      do k=1,levs
        call gfsio_readrecv(gfile_in,'tmp','layer',k,gfsio_data,iret)
        call split2d(gfsio_data,buffo,global_lats_r)
        call interpred(1,kmsk,buffo,teg(1,1,k),global_lats_r,lonsperlar)
      enddo
!
!  read tracers
!
      rqg(:,:,:) = 0.0
!  read q
      do k=1,levs
        call gfsio_readrecv(gfile_in,'spfh','layer',k,gfsio_data,iret)
        call split2d(gfsio_data,buffo,global_lats_r)
        call interpred(1,kmsk,buffo,rqg(1,1,k),global_lats_r,lonsperlar)
      enddo
!
!  other tracers
!
!  read o3
      if (ntozi .gt. 0) then
        do k=1,levs
          call gfsio_readrecv(gfile_in,'o3mr','layer',k,gfsio_data,
     &                                                  iret)
          call split2d(gfsio_data,buffo,global_lats_r)
          call interpred(1,kmsk,buffo,rqg(1,1,k+(ntoz-1)*levs),
     &                                global_lats_r,lonsperlar)
        enddo
      endif
!  read clw
      if (ntcwi .gt. 0) then
        do k=1,levs
          call gfsio_readrecv(gfile_in,'clwmr','layer',k,gfsio_data,
     &                                                         iret)
          call split2d(gfsio_data,buffo,global_lats_r)
          call interpred(1,kmsk,buffo,rqg(1,1,k+(ntcw-1)*levs),
     &                                global_lats_r,lonsperlar)
        enddo
      endif
!
!   convert from gaussian grid to spectral space
!       also convert from real t to either virtual t or enthalpy
!
      call grid_to_spect
     &     (zsg,psg,uug,vvg,teg,rqg,
     &      gze,gzo,qe,qo,die,dio,zee,zeo,tee,teo,rqe,rqo,
     &      ls_node,ls_nodes,max_ls_nodes,
     &      lats_nodes_r,global_lats_r,lonsperlar,
     &      epse,epso,snnp1ev,snnp1od,plnew_r,plnow_r)

!
      call gfsio_close(gfile_in,iret)
!
      iprint=0
 
!!!!
      return
      end

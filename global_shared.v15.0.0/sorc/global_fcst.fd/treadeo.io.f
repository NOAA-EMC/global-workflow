      subroutine treadeo(nft,fhour,idate,
     x                   gze,qe,tee,die,zee,rqe,
     x                   gzo,qo,teo,dio,zeo,rqo,
     x                   ls_node,ls_nodes,max_ls_nodes,
     &                   plnev_r,plnod_r,plnew_r,plnow_r,
     &                   lats_nodes_r,lats_nodes_a,
     x                   snnp1ev,snnp1od,pdryini,iprint,
     &                   phy_f3d, phy_f2d, global_lats_r, lonsperlar,
     &                   dyn_f3d, dyn_f2d, global_lats_a, lonsperlat,
     &                   cfile)
 
      use resol_def
      use layout1
      use coordinate_def					! hmhj
      use sig_io
      use namelist_def
      use vert_def
      use mpi_def
      use physcons, rerth => con_rerth, grav => con_g, rkap => con_rocp
     &            , cpd => con_cp					! hmhj
      use sigio_module
      use sigio_r_module
!
      implicit none
      character*(*) cfile
      integer              nft
      real(kind=kind_evod) fhour
      integer              idate(4),ntraci, ntozi, ntcwi, ncldi, 
     &                     ixgr, ixga, nt0, direction
!
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
!    &,                    sl(levs)
!    &,                    si(levp1)
     &,                    z00

      real (kind=kind_phys)
     &     phy_f3d(lonr,levs,num_p3d,lats_node_r),
     &     phy_f2d(lonr,num_p2d,lats_node_r)
      real (kind=kind_evod)
     &     dyn_f3d(lonf,levs,num_a3d,lats_node_a),
     &     dyn_f2d(lonf,num_a2d,lats_node_a)
 
      real(kind=kind_evod) plnev_r(len_trie_ls,latr2)
      real(kind=kind_evod) plnod_r(len_trie_ls,latr2)
      real(kind=kind_evod) plnew_r(len_trie_ls,latr2)
      real(kind=kind_evod) plnow_r(len_trie_ls,latr2)
      integer            lats_nodes_r(nodes)
      integer            lats_nodes_a(nodes)
!
      integer              ls_node(ls_dim,3)
!
!cmr  ls_node(1,1) ... ls_node(ls_max_node,1) : values of l
!cmr  ls_node(1,2) ... ls_node(ls_max_node,2) : values of jbasev
!cmr  ls_node(1,3) ... ls_node(ls_max_node,3) : values of jbasod
!
      integer              ls_nodes(ls_dim,nodes)
      integer              max_ls_nodes(nodes)
      real(kind=kind_evod) snnp1ev(len_trie_ls)
      real(kind=kind_evod) snnp1od(len_trio_ls)
      integer              iprint
      integer              j,k,l,locl,n,lv,kk
      integer              i,lan,lat,lons_lat,nn
      integer              indev
      integer              indod
      integer              indev1,indev2
      integer              indod1,indod2
      real(kind=kind_evod) ga2,order,realform
      real(kind=kind_evod) waves,xlayers
!     real(kind=kind_evod) xi(levp1),xl(levs)
      real(kind=kind_evod), target ::  trisca(lnt2)
      real(kind=kind_evod) sikp1(levp1)
!     real(kind=kind_io4)   buf(lnt2)
!     real(kind=kind_io4)   subcen,ppid,slid,vcid,vmid
      real(kind=kind_io8)   pdryini
      integer              indlsev,jbasev
      integer              indlsod,jbasod
c$$$      real(kind=kind_io4) z(lnt2)
!
      type(sigio_head) head
      type(sigio_dbti) dati
!
!     integer              idvc
      integer              iret, num_dta
      real(kind=kind_evod) psurfff
      real(kind=kind_evod) pressk, tem, rkapi, rkapp1
!
      integer kmsk(lonr,latr), global_lats_r(latr), lonsperlar(latr)
      real(kind=kind_ior), target ::  buff1(lonr*latr)
      real(kind=kind_io8) buffo(lonr,lats_node_r)
     &,                   buff2(lonr,lats_node_r)
      real(kind=kind_evod) teref(levp1),ck5p(levp1)			! hmhj
 
      integer kmska(lonf,latg),global_lats_a(latg), lonsperlat(latg)
      real(kind=kind_evod), target ::  buff1a(lonf*latg)
      real(kind=kind_io8) buffoa(lonf,lats_node_a)
     &,                   buff2a(lonf,lats_node_a)
      logical read_on_every_task
 
      include 'function_indlsev'
      include 'function_indlsod'
!!
c$$$      common /z00_com/z
!!
!     print *,' enter treadeo.io_fd '					! hmhj
      read_on_every_task = .false.

      call sigio_rropen(nft,cfile,iret)
      call sigio_alhead(head,iret)
      call sigio_rrhead(nft,head,iret)
!
      ivsinp = head%ivs
      if (me .eq. 0) then
        print *,' in treadeo iret=',iret,' cfile=',cfile
     &,' ivs=',head%ivs,' levs=',head%levs
      endif
      if (iret .ne. 0) then
        print *,' unable to read from unit ',nft,' job aborted'
     &,' iret=',iret,' me=',me
        call mpi_quit(7777)
      endif
!
      idvc = head%idvc  !idvc=3:sigma-theta and/or p, idvc=2:sigma-p, idvc=1:sigma files
      idsl = head%idsl
!     rewind(nft)
!     read(nft)

      rkapi  = 1.0 / rkap
      rkapp1 = 1.0 + rkap

      if (me .eq. 0) then
        print *,' gen_coord_hybrid=',gen_coord_hybrid,
     &' idvc=',head%idvc,' idvm=',head%idvm,' hybrid=',hybrid
      endif
      if (gen_coord_hybrid) then					! hmhj

        sfcpress_id  = mod ( head%idvm , 10 )				! hmhj
        thermodyn_id = mod ( head%idvm / 10 , 10 )			! hmhj
        if (me == 0) then
          write(0,*)' sfcpress_id thermodyn_id ',
     &                sfcpress_id,thermodyn_id
        endif
!   ak bk ck in file have the same order as model			! hmhj
        do k=1,levp1							! hmhj
          ak5(k) = head%vcoord(k,1)/1000.				! hmhj
          bk5(k) = head%vcoord(k,2)					! hmhj
          ck5(k) = head%vcoord(k,3)/1000.				! hmhj
        enddo								! hmhj
        vertcoord_id = 0						! hmhj
        do k=1,levp1							! hmhj
          if( ck5(k).ne.0.0 ) vertcoord_id = 3				! hmhj
        enddo								! hmhj
! provide better estimated press 					! hmhj
        psurfff = 101.3							! hmhj
        if( thermodyn_id == 3 ) then					! hmhj
         do k=1,levp1							! hmhj
          thref(k) = 300.*cpd						! hmhj
          teref(k) = 255.*cpd						! hmhj
         enddo								! hmhj
        else								! hmhj
         do k=1,levp1							! hmhj
          thref(k) = 300.						! hmhj
          teref(k) = 255.						! hmhj
         enddo								! hmhj
        endif								! hmhj
        ck5p(levp1) = ck5(levp1)					! hmhj
        do k=1,levp1							! hmhj
          ck5p(k) = ck5(k)*(teref(k)/thref(k))**rkapi	          	! hmhj
        enddo								! hmhj
        if( me == 0 ) then						! hmhj
          do k=1,levp1							! hmhj
            pressk = ak5(k) + bk5(k)*psurfff + ck5p(k)			! hmhj
            write(0,180) k,ak5(k),bk5(k),ck5(k),pressk
!           print 180,k,ak5(k),bk5(k),ck5(k),pressk			! hmhj
180         format('k=',i3,'  ak5=',f14.9,'  bk5=',e15.8,		! hmhj
     &            '   ck5=',f14.9,'  closed pressk=',f11.6)		! hmhj
          enddo								! hmhj
        endif								! hmhj
        do k=1,levp1							! hmhj
          si(k) = ak5(k)/psurfff + bk5(k) + ck5p(k)/psurfff		! hmhj
        enddo								! hmhj
        do k=1,levs							! hmhj
          sl(k) = 0.5*(si(k)+si(k+1))					! hmhj
        enddo								! hmhj

      else if (hybrid .and. idvc .eq. 2) then				! hmhj
!       idsl=slid  !=2,pk=0.5*(p(k+1/2)+p(k-1/2)) check alfa(1)  am_bm
!   ak bk order in "sigma" file is bottom to top !!!!!!!!!!!!!!!!!!
        psurfff = 101.3
        do k=1,levp1
          ak5(k) = head%vcoord(levp1+1-k,1)/1000.
          bk5(k) = head%vcoord(levp1+1-k,2)
          pressk = ak5(k)+bk5(k)*psurfff
          
          if(me.eq.0)print 190,k,ak5(k),bk5(k),pressk
190       format('k=',i3,'  ak5=',e15.8,'  bk5=',e15.8,
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
          si(levs+2-k)=ak5(k)/psurfff+bk5(k) !ak(k) bk(k) go top to bottom
        enddo
        do k=1,levs
          sl(k)=0.5*(si(k)+si(k+1))
        enddo
!
!     elseif (head%idvc .eq. 1) then
      elseif (head%idvc .le. 1) then
        si(:)    = head%vcoord(:,1)
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
        print *,' non compatible initial state idvc=',head%idvc
     &,' iret=',iret
        call mpi_quit(333)
      endif
!
csela print*,' read second record successfully '
      fhour       = head%fhour
      idate       = head%idate
      waves       = head%jcap
      xlayers     = head%levs
      itrun       = head%itrun
      order       = head%iorder
      realform    = head%irealf
      icen        = 7
      icen2       = head%icen2
      igen        = head%igen
      ienst       = head%iens(1)
      iensi       = head%iens(2)
!     runid       = head%idrun
!     usrid       = head%idusr
      if (fhour .gt. 0.0 .and. head%nxss .eq. 0 .and.
     &    head%pdryini > 0.0 ) then
        if (pdryini .eq. 0.0) pdryini = head%pdryini
      endif
      if (me == 0) print *,' in tread pdryini=',pdryini,
     &                     ' head=',head%pdryini
!sela ntraci = nint(tracers-1)
      ntraci = head%ntrac
      if (head%idvt .gt. 0.0) then
        nt0   = mod(head%idvt,10)
        if (nt0 > 0) then
          ntcwi = head%idvt / 10
          ntozi = head%idvt - ntcwi * 10 + 1
          ntcwi = ntcwi + 1
        else
          ntcwi = ntcw
          ntozi = ntoz
        endif
        ncldi = head%ncldt
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
      ixgr = head%ixgr
      ixga = head%ixga
!
      if (ntrac <= 3) then
        idvt = (ntcw-1)*10 + ntoz - 1
      else
        idvt = head%idvt
      endif
!
      if (me.eq.0) then
        write(*,*)'nfile,in treadeo fhour,idate=',nft,fhour,idate
     &, ' ntozi=',ntozi,' ntcwi=',ntcwi,' ncldi=',ncldi
     &, ' ntraci=',ntraci,' tracers=',head%ntrac,' vtid=',head%idvt
     &,  head%ncldt,' idvc=',head%idvc,' jcap=',head%jcap
     &, ' ixgr=',ixgr,' ixga=',ixga,' pdryini=',pdryini
      endif
!cjfe
      if(iprint.eq.1)
     x print *,'tread unit,fhour,idate=',nft,fhour,idate
 
!
      dati%i = 1                                           ! hs
      dati%f => trisca
      call sigio_rrdbti(nft,head,dati,iret)
      if (me == 0) print *,' z_r=',trisca(1:10),' iret=',iret
      z   = trisca
      z_r = trisca
      call triseori(trisca,gze,gzo,1,ls_node)
      z00=trisca(1)
      ga2=grav/(rerth*rerth)
      do locl=1,ls_max_node
              l=ls_node(locl,1)
         jbasev=ls_node(locl,2)
         indev1 = indlsev(l,l)
         if (mod(l,2).eq.mod(jcap+1,2)) then
            indev2 = indlsev(jcap+1,l)
         else
            indev2 = indlsev(jcap  ,l)
         endif
         do indev = indev1 , indev2
            gze(indev,1)=
     x      gze(indev,1)*snnp1ev(indev)*ga2
            gze(indev,2)=
     x      gze(indev,2)*snnp1ev(indev)*ga2
         end do
      end do
      do locl=1,ls_max_node
              l=ls_node(locl,1)
         jbasod=ls_node(locl,3)
         indod1 = indlsod(l+1,l)
         if (mod(l,2).eq.mod(jcap+1,2)) then
            indod2 = indlsod(jcap  ,l)
         else
            indod2 = indlsod(jcap+1,l)
         endif
         do indod = indod1 , indod2
            gzo(indod,1)=
     x      gzo(indod,1)*snnp1od(indod)*ga2
            gzo(indod,2)=
     x      gzo(indod,2)*snnp1od(indod)*ga2
         end do
      end do
 
      if (mod(head%idvm/10,10) == 3 .and. me == 0)then
        print *,' cpi=',head%cpi(1:ntraci+1)
        print *,' ri=',head%ri(1:ntraci+1)
      endif
      dati%i = 2                               ! surface pressure
      dati%f => trisca                       
      if (read_on_every_task) then
      call sigio_rrdbti(nft,head,dati,iret)
      else
        if (me == me_l_0) call sigio_rrdbti(nft,head,dati,iret)
        if (.not.liope.or.icolor.ne.2) then
          call mpi_bcast(trisca,lnt2,mpi_real8,me_l_0,mc_comp,iret)
        endif
      endif
      if (me == 0) print *,' sfcpres=',trisca(1:10)
      call triseori(trisca,qe,qo,1,ls_node)
      do k=1,levs
        dati%i = k + 2                        ! virtual temperature or cpt
        dati%f => trisca                       
        if (read_on_every_task) then
        call sigio_rrdbti(nft,head,dati,iret)
        else
          if (me == me_l_0) call sigio_rrdbti(nft,head,dati,iret)
          if (.not.liope.or.icolor.ne.2) then
            call mpi_bcast(trisca,lnt2,mpi_real8,me_l_0,mc_comp,iret)
          endif
        endif
        call triseori(trisca,tee(1,1,k),teo(1,1,k),1,ls_node)
      enddo
!
      do k=1,levs
         dati%i = levs + 2 + (k-1) * 2 + 1     ! divergence
         dati%f => trisca
         if (read_on_every_task) then
         call sigio_rrdbti(nft,head,dati,iret)
         else
           if (me == me_l_0) call sigio_rrdbti(nft,head,dati,iret)
           if (.not.liope.or.icolor.ne.2) then
             call mpi_bcast(trisca,lnt2,mpi_real8,me_l_0,mc_comp,iret)
           endif
         endif
         call triseori(trisca,die(1,1,k),dio(1,1,k),1,ls_node)
!
         dati%i = levs + 2 + (k-1) * 2 + 2     ! vorticity
         dati%f => trisca
         if (read_on_every_task) then
         call sigio_rrdbti(nft,head,dati,iret)
         else
           if (me == me_l_0) call sigio_rrdbti(nft,head,dati,iret)
           if (.not.liope.or.icolor.ne.2) then
             call mpi_bcast(trisca,lnt2,mpi_real8,me_l_0,mc_comp,iret)
           endif
         endif
         call triseori(trisca,zee(1,1,k),zeo(1,1,k),1,ls_node)
      end do
csela print*,' levh=',levh
!
!
      rqe=0.
      rqo=0.
      do k=1,ntraci
        kk = 0
        if (k .eq. 1) then
          kk = 1
        elseif (k .eq. ntozi) then
          kk = ntoz
        elseif (k .ge. ntcwi .and. k .lt. ntcwi+ncldi-1) then
          do n=1,ncldi
            if (k .eq. ntcwi+n-1) kk = ntcw+n-1
          enddo
        else
          kk = k
        endif
!
        do lv=1,levs
          dati%i = levs * (2+k) + 2 + lv             ! tracers starting with q
          dati%f => trisca
          if (read_on_every_task) then
          call sigio_rrdbti(nft,head,dati,iret)
          else
            if (me == me_l_0) call sigio_rrdbti(nft,head,dati,iret)
            if (.not.liope.or.icolor.ne.2) then
              call mpi_bcast(trisca,lnt2,mpi_real8,me_l_0,mc_comp,iret)
            endif
          endif
          call triseori(trisca,rqe(1,1,lv,kk),rqo(1,1,lv,kk),1,ls_node)
        end do
      end do

!
!--extra physics arrarys
      if ( ixgr.gt.0 .and. fhour.gt.0.1) then
        kmsk(:,:)  = 0

!       if (head%irealf .eq. 1) then
!         do nn=1,num_p3d
!           do k=1,levs
!             call split2d(data%xgr(1,1,(nn-1)*levs+k)
!    &,                                 buffo,global_lats_r)
!             call interpred(1,kmsk,buffo,buff2,global_lats_r,
!    &                lonsperlar)
!
!             do lan=1,lats_node_r
!               lat = global_lats_r(ipt_lats_node_r-1+lan)
!               lons_lat = lonsperlar(lat)
!               iblk=0
!               il=1
!               do lon=1,lons_lat,ngptc
!                 njeff=min(ngptc,lons_lat-lon+1)
!                 iblk=iblk+1
!                 do i=1,njeff
!                   phy_f3d(i,k,iblk,lan,nn) = buff2(il,lan)
!                   il=il+1
!                 enddo
!               enddo
!             enddo
!           enddo
!         enddo
!         do nn=1,num_p2d
!           call split2d(data%xgr(1,1,num_p3d*levs+nn)
!    &,                                 buffo,global_lats_r)
!           call interpred(1,kmsk,buffo,buff2,global_lats_r,
!    &              lonsperlar)
!           do j=1,lats_node_r
!             do i=1,lonr
!               phy_f2d(i,j,nn) = buff2(i,j)
!             enddo
!           enddo
!         enddo
!       elseif (head%irealf .eq. 2) then

          num_dta = (ntraci+3)*levs + 2

          do nn=1,num_p3d
            do k=1,levs
              dati%i = num_dta + (nn-1)*levs + k      ! physics 3d grid fields
              dati%f => buff1
              call sigio_rrdbti(nft,head,dati,iret)
              call split2d_r(buff1(1),buffo,global_lats_r)
              call interpred(1,kmsk,buffo,buff2,global_lats_r,
     &                lonsperlar)
!
              do lan=1,lats_node_r
                lat = global_lats_r(ipt_lats_node_r-1+lan)
                lons_lat = lonsperlar(lat)
                do i=1,lons_lat
                  phy_f3d(i,k,nn,lan) = buff2(i,lan)
                enddo
              enddo
            enddo
          enddo

          do nn=1,num_p2d
            dati%i = num_dta + num_p3d*levs + nn      ! physics 2d grid fields
            dati%f => buff1
            call sigio_rrdbti(nft,head,dati,iret)
            call split2d_r(buff1,buffo,global_lats_r)
            call interpred(1,kmsk,buffo,buff2,global_lats_r,
     &              lonsperlar)
            do j=1,lats_node_r
              do i=1,lonr
                phy_f2d(i,nn,j) = buff2(i,j)
              enddo
            enddo
          enddo
      endif

      if (head%nxss .gt. 0) then
       dati%i =num_dta+num_p3d*levs+num_p2d+1    ! pdryini
       dati%f => buff1
       call sigio_rrdbti(nft,head,dati,iret)
       pdryini = buff1(1)
      endif
!
!--extra dynamics arrays
      if ( ixga.gt.0 .and. fhour.gt.0.1) then
        kmska(:,:)  = 0

        num_dta = (ntraci+3)*levs + 2
        if(ixgr.gt.0) num_dta = num_dta+num_p3d*levs+num_p2d
        if (head%nxss.gt.0) num_dta=num_dta+1

          do nn=1,num_a3d
            do k=1,levs
              dati%i = num_dta + (nn-1)*levs + k      ! dynamics 3d grid fields
              dati%f => buff1a
              call sigio_rrdbti(nft,head,dati,iret)
              call split2d_a(buff1a(1),buffoa,global_lats_a)
              call interpred_a(1,kmska,buffoa,buff2a,global_lats_a,
     &                lonsperlat)
!
              do lan=1,lats_node_a
                lat = global_lats_a(ipt_lats_node_a-1+lan)
                lons_lat = lonsperlat(lat)
                do i=1,lons_lat
                  dyn_f3d(i,k,nn,lan) = buff2a(i,lan)
                enddo
              enddo
            enddo
          enddo

          do nn=1,num_a2d
            dati%i = num_dta + num_a3d*levs + nn      ! dynamics 2d grid fields
            dati%f => buff1a
            call sigio_rrdbti(nft,head,dati,iret)
            call split2d_a(buff1a,buffoa,global_lats_a)
            call interpred_a(1,kmska,buffoa,buff2a,global_lats_a,
     &              lonsperlat)
            do j=1,lats_node_a
              do i=1,lonf
                dyn_f2d(i,nn,j) = buff2a(i,j)
              enddo
            enddo
          enddo
      endif

      iprint=0
 
100   format(1h0, 12   (e10.3))
101   format (1h0, 'if above two rows not zero,inconsistency in sig.def'
     1,'on nft=',i2,2x,f6.1,2x,4(i4),'z00=',e12.4)
!!!!
!sela do locl=1,ls_max_node
!sela         l=ls_node(locl,1)
!sela    jbasev=ls_node(locl,2)
!sela    do indev = indlsev(l,l) , indlsev(jcap+mod(l,2),l)
!sela       rqe(indev,1,3)=rqe(indev,1,2)
!sela       rqe(indev,2,3)=rqe(indev,2,2)
!sela    end do
!sela end do
!sela do locl=1,ls_max_node
!sela         l=ls_node(locl,1)
!sela    jbasod=ls_node(locl,3)
!sela    do indod = indlsod(l+1,l) , indlsod(jcap+mod(l+1,2),l)
!sela       rqo(indod,1,3)=rqo(indod,1,2)
!sela       rqo(indod,2,3)=rqo(indod,2,2)
!sela    end do
!sela end do
!
!   convert from virtual temperature to enthalpy if need
!
      if( thermodyn_id.le.1 .and. sfcpress_id.le.1
     &    .and. gen_coord_hybrid ) then

!
        if (.not.liope.or.icolor.ne.2) then
!
          direction=1	! from (tv,lnps) to (enthalpy,ps)
          call spect_tv_enthalpy_ps
!!    &       (direction,run_enthalpy,
     &       (direction,
     &        qe,qo,tee,teo,rqe,rqo,
     &        ls_node,ls_nodes,max_ls_nodes,
     &        lats_nodes_r,global_lats_r,lonsperlar,
     &        plnev_r,plnod_r,plnew_r,plnow_r)

        endif	! .not.liope.or.icolor.ne.2
!
        if( run_enthalpy ) then
          do k=1,levp1
            thref(k)=300.*cpd	
            teref(k)=255.*cpd
          enddo		
          thermodyn_id = 3
          sfcpress_id  = 2
        else
          thermodyn_id = 1
          sfcpress_id  = 2
        endif

      endif

      return
      end
      subroutine treadeo_iau(nft,nft2,fhour,idate,
     x                   qe,tee,die,zee,rqe,
     x                   qo,teo,dio,zeo,rqo,
     x                   ls_node,ls_nodes,max_ls_nodes,
     &                   cfile,cfile2,iauinc)
 
      use resol_def
      use layout1
      use coordinate_def
      use sig_io
      use namelist_def
      use vert_def
      use mpi_def
      use sigio_module
      use sigio_r_module
!
      implicit none
      character*(*) cfile,cfile2
      integer              nft,nft2
      real(kind=kind_evod) fhour
      integer              idate(4),ntraci, ntozi, ntcwi, ncldi, 
     &                     nt0, direction
!
      real(kind=kind_evod) qe(len_trie_ls,2)
     &,                    tee(len_trie_ls,2,levs)
     &,                    die(len_trie_ls,2,levs)
     &,                    zee(len_trie_ls,2,levs)
     &,                    rqe(len_trie_ls,2,levs*ntrac)
     &,                     qo(len_trio_ls,2)
     &,                    teo(len_trio_ls,2,levs)
     &,                    dio(len_trio_ls,2,levs)
     &,                    zeo(len_trio_ls,2,levs)
     &,                    rqo(len_trio_ls,2,levs*ntrac)

!
      integer              ls_node(ls_dim,3)
      logical              iauinc
!
!cmr  ls_node(1,1) ... ls_node(ls_max_node,1) : values of l
!cmr  ls_node(1,2) ... ls_node(ls_max_node,2) : values of jbasev
!cmr  ls_node(1,3) ... ls_node(ls_max_node,3) : values of jbasod
!
      integer              ls_nodes(ls_dim,nodes)
      integer              max_ls_nodes(nodes)
      integer              j,k,l,locl,n,lv,kk
      integer              i,lan,lat,lons_lat,nn
      integer              icount
      integer              ifield,lev,nloops,iproc,maxlev,ll
      integer,dimension(5) :: ivar,ilev
      integer,allocatable,dimension(:,:) :: jlev,jproc,jout,jvar
      real(kind=kind_evod), target :: trisca(lnt2),trisca2(lnt2)
!
      type(sigio_head) head,head2
      type(sigio_dbti) dati,dati2
!
      integer              iret
!
 
!!
!     print *,' enter treadeo.io_fd '					! hmhj


      ifield=1
      lev=0
      nloops=((3+ntrac)*levs+1)/nodes_comp+1


      call sigio_rropen(nft,cfile,iret)
      call sigio_alhead(head,iret)
      call sigio_rrhead(nft,head,iret)

      if(me == 0)
     &   print *,' in treadeo iret=',iret,' cfile=',cfile
     &   ,' ivs=',head%ivs,' levs=',head%levs
      if (iret .ne. 0) then
         print *,' unable to read from unit ',nft,' job aborted'
     &   ,' iret=',iret,' me=',me
         call mpi_quit(7777)
      endif

      if(.not. iauinc)then
         call sigio_rropen(nft2,cfile2,iret)
         call sigio_alhead(head2,iret)
         call sigio_rrhead(nft2,head2,iret)
         if(me == 0)
     &   print *,' in treadeo iret=',iret,' cfile2=',cfile2
     &      ,' ivs=',head%ivs,' levs=',head%levs
         if (iret .ne. 0) then
           print *,' unable to read from unit ',nft2,' job aborted'
     &      ,' iret=',iret,' me=',me
           call mpi_quit(8777)
         endif
      end if
!
!
      if(levs /= head%levs .or. jcap /= head%jcap)then
         print *,' in treadeo_iau error in levs or jcap',levs,jcap
         call mpi_quit (7780)
      end if
      if(ntrac /= head%ntrac)then
         print *,' in treadeo_iau error in ntrac ',ntrac,head%ntrac
         call mpi_quit (7781)
      end if
      fhour       = head%fhour

      ntraci = ntrac
      if (idvt .gt. 0) then
        nt0   = mod(idvt,10)
        if (nt0 > 0) then
          ntcwi = idvt / 10
          ntozi = idvt - ntcwi * 10 + 1
          ntcwi = ntcwi + 1
        else
          ntcwi = ntcw
          ntozi = ntoz
        endif
        ncldi = head%ncldt
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

!   Create mapping from file structure to variable/level structure

!                 ! surface height (currently not used)
      ivar(1)=1   ! surface pressure
      ivar(2)=2   ! temperature
      ivar(3)=3   ! divergence
      ivar(4)=4   ! vorticity
      ivar(5)=5   ! tracers
!  Number of levels for each variable
      ilev(1)=1       
      ilev(2)=levs
      ilev(3)=levs
      ilev(4)=levs
      ilev(5)=levs*ntrac

      allocate(jlev(nodes_comp,nloops),jvar(nodes_comp,nloops),
     &     jproc(nodes_comp,nloops),jout(nodes_comp,nloops))
      icount=0
      k = 0
      iproc=-1
      do ll=1,nloops
        maxlev=min(nodes_comp,(3+ntrac)*levs+1-icount)
        if(me == 0)print *,' in treadeo ',maxlev,nodes_comp,nloops
        do i=1,maxlev
           icount=icount+1
           iproc=iproc+1
           if(iproc == nodes_comp)iproc=0
           lev = lev+1
           if(lev > ilev(ifield))then
              ifield=ifield+1
              lev = 1
           end if
           if(ifield == 5)then
              if(lev == 1)k=k+1
              kk = 0
              if (k .eq. 1) then
                 kk = 1
              elseif (k .eq. ntozi) then
                 kk = ntoz
              elseif (k .ge. ntcwi .and. k .lt. ntcwi+ncldi-1) then
                 do n=1,ncldi
                   if (k .eq. ntcwi+n-1) kk = ntcw+n-1
                 enddo
              else
                 kk = k
              end if
              jout(i,ll)=(kk-1)*levs+lev
           else
              jout(i,ll)=lev
           end if
           jlev(i,ll)=lev
           jvar(i,ll)=ifield
           jproc(i,ll)=iproc
!          if(me == 0) 
!    &     print *,'in treadeo j values ',i,ll,jout(i,ll),jlev(i,ll),
!    &     jvar(i,ll),jproc(i,ll)
         end do
      end do
!
      if (me.eq.0) then
        write(*,*)'nfile,in treadeo fhour,idate=',nft,fhour,idate
     &, ' ntozi=',ntozi,' ntcwi=',ntcwi,' ncldi=',ncldi
     &, ' ntrac =',ntrac,' vtid=',head%idvt
     &, ' idvc=',head%idvc,' jcap=',jcap
      endif

      icount=0
      dati%f => trisca                       
      dati2%f => trisca2
      do ll=1,nloops
        maxlev=min(nodes_comp,(3+ntrac)*levs+1-icount)
        do i=1,maxlev
          icount=icount+1
          if(me == jproc(i,ll))then
             if(jvar(i,ll) == 1)then
                dati%i = 2                 ! surface pressure
             else if (jvar(i,ll) == 2)then
                dati%i = jlev(i,ll) + 2       ! virtual temperature or cpt
             else if (jvar(i,ll) == 3)then
                dati%i = levs + 2 + (jlev(i,ll)-1) * 2 + 1     ! divergence
             else if (jvar(i,ll) == 4)then
                dati%i = levs + 2 + (jlev(i,ll)-1) * 2 + 2     ! vorticity
             else if (jvar(i,ll) == 5)then
                dati%i = 3*levs + 2 + jlev(i,ll)          ! tracer
             end if
               
             dati2%i = dati%i
             call sigio_rrdbti(nft,head,dati,iret)
             if(.not. iauinc)then
                call sigio_rrdbti(nft2,head2,dati2,iret)
                trisca=trisca-trisca2
             end if
          end if
        end do
        do i=1,maxlev
          if(me == jproc(i,ll))trisca2=trisca
          call mpi_bcast(trisca2,lnt2,mpi_real8,jproc(i,ll),mc_comp,
     &       iret)
          k=jout(i,ll)
          if(jvar(i,ll) == 1)then
            call triseori(trisca2,qe,qo,1,ls_node)
          else if(jvar(i,ll) == 2)then
            call triseori(trisca2,tee(1,1,k),teo(1,1,k),1,ls_node)
          else if(jvar(i,ll) == 3)then
            call triseori(trisca2,die(1,1,k),dio(1,1,k),1,ls_node)
          else if(jvar(i,ll) == 4)then
            call triseori(trisca2,zee(1,1,k),zeo(1,1,k),1,ls_node)
          else if(jvar(i,ll) == 5)then
            call triseori(trisca2,rqe(1,1,k),rqo(1,1,k),1,ls_node)
          end if
        end do
      end do
      call sigio_rclose(nft,iret)
      if(.not. iauinc)call sigio_rclose(nft2,iret)

      deallocate(jlev,jvar,jproc,jout)

!
!
 
100   format(1h0, 12   (e10.3))
!!!!
!
!   convert from virtual temperature to enthalpy if need
!
      if( thermodyn_id.le.1 .and. sfcpress_id.le.1
     &    .and. gen_coord_hybrid ) then

!
        if (.not.liope.or.icolor.ne.2) then
!
!  Conversion to Enthalpy not allowed for iau update(not linear)
          print *,' Conversion to Entahlpy not allowed for iau '
          call mpi_quit (7790)
!         direction=1               ! from (tv,lnps) to (enthalpy,ps)
!         call spect_tv_enthalpy_ps
!    &       (direction,
!    &        qe,qo,tee,teo,rqe,rqo,
!    &        ls_node,ls_nodes,max_ls_nodes,
!    &        lats_nodes_r,global_lats_r,lonsperlar,
!    &        plnev_r,plnod_r,plnew_r,plnow_r)

        endif                     ! .not.liope.or.icolor.ne.2
!
      endif

      return
      end subroutine treadeo_iau

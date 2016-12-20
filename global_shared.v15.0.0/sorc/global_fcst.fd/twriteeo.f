      subroutine twriteeo(nsig,ioproc,fhour,idate,
     x           zqe_ls,qe_ls,tee_ls,die_ls,zee_ls,rqe_ls,gze_ls,
     x           zqo_ls,qo_ls,teo_ls,dio_ls,zeo_ls,rqo_ls,gzo_ls,
     x           sl,si,pdryini,
     x           ls_nodes,max_ls_nodes,ixgr,ixga,
     &           phy_f3d,phy_f2d,global_lats_r,lonsperlar,
     &           dyn_f3d,dyn_f2d,global_lats_a,lonsperlat)
!
!
      use resol_def
      use layout1
      use coordinate_def					! hmhj
      use sig_io
      use namelist_def
      use mpi_def
      use sigio_module
      use sigio_r_module
      use tracer_const
!
      implicit none
!
      integer   nsig, ioproc
      real (kind=kind_phys)
     &     phy_f3d(lonr,levs,num_p3d,lats_node_r)
     &,    phy_f2d(lonr,num_p2d,lats_node_r)
      real (kind=kind_evod)
     &     dyn_f3d(lonf,levs,num_a3d,lats_node_a)
     &,    dyn_f2d(lonf,num_a2d,lats_node_a)
!
      integer kmsk0(lonr,lats_node_r), global_lats_r(latr)
     &,       lonsperlar(latr)
      real(kind=kind_io8) buffo(lonr,lats_node_r)
     &,                   buff1(lonr,lats_node_r)
      real (kind=kind_ior), target :: buff2(lonr*latr)
!
      integer kmsk0a(lonf,lats_node_a), global_lats_a(latg)
     &,       lonsperlat(latg)
      real(kind=kind_io8) buffoa(lonf,lats_node_a)
     &,                   buff1a(lonf,lats_node_a)
      real (kind=kind_ior), target :: buff2a(lonf*latg)
!
      real(kind=8) t1,t2,t3,t4,rtc
!
      real(kind=kind_evod) fhour
!
      integer              idate(4)
!
      real(kind=kind_evod) zqe_ls(len_trie_ls,2)
     &,                     qe_ls(len_trie_ls,2)
     &,                    tee_ls(len_trie_ls,2,levs)
     &,                    die_ls(len_trie_ls,2,levs)
     &,                    zee_ls(len_trie_ls,2,levs)
     &,                    rqe_ls(len_trie_ls,2,levh)
     &,                    gze_ls(len_trie_ls,2)
!
     &,                    zqo_ls(len_trio_ls,2)
     &,                     qo_ls(len_trio_ls,2)
     &,                    teo_ls(len_trio_ls,2,levs)
     &,                    dio_ls(len_trio_ls,2,levs)
     &,                    zeo_ls(len_trio_ls,2,levs)
     &,                    rqo_ls(len_trio_ls,2,levh)
     &,                    gzo_ls(len_trio_ls,2)
!
      real (kind=kind_io8) pdryini
!!
      real(kind=kind_evod) sl(levs), si(levp1)
!
      integer              ls_nodes(ls_dim,nodes)
      integer              max_ls_nodes(nodes)
!
      integer              ierr,j,k,l,lenrec,locl,n,node
!
      integer              indjoff
      integer              indev
      integer              indod
      integer              indev1,indev2
      integer              indod1,indod2
      integer              ixgr,ixga
!
      real(kind=kind_ior), target ::   buf(lnt2)
      real(kind=kind_ior)   tmps(4+nodes+jcap1*nodes)
      real(kind=kind_ior)   tmpr(3+nodes+jcap1*(nodes-1))
!
      real (kind=kind_evod), target :: data_xss(1)
      type(sigio_head) head
      type(sigio_dbti) dati
      integer iret, num_dta, a, b
      logical first
      save head, first
      data first /.true./
!
      integer  il,ilen,i,msgtag,ls_diml,nodesl
c$$$      integer              kwq
c$$$      integer              kwte
c$$$      integer              kwdz
c$$$      integer              kwrq
c$$$cc
c$$$      parameter ( kwq  = 0*levs+0*levh+1 ,  !   qe/o_ls
c$$$     x            kwte = 0*levs+0*levh+2 ,  !  tee/o_ls
c$$$     x            kwdz = 1*levs+0*levh+2 ,  !  die/o_ls  zee/o_ls
c$$$     x            kwrq = 3*levs+0*levh+2 )  !  rqe/o_ls
cc
cc
      integer              indlsev,jbasev
      integer              indlsod,jbasod
!
      include 'function_indlsev'
      include 'function_indlsod'
!
!
      integer              joff, jj
!
      joff(n,l)=(jcap1)*(jcap2)-(jcap1-l)*(jcap2-l)+2*(n-l)
!
!
      real(kind=kind_mpi_r),allocatable :: trieo_ls_node (:,:,:)
      real(kind=kind_mpi_r),allocatable :: trieo_ls_nodes(:,:,:,:)
!
      integer      lan,lat,lons_lat,nn
!
!
!$$$      real(kind=kind_io4) z(lnt2)
!     real(kind=kind_io8) buffo(lonr,lats_node_r)
!!
!$$$      common /z00_com/z
!!

      if (me == 0)
     & write(0,*)' gwvx alloc  trieo_ls_node  old twriteeo'

      allocate ( trieo_ls_node  ( len_trie_ls_max+len_trio_ls_max,
     &                            2, 3*levs+1*levh+1 ) )

      if (me == 0)
     & write(0,*)' gwvx allocated  trieo_ls_node old twriteeo '
!
      if (comp_task) then
!
        do j=1,len_trie_ls
          trieo_ls_node(j,1,kwq) = qe_ls(j,1)
          trieo_ls_node(j,2,kwq) = qe_ls(j,2)
        enddo
!
        do j=1,len_trio_ls
          trieo_ls_node(j+len_trie_ls_max,1,kwq) = qo_ls(j,1)
          trieo_ls_node(j+len_trie_ls_max,2,kwq) = qo_ls(j,2)
        enddo
!
!$omp parallel do private(k,j)
        do k=1,levs
          do j=1,len_trie_ls
            trieo_ls_node(j,1,kwte+  k-1) = tee_ls(j,1,k)
            trieo_ls_node(j,2,kwte+  k-1) = tee_ls(j,2,k)
            trieo_ls_node(j,1,kwdz+2*k-2) = die_ls(j,1,k)
            trieo_ls_node(j,2,kwdz+2*k-2) = die_ls(j,2,k)
            trieo_ls_node(j,1,kwdz+2*k-1) = zee_ls(j,1,k)
            trieo_ls_node(j,2,kwdz+2*k-1) = zee_ls(j,2,k)
          enddo
          do j=1,len_trio_ls
            jj = j+len_trie_ls_max
            trieo_ls_node(jj,1,kwte+  k-1) = teo_ls(j,1,k)
            trieo_ls_node(jj,2,kwte+  k-1) = teo_ls(j,2,k)
            trieo_ls_node(jj,1,kwdz+2*k-2) = dio_ls(j,1,k)
            trieo_ls_node(jj,2,kwdz+2*k-2) = dio_ls(j,2,k)
            trieo_ls_node(jj,1,kwdz+2*k-1) = zeo_ls(j,1,k)
            trieo_ls_node(jj,2,kwdz+2*k-1) = zeo_ls(j,2,k)
          enddo
        enddo
!
!$omp parallel do private(k,j)
        do k=1,levh
          do j=1,len_trie_ls
            trieo_ls_node(j,1,kwrq+  k-1) = rqe_ls(j,1,k)
            trieo_ls_node(j,2,kwrq+  k-1) = rqe_ls(j,2,k)
          enddo
          do j=1,len_trio_ls
            jj = j+len_trie_ls_max
            trieo_ls_node(jj,1,kwrq+  k-1) = rqo_ls(j,1,k)
            trieo_ls_node(jj,2,kwrq+  k-1) = rqo_ls(j,2,k)
          enddo
        enddo
      endif !  comp_task
!
      if (liope) then
        if (me == 0) then
          tmps            = 0.
          tmps(1)         = pdryini
          tmps(2:nodes+1) = max_ls_nodes(1:nodes)
          tmps(nodes+2)   = ls_dim
          tmps(nodes+3)   = len_trie_ls_max
          tmps(nodes+4)   = len_trio_ls_max
          il=nodes+4
          do i=1,nodes
            do j=1,ls_dim
              il=il+1
              tmps(il) = ls_nodes(j,i)
            enddo
          enddo
          ilen   = 4+nodes+jcap1*nodes
          msgtag = 2345
          call mpi_send(tmps,ilen,mpi_r_io_r,ioproc,
     &                  msgtag,mpi_comm_all,info)
        endif
        if (me == ioproc) then
          ilen   = 4+nodes-1+jcap1*(nodes-1)
          msgtag = 2345
          call mpi_recv(tmpr,ilen,mpi_r_io_r,0,
     &                  msgtag,mpi_comm_all,stat,info)
          ls_nodes = 0
          max_ls_nodes(1:nodes-1) = int(tmpr(2:nodes-1+1))
          ls_diml                 = int(tmpr(nodes+1))
          len_trie_ls_max         = int(tmpr(nodes+2))
          len_trio_ls_max         = int(tmpr(nodes+3))
          il=nodes+3
          do i=1,nodes-1
            do j=1,ls_diml
              il=il+1
              ls_nodes(j,i) = int(tmpr(il))
            enddo
          enddo
        endif
      else
        tmpr(1) = pdryini
      endif                 ! end of if (liope) 
!
!x      print *,' gwvx alloc  trieo_ls_nodes '
      if ( me == ioproc ) then
!gwv      write(0,*)  'alloc parms twrite ',len_trie_ls_max+len_trio_ls_max,
!gwv     1 2,3*levs+1*levh+1, nodes,1
 
         allocate ( trieo_ls_nodes ( len_trie_ls_max+len_trio_ls_max,
     x                               2, 3*levs+1*levh+1, nodes ),
     1              stat=ierr )
      else
         allocate ( trieo_ls_nodes ( 2, 2, 2, 2 ),stat=ierr )
      endif
      if (ierr .ne. 0) then
        write (0,*) ' gwx trieo_ls_nodes allocate failed'
        call mpi_abort(mpi_comm_all,ierr,i)
       endif
!
      lenrec = (len_trie_ls_max+len_trio_ls_max) * 2 * (3*levs+1*levh+1)
!
!     t1=rtc()

      if (jcap < 1200 .and. ixgr <= 0) then
        call mpi_gather(trieo_ls_node , lenrec, mpi_r_mpi_r,
     &                  trieo_ls_nodes, lenrec, mpi_r_mpi_r,
     &                  ioproc, mpi_comm_all, ierr)
      else
        call mpi_gathe8(trieo_ls_node , lenrec, mpi_r_mpi_r,
     &                  trieo_ls_nodes, lenrec, mpi_r_mpi_r,
     &                  ioproc, mpi_comm_all, ierr)
      endif
!
!     t2=rtc()
!sela print *,' wrt twrite gather 1 time ',t2-t1
!
      deallocate ( trieo_ls_node  )
!
      if ( me .ne. ioproc ) then
           deallocate ( trieo_ls_nodes )
      endif
!
      if (me == ioproc) then
!
!     print *,' in twriteeo fhour=',fhour
!     if (fhour .gt. 0.0) call mpi_quit(1111)
!
        head%ivs     = 200509
        head%levs    = levs
        head%ntrac   = ntrac
        if (gen_coord_hybrid) then				! hmhj
          head%nvcoord = 3					! hmhj
        else if (hybrid) then					! hmhj
          head%nvcoord = 2
        else
          head%nvcoord = 1
        endif
!
        if (first) then
!         allocate(dati%f(lonr*latr))
!         call sigio_alhead(head,iret)
          idvm = thermodyn_id*10 + sfcpress_id
          call sigio_alhead(head,iret,levs,head%nvcoord,ntrac,idvm)
          first = .false.
        endif
!
        rewind(nsig)
!       head%clabsig=char(0)//char(0)//char(0)//char(0)//
!    &               char(0)//char(0)//char(0)//char(0)
        head%clabsig='GFS SIG '
!       write(nsig)head%clabsig
!     print 3000,lab,n
 3000 format(/ 'twriteeo lab ',4a10,' n=',i3)
!
        head%fhour   = fhour
        head%idate   = idate
        head%jcap    = jcap
        head%levs    = levs
        head%latb    = latr
        head%lonb    = lonr
        head%itrun   = itrun
        head%iorder  = 2
        head%irealf  = 2      ! for real * 8 data
        head%igen    = igen
        head%latf    = latg
        head%latr    = latr
        head%lonf    = lonf
        head%lonr    = lonr
        head%ntrac   = ntrac
        head%icen2   = icen2
        head%iens(1) = ienst
        head%iens(2) = iensi
        head%idpp    = 0
        head%idsl    = 0    ! idsl=2 for middle of layer
        head%idvm    = 0
        head%idvt    = idvt
!       head%idvt    = (ntoz-1) + 10 * (ntcw-1)
        head%idrun   = 0
        head%idusr   = 0
        head%pdryini = tmpr(1)
        head%ncldt   = ncld
        head%ixgr    = ixgr
        head%ixga    = ixga
        if (ixgr > 0) then
          head%nxgr  = num_p3d*levs + num_p2d
        else
          head%nxgr  = 0
        endif
        if (ixga > 0) then
          head%nxga  = num_a3d*levs + num_a2d
        else
          head%nxga  = 0
        endif
        if(me == 0) then 
          print*,'head%nxgr:',head%nxgr , ' head%ixgr:',ixgr
          print*,'head%nxga:',head%nxga , ' head%ixga:',ixga
        endif

!--for physics
        a = mod(head%ixgr,10)
        b = int(mod(head%ixgr,100)/10)
        if (a == 3 .or. b == 2) then  !zhao or ferrier micro with nxss=1
          head%nxss  = 1
        else
          head%nxss  = 0
        endif
        if (me == 0) print*,'head%nxss:',head%nxss, ' a=',a, ' b=',b 
!!
!!
        if (gen_coord_hybrid) then				! hmhj

          head%idvc    = 3					! hmhj
          head%idvm    = thermodyn_id*10 + sfcpress_id 		! hmhj
          head%idsl    = 2    ! idsl=2 for middle of layer	! hmhj
          do k=1,levp1						! hmhj
            head%vcoord(k,1) = ak5(k)*1000.			! hmhj
            head%vcoord(k,2) = bk5(k)				! hmhj
            head%vcoord(k,3) = ck5(k)*1000.			! hmhj
          enddo							! hmhj
          if (thermodyn_id == 3) then
            head%cpi(1:ntrac+1) = cpi(0:ntrac)
            head%ri(1:ntrac+1)  = ri(0:ntrac)
          endif
!     print *,' thermodyn_id=',thermodyn_id,' cpi=',head%cpi(0:ntrac+1)
!    &,' ri=',head%ri(1:ntrac+1)

        else if (hybrid) then					! hmhj
          head%idvc    = 2    ! for hybrid vertical coord.
          do k=1,levp1
!sela       ak5(k)=ak5r4(levp1+1-k)/1000.  !this is from tread
            head%vcoord(k,1) = ak5(levp1+1-k)*1000.


!sela       bk5(k)=bk5r4(levp1+1-k)        !this is from tread
            head%vcoord(k,2) = bk5(levp1+1-k)


!           if(me == 0)print 190,k,head%vcoord(k,1),head%vcoord(k,2)
190         format('in twrite k=',i2,'  ak5r4=',f13.6,'  bk5r4=',e13.5)

          enddo
        else
          head%idvc    = 1    ! for sigma vertical coord. (default)
          head%vcoord(:,1) = si (:)
        endif
!
        call sigio_rwhead(nsig,head,iret)
!
        buf = z_r
        dati%i=1
        dati%f => buf
        call sigio_rwdbti(nsig,head,dati,iret)
!!
!       t3=rtc()
!
        if (liope) then
          nodesl = nodes-1
        else
          nodesl = nodes
        endif
!!
        do k=1,3*levs+1*levh+1
          do node=1,nodesl
!
            jbasev = 0
            do locl=1,max_ls_nodes(node)
              l=ls_nodes(locl,node)
              indev1 = indlsev(l,l)
              if (mod(l,2) == mod(jcap+1,2)) then
                indev2 = indlsev(jcap-1,l)
              else
                indev2 = indlsev(jcap  ,l)
              endif
              indjoff=joff(l,l)
              do indev = indev1 , indev2
                buf(indjoff+1) = trieo_ls_nodes(indev,1,k,node)
                buf(indjoff+2) = trieo_ls_nodes(indev,2,k,node)
                indjoff = indjoff+4
              end do
              jbasev = jbasev + (jcap+3-l)/2
            end do
!
            jbasod=len_trie_ls_max
            do locl=1,max_ls_nodes(node)
              l=ls_nodes(locl,node)
!
              if ( l /= jcap ) then  ! fix for out of bound error
                indod1 = indlsod(l+1,l)
                if (mod(l,2) == mod(jcap+1,2)) then
                  indod2 = indlsod(jcap  ,l)
                else
                  indod2 = indlsod(jcap-1,l)
                endif
                indjoff=joff(l+1,l)
                do indod = indod1 , indod2
                  buf(indjoff+1) = trieo_ls_nodes(indod,1,k,node)
                  buf(indjoff+2) = trieo_ls_nodes(indod,2,k,node)
                  indjoff=indjoff+4
                end do
                jbasod = jbasod+(jcap+2-l)/2
              endif  ! fix for out of bound error
            end do
          end do
!
          dati%i = k+1
          dati%f => buf
          call sigio_rwdbti(nsig,head,dati,iret)
        end do
!
!
!       t4=rtc  ()
!sela print *, ' disk time for sig twriteo wrt ',t4-t3
!
!       print *,' twriteeo fhour=',fhour,' idate=',idate,' nsig=',nsig
!       print 3001,fhour,idate,nsig
!3001 format(/ 'twriteeo fhour=',f6.2,2x,4i5,2x,'n=',i2)
!
        num_dta = 3*levs+1*levh+2
        deallocate ( trieo_ls_nodes )
!
      endif   !me == ioproc
!!
!!--write for extra physics fields
      if (ixgr  > 0) then
!$omp parallel do private(lan,i)
        do lan=1,lats_node_r
          do i=1,lonr
            kmsk0(i,lan) = 0
          enddo
        enddo
        do nn=1,num_p3d
          do k=1,levs
!$omp parallel do private(lan,i)
            do lan=1,lats_node_r
              do i=1,lonr
                buff1(i,lan) = 0.0
              enddo
            enddo
!$omp parallel do private(lan,lat,lons_lat,i)
            do lan=1,lats_node_r
              lat = global_lats_r(ipt_lats_node_r-1+lan)
              lons_lat = lonsperlar(lat)
              do i=1,lons_lat
                buff1(i,lan) = phy_f3d(i,k,nn,lan)
              enddo
            enddo
            call uninterpred(1,kmsk0,buffo,buff1,global_lats_r
     &,                                         lonsperlar)
            call unsplit2d_r(ioproc,buff2(1),buffo, global_lats_r)
!
            if (me == ioproc) then
              dati%i = num_dta + (nn-1)*levs + k
              dati%f => buff2
              call sigio_rwdbti(nsig,head,dati,iret)
            endif
          end do
        end do
!
!
        do nn=1,num_p2d
!$omp parallel do private(lan,i)
          do lan=1,lats_node_r
            do i=1,lonr
              buff1(i,lan) = phy_f2d(i,nn,lan)
            enddo
          enddo
          call uninterpred(1,kmsk0,buffo,buff1,global_lats_r,lonsperlar)
          call unsplit2d_r(ioproc,buff2(1),buffo,global_lats_r)
           if (me == ioproc) then
             dati%i = num_dta + num_p3d*levs + nn
             dati%f => buff2
             call sigio_rwdbti(nsig,head,dati,iret)
          endif
        enddo
!!
        if (me == ioproc) then
          if (head%nxss .gt. 0) then              
            data_xss(1) = tmpr(1)
            dati%i = num_dta + num_p3d*levs + num_p2d + 1
            dati%f => data_xss
            call sigio_rwdbti(nsig,head,dati,iret)
          endif
        endif
      endif
!
      if (me.eq.ioproc) then
        num_dta = 3*levs+1*levh+2
        if (ixgr .gt. 0) num_dta = num_dta + num_p3d*levs + num_p2d 
        if (head%nxss .gt. 0) num_dta = num_dta + 1
      endif

!!--write for extra dynamics fields
      if (ixga .gt. 0) then
        kmsk0a=0
        do nn=1,num_a3d
          do k=1,levs
            buff1a = 0.0
            do lan=1,lats_node_a
              lat = global_lats_a(ipt_lats_node_a-1+lan)
              lons_lat = lonsperlat(lat)
              do i=1,lons_lat
                buff1a(i,lan) = dyn_f3d(i,k,nn,lan)
              enddo
            enddo
            call uninterpred_a(1,kmsk0a,buffoa,buff1a,
     &                       global_lats_a,lonsperlat)
            call unsplit2d_a(ioproc,buff2a(1),buffoa, global_lats_a)
!
            if (me.eq.ioproc) then
              dati%i = num_dta + (nn-1)*levs + k
              dati%f => buff2a
              call sigio_rwdbti(nsig,head,dati,iret)
            endif
          end do
        end do
!
        do nn=1,num_a2d
          buff1a(:,:) = dyn_f2d(:,nn,:)
          call uninterpred_a(1,kmsk0a,buffoa,buff1a,
     &                       global_lats_a,lonsperlat)
          call unsplit2d_a(ioproc,buff2a(1),buffoa,global_lats_a)
           if (me.eq.ioproc) then
             dati%i = num_dta + num_a3d*levs + nn
             dati%f => buff2a
             call sigio_rwdbti(nsig,head,dati,iret)
          endif
        enddo
      endif

!     print *,' leave twriteeo_fd ' 				! hmhj

      return
      end

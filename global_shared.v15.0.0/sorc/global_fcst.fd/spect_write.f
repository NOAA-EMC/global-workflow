      subroutine spect_write(nsig,ioproc,fhour,idate,
     &                       sl,si,pdryini,
     &                       ls_nodes,max_ls_nodes,
     &                       global_lats_r,lonsperlar,
     &                       trieo_ls_nodes_buf,ixgr)
!
      use resol_def
      use layout1
      use coordinate_def
      use sig_io
      use namelist_def
      use mpi_def
      use sigio_module
      use sigio_r_module
      use tracer_const
      implicit none
!
      integer              nsig,ioproc

!     real(kind=8) t1,t2,t3,t4,rtc
!     real(kind=8) t5,t6,ta,tb
!
      real(kind=kind_evod) fhour
!
!     logical hybrid
      integer              idate(4)
!
      real (kind=kind_io8) pdryini
!!
      real(kind=kind_evod) sl(levs)
      real(kind=kind_evod) si(levp1)
!
      integer              ls_nodes(ls_dim,nodes)
      integer              max_ls_nodes(nodes)
!
      integer              j,k,l,locl,n,node
!     integer              ivs_loc
!
      integer              indjoff
      integer              indev
      integer              indod
      integer              ixgr
!
      real(kind=kind_io4), target :: buf(lnt2)
!
      integer              global_lats_r(latr)
      integer                 lonsperlar(latr)
!
      type(sigio_head) head
      type(sigio_dati) dati
      integer iret
      logical first
      save head, first
      data first /.true./
!
!
      integer  i,nodesl
!$$$      integer              kwq
!$$$      integer              kwte
!$$$      integer              kwdz
!$$$      integer              kwrq
!
!$$$      parameter ( kwq  = 0*levs+0*levh+1 ,  !   qe/o_ls
!$$$     x            kwte = 0*levs+0*levh+2 ,  !  tee/o_ls
!$$$     x            kwdz = 1*levs+0*levh+2 ,  !  die/o_ls  zee/o_ls
!$$$     x            kwrq = 3*levs+0*levh+2 )  !  rqe/o_ls
!
!
      integer              indlsev,jbasev
      integer              indlsod,jbasod
!
      include 'function_indlsev'
      include 'function_indlsod'
!
!
      integer              joff
!
      joff(n,l) = (jcap1)*(jcap2)-(jcap1-l)*(jcap2-l)+2*(n-l)
!
      real(kind=kind_mpi)trieo_ls_nodes_buf
     & (len_trie_ls_max+len_trio_ls_max, 2, 3*levs+1*levh+1,nodes,1)
 
!
!     integer kmsk0(lonr,lats_node_r)
!
!$$$      real(kind=kind_io4) z(lnt2)
!
      if (me == ioproc) then
!
        if (first) then
          first = .false.
!         if (ivsupa .gt. 0) head%ivs = ivsupa
!         ivs_loc = max(ivsinp, ivsupa)
!         head%ivs = max(head%ivs, ivs_loc)
          head%ivs = 198410
          if (levs > 99) head%ivs = 200509
          head%levs    = levs
          head%ntrac   = ntrac
          if (gen_coord_hybrid) then                           ! hmhj
            head%ivs = 200509
            head%nvcoord = 3                                   ! hmhj
!           call sigio_alhead(head,iret,levs,3,ntrac)	       ! hmhj
          else if (hybrid) then                                ! hmhj
            head%nvcoord = 2
!           call sigio_alhead(head,iret,levs,2,ntrac)
          endif
          idvm = thermodyn_id*10 + sfcpress_id
!     print *,' before alhead idvm=',idvm,thermodyn_id,sfcpress_id
          call sigio_alhead(head,iret,levs,head%nvcoord,ntrac,idvm)
          head%nxgr = 0
          head%nxss = 0
        endif
        rewind(nsig)
        head%clabsig=char(0)//char(0)//char(0)//char(0)//
     &               char(0)//char(0)//char(0)//char(0)
!       write(nsig)lab
!       print 3000,lab,n
 3000 format(/ 'twriteeo lab ',4a10,' n=',i3)
!
        head%fhour   = fhour
        head%idate   = idate
        head%jcap    = jcap
        head%latb    = latr
        head%lonb    = lonr
        head%itrun   = 1
        head%iorder  = 2
        head%irealf  = 1      ! for real * 4 data
        head%igen    = igen
        head%latf    = latg
        head%latr    = latr
        head%lonf    = lonf
        head%lonr    = lonr
        head%icen2   = icen2
        head%iens(1) = ienst
        head%iens(2) = iensi
        head%idpp    = 0
        head%idsl    = 0    ! idsl=2 for middle of layer
        head%idvm    = 0
!       head%idvt    = (ntoz-1) + 10 * (ntcw-1)
        head%idvt    = idvt
        head%idrun   = 0
        head%idusr   = 0
        head%pdryini = pdryini
        head%ncldt   = ncld
        head%ixgr   = ixgr
!hmhj   head%ldata(:) = lnt2

!!
        if (gen_coord_hybrid) then                              ! hmhj
          head%idvc    = 3                                      ! hmhj
          head%idvm    = thermodyn_id*10+sfcpress_id            ! hmhj
          head%idsl    = 2                                      ! hmhj
          do k=1,levp1                                          ! hmhj
            head%vcoord(k,1) = ak5(k)*1000.                     ! hmhj
            head%vcoord(k,2) = bk5(k)                           ! hmhj
            head%vcoord(k,3) = ck5(k)*1000.                     ! hmhj
          enddo
          if (thermodyn_id == 3) then
            head%cpi(1:ntrac+1) = cpi(0:ntrac)
            head%ri(1:ntrac+1)  = ri(0:ntrac)
          endif
!     print *,' thermodyn_id=',thermodyn_id,' cpi=',head%cpi(0:ntrac+1)
!    &,' ri=',head%ri(1:ntrac+1)
        else if (hybrid) then                                   ! hmhj
          head%idvc    = 2    ! for hybrid vertical coord.
          do k=1,levp1
!sela       ak5(k)=ak5r4(levp1+1-k)/1000.  !this is from tread
            head%vcoord(k,1) = ak5(levp1+1-k)*1000.
!sela       bk5(k)=bk5r4(levp1+1-k)        !this is from tread
            head%vcoord(k,2) = bk5(levp1+1-k)
!           print 190,k,head%vcoord(k,1),head%vcoord(k,2)
190         format('in twrite k=',i2,'  ak5r4=',f13.6,'  bk5r4=',e13.5)
          enddo
        endif
!
!       head%ndata = 3*levs+levh+2
        call sigio_rwhead(nsig,head,iret)
!
        buf    =  z
        dati%i = 1
        dati%f => buf
        call sigio_rwdati(nsig,head,dati,iret)
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
           jbasev=0
           do locl=1,max_ls_nodes(node)
             l = ls_nodes(locl,node)
             indjoff = joff(l,l)
             do indev = indlsev(l,l) , indlsev(jcap-mod(l,2),l)
               buf(indjoff+1) = trieo_ls_nodes_buf(indev,1,k,node,1)
               buf(indjoff+2) = trieo_ls_nodes_buf(indev,2,k,node,1)
               indjoff = indjoff + 4
             end do
              jbasev = jbasev + (jcap+3-l)/2
           end do
!
           jbasod = len_trie_ls_max
           do locl=1,max_ls_nodes(node)
             l = ls_nodes(locl,node)
             if ( l /= jcap ) then  ! fix for out of bound error
               indjoff = joff(l+1,l)
               do indod = indlsod(l+1,l) , indlsod(jcap-mod(l+1,2),l)
                  buf(indjoff+1) = trieo_ls_nodes_buf(indod,1,k,node,1)
                  buf(indjoff+2) = trieo_ls_nodes_buf(indod,2,k,node,1)
                  indjoff = indjoff + 4
               end do
               jbasod = jbasod + (jcap+2-l)/2
             endif  ! fix for out of bound error
           end do
         end do
!
          dati%i = k+1
          dati%f => buf
          call sigio_rwdati(nsig,head,dati,iret)
       end do
!
!
!       t4=rtc  ()
!sela   print *, ' disk time for sig twriteo wrt ',t4-t3
!
        write(0,*)' twriteeo fhour=',fhour,' idate=',idate,' nsig=',nsig
!       print 3001,fhour,idate,nsig
!3001   format(/ 'twriteeo fhour=',f6.2,2x,4i5,2x,'n=',i2)
!
      endif   !me == ioproc
!!
      return
      end

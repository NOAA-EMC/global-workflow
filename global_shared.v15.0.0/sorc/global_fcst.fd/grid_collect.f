      subroutine grid_collect
     &        (zsg,psg,uug,vvg,teg,rqg,dpg,
     &        global_lats_r,lonsperlar)
!
      use resol_def
      use mod_state
      use layout1
      use mpi_def
      implicit none
!!
      integer              global_lats_r(latr)
      integer              lonsperlar(latr)
!!
      real(kind=kind_rad) zsg(lonr,lats_node_r)
      real(kind=kind_rad) psg(lonr,lats_node_r)
      real(kind=kind_rad) uug(lonr,lats_node_r,levs)
      real(kind=kind_rad) vvg(lonr,lats_node_r,levs)
      real(kind=kind_rad) teg(lonr,lats_node_r,levs)
      real(kind=kind_rad) rqg(lonr,lats_node_r,levh)
      real(kind=kind_rad) dpg(lonr,lats_node_r,levs)
!
      real(kind=kind_io8) buffo(lonr,lats_node_r)
      real(kind=kind_io8) buffi(lonr,lats_node_r)
      integer kmsk(lonr,lats_node_r)
      integer k
      integer icount
!
      data  icount/0/
!
      ngridg=1
      if(allocated(buff_mult_pieceg)) then
         continue
      else
         allocate(buff_mult_pieceg(lonr,ngrids_gg,lats_node_r))
      endif
!
      kmsk = 0
!
      buffi(:,:) = zsg(:,:)
      call uninterpreg(1,kmsk,buffo,buffi,global_lats_r,lonsperlar)
!
      buffi(:,:) = psg(:,:)
      call uninterpreg(1,kmsk,buffo,buffi,global_lats_r,lonsperlar)
!
      do k=1,levs
        buffi(:,:) = dpg(:,:,k)
        call uninterpreg(1,kmsk,buffo,buffi,global_lats_r,lonsperlar)
      enddo
      do k=1,levs
        buffi(:,:) = uug(:,:,k)
        call uninterpreg(1,kmsk,buffo,buffi,global_lats_r,lonsperlar)
      enddo
      do k=1,levs
        buffi(:,:) = vvg(:,:,k)
        call uninterpreg(1,kmsk,buffo,buffi,global_lats_r,lonsperlar)
      enddo
      do k=1,levs
        buffi(:,:) = teg(:,:,k)
        call uninterpreg(1,kmsk,buffo,buffi,global_lats_r,lonsperlar)
      enddo
      if (levh .gt. 0) then
        do k=1,levh
          buffi(:,:) = rqg(:,:,k)
          call uninterpreg(1,kmsk,buffo,buffi,global_lats_r,lonsperlar)
        enddo
      endif
!
!     print *,' finished gridc_collect for  ngridg=',ngridg
  999 continue
      ngrid=1
      return
      end

       subroutine atmgg_move(ioproc)
c
c***********************************************************************
c
      use resol_def
      use mod_state
      use layout1
      use mpi_def
      implicit none
!
c     integer lats_nodes_r(nodes),ipt,ioproc,nproct
      integer ioproc
      integer proc,msgtag,i,ierr
      integer illen
!     integer ubound
      integer icount
      data icount/0/
      integer maxlats_comp
c  allocate the data structures
c
      if(icount .eq. 0) then
         allocate(ivarg_global(10))
         allocate(ivarg_global_a(10,nodes))
         ivarg_global(1)=ipt_lats_node_r
         ivarg_global(2)= lats_node_r
         ivarg_global(3)=lats_node_r_max
         call mpi_gather(ivarg_global,10,mpi_integer,
     &       ivarg_global_a,10,mpi_integer,ioproc,mpi_comm_all,ierr)
         icount=icount+1
      endif
!     print *,' icount=',icount
!!
      if(allocated(buff_mult_piecesg)) then
          continue
      else
          maxlats_comp=lats_node_r_max
          if(.not. liope .or. me .ne. ioproc) then
            continue
          else
c           maxlats_comp=ivarg_global_a(3,ioproc)
            maxlats_comp=ivarg_global_a(3,1)
          endif
!         print *,' index for maxlat set ',ioproc
cgwv watch this!!
          print *,' allocating ', lonr,ngrids_sfcc,maxlats_comp,nodes
          allocate
     1    (buff_mult_piecesg(lonr,ngrids_gg,maxlats_comp,nodes))
          print *,' allocated', lonr,ngrids_gg,maxlats_comp,nodes
      endif


c
c   sendloop of grids from comp processors to i/o task.  the
c   i/o task may or may not be a comp task also.  the
c   send logic on that task is different for these two cases
c
c  big send
c     if(me .gt. -1) return
!
!      buff_mult_piece(:,1:ngrids_sfcc,:)=
!    1 buff_mult_piecea(:,1:ngrids_sfcc,:)
!
      if (me .ne. ioproc) then    !   sending the data
         msgtag=me
         illen=lats_node _r
         call mpi_send            !  send the local grid domain
     &(buff_mult_pieceg,illen*lonr*ngrids_gg,mpi_r_io,ioproc,
     &                  msgtag,mpi_comm_all,info)
      else
        if( mc_comp .ne. mpi_comm_null) then
!
c iotask is also a compute task.  send is replaced with direct
c  array copy
           buff_mult_piecesg(:,:,1:lats_node_r,ioproc+1)=
     1     buff_mult_pieceg(:,:,1:lats_node_r)
!                              end compute tasks portion of logic
        endif
!
c  end compute tasks portion of logic
c  receiving part of i/o task
!
!!
!!      for pes ioproc
        do proc=1,nodes_comp
          if (proc.ne.ioproc+1) then
            msgtag=proc-1
            illen=ivarg_global_a(2,proc)
!           print *,' pux target ',ubound(buff_mult_piecesg)
            call mpi_recv(buff_mult_piecesg(1,1,1,proc),
     1        illen*lonr*ngrids_gg
     1        ,mpi_r_io,proc-1,
     &                msgtag,mpi_comm_all,stat,info)
          endif
        enddo
!       buff_mult_piecesa(:,1:ngrids_sfcc,:,:)=
!    1 buff_mult_pieces(:,1:ngrids_sfcc,:,:)
      endif
      call mpi_barrier(mpi_comm_all,ierr)
!!
      return
      end
      subroutine atmgg_wrt(ioproc,cfile,xhour,idate
     &,                  global_lats_r,lonsperlar,pdryini)
cc
      use gfsio_module
      use resol_def
      use namelist_def
      use vert_def
      use gfsio_def
      use coordinate_def
      use sig_io
      use mod_state
      use layout1
      use mpi_def
      use physcons, rk => con_rocp
      implicit none
!!
      real (kind=kind_phys), parameter :: rk1 = rk + 1.0, rkr = 1.0/rk
     &,                                   p0=100000.0, p0i=1.0/p0
      real (kind=kind_io4), parameter  :: zero4=0.0
      integer ioproc
      character*40 cfile, tracer
      real(kind=kind_io8) xhour, pdryini
      integer idate(4),k,ngridgg, nt
!
      integer              global_lats_r(latr),   lonsperlar(latr)
!!
!     real(kind=kind_evod) gencode,ppid,realform
      real(kind=kind_io4) yhour, pdryini4
      real(kind=kind_io4), allocatable :: vcoord4(:,:)
      real(kind=kind_io4)              :: pup(lonr*latr)
     &,                                   pdn(lonr*latr)
     &,                                   plyr(lonr*latr)
     &,                                   pupk(lonr*latr)
     &,                                   pdnk(lonr*latr)
!     real(kind=kind_io8), allocatable :: buff(:)
!     real(kind=kind_io4) yhour, pdryini4, vcoord4(levp1,3)
      integer iret, ks,iorder_l,i
!     integer iret, ks,irealf,iorder, idusr
      character * 8, allocatable :: recname(:), reclevtyp(:)
      integer,       allocatable :: reclev(:)
      logical first
      save first, recname, reclevtyp, reclev, vcoord4
      data first /.true./
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!    build upper air fields in to buff_mult
!
      ngridg=1
      do ngridgg=1,ngrids_gg
!       print *,' inside atmgg_wrt calling unsp ngridgg=',ngridgg
        call unsplit2g(ioproc,buff_multg(1,ngridgg),global_lats_r)
      enddo
!     print *,' finished ngrid loop ngrids_gg=',ngrids_gg
!    building upper air  field is done
!
      if (me.eq.ioproc) then
!
!     print *,' me=',me,' in the atmgg_wrt'
        if (first) then
          first = .false.
          allocate (recname(levs*(4+ntrac)+2),
     &              reclevtyp(levs*(4+ntrac)+2),
     &              reclev(levs*(4+ntrac)+2))
!    &              buff(lonr*lats_nodes_r))
          recname(1)   = 'hgt'
          recname(2)   = 'pres'
          reclevtyp(1) = 'sfc'
          reclevtyp(2) = 'sfc'
          reclev(1)    = 1
          reclev(2)    = 1
          do k=1,levs
            recname(k+2)        = 'dpres'
            recname(k+2+levs)   = 'ugrd'
            recname(k+2+levs*2) = 'vgrd'
            recname(k+2+levs*3) = 'tmp'
            recname(k+2+levs*4) = 'spfh'
          enddo
          do nt=2,ntrac
              write(tracer,'("tracer",i2)') nt
!     print *,' tracer=',tracer,' ntrac=',ntrac,' ntoz=',ntoz,
!    &' ntcw=',ntcw,' nt=',nt
            if (nt == ntoz) then
              do k=1,levs
                recname(k+2+levs*(3+ntoz)) = 'o3mr'
              enddo
            elseif (nt == ntcw) then
              do k=1,levs
                recname(k+2+levs*(3+ntcw)) = 'clwmr'
              enddo
            else
              write(tracer,'("tracer",i2)') nt
!     print *,' tracer=',tracer
              do k=1,levs
                recname(k+2+levs*(3+nt)) = trim(tracer)
              enddo
            endif
          enddo
          do nt=1,ntrac+4
            do k=1,levs
              reclevtyp(k+2+(nt-1)*levs) = 'layer'
              reclev(k+2+(nt-1)*levs)    = k
            enddo
          enddo
!
          idpp  = 0
          idusr = 0
          idrun = 0
!         gfile_out = gfile_in
          if (gen_coord_hybrid) then                                      ! hmhj
            idvc    = vertcoord_id
            idvm    = thermodyn_id*10 + sfcpress_id    ! 1: ln(ps) 2:ps   ! hmhj
            idsl    = 2    ! idsl=2 for middle of layer                   ! hmhj
            nvcoord = 3
            allocate (vcoord4(levp1,nvcoord))
            do k=1,levp1                                                  ! hmhj
              vcoord4(k,1) = ak5(k)*1000.                                 ! hmhj
              vcoord4(k,2) = bk5(k)                                       ! hmhj
              vcoord4(k,3) = ck5(k)*1000.                                 ! hmhj
            enddo                                                         ! hmhj
          else if (hybrid) then                                           ! hmhj
            idvc    = 2                        ! for hybrid vertical coord.
            nvcoord = 2
            allocate (vcoord4(levp1,nvcoord))
            do k=1,levp1
              vcoord4(k,1) = ak5(levp1+1-k)*1000.
              vcoord4(k,2) = bk5(levp1+1-k)
              print 190,k,vcoord4(k,1),vcoord4(k,2)
190           format('in gfsio k=',i2,'  ak5r4=',f13.6,'  bk5r4=',e13.5)
            enddo
          endif
        endif
!
        pdryini4 = pdryini
        iorder_l = 2
        irealf   = 2
        yhour    = xhour
!       idvt    = (ntoz-1) + 10 * (ntcw-1)
!       idusr    = usrid

      print *,' calling gfsio_open lonr=',lonr,' latr=',latr
     &,' idate=',idate,' yhour=',yhour
!
        call gfsio_open(gfile_out,trim(cfile),'write',iret,
     &    version=ivsupa,fhour=yhour,idate=idate,nrec=2+levs*(4+ntrac),
     &    latb=latr,lonb=lonr,levs=levs,jcap=jcap,itrun=itrun,
     &    iorder=iorder_l,irealf=irealf,igen=igen,latf=latg,lonf=lonf,
     &    latr=latr,lonr=lonr,ntrac=ntrac,icen2=icen2,iens=iens,
     &    idpp=idpp,idsl=idsl,idvc=idvc,idvm=idvm,idvt=idvt,idrun=idrun,
     &    idusr=idusr,pdryini=pdryini4,ncldt=ncldt,nvcoord=nvcoord,
     &    vcoord=vcoord4,recname=recname,reclevtyp=reclevtyp,
     &    reclev=reclev)
!
!     print *,' after calling gfsio_open iret=',iret
!     if (yhour .gt. 5.99) call mpi_quit(3333)
!
!     print *,' buff_multg=',buff_multg(lonb*latb/2,:)
!        buff(:) = buff_multg(:,1)
         call gfsio_writerecv(gfile_out,'hgt','sfc',1,buff_multg(:,1),
     &                                                iret)
!     print *,' hgt ',' iret=',iret,' hgt=',buff_multg(1000,1)
!        buff(:) = buff_multg(:,2)
         call gfsio_writerecv(gfile_out,'pres','sfc',1,buff_multg(:,2),
     &                                                iret)
!     print *,' pres ',' iret=',iret,' pres=',buff_multg(1000,2)
!
         ks = 2
         do k=1,levs
!          call gfsio_getrechead(gfile_out,ngrid,vname,vlevtyp,vlev,iret)
!          print *,jrec,vname,vlevtyp,vlev
!          buff(:) = buff_multg(:,k+ks)
           call gfsio_writerecv(gfile_out,'dpres','layer',k,
     &                          buff_multg(:,k+ks), iret)
!     print *,' dp k=',k,' iret=',iret,' dp=',buff_multg(1000,k+ks)
         enddo
!
!     write out the layer mean pressure
!
         pdn(:) = buff_multg(:,2)
         pdnk   = (pdn*p0i) ** rk
         do k=1,levs
           pup(:) = max(pdn(:)-buff_multg(:,2+k), zero4)
!     print *,' pup=',pup(1),' pdn=',pdn(1),' k=',k,' pdnk=',pdnk(1)
           if (idvc == 3 .and. mod(idvm,10) == 2) then
             plyr = 0.5 * (pup + pdn)
           else
             do i=1,lonr*latr
               pupk(i) = (pup(i)*p0i) ** rk
               plyr(i) = p0*((pdnk(i)*pdn(i)-pupk(i)*pup(i)) /
     &                   (rk1*(pdn(i)-pup(i)))) ** rkr
               pdn(i)  = pup(i)
               pdnk(i) = pupk(i)
             enddo
           endif
!     print *,' pupk=',pupk(1),' plyr=',plyr(1),' k=',k,' rkr=',rkr
           call gfsio_writerecv(gfile_out,'pres','layer',k,plyr, iret)
         enddo
!
         ks = ks + levs
         do k=1,levs
!          buff(:) = buff_multg(:,k+ks)
           call gfsio_writerecv(gfile_out,'ugrd','layer',k,
     &                          buff_multg(:,k+ks), iret)
!     print *,' u k=',k,' iret=',iret,' u=',buff_multg(1000,k+ks)
         enddo
         ks = ks + levs
         do k=1,levs
!          buff(:) = buff_multg(:,k+ks)
           call gfsio_writerecv(gfile_out,'vgrd','layer',k,
     &                          buff_multg(:,k+ks), iret)
!     print *,' v k=',k,' iret=',iret,' v=',buff_multg(1000,k+ks)
         enddo
         ks = ks + levs
         do k=1,levs
!          buff(:) = buff_multg(:,k+ks)
           call gfsio_writerecv(gfile_out,'tmp','layer',k,
     &                          buff_multg(:,k+ks), iret)
!     print *,' t k=',k,' iret=',iret,' t=',buff_multg(1000,k+ks)
         enddo
         ks = ks + levs
         do k=1,levs
!          buff(:) = buff_multg(:,k+ks)
           call gfsio_writerecv(gfile_out,'spfh','layer',k,
     &                          buff_multg(:,k+ks), iret)
!     print *,' q k=',k,' iret=',iret,' q=',buff_multg(1000,k+ks)
         enddo
         if (ntoz .gt. 0) then
           ks = 2 + levs*(ntoz+3)
           do k=1,levs
!            buff(:) = buff_multg(:,k+ks)
             call gfsio_writerecv(gfile_out,'o3mr','layer',k,
     &                          buff_multg(:,k+ks), iret)
           enddo
         endif
         if (ntcw .gt. 0) then
           ks = 2 + levs*(ntcw+3)
           do k=1,levs
!            buff(:) = buff_multg(:,k+ks)
             call gfsio_writerecv(gfile_out,'clwmr','layer',k,
     &                          buff_multg(:,k+ks), iret)
           enddo
         endif
         if (ntrac .gt. ntcw) then
           do nt=ntcw+1,ntrac
             ks = 2 + levs*(nt+3)
             write(tracer,'("tracer",i2)') nt
!            print *,' tracer=',tracer
             do k=1,levs
!              buff(:) = buff_multg(:,k+ks)
               call gfsio_writerecv(gfile_out,trim(tracer),'layer',k,
     &                          buff_multg(:,k+ks), iret)
             enddo
           enddo
         endif
      endif
!
!     print *,' return code before closing iret=',iret
      call gfsio_close(gfile_out,iret)
!     print *,' return code after closing iret=',iret
!     if (allocated(vcoord4)) deallocate(vcoord4)
!     print *,' after all atmgg writes iret=',iret
      return
      end
!
!

      subroutine uninterpreg(iord,kmsk,f,fi,global_lats_r,lonsperlar)
!!
      use resol_def
      use mod_state
      use layout1
      implicit none
!!
      integer              global_lats_r(latr)
      integer,intent(in):: iord
      integer,intent(in):: kmsk(lonr,lats_node_r)
      integer,intent(in):: lonsperlar(latr)
      real(kind=kind_io8),intent(out):: f(lonr,lats_node_r)
      real(kind=kind_io8),intent(in):: fi(lonr,lats_node_r)
!      real(kind=4) f4(lonr,lats_node_r)
      integer j,lons,lat
      integer i
!!
      do j=1,lats_node_r
         lat=global_lats_r(ipt_lats_node_r-1+j)
         lons=lonsperlar(lat)
         if(lons.ne.lonr) then
           call intlon(iord,1,1,lons,lonr,
     &                 kmsk(1,j),fi(1,j),f(1,j))
!          f4(:,j)=fi(:,j)
         else
            f(:,j)=fi(:,j)
!           f4(:,j)=fi(:,j)
         endif
      enddo
!     print *,' ngridg=',ngridg
      do j=1,lats_node_r
        do i=1,lonr
          buff_mult_pieceg(i,ngridg,j) = f (i,j)
        end do
      end do
      ngridg=ngridg+1
      end subroutine
       subroutine unsplit2g(ioproc,x,global_lats_r)
c
c***********************************************************************
c
      use resol_def
      use mod_state
      use layout1
      use mpi_def
      implicit none
!!
      real(kind=kind_io4) x(lonr,latr)
!     real(kind=kind_io4) tmp(lonr,latr+2)
      integer global_lats_r(latr),ipt_lats_node_rl
      integer lats_nodes_rl
      integer maxfld,ioproc,nproct
      integer proc,j,lat,i
      integer ifldu/0/
      save ifldu
!     character*8 cna
c
c@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
c
!!
!     write(cna,985)600+ngridg
!985   format('fort.',i3)
      x=0.
!     maxfld=50
      ifldu=ifldu+1
!!
      if (me.ne.ioproc) then
            continue
      else
!!
!!     for pes ioproc
c        if (.not.liope) then
c            continue
c        else
c          nproct=nodes-1
c        endif
           nproct=nodes_comp
!     print *,' ngridg=',ngridg,' ifldu=',ifldu,' nproct=',nproct
        do proc=1,nproct
c         if (proc.ne.ioproc+1) then
c         if (.not.liope) then
c             continue
c         else
            ipt_lats_node_rl=ivarg_global_a(1,proc)
            lats_nodes_rl=ivarg_global_a(2,proc)
c         endif
         do j=1,lats_nodes_rl
           lat=global_lats_r(ipt_lats_node_rl-1+j)
           do i=1,lonr
c              x(i,lat)=tmp(i,j)
              x(i,lat)=buff_mult_piecesg(i,ngridg,j,proc)
           enddo
         enddo
c         endif   !(proc.ne.ioproc+1)
        enddo
!!
c        call baclose(563,i)
c         print *,cna,' unsplitfclose  ',i
c        call baopenw(563,cna,i)
c         print *,cna,' unsplitf open  ',i
      endif
        ngridg=ngridg+1
!!
      return
      end

       subroutine para_nstio_w(ioproc,nst_fld,nw,cfile,
     &                         xhour,idate,global_lats_r,lonsperlar)
!!
      use resol_def
      use layout1
      use namelist_def
      use nstio_module
      use nst_var_esmfmod
      implicit none
!!
      type(nst_var_data)        :: nst_fld
!
      integer nw,ioproc
      character*(*) cfile
      real(kind=kind_io8) xhour
      integer              global_lats_r(latr)
      integer              lonsperlar(latr)
!!
!!
      real(kind=kind_ior) buff4(lonr,latr)
      real(kind=kind_io8) bfo(lonr,lats_node_r)
      integer kmsk(lonr,lats_node_r)
      integer idate(4)
!
      type(nstio_head) head
      type(nstio_dbta) data
      integer iret
      logical first
      save head, first
      data first /.true./
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
      if (me.eq.ioproc) then
        if (first) then
          head%clabnst= char(0)//char(0)//char(0)//char(0)//
     &                   char(0)//char(0)//char(0)//char(0)
          head%latb    = latr
          head%lonb    = lonr
          head%ivo     = ivsnst
          head%irealf  = 2
          head%lsea    = lsea
          call nstio_alhead(head,iret)
          head%lpl     = lonsperlar(1:latr/2)
!         if (lsea == 0) then
!           head%zsea   = (/-0.1,-2.0/)
!         else
!         endif
          first = .false.
        endif
        head%fhour   = xhour
        head%idate   = idate
!
        call nstio_aldbta(head,data,iret)
        print 99,nw,xhour,idate,iret
99      format(1h ,'in para_nstio_w nw=',i7,2x,'hour=',f8.2,3x,'idate=',
     &  4(1x,i4),' iret=',i2)
      endif
!!
      kmsk= nint(nst_fld%slmsk)
!
      call uninterpred(1,kmsk,bfo,nst_fld%slmsk,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%slmsk=buff4
!
      call uninterpred(1,kmsk,bfo,nst_fld%xt,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%xt=buff4
!
      call uninterpred(1,kmsk,bfo,nst_fld%xs,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%xs=buff4
!
      call uninterpred(1,kmsk,bfo,nst_fld%xu,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%xu=buff4
!
      call uninterpred(1,kmsk,bfo,nst_fld%xv,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%xv=buff4
!
      call uninterpred(1,kmsk,bfo,nst_fld%xz,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%xz=buff4
!
      call uninterpred(1,kmsk,bfo,nst_fld%dt_cool,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%dt_cool=buff4
!
      call uninterpred(1,kmsk,bfo,nst_fld%z_c,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%z_c=buff4
!
      call uninterpred(1,kmsk,bfo,nst_fld%c_0,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%c_0=buff4
!
      call uninterpred(1,kmsk,bfo,nst_fld%c_d,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%c_d=buff4
!
      call uninterpred(1,kmsk,bfo,nst_fld%w_0,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%w_0=buff4
!
      call uninterpred(1,kmsk,bfo,nst_fld%w_d,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%w_d=buff4
!
      call uninterpred(1,kmsk,bfo,nst_fld%d_conv,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%d_conv=buff4
!
      call uninterpred(1,kmsk,bfo,nst_fld%ifd,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%ifd=buff4
!
      call uninterpred(1,kmsk,bfo,nst_fld%tref,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%tref=buff4
!
      call uninterpred(1,kmsk,bfo,nst_fld%qrain,
     &                 global_lats_r,lonsperlar)
      call unsplit2d_r(ioproc,buff4,bfo,global_lats_r)
      if(me.eq.ioproc) data%qrain=buff4
!
      if(me.eq.ioproc) then
        call nstio_swohdc(nw,cfile,head,ngrids_nst,data,iret)
        print *,' calling nstio_swohdc with nw=',nw,' cfile=',cfile
     &,' ivo=',head%ivo,' idate=',head%idate,'iret=',iret
        call nstio_axdbta(data,iret)
        print*,' call nstio_axdbta, iret = ',iret
      endif
!
      return
      end

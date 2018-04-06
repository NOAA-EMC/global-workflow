program nst_tref_chg
! Abstract:  Replace tref to be a new one by
!            1. Read in a new Tref
!            2. Smoothing Tref
! Created by Xu Li, Mar., 2017
  use nemsio_module, only: nemsio_init,nemsio_open,nemsio_close
  use nemsio_module, only: nemsio_gfile,nemsio_getfilehead
  use nemsio_module, only: nemsio_readrec,nemsio_writerec,nemsio_readrecv,nemsio_writerecv
  use setup, only: nsmth,istyp,init_setup
  implicit none
  integer, parameter :: nrec_nst=19
  character(5) :: fnamei,fnameo,ftfanl
  character(16), dimension(nrec_nst) :: recname
  integer :: lonb,latb,nlon,nlat
  integer :: iret,n,i,j
  real, allocatable, dimension(:)   :: rwork1d
  real, allocatable, dimension(:,:) :: tf_anl,work2d,land,tref
  integer, allocatable, dimension(:,:) :: mask
  type(nemsio_gfile):: gfilei, gfileo
  namelist/config/nsmth,istyp

  fnamei='fnsti'
  fnameo='fnsto'
  ftfanl='ftfan'

  call init_setup

  read(5,config)

  write(*,*) 'nsmth,istyp : ',nsmth,istyp
!
! assign recname, reclevtyp and reclev for nsst file
!
!Initiate nemsio
 call nemsio_init(iret=iret)
!open gfilei_nst (nstf06_mem???)
 call nemsio_open(gfilei,fnamei,'read',iret=iret)
! read head of gfilei_nst
 call nemsio_getfilehead(gfilei, dimx=lonb,dimy=latb,iret=iret)

 nlon = lonb
 nlat = latb
 write(*,*) 'lonb, latb : ',lonb, latb
 allocate(rwork1d(lonb*latb))
 allocate(tref(lonb,latb),tf_anl(lonb,latb),land(lonb,latb), mask(lonb,latb), work2d(lonb,latb))
!open nemsio nstanl
 gfileo=gfilei
!      read the nrec_sfc variables from sfcgcy and then write then to sfcanl
!
! recname(1)  = 'land            '
! recname(2)  = 'xt              '
! recname(3)  = 'xs              '
! recname(4)  = 'xu              '
! recname(5)  = 'xv              '
! recname(6)  = 'xz              '
! recname(7)  = 'zm              '
! recname(8)  = 'xtts            '
! recname(9)  = 'xzts            '
! recname(10) = 'dtcool          '
! recname(11) = 'zc              '
! recname(12) = 'c0              '
! recname(13) = 'cd              '
! recname(14) = 'w0              '
! recname(15) = 'wd              '
! recname(16) = 'dconv           '
! recname(17) = 'ifd             '
! recname(18) = 'tref            '
! recname(19) = 'qrain           '


 recname(1)  = 'c0              '
 recname(2)  = 'cd              '
 recname(3)  = 'dconv           '
 recname(4)  = 'dtcool          '
 recname(5)  = 'ifd             '
 recname(6)  = 'land            '
 recname(7)  = 'qrain           '
 recname(8)  = 'tref            '
 recname(9)  = 'w0              '
 recname(10) = 'wd              '
 recname(11) = 'xs              '
 recname(12) = 'xt              '
 recname(13) = 'xtts            '
 recname(14) = 'xu              '
 recname(15) = 'xv              '
 recname(16) = 'xz              '
 recname(17) = 'xzts            '
 recname(18) = 'zc              '
 recname(19) = 'zm              '

 call nemsio_open(gfileo,fnameo,'write',iret=iret,recname=recname)
      
! xt
  call nemsio_readrecv(gfilei, 'xt',    'sfc', 1, rwork1d, iret=iret)
  if ( iret /= 0 ) write(6,*) 'readrecv for gfilei for xt', '  Status = ', iret
  call nemsio_writerecv(gfileo,'xt','sfc',1,rwork1d,iret)
  if ( iret /= 0 ) write(6,*) 'writerecv for gfileo for xt', '  Status = ', iret
! xs
  call nemsio_readrecv(gfilei, 'xs',    'sfc', 1, rwork1d, iret=iret)
  if ( iret /= 0 ) write(6,*) 'readrecv for gfilei for xs', '  Status = ', iret
  call nemsio_writerecv(gfileo,'xs','sfc',1,rwork1d,iret)
  if ( iret /= 0 ) write(6,*) 'writerecv for gfileo for xs', '  Status = ', iret
! xu
  call nemsio_readrecv(gfilei, 'xu',    'sfc', 1, rwork1d, iret=iret)
  if ( iret /= 0 ) write(6,*) 'readrecv for gfilei for xu', '  Status = ', iret
  call nemsio_writerecv(gfileo,'xu','sfc',1,rwork1d,iret)
  if ( iret /= 0 ) write(6,*) 'writerecv for gfileo for xu', '  Status = ', iret
! xv
  call nemsio_readrecv(gfilei, 'xv',    'sfc', 1, rwork1d, iret=iret)
  if ( iret /= 0 ) write(6,*) 'readrecv for gfilei for xv', '  Status = ', iret
  call nemsio_writerecv(gfileo,'xv','sfc',1,rwork1d,iret)
  if ( iret /= 0 ) write(6,*) 'writerecv for gfileo for xv', '  Status = ', iret
! xz
  call nemsio_readrecv(gfilei, 'xz',    'sfc', 1, rwork1d, iret=iret)
  if ( iret /= 0 ) write(6,*) 'readrecv for gfilei for xz', '  Status = ', iret
  call nemsio_writerecv(gfileo,'xz','sfc',1,rwork1d,iret)
  if ( iret /= 0 ) write(6,*) 'writerecv for gfileo for xz', '  Status = ', iret
! zm
  call nemsio_readrecv(gfilei, 'zm',    'sfc', 1, rwork1d, iret=iret)
  if ( iret /= 0 ) write(6,*) 'readrecv for gfilei for zm', '  Status = ', iret
  call nemsio_writerecv(gfileo,'zm','sfc',1,rwork1d,iret)
  if ( iret /= 0 ) write(6,*) 'writerecv for gfileo for zm', '  Status = ', iret
! xtts
  call nemsio_readrecv(gfilei, 'xtts',    'sfc', 1,rwork1d,iret=iret)
  if ( iret /= 0 ) write(6,*) 'readrecv for gfilei for xtts', '  Status = ', iret
  call nemsio_writerecv(gfileo,'xtts','sfc',1,rwork1d,iret)
  if ( iret /= 0 ) write(6,*) 'writerecv for gfileo for xtts', '  Status = ', iret
! xzts
  call nemsio_readrecv(gfilei, 'xzts',    'sfc', 1,rwork1d,iret=iret)
  if ( iret /= 0 ) write(6,*) 'readrecv for gfilei for xzts', '  Status = ', iret
  call nemsio_writerecv(gfileo,'xzts','sfc',1,rwork1d,iret)
  if ( iret /= 0 ) write(6,*) 'writerecv for gfileo for xzts', '  Status = ', iret
! dt_cool
  call nemsio_readrecv(gfilei, 'dtcool','sfc', 1, rwork1d, iret=iret)
  if ( iret /= 0 ) write(6,*) 'readrecv for gfilei for dtcool', '  Status = ', iret
  call nemsio_writerecv(gfileo,'dtcool','sfc',1,rwork1d,iret)
  if ( iret /= 0 ) write(6,*) 'writerecv for gfileo for dtcool', '  Status = ', iret
! z_c
  call nemsio_readrecv(gfilei, 'zc','sfc', 1, rwork1d, iret=iret)
  if ( iret /= 0 ) write(6,*) 'readrecv for gfilei for zc', '  Status = ', iret
  call nemsio_writerecv(gfileo,'zc','sfc',1,rwork1d,iret)
  if ( iret /= 0 ) write(6,*) 'writerecv for gfileo for zc', '  Status = ', iret
! c_0
  call nemsio_readrecv(gfilei, 'c0','sfc', 1, rwork1d, iret=iret)
  if ( iret /= 0 ) write(6,*) 'readrecv for gfilei for c0', '  Status = ', iret
  call nemsio_writerecv(gfileo,'c0','sfc',1,rwork1d,iret)
  if ( iret /= 0 ) write(6,*) 'writerecv for gfileo for c0', '  Status = ', iret
! c_d
  call nemsio_readrecv(gfilei, 'cd','sfc', 1, rwork1d, iret=iret)
  if ( iret /= 0 ) write(6,*) 'readrecv for gfilei for cd', '  Status = ', iret
  call nemsio_writerecv(gfileo,'cd','sfc',1,rwork1d,iret)
  if ( iret /= 0 ) write(6,*) 'writerecv for gfileo for cd', '  Status = ', iret
! w_0
  call nemsio_readrecv(gfilei, 'w0','sfc', 1, rwork1d, iret=iret)
  if ( iret /= 0 ) write(6,*) 'readrecv for gfilei for w0', '  Status = ', iret
  call nemsio_writerecv(gfileo,'w0','sfc',1,rwork1d,iret)
  if ( iret /= 0 ) write(6,*) 'writerecv for gfileo for w0', '  Status = ', iret
! w_d
  call nemsio_readrecv(gfilei, 'wd','sfc', 1, rwork1d, iret=iret)
  if ( iret /= 0 ) write(6,*) 'readrecv for gfilei for wd', '  Status = ', iret
  call nemsio_writerecv(gfileo,'wd','sfc',1,rwork1d,iret)
  if ( iret /= 0 ) write(6,*) 'writerecv for gfileo for wd', '  Status = ', iret
! tref
  call nemsio_readrecv(gfilei, 'tref',  'sfc', 1, rwork1d, iret=iret)
  if ( iret /= 0 ) write(6,*) 'readrecv for gfilei for tref', '  Status = ', iret

  tref=reshape(rwork1d,(/size(tref,1),size(tref,2)/))

! slmsk/land
  call nemsio_readrecv(gfilei,'land',  'sfc', 1, rwork1d,iret=iret)
  write(*,*) ' nemsio_readrecv, land, iret = ',iret

  if ( iret /= 0 ) then
! For some retro run periods to prepare FY17 implementation, the sfc mask name could be slmsk 
     print *,'could not read land, try to read slmsk instead as the old sfc mask name'
     call nemsio_readrecv(gfilei,'slmsk','sfc',1,rwork1d,iret=iret)
     if (iret /= 0) then
        write(6,*) 'readrecv for gfilei for slmsk', '  Status = ', iret
     endif
  endif
  land=reshape(rwork1d,(/size(land,1),size(land,2)/))

  call nemsio_writerecv(gfileo,'land','sfc',1,rwork1d,iret=iret)

  if ( iret /= 0 ) write(6,*) 'writerecv for gfileo for land', '  Status = ', iret

  if ( nsmth == 100 ) then
     call read_tfs_nc(ftfanl,tf_anl,int(land),nlon,nlat)
     write(*,*) 'nlon, nlat : ',nlon, nlat
     if ( nlon /= lonb .or. nlat /= latb ) then
        write(*,*) 'inconsistent dimensions, nlon,nlat,lonb,latb ; ',nlon,nlat,lonb,latb
        stop
     endif
     do j = 1, latb
        do i = 1, lonb
           if (  land(i,j) == 0.0 ) then
               tref(i,j) = tf_anl(i,j)
           endif
        enddo
     enddo
  else
     do n = 1, nsmth
        write(*,'(a,I4)') ' smooth times : ',n
        work2d = tref
        call smth9_msk(work2d,tref,int(land),lonb,latb,istyp)
     enddo
  endif

  rwork1d = reshape( tref,(/size(rwork1d)/) )
  call nemsio_writerecv(gfileo,'tref','sfc',1,rwork1d,iret=iret)
  if ( iret /= 0 ) write(6,*) 'writerecv for gfileo for tref', '  Status = ', iret

! d_conv
  call nemsio_readrecv(gfilei, 'dconv',  'sfc', 1, rwork1d,iret=iret)
  if ( iret /= 0 ) write(6,*) 'readrecv for gfilei for dconv', '  Status = ', iret
  call nemsio_writerecv(gfileo,'dconv','sfc',1,rwork1d,iret)
  if ( iret /= 0 ) write(6,*) 'writerecv for gfileo for dconv', '  Status = ', iret
! ifd
  call nemsio_readrecv(gfilei, 'ifd',  'sfc', 1, rwork1d, iret=iret)
  if ( iret /= 0 ) write(6,*) 'readrecv for gfilei for ifd', '  Status = ', iret
  call nemsio_writerecv(gfileo,'ifd','sfc',1,rwork1d,iret)
  if ( iret /= 0 ) write(6,*) 'writerecv for gfileo for ifd', '  Status = ', iret
! qrain
  call nemsio_readrecv(gfilei,'qrain',  'sfc', 1, rwork1d,iret=iret)
  if ( iret /= 0 ) write(6,*) 'readrecv for gfilei for qrain', '  Status = ', iret
  call nemsio_writerecv(gfileo,'qrain','sfc',1,rwork1d,iret=iret)
  if ( iret /= 0 ) write(6,*) 'writerecv for gfileo for ifd', '  Status = ', iret

  deallocate(rwork1d)
  call nemsio_close(gfilei,iret=iret)
  call nemsio_close(gfileo,iret=iret)


end program nst_tref_chg

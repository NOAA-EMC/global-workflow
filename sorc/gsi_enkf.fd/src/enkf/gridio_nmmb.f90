module gridio

use nemsio_module, only: nemsio_gfile,nemsio_open,nemsio_close,nemsio_getfilehead,&
                         nemsio_getheadvar,nemsio_realkind,nemsio_intkind,&
                         nemsio_readrecv,nemsio_init,nemsio_setheadvar,nemsio_writerecv
use params, only: nlons,nlats,nlevs,pseudo_rh,cliptracers,datapath
use kinds, only: i_kind,r_double,r_kind,r_single
use constants, only: zero,one,cp,fv,rd,grav,zero,max_varname_length
use gridinfo, only: npts,wind2mass,mass2wind
use mpisetup, only: nproc
use mpeu_util, only: getindex

! 2017-05-12 Y. Wang and X. Wang - add more state variables for radar DA,
!                                  xuguang.wang@ou.edu

implicit none
private
public :: readgriddata, writegriddata
contains

subroutine readgriddata(nanal1,nanal2,vars3d,vars2d,n3d,n2d,levels,ndim,ntimes,fileprefixes,reducedgrid,grdin,qsat)
implicit none
integer, intent(in) :: nanal1,nanal2
character(len=max_varname_length), dimension(n2d), intent(in) :: vars2d
character(len=max_varname_length), dimension(n3d), intent(in) :: vars3d
integer, intent(in) :: n2d,n3d
integer, dimension(0:n3d), intent(in) :: levels
integer, intent(in) :: ndim, ntimes
character(len=120), dimension(7), intent(in)  :: fileprefixes
logical, intent(in) :: reducedgrid
real(r_single), dimension(npts,ndim,ntimes,nanal2-nanal1+1), intent(out) :: grdin
real(r_double), dimension(npts,nlevs,ntimes,nanal2-nanal1+1), intent(out) :: qsat
real(nemsio_realkind) nems_wrk(nlons*nlats), nems_wrk2(nlons*nlats), field1(nlevs)
real(nemsio_realkind) f_ice(nlons*nlats),f_rain(nlons*nlats),clwmr(nlons*nlats), &
                      f_rimef(nlons*nlats)
real(r_single) aeta1(nlevs),aeta2(nlevs),pt,pdtop

character(len=500) :: filename
character(len=7) charnanal

real(r_single), allocatable, dimension(:,:) :: pslg
real(r_kind) clip
!real(r_single) :: ak(nlevs),bk(nlevs)
real(r_single), dimension(nlons*nlats) :: psg
type(nemsio_gfile) :: gfile
logical ice
real(r_single) f_i,f_r,f_rif,clmr,qi,qli,qr,ql
integer(i_kind) iret,k,kk,nb,ii,ne,nanal
integer :: u_ind, v_ind, t_ind, tsen_ind, q_ind, oz_ind, cw_ind, prse_ind,&
           ql_ind, qr_ind, qi_ind, qli_ind, dbz_ind, w_ind
integer :: ps_ind, sst_ind
!integer(nemsio_intkind) :: idvc

u_ind    = getindex(vars3d, 'u')   !< indices in the state var arrays
v_ind    = getindex(vars3d, 'v')   ! U and V (3D)
t_ind    = getindex(vars3d, 'tv')  ! Tv (3D)
tsen_ind = getindex(vars3d, 'tsen') ! T sensible (3D)
q_ind    = getindex(vars3d, 'q')   ! Q (3D)
oz_ind   = getindex(vars3d, 'oz')  ! Oz (3D)
cw_ind   = getindex(vars3d, 'cw')  ! CW (3D)
ql_ind  = getindex(vars3d, 'ql')  ! QL (3D)
qr_ind  = getindex(vars3d, 'qr')  ! QR (3D)
qi_ind  = getindex(vars3d, 'qi')  ! QI (3D)
qli_ind = getindex(vars3d, 'qli')  ! QLI (3D)
dbz_ind = getindex(vars3d, 'dbz')  ! dBZ (3D)
w_ind   = getindex(vars3d, 'w')  ! W (3D)
prse_ind = getindex(vars3d, 'prse')

ps_ind   = getindex(vars2d, 'ps')  ! Ps (2D)
sst_ind  = getindex(vars2d, 'sst')

ne = 0
ensmemloop    : do nanal=nanal1,nanal2
ne = ne + 1
backgroundloop: do nb=1,ntimes

if (nanal > 0) then
  write(charnanal,'(a3, i3.3)') 'mem', nanal
else
  charnanal = 'ensmean'
endif

filename = trim(adjustl(datapath))//trim(adjustl(fileprefixes(nb)))//trim(charnanal)

call nemsio_init(iret=iret)
if(iret/=0) then
   write(6,*)'NMMB gridio/readgriddata: nmmb model: problem with nemsio_init, iret=',iret
   call stop2(23)
end if
call nemsio_open(gfile,filename,'READ',iret=iret)
if (iret/=0) then
   write(6,*)'NMMB gridio/readgriddata: nmmb model: problem with nemsio_open, iret=',iret, trim(filename)
   call stop2(23)
end if


call nemsio_getheadvar(gfile,'PT',pt,iret)
pt = 0.01*pt
call nemsio_getheadvar(gfile,'PDTOP',pdtop,iret)
pdtop = 0.01*pdtop
call nemsio_getheadvar(gfile,'SGML1',field1,iret)
do k=1,nlevs
  aeta1(k)=field1(nlevs+1-k)
enddo
call nemsio_getheadvar(gfile,'SGML2',field1,iret)
do k=1,nlevs
  aeta2(k)=field1(nlevs+1-k)
  aeta1(k) = aeta1(k) + aeta2(k)
enddo
call nemsio_readrecv(gfile,'dpres','hybrid sig lev',1,nems_wrk,iret=iret)
if (iret/=0) then
   write(6,*)'gridio/readgriddata: nmmb model: problem with nemsio_readrecv(dpres), iret=',iret
   call stop2(23)
endif
psg = 0.01*nems_wrk + pt ! surface pressure, units of hPa
! pressure on model levels
allocate(pslg(nlons*nlats,nlevs))
do k=1,nlevs
   pslg(:,k) = aeta1(k)*pdtop + aeta2(k)*(psg - pdtop - pt) + pt
   if (nanal .eq. 1) print *,'nemsio, min/max pressi',k,minval(pslg(:,k)),maxval(pslg(:,k))
   if (prse_ind > 0)   grdin(:,levels(prse_ind-1)+k,nb,ne) = pslg(:,k)
enddo


! get surface pressure and pressure on model levels
!call nemsio_readrecv(gfile,'pres','sfc',1,nems_wrk,iret=iret)
!if (iret/=0) then
!   write(6,*)'NMMB gridio/readgriddata: NMMB model: problem with nemsio_readrecv(ps), iret=',iret
!   call stop2(23)
!endif
!psg = 0.01_r_kind*nems_wrk ! convert ps to millibars.
!print *, 'read pres sfc: ', minval(psg), maxval(psg)

!call nemsio_getfilehead(gfile,iret=iret,vcoord=nems_vcoord,idvc=idvc)
!if ( iret /= 0 ) then
!   write(6,*)' NMMB gridio:  ***ERROR*** problem reading header ', &
!      'vcoord, Status = ',iret
!   call stop2(99)
!endif
!print *, 'idvc: ', idvc
!if ( idvc == 0 ) then                         ! sigma coordinate, old file format.
!   ak = zero
!   bk = nems_vcoord(1:nlevs,1,1)
!elseif ( idvc == 1 ) then                     ! sigma coordinate
!   ak = zero
!   bk = nems_vcoord(1:nlevs,2,1)
!elseif ( idvc == 2 .or. idvc == 3 ) then      ! hybrid coordinate
!   ak = 0.01_r_kind*nems_vcoord(1:nlevs,1,1) ! convert to mb
!   bk = nems_vcoord(1:nlevs,2,1)
!else
!   write(6,*)'gridio:  ***ERROR*** INVALID value for idvc=',idvc
!   call stop2(85)
!endif
if (nanal .eq. 1) then
   print *,'time level ',nb
   print *,'---------------'
endif
if (ps_ind > 0) then
  grdin(:,levels(n3d)+ps_ind,nb,ne) = psg
endif

! pressure on model levels
!allocate(pslg(nlons*nlats,nlevs))
!do k=1,nlevs
!   pslg(:,k)=ak(k)+bk(k)*psg
!   if (nanal .eq. 1) print *,'nemsio, min/max pressi',k,minval(pslg(:,k)),maxval(pslg(:,k))
!enddo
! get u,v
do k=1,nlevs
   kk = nlevs+1-k ! grids ordered from top to bottom in NMMB
   call nemsio_readrecv(gfile,'ugrd','mid layer',kk,nems_wrk,iret=iret)
   call wind2mass(nems_wrk,nlons,nlats)
   if (iret/=0) then
      write(6,*)'gridio/readgriddata: nmmb model: problem with nemsio_readrecv(ugrd), iret=',iret
      call stop2(23)
   endif
   if (u_ind > 0) then
     grdin(:,levels(u_ind-1)+k,nb,ne) = nems_wrk
   endif
   call nemsio_readrecv(gfile,'vgrd','mid layer',kk,nems_wrk,iret=iret)
   call wind2mass(nems_wrk,nlons,nlats)
   if (iret/=0) then
      write(6,*)'gridio/readgriddata: nmmb model: problem with nemsio_readrecv(vgrd), iret=',iret
      call stop2(23)
   endif
   if (v_ind > 0) then
     grdin(:,levels(v_ind-1)+k,nb,ne) = nems_wrk
   endif
enddo
ice = .false. ! calculate qsat w/resp to ice?
clip = tiny(grdin(1,1,1,1))
! get sensible temperature and humidity
do k=1,nlevs
   kk = nlevs+1-k ! grids ordered from top to bottom in NMMB
   call nemsio_readrecv(gfile,'tmp','mid layer',kk,nems_wrk,iret=iret)
   if (iret/=0) then
      write(6,*)'gridio/readgriddata: nmmb model: problem with nemsio_readrecv(tmp), iret=',iret
      call stop2(23)
   endif
   call nemsio_readrecv(gfile,'spfh','mid layer',kk,nems_wrk2,iret=iret)
   if (iret/=0) then
      write(6,*)'gridio/readgriddata: nmmb model: problem with nemsio_readrecv(spfh), iret=',iret
      call stop2(23)
   endif
   if (cliptracers)  where (nems_wrk2 < clip) nems_wrk2 = clip
   if (tsen_ind > 0) then
     grdin(:,levels(tsen_ind-1)+k,nb,ne) = nems_wrk
   endif
   if (t_ind > 0) then
     grdin(:,levels(t_ind-1)+k,nb,ne) = nems_wrk*(1. + fv*nems_wrk2)
   endif
   if (q_ind > 0) then
     grdin(:,levels(q_ind-1)+k,nb,ne) = nems_wrk2
   endif
enddo
! compute qsat
if (pseudo_rh) then
   call genqsat1(grdin(:,levels(q_ind-1)+1:levels(q_ind-1)+nlevs,nb,ne),qsat(:,:,nb,ne),pslg,&
                 grdin(:,levels(t_ind-1)+1:levels(t_ind-1)+nlevs,nb,ne),ice,npts,nlevs)
else
   qsat(:,:,nb,ne) = 1._r_double
end if
! other tracers
if (oz_ind > 0) then 
    do k=1,nlevs
       kk = nlevs+1-k ! grids ordered from top to bottom in NMMB
       call nemsio_readrecv(gfile,'o3mr','mid layer',kk,nems_wrk,iret=iret)
       if (iret/=0) then
          write(6,*)'gridio/readgriddata: nmmb model: problem with nemsio_readrecv(o3mr), iret=',iret
          call stop2(23)
       endif
       if (cliptracers)  where (nems_wrk < clip) nems_wrk = clip
       grdin(:,levels(oz_ind-1)+k,nb,ne) = nems_wrk
    enddo
endif
if (cw_ind > 0) then
    do k=1,nlevs
       kk = nlevs+1-k ! grids ordered from top to bottom in NMMB
       call nemsio_readrecv(gfile,'clwmr','mid layer',kk,nems_wrk,iret=iret)
       if (iret/=0) then
          write(6,*)'gridio/readgriddata: nmmb model: problem with nemsio_readrecv(clwmr), iret=',iret
          call stop2(23)
       endif
       if (cliptracers)  where (nems_wrk < clip) nems_wrk = clip
       grdin(:,levels(cw_ind-1)+k,nb,ne) = nems_wrk
    end do
end if
if (ql_ind > 0 .and. qr_ind > 0 .and. qi_ind > 0 .and. qli_ind > 0 ) then
    do k=1,nlevs
       kk = nlevs+1-k ! grids ordered from top to bottom in NMMB
       call nemsio_readrecv(gfile,'clwmr','mid layer',kk,nems_wrk,iret=iret)
       if (iret/=0) then
          write(6,*)'gridio/readgriddata: nmmb model: problem with nemsio_readrecv(clwmr), iret=',iret
          call stop2(23)
       endif
       if (cliptracers)  where (nems_wrk < clip) nems_wrk = clip
       clwmr = nems_wrk

       where( clwmr < 1.e-12_r_kind )
          clwmr = 0.0_r_kind
       end where

       call nemsio_readrecv(gfile,'f_rain','mid layer',kk,nems_wrk,iret=iret)
       if (iret/=0) then
          write(6,*)'gridio/readgriddata: nmmb model: problem with nemsio_readrecv(clwmr), iret=',iret
          call stop2(23)
       endif
       if (cliptracers)  where (nems_wrk < clip) nems_wrk = clip
       f_rain = nems_wrk

       call nemsio_readrecv(gfile,'f_ice','mid layer',kk,nems_wrk,iret=iret)
       if (iret/=0) then
          write(6,*)'gridio/readgriddata: nmmb model: problem with nemsio_readrecv(clwmr), iret=',iret
          call stop2(23)
       endif
       if (cliptracers)  where (nems_wrk < clip) nems_wrk = clip
       f_ice = nems_wrk

       call nemsio_readrecv(gfile,'f_rimef','mid layer',kk,nems_wrk,iret=iret)
       if (iret/=0) then
          write(6,*)'gridio/readgriddata: nmmb model: problem with nemsio_readrecv(clwmr), iret=',iret
          call stop2(23)
       endif
       if (cliptracers)  where (nems_wrk < clip) nems_wrk = clip
       f_rimef = nems_wrk

       ! ==== convert to hydrometeors
       do ii = 1, nlons*nlats
         f_i = f_ice(ii)
         f_r = f_rain(ii)
         f_rif = f_rimef(ii)
         clmr  = clwmr(ii)
         call fraction2variablenew(f_i,f_r,f_rif,clmr,qi,qli,qr,ql)
         grdin(ii,k+(qli_ind-1)*nlevs,nb,ne) = qli
         grdin(ii,k+(qr_ind-1)*nlevs,nb,ne)  = qr
         grdin(ii,k+(ql_ind-1)*nlevs,nb,ne)  = ql
         grdin(ii,k+(qi_ind-1)*nlevs,nb,ne)  = qi
       end do

    enddo
endif

if ( dbz_ind > 0 )then
   do k=1,nlevs
       kk = nlevs+1-k ! grids ordered from top to bottom in NMMB
       call nemsio_readrecv(gfile,'refl_10cm','mid layer',kk,nems_wrk,iret=iret)
       if (iret/=0) then
          write(6,*)'gridio/readgriddata: nmmb model: problem with nemsio_readrecv(refl_10cm), iret=',iret
          call stop2(23)
       endif
       if (cliptracers)  where (nems_wrk < clip) nems_wrk = clip
       grdin(:,(dbz_ind-1)*nlevs+k,nb,ne) = nems_wrk
    enddo
endif

if ( w_ind > 0 )then
   do k=1,nlevs
       kk = nlevs+1-k ! grids ordered from top to bottom in NMMB
       call nemsio_readrecv(gfile,'dwdt','mid layer',kk,nems_wrk,iret=iret)
       if (iret/=0) then
          write(6,*)'gridio/readgriddata: nmmb model: problem with nemsio_readrecv(refl_10cm), iret=',iret
          call stop2(23)
       endif
       grdin(:,(w_ind-1)*nlevs+k,nb,ne) = nems_wrk
    enddo
endif

if (sst_ind > 0) then
   grdin(:,levels(n3d)+sst_ind, nb, ne) = zero
endif

deallocate(pslg)
call nemsio_close(gfile, iret=iret)

end do backgroundloop ! loop over backgrounds to read in
end do ensmemloop     ! loop over ens members read by this task

end subroutine readgriddata

subroutine writegriddata(nanal1,nanal2,vars3d,vars2d,n3d,n2d,levels,ndim,grdin,no_inflate_flag)
use params, only: nbackgrounds, anlfileprefixes,fgfileprefixes

implicit none

integer, intent(in) :: nanal1,nanal2
character(len=max_varname_length), dimension(n2d), intent(in) :: vars2d
character(len=max_varname_length), dimension(n3d), intent(in) :: vars3d
integer, intent(in) :: n2d,n3d,ndim
integer, dimension(0:n3d), intent(in) :: levels
real(r_single), dimension(npts,ndim,nbackgrounds,nanal2-nanal1+1), intent(inout) :: grdin
logical, intent(in) :: no_inflate_flag 
  !Not used here, but added to make writegriddata(...) consistent with gridio_gfs.f90

character(len=500):: filename

character(len=3) charnanal
integer(nemsio_intkind) iret,nfhour,jdate(7),idat(3),ihrst,nfminute,ntimestep,nfsecond
integer iadate(4),idate(4),k,kk,nb,ne,nanal
integer,dimension(8):: ida,jda
integer :: u_ind, v_ind, t_ind, q_ind, oz_ind, cw_ind, ql_ind, qr_ind, qi_ind, qli_ind, dbz_ind, w_ind
integer :: ps_ind, ii
real(r_single) f_i,f_r,f_rif,clmr,qi,qli,qr,ql
real(r_double),dimension(5):: fha
real(nemsio_realkind), dimension(nlons*nlats) :: nems_wrk,nems_wrk2
real(nemsio_realkind) f_ice(nlons*nlats),f_rain(nlons*nlats),clwmr(nlons*nlats),&
                      f_rimef(nlons*nlats)
real(r_kind) clip
type(nemsio_gfile) :: gfile


u_ind   = getindex(vars3d, 'u')   !< indices in the state var arrays
v_ind   = getindex(vars3d, 'v')   ! U and V (3D)
t_ind   = getindex(vars3d, 'tv')  ! Tv (3D)
q_ind   = getindex(vars3d, 'q')   ! Q (3D)
oz_ind  = getindex(vars3d, 'oz')  ! Oz (3D)
cw_ind  = getindex(vars3d, 'cw')  ! CW (3D)
ql_ind  = getindex(vars3d, 'ql')  ! QL (3D)
qr_ind  = getindex(vars3d, 'qr')  ! QR (3D)
qi_ind  = getindex(vars3d, 'qi')  ! QI (3D)
qli_ind = getindex(vars3d, 'qli')  ! QLI (3D)
dbz_ind = getindex(vars3d, 'dbz')  ! dBZ (3D)
w_ind   = getindex(vars3d, 'w')  ! W (3D)

ps_ind  = getindex(vars2d, 'ps')  ! Ps (2D)

clip = tiny(grdin(1,1,1,1))

ne = 0
ensmemloop: do nanal=nanal1,nanal2
ne = ne + 1
! First guess file should be copied to analysis file at scripting
! level; only variables updated by EnKF are changed
backgroundloop: do nb=1,nbackgrounds

write(charnanal,'(i3.3)') nanal
filename = trim(adjustl(datapath))//trim(adjustl(anlfileprefixes(nb)))//"mem"//charnanal

call nemsio_init(iret=iret)
if(iret/=0) then
   write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_init, iret=',iret
   call stop2(23)
end if
call nemsio_open(gfile,filename,'RDWR',iret=iret)
if (iret/=0) then
   write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_open, iret=',iret
   call stop2(23)
end if

call nemsio_getheadvar(gfile,'idate',idate,iret)
if (iret/=0) then
   write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_getheadvar, iret=',iret
   call stop2(23)
end if
call nemsio_getheadvar(gfile,'nfhour',nfhour,iret)
if (iret/=0) then
   write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_getheadvar, iret=',iret
   call stop2(23)
end if
! update header information
! Compute analysis time from guess date and forecast length.
fha=zero; ida=0; jda=0
fha(2)=nfhour    ! relative time interval in hours
ida(1)=idate(4) ! year
ida(2)=idate(2) ! month
ida(3)=idate(3) ! day
ida(4)=0                ! time zone
ida(5)=idate(1) ! hour
ida(6)=idate(5) ! minute
call w3movdat(fha,ida,jda)
!     JDAT       INTEGER (8) NCEP ABSOLUTE DATE AND TIME
!                (YEAR, MONTH, DAY, TIME ZONE,
!                 HOUR, MINUTE, SECOND, MILLISECOND)
iadate(1)=jda(5) ! hour
iadate(2)=jda(2) ! mon
iadate(3)=jda(3) ! day
iadate(4)=jda(1) ! year
iadate(5)=jda(6) ! minute
if (nproc .eq. 0) then
  print *,'nfhour = ',nfhour
  print *,'idate = ',idate
  print *,'iadate = ',iadate
end if
idat = 0
jdate = 0
jdate(1)=jda(1)    !  new year
jdate(2)=jda(2)    !  new month
jdate(3)=jda(3)    !  new day
jdate(4)=jda(5)    !  new hour
jdate(5)=jda(6)    !  new minute
idat(3)=jdate(1)       !  forecast starting year
idat(2)=jdate(2)       !  forecast starting month
idat(1)=jdate(3)       !  forecast starting day  
ihrst=jdate(4)         !  forecast starting hour (0-23)
call nemsio_setheadvar(gfile,'idate',jdate,iret)
if (iret/=0) then
   write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_setheadvar(jdate), iret=',iret
   call stop2(23)
end if
call nemsio_setheadvar(gfile,'idat',idat,iret)
if (iret/=0) then
   write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_setheadvar(idat), iret=',iret
   call stop2(23)
end if
nfhour = 0; nfminute = 0; nfsecond = 0; ntimestep = 0
call nemsio_setheadvar(gfile,'nfhour',nfhour,iret)
if (iret/=0) then
   write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_setheadvar(nfhour), iret=',iret
   call stop2(23)
end if
call nemsio_setheadvar(gfile,'nfminute',nfminute,iret)
if (iret/=0) then
   write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_setheadvar(nfminute), iret=',iret
   call stop2(23)
end if
call nemsio_setheadvar(gfile,'nfsecondn',nfsecond,iret)
if (iret/=0) then
   write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_setheadvar(nfsecondn), iret=',iret
   call stop2(23)
end if
call nemsio_setheadvar(gfile,'ihrst',ihrst,iret)
if (iret/=0) then
   write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_setheadvar(ihrst), iret=',iret
   call stop2(23)
end if
call nemsio_setheadvar(gfile,'ntimestep',ntimestep,iret)
if (iret/=0) then
   !write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_setheadvar(ntimestep), iret=',iret
   !call stop2(23)
   write(6,*)'warning - gridio/writegriddata: nmmb model: problem with nemsio_setheadvar(ntimestep), iret=',iret
end if


! update u,v
do k=1,nlevs
   kk = nlevs+1-k ! grids ordered from top to bottom in NMMB
   call nemsio_readrecv(gfile,'ugrd','mid layer',kk,nems_wrk,iret=iret)
   if (iret/=0) then
      write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_readrecv(ugrd), iret=',iret
      call stop2(23)
   endif
   if (u_ind > 0) then
     nems_wrk2 = grdin(:,levels(u_ind-1) + k,nb,ne)
     call mass2wind(nems_wrk2,nlons,nlats)
     nems_wrk = nems_wrk + nems_wrk2
   endif
   call nemsio_writerecv(gfile,'ugrd','mid layer',kk,nems_wrk,iret=iret)
   if (iret/=0) then
      write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_writerecv(ugrd), iret=',iret
      call stop2(23)
   endif

   call nemsio_readrecv(gfile,'vgrd','mid layer',kk,nems_wrk,iret=iret)
   if (iret/=0) then
      write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_readrecv(vgrd), iret=',iret
      call stop2(23)
   endif
   if (v_ind > 0) then
     nems_wrk2 = grdin(:,levels(v_ind-1) + k,nb,ne)
     call mass2wind(nems_wrk2,nlons,nlats)
     nems_wrk = nems_wrk + nems_wrk2
   endif
   call nemsio_writerecv(gfile,'vgrd','mid layer',kk,nems_wrk,iret=iret)
   if (iret/=0) then
      write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_writerecv(vgrd), iret=',iret
      call stop2(23)
   endif
enddo

clip = tiny(grdin(1,1,1,1))
! update sensible temperature and humidity
do k=1,nlevs
   kk = nlevs+1-k ! grids ordered from top to bottom in NMMB
   call nemsio_readrecv(gfile,'tmp','mid layer',kk,nems_wrk,iret=iret)
   if (iret/=0) then
      write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_readrecv(tmp), iret=',iret
      call stop2(23)
   endif
   call nemsio_readrecv(gfile,'spfh','mid layer',kk,nems_wrk2,iret=iret)
   if (iret/=0) then
      write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_readrecv(spfh), iret=',iret
      call stop2(23)
   endif
   nems_wrk = nems_wrk*(1. + fv*nems_wrk2) 
   if (t_ind > 0) then 
      nems_wrk = nems_wrk + grdin(:,levels(t_ind-1)+k,nb,ne)
   endif
   if (q_ind > 0) then
      nems_wrk2 = nems_wrk2 + grdin(:,levels(q_ind-1)+k,nb,ne)
   endif
   if (cliptracers)  where (nems_wrk2 < clip) nems_wrk2 = clip
   ! nems_wrk is now updated Tv, convert back to T
   nems_wrk = nems_wrk/(1. + fv*nems_wrk2)
   call nemsio_writerecv(gfile,'tmp','mid layer',kk,nems_wrk,iret=iret)
   if (iret/=0) then
      write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_writerecv(tmp), iret=',iret
      call stop2(23)
   endif
   call nemsio_writerecv(gfile,'spfh','mid layer',kk,nems_wrk2,iret=iret)
   if (iret/=0) then
      write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_writerecv(spfh), iret=',iret
      call stop2(23)
   endif
enddo
! update other tracers
if (oz_ind > 0) then
    do k=1,nlevs
       kk = nlevs+1-k ! grids ordered from top to bottom in NMMB
       call nemsio_readrecv(gfile,'o3mr','mid layer',kk,nems_wrk,iret=iret)
       if (iret/=0) then
          write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_readrecv(o3mr), iret=',iret
          call stop2(23)
       endif
       nems_wrk = nems_wrk + grdin(:,levels(oz_ind-1)+k,nb,ne)
       if (cliptracers)  where (nems_wrk < clip) nems_wrk = clip
       call nemsio_writerecv(gfile,'o3mr','mid layer',kk,nems_wrk,iret=iret)
       if (iret/=0) then
          write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_writerecv(o3mr), iret=',iret
         call stop2(23)
       endif
    enddo
endif
if (cw_ind > 0) then
    do k=1,nlevs
       kk = nlevs+1-k ! grids ordered from top to bottom in NMMB
       call nemsio_readrecv(gfile,'clwmr','mid layer',kk,nems_wrk,iret=iret)
       if (iret/=0) then
          write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_readrecv(o3mr), iret=',iret
          call stop2(23)
       endif
       nems_wrk = nems_wrk + grdin(:,levels(cw_ind-1)+k,nb,ne)
       if (cliptracers)  where (nems_wrk < clip) nems_wrk = clip
       call nemsio_writerecv(gfile,'clwmr','mid layer',kk,nems_wrk,iret=iret)
       if (iret/=0) then
          write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_writerecv(o3mr), iret=',iret
         call stop2(23)
       endif
    enddo
end if
if( ql_ind > 0 .and. qr_ind > 0 .and. qi_ind > 0 .and. qli_ind > 0 ) then
    do k=1,nlevs
       kk = nlevs+1-k ! grids ordered from top to bottom in NMMB
       call nemsio_readrecv(gfile,'clwmr','mid layer',kk,nems_wrk,iret=iret)
       if (iret/=0) then
          write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_readrecv(clwmr), iret=',iret
          call stop2(23)
       endif
       if (cliptracers)  where (nems_wrk < clip) nems_wrk = clip

       clwmr = nems_wrk

       where( clwmr < 1.e-12_r_kind )
          clwmr = 0.0_r_kind
       end where

       call nemsio_readrecv(gfile,'f_rain','mid layer',kk,nems_wrk,iret=iret)
       if (iret/=0) then
          write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_readrecv(f_rain), iret=',iret
          call stop2(23)
       endif
       if (cliptracers)  where (nems_wrk < clip) nems_wrk = clip

       f_rain = nems_wrk

       call nemsio_readrecv(gfile,'f_ice','mid layer',kk,nems_wrk,iret=iret)
       if (iret/=0) then
          write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_readrecv(f_ice), iret=',iret
          call stop2(23)
       endif
       if (cliptracers)  where (nems_wrk < clip) nems_wrk = clip

       f_ice = nems_wrk

       call nemsio_readrecv(gfile,'f_rimef','mid layer',kk,nems_wrk,iret=iret)
       if (iret/=0) then
          write(6,*)'gridio/readgriddata: nmmb model: problem with nemsio_readrecv(clwmr), iret=',iret
          call stop2(23)
       endif
       if (cliptracers)  where (nems_wrk < clip) nems_wrk = clip
       f_rimef = nems_wrk

       ! ==== convert to hydrometeors
       do ii = 1, nlons*nlats
         f_i = f_ice(ii)
         f_r = f_rain(ii)
         f_rif = f_rimef(ii)
         clmr  = clwmr(ii)
         call fraction2variablenew(f_i,f_r,f_rif,clmr,qi,qli,qr,ql)
         grdin(ii,k+(qli_ind-1)*nlevs,nb,ne) = qli + grdin(ii,k+(qli_ind-1)*nlevs,nb,ne)
         grdin(ii,k+(qr_ind-1)*nlevs,nb,ne)  = qr  + grdin(ii,k+(qr_ind-1)*nlevs,nb,ne)
         grdin(ii,k+(ql_ind-1)*nlevs,nb,ne)  = ql  + grdin(ii,k+(ql_ind-1)*nlevs,nb,ne)
         grdin(ii,k+(qi_ind-1)*nlevs,nb,ne)  = qi  + grdin(ii,k+(qi_ind-1)*nlevs,nb,ne)

         qli = grdin(ii,k+(qli_ind-1)*nlevs,nb,ne)
         qr  = grdin(ii,k+(qr_ind-1)*nlevs,nb,ne)
         ql  = grdin(ii,k+(ql_ind-1)*nlevs,nb,ne)
         qi  = grdin(ii,k+(qi_ind-1)*nlevs,nb,ne)
         call variable2fractionnew(qli, qi, qr, ql, f_i, f_r,f_rif)
         f_ice(ii)  = f_i
         f_rain(ii) = f_r
         f_rimef(ii)=f_rif
       end do

       clwmr = grdin(:,k+(ql_ind-1)*nlevs,nb,ne) + grdin(:,k+(qr_ind-1)*nlevs,nb,ne) + &
               grdin(:,k+(qli_ind-1)*nlevs,nb,ne)
       nems_wrk =  clwmr

       if (cliptracers)  where (nems_wrk < clip) nems_wrk = clip
       call nemsio_writerecv(gfile,'clwmr','mid layer',kk,nems_wrk,iret=iret)
       if (iret/=0) then
          write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_writerecv(clwmr), iret=',iret
          call stop2(23)
       endif

       nems_wrk = f_ice
       if (cliptracers)  where (nems_wrk < clip) nems_wrk = clip
       call nemsio_writerecv(gfile,'f_ice','mid layer',kk,nems_wrk,iret=iret)
       if (iret/=0) then
          write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_writerecv(f_rain), iret=',iret
          call stop2(23)
       endif

       nems_wrk = f_rain
       if (cliptracers)  where (nems_wrk < clip) nems_wrk = clip
       call nemsio_writerecv(gfile,'f_rain','mid layer',kk,nems_wrk,iret=iret)
       if (iret/=0) then
          write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_writerecv(f_ice), iret=',iret
          call stop2(23)
       endif

       nems_wrk = f_rimef
       if (cliptracers)  where (nems_wrk < clip) nems_wrk = clip
       call nemsio_writerecv(gfile,'f_rimef','mid layer',kk,nems_wrk,iret=iret)
       if (iret/=0) then
          write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_writerecv(f_rimef), iret=',iret
          call stop2(23)
       endif

    enddo
endif

if (dbz_ind > 0) then
    do k=1,nlevs
       kk = nlevs+1-k ! grids ordered from top to bottom in NMMB
       call nemsio_readrecv(gfile,'refl_10cm','mid layer',kk,nems_wrk,iret=iret)
       if (iret/=0) then
          write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_readrecv(clwmr), iret=',iret
          call stop2(23)
       endif
       where (nems_wrk < 0.0 ) nems_wrk = 0.0
       nems_wrk = nems_wrk + grdin(:,(dbz_ind-1)*nlevs+k,nb,ne)
       if (cliptracers)  where (nems_wrk < clip) nems_wrk = clip
       call nemsio_writerecv(gfile,'refl_10cm','mid layer',kk,nems_wrk,iret=iret)
       if (iret/=0) then
          write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_writerecv(clwmr), iret=',iret
         call stop2(23)
       endif
    enddo
endif

if (w_ind > 0) then
    do k=1,nlevs
       kk = nlevs+1-k ! grids ordered from top to bottom in NMMB
       call nemsio_readrecv(gfile,'dwdt','mid layer',kk,nems_wrk,iret=iret)
       if (iret/=0) then
          write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_readrecv(clwmr), iret=',iret
          call stop2(23)
       endif
       nems_wrk = nems_wrk + grdin(:,(w_ind-1)*nlevs+k,nb,ne)
       call nemsio_writerecv(gfile,'dwdt','mid layer',kk,nems_wrk,iret=iret)
       if (iret/=0) then
          write(6,*)'gridio/writegriddata: nmmb model: problem with nemsio_writerecv(clwmr), iret=',iret
         call stop2(23)
       endif
    enddo
endif

call nemsio_close(gfile, iret=iret)

end do backgroundloop ! loop over backgrounds to read in
end do ensmemloop     ! loop over ens members to write on this task

end subroutine writegriddata

  Subroutine fraction2variablenew(f_ice,f_rain,f_rimef, wc, qi,qs,qr,qw)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  gsdcloudanalysis      driver for generalized cloud/hydrometeor
! analysis
!
!   PRGMMR: Ting Lei          ORG: EMC/NCEP        DATE: 2016
!
! ABSTRACT:
!  This subroutine convert fraction to qi, qs, qr, qw exactly
!  following their theorectical formula in NMMB ferrier-Algo scheme
!  and, the exact physical meaning of qi, qs, qr, qw are not considerred
!  and are only used as the intermidiate variables
!
! PROGRAM HISTORY LOG:
!   input argument list:
!     mype     - processor ID that does this IO
!
!   output argument list:
!
! USAGE:
!   INTPUT:
!     wc: summation of qi, qr and qw
!     f_ice     -  fraction of condensate in form of ice
!     f_rain    -  fraction of liquid water in form of rain
!     f_rimef   -  ratio of total ice growth to deposition groth
!   OUTPUT
!     qi    -
!     qs    -
!     qr    -
!     qw    -
!clt CW=QC+QR+QS
!    QS=F_ICE*CW
!   QR=F_RAIN*(1-F_ICE)*CW
!   QC=(1-F_RAIN)*(1-F_ICE)*CW
!   QG(qi in the above)=QS*F_RIMEF
!
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  WCOSS at NOAA/ESRL - college park, DC
!
!$$$

 use kinds, only: r_kind,r_single

   real(r_single)  qi,qs, qr, qw, wc
   real(r_single) f_ice, f_rain,f_rimef
   real(r_single) onemf_ice, onemf_rain

   onemf_ice=1-f_ice
   onemf_rain=1-f_rain

   if(wc > 0.0_r_single) then

     if(f_ice>1.0_r_single) f_ice=1.0_r_single
     if(f_ice<0.0_r_single) f_ice=0.0_r_single
     if(f_rain>1.0_r_single) f_rain=1.0_r_single
     if(f_rain<0.0_r_single) f_rain=0.0_r_single
     qs=f_ice*wc
     qr=f_rain*onemf_ice*wc
     qw=onemf_rain*onemf_ice*wc
     qi=qs*f_rimef
else
   qi=0.0_r_single; qs=0.0_r_single; qr=0.0_r_single; qw=0.0_r_single
   end if

  end subroutine fraction2variablenew

  subroutine variable2fractionnew( qs,qi, qr, qw, f_ice, f_rain,f_rimef)
!clt modified from variable2fraction, see explanation in fration2variablenew

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  gsdcloudanalysis      driver for generalized cloud/hydrometeor
! analysis
!
!   PRGMMR: Ting Lei          ORG: EMC/NCEP        DATE: 2016
!
! ABSTRACT:
!  This subroutine qi qr qw and qs  to fraction
!  following their theorectical formula in NMMB ferrier-Algo scheme
!  and, the exact physical meaning of qi, qs, qr, qw are not considerred
!  and are only used as the intermidiate variables
!
! PROGRAM HISTORY LOG:
!
!
!   input argument list:
!     mype     - processor ID that does this IO
!
!   output argument list:
!
! USAGE:
!   INPUT
!     qi    -
!     qi    -
!     qr    -
!     qw    -
!   OUTPUT:
!     f_ice     -  fraction of condensate in form of ice
!     f_rain    -  fraction of liquid water in form of rain
!     f_rimef   -  ratio of total ice growth to deposition groth
!
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  WCOSS at NOAA/ESRL - college park, DC
!
!$$$
 use kinds, only: r_kind,r_single

   real(r_single)  qi, qr, qw, wc, dum
   real(r_single)  qs
   real(r_single) f_ice, f_rain,f_rimef
   real(r_single),parameter:: epsq=1.e-12_r_single
   real(r_single) onemf_ice

   wc=qs+qr+qw
   if(wc > 0.0_r_single) then
     if(qs<epsq)then
           f_ice=0.0_r_single
     else
           dum=qs/wc
           if(dum<1.0_r_single) then
             f_ice=dum
           else
             f_ice=1.0_r_single
           end if
     end if
     onemf_ice=1-f_ice
     if(qr < epsq) then
           f_rain=0.0_r_single
     else
            dum=qr/(onemf_ice*wc)
           if(dum<1.0_r_single) then
             f_rain=dum
           else
             f_rain=1.0_r_single
            endif
     end if
     if(qi< epsq) then
           f_rimef=1.0_r_single
      else
           if(qs>epsq) then
           f_rimef=min(qi/qs,50.0)
           else
           f_rimef=1.0_r_single !cltthinkdeb
          endif

    endif   

   else
           f_rain=0.0_r_single
           f_ice=0.0_r_single
           f_rimef=1.0_r_single
   end if

  end subroutine variable2fractionnew

end module gridio

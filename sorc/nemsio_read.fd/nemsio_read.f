!- - - - -- - -- - -- - -- - - -- - --  -- - -- - -- - - -- - - - -- - --
! the program reads and list the contents in a nemsio file
! - - - - -- - -- - -- - -- - - -- - --  -- - -- - -- - - -- - - - -- -
!  Revision history
!   Sep 2008:  Jun Wang Initial code
! --
  program main
!
  use nemsio_module
  implicit none
!
  integer, parameter:: double=selected_real_kind(p=13,r=200)
  type(nemsio_gfile) :: gfile,gfilem2,gfilem3,gfiled2
!
  real (kind=8) timef
  character(255) cin
  character(8) gdatatype,modelname
  character(2) level
  real,allocatable  :: tmp(:)
!---------------------------------------------------------------------------
!--- nemsio meta data
  real  isecond,stime,etime,dummy
  integer nrec,im,jm,lm,l,idate(7),version, im2,jm2, nframe, &
          ntrac,irealf,nrec1,version1,nmeta1,nfhour,nfminute,nfsecond, &
          nfsecondn,nfsecondd,nmeta,tlmeta
  integer nsoil,jcap,ncld,idsl,idvc,idvm,idrt,rlon_min,rlon_max, &
          rlat_min,rlat_max
  integer nmetavari,nmetavarr,nmetavarl,nmetavarc,nmetavarr8,    &
          nmetaaryi,nmetaaryr,nmetaaryl,nmetaaryc,nmetaaryr8
  integer ihrst,idat(3),mp_physics,sf_surface_physics,icycle,fieldsize
  logical global, run,extrameta
  character(16),allocatable :: recname(:),reclevtyp(:)
  integer,allocatable       :: reclev(:)
  real,allocatable          :: cpi(:),ri(:)
!---------------------------------------------------------------------------
!--- local vars
  character(16) vname
  character(32) gtype
  character(16) vlevtyp
  integer i,ii,j,jj,jrec,krec,vlev,iret,lev,ista,iend,jsta,jend
!---------------------------------------------------------------------------
!
  character(16),allocatable :: variname(:),varrname(:),varlname(:),varcname(:),varr8name(:), &
                               aryiname(:),aryrname(:),arylname(:),arycname(:),aryr8name(:)
  integer,allocatable :: varival(:),aryilen(:),aryrlen(:),aryllen(:),aryclen(:),aryr8len(:)
  integer,allocatable :: aryival(:,:)
  real,allocatable :: varrval(:),aryrval(:,:)
  real(8),allocatable :: varr8val(:),aryr8val(:,:)
  logical,allocatable :: varlval(:),arylval(:,:)
  character(16),allocatable :: varcval(:),arycval(:,:)
!
!---------------------------------------------------------------------------
!
!-------------set up nemsio write--------------------------
  call nemsio_init(iret=iret)
  print *,'nemsio_init, iret=',iret
!
!+++++++++++++++++ read nemsil file with 2 meta data
!+++++++++++++++++++++++++++
!
!--- open gfile for reading
  print *,'3b:: start reading nemsio file '
!  cin='nemsio_2meta_big'
  call getarg(1,cin)
  call nemsio_open(gfile,trim(cin),'read',iret=iret)
  if(iret/=0) print *,'3b:: after open read, ',trim(cin), ' iret=',iret
!
!--- get dimension
  im=0;jm=0;lm=0;nframe=0;nrec=0
  call nemsio_getfilehead(gfile,dimx=im,dimy=jm,dimz=lm,nframe=nframe,nrec=nrec,&
       gdatatype=gdatatype,modelname=modelname,nmeta=nmeta,ntrac=ntrac,tlmeta=tlmeta,iret=iret)
  print *,'3b:: gfilem2,im=',im,'jm=',jm,'lm=',lm,'nframe=',nframe,'nrec=',nrec, &
       'gdatatype=',gdatatype,' modelname=',modelname,' nmeta=',nmeta,'ntrac=',ntrac, &
       'tlmeta=',tlmeta,'iret=',iret
!--- meta data info
  call nemsio_getfilehead(gfile,nfhour=nfhour,nfminute=nfminute,nsoil=nsoil,ncldt=ncld,&
       idsl=idsl,idvc=idvc,idvm=idvm,idrt=idrt,iret=iret)
  print *,'3b:: gfilem2,nfhour=',nfhour,'jcap=',jcap,'ncld=',ncld,'idvc=',idvc,'idrt=',idrt
!
! call nemsio_getheadvar(gfile,'nfhour',nfhour,iret=iret)
! print *,'nfhour=',nfhour
! call nemsio_getheadvar(gfile,'latf', latf,iret=iret)
! print *,'latf=',latf
!
  call nemsio_getfilehead(gfile,nmetavari=nmetavari,nmetavarr=nmetavarr,nmetavarl=nmetavarl, &
       nmetavarc=nmetavarc,nmetavarr8=nmetavarr8,nmetaaryi=nmetaaryi,nmetaaryr=nmetaaryr,    &
       nmetaaryr8=nmetaaryr8,nmetaaryl=nmetaaryl, nmetaaryc=nmetaaryc)
  print *,'nmetavari=',nmetavari,'nmetavarr=',nmetavarr,'nmetavarl=',nmetavarl,            &
          'nmetavarc=',nmetavarc,'nmetavarr8=',nmetavarr8
  print *,'nmetaaryi=',nmetaaryi,'nmetaaryr=',nmetaaryr,'nmetaaryl=',nmetaaryl,            &
          'nmetaaryc=',nmetaaryc,'nmetaaryr8=',nmetaaryr8
  if(nmetavari>0) then
    allocate(variname(nmetavari),varival(nmetavari))
    call nemsio_getfilehead(gfile,variname=variname,varival=varival)
    print *,'variname=',variname,'varival=',varival
  endif
  if(nmetavarr>0) then
    allocate(varrname(nmetavarr),varrval(nmetavarr))
    call nemsio_getfilehead(gfile,varrname=varrname,varrval=varrval)
    print *,'varrname=',varrname,'varrval=',varrval
  endif
  if(nmetavarr8>0) then
    allocate(varr8name(nmetavarr8),varr8val(nmetavarr8))
    call nemsio_getfilehead(gfile,varr8name=varr8name,varr8val=varr8val)
    print *,'varr8name=',varr8name,'varr8val=',varr8val
  endif
  if(nmetavarl>0) then
    allocate(varlname(nmetavarl),varlval(nmetavarl))
    call nemsio_getfilehead(gfile,varlname=varlname,varlval=varlval)
    print *,'varlname=',varlname,'varlval=',varlval
  endif
  if(nmetavarc>0) then
    allocate(varcname(nmetavarc),varcval(nmetavarc))
    call nemsio_getfilehead(gfile,varcname=varcname,varcval=varcval)
    print *,'varcname=',varcname,'varcval=',varcval
  endif

  if(nmetaaryi>0) then
    allocate(aryiname(nmetaaryi),aryilen(nmetaaryi))
    call nemsio_getfilehead(gfile,aryiname=aryiname,aryilen=aryilen)
    print *,'aryiname=',aryiname,'aryilen=',aryilen
    allocate(aryival(maxval(aryilen),nmetaaryi))
    call nemsio_getfilehead(gfile,aryival=aryival)
    do i=1,nmetaaryi
      print *,'aryiname=',aryiname(i),aryilen(i),aryival(1:aryilen(i),i)
    enddo
  endif
  if(nmetaaryr>0) then
    allocate(aryrname(nmetaaryr),aryrlen(nmetaaryr))
    call nemsio_getfilehead(gfile,aryrname=aryrname,aryrlen=aryrlen)
    print *,'aryrname=',aryrname,'aryrlen=',aryrlen
    allocate(aryrval(maxval(aryrlen),nmetaaryr))
    call nemsio_getfilehead(gfile,aryrval=aryrval)
    do i=1,nmetaaryr
      print *,'aryrname=',aryrname(i),aryrlen(i),aryrval(1:aryrlen(i),i)
    enddo
  endif
  if(nmetaaryr8>0) then
    allocate(aryr8name(nmetaaryr8),aryr8len(nmetaaryr8))
    call nemsio_getfilehead(gfile,aryr8name=aryr8name,aryr8len=aryr8len)
    print *,'aryr8name=',aryr8name,'aryr8len=',aryr8len
    allocate(aryr8val(maxval(aryr8len),nmetaaryr8))
    call nemsio_getfilehead(gfile,aryr8val=aryr8val)
    do i=1,nmetaaryr8
      print *,'aryr8name=',aryr8name(i),aryr8len(i),aryr8val(1:aryr8len(i),i)
    enddo
  endif
  if(nmetaaryl>0) then
    allocate(arylname(nmetaaryl),aryllen(nmetaaryl))
    call nemsio_getfilehead(gfile,arylname=arylname,aryllen=aryllen)
    print *,'arylname=',arylname,'aryllen=',aryllen
    allocate(arylval(maxval(aryllen),nmetaaryl))
    call nemsio_getfilehead(gfile,arylval=arylval)
    do i=1,nmetaaryl
      print *,'arylname=',arylname(i),aryllen(i),arylval(1:aryllen(i),i)
    enddo
  endif
  if(nmetaaryc>0) then
    allocate(arycname(nmetaaryc),aryclen(nmetaaryc))
    call nemsio_getfilehead(gfile,arycname=arycname,aryclen=aryclen)
    print *,'arycname=',arycname,'aryclen=',aryclen
    allocate(arycval(maxval(aryclen),nmetaaryc))
    call nemsio_getfilehead(gfile,arycval=arycval)
    do i=1,nmetaaryc
      print *,'arycname=',arycname(i),aryclen(i),arycval(1:aryclen(i),i)
    enddo
  endif


!
!---read fields
!
  fieldsize=(im+2*nframe)*(jm+2*nframe)
  allocate(tmp(fieldsize))
  do jrec=1,nrec
    call nemsio_getrechead(gfile,jrec,vname,vlevtyp,vlev,iret)
    call nemsio_readrec(gfile,jrec,tmp,iret=iret)
    print *,'3b:: read,jrec=',jrec,'iret=',iret,' vname=',trim(vname), &
       ' vlevtyp=',trim(vlevtyp),' vlev=',vlev,'data=',maxval(tmp),minval(tmp)
   enddo
!
!--- close nemsio file
  call nemsio_close(gfile,iret=iret)
  if ( iret .ne.0) print *,'iret=',iret

!!---------------------------------------------------------------------------
  deallocate(tmp)
!
!---------------------------------------------------------------------------
!
  call nemsio_finalize()
! - - - - -- - -- - -- - -- - - -- - --  -- - -- - -- - - -- - - - -- -
! --
  stop

 end program


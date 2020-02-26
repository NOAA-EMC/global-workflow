!- - - - -- - -- - -- - -- - - -- - --  -- - -- - -- - - -- - - - -- - --
! the program convert a nemsio file to the opposite endian
! - - - - -- - -- - -- - -- - - -- - --  -- - -- - -- - - -- - - - -- -
! --
  program main
!
  use nemsio_module
  implicit none
!
  type(nemsio_gfile) :: gfile,gfilew
!
  character(255) cin,cout
  character(8)   gdatatype,lcnv
  logical        datareal4
  real,allocatable  :: tmp(:)
  real(8),allocatable  :: tmp8(:)
  character(16) file_endian,file_endianw
!---------------------------------------------------------------------------
!--- nemsio meta data
  integer i,nrec,fieldsize,iret
!---------------------------------------------------------------------------
!
!--- initialize
  call nemsio_init(iret=iret)
!
!--- open gfile for reading
  call getarg(1,cin)
  call getarg(2,lcnv)
  if(cin=='')  then
    print *,'Usage: nemsio_cvt $FILE_NAME '   
    stop
  endif
  if(lcnv=='')lcnv='YES'
!  print *,'cin=',trim(cin),' lcnv=',trim(lcnv)
!
  call nemsio_open(gfile,trim(cin),'read',iret=iret)
  if(iret/=0) print *,'3b:: after open read, ',trim(cin), ' iret=',iret
!   
!to set file header  gfilew for write
  call set_gfile(gfile,lcnv,cin,cout,gfilew,gdatatype,fieldsize,nrec,       &
                 file_endian,file_endianw)
!
!if just check file endian, not convert
  if(lcnv == 'NO') then
     stop
  endif
!
  if(gdatatype(1:4)=="bin4".or.gdatatype(1:4)=="grib") then
    allocate(tmp(fieldsize))
    datareal4=.true.
  else if(gdatatype(1:4)=="bin8") then
    allocate(tmp8(fieldsize))
    datareal4=.false.
  endif
!
  do i=1,nrec
    if(datareal4) then
      call nemsio_readrec(gfile,i,data=tmp,iret=iret)
      call nemsio_writerec(gfilew,i,data=tmp,iret=iret)
    else
      call nemsio_readrec(gfile,i,data=tmp8,iret=iret)
      call nemsio_writerec(gfilew,i,data=tmp8,iret=iret)
    endif
     
  enddo
!
!--- close nemsio file
   call nemsio_close(gfile,iret=iret)
  if ( iret .ne.0) print *,'open input file,iret=',iret
   call nemsio_close(gfilew,iret=iret)
  if ( iret .ne.0) print *,'open output file,iret=',iret

!!---------------------------------------------------------------------------
  if(allocated(tmp))deallocate(tmp)
  if(allocated(tmp8))deallocate(tmp8)
!
!---------------------------------------------------------------------------
!
  call nemsio_finalize()
!
  print *,'File ',trim(cin),' in ',trim(file_endian),    &
     ' is converted to ', trim(cout),' in ',trim(file_endianw)
!
! - - - - -- - -- - -- - -- - - -- - --  -- - -- - -- - - -- - - - -- -
! --
  stop

 end program

!############################################################################
!
 subroutine set_gfile(gfile,lcnv,cin,cout,gfilew,gdatatype,fieldsize, &
                      nrec,file_endian,file_endianw)
!
   use nemsio_module
   implicit none
!
   type(nemsio_gfile),intent(in)     :: gfile
   character(*),intent(in)           :: lcnv,cin
   character(*),intent(out)          :: cout
   type(nemsio_gfile),intent(inout)  :: gfilew
   character(8),intent(out)          :: gdatatype
   integer,intent(out)               :: fieldsize,nrec
   character(*),intent(out)          :: file_endian,file_endianw
!
!  local vars
   integer im,jm,lm,ntrac,nframe,nmetavari,nmetavarr,nmeta,           &
           nmetavarl,nmetavarc,nmetavarr8,nmetaaryi,nmetaaryr,        &
           nmetaaryl,nmetaaryc,nmetaaryr8,iret
   integer,allocatable :: reclev(:),varival(:),aryilen(:),aryrlen(:), &
           aryllen(:),aryclen(:),aryr8len(:),aryival(:,:)
   real,allocatable    :: lat(:),lon(:),dx(:),dy(:),cpi(:),ri(:),     &
           vcoord(:,:,:),varrval(:),aryrval(:,:)
   real(8),allocatable :: varr8val(:),aryr8val(:,:)
   logical extrameta
   logical,allocatable :: varlval(:),arylval(:,:)
   character(16),allocatable :: recname(:),reclevtyp(:),variname(:),  &
           varrname(:),varlname(:),varcname(:),varr8name(:),          &
           aryiname(:),aryrname(:),arylname(:),arycname(:),           &
           aryr8name(:),varcval(:),arycval(:,:)
!
   gfilew=gfile
!
   call nemsio_getfilehead(gfile,dimx=im,dimy=jm,dimz=lm,nrec=nrec,   &
        nmeta=nmeta,gdatatype=gdatatype,                              &
        nframe=nframe,ntrac=ntrac,file_endian=file_endian,            &
        extrameta=extrameta,nmetavari=nmetavari,nmetavarr=nmetavarr,&
        nmetavarl=nmetavarl,nmetavarc=nmetavarc,nmetavarr8=nmetavarr8,&
        nmetaaryi=nmetaaryi,nmetaaryr=nmetaaryr,nmetaaryl=nmetaaryl,  &
        nmetaaryc=nmetaaryc,nmetaaryr8=nmetaaryr8)
!   
!    print *,'in set_gfile,im=',im,'jm=',jm,'lm=',lm,'nrec=',nrec, &
!      'nmeta=',nmeta,'gdatatype=',gdatatype,'file_endian=',file_endian, &
!      'ntrac=',ntrac,'extrameta=',extrameta

!
   if(trim(file_endian)=='big_endian') then
     file_endianw='little_endian'
     gdatatype=trim(gdatatype(1:4))//"_le"
     cout=trim(cin)//"_le"
   elseif(trim(file_endian)=='little_endian') then
     file_endianw='big_endian'
     gdatatype=trim(gdatatype(1:4))//"_be"
     cout=trim(cin)//"_be"
   else
     print *,'wrong endian in input file: ',trim(file_endian)
   endif
   if(lcnv == 'NO') then
     print *,'File ',trim(cin),' is in ',trim(file_endian)
     return
   endif
!
!set array
   fieldsize=(im+2*nframe)*(jm+2*nframe)
   allocate(recname(nrec),reclevtyp(nrec),reclev(nrec))
   allocate(lon(fieldsize),lat(fieldsize),dx(fieldsize),dy(fieldsize))
   allocate(cpi(ntrac+1),ri(ntrac+1))
   allocate(vcoord(lm+1,3,2))
!
   if(nmeta>=5) then
     call nemsio_getfilehead(gfile,iret=iret, recname=recname,          &
        reclevtyp=reclevtyp, reclev=reclev)
     if(iret/=0) print *,'nemsio_cnv: cant get record infor!'
   endif
!
   if(nmeta>=6) then
     call nemsio_getfilehead(gfile,iret=iret, vcoord=vcoord)
     if(iret/=0) print *,'nemsio_cnv: cant get vcoord!'
   endif
!
   if(nmeta>=8) then
     call nemsio_getfilehead(gfile,iret=iret, lon=lon,lat=lat)
     if(iret/=0) print *,'nemsio_cnv: cant get lat/lon!'
   endif
!
   if(nmeta>=10) then
     call nemsio_getfilehead(gfile,iret=iret, dx=dx,dy=dy)
     if(iret/=0) print *,'nemsio_cnv: cant get dx/dy!'
   endif
!
   if(nmeta>=12) then
     call nemsio_getfilehead(gfile,iret=iret, cpi=cpi,ri=ri)
     if(iret/=0) print *,'nemsio_cnv: cant get cpi/ri!'
   endif
!
! get user defined header vars
   if(extrameta) then
!
     if(nmetavari>0) then
       allocate(variname(nmetavari),varival(nmetavari))
       call nemsio_getfilehead(gfile,iret=iret,variname=variname,       &
          varival=varival)
     endif
!
     if(nmetavarr>0) then
       allocate(varrname(nmetavarr),varrval(nmetavarr))
       call nemsio_getfilehead(gfile,iret=iret,varrname=varrname,       &
          varrval=varrval)
     endif
!
     if(nmetavarl>0) then
       allocate(varlname(nmetavarl),varlval(nmetavarl))
       call nemsio_getfilehead(gfile,iret=iret,varlname=varlname,       &
          varlval=varlval)
     endif
!
     if(nmetavarc>0) then
       allocate(varcname(nmetavarc),varcval(nmetavarc))
       call nemsio_getfilehead(gfile,iret=iret,varcname=varcname,       &
          varcval=varcval)
     endif
!
     if(nmetavarr8>0) then
       allocate(varr8name(nmetavarr8),varr8val(nmetavarr8))
       call nemsio_getfilehead(gfile,iret=iret,varr8name=varr8name,       &
          varr8val=varr8val)
     endif
!
     if(nmetaaryi>0) then
       allocate(aryiname(nmetaaryi),aryilen(nmetaaryi))
       call nemsio_getfilehead(gfile,iret=iret,aryiname=aryiname,       &
          aryilen=aryilen)
       allocate(aryival(maxval(aryilen),nmetaaryi))
       call nemsio_getfilehead(gfile,iret=iret,aryival=aryival)
     endif
!
     if(nmetaaryr>0) then
       allocate(aryrname(nmetaaryr),aryrlen(nmetaaryr))
       call nemsio_getfilehead(gfile,iret=iret,aryrname=aryrname,       &
          aryrlen=aryrlen)
       allocate(aryrval(maxval(aryrlen),nmetaaryr))
       call nemsio_getfilehead(gfile,iret=iret,aryrval=aryrval)
     endif
!
     if(nmetaaryl>0) then
       allocate(arylname(nmetaaryl),aryllen(nmetaaryl))
       call nemsio_getfilehead(gfile,iret=iret,arylname=arylname,       &
          aryllen=aryllen)
       allocate(arylval(maxval(aryllen),nmetaaryl))
       call nemsio_getfilehead(gfile,iret=iret,arylval=arylval)
     endif
!
     if(nmetaaryc>0) then
       allocate(arycname(nmetaaryc),aryclen(nmetaaryc))
       call nemsio_getfilehead(gfile,iret=iret,arycname=arycname,       &
          aryclen=aryclen)
       allocate(arycval(maxval(aryclen),nmetaaryc))
       call nemsio_getfilehead(gfile,iret=iret,arycval=arycval)
     endif
!
     if(nmetaaryr8>0) then
       allocate(aryr8name(nmetaaryr8),aryr8len(nmetaaryr8))
       call nemsio_getfilehead(gfile,iret=iret,aryr8name=aryr8name,       &
          aryr8len=aryr8len)
       allocate(aryr8val(maxval(aryr8len),nmetaaryr8))
       call nemsio_getfilehead(gfile,iret=iret,aryr8val=aryr8val)
     endif
!
   endif
!
   call nemsio_open(gfilew,trim(cout),'write',iret=iret,            &
          gdatatype=gdatatype,                                        &
          recname=recname,reclevtyp=reclevtyp,reclev=reclev,          &
          lat=lat,lon=lon,cpi=cpi,ri=ri,extrameta=extrameta,          &
          nmetavari=nmetavari,nmetavarr=nmetavarr,nmetavarl=nmetavarl,&
          nmetavarc=nmetavarc,nmetavarr8=nmetavarr8,                  &
          nmetaaryi=nmetaaryi,nmetaaryr=nmetaaryr,nmetaaryl=nmetaaryl,&
          nmetaaryc=nmetaaryc,nmetaaryr8=nmetaaryr8,                  &
          variname=variname,varival=varival,                          &
          varrname=varrname,varrval=varrval,                          &
          varlname=varlname,varlval=varlval,                          &
          varcname=varcname,varcval=varcval,                          &
          varr8name=varr8name,varr8val=varr8val,                        &
          aryiname=aryiname,aryival=aryival,                          &
          aryrname=aryrname,aryrval=aryrval,                          &
          arylname=arylname,arylval=arylval,                          &
          arycname=arycname,arycval=arycval,                          &
          aryr8name=aryiname,aryr8val=aryr8val)  
   if(iret.ne.0) then 
       print *,'ERROR: after open, file=',trim(cout),'fiel_endian=',  &
         trim(file_endian)
   endif
!
   if(allocated(recname)) deallocate(recname,reclevtyp,reclev)
   if(allocated(vcoord))  deallocate(vcoord)
   if(allocated(lon))     deallocate(lon,lat)
   if(allocated(dy))      deallocate(dx,dy)
   if(allocated(ri))      deallocate(cpi,ri)
   if(allocated(variname))deallocate(variname,varival)
   if(allocated(varrname))deallocate(varrname,varrval)
   if(allocated(varlname))deallocate(varlname,varlval)
   if(allocated(varcname))deallocate(varcname,varcval)
   if(allocated(varr8name))deallocate(varr8name,varr8val)
   if(allocated(aryiname))deallocate(aryiname,aryilen,aryival)
   if(allocated(aryrname))deallocate(aryrname,aryrlen,aryrval)
   if(allocated(arylname))deallocate(arylname,aryllen,arylval)
   if(allocated(arycname))deallocate(arycname,aryclen,arycval)
   if(allocated(aryr8name))deallocate(aryr8name,aryr8len,aryr8val)
!
 end subroutine set_gfile

!- - - - -- - -- - -- - -- - - -- - --  -- - -- - -- - - -- - - - -- - --
! the program provide value for a variable/field in a nemsio file
! revision history:
!  Jan 2010:  Jun Wang  Initial code
! - - - - -- - -- - -- - -- - - -- - --  -- - -- - -- - - -- - - - -- - --
!
  program main
!
  use nemsio_module
  implicit none
!
  type(nemsio_gfile) :: gfile
!
  character(255) cin,filenm
  character(16) varname
  character(16) varlevtyp
  character(3) cvarlev
  integer varlev
  real,allocatable  :: data(:)
!---------------------------------------------------------------------------
!--- nemsio meta data
  integer nrec,im,jm,nframe,nmetaaryi,nmetaaryr,nmetaaryl,nmetaaryc,        &
          nmetaaryr8,i,j,fieldsize,iret,idate(7),levs,ntrac, tlmeta
  integer  ivar
  real(4)  rvar
  real(8)  r8var
  logical  lvar
  character(10)  odate
  character(16)  cvar, file_endian
  character(35) mystr,sweep_blanks
  character(4) gdatatype
  character(16),allocatable:: recname(:)
  character(16),allocatable  :: reclevtyp(:)
  integer,allocatable:: reclev(:)
  real(4),allocatable ::vcoord(:,:,:),lat(:),lon(:),cpi(:),ri(:),dx(:),dy(:)
!---------------------------------------------------------------------------
!
  character(16),allocatable :: aryiname(:),aryrname(:),arylname(:),arycname(:),&
      aryr8name(:)
  integer,allocatable :: aryilen(:),aryrlen(:),aryllen(:),aryclen(:),aryr8len(:)
  integer,allocatable :: iary(:)
  real(4),allocatable :: rary(:)
  real(8),allocatable :: r8ary(:)
  logical,allocatable :: lary(:)
  character(16),allocatable :: cary(:)
!
!-------------set up nemsio write--------------------------
  call nemsio_init(iret=iret)
!
!---------------------------------------------------------------------------
!******Example 2:  read full history file
!---------------------------------------------------------------------------
!--- open gfile for reading
  call getarg(1,cin)
  call nemsio_open(gfile,trim(cin),'READ',iret=iret)
  if (iret .ne.0 ) then
    print *,'ERROR: can not open file ',trim(cin)
    stop
  endif
!
  call nemsio_getfilehead(gfile,iret=iret,gdatatype=gdatatype,dimx=im,dimy=jm, &
    nframe=nframe,dimz=levs,nrec=nrec,ntrac=ntrac,tlmeta=tlmeta,               &
    file_endian=file_endian,nmetaaryi=nmetaaryi,nmetaaryr=nmetaaryr,           &
    nmetaaryl=nmetaaryl,nmetaaryc=nmetaaryc)
!
   fieldsize=(im+2*nframe)*(jm+2*nframe)
!
   call getarg(2,varname)
   call getarg(3,varlevtyp)
   call getarg(4,cvarlev)
   read(cvarlev,'(I3)')varlev
!
!*** 1: test for var:
    call nemsio_getheadvar(gfile,trim(varname),ivar,iret=iret)
    if(iret/=0) then
      call nemsio_getheadvar(gfile,trim(varname),rvar,iret=iret)
      if(iret/=0) then
        call nemsio_getheadvar(gfile,trim(varname),lvar,iret=iret)
        if(iret/=0) then
          call nemsio_getheadvar(gfile,trim(varname),cvar,iret=iret)
          if(iret/=0) then
            call nemsio_getheadvar(gfile,trim(varname),r8var,iret=iret)
            if(iret==0) then
              print *,trim(varname),'=',r8var
              stop
            endif
          else
            print *,trim(varname),'=',trim(cvar)
            stop
          endif
        else
           print *,trim(varname),'=',lvar
            stop
        endif
      else
        print *,trim(varname),'=',rvar
        stop
      endif
    else
      print *,trim(varname),'=',ivar
      stop
    endif

!*** 5: test for array:
! *** integer  
!idate
   if( trim(varname)=='idate') then
     call nemsio_getfilehead(gfile,idate=idate,iret=iret)
     if(iret==0) then
       print *,'idate=',idate
       write(odate,'(I4.4,I2.2,I2.2,I2.2)')idate(1),idate(2),idate(3),idate(4)
       print *,'idate_ymdh=',odate
       stop
     endif
   endif
!vcoord
   if(equal_str_nocase(trim(varname),'vcoord')) then
     allocate(vcoord(levs+1,3,2))
     call nemsio_getfilehead(gfile,vcoord=vcoord,iret=iret)
     if(iret==0) then
       print *,'levs=',levs,'vcoord(1:levs+1,1,1)=',vcoord(:,1,1)
       print *,'levs=',levs,'vcoord(1:levs+1,2,1)=',vcoord(:,2,1)
       print *,'levs=',levs,'vcoord(1:levs+1,3,1)=',vcoord(:,3,1)
       print *,'levs=',levs,'vcoord(1:levs+1,1,2)=',vcoord(:,1,2)
       print *,'levs=',levs,'vcoord(1:levs+1,2,2)=',vcoord(:,2,2)
       print *,'levs=',levs,'vcoord(1:levs+1,3,2)=',vcoord(:,3,2)
       deallocate(vcoord)
       stop
     endif
     deallocate(vcoord)
    endif
!
!recname
   if(equal_str_nocase(trim(varname),'recname')) then
     allocate(recname(nrec))
     call nemsio_getfilehead(gfile,recname=recname,iret=iret)
     if(iret==0) then
       print *,'nrec=',nrec,'recname(1:nrec)=',recname
       deallocate(recname)
       stop
     endif
     deallocate(recname)
    endif
!
!reclevtyp
   if(equal_str_nocase(trim(varname),'reclevtyp')) then
     allocate(reclevtyp(nrec))
     call nemsio_getfilehead(gfile,reclevtyp=reclevtyp,iret=iret)
     if(iret==0) then
       print *,'nrec=',nrec,'reclevtyp(1:nrec)=',reclevtyp
       deallocate(reclevtyp)
       stop
     endif
     deallocate(reclevtyp)
    endif
!
!reclev
   if(equal_str_nocase(trim(varname),'reclev')) then
     allocate(reclev(nrec))
     call nemsio_getfilehead(gfile,reclev=reclev,iret=iret)
     if(iret==0) then
       print *,'nrec=',nrec,'reclev(1:nrec)=',reclev
       deallocate(reclev)
       stop
     endif
     deallocate(reclev)
    endif
!
!lat
   if(equal_str_nocase(trim(varname),'lat')) then
     allocate(lat((im+2*nframe)*(jm+2*nframe)))
     call nemsio_getfilehead(gfile,lat=lat,iret=iret)
     if(iret==0) then
       print *,'domainsize=',(im+2*nframe)*(jm+2*nframe),'lat(1:domainsize)=',lat
       deallocate(lat)
       stop
     endif
     deallocate(lat)
    endif
!
!lon
   if(equal_str_nocase(trim(varname),'lon')) then
     allocate(lon((im+2*nframe)*(jm+2*nframe)))
     call nemsio_getfilehead(gfile,lon=lon,iret=iret)
     if(iret==0) then
       print *,'domainsize=',(im+2*nframe)*(jm+2*nframe),'lon(1:domainsize)=',lon
       deallocate(lon)
       stop
     endif
     deallocate(lon)
    endif
!
!dx
   if(equal_str_nocase(trim(varname),'dx')) then
     allocate(dx((im+2*nframe)*(jm+2*nframe)))
     call nemsio_getfilehead(gfile,dx=dx,iret=iret)
     if(iret==0) then
       print *,'domainsize=',(im+2*nframe)*(jm+2*nframe),'dx(1:domainsize)=',dx
       deallocate(dx)
       stop
     endif
     deallocate(dx)
    endif
!
!dy
   if(equal_str_nocase(trim(varname),'dy')) then
     allocate(dy((im+2*nframe)*(jm+2*nframe)))
     call nemsio_getfilehead(gfile,dy=dy,iret=iret)
     if(iret==0) then
       print *,'domainsize=',(im+2*nframe)*(jm+2*nframe),'dy(1:domainsize)=',dy
       deallocate(dy)
       stop
     endif
     deallocate(dy)
    endif
!
!cpi
   if(equal_str_nocase(trim(varname),'cpi')) then
     allocate(cpi(ntrac+1))
     call nemsio_getfilehead(gfile,cpi=cpi,iret=iret)
     if(iret==0) then
       print *,'ntrac+1=',ntrac+1,'cpi(1:ntrac+1)=',cpi
       deallocate(cpi)
       stop
     endif
     deallocate(cpi)
    endif
!
!ri
   if(equal_str_nocase(trim(varname),'ri')) then
     allocate(ri(ntrac+1))
     call nemsio_getfilehead(gfile,ri=ri,iret=iret)
     if(iret==0) then
       print *,'ntrac+1=',ntrac+1,'ri(1:ntrac+1)=',ri
       deallocate(ri)
       stop
     endif
     deallocate(ri)
    endif
!
!tlmeta
    if(equal_str_nocase(trim(varname),'tlmeta')) then
       print *,'tlmeta=',tlmeta
       stop
    endif
!
!file_endian
    if(equal_str_nocase(trim(varname),'file_endian')) then
       print *,'file_endian=',file_endian
       stop
    endif

!int array
   if(nmetaaryi>0) then
     allocate(aryiname(nmetaaryi),aryilen(nmetaaryi))
     call nemsio_getfilehead(gfile,iret=iret,aryiname=aryiname,aryilen=aryilen)
     Do i=1,nmetaaryi
!       if(trim(varname)==aryiname(i)) then
       if(equal_str_nocase(trim(varname),trim(aryiname(i)))) then
         j=i
         call nemsio_getfilehead(gfile, aryilen=aryilen)
         allocate(iary(aryilen(j)))
         call nemsio_getheadvar(gfile,trim(varname),iary,iret=iret)
         if(iret==0) then
           print *,trim(varname),'(1:',aryilen(j),')=',iary
           if(equal_str_nocase(trim(varname),"fcstdate")) then
             write(odate,'(I4.4,I2.2,I2.2,I2.2)')iary(1),iary(2),iary(3),iary(4)
             print *,'fcstdate_ymdh=',odate
           endif
           call nemsio_close(gfile)
           call nemsio_finalize()
           stop
         endif
       endif
     enddo
   endif
!
! *** real4
   if(nmetaaryr>0) then
     allocate(aryrname(nmetaaryr),aryrlen(nmetaaryr))
     call nemsio_getfilehead(gfile,iret=iret,aryrname=aryrname,aryrlen=aryrlen)
     Do i=1,nmetaaryr
       if(equal_str_nocase(trim(varname),trim(aryrname(i)))) then
         j=i
         call nemsio_getfilehead(gfile, aryrlen=aryrlen)
         allocate(rary(aryrlen(j)))
         call nemsio_getheadvar(gfile,trim(varname),rary,iret=iret)
         if(iret==0) then
           print *,trim(varname),'(1:',aryrlen(j),')=',rary
           call nemsio_close(gfile)
           call nemsio_finalize()
           stop
         endif
       endif
     enddo
   endif
!
! *** real8
   if(nmetaaryr8>0) then
     allocate(aryr8name(nmetaaryr8),aryr8len(nmetaaryr8))
     call nemsio_getfilehead(gfile,iret=iret,aryr8name=aryr8name,aryr8len=aryr8len)
     Do i=1,nmetaaryr8
       if(equal_str_nocase(trim(varname),trim(aryr8name(i)))) then
         j=i
         call nemsio_getfilehead(gfile, aryr8len=aryr8len)
         allocate(rary(aryr8len(j)))
         call nemsio_getheadvar(gfile,trim(varname),rary,iret=iret)
         if(iret==0) then
           print *,trim(varname),'(1:',aryr8len(j),')=',rary
           call nemsio_close(gfile)
           call nemsio_finalize()
           stop
         endif
       endif
     enddo
   endif
!
   if(nmetaaryl>0) then
     allocate(arylname(nmetaaryl),aryllen(nmetaaryl))
     call nemsio_getfilehead(gfile,iret=iret,arylname=arylname,aryllen=aryllen)
     Do i=1,nmetaaryl
       if(equal_str_nocase(trim(varname),trim(arylname(i)))) then
         j=i
         allocate(lary(aryllen(j)))
         call nemsio_getheadvar(gfile,trim(varname),lary,iret=iret)
         if(iret==0) then
           print *,trim(varname),'(1:',aryllen(j),')=',lary
           call nemsio_close(gfile)
           call nemsio_finalize()
           stop
         endif
       endif
     enddo
   endif
!
   if(nmetaaryc>0) then
     allocate(arycname(nmetaaryc),aryclen(nmetaaryc))
     call nemsio_getfilehead(gfile,iret=iret,arycname=arycname,aryclen=aryclen)
     Do i=1,nmetaaryc
       if(equal_str_nocase(trim(varname),trim(arycname(i)))) then
         j=i
         allocate(cary(aryclen(j)))
         call nemsio_getheadvar(gfile,trim(varname),cary,iret=iret)
         if(iret==0) then
           print *,trim(varname),'(1:',aryclen(j),')=',cary
           call nemsio_close(gfile)
           call nemsio_finalize()
           stop
         endif
       endif
     enddo
   endif
!
!   
!*** 6: test for 2D array:
   allocate(data(fieldsize))
   call nemsio_readrecv(gfile,varname,varlevtyp,varlev,data=data,iret=iret)
   if(iret==0)  then
      print *,'fieldsize=',(im+2*nframe)*(jm+2*nframe),'i=',im+2*nframe
      do j=1,jm+2*nframe
        print *,'j=',j,trim(varname),'=',data(1+(j-1)*(im+2*nframe):j*(im+2*nframe))
      enddo
!-- output pure binary file for 1 2D array
      mystr=trim(varname)//trim(varlevtyp)//trim(cvarlev)
      filenm=sweep_blanks(mystr)
      open(991,file=trim(filenm),form='unformatted')
      write(991) ((data(i+(j-1)*(im+2*nframe)),i=1,im+2*nframe),j=1,jm+2*nframe)
      close(991)
!
      deallocate(data)
      call nemsio_close(gfile)
      call nemsio_finalize()
      stop
   endif
!
   call nemsio_close(gfile)
   call nemsio_finalize()
!
   print *,'no ',trim(varname), ' in the nemsio file!'
!   
! - - - - -- - -- - -- - -- - - -- - --  -- - -- - -- - - -- - - - -- - --
  stop

!-----------------------------------------------------------------------
!
 end program

!-----------------------------------------------------------------------
!
 character(35) function sweep_blanks(in_str)
!
   implicit none
!
   character(*), intent(in) :: in_str
   character(35) :: out_str
   character :: ch
   integer :: j

   out_str = " "
   do j=1, len_trim(in_str)
     ! get j-th char
     ch = in_str(j:j)
     if (ch .ne. " ") then
       out_str = trim(out_str) // ch
     endif
     sweep_blanks = out_str 
   end do
 end function sweep_blanks

module module_write_nemsio

  use esmf
  use nemsio_module
  use module_fv3_io_def, only : write_nemsioflip, write_fsyncflag
 
  implicit none
 
  include 'mpif.h'

  private
  logical :: first_nemsio_call
  integer :: im,jm,lm, idate(7),nmeta, nsoil,ncld, idrt, ntrac
  integer :: mype, ntasks, mpi_comm, nbdl
  logical :: hydrostatic
  real(kind=ESMF_KIND_R4) :: varr4
  integer,dimension(200,100)      :: nfldlev
  character(16),dimension(3000,5) :: recname,reclevtyp 
  integer,dimension(3000,5)       :: reclev 

  integer,dimension(:), allocatable    :: nrec
  integer,dimension(:), allocatable    :: idsl, idvc,idvm
  integer,dimension(:), allocatable    :: fieldcount
  integer, dimension(:), allocatable   :: idisp, irecv
!
  integer,dimension(:), allocatable :: nmetavari,nmetavarc, nmetavarr4,nmetavarr8
  integer,dimension(:), allocatable :: nmetaaryi,nmetaaryc, nmetaaryr4,nmetaaryr8
  character(16),dimension(:,:),allocatable :: variname, varcname, varr4name, varr8name
  character(16),dimension(:,:),allocatable :: aryiname
  integer, dimension(:,:), allocatable   :: varival, aryilen
  integer, dimension(:,:,:), allocatable :: aryival
  real(4), dimension(:,:), allocatable   :: varr4val
  real(8), dimension(:,:), allocatable   :: varr8val
  character(16), dimension(:,:), allocatable   :: varcval
  logical, dimension(:), allocatable :: extrameta
!
  real(4), dimension(:,:,:), allocatable  :: vcoord
  real(4), dimension(:), allocatable      :: lon1d, lat1d

  logical         :: first_set

  public nemsio_first_call, write_nemsio
  
  contains

  subroutine nemsio_first_call(fieldbundle, imo, jmo, &
             wrt_mype, wrt_ntasks, wrt_mpi_comm, wrt_nbdl, mybdl,   &
             inidate, lat, lon, rc)
    type(ESMF_FieldBundle), intent(in)     :: fieldbundle
    integer, intent(in)                    :: imo, jmo
    integer, intent(in)                    :: wrt_mype, wrt_ntasks, wrt_mpi_comm
    integer, intent(in)                    :: wrt_nbdl, mybdl
    integer, intent(in)                    :: inidate(7)
    real, intent(in)                       :: lat(:), lon(:)
    integer, optional,intent(out)          :: rc

!** local vars
    integer i,j, nfld, nidx, nlen
    integer fieldDimCount,gridDimCount
    character(100) :: fieldname
    type(ESMF_GRID)            :: wrtgrid
    type(ESMF_TypeKind_Flag)   :: typekind

    integer, dimension(:), allocatable :: ungriddedLBound, ungriddedUBound
    type(ESMF_Field), allocatable  :: fcstField(:)
    real(ESMF_KIND_R8), dimension(:,:), pointer   :: lonPtr, latPtr
    
!-------------------------------------------------------------------
!
    im    = imo
    jm    = jmo
    nmeta = 8
    idrt  = 4
    idate(1:7) = inidate(1:7)
    mype  = wrt_mype
    ntasks= wrt_ntasks
    nbdl  = wrt_nbdl
    mpi_comm = wrt_mpi_comm
    if(.not.allocated(idisp)) allocate(idisp(ntasks),irecv(ntasks))
    if(.not.allocated(fieldCount)) allocate(fieldCount(nbdl))
    if(.not.allocated(nrec)) allocate(nrec(nbdl))
    if(.not.allocated(idsl)) then
      allocate(idsl(nbdl),idvc(nbdl),idvm(nbdl))
      idsl=-9999; idvc=-9999; idvm=-9999; first_set=.true.
      nsoil = 4
      ntrac = 3
      ncld  = 1
    endif
!
!** get attibute info from fieldbundle
    call get_global_attr(fieldbundle, mybdl, rc=rc)

!** get meta info from fieldbundle
    call ESMF_FieldBundleGet(fieldbundle, fieldCount=fieldCount(mybdl), &
         grid=wrtGrid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
               line=__LINE__, &
               file=__FILE__)) &
               return  ! bail out
!
    call ESMF_GridGet(wrtgrid, dimCount=gridDimCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
!
!** get lat lon:
    if(.not.allocated(lon1d)) allocate(lon1d(imo*jmo))
    if(.not.allocated(lat1d)) allocate(lat1d(imo*jmo))
    do j=1,jmo
      do i=1,imo
        lon1d((j-1)*imo+i) = lon(i)
        lat1d((j-1)*imo+i) = lat(j)
      enddo
    enddo
!
    allocate(fcstField(fieldCount(mybdl)))
    call ESMF_FieldBundleGet(fieldbundle, fieldList=fcstField,     &
         itemorderflag=ESMF_ITEMORDER_ADDORDER, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    nrec(mybdl)=0
    do i=1,fieldcount(mybdl)

       call ESMF_FieldGet(fcstField(i), typekind=typekind, &
            dimCount=fieldDimCount, grid=wrtGrid, name=fieldname, rc=rc)
       if( index(trim(fieldname),"vector") >0) cycle
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out
       if (fieldDimCount > gridDimCount) then
         allocate(ungriddedLBound(fieldDimCount-gridDimCount))
         allocate(ungriddedUBound(fieldDimCount-gridDimCount))
         call ESMF_FieldGet(fcstField(i), ungriddedLBound=ungriddedLBound, &
                    ungriddedUBound=ungriddedUBound, rc=rc)
         if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out
         nfldlev(i,mybdl) = ungriddedUBound(fieldDimCount-gridDimCount) - &
                        ungriddedLBound(fieldDimCount-gridDimCount) + 1
         nrec(mybdl) = nrec(mybdl) + nfldlev(i,mybdl)
         deallocate(ungriddedLBound)
         deallocate(ungriddedUBound)
       else if(fieldDimCount == 2) then
         nfldlev(i,mybdl) = 1
         nrec(mybdl) = nrec(mybdl) + 1
       endif
       
    enddo
!
    nfld = 1
    do i=1,fieldcount(mybdl)
      call ESMF_FieldGet(fcstField(i),name=fieldName,rc=rc)
      if( nfldlev(i,mybdl) == 1) then
        reclevtyp(nfld,mybdl) = "sfc"
        reclev(nfld,mybdl)    = 1
        recname(nfld,mybdl)   = trim(fieldName)

        nlen = len(trim(fieldName))
        if( nlen > 3) then
          if( fieldName(nlen-2:nlen) == 'sfc') recname(nfld,mybdl) = fieldName(1:nlen-3)
          if( fieldName(nlen-2:nlen) == 'nst') recname(nfld,mybdl) = fieldName(1:nlen-3)
          if (fieldName(nlen-2:nlen)== '10m') then
            reclevtyp(nfld,mybdl) = "10 m above gnd"
            if( fieldName(nlen-2:nlen) == "10m") then
              recname(nfld,mybdl) = fieldName(1:nlen-3)
              if (trim(fieldName) == "f10m") recname(nfld,mybdl) = trim(fieldName)
            endif
          else if (fieldName(nlen-1:nlen)== '2m') then
            reclevtyp(nfld,mybdl) = "2 m above gnd"
            if( fieldName(nlen-1:nlen) == '2m') then
              recname(nfld,mybdl) = fieldName(1:nlen-2)
            endif
          else if( index(trim(fieldName),'soil')>0 ) then
            if( index(trim(fieldName),'soilt')>0) then
               recname(nfld,mybdl) = 'tmp'
            else if( index(trim(fieldName),'soilm')>0 .and.  &
              trim(reclevtyp(nfld,mybdl))=='sfc') then
               recname(nfld,mybdl) = 'soilm'
               reclevtyp(nfld,mybdl) = '0-200 cm down'
            else
              recname(nfld,mybdl)   = fieldName(1:nlen-1)
            endif
            if(fieldName(nlen:nlen)=='1') then
              reclevtyp(nfld,mybdl) = '0-10 cm down'
            else if (fieldName(nlen:nlen)=='2') then
              reclevtyp(nfld,mybdl) = '10-40 cm down'
            else if (fieldName(nlen:nlen)=='3') then
              reclevtyp(nfld,mybdl) = '40-100 cm down'
            else if (fieldName(nlen:nlen)=='4') then
              reclevtyp(nfld,mybdl) = '100-200 cm down'
            endif
          else if( fieldName(nlen-2:nlen) =='clm' ) then
            recname(nfld,mybdl)   = fieldName(1:nlen-3)
            reclevtyp(nfld,mybdl) = 'atmos col'
          else if( fieldName(nlen-2:nlen) =='toa' ) then
            recname(nfld,mybdl)   = fieldName(1:nlen-3)
            reclevtyp(nfld,mybdl) = 'nom. top'
          else if( fieldName(nlen-2:nlen) =='lcl' ) then
            recname(nfld,mybdl)   = fieldName(1:nlen-3)
            reclevtyp(nfld,mybdl) = 'low cld lay'
          else if( fieldName(nlen-2:nlen) =='mcl' ) then
            recname(nfld,mybdl)   = fieldName(1:nlen-3)
            reclevtyp(nfld,mybdl) = 'mid cld lay'
          else if( fieldName(nlen-2:nlen) =='hcl' ) then
            recname(nfld,mybdl)   = fieldName(1:nlen-3)
            reclevtyp(nfld,mybdl) = 'high cld lay'
          else if( fieldName(nlen-2:nlen) =='lct' ) then
            recname(nfld,mybdl)   = fieldName(1:nlen-3)
            reclevtyp(nfld,mybdl) = 'low cld top'
          else if( fieldName(nlen-2:nlen) =='mct' ) then
            recname(nfld,mybdl)   = fieldName(1:nlen-3)
            reclevtyp(nfld,mybdl) = 'mid cld top'
          else if( fieldName(nlen-2:nlen) =='hct' ) then
            recname(nfld,mybdl)   = fieldName(1:nlen-3)
            reclevtyp(nfld,mybdl) = 'high cld top'
          else if( fieldName(nlen-2:nlen) =='lcb' ) then
            recname(nfld,mybdl)   = fieldName(1:nlen-3)
            reclevtyp(nfld,mybdl) = 'low cld bot'
          else if( fieldName(nlen-2:nlen) =='mcb' ) then
            recname(nfld,mybdl)   = fieldName(1:nlen-3)
            reclevtyp(nfld,mybdl) = 'mid cld bot'
          else if( fieldName(nlen-2:nlen) =='hcb' ) then
            recname(nfld,mybdl)   = fieldName(1:nlen-3)
            reclevtyp(nfld,mybdl) = 'high cld bot'
          endif
        endif
        if( nlen > 5) then
          if( fieldName(nlen-4:nlen) =='cnvcl' ) then
            recname(nfld,mybdl)   = fieldName(1:nlen-5)
            reclevtyp(nfld,mybdl) = 'convect-cld laye'
          else if( fieldName(nlen-5:nlen) =='cnvclt' ) then
            recname(nfld,mybdl)   = fieldName(1:nlen-6)
            reclevtyp(nfld,mybdl) = 'convect-cld top'
          else if( fieldName(nlen-5:nlen) =='cnvclb' ) then
            recname(nfld,mybdl)   = fieldName(1:nlen-6)
            reclevtyp(nfld,mybdl) = 'convect-cld bot'
          else if( fieldName(nlen-4:nlen) =='bndcl' ) then
            recname(nfld,mybdl)   = fieldName(1:nlen-5)
            reclevtyp(nfld,mybdl) = 'bndary-layer cld'

          endif
        endif
        nfld = nfld + 1
      else
        lm   = nfldlev(i,mybdl)
        if(first_set) then
          idsl(mybdl) = 1
          idvc(mybdl) = 2
          idvm(mybdl) = 1
          first_set = .false.
        endif
        do j = 1,nfldlev(i,mybdl)
          recname(nfld,mybdl)   = trim(fieldName)
          if(trim(fieldName) == "preshy" .or. trim(fieldName) == "presnh") then
            recname(nfld,mybdl) = "pres"
          endif
          reclevtyp(nfld,mybdl) = "mid layer"
          reclev(nfld,mybdl)    = j
          nfld = nfld + 1
        enddo
      endif
    enddo
!
  end subroutine nemsio_first_call

!----------------------------------------------------------------------------------------
  subroutine write_nemsio(fieldbundle, filename, nf_hours, &
             nf_minutes, nf_seconds, nfsecond_num, nfsecond_den, mybdl, rc)
!
    type(ESMF_FieldBundle), intent(in)     :: fieldbundle
    character(*), intent(in)               :: filename
    integer, intent(in)                    :: nf_hours, nf_minutes, nf_seconds
    integer, intent(in)                    :: nfsecond_num, nfsecond_den
    integer, intent(in)                    :: mybdl
    integer, optional,intent(out)          :: rc
!
!** local vars
    integer i,ii,j,m,n,k, k1,k2,k3,jrec, nfseconds, nofsync, FFSYNC
    integer istart, iend, jstart, jend, kstart, kend, nlen
    real    fhour
    logical OPENED
    real(4),dimension(:),allocatable    :: tmp
    real(4),dimension(:,:),allocatable  :: arrayr4
    real(4),dimension(:,:),pointer      :: arrayr42d
    real(8),dimension(:,:),pointer      :: arrayr82d
    real(4),dimension(:,:,:),pointer    :: arrayr43d
    real(8),dimension(:,:,:),pointer    :: arrayr83d
    type(ESMF_Field), allocatable  :: fcstField(:)
    type(ESMF_TypeKind_Flag)       :: typekind
    type(nemsio_gfile) ::  nemsiofile
    character(128) :: fieldname
!

!** init nemsio
    call nemsio_init(iret=rc)
!
!**  OPEN NEMSIO FILE
!
    if(mype==0) print *,'in write_nemsio,bf nemsio_open, filename=',trim(filename),    &
      'idate=',idate,'nfour=',NF_HOURS,NF_MINUTES,NF_SECONDS, 'mybdl=',mybdl,          &
      'dim=',im,jm,lm,'nmeta=',nmeta,'idrt=',idrt,'nsoil=',nsoil,                      &
      'ntrac=',ntrac,'nrec=',nrec(mybdl),'extrameta=',extrameta(mybdl),                &
      'vcoord=',vcoord(1:5,1,1),'nfhours=',nf_hours,nf_minutes,nfseconds,nfsecond_den, &
      'idsl=',idsl(mybdl),'idvc=',idvc(mybdl),idvm(mybdl)
!      'nmetavari=',nmetavari(mybdl),'nmetavarc=',nmetavarc(mybdl)
!    if(nmetavari(mybdl)>0) print *,'in write_nemsio,bf nemsio_open,nmetavari=', &
!      nmetavari(mybdl),'varival=',trim(variname(1,mybdl)),varival(1,mybdl)
!    if(nmetavarc(mybdl)>0) print *,'in write_nemsio,bf nemsio_open,nmetavarc=', &
!      nmetavarc(mybdl),'varcval=',trim(varcname(1,mybdl)),varcval(1,mybdl)
!    if(nmetaaryi(mybdl)>0) print *,'in write_nemsio,bf nemsio_open,nmetaaryi=', &
!      nmetaaryi(mybdl),'aryival=',trim(aryiname(1,mybdl)),aryilen(1,mybdl),aryival(1:3,1,mybdl)
 
    if(mype==0) then
      nfseconds = nf_seconds*nfsecond_den + nfsecond_num
      call nemsio_open(nemsiofile,trim(FILENAME),'write',rc,             &
        modelname="FV3GFS", gdatatype="bin4",                            &
        idate=idate,nfhour=nf_hours, nfminute=nf_minutes,                &
        nfsecondn=nfseconds, nfsecondd=nfsecond_den,                     &
        dimx=im,dimy=jm,dimz=lm, nmeta=nmeta,idrt=idrt,                  &
        nsoil=nsoil,ntrac=ntrac,nrec=nrec(mybdl), ncldt=ncld,            &
        idsl=idsl(mybdl),idvc=idvc(mybdl), idvm=idvm(mybdl),             &
        vcoord=vcoord, lon=lon1d,lat=lat1d,                              &
        extrameta=extrameta(mybdl),recname=RECNAME(1:nrec(mybdl),mybdl), &
        reclevtyp=RECLEVTYP(1:nrec(mybdl),mybdl),                        &
        reclev=RECLEV(1:nrec(mybdl),mybdl),                              &
        nmetavari=nmetavari(mybdl), nmetavarr=nmetavarr4(mybdl),         &
        nmetavarc=nmetavarc(mybdl), nmetaaryi=nmetaaryi(mybdl),          &
        variname=variname(1:nmetavari(mybdl),mybdl),                     &
        varival=varival(1:nmetavari(mybdl),mybdl),                       &
        varrname=varr4name(1:nmetavarr4(mybdl),mybdl),                   &
        varrval=varr4val(1:nmetavarr4(mybdl),mybdl),                     &
        varcname=varcname(1:nmetavarc(mybdl),mybdl),                     &
        varcval=varcval(1:nmetavarc(mybdl),mybdl),                       &
        aryiname=aryiname(1:nmetaaryi(mybdl),mybdl),                     &  
        aryilen=aryilen(1:nmetaaryi(mybdl),mybdl),                       & 
        aryival=aryival(1:maxval(aryilen(1:nmetaaryi(mybdl),mybdl)),     &
          1:nmetaaryi(mybdl),mybdl) )
       if(rc/=0) print *,'nemsio_open, file=',trim(filename),' iret=',rc

    endif
!
!** collect data to first pe and write out data
!
    allocate(arrayr4(im,jm),tmp(im*jm))
    allocate(fcstField(fieldCount(mybdl)))
    call ESMF_FieldBundleGet(fieldbundle, fieldList=fcstField,     &
         itemorderflag=ESMF_ITEMORDER_ADDORDER, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) & return  ! bail out

    jrec = 1
    do i=1, fieldcount(mybdl)
!
       call ESMF_FieldGet(fcstField(i),typekind=typekind, name=fieldname, rc=rc)

       if( nfldlev(i,mybdl) == 1) then
         if( typekind == ESMF_TYPEKIND_R4) then
           call ESMF_FieldGet(fcstField(i), localDe=0, farrayPtr=arrayr42d, rc=rc)
           if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
             line=__LINE__, file=__FILE__)) return  ! bail out
           istart = lbound(arrayr42d,1)
           iend   = ubound(arrayr42d,1)
           jstart = lbound(arrayr42d,2)
           jend   = ubound(arrayr42d,2)
           nlen   = (iend-istart+1) * (jend-jstart+1)
         elseif( typekind == ESMF_TYPEKIND_R8) then
           call ESMF_FieldGet(fcstField(i), localDe=0, farrayPtr=arrayr82d, rc=rc)
           if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
             line=__LINE__, file=__FILE__)) return  ! bail out
           istart = lbound(arrayr82d,1)
           iend   = ubound(arrayr82d,1)
           jstart = lbound(arrayr82d,2)
           jend   = ubound(arrayr82d,2)
           nlen   = (iend-istart+1) * (jend-jstart+1)
           allocate( arrayr42d(istart:iend,jstart:jend))
           do n=jstart,jend
             do m=istart,iend
               arrayr42d(m,n) = arrayr82d(m,n)
             enddo
           enddo
         endif
! send data to task 0
         call mpi_gather(nlen, 1, MPI_INTEGER, irecv(:), 1, MPI_INTEGER, 0, mpi_comm, rc)
         if(mype == 0) then
           idisp(1) = 0
           do n=1,ntasks-1
             idisp(n+1) = idisp(n) + irecv(n)
           enddo
!           if(mype==0) print *,' collect data, idisp=',idisp(:)
!           if(mype==0) print *,' collect data, irecv=',irecv(:)
         endif
!         if( trim(recname(jrec,mybdl))=="HGTsfc" .and. trim(recname(jrec,mybdl))=="sfc") then
!           print *,'in write nemsio,fb=',i,' write jrec=',jrec,' val=',maxval(arrayr42d(istart:iend,jstart:jend)),  &
!             minval(arrayr42d(istart:iend,jstart:jend)),maxloc(arrayr42d(istart:iend,jstart:jend)), &
!             minloc(arrayr42d(istart:iend,jstart:jend))
!         endif
         call mpi_gatherv(arrayr42d,nlen,MPI_REAL, arrayr4,irecv,idisp(:), MPI_REAL, &
              0, mpi_comm, rc)
         if(mype==0) then
!           print *,'in write nemsio, value=',maxval(arrayr4(1:im,1:jm)), &
!              minval(arrayr4(1:im,1:jm)),maxloc(arrayr4(1:im,1:jm)),minloc(arrayr4(1:im,1:jm))
           tmp = reshape(arrayr4, (/im*jm/))
           call nemsio_writerec(nemsiofile, jrec, tmp, iret=rc)
!           print *,'in write nemsio,fb=',i,' write jrec=',jrec,'fld is',  &
!             trim(recname(jrec,mybdl)), 'rc=', &
!             rc, 'value=',maxval(tmp),minval(tmp),maxloc(tmp),minloc(tmp)
         endif
         jrec = jrec + 1

      elseif (nfldlev(i,mybdl) > 1) then

         if( typekind == ESMF_TYPEKIND_R4) then
           call ESMF_FieldGet(fcstField(i), localDe=0, farrayPtr=arrayr43d, rc=rc)
           if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
             line=__LINE__, file=__FILE__)) return  ! bail out

           istart = lbound(arrayr43d,1)
           iend   = ubound(arrayr43d,1)
           jstart = lbound(arrayr43d,2)
           jend   = ubound(arrayr43d,2)
           kstart = lbound(arrayr43d,3)
           kend   = ubound(arrayr43d,3)
           nlen   = (iend-istart+1) * (jend-jstart+1)
         elseif( typekind == ESMF_TYPEKIND_R8) then
           call ESMF_FieldGet(fcstField(i), localDe=0, farrayPtr=arrayr83d, rc=rc)
           if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
             line=__LINE__, file=__FILE__)) return  ! bail out
           istart = lbound(arrayr83d,1)
           iend   = ubound(arrayr83d,1)
           jstart = lbound(arrayr83d,2)
           jend   = ubound(arrayr83d,2)
           kstart = lbound(arrayr83d,3)
           kend   = ubound(arrayr83d,3)
           nlen   = (iend-istart+1) * (jend-jstart+1)
         endif

         ! send data to task 0
         call mpi_gather(nlen, 1, MPI_INTEGER, irecv, 1, MPI_INTEGER, 0, mpi_comm, rc)
         if(mype == 0) then
           idisp(1) = 0
           do n=1,ntasks-1
             idisp(n+1) = idisp(n) + irecv(n)
           enddo
         endif
! write out all levels
         allocate(arrayr42d(istart:iend,jstart:jend))
!         do k=kstart,kend
         if ( write_nemsioflip ) then
           k1=kend;   k2=kstart; k3=-1
         else
           k1=kstart; k2=kend;   k3=1
         endif
         do k=k1,k2,k3
           if (typekind == ESMF_TYPEKIND_R4) then
             do n=jstart,jend
               do m=istart,iend
                  arrayr42d(m,n)=arrayr43d(m,n,k)    
               enddo
             enddo
           elseif (typekind == ESMF_TYPEKIND_R8) then
             do n=jstart,jend
               do m=istart,iend
                  arrayr42d(m,n)=arrayr83d(m,n,k)    
               enddo
             enddo
           endif
!
           call mpi_gatherv(arrayr42d, nlen, MPI_REAL, arrayr4, irecv,idisp, MPI_REAL, &
              0, mpi_comm, rc)
           if(mype==0) then
             tmp = reshape(arrayr4, (/im*jm/))
             call nemsio_writerec(nemsiofile, jrec, tmp, iret=rc)
             jrec = jrec + 1
           endif
         enddo
         deallocate(arrayr42d)
!
      endif
    enddo
!
    deallocate(tmp)
    deallocate(arrayr4)
    deallocate(fcstField)
!
!** close nemsio file
    call nemsio_close(nemsiofile, iret=rc)
!
    call nemsio_finalize()
!
!** ffsync
    if( write_fsyncflag ) then
      do n=751,900
        inquire(n,opened=opened)
        if(.not.opened)then
          nofsync=n
          exit
        endif
      enddo
!
      open(unit=nofsync, file=trim(FILENAME) )
        rc=FFSYNC(nofsync)
        if (rc.ne.0) then
          print *,"Error returned from ffsync, file=", trim(FILENAME),"rc=",RC
        ENDIF
      close(nofsync)
!
    endif
!
  end subroutine write_nemsio

!----------------------------------------------------------------------------------------

  subroutine write_nemaio_final()

!**
    if(allocated(lon1d))  deallocate(lon1d)
    if(allocated(lat1d))  deallocate(lat1d)
    if(allocated(vcoord)) deallocate(vcoord)
    if(allocated(idsl))   deallocate(idsl, idvc,idvm)
    deallocate(irecv)
    deallocate(idisp)
    deallocate(fieldcount)

  end subroutine write_nemaio_final
!
!----------------------------------------------------------------------------------------

  subroutine get_global_attr(fldbundle, mybdl, rc)
    type(ESMF_FieldBundle), intent(in) :: fldbundle
    integer, intent(in)                :: mybdl
    integer, intent(out)               :: rc

! local variable
   integer i,j,k,n,kz, attcount
   integer ni,naryi,nr4,nr8, nc
   integer aklen
   character(80) attName, hydrostatics, fldname
   type(ESMF_TypeKind_Flag)                :: typekind
   real(4), dimension(:), allocatable :: ak4,bk4
   real(8), dimension(:), allocatable :: ak8,bk8
!
! look at the field bundle attributes 
    call ESMF_AttributeGet(fldbundle, convention="NetCDF", purpose="FV3", &
      attnestflag=ESMF_ATTNEST_OFF, Count=attcount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

! first loop over all the attributes to find the count for integer attr, real
! attr, etc
    j=1
    k=1
    if (.not. allocated(nmetavari))  then
      allocate(nmetavari(nbdl),nmetavarr4(nbdl),nmetavarr8(nbdl),nmetavarc(nbdl))
      allocate(nmetaaryi(nbdl),nmetaaryr4(nbdl),nmetaaryr8(nbdl),nmetaaryc(nbdl))
      allocate(extrameta(nbdl))
    endif
    nmetavari(mybdl)=0; nmetavarr4(mybdl)=0; nmetavarr8(mybdl)=0; nmetavarc(mybdl)=0
    nmetaaryi(mybdl)=0; nmetaaryr4(mybdl)=0; nmetaaryr8(mybdl)=0; nmetaaryc(mybdl)=0
    aklen=0.
    do i=1, attCount

      call ESMF_AttributeGet(fldbundle, convention="NetCDF", purpose="FV3", &
        attnestflag=ESMF_ATTNEST_OFF, attributeIndex=i,name=attName, typekind=typekind, &
        itemCount=n, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

! add this attribute to the list of transfers
      if (typekind==ESMF_TYPEKIND_CHARACTER) then
        if( n == 1) then
          nmetavarc(mybdl) = nmetavarc(mybdl) + 1
        else if (n > 1) then
          nmetaaryc(mybdl) = nmetaaryc(mybdl) + 1
        endif
      else if (typekind==ESMF_TYPEKIND_I4) then
        if( n == 1) then
          nmetavari(mybdl) = nmetavari(mybdl) + 1
        else if (n > 1) then
          nmetaaryi(mybdl) = nmetaaryi(mybdl) + 1
        endif
      else if (typekind==ESMF_TYPEKIND_R4) then
        if( n == 1) then
          nmetavarr4(mybdl) = nmetavarr4(mybdl) + 1
        else if (n > 1) then
          if( trim(attName) == "ak" .or. trim(attName) == "bk") then
            aklen = n
          else
            nmetaaryr4(mybdl) = nmetaaryr4(mybdl) + 1
          endif
        endif
      else if (typekind==ESMF_TYPEKIND_R8) then
        if( n == 1) then
          nmetavarr8(mybdl) = nmetavarr8(mybdl) + 1
        else if (n > 1) then
          if( trim(attName) == "ak" .or. trim(attName) == "bk") then
            aklen = n
          else
            nmetaaryr8(mybdl) = nmetaaryr8(mybdl) + 1
          endif
        endif
      endif
    enddo
    if(mybdl==2) nmetaaryi(mybdl) = nmetaaryi(mybdl) + 1
!    print *,'in get _global_attr, nmetavarc=',nmetavarc(mybdl),'nmetaaryc=',nmetaaryc(mybdl), &
!      'nmetavari=',nmetavari(mybdl),'nmetaaryi=',nmetaaryi(mybdl),'nmetavarr4=',nmetavarr4(mybdl), &
!      'nmetavarr8=',nmetavarr8(mybdl) , 'aklen=',aklen
!
! get value:
    if (nmetavari(mybdl) > 0) then 
      if(.not.allocated(variname))   allocate(variname(100,nbdl),varival(100,nbdl))
    endif
    if (nmetavarr4(mybdl) > 0) then
      if(.not.allocated(varr4name))  allocate(varr4name(100,nbdl),varr4val(100,nbdl))
    endif
    if (nmetavarr8(mybdl) > 0) then
      if(.not.allocated(varr8name))  allocate(varr8name(100,nbdl),varr8val(100,nbdl))
    endif
    nmetavarc(mybdl) = nmetavarc(mybdl) + 2
    if (nmetavarc(mybdl) > 0)  then
      if(.not.allocated(varcname))  allocate(varcname(100,nbdl),varcval(100,nbdl))
    endif
    if (nmetaaryi(mybdl) > 0)  then
      if(.not.allocated(aryiname))  then
        allocate(aryiname(20,nbdl),aryilen(20,nbdl))
        allocate(aryival(2000,20,nbdl))
      endif
    endif
    if (aklen > 0)  then
      if(.not.allocated(vcoord))  allocate(vcoord(aklen,3,2))
      vcoord = 0.
    endif
!
    ni=0; nr4=0; nr8=0; nc=0; naryi=0
    do i=1, attCount

      call ESMF_AttributeGet(fldbundle, convention="NetCDF", purpose="FV3", &
        attnestflag=ESMF_ATTNEST_OFF, attributeIndex=i, name=attName, &
        typekind=typekind, itemCount=n,  rc=rc)
       print *,'in write nemsio fist get att, att=',trim(attName),'n=',n
      
      if (typekind==ESMF_TYPEKIND_I4 ) then
        if(n==1) then
         ni = ni + 1
         variname(ni,mybdl) = trim(attName)
         call ESMF_AttributeGet(fldbundle, convention="NetCDF", purpose="FV3", &
           name=trim(variname(ni,mybdl)), value=varival(ni,mybdl), rc=rc)
         if (trim(variname(ni,mybdl)) == 'ncnsto') ntrac=varival(ni,mybdl)
         if (trim(variname(ni,mybdl)) == 'ncld')   ncld=varival(ni,mybdl)
         if (trim(variname(ni,mybdl)) == 'nsoil')  nsoil=varival(ni,mybdl)
        else
         naryi = naryi + 1
         aryiname(naryi,mybdl) = trim(attName)
         aryilen(naryi,mybdl)  = n
         call ESMF_AttributeGet(fldbundle, convention="NetCDF", purpose="FV3", &
              name=trim(attName), valueList=aryival(1:n,naryi,mybdl),rc=rc)
        endif
      else if (typekind==ESMF_TYPEKIND_R4) then
        if(n==1) then
         nr4 = nr4 + 1
         varr4name(nr4,mybdl) = trim(attName)
         call ESMF_AttributeGet(fldbundle, convention="NetCDF", purpose="FV3", &
           name=trim(varr4name(nr4,mybdl)), value=varr4val(nr4,mybdl), rc=rc)
         if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out
        else
          if(trim(attName) =="ak") then
            allocate(ak4(n))
            call ESMF_AttributeGet(fldbundle, convention="NetCDF", purpose="FV3", &
              name=trim(attName), valueList=ak4, rc=rc)
            if( write_nemsioflip ) then
              kz=size(ak4)
              do k=1,kz
                vcoord(k,1,1) = ak4(kz-k+1)
              enddo
            else
              vcoord(:,1,1) = ak4(:)
            endif
            deallocate(ak4)
          else if(trim(attName) =="bk") then
            allocate(bk4(n))
            call ESMF_AttributeGet(fldbundle, convention="NetCDF", purpose="FV3", &
              name=trim(attName), valueList=bk4, rc=rc)
            if( write_nemsioflip ) then
              kz=size(bk4)
              do k=1,kz
                vcoord(k,2,1) = bk4(kz-k+1)
              enddo
            else
              vcoord(:,2,1) = bk4(:)
            endif
            deallocate(bk4)
          endif
        endif
      else if (typekind==ESMF_TYPEKIND_R8) then
        if(n==1) then
         nr8 = nr8 + 1
         varr8name(nr8,mybdl) = trim(attName)
         call ESMF_AttributeGet(fldbundle, convention="NetCDF", purpose="FV3", &
           name=trim(varr8name(nr8,mybdl)), value=varr8val(nr8,mybdl), rc=rc)
        else
          if(trim(attName) =="ak") then
            allocate(ak8(n))
            call ESMF_AttributeGet(fldbundle, convention="NetCDF", purpose="FV3", &
              name=trim(attName), valueList=ak8, rc=rc)
            if( write_nemsioflip ) then
              kz=size(ak8)
              do k=1,kz
                vcoord(k,1,1) = ak8(kz-k+1)
              enddo
            else
              vcoord(:,1,1) = ak8(:)
            endif
            deallocate(ak8)
          else if(trim(attName) =="bk") then
            allocate(bk8(n))
            call ESMF_AttributeGet(fldbundle, convention="NetCDF", purpose="FV3", &
              name=trim(attName), valueList=bk8, rc=rc)
            if( write_nemsioflip ) then
              kz=size(bk8)
              do k=1,kz
                vcoord(k,2,1) = bk8(kz-k+1)
              enddo
            else
              vcoord(:,2,1) = bk8(:)
            endif
            deallocate(bk8)
          endif
        endif
      else if (typekind==ESMF_TYPEKIND_CHARACTER) then
        if(n==1) then
         nc = nc + 1
         varcname(nc,mybdl) = trim(attName)
         call ESMF_AttributeGet(fldbundle, convention="NetCDF", purpose="FV3", &
           name=trim(varcname(nc,mybdl)), value=varcval(nc,mybdl), rc=rc)
        endif
      endif
     
!      if(nmetavari(mybdl)>0) print *,'variname=',variname(1,mybdl),'varival=',varival(1,mybdl)
!      if(nmetavarc(mybdl)>0) print *,'varcname=',varcname(1,mybdl),'varcval=',varcval(1,mybdl)
!
    enddo
!
    if(write_nemsioflip) then
      nc=nc+1
      varcname(nc,mybdl) = 'y-direction'
      varcval(nc,mybdl) = 'north2south'
      nc=nc+1
      varcname(nc,mybdl) = 'z-direction'
      varcval(nc,mybdl) = 'bottom2top'
    else
      nc=nc+1
      varcname(nc,mybdl) = 'y-direction'
      varcval(nc,mybdl) = 'south2north'
      nc=nc+1
      varcname(nc,mybdl) = 'z-direction'
      varcval(nc,mybdl) = 'top2bottom'
    endif
!
!output lpl
    if( mybdl == 2 ) then
      naryi = naryi + 1
      aryiname(naryi,mybdl) = 'lpl'
      aryilen(naryi,mybdl)  = jm
      aryival(1:jm,naryi,mybdl) = im
    endif

    if( nmetavari(mybdl)>0 .or. nmetavarc(mybdl)>0 .or. nmetavarr4(mybdl) >0 .or. nmetavarr8(mybdl)>0 &
      .or. nmetaaryi(mybdl)>0) then
      extrameta(mybdl) = .true.
    else
      extrameta(mybdl) = .false.
    endif
!
  end subroutine get_global_attr
!
!----------------------------------------------------------------------------------------

end module module_write_nemsio

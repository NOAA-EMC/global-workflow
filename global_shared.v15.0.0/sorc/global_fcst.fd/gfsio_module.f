!----------------------------------------------------------------------------
module gfsio_module
!$$$ module document block
!
! module:   gfsio_module      api for global forecast model output 
!   in gaussian grid
!
! abstract: this module takes model output in gaussian(native) grid and writes them
!   out in grib format
!
! program history log
!    2006-11-10    jun wang
!    2013-01-02    jun wang  add precision option in gfsio_writerec routine 
!    2013-01-03    jun wang  add byteswap for little endian machine, gfsio output 
!                            files will be big endian files
!
! public variables
! public defined types
!   gfsio_gfile
!     private
!        gtype:   character(gfsio_charkind)  gfsio file identifier
!        version: integer(gfsio_intkind)  verion number
!        nmeta:   integer(gfsio_intkind)  number of metadata rec
!        lmeta:   integer(gfsio_intkind)  length of metadata rec 2 for model paramodels
!        nrec:    integer(gfsio_intkind)  number of data rec
!        fhour:   real(gfsio_intkind)  forecast hour
!        idate:   integer(gfsio_intkind)   initial date
!        jcap:    integer(gfsio_intkind):: spectral truncation
!        levs:    integer(gfsio_intkind):: number of levels
!        latf:    integer(gfsio_intkind):: latitude in dynamics
!        lonf:    integer(gfsio_intkind):: longitudes in dynamics
!        latb:    integer(gfsio_intkind):: latitudes in physics
!        lonb:    integer(gfsio_intkind):: longitudes in physics
!        latr:    integer(gfsio_intkind):: latitudes in radiation
!        lonr:    integer(gfsio_intkind):: longitudes in radiation
!        itrun:   integer(gfsio_intkind):: truncation flag (=1 for triangular)
!        iorder:  integer(gfsio_intkind):: coefficient order flag (=2 for ibm order)
!        irealf:  integer(gfsio_intkind):: floating point flag
!                 (=1 for 4-byte ieee, =2 for 8-byte ieee)
!        igen:    integer(gfsio_intkind):: model generating flag
!        ntrac:   integer(gfsio_intkind):: number of tracers
!        icen2:   integer(gfsio_intkind):: subcenter id
!        iens:    integer(gfsio_intkind):: ensemble ids
!        idpp:    integer(gfsio_intkind):: processing id
!        idsl:    integer(gfsio_intkind):: semi-lagrangian id
!        idvc:    integer(gfsio_intkind):: vertical coordinate id
!        idvm:    integer(gfsio_intkind):: mass variable id
!        idvt:    integer(gfsio_intkind):: tracer variable id
!        idun:    integer(gfsio_intkind):: run id
!        idusr:   integer(gfsio_intkind):: user-defined id
!        pdryini: integer(gfsio_intkind):: global mean dry air pressure (kpa)
!        ncldt:   integer(gfsio_intkind):: number of cloud types
!        ixgr:    integer(gfsio_intkind):: extra grid field id
!        nvcoord: integer(gfsio_intkind):: number of vcoord profiles
!        idrt:    integer(gfsio_intkind):: grid identifier
!                 (idrt=4 for gaussian grid,
!                  idrt=0 for equally-spaced grid including poles,
!                  idrt=256 for equally-spaced grid excluding poles)
!
!        vcoord:  real(gfsio_realkind),allocatable      :: vcoord(:,:)
!        recname: character(gfsio_charkind),allocatable :: recname(:)
!        reclevtyp: character(gfsio_charkind*2),allocatable :: reclevtyp(:)
!        reclev:  integer(gfsio_intkind),allocatable    :: reclev(:)
!        glat1d:  real(gfsio_realkind),allocatable      :: glat1d(:)
!        glon1d:  real(gfsio_realkind),allocatable      :: glon1d(:)
!        cpi:     real(gfsio_realkind),allocatable      :: cpi(:)
!        ri:      real(gfsio_realkind),allocatable      :: ri(:)
!
!!--- file handler
!        gfname:  character(255)  file name
!        gaction: character(gfsio_charkind)  read/write
!        flunit:  integer(gfsio_intkind)  unit number  
!
! public method
!   gfsio_init
!   gfsio_finalize
!   gfsio_open
!   gfsio_writerec
!   gfsio_readirec
!   gfsio_writerecv
!   gfsio_readirecv
!   gfsio_writerecw34
!   gfsio_readirecw34
!   gfsio_writerecvw34
!   gfsio_readirecvw34
!   gfsio_close
!   gfsio_getfilehead
!   gfsio_getrechead
! possible return code
!          0   successful call
!         -1   open or close i/o error
!         -2   array size
!         -3   meta data i/o error (possible eof)
!         -4   getgb/putgb error
!         -5   search record and set grib message info error
!         -6   allocate/deallocate error
!         -7   set grib table
!         -8   file meta data initialization (default:1152*576)
!         -9   not gfsio type file
!         -10  get/close file unit
!
!$$$ end module document block
!
  implicit none
  private
!------------------------------------------------------------------------------
! private variables
  integer,parameter:: gfsio_lmeta1=32
  integer,parameter:: gfsio_intkind=4,gfsio_realkind=4,gfsio_dblekind=8
  integer,parameter:: gfsio_charkind=8
  real(gfsio_intkind),parameter:: gfsio_intfill=-9999_gfsio_intkind
  real(gfsio_intkind),parameter:: gfsio_kpds_intfill=-1_gfsio_intkind
  real(gfsio_realkind),parameter:: gfsio_realfill=-9999._gfsio_realkind
! real(gfsio_dblekind),parameter:: gfsio_dblefill=-9999._gfsio_dblekind
!------------------------------------------------------------------------------
!---  public types
  type,public :: gfsio_gfile
    private
    character(gfsio_charkind) :: gtype=' '
    integer(gfsio_intkind):: version=gfsio_intfill
    integer(gfsio_intkind):: nmeta=gfsio_intfill
    integer(gfsio_intkind):: lmeta=gfsio_intfill
    integer(gfsio_intkind):: nrec=gfsio_intfill
    integer(gfsio_intkind):: fhour=gfsio_intfill
    integer(gfsio_intkind):: idate(4)=gfsio_intfill
    integer(gfsio_intkind):: jcap=gfsio_intfill
    integer(gfsio_intkind):: levs=gfsio_intfill
    integer(gfsio_intkind):: latf=gfsio_intfill
    integer(gfsio_intkind):: lonf=gfsio_intfill
    integer(gfsio_intkind):: latb=gfsio_intfill
    integer(gfsio_intkind):: lonb=gfsio_intfill
    integer(gfsio_intkind):: latr=gfsio_intfill
    integer(gfsio_intkind):: lonr=gfsio_intfill
    integer(gfsio_intkind):: itrun=gfsio_intfill
    integer(gfsio_intkind):: iorder=gfsio_intfill
    integer(gfsio_intkind):: irealf=gfsio_intfill
    integer(gfsio_intkind):: igen=gfsio_intfill
    integer(gfsio_intkind):: ntrac=gfsio_intfill
    integer(gfsio_intkind):: icen2=gfsio_intfill
    integer(gfsio_intkind):: iens(2)=gfsio_intfill
    integer(gfsio_intkind):: idpp=gfsio_intfill
    integer(gfsio_intkind):: idsl=gfsio_intfill
    integer(gfsio_intkind):: idvc=gfsio_intfill
    integer(gfsio_intkind):: idvm=gfsio_intfill
    integer(gfsio_intkind):: idvt=gfsio_intfill
    integer(gfsio_intkind):: idrun=gfsio_intfill
    integer(gfsio_intkind):: idusr=gfsio_intfill
    integer(gfsio_intkind):: pdryini=gfsio_intfill
    integer(gfsio_intkind):: ncldt=gfsio_intfill
    integer(gfsio_intkind):: ixgr=gfsio_intfill
    integer(gfsio_intkind):: nvcoord=gfsio_intfill
    integer(gfsio_intkind):: idrt=gfsio_intfill
    real(gfsio_realkind),allocatable      :: vcoord(:,:)
    character(gfsio_charkind),allocatable :: recname(:)
    character(gfsio_charkind*2),allocatable :: reclevtyp(:)
    integer(gfsio_intkind),allocatable    :: reclev(:)
    real(gfsio_realkind),allocatable      :: glat1d(:)
    real(gfsio_realkind),allocatable      :: glon1d(:)
    real(gfsio_realkind),allocatable      :: cpi(:)
    real(gfsio_realkind),allocatable      :: ri(:)
    character(255) :: gfname
    character(gfsio_charkind) :: gaction
    integer(gfsio_intkind):: flunit=gfsio_intfill
    character,allocatable     :: cbuf(:)
    integer(gfsio_intkind):: mbuf=0,nlen,nnum,mnum
  end type gfsio_gfile
!------------------------------------------------------------------------------
!--- private types
  type :: gfsio_meta1
    sequence
    character(gfsio_charkind) :: gtype
    integer(gfsio_intkind) :: version,nmeta,lmeta,reserve(3)
  end type gfsio_meta1
!
  type :: gfsio_meta2
    sequence
    integer(gfsio_intkind) :: nrec,fhour,idate(4),latb,lonb,levs, &
      jcap,itrun,iorder,irealf,igen,latf,lonf,latr,lonr,ntrac,icen2, &
      iens(2),idpp,idsl,idvc,idvm,idvt,idrun,idusr,pdryini,ncldt, &
      ixgr,nvcoord,idrt
  end type gfsio_meta2
!
  type  :: gfsio_grbmeta
    integer(gfsio_intkind)   :: jf=gfsio_intfill
    integer(gfsio_intkind)   :: j=gfsio_kpds_intfill
    logical*1,allocatable    :: lbms(:)
    integer(gfsio_intkind)   :: jpds(200)=gfsio_kpds_intfill
    integer(gfsio_intkind)   :: jgds(200)=gfsio_kpds_intfill
  end type gfsio_grbmeta
!
  type :: gfsio_grbtbl_item
    character(gfsio_charkind)     :: shortname=' '
    character(gfsio_charkind*4)   :: leveltype=' '
    integer(gfsio_intkind)    :: precision,g1tbl,g1param,g1level 
  end type gfsio_grbtbl_item
!
  type(gfsio_grbtbl_item),save  :: gribtable(255)
!----- interface
  interface gfsio_readrec
    module procedure gfsio_readrec4
    module procedure gfsio_readrec8
  end interface gfsio_readrec
!
  interface gfsio_readrecv
    module procedure gfsio_readrecv4
    module procedure gfsio_readrecv8
  end interface gfsio_readrecv
!
  interface gfsio_writerec
    module procedure gfsio_writerec4
    module procedure gfsio_writerec8
  end interface gfsio_writerec
!
  interface gfsio_writerecv
    module procedure gfsio_writerecv4
    module procedure gfsio_writerecv8
  end interface gfsio_writerecv
!
  interface splat
    module procedure gfsio_splat4
    module procedure gfsio_splat8
  end interface splat
!
!--- file unit for putgb/getgb ----
  integer(gfsio_intkind),save   :: fileunit(600:699)=0
!------------------------------------------------------------------------------
!public mehtods
  public gfsio_init,gfsio_finalize,gfsio_open,gfsio_close
  public gfsio_readrec,gfsio_writerec,gfsio_readrecv,gfsio_writerecv
  public gfsio_readrecw34,gfsio_writerecw34,gfsio_readrecvw34,gfsio_writerecvw34
  public gfsio_getfilehead,gfsio_getrechead
!
contains
!-------------------------------------------------------------------------------
  subroutine gfsio_init(iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! initialization
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    implicit none
    integer(gfsio_intkind),optional,intent(out):: iret
    integer :: ios
!------------------------------------------------------------
! abstract: set grib table 
!------------------------------------------------------------
    call gfsio_setgrbtbl(ios)
    if ( ios.ne.0) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call gfsio_stop
       endif
    endif
    if ( present(iret)) iret=0
  end subroutine gfsio_init
!------------------------------------------------------------------------------
  subroutine gfsio_finalize()
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! abstract: finalization
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    implicit none

  end subroutine gfsio_finalize
!------------------------------------------------------------------------------
  subroutine gfsio_open(gfile,gfname,gaction,iret,gtype,version, &
      nmeta,lmeta,fhour,idate,&
      nrec,latb,lonb,levs,jcap,itrun,iorder,irealf,igen,latf,lonf,latr,&
      lonr,ntrac,icen2,iens,idpp,idsl,idvc,idvm,idvt,idrun,idusr,&
      pdryini,ncldt,ixgr,nvcoord,idrt,vcoord,recname,reclevtyp,reclev, &
      cpi, ri)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! abstract: open gfsio file, and read/write the meta data
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    implicit none
    type(gfsio_gfile),intent(inout)     :: gfile
    character*(*),intent(in)            :: gfname
    character*(*),intent(in)            :: gaction
!------------------------------------------------------------
! optional vaviables
!------------------------------------------------------------
    integer(gfsio_intkind),optional,intent(out):: iret
    character*(*),optional,intent(in)          :: gtype
    integer(gfsio_intkind),optional,intent(in) :: version
    real(gfsio_realkind),optional,intent(in)   :: fhour,pdryini
    integer(gfsio_intkind),optional,intent(in) :: idate(4),iens(2)
    integer(gfsio_intkind),optional,intent(in) :: nrec,latb,lonb,&
      levs,nmeta,lmeta,&
      jcap,itrun,iorder,irealf,igen,latf,lonf,latr,lonr,ntrac,icen2,&
      idpp,idsl,idvc,idvm,idvt,idrun,idusr,ncldt,ixgr,nvcoord
    integer(gfsio_intkind),optional,intent(in) :: idrt
    real(gfsio_realkind),optional,intent(in)   :: vcoord(:,:)
    character*(*),optional,intent(in)          :: recname(:)
    character*(*),optional,intent(in)          :: reclevtyp(:)
    integer(gfsio_intkind),optional,intent(in) :: reclev(:)
    real(gfsio_realkind),optional,intent(in)   :: cpi(:)
    real(gfsio_realkind),optional,intent(in)   :: ri(:)
    integer(gfsio_intkind)      :: ios
!------------------------------------------------------------
! assign a unit number 
!------------------------------------------------------------
    if (present(iret)) iret=-1
    call gfsio_getlu(gfile,gfname,gaction,ios)
    if ( ios.ne.0 ) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call gfsio_stop
       endif
    endif
!------------------------------------------------------------
! open and read meta data for read
!------------------------------------------------------------
    if ( gaction .eq. "read" .or. gaction .eq. "read") then
      call baopenr(gfile%flunit,gfname,ios)
      if ( ios.ne.0) then
       if ( present(iret))  then
         return
       else
         call gfsio_stop
       endif
      endif
!
! read  meta data for gfile
!
      call gfsio_rcreate(gfile,ios)
      if ( ios.ne.0) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call gfsio_stop
       endif
      endif
!
!set grib index buf 
!
      gfile%mbuf=256*1024
      gfile%nnum=0
      gfile%nlen=0
      gfile%mnum=-1
      if(allocated(gfile%cbuf)) deallocate(gfile%cbuf)
      allocate(gfile%cbuf(gfile%mbuf))
!------------------------------------------------------------
! open and write meta data for write
!------------------------------------------------------------
    elseif (gaction .eq. "write" .or. gaction .eq. "write" ) then
      call baopenwt(gfile%flunit,gfname,ios)
      if ( ios.ne.0) then
       if ( present(iret))  then
         return
       else
         call gfsio_stop
       endif
      endif
      call gfsio_wcreate(gfile,ios,gtype=gtype,version=version, & 
        nmeta=nmeta,lmeta=lmeta,fhour=fhour,idate=idate, &
        nrec=nrec,lonb=lonb,latb=latb,levs=levs,jcap=jcap, &
        itrun=itrun,iorder=iorder,irealf=irealf,igen=igen,latf=latf,&
        lonf=lonf,latr=latr,lonr=lonr,ntrac=ntrac,icen2=icen2,iens=iens,&
        idpp=idpp,idsl=idsl,idvc=idvc,idvm=idvm,idvt=idvt,idrun=idrun,&
        idusr=idusr,pdryini=pdryini,ncldt=ncldt,ixgr=ixgr,nvcoord=nvcoord,&
        idrt=idrt, &
        vcoord=vcoord,recname=recname,reclevtyp=reclevtyp,reclev=reclev, &
        cpi=cpi,ri=ri)
      if ( ios.ne.0) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call gfsio_stop
       endif
     endif
!------------------------------------------------------------
! if gaction is wrong
!------------------------------------------------------------
    else
       if ( present(iret))  then
         return
       else
         call gfsio_stop
       endif
    endif
    iret=0
  end subroutine gfsio_open
!------------------------------------------------------------------------------
  subroutine gfsio_close(gfile,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! abstract: close gfile including closing the file, returning unit number, 
!           setting file meta data empty
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    implicit none
    type(gfsio_gfile),intent(inout)     :: gfile
    integer(gfsio_intkind),optional,intent(out)  :: iret
    integer(gfsio_intkind)      :: ios
!------------------------------------------------------------
! close the file
!------------------------------------------------------------
    if ( present(iret) ) iret=-1
    call baclose(gfile%flunit,ios)
    if ( ios.ne.0) then
       if ( present(iret))  then
         return
       else
         call gfsio_stop
       endif
    endif
!------------------------------------------------------------
! free the file unit
!------------------------------------------------------------
    call gfsio_clslu(gfile,ios)
    if ( ios.ne.0) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call gfsio_stop
       endif
    endif
!------------------------------------------------------------
! empty gfile meta data
!------------------------------------------------------------
    call gfsio_axmeta(gfile,ios)
    if ( ios.ne.0) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call gfsio_stop
       endif
    endif
    if ( present(iret)) iret=0
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  end subroutine gfsio_close
!------------------------------------------------------------------------------
  subroutine gfsio_rcreate(gfile,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read gfsio meta data
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(gfsio_gfile),intent(inout)     :: gfile
    integer(gfsio_intkind),intent(out):: iret
    integer(gfsio_intkind)      :: ios,iskip,iread,nread
    type(gfsio_meta1)           :: meta1
    type(gfsio_meta2)           :: meta2
    integer                     :: nmeta
!------------------------------------------------------------
! read first meta data record
!------------------------------------------------------------
    iret=-3
    iskip=0
    iread=gfsio_lmeta1
    call bafrread(gfile%flunit,iskip,iread,nread,meta1)
    if(nread.lt.iread) return
    gfile%gtype=meta1%gtype
    gfile%version=meta1%version
    gfile%nmeta=meta1%nmeta
    gfile%lmeta=meta1%lmeta
    if ( gfile%gtype(1:5) .ne. 'GFSIO' ) then
      iret=-9
      return
    endif
    if ( gfile%nmeta .eq. 6 ) then
      gfile%nmeta=10
      nmeta=7
    else
      nmeta=gfile%nmeta-1
    endif
!------------------------------------------------------------
! read second meta data record
!------------------------------------------------------------
    iskip=iskip+nread
    iread=gfile%lmeta
    call bafrread(gfile%flunit,iskip,iread,nread,meta2)
    if(nread.lt.iread) return
    gfile%fhour=meta2%fhour
    gfile%idate=meta2%idate
    gfile%nrec=meta2%nrec
    gfile%latb=meta2%latb
    gfile%lonb=meta2%lonb
    gfile%levs=meta2%levs
    gfile%itrun=meta2%itrun
    gfile%jcap=meta2%jcap
    gfile%iorder=meta2%iorder
    gfile%irealf=meta2%irealf
    gfile%igen=meta2%igen
    gfile%latf=meta2%latf
    gfile%lonf=meta2%lonf
    gfile%latr=meta2%latr
    gfile%lonr=meta2%lonr
    gfile%ntrac=meta2%ntrac
    gfile%icen2=meta2%icen2
    gfile%iens=meta2%iens
    gfile%idpp=meta2%idpp
    gfile%idsl=meta2%idsl
    gfile%idvc=meta2%idvc
    gfile%idvm=meta2%idvm
    gfile%idvt=meta2%idvt
    gfile%idrun=meta2%idrun
    gfile%idusr=meta2%idusr
    gfile%pdryini=meta2%pdryini
    gfile%ncldt=meta2%ncldt
    gfile%ixgr=meta2%ixgr
    gfile%nvcoord=meta2%nvcoord
    gfile%idrt=meta2%idrt
    nmeta=nmeta-1
!------------------------------------------------------------
! set up gfile arrays
!------------------------------------------------------------
    call gfsio_almeta(gfile,ios)
    if ( ios .ne. 0 ) then
      iret=ios
      return
    endif
!------------------------------------------------------------
! read gfile meta data array
!------------------------------------------------------------
!vcoord
    iskip=iskip+nread
    iread=gfsio_realkind*size(gfile%vcoord)
    call bafrread(gfile%flunit,iskip,iread,nread,gfile%vcoord)
    if(nread.lt.iread) return
    nmeta=nmeta-1
!recname
    iskip=iskip+nread
    iread=gfsio_charkind*size(gfile%recname)
    call bafrread(gfile%flunit,iskip,iread,nread,gfile%recname)
    if(nread.lt.iread) return
    nmeta=nmeta-1
!reclevtyp
    iskip=iskip+nread
    iread=2*gfsio_charkind*size(gfile%reclevtyp)
    call bafrread(gfile%flunit,iskip,iread,nread,gfile%reclevtyp)
    if(nread.lt.iread) return
    nmeta=nmeta-1
!reclev
    iskip=iskip+nread
    iread=gfsio_intkind*size(gfile%reclev)
    call bafrread(gfile%flunit,iskip,iread,nread,gfile%reclev)
    if(nread.lt.iread) return
    nmeta=nmeta-1
!glat
    iskip=iskip+nread
    iread=gfsio_realkind*size(gfile%glat1d)
    call bafrread(gfile%flunit,iskip,iread,nread,gfile%glat1d)
    if(nread.lt.iread) return
    nmeta=nmeta-1
!glon
    iskip=iskip+nread
    iread=gfsio_realkind*size(gfile%glon1d)
    call bafrread(gfile%flunit,iskip,iread,nread,gfile%glon1d)
    if(nread.lt.iread) return
    nmeta=nmeta-1
!cpi
    if ( nmeta .gt.0 ) then
      iskip=iskip+nread
      iread=gfsio_realkind*size(gfile%cpi)
      call bafrread(gfile%flunit,iskip,iread,nread,gfile%cpi)
      if(nread.lt.iread) return
      nmeta=nmeta-1
!ri
      iskip=iskip+nread
      iread=gfsio_realkind*size(gfile%ri)
      call bafrread(gfile%flunit,iskip,iread,nread,gfile%ri)
      if(nread.lt.iread) return
      nmeta=nmeta-1
    else
     gfile%cpi=1003.
     gfile%ri=287.
    endif
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    iret=0
  end subroutine gfsio_rcreate
!------------------------------------------------------------------------------
  subroutine gfsio_wcreate(gfile,iret,gtype,version,nmeta,lmeta,&
      fhour,idate, nrec,latb,lonb,&
      levs,jcap,itrun,iorder,irealf,igen,latf,lonf,latr,lonr,&
      ntrac,icen2,iens,idpp,idsl,idvc,idvm,idvt,idrun,idusr,pdryini, &
      ncldt,ixgr,nvcoord,idrt,vcoord,recname,reclevtyp,reclev, &
      cpi,ri)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: write gfsio meta data
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(gfsio_gfile),intent(inout)            :: gfile
    integer(gfsio_intkind),intent(out)         :: iret
!optional variables
    character*(*),optional,intent(in)          :: gtype
    integer(gfsio_intkind),optional,intent(in) :: version
    real(gfsio_realkind),optional,intent(in)   :: fhour,pdryini
    integer(gfsio_intkind),optional,intent(in) :: idate(4),iens(2)
    integer(gfsio_intkind),optional,intent(in) :: nrec,latb,lonb,levs, &
      jcap,itrun, iorder,irealf,igen,latf,lonf,latr,lonr,ntrac,icen2,idpp, &
      idsl,idvc,idvm,idvt,idrun,idusr,ncldt,ixgr,nvcoord,nmeta,lmeta
    integer(gfsio_intkind),optional,intent(in) :: idrt
    real(gfsio_realkind),optional,intent(in)   :: vcoord(:,:)
    character*(*),optional,intent(in)          :: recname(:)
    character*(*),optional,intent(in)          :: reclevtyp(:)
    integer(gfsio_intkind),optional,intent(in) :: reclev(:)
    real(gfsio_realkind),optional,intent(in)   :: cpi(:)
    real(gfsio_realkind),optional,intent(in)   :: ri(:)
!local variables
    real(gfsio_dblekind),allocatable :: slat(:)
    real(gfsio_realkind) :: radi
    integer(gfsio_intkind)      :: iskip,iwrite,nwrite,n
    type(gfsio_meta1)           :: meta1
    type(gfsio_meta2)           :: meta2
    integer(gfsio_intkind) :: ios
!------------------------------------------------------------
! set gfile meta data to operational model (default) if it's empty
!------------------------------------------------------------
    iret=-3
    if ( gfile%lonb .eq. gfsio_intfill .or. gfile%latb .eq. gfsio_intfill &
      .or. gfile%levs .eq. gfsio_intfill .or. gfile%nrec .eq. gfsio_intfill &
      .or. gfile%idate(1) .eq.gfsio_intfill ) then
      call gfsio_gfinit(gfile,ios)
      if (ios .ne.0 ) then
        iret=ios
        return
      endif
    endif
!------------------------------------------------------------
! set up basic gfile meta data variables from outsides to 
! define meta data array
!------------------------------------------------------------
    if(present(gtype)) gfile%gtype=gtype
    if ( gfile%gtype(1:5) .ne. 'GFSIO' ) then
      iret=-9
      return
    endif
    if(present(version)) gfile%version=version
    if(present(nmeta)) gfile%nmeta=nmeta
    if(present(lmeta)) gfile%lmeta=lmeta
    if(present(fhour)) gfile%fhour=nint(fhour*3600)
    if(present(idate)) gfile%idate=idate
    if ( gfile%idate(4) .lt. 50) then
       gfile%idate(4)=2000+gfile%idate(4)
    else if (gfile%idate(4) .lt. 100) then
       gfile%idate(4)=1999+gfile%idate(4)
    endif
    if(present(nrec)) gfile%nrec=nrec
    if(present(latb)) gfile%latb=latb
    if(present(lonb)) gfile%lonb=lonb
    if(present(levs)) gfile%levs=levs
    if(present(nvcoord)) gfile%nvcoord=nvcoord
    if(present(ntrac)) gfile%ntrac=ntrac
!------------------------------------------------------------
! check gfile meta data array size
!------------------------------------------------------------
    call gfsio_chkgfary(gfile,ios)
    if (ios.ne. 0) then
      iret=ios
      return
    endif
!------------------------------------------------------------
! continue to set gfile meta data variables tnd arrays
!------------------------------------------------------------
    if(present(jcap)) gfile%jcap=jcap
    if(present(itrun)) gfile%itrun=itrun
    if(present(iorder)) gfile%iorder=iorder
    if(present(irealf)) gfile%irealf=irealf
    if(present(igen)) gfile%igen=igen
    if(present(latf)) gfile%latf=latf
    if(present(lonf)) gfile%lonf=lonf
    if(present(latr)) gfile%latr=latr
    if(present(lonr)) gfile%lonr=lonr
    if(present(icen2)) gfile%icen2=icen2
    if(present(iens)) gfile%iens=iens
    if(present(idpp)) gfile%idpp=idpp
    if(present(idsl)) gfile%idsl=idsl
    if(present(idvc)) gfile%idvc=idvc
    if(present(idvm)) gfile%idvm=idvm
    if(present(idvt)) gfile%idvt=idvt
    if(present(idrun)) gfile%idrun=idrun
    if(present(idusr)) gfile%idusr=idusr
    if(present(pdryini)) gfile%pdryini=nint(pdryini*1.0e5)
    if(present(ncldt)) gfile%ncldt=ncldt
    if(present(ixgr)) gfile%ixgr=ixgr
!set idrt to default gaussian grid if not defined from outside
    if(present(idrt)) then 
      gfile%idrt=idrt
    else
      gfile%idrt=4
    endif
!vcoord
    if(present(vcoord) ) then
       if ((gfile%levs+1)*gfile%nvcoord.ne.size(vcoord)) then
         return
       else
         gfile%vcoord=vcoord
       endif
    endif
!recname
    if(present(recname) ) then
       if (gfile%nrec.ne.size(recname)) then
         return
       else
         gfile%recname=recname
       endif
    endif
!reclevtyp
    if(present(reclevtyp)) then
       if (gfile%nrec.ne.size(reclevtyp)) then
         return
       else
         gfile%reclevtyp=reclevtyp
       endif
    endif
!reclev
    if(present(reclev) ) then
       if (gfile%nrec.ne.size(reclev)) then
         return
       else
         gfile%reclev=reclev
       endif
    endif
!glat
    if (gfile%latb.ne.size(gfile%glat1d)) then
       return
    else
       allocate(slat(gfile%latb))
       call splat(gfile%idrt,gfile%latb,slat)
       radi=180.0 / (4.*atan(1.))
       do  n=1,gfile%latb
         gfile%glat1d(n) = asin(slat(n)) * radi
       enddo
       deallocate(slat)
    endif
!glon
    if (gfile%lonb.ne.size(gfile%glon1d)) then
       return
    else
       do  n=1,gfile%lonb
         gfile%glon1d(n) = 360./gfile%lonb*(n-1) 
       enddo
    endif
!cpi
    if( present(cpi) ) then
      if ( any(cpi.gt.0) ) then
       if (gfile%ntrac+1.ne.size(gfile%cpi)) then
         return
       else 
         gfile%cpi = cpi
       endif
      endif
    else
      print *,'warning, cp are not provided through gfsio_open'
    endif
!ri
    if( present(ri) ) then
      if (any(ri.gt.0) ) then
       if (gfile%ntrac+1.ne.size(gfile%ri)) then
         return
      else
         gfile%ri = ri
      endif
      endif
    else
      print *,'warning, r are not provided though gfsio_open'
    endif
!------------------------------------------------------------
! write out first meta data record
!------------------------------------------------------------
    meta1%gtype=gfile%gtype
    meta1%version=gfile%version
    meta1%nmeta=gfile%nmeta
    meta1%lmeta=gfile%lmeta
    meta1%reserve=0
    iskip=0
    iwrite=gfsio_lmeta1
    call bafrwrite(gfile%flunit,iskip,iwrite,nwrite,meta1)
    if(nwrite.lt.iwrite) return
!------------------------------------------------------------
! write out second meta data record
!------------------------------------------------------------
    meta2%fhour=gfile%fhour
    meta2%idate=gfile%idate
    meta2%nrec=gfile%nrec
    meta2%latb=gfile%latb
    meta2%lonb=gfile%lonb
    meta2%levs=gfile%levs
    meta2%jcap=gfile%jcap
    meta2%itrun=gfile%itrun
    meta2%iorder=gfile%iorder
    meta2%irealf=gfile%irealf
    meta2%igen=gfile%igen
    meta2%latf=gfile%latf
    meta2%lonf=gfile%lonf
    meta2%latr=gfile%latr
    meta2%lonr=gfile%lonr
    meta2%ntrac=gfile%ntrac
    meta2%icen2=gfile%icen2
    meta2%iens=gfile%iens
    meta2%idpp=gfile%idpp
    meta2%idsl=gfile%idsl
    meta2%idvc=gfile%idvc
    meta2%idvm=gfile%idvm
    meta2%idvt=gfile%idvt
    meta2%idrun=gfile%idrun
    meta2%idusr=gfile%idusr
    meta2%pdryini=gfile%pdryini
    meta2%ncldt=gfile%ncldt
    meta2%ixgr=gfile%ixgr
    meta2%nvcoord=gfile%nvcoord
    meta2%idrt=gfile%idrt
    iskip=iskip+nwrite
    iwrite=gfile%lmeta
    call bafrwrite(gfile%flunit,iskip,iwrite,nwrite,meta2)
    if(nwrite.lt.iwrite) return
!------------------------------------------------------------
! write out gfsio meta data array records
!------------------------------------------------------------
!vcoord
    iskip=iskip+nwrite
    iwrite=gfsio_realkind*size(gfile%vcoord)
    call bafrwrite(gfile%flunit,iskip,iwrite,nwrite,gfile%vcoord)
    if(nwrite.lt.iwrite) return
!recname
    iskip=iskip+nwrite
    iwrite=gfsio_charkind*size(gfile%recname)
    call bafrwrite(gfile%flunit,iskip,iwrite,nwrite,gfile%recname)
    if(nwrite.lt.iwrite) return
!reclevtyp
    iskip=iskip+nwrite
    iwrite=2*gfsio_charkind*size(gfile%reclevtyp)
    call bafrwrite(gfile%flunit,iskip,iwrite,nwrite,gfile%reclevtyp)
    if(nwrite.lt.iwrite) return
!reclev
    iskip=iskip+nwrite
    iwrite=gfsio_intkind*size(gfile%reclev)
    call bafrwrite(gfile%flunit,iskip,iwrite,nwrite,gfile%reclev)
    if(nwrite.lt.iwrite) return
!glat
    iskip=iskip+nwrite
    iwrite=gfsio_realkind*size(gfile%glat1d)
    call bafrwrite(gfile%flunit,iskip,iwrite,nwrite,gfile%glat1d)
    if(nwrite.lt.iwrite) return
!glon
    iskip=iskip+nwrite
    iwrite=gfsio_realkind*size(gfile%glon1d)
    call bafrwrite(gfile%flunit,iskip,iwrite,nwrite,gfile%glon1d)
    if(nwrite.lt.iwrite) return
!
!cpi
      iskip=iskip+nwrite
      iwrite=gfsio_realkind*size(gfile%cpi)
      call bafrwrite(gfile%flunit,iskip,iwrite,nwrite,gfile%cpi)
      if(nwrite.lt.iwrite) return
!ri
      iskip=iskip+nwrite
      iwrite=gfsio_realkind*size(gfile%ri)
      call bafrwrite(gfile%flunit,iskip,iwrite,nwrite,gfile%ri)
      if(nwrite.lt.iwrite) return
!
    iret=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine gfsio_wcreate
!------------------------------------------------------------------------------
  subroutine gfsio_getfilehead(gfile,iret,gtype,gfname,gaction, &
      version,fhour,idate, &
      nrec,latb,lonb,&
      levs,jcap,itrun,iorder,irealf,igen,latf,lonf,latr,lonr, &
      ntrac,icen2,iens,&
      idpp,idsl,idvc,idvm,idvt,idrun,idusr,pdryini,ncldt,ixgr,nvcoord,nmeta, &
      lmeta,idrt,vcoord,recname,reclevtyp,reclev,glat1d,glon1d, &
      cpi,ri)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: get gfsio meta data information from outside
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(gfsio_gfile),intent(in)                :: gfile
    integer(gfsio_intkind),optional,intent(out) :: iret
    character*(*),optional,intent(out)          :: gtype,gfname,gaction
    integer(gfsio_intkind),optional,intent(out) :: version
    real(gfsio_realkind),optional,intent(out)   :: fhour,pdryini
    integer(gfsio_intkind),optional,intent(out) :: idate(4),iens(2)
    integer(gfsio_intkind),optional,intent(out) :: nrec,latb, &
      lonb,levs,jcap,itrun,&
      iorder,irealf,igen,latf,lonf,latr,lonr,ntrac,icen2,idpp,idsl, &
      idvc,idvm,idvt,idrun,idusr,ncldt,ixgr,nvcoord,nmeta,lmeta,idrt
    real(gfsio_realkind),optional,intent(out)   :: vcoord(:,:)
    character(*),optional,intent(out)           :: recname(:)
    character(*),optional,intent(out)           :: reclevtyp(:)
    integer(gfsio_intkind),optional,intent(out) :: reclev(:)
    real(gfsio_realkind),optional,intent(out)   :: glat1d(:),glon1d(:)
    real(gfsio_realkind),optional,intent(out)   :: cpi(:),ri(:)
!------------------------------------------------------------
    if (present(iret)) iret=-3
    if(present(gtype)) gtype=gfile%gtype
    if(present(gfname)) gfname=gfile%gfname
    if(present(gaction)) gaction=gfile%gaction
    if(present(version)) version=gfile%version
    if(present(fhour)) fhour=gfile%fhour/3600.
    if(present(idate)) idate=gfile%idate
    if(present(nmeta)) nmeta=gfile%nmeta
    if(present(lmeta)) lmeta=gfile%lmeta
    if(present(nrec)) nrec=gfile%nrec
    if(present(latb)) latb=gfile%latb
    if(present(lonb)) lonb=gfile%lonb
    if(present(levs)) levs=gfile%levs
    if(present(jcap)) jcap=gfile%jcap
    if(present(itrun)) itrun=gfile%itrun
    if(present(iorder)) iorder=gfile%iorder
    if(present(irealf)) irealf=gfile%irealf
    if(present(igen)) igen=gfile%igen
    if(present(latf)) latf=gfile%latf
    if(present(lonf)) lonf=gfile%lonf
    if(present(latr)) latr=gfile%latr
    if(present(lonr)) lonr=gfile%lonr
    if(present(ntrac)) ntrac=gfile%ntrac
    if(present(icen2)) icen2=gfile%icen2
    if(present(iens)) iens=gfile%iens
    if(present(idpp)) idpp=gfile%idpp
    if(present(idsl)) idsl=gfile%idsl
    if(present(idvc)) idvc=gfile%idvc
    if(present(idvm)) idvm=gfile%idvm
    if(present(idvt)) idvt=gfile%idvt
    if(present(idrun)) idrun=gfile%idrun
    if(present(idusr)) idusr=gfile%idusr
    if(present(pdryini)) pdryini=(gfile%pdryini/1.0e5)
    if(present(ncldt)) ncldt=gfile%ncldt
    if(present(ixgr)) ixgr=gfile%ixgr
    if(present(idrt)) idrt=gfile%idrt
    if(present(nvcoord)) nvcoord=gfile%nvcoord
    if(present(vcoord)) then
       if (size(vcoord) .ne. (gfile%levs+1)*gfile%nvcoord ) then
         if ( present(iret))  then
           return
         else
           call gfsio_stop
         endif
       else
         vcoord=gfile%vcoord
       endif
    endif
    if(present(recname) ) then
       if (gfile%nrec.ne.size(recname)) then
         if ( present(iret))  then
           return
         else
           call gfsio_stop
         endif
       else
         recname=gfile%recname
       endif
    endif
    if(present(reclevtyp)) then
       if (gfile%nrec.ne.size(reclevtyp)) then
         if ( present(iret))  then
           return
         else
           call gfsio_stop
         endif
       else
         reclevtyp=gfile%reclevtyp
       endif
    endif
    if(present(reclev) ) then
       if (gfile%nrec.ne.size(reclev)) then
         if ( present(iret))  then
           return
         else
           call gfsio_stop
         endif
       else
         reclev=gfile%reclev
       endif
    endif
    if(present(glat1d) ) then
       if (gfile%latb.ne.size(glat1d)) then
         if ( present(iret))  then
           return
         else
           call gfsio_stop
         endif
       else
         glat1d=gfile%glat1d
       endif
    endif
    if(present(glon1d) ) then
       if (gfile%lonb.ne.size(glon1d)) then
         if ( present(iret))  then
           return
         else
           call gfsio_stop
         endif
       else
         glon1d=gfile%glon1d
       endif
    endif
!cpi
    if(present(cpi) ) then
       if (gfile%ntrac+1.ne.size(cpi)) then
         if ( present(iret))  then
           return
         else
           call gfsio_stop
         endif
       else
         cpi=gfile%cpi
       endif
    endif
!ri
    if(present(ri) ) then
       if (gfile%ntrac+1.ne.size(ri)) then
         if ( present(iret))  then
           return
         else
           call gfsio_stop
         endif
       else
         ri=gfile%ri
       endif
    endif
!
    if ( present(iret)) iret=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine gfsio_getfilehead
!------------------------------------------------------------------------------
  subroutine gfsio_readrecw34(gfile,jrec,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read gfsio data by record number into a 2d 32 bits array, 
!           using w3_4 library to compile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(gfsio_gfile),intent(in)                 :: gfile
    integer(gfsio_intkind),intent(in)            :: jrec
    real(gfsio_realkind),intent(out)             :: data(gfile%latb*gfile%lonb)
    integer(gfsio_intkind),optional,intent(out)  :: iret
    type(gfsio_grbmeta)         :: grbmeta
    integer(gfsio_intkind)      :: luidx
    integer(gfsio_intkind)      :: kf,k,kpds(200),kgds(200)
    logical*1,allocatable       :: lbms(:)
    integer(gfsio_intkind)      :: n=gfsio_kpds_intfill
    integer(gfsio_intkind)      :: ios,i,w34
!------------------------------------------------------------
! set up grib meta 
!------------------------------------------------------------
    luidx=0
    if ( present(iret)) iret=-4
    w34=1
    call gfsio_setrqst(gfile,grbmeta,ios,jrec=jrec,w34=w34)
    if (ios.ne.0) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call gfsio_stop
       endif
    endif
    allocate(lbms(grbmeta%jf))
    n=0
!------------------------------------------------------------
! get data from getgb
!------------------------------------------------------------
!    print *,'in gfsio, before getgbm,mbuf=',gfile%mbuf,&
!     'nlen=',gfile%nlen,'nnum=',gfile%nnum,'mnum=',gfile%mnum 
    call getgbm(gfile%flunit,luidx,grbmeta%jf,n,grbmeta%jpds,grbmeta%jgds,&
      gfile%mbuf,gfile%cbuf,gfile%nlen,gfile%nnum,gfile%mnum, &
      kf,k,kpds,kgds,lbms,data,ios)
    deallocate(lbms,grbmeta%lbms)
    if(ios.ne.0) then
       if ( present(iret))  then
         print *,'getgb_ios=',ios
         return
       else
         call gfsio_stop
       endif
    endif
    if (present(iret)) iret=0
  end subroutine gfsio_readrecw34
!------------------------------------------------------------------------------
  subroutine gfsio_readrec4(gfile,jrec,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read gfsio data by record number into a 2d 32 bits array, 
!           using w3_d library to compile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(gfsio_gfile),intent(in)                 :: gfile
    integer(gfsio_intkind),intent(in)            :: jrec
    real(gfsio_realkind),intent(out)             :: data(gfile%latb*gfile%lonb)
    integer(gfsio_intkind),optional,intent(out)  :: iret
    real(gfsio_dblekind)        :: data8(gfile%latb*gfile%lonb)
    type(gfsio_grbmeta)         :: grbmeta
    integer(gfsio_intkind)      :: luidx
    integer(gfsio_intkind)      :: kf,k,kpds(200),kgds(200)
    logical*1,allocatable       :: lbms(:)
    integer(gfsio_intkind)      :: n=gfsio_kpds_intfill
    integer(gfsio_intkind)      :: ios,i
!------------------------------------------------------------
! set up grib meta 
!------------------------------------------------------------
    luidx=0
    if ( present(iret)) iret=-4
    call gfsio_setrqst(gfile,grbmeta,ios,jrec=jrec)
    if (ios.ne.0) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call gfsio_stop
       endif
    endif
!------------------------------------------------------------
! get data from getgb _w3d
!------------------------------------------------------------
    data8=data
    allocate(lbms(grbmeta%jf))
    n=0
    call getgbm(gfile%flunit,luidx,grbmeta%jf,n,grbmeta%jpds,grbmeta%jgds,&
      gfile%mbuf,gfile%cbuf,gfile%nlen,gfile%nnum,gfile%mnum, &
      kf,k,kpds,kgds,lbms,data8,ios)
    data=data8
    deallocate(lbms,grbmeta%lbms)
    if(ios.ne.0) then
       if ( present(iret))  then
         print *,'getgb_ios=',ios
         return
       else
         call gfsio_stop
       endif
    endif
    if (present(iret)) iret=0
  end subroutine gfsio_readrec4
!------------------------------------------------------------------------------
  subroutine gfsio_readrec8(gfile,jrec,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read gfsio data by record number into a 2d 64 bits array, 
!           using w3_d library to compile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(gfsio_gfile),intent(in)                 :: gfile
    integer(gfsio_intkind),intent(in)            :: jrec
    real(gfsio_dblekind),intent(out)             :: data(gfile%latb*gfile%lonb)
    integer(gfsio_intkind),optional,intent(out)  :: iret
    type(gfsio_grbmeta)         :: grbmeta
    integer(gfsio_intkind)      :: luidx
    integer(gfsio_intkind)      :: kf,k,kpds(200),kgds(200)
    logical*1,allocatable       :: lbms(:)
    integer(gfsio_intkind)      :: n=gfsio_kpds_intfill
    integer(gfsio_intkind)      :: ios,i
!------------------------------------------------------------
! set up grib meta 
!------------------------------------------------------------
    luidx=0
    if ( present(iret)) iret=-4
    call gfsio_setrqst(gfile,grbmeta,ios,jrec=jrec)
    if (ios.ne.0) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call gfsio_stop
       endif
    endif
!------------------------------------------------------------
! get data from getgb _w3d
!------------------------------------------------------------
    allocate(lbms(grbmeta%jf))
    n=0
    call getgbm(gfile%flunit,luidx,grbmeta%jf,n,grbmeta%jpds,grbmeta%jgds,&
      gfile%mbuf,gfile%cbuf,gfile%nlen,gfile%nnum,gfile%mnum, &
      kf,k,kpds,kgds,lbms,data,ios)
    deallocate(lbms,grbmeta%lbms)
    if(ios.ne.0) then
       if ( present(iret))  then
         print *,'getgb_ios=',ios
         return
       else
         call gfsio_stop
       endif
    endif
    if (present(iret)) iret=0
  end subroutine gfsio_readrec8
!------------------------------------------------------------------------------
  subroutine gfsio_readrecvw34(gfile,vname,vlevtyp,vlev,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read gfsio data by field name into 32 bits array, 
!           using w3_4 library to compile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(gfsio_gfile),intent(in)                 :: gfile
    character*(*),intent(in)                     :: vname,vlevtyp
    integer(gfsio_intkind),intent(in)            :: vlev
    real(gfsio_realkind),intent(out)             :: data(gfile%latb*gfile%lonb)
    integer(gfsio_intkind),optional,intent(out)  :: iret
    type(gfsio_grbmeta)         :: grbmeta
    integer(gfsio_intkind)      :: luidx
    integer(gfsio_intkind)      :: kf,k,kpds(200),kgds(200)
    logical*1,allocatable       :: lbms(:)
    integer(gfsio_intkind)      :: n=gfsio_kpds_intfill
    integer(gfsio_intkind)      :: ios,i,w34
!------------------------------------------------------------
! set up grib meta 
!------------------------------------------------------------
    luidx=0
    if ( present(iret)) iret=-4
    w34=1
    call gfsio_setrqst(gfile,grbmeta,ios,vname=vname, &
      vlevtyp=vlevtyp, vlev=vlev ,w34=w34)
    if (ios.ne.0) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call gfsio_stop
       endif
    endif
!------------------------------------------------------------
! get data from getgb _w34
!------------------------------------------------------------
    allocate(lbms(grbmeta%jf))
    n=0
    call getgbm(gfile%flunit,luidx,grbmeta%jf,n,grbmeta%jpds,grbmeta%jgds,&
      gfile%mbuf,gfile%cbuf,gfile%nlen,gfile%nnum,gfile%mnum, &
      kf,k,kpds,kgds,lbms,data,ios)
    deallocate(lbms,grbmeta%lbms)
    if(ios.ne.0) then
       if ( present(iret))  then
          print *,'getgb_ios=',ios
         return
       else
         call gfsio_stop
       endif
    endif
    if ( present(iret)) iret=0
  end subroutine gfsio_readrecvw34
!------------------------------------------------------------------------------
  subroutine gfsio_readrecv4(gfile,vname,vlevtyp,vlev,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read gfsio data by field name into a 2d 32bits array, 
!           using w3_d library to compile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(gfsio_gfile),intent(in)                 :: gfile
    character*(*),intent(in)                     :: vname,vlevtyp
    integer(gfsio_intkind),intent(in)            :: vlev
    real(gfsio_realkind),intent(out)             :: data(gfile%latb*gfile%lonb)
    integer(gfsio_intkind),optional,intent(out)  :: iret
    real(gfsio_dblekind)        :: data8(gfile%latb*gfile%lonb)
    type(gfsio_grbmeta)         :: grbmeta
    integer(gfsio_intkind)      :: luidx
    integer(gfsio_intkind)      :: kf,k,kpds(200),kgds(200)
    logical*1,allocatable       :: lbms(:)
    integer(gfsio_intkind)      :: n=gfsio_kpds_intfill
    integer(gfsio_intkind)      :: ios,i
!------------------------------------------------------------
! set up grib meta 
!------------------------------------------------------------
    luidx=0
    if ( present(iret)) iret=-4
    call gfsio_setrqst(gfile,grbmeta,ios,vname=vname, &
      vlevtyp=vlevtyp, vlev=vlev )
    if (ios.ne.0) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call gfsio_stop
       endif
    endif
!------------------------------------------------------------
! get data from getgb _w3d
!------------------------------------------------------------
    data8=data
    allocate(lbms(grbmeta%jf))
    n=0
    call getgbm(gfile%flunit,luidx,grbmeta%jf,n,grbmeta%jpds,grbmeta%jgds,&
      gfile%mbuf,gfile%cbuf,gfile%nlen,gfile%nnum,gfile%mnum, &
      kf,k,kpds,kgds,lbms,data8,ios)
    data=data8
    deallocate(lbms,grbmeta%lbms)
    if(ios.ne.0) then
       if ( present(iret))  then
          print *,'getgb_ios=',ios
         return
       else
         call gfsio_stop
       endif
    endif
    if ( present(iret)) iret=0
  end subroutine gfsio_readrecv4
!------------------------------------------------------------------------------
  subroutine gfsio_readrecv8(gfile,vname,vlevtyp,vlev,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read gfsio data by field name into a 2d 64bits array, 
!           using w3_d library to compile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(gfsio_gfile),intent(in)                 :: gfile
    character*(*),intent(in)                     :: vname,vlevtyp
    integer(gfsio_intkind),intent(in)            :: vlev
    real(gfsio_dblekind),intent(out)             :: data(gfile%latb*gfile%lonb)
    integer(gfsio_intkind),optional,intent(out)  :: iret
    type(gfsio_grbmeta)         :: grbmeta
    integer(gfsio_intkind)      :: luidx
    integer(gfsio_intkind)      :: kf,k,kpds(200),kgds(200)
    logical*1,allocatable       :: lbms(:)
    integer(gfsio_intkind)      :: n=gfsio_kpds_intfill
    integer(gfsio_intkind)      :: ios,i
!------------------------------------------------------------
! set up grib meta 
!------------------------------------------------------------
    luidx=0
    if ( present(iret)) iret=-4
    call gfsio_setrqst(gfile,grbmeta,ios,vname=vname, &
      vlevtyp=vlevtyp, vlev=vlev )
    if (ios.ne.0) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call gfsio_stop
       endif
    endif
!------------------------------------------------------------
! get data from getgb _w3d
!------------------------------------------------------------
    allocate(lbms(grbmeta%jf))
    n=0
    call getgbm(gfile%flunit,luidx,grbmeta%jf,n,grbmeta%jpds,grbmeta%jgds,&
      gfile%mbuf,gfile%cbuf,gfile%nlen,gfile%nnum,gfile%mnum, &
      kf,k,kpds,kgds,lbms,data,ios)
    deallocate(lbms,grbmeta%lbms)
    if(ios.ne.0) then
       if ( present(iret))  then
          print *,'getgb_ios=',ios
         return
       else
         call gfsio_stop
       endif
    endif
    if ( present(iret)) iret=0
  end subroutine gfsio_readrecv8
!------------------------------------------------------------------------------
  subroutine gfsio_writerecw34(gfile,jrec,data,iret,idrt,precision)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read gfsio data by record number into a 2d 32bits array, 
!           using w3_4 library to compile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(gfsio_gfile),intent(in)               :: gfile
    integer(gfsio_intkind),intent(in)          :: jrec
    real(gfsio_realkind),intent(in)            :: data(gfile%latb*gfile%lonb)
    integer(gfsio_intkind),optional,intent(out):: iret
    integer(gfsio_intkind),optional,intent(in) :: idrt
    integer(gfsio_intkind),optional,intent(in) :: precision
    type(gfsio_grbmeta)         :: grbmeta
    integer(gfsio_intkind)      :: n=gfsio_kpds_intfill
    integer(gfsio_intkind)      :: i
    integer(gfsio_intkind)      :: ios,w34
    integer(gfsio_intkind)      :: kens(200),ibs,nbits
!---
    real(gfsio_realkind)      :: max,min
!------------------------------------------------------------
! set up grib meta 
!------------------------------------------------------------
    if(present(iret)) iret=-4
    w34=1
    if(present(idrt)) then
      call gfsio_setrqst(gfile,grbmeta,ios,jrec=jrec,w34=w34,idrt=idrt,precision=precision)
    else
      call gfsio_setrqst(gfile,grbmeta,ios,jrec=jrec,w34=w34,precision=precision)
    endif
    if (ios.ne.0) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call gfsio_stop
       endif
    endif
!------------------------------------------------------------
! check precision -- for pressure now
!------------------------------------------------------------
    max=data(1)
    do i=1,gfile%latb*gfile%lonb
     if(data(i) .gt.max) max=data(i)
    enddo
    if ( grbmeta%jpds(5).eq.1 .and. grbmeta%jpds(6).eq.109 ) then
     grbmeta%jpds(22)=min(int(5-log10(max)),2)
    endif
!------------------------------------------------------------
! get data from putgb _w34
!------------------------------------------------------------
!call putgben instead of getgb for oytgben has maxbits set to 
!24, grads has issues with number bits >24
    kens=0;ibs=0;nbits=0
    call putgben(gfile%flunit,grbmeta%jf,grbmeta%jpds,grbmeta%jgds, &
      kens,ibs,nbits,grbmeta%lbms,data,ios)
    deallocate(grbmeta%lbms)
    if(ios.ne.0) then
       if ( present(iret))  then
          print *,'putgben_ios=',ios
         iret=ios
         return
       else
         call gfsio_stop
       endif
    endif
    if(present(iret)) iret=0
  end subroutine gfsio_writerecw34
!------------------------------------------------------------------------------
  subroutine gfsio_writerec4(gfile,jrec,data,iret,idrt,precision)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read gfsio data by record number into a 2d 32bits array, 
!           using w3_d library to compile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(gfsio_gfile),intent(in)               :: gfile
    integer(gfsio_intkind),intent(in)          :: jrec
    real(gfsio_realkind),intent(in)            :: data(gfile%latb*gfile%lonb)
    integer(gfsio_intkind),optional,intent(out):: iret
    integer(gfsio_intkind),optional,intent(in) :: idrt
    integer(gfsio_intkind),optional,intent(in) :: precision
    real(gfsio_dblekind)        :: data8(gfile%latb*gfile%lonb)
    type(gfsio_grbmeta)         :: grbmeta
    integer(gfsio_intkind)      :: n=gfsio_kpds_intfill
    integer(gfsio_intkind)      :: i
    integer(gfsio_intkind)      :: ios
    real(gfsio_intkind)         :: max
    integer(gfsio_intkind)      :: kens(200),ibs,nbits
!------------------------------------------------------------
! set up grib meta 
!------------------------------------------------------------
    if(present(iret)) iret=-4
    if(present(idrt)) then
      call gfsio_setrqst(gfile,grbmeta,ios,jrec=jrec,idrt=idrt,precision=precision)
    else
      call gfsio_setrqst(gfile,grbmeta,ios,jrec=jrec,precision=precision)
    endif
    if (ios.ne.0) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call gfsio_stop
       endif
    endif
!------------------------------------------------------------
! check precision -- for pressure now
!------------------------------------------------------------
    max=data(1)
    do i=1,gfile%latb*gfile%lonb
     if(data(i) .gt.max) max=data(i)
    enddo
    if ( grbmeta%jpds(5).eq.1 .and. grbmeta%jpds(6).eq.109 ) then
     grbmeta%jpds(22)=min(int(5-log10(max)),2)
    endif
!------------------------------------------------------------
! get data from putgb _w3d
!------------------------------------------------------------
    data8=data
!call putgben instead of getgb for oytgben has maxbits set to 
!24, grads has issues with number bits >24
    kens=0;ibs=0;nbits=0
    call putgben(gfile%flunit,grbmeta%jf,grbmeta%jpds,grbmeta%jgds, &
      kens,ibs,nbits,grbmeta%lbms,data8,ios)
    deallocate(grbmeta%lbms)
    if(ios.ne.0) then
       if ( present(iret))  then
          print *,'putgben_ios=',ios
         iret=ios
         return
       else
         call gfsio_stop
       endif
    endif
    if(present(iret)) iret=0
  end subroutine gfsio_writerec4
!------------------------------------------------------------------------------
  subroutine gfsio_writerec8(gfile,jrec,data8,iret,idrt,precision)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read gfsio data by record number into a 2d 64bits array, 
!           using w3_d library to compile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(gfsio_gfile),intent(in)               :: gfile
    integer(gfsio_intkind),intent(in)          :: jrec
    real(gfsio_dblekind),intent(in)            :: data8(gfile%latb*gfile%lonb)
    integer(gfsio_intkind),optional,intent(out):: iret
    integer(gfsio_intkind),optional,intent(in) :: idrt
    integer(gfsio_intkind),optional,intent(in) :: precision
    type(gfsio_grbmeta)         :: grbmeta
    integer(gfsio_intkind)      :: n=gfsio_kpds_intfill
    integer(gfsio_intkind)      :: i
    integer(gfsio_intkind)      :: ios
    integer(gfsio_intkind)      :: kens(200),ibs,nbits
!---
    real(gfsio_realkind)      :: max,min
!------------------------------------------------------------
! set up grib meta 
!------------------------------------------------------------
    if(present(iret)) iret=-4
    if(present(idrt)) then
      call gfsio_setrqst(gfile,grbmeta,ios,jrec=jrec,idrt=idrt,precision=precision)
    else
      call gfsio_setrqst(gfile,grbmeta,ios,jrec=jrec,precision=precision)
    endif
    if (ios.ne.0) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call gfsio_stop
       endif
    endif
!------------------------------------------------------------
! check precision -- for pressure now
!------------------------------------------------------------
    max=data8(1)
    do i=1,gfile%latb*gfile%lonb
     if(data8(i) .gt.max) max=data8(i)
    enddo
    if ( grbmeta%jpds(5).eq.1 .and. grbmeta%jpds(6).eq.109 ) then
     grbmeta%jpds(22)=min(int(5-log10(max)),2)
    endif
!------------------------------------------------------------
! get data from putgb _w3d
!------------------------------------------------------------
!call putgben instead of getgb for oytgben has maxbits set to 
!24, grads has issues with number bits >24
    kens=0;ibs=0;nbits=0
    call putgben(gfile%flunit,grbmeta%jf,grbmeta%jpds,grbmeta%jgds, &
      kens,ibs,nbits,grbmeta%lbms,data8,ios)
    deallocate(grbmeta%lbms)
    if(ios.ne.0) then
       if ( present(iret))  then
          print *,'putgben_ios=',ios
         iret=ios
         return
       else
         call gfsio_stop
       endif
    endif
    if(present(iret)) iret=0
  end subroutine gfsio_writerec8
!------------------------------------------------------------------------------
  subroutine gfsio_writerecvw34(gfile,vname,vlevtyp,vlev,data,iret,idrt,precision)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read gfsio data by field name into a 2d 32bits array, 
!           using w3_4 library to compile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(gfsio_gfile),intent(in)               :: gfile
    character*(*),intent(in)                   :: vname,vlevtyp
    integer(gfsio_intkind),intent(in)          :: vlev
    real(gfsio_realkind),intent(in)            :: data(gfile%latb*gfile%lonb)
    integer(gfsio_intkind),optional,intent(out):: iret
    integer(gfsio_intkind),optional,intent(in) :: idrt
    integer(gfsio_intkind),optional,intent(in) :: precision
    type(gfsio_grbmeta)         :: grbmeta
    integer(gfsio_intkind)      :: n=gfsio_kpds_intfill
    integer(gfsio_intkind)      :: i
    integer(gfsio_intkind)      :: ios,w34
    integer(gfsio_intkind)      :: kens(200),ibs,nbits
    real(gfsio_realkind)        :: max
!------------------------------------------------------------
! set up grib meta 
!------------------------------------------------------------
    if(present(iret)) iret=-4
    w34=1
    if(present(idrt)) then
      call gfsio_setrqst(gfile,grbmeta,ios,vname=vname, &
        vlevtyp=vlevtyp, vlev=vlev, w34=w34, idrt=idrt,precision=precision)
    else
      call gfsio_setrqst(gfile,grbmeta,ios,vname=vname, &
        vlevtyp=vlevtyp, vlev=vlev, w34=w34,precision=precision)
    endif
    if (ios.ne.0) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call gfsio_stop
       endif
    endif
!------------------------------------------------------------
! check precision -- for pressure now
!------------------------------------------------------------
    max=data(1)
    do i=1,gfile%latb*gfile%lonb
     if(data(i) .gt.max) max=data(i)
    enddo
    if ( grbmeta%jpds(5).eq.1 .and. grbmeta%jpds(6).eq.109 ) then
     grbmeta%jpds(22)=min(int(5-log10(max)),2)
    endif
!------------------------------------------------------------
! get data from putgb _w34
!------------------------------------------------------------
!call putgben instead of getgb for oytgben has maxbits set to 
!24, grads has issues with number bits >24
    kens=0;ibs=0;nbits=0
    call putgben(gfile%flunit,grbmeta%jf,grbmeta%jpds,grbmeta%jgds, &
      kens,ibs,nbits,grbmeta%lbms,data,ios)
    deallocate(grbmeta%lbms)
    if(ios.ne.0) then
       if ( present(iret))  then
          print *,'putgben_ios=',ios
         iret=ios
         return
       else
         call gfsio_stop
       endif
    endif
    if(present(iret)) iret=0
  end subroutine gfsio_writerecvw34
!------------------------------------------------------------------------------
  subroutine gfsio_writerecv4(gfile,vname,vlevtyp,vlev,data,iret,idrt,precision)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read gfsio data by field name into a 2d 32bits array, 
!           using w3_d library to compile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(gfsio_gfile),intent(in)               :: gfile
    character*(*),intent(in)                   :: vname,vlevtyp
    integer(gfsio_intkind),intent(in)          :: vlev
    real(gfsio_realkind),intent(in)            :: data(gfile%latb*gfile%lonb)
    integer(gfsio_intkind),optional,intent(out):: iret
    integer(gfsio_intkind),optional,intent(in) :: idrt
    integer(gfsio_intkind),optional,intent(in) :: precision
    real(gfsio_dblekind)        :: data8(gfile%latb*gfile%lonb)
    type(gfsio_grbmeta)         :: grbmeta
    integer(gfsio_intkind)      :: n=gfsio_kpds_intfill
    integer(gfsio_intkind)      :: i
    integer(gfsio_intkind)      :: ios
    integer(gfsio_intkind)      :: kens(200),ibs,nbits
    real(gfsio_realkind)        :: max
!------------------------------------------------------------
! set up grib meta 
!------------------------------------------------------------
    if(present(iret)) iret=-4
    if(present(idrt)) then
      call gfsio_setrqst(gfile,grbmeta,ios,vname=vname, &
        vlevtyp=vlevtyp, vlev=vlev, idrt=idrt,precision=precision)
    else
      call gfsio_setrqst(gfile,grbmeta,ios,vname=vname, &
        vlevtyp=vlevtyp, vlev=vlev,precision=precision)
    endif
    if (ios.ne.0) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call gfsio_stop
       endif
    endif
!------------------------------------------------------------
! check precision -- for pressure now
!------------------------------------------------------------
    max=data(1)
    do i=1,gfile%latb*gfile%lonb
     if(data(i) .gt.max) max=data(i)
    enddo
    if ( grbmeta%jpds(5).eq.1 .and. grbmeta%jpds(6).eq.109 ) then
     grbmeta%jpds(22)=min(int(5-log10(max)),2)
    endif
!------------------------------------------------------------
! get data from putgb _w3d
!------------------------------------------------------------
    data8=data
!call putgben instead of getgb for oytgben has maxbits set to 
!24, grads has issues with number bits >24
    kens=0;ibs=0;nbits=0
    call putgben(gfile%flunit,grbmeta%jf,grbmeta%jpds,grbmeta%jgds, &
      kens,ibs,nbits,grbmeta%lbms,data8,ios)
    deallocate(grbmeta%lbms)
    if(ios.ne.0) then
       if ( present(iret))  then
          print *,'putgben_ios=',ios
         iret=ios
         return
       else
         call gfsio_stop
       endif
    endif
    if(present(iret)) iret=0
  end subroutine gfsio_writerecv4
!------------------------------------------------------------------------------
  subroutine gfsio_writerecv8(gfile,vname,vlevtyp,vlev,data8,iret,idrt,precision)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read gfsio data by field name into a 2d 64bits array, 
!           using w3_d library to compile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(gfsio_gfile),intent(in)               :: gfile
    character*(*),intent(in)                   :: vname,vlevtyp
    integer(gfsio_intkind),intent(in)          :: vlev
    real(gfsio_dblekind),intent(in)            :: data8(gfile%latb*gfile%lonb)
    integer(gfsio_intkind),optional,intent(out):: iret
    integer(gfsio_intkind),optional,intent(in) :: idrt
    integer(gfsio_intkind),optional,intent(in) :: precision
    type(gfsio_grbmeta)         :: grbmeta
    integer(gfsio_intkind)      :: n=gfsio_kpds_intfill
    integer(gfsio_intkind)      :: i
    integer(gfsio_intkind)      :: ios
    integer(gfsio_intkind)      :: kens(200),ibs,nbits
    real(gfsio_realkind)        :: max
!------------------------------------------------------------
! set up grib meta 
!------------------------------------------------------------
    if(present(iret)) iret=-4
    if(present(idrt)) then
      call gfsio_setrqst(gfile,grbmeta,ios,vname=vname, &
        vlevtyp=vlevtyp, vlev=vlev, idrt=idrt, precision=precision)
    else
      call gfsio_setrqst(gfile,grbmeta,ios,vname=vname, &
        vlevtyp=vlevtyp, vlev=vlev, precision=precision)
    endif
    if (ios.ne.0) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call gfsio_stop
       endif
    endif
!------------------------------------------------------------
! check precision -- for pressure now
!------------------------------------------------------------
    max=data8(1)
    do i=1,gfile%latb*gfile%lonb
     if(data8(i) .gt.max) max=data8(i)
    enddo
    if ( grbmeta%jpds(5).eq.1 .and. grbmeta%jpds(6).eq.109 ) then
     grbmeta%jpds(22)=min(int(5-log10(max)),2)
    endif
!------------------------------------------------------------
! get data from putgb _w3d
!------------------------------------------------------------
!call putgben instead of getgb for oytgben has maxbits set to 
!24, grads has issues with number bits >24
    kens=0;ibs=0;nbits=0
    call putgben(gfile%flunit,grbmeta%jf,grbmeta%jpds,grbmeta%jgds, &
      kens,ibs,nbits,grbmeta%lbms,data8,ios)
    deallocate(grbmeta%lbms)
    if(ios.ne.0) then
       if ( present(iret))  then
          print *,'putgben_ios=',ios
         iret=ios
         return
       else
         call gfsio_stop
       endif
    endif
    if(present(iret)) iret=0
  end subroutine gfsio_writerecv8
!----------------------------------------------------------------------------
  subroutine gfsio_setrqst(gfile,grbmeta,iret,jrec,vname,vlevtyp,vlev,w34,idrt,precision)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: if given record number, find record name, lev typ, and levs or
!           record name,lev type and lev can be got from argument list.
!           with record name,lev typ and level, set up grib meta, jpds and
!           jgds
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(gfsio_gfile),intent(in)                :: gfile
    type(gfsio_grbmeta),intent(out)             :: grbmeta
    integer(gfsio_intkind),optional,intent(in)  :: jrec
    character*(*),optional,intent(in)           :: vname,vlevtyp
    integer(gfsio_intkind),optional,intent(in)  :: vlev
    integer(gfsio_intkind),intent(out)          :: iret
    integer(gfsio_intkind),optional,intent(in)  :: w34
    integer(gfsio_intkind),optional,intent(in)  :: idrt
    integer(gfsio_intkind),optional,intent(in)  :: precision
    character(255) :: name,levtyp
    integer :: icen,igrid,iptv,itl,ibms,iftu,ip2,itr,ina,inm,ios
    integer :: i,il1,il2,lev,krec,idrt_in
!------------------------------------------------------------
! with record number, find record name, level type and level
!------------------------------------------------------------
    iret=-5
    if ( present(jrec)) then
      if ( jrec.gt.0 .and. jrec.le.gfile%nrec) then
        name=gfile%recname(jrec)
        levtyp=gfile%reclevtyp(jrec)
        lev=gfile%reclev(jrec)
      else
        return
      endif
    elseif ( present(vname) .and. present(vlevtyp) .and. present(vlev)) then
      name=trim(vname)
      levtyp=trim(vlevtyp)
      lev=vlev
    else 
       return
    endif
!------------------------------------------------------------
! find index in grib table according to recname and reclevtyp
!------------------------------------------------------------
    call gfsio_grbtbl_search(trim(name),trim(levtyp),krec,ios)
    if(ios.ne.0) return
!*** lev: for sfc
    if ( gribtable(krec)%leveltype .eq.'sfc' ) then
        lev=0
    endif
!------------------------------------------------------------
! for read, just need to set up jpds(05-07)
!------------------------------------------------------------
!--- read:set jpds5,6,7
    if ( gfile%gaction .eq."read".or. gfile%gaction .eq."read") then
      grbmeta%jpds(05)=gribtable(krec)%g1param
      grbmeta%jpds(06)=gribtable(krec)%g1level
      grbmeta%jpds(07)=lev
      if ( grbmeta%jpds(06).eq.110 ) then
        grbmeta%jpds(07)=256*(lev-1)+lev
      endif
    else
!------------------------------------------------------------
! for write, need to set up jgds(1:25), jpds(01-20)
!------------------------------------------------------------
      if (present(precision)) then
        gribtable(krec)%precision=precision
      endif
      if (present(idrt)) then
        idrt_in = idrt
      else
!*** gfile idrt
        idrt_in=gfile%idrt
      endif
      icen=7
      if ( present(w34) ) then
        call gfsio_makglgds(gfile,idrt_in,igrid,grbmeta%jgds,ios,w34)
      else
        call gfsio_makglgds(gfile,idrt_in,igrid,grbmeta%jgds,ios)
      endif
      if(ios.ne.0) return
      iptv=2
      itl=1
      il1=0
      il2=0
      ibms=0
      iftu=1
      ip2=0
      itr=10
      ina=0
      inm=0
      call gfsio_makglpds(gfile,iptv,icen,igrid,ibms,&
                    iftu,ip2,itr,ina,inm,jrec,krec,lev,grbmeta%jpds,ios)
      if(ios.ne.0) return
    endif
!------------------------------------------------------------
! set up grib meta lbms
!------------------------------------------------------------
    grbmeta%jf=gfile%latb*gfile%lonb
    allocate(grbmeta%lbms(grbmeta%jf))
! ***** for sig 
    grbmeta%lbms=.true.
    iret=0 
  end subroutine gfsio_setrqst    
!------------------------------------------------------------------------------
  subroutine gfsio_getrechead(gfile,jrec,name,levtyp,lev,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: given record number, return users record name, lev typ, and levs
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(gfsio_gfile),intent(in)                :: gfile
    integer(gfsio_intkind),intent(in)           :: jrec
    character*(*),intent(out)                   :: name,levtyp
    integer(gfsio_intkind),intent(out)          :: lev
    integer(gfsio_intkind),optional,intent(out) :: iret
! - - - - - - - - - - - - - -  - - - - - - - -  - - - - - - - - - - - - - - - -
    if( present(iret)) iret=-6
    if ( jrec.gt.0 .or. jrec.le.gfile%nrec) then
      name=gfile%recname(jrec)
      levtyp=gfile%reclevtyp(jrec)
      lev=gfile%reclev(jrec)
      if(present(iret)) iret=0
      return
    else
      if ( present(iret))  then
       return
      else
        call gfsio_stop
      endif
    endif
  end subroutine gfsio_getrechead
!------------------------------------------------------------------------------
  subroutine gfsio_makglgds(gfile,idrt,igrid,kgds,iret,w34)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: set up gds for grib meta
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(gfsio_gfile),intent(in) :: gfile
    integer(gfsio_intkind),intent(out)  :: iret
    integer,intent(in):: idrt
    integer,optional,intent(in):: w34
    integer,intent(out):: igrid,kgds(200)
    real(gfsio_dblekind) :: slat8(gfile%latb)
    real(gfsio_intkind) :: slat4(gfile%latb)
    integer :: n
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=-5
    igrid=255
    if(idrt.eq.0.and.gfile%lonb.eq.144.and.gfile%latb.eq.73) igrid=2
    if(idrt.eq.0.and.gfile%lonb.eq.360.and.gfile%latb.eq.181) igrid=3
    if(idrt.eq.0.and.gfile%lonb.eq.720.and.gfile%latb.eq.361) igrid=4
    if(idrt.eq.4.and.gfile%lonb.eq.192.and.gfile%latb.eq.94) igrid=98
    if(idrt.eq.4.and.gfile%lonb.eq.384.and.gfile%latb.eq.192) igrid=126
    if(idrt.eq.4.and.gfile%lonb.eq.512.and.gfile%latb.eq.256) igrid=170
    if(idrt.eq.4.and.gfile%lonb.eq.768.and.gfile%latb.eq.384) igrid=127
    kgds(1)=modulo(idrt,256)
    kgds(2)=gfile%lonb
    kgds(3)=gfile%latb
    select case(idrt)
    case(0)
      kgds(4)=90000
    case(4)
!------------------------------------------------------------
! call different split for w3_4 lib and w3_d lib
!------------------------------------------------------------
      if (present (w34)) then
        call splat(idrt,gfile%latb,slat4)
        kgds(4)=nint(180000./acos(-1.)*asin(slat4(1)))
      else
        call splat(idrt,gfile%latb,slat8)
        kgds(4)=nint(180000./acos(-1.)*asin(slat8(1)))
      endif
    case(256)
      kgds(4)=90000-nint(0.5*180000./gfile%latb)
    end select
    kgds(5)=0
    kgds(6)=128
    kgds(7)=-kgds(4)
    kgds(8)=-nint(360000./gfile%lonb)
    kgds(9)=-kgds(8)
    select case(idrt)
    case(0)
      kgds(10)=nint(180000./(gfile%latb-1))
    case(4)
      kgds(10)=gfile%latb/2
    case(256)
      kgds(10)=nint(180000./gfile%latb)
    end select
    kgds(11)=0
    kgds(12)=0
    kgds(13:18)=-1
    kgds(19)=0
    kgds(20)=255
    kgds(21:)=-1
    iret=0
  end subroutine gfsio_makglgds
!------------------------------------------------------------------------------
  subroutine gfsio_makglpds(gfile,iptv,icen,igrid,ibms,&
                    iftu,ip2,itr,ina,inm,jrec,krec,lev,kpds,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: set up gps for grib meta
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(gfsio_gfile),intent(in)  :: gfile
    integer,intent(in):: iptv,icen,igrid,ibms,&
          iftu,ip2,itr,ina,inm,jrec,krec,lev
   integer,intent(out):: kpds(200)
    integer(gfsio_intkind),intent(out)  :: iret
   integer :: i
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=-5
    kpds(01)=icen
    kpds(02)=gfile%igen
    kpds(03)=igrid
    kpds(04)=128+64*ibms
    kpds(05)=gribtable(krec)%g1param
    kpds(06)=gribtable(krec)%g1level
    kpds(07)=lev
!*** deal with dpres 
    if ( kpds(06).eq.110 ) then
    kpds(07)=256*(lev-1)+lev
    endif
!***
    kpds(08)=mod(gfile%idate(4)-1,100)+1
    kpds(09)=gfile%idate(2)
    kpds(10)=gfile%idate(3)
    kpds(11)=gfile%idate(1)
    kpds(12)=0
    kpds(13)=iftu
    kpds(14)=gfile%fhour/3600
    kpds(15)=ip2
    kpds(16)=itr
    kpds(17)=ina
    kpds(18)=1
    kpds(19)=iptv
    kpds(20)=inm
    kpds(21)=(gfile%idate(4)-1)/100+1
    kpds(22)=gribtable(krec)%precision
    kpds(23)=gfile%icen2
    kpds(24)=0
    kpds(25)=0
    kpds(26:)=-1
    iret=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine gfsio_makglpds
!------------------------------------------------------------------------------
  subroutine gfsio_grbtbl_search(vname,vlevtyp,krec,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: given record name, levtyp and index number in grib table
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    character*(*),intent(in)   :: vname,vlevtyp
    integer(gfsio_intkind),intent(out)   :: krec
    integer(gfsio_intkind),intent(out)   :: iret
    integer  :: i
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=-6
    krec=0
    do i=1,size(gribtable)
      if(trim(vname).eq.trim(gribtable(i)%shortname) .and. &
        trim(vlevtyp).eq.trim(gribtable(i)%leveltype) )then
        krec=i
        iret=0
        exit
      endif
    enddo 
  end subroutine gfsio_grbtbl_search
!------------------------------------------------------------------------------
  subroutine gfsio_chkgfary(gfile,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: check if arrays in gfile is allocated and with right size
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(gfsio_gfile),intent(inout)         :: gfile
    integer(gfsio_intkind),intent(out)   :: iret
    integer   :: ios
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=-2
    if ( gfile%lonb .eq. gfsio_intfill .or. gfile%latb .eq. gfsio_intfill &
      .or. gfile%levs .eq. gfsio_intfill .or. gfile%nrec .eq. gfsio_intfill &
      .or. gfile%idate(1) .eq.gfsio_intfill .or. gfile%ntrac .eq.gfsio_intfill ) then
      return
    else
      if (.not. allocated(gfile%vcoord) .or. size(gfile%vcoord).ne. &
         ((gfile%levs+1)*gfile%nvcoord)) then
         call gfsio_almeta1(gfile,ios)
        if (ios .ne. 0) return
     endif
     if ( .not.allocated(gfile%glat1d) .or. size(gfile%glat1d).ne.gfile%latb) &
       then
        call gfsio_almeta2(gfile,ios)
        if (ios .ne. 0) return
     endif
     if ( .not.allocated(gfile%glon1d) .or. size(gfile%glon1d).ne.gfile%lonb) &
       then
        call gfsio_almeta4(gfile,ios)
        if (ios .ne. 0) return
     endif
     if ( .not.allocated(gfile%cpi) .or. size(gfile%cpi).ne.gfile%ntrac+1) &
       then
        call gfsio_almeta5(gfile,ios)
        if (ios .ne. 0) return
     endif
     if ( .not.allocated(gfile%ri) .or. size(gfile%ri).ne.gfile%ntrac+1) &
       then
        call gfsio_almeta6(gfile,ios)
        if (ios .ne. 0) return
     endif

     if (allocated(gfile%recname) .and. size(gfile%recname).eq.gfile%nrec)&
     then
        if (allocated(gfile%reclevtyp) .and. size(gfile%reclevtyp) &
        .eq.gfile%nrec) then
           if (allocated(gfile%reclev) .and. size(gfile%reclev).eq. &
             gfile%nrec) then
               iret=0
               return
           endif
         endif
     endif
     call  gfsio_almeta3(gfile,ios)
     if (ios .ne. 0) return
     iret=0
    endif
  end subroutine gfsio_chkgfary
!------------------------------------------------------------------------------
  subroutine gfsio_almeta(gfile,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: allocate all the arrays in gfile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(gfsio_gfile),intent(inout)  :: gfile 
    integer(gfsio_intkind),intent(out)  :: iret
    integer ::dimvcoord1,dimvcoord2,dimrecname,dimreclevtyp,dimreclev
    integer ::dimglat1d,dimglon1d
    integer ::dimcp4,dimr
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    dimvcoord1=gfile%levs+1
    dimvcoord2=gfile%nvcoord
    dimrecname=gfile%nrec
    dimreclevtyp=gfile%nrec
    dimreclev=gfile%nrec
    dimglat1d=gfile%latb
    dimglon1d=gfile%lonb
    dimcp4=gfile%ntrac+1
    dimr=gfile%ntrac+1
    if(allocated(gfile%vcoord)) deallocate(gfile%vcoord)
    if(allocated(gfile%recname)) deallocate(gfile%recname)
    if(allocated(gfile%reclevtyp)) deallocate(gfile%reclevtyp)
    if(allocated(gfile%reclev)) deallocate(gfile%reclev)
    if(allocated(gfile%glat1d)) deallocate(gfile%glat1d)
    if(allocated(gfile%glon1d)) deallocate(gfile%glon1d)
    if(allocated(gfile%cpi)) deallocate(gfile%cpi)
    if(allocated(gfile%ri)) deallocate(gfile%ri)
    allocate(gfile%vcoord(dimvcoord1,dimvcoord2), &
             gfile%recname(dimrecname),  gfile%reclevtyp(dimreclevtyp), &
             gfile%reclev(dimreclev), gfile%glat1d(dimglat1d), &
             gfile%glon1d(dimglon1d), &
             gfile%cpi(dimcp4), gfile%ri(dimr), &
             stat=iret)
    if(iret.eq.0) then
      gfile%vcoord=gfsio_realfill
      gfile%reclev=gfsio_realfill
      gfile%recname=' '
      gfile%reclevtyp=' '
      gfile%glat1d=gfsio_realfill
      gfile%glon1d=gfsio_realfill
      gfile%cpi=gfsio_realfill
      gfile%ri=gfsio_realfill
    endif
    if(iret.ne.0) iret=-6
  end subroutine gfsio_almeta
!------------------------------------------------------------------------------
  subroutine gfsio_almeta1(gfile,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: allocate vcoord in gfile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(gfsio_gfile),intent(inout)  :: gfile
    integer(gfsio_intkind),intent(out)  :: iret
    integer :: dimvcoord1,dimvcoord2
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    dimvcoord1=gfile%levs+1
    dimvcoord2=gfile%nvcoord
    if(allocated(gfile%vcoord)) deallocate(gfile%vcoord)
    allocate(gfile%vcoord(dimvcoord1,dimvcoord2),stat=iret)
    if(iret.eq.0) then
      gfile%vcoord=gfsio_realfill
    endif
    if(iret.ne.0) iret=-6
  end subroutine gfsio_almeta1
!------------------------------------------------------------------------------
  subroutine gfsio_almeta2(gfile,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: allocate lat1d in gfile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(gfsio_gfile),intent(inout)  :: gfile
    integer(gfsio_intkind),intent(out)  :: iret
    integer :: dimglat1d
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    dimglat1d=gfile%latb
    if(allocated(gfile%glat1d)) deallocate(gfile%glat1d)
    allocate(gfile%glat1d(dimglat1d),stat=iret)
    if(iret.eq.0) then
      gfile%glat1d=gfsio_realfill
    endif
    if(iret.ne.0) iret=-6
  end subroutine gfsio_almeta2
!------------------------------------------------------------------------------
  subroutine gfsio_almeta4(gfile,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: allocate lon1d in gfile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(gfsio_gfile),intent(inout)  :: gfile
    integer(gfsio_intkind),intent(out)  :: iret
    integer :: dimglon1d
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    dimglon1d=gfile%lonb
    if(allocated(gfile%glon1d)) deallocate(gfile%glon1d)
    allocate(gfile%glon1d(dimglon1d),stat=iret)
    if(iret.eq.0) then
      gfile%glon1d=gfsio_realfill
    endif
    if(iret.ne.0) iret=-6
  end subroutine gfsio_almeta4
!------------------------------------------------------------------------------
  subroutine gfsio_almeta5(gfile,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: allocate lon1d in gfile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(gfsio_gfile),intent(inout)  :: gfile
    integer(gfsio_intkind),intent(out)  :: iret
    integer :: dim1d
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    dim1d=gfile%ntrac+1
    if(allocated(gfile%cpi)) deallocate(gfile%cpi)
    allocate(gfile%cpi(dim1d),stat=iret)
     if(iret.eq.0) then
      gfile%cpi=gfsio_realfill
    endif
    if(iret.ne.0) iret=-6
  end subroutine gfsio_almeta5
!------------------------------------------------------------------------------
  subroutine gfsio_almeta6(gfile,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: allocate lon1d in gfile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(gfsio_gfile),intent(inout)  :: gfile
    integer(gfsio_intkind),intent(out)  :: iret
    integer :: dim1d
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    dim1d=gfile%ntrac+1
    if(allocated(gfile%ri)) deallocate(gfile%ri)
     allocate(gfile%ri(dim1d),stat=iret)
     if(iret.eq.0) then
        gfile%ri=gfsio_realfill
    endif
    if(iret.ne.0) iret=-6
  end subroutine gfsio_almeta6
!------------------------------------------------------------------------------
  subroutine gfsio_almeta3(gfile,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: allocate recnam, reclvevtyp, and reclev in gfile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(gfsio_gfile),intent(inout)  :: gfile
    integer(gfsio_intkind),intent(out)  :: iret
    integer :: dimrecname,dimreclevtyp,dimreclev
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    dimrecname=gfile%nrec
    dimreclevtyp=gfile%nrec
    dimreclev=gfile%nrec
    if(allocated(gfile%recname)) deallocate(gfile%recname)
    if(allocated(gfile%reclevtyp)) deallocate(gfile%reclevtyp)
    if(allocated(gfile%reclev)) deallocate(gfile%reclev)
    allocate(gfile%recname(dimrecname),  gfile%reclevtyp(dimreclevtyp), &
             gfile%reclev(dimreclev), stat=iret)
    if(iret.eq.0) then
      gfile%reclev=gfsio_intfill
      gfile%recname=' '
      gfile%reclevtyp=' '
    endif
    if(iret.ne.0) iret=-6
  end subroutine gfsio_almeta3
!------------------------------------------------------------------------------
  subroutine gfsio_axmeta(gfile,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: empty gfile variables and decallocate arrays in gfile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(gfsio_gfile),intent(inout)      :: gfile
    integer(gfsio_intkind),intent(out)  :: iret
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=-6
    gfile%gtype=' '
    gfile%version=gfsio_intfill
    gfile%nmeta=gfsio_intfill
    gfile%lmeta=gfsio_intfill
    gfile%nrec=gfsio_intfill
    gfile%fhour=gfsio_intfill
    gfile%idate(4)=gfsio_intfill
    gfile%latb=gfsio_intfill
    gfile%lonb=gfsio_intfill
    gfile%levs=gfsio_intfill
    gfile%jcap=gfsio_intfill
    gfile%itrun=gfsio_intfill
    gfile%iorder=gfsio_intfill
    gfile%irealf=gfsio_intfill
    gfile%igen=gfsio_intfill
    gfile%latf=gfsio_intfill
    gfile%lonf=gfsio_intfill
    gfile%latr=gfsio_intfill
    gfile%lonr=gfsio_intfill
    gfile%ntrac=gfsio_intfill
    gfile%icen2=gfsio_intfill
    gfile%iens(2)=gfsio_intfill
    gfile%idpp=gfsio_intfill
    gfile%idsl=gfsio_intfill
    gfile%idvc=gfsio_intfill
    gfile%idvm=gfsio_intfill
    gfile%idvt=gfsio_intfill
    gfile%idrun=gfsio_intfill
    gfile%idusr=gfsio_intfill
    gfile%pdryini=gfsio_intfill
    gfile%ncldt=gfsio_intfill
    gfile%ixgr=gfsio_intfill
    gfile%nvcoord=gfsio_intfill
    if(allocated(gfile%vcoord)) deallocate(gfile%recname)
    if(allocated(gfile%recname)) deallocate(gfile%recname)
    if(allocated(gfile%reclevtyp)) deallocate(gfile%reclevtyp)
    if(allocated(gfile%reclev)) deallocate(gfile%reclev)
    if(allocated(gfile%glat1d)) deallocate(gfile%glat1d)
    if(allocated(gfile%glon1d)) deallocate(gfile%glon1d)
    if(allocated(gfile%cpi)) deallocate(gfile%cpi)
    if(allocated(gfile%ri)) deallocate(gfile%ri)
    gfile%mbuf=0
    gfile%nnum=0
    gfile%nlen=0
    gfile%mnum=0
    if(allocated(gfile%cbuf)) deallocate(gfile%cbuf)
    iret=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine gfsio_axmeta
!------------------------------------------------------------------------------
  subroutine gfsio_setgrbtbl(iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: set up grib table
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    integer(gfsio_intkind),intent(out)  :: iret
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=-7
    gribtable(1)=gfsio_grbtbl_item('hgt','sfc',1,0,7,1)
    gribtable(2)=gfsio_grbtbl_item('pres','sfc',0,0,1,1)
    gribtable(3)=gfsio_grbtbl_item('pres','layer',0,0,1,109)
    gribtable(4)=gfsio_grbtbl_item('dpres','layer',2,0,1,110)
    gribtable(5)=gfsio_grbtbl_item('tmp','layer',2,0,11,109)
    gribtable(6)=gfsio_grbtbl_item('ugrd','layer',2,0,33,109)
    gribtable(7)=gfsio_grbtbl_item('vgrd','layer',2,0,34,109)
    gribtable(8)=gfsio_grbtbl_item('spfh','layer',7,0,51,109)
    gribtable(9)=gfsio_grbtbl_item('o3mr','layer',9,0,154,109)
    gribtable(10)=gfsio_grbtbl_item('clwmr','layer',7,0,153,109)
    gribtable(11)=gfsio_grbtbl_item('vvel','layer',6,0,39,109)
    iret=0
  end subroutine gfsio_setgrbtbl
!------------------------------------------------------------------------------
  subroutine gfsio_gfinit(gfile,iret,gtype,version)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: set gfile variables to operational model output
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(gfsio_gfile),intent(inout)     :: gfile
    character*(*),optional,intent(in)            :: gtype
    integer(gfsio_intkind),optional,intent(in)   :: version
    integer(gfsio_intkind),intent(out)  :: iret
    real(gfsio_realkind),allocatable  :: vcoord(:)
    integer  :: i,j,rec
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! set operational format
!
    iret=-8
    gfile%gtype='GFSIOATM'
    gfile%version=200701
    gfile%nmeta=10
    gfile%lmeta=34*gfsio_intkind
    gfile%fhour=0
    gfile%latb=576
    gfile%lonb=1152
    gfile%levs=64
    gfile%nrec=2+9*gfile%levs
    gfile%jcap=382
    gfile%itrun=1
    gfile%iorder=2
    gfile%irealf=1
    gfile%igen=81
    gfile%latf=576
    gfile%lonf=1152
    gfile%latr=576
    gfile%lonr=1152
    gfile%ntrac=3
    gfile%icen2=0
    gfile%iens=(0,0)
    gfile%idpp=0
    gfile%idsl=0
    gfile%idvc=1
    gfile%idvm=0
    gfile%idvt=0
    gfile%idrun=0
    gfile%idusr=0
    gfile%pdryini=9826330
    gfile%ncldt=1
    gfile%ixgr=0
    gfile%idrt=4
    gfile%nvcoord=1
    call gfsio_almeta(gfile,iret)
    if (iret .ne.0) return
    allocate(vcoord(gfile%levs+1))
    vcoord=(/1.0000000,0.99467099,0.98863202,0.98180002,0.97408301, &
      0.96538502,0.95560300,0.94463098,0.93235999,0.91867799,0.90347999, &
      0.88666302,0.86813903,0.84783000,0.82568502,0.80167699,0.77581102, &
      0.74813300,0.71872902,0.68773103,0.65531600,0.62170500,0.58715999, &
      0.55197400,0.51646298,0.48095500,0.44577801,0.41124901,0.37765899, &
      0.34526899,0.31430000,0.28492799,0.25728399,0.23145400,0.20748200, &
      0.18537199,0.16509899,0.14660800,0.12982300,0.11465500,0.10100200, &
      0.88756002e-01,0.77808000e-01,0.68048999e-01,0.59370000e-01, &
      0.51670998e-01,0.44854999e-01,0.38830999e-01,0.33514999e-01, &
      0.28829999e-01,0.24707999e-01,0.21083999e-01,0.17901000e-01, &
      0.15107000e-01,0.12658000e-01,0.10511000e-01,0.86310003e-02, &
      0.69849999e-02,0.55439998e-02,0.42840000e-02,0.31830000e-02, &
      0.22199999e-02,0.13780000e-02,0.64200000e-03,0.0000000 /)
    gfile%vcoord(1:gfile%levs+1,1)=vcoord(1:gfile%levs+1)

   rec=1
   gfile%recname(rec)='hgt'
   gfile%recname(rec+1)='pres'
   gfile%recname(rec+2:rec+gfile%levs+1)='pres'
   gfile%recname(rec+gfile%levs+2:rec+2*gfile%levs+1)='dpres'
   gfile%recname(rec+2*gfile%levs+2:rec+3*gfile%levs+1)='tmp'
   gfile%recname(rec+3*gfile%levs+2:rec+4*gfile%levs+1)='ugrd'
   gfile%recname(rec+4*gfile%levs+2:rec+5*gfile%levs+1)='vgrd'
   gfile%recname(rec+5*gfile%levs+2:rec+6*gfile%levs+1)='spfh'
   gfile%recname(rec+6*gfile%levs+2:rec+7*gfile%levs+1)='o3mr'
   gfile%recname(rec+7*gfile%levs+2:rec+8*gfile%levs+1)='clwmr'
   gfile%recname(rec+7*gfile%levs+2:rec+8*gfile%levs+1)='clwmr'
   gfile%recname(rec+8*gfile%levs+2:rec+9*gfile%levs+1)='vvel'
   gfile%reclevtyp(rec)='sfc'
   gfile%reclevtyp(rec+1)='sfc'
   gfile%reclevtyp(rec+2:rec+gfile%levs+1)='layer'
   gfile%reclevtyp(rec+gfile%levs+2:rec+2*gfile%levs+1)='layer'
   gfile%reclevtyp(rec+2*gfile%levs+2:rec+3*gfile%levs+1)='layer'
   gfile%reclevtyp(rec+3*gfile%levs+2:rec+4*gfile%levs+1)='layer'
   gfile%reclevtyp(rec+4*gfile%levs+2:rec+5*gfile%levs+1)='layer'
   gfile%reclevtyp(rec+5*gfile%levs+2:rec+6*gfile%levs+1)='layer'
   gfile%reclevtyp(rec+6*gfile%levs+2:rec+7*gfile%levs+1)='layer'
   gfile%reclevtyp(rec+7*gfile%levs+2:rec+8*gfile%levs+1)='layer'
   gfile%reclevtyp(rec+8*gfile%levs+2:rec+9*gfile%levs+1)='layer'
   gfile%reclev=1
   rec=2
   do j=3,11
     do i=1,gfile%levs
       gfile%reclev(rec+(j-3)*gfile%levs+i)=i
     enddo
   enddo
   gfile%cpi=1003.
   gfile%ri=287.
   iret=0
  end subroutine gfsio_gfinit
!------------------------------------------------------------------------------
  subroutine gfsio_stop()
    implicit none
     stop
  end subroutine gfsio_stop
!------------------------------------------------------------------------------
!  temporary subroutines for basio file unit
    subroutine gfsio_getlu(gfile,gfname,gaction,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: set unit number to the first number available between 600-699
!           according to unit number array fileunit
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
      implicit none
     type(gfsio_gfile),intent (inout) :: gfile
     character*(*),intent(in)       :: gfname,gaction
     integer,intent(out) :: iret
     integer :: i
     iret=-10
     gfile%gfname=gfname
     gfile%gaction=gaction
     do i=600,699
       if ( fileunit(i) .eq. 0 ) then 
         gfile%flunit=i
         fileunit(i)=i
         iret=0
         exit
       endif
     enddo
    end subroutine gfsio_getlu
!------------------------------------------------------------------------------
!  temporary subroutines for free unit number 
    subroutine gfsio_clslu(gfile,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: free unit number array index corresponding to unit number
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
      implicit none
     type(gfsio_gfile),intent (inout) :: gfile
     integer, intent(out) :: iret
     iret=-10
     if ( fileunit(gfile%flunit) .ne. 0 ) then
       fileunit(gfile%flunit)=0
       gfile%flunit=0
       iret=0
     endif
    end subroutine gfsio_clslu
!----------------------------------------------------------------------
      subroutine gfsio_splat4(idrt,jmax,aslat)
!$$$
      implicit none
      integer(gfsio_intkind),intent(in) :: idrt,jmax
      real(4),intent(out) :: aslat(jmax)
      integer(gfsio_intkind),parameter:: kd=selected_real_kind(15,45)
      real(kind=kd):: pk(jmax/2),pkm1(jmax/2),pkm2(jmax/2)
      real(kind=kd):: aslatd(jmax/2),sp,spmax,eps=10.*epsilon(sp)
      integer,parameter:: jz=50
      real(gfsio_dblekind) bz(jz)
      data bz        / 2.4048255577,  5.5200781103, &
       8.6537279129, 11.7915344391, 14.9309177086, 18.0710639679, &
      21.2116366299, 24.3524715308, 27.4934791320, 30.6346064684, &
      33.7758202136, 36.9170983537, 40.0584257646, 43.1997917132, &
      46.3411883717, 49.4826098974, 52.6240518411, 55.7655107550, &
      58.9069839261, 62.0484691902, 65.1899648002, 68.3314693299, &
      71.4729816036, 74.6145006437, 77.7560256304, 80.8975558711, &
      84.0390907769, 87.1806298436, 90.3221726372, 93.4637187819, &
      96.6052679510, 99.7468198587, 102.888374254, 106.029930916, &
      109.171489649, 112.313050280, 115.454612653, 118.596176630, &
      121.737742088, 124.879308913, 128.020877005, 131.162446275, &
      134.304016638, 137.445588020, 140.587160352, 143.728733573, &
      146.870307625, 150.011882457, 153.153458019, 156.295034268 /
      real(8):: dlt
      integer(4):: jhe,jho
      real,parameter :: pi=3.14159265358979,c=(1.-(2./pi)**2)*0.25
      real r
      integer jh,n,j
!c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!c  gaussian latitudes
!      print *,'gfsio_module,in splat4',idrt,jmax
      if(idrt.eq.4) then
        jh=jmax/2
        jhe=(jmax+1)/2
        r=1./sqrt((jmax+0.5)**2+c)
        do j=1,min(jh,jz)
          aslatd(j)=cos(bz(j)*r)
        enddo
        do j=jz+1,jh
          aslatd(j)=cos((bz(jz)+(j-jz)*pi)*r)
        enddo
        spmax=1.
        do while(spmax.gt.eps)
          spmax=0.
          do j=1,jh
            pkm1(j)=1.
            pk(j)=aslatd(j)
          enddo
          do n=2,jmax
            do j=1,jh
              pkm2(j)=pkm1(j)
              pkm1(j)=pk(j)
              pk(j)=((2*n-1)*aslatd(j)*pkm1(j)-(n-1)*pkm2(j))/n
            enddo
          enddo
          do j=1,jh
            sp=pk(j)*(1.-aslatd(j)**2)/(jmax*(pkm1(j)-aslatd(j)*pk(j)))
            aslatd(j)=aslatd(j)-sp
            spmax=max(spmax,abs(sp))
          enddo
        enddo
!cdir$ ivdep
        do j=1,jh
          aslat(j)=aslatd(j)
          aslat(jmax+1-j)=-aslat(j)
        enddo
        if(jhe.gt.jh) then
          aslat(jhe)=0.
        endif
!c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!c  equally-spaced latitudes including poles
      elseif(idrt.eq.0) then
        jh=jmax/2
        jhe=(jmax+1)/2
        jho=jhe-1
        dlt=pi/(jmax-1)
        aslat(1)=1.
        do j=2,jh
          aslat(j)=cos((j-1)*dlt)
        enddo
!cdir$ ivdep
        do j=1,jh
          aslat(jmax+1-j)=-aslat(j)
        enddo
        if(jhe.gt.jh) then
          aslat(jhe)=0.
        endif
!c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!c  equally-spaced latitudes excluding poles
      elseif(idrt.eq.256) then
        jh=jmax/2
        jhe=(jmax+1)/2
        jho=jhe
        dlt=pi/jmax
        aslat(1)=1.
        do j=1,jh
          aslat(j)=cos((j-0.5)*dlt)
        enddo
!cdir$ ivdep
        do j=1,jh
          aslat(jmax+1-j)=-aslat(j)
        enddo
        if(jhe.gt.jh) then
          aslat(jhe)=0.
        endif
      endif
!c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     end subroutine gfsio_splat4
!----------------------------------------------------------------------
      subroutine gfsio_splat8(idrt,jmax,aslat)
!$$$
      implicit none
      integer(gfsio_intkind),intent(in) :: idrt,jmax
      real(gfsio_dblekind),intent(out) :: aslat(jmax)
      integer(gfsio_intkind),parameter:: kd=selected_real_kind(15,45)
      real(kind=kd):: pk(jmax/2),pkm1(jmax/2),pkm2(jmax/2)
      real(kind=kd):: aslatd(jmax/2),sp,spmax,eps=10.*epsilon(sp)
      integer,parameter:: jz=50
      real(gfsio_dblekind) bz(jz)
      data bz        / 2.4048255577,  5.5200781103, &
       8.6537279129, 11.7915344391, 14.9309177086, 18.0710639679, &
      21.2116366299, 24.3524715308, 27.4934791320, 30.6346064684, &
      33.7758202136, 36.9170983537, 40.0584257646, 43.1997917132, &
      46.3411883717, 49.4826098974, 52.6240518411, 55.7655107550, &
      58.9069839261, 62.0484691902, 65.1899648002, 68.3314693299, &
      71.4729816036, 74.6145006437, 77.7560256304, 80.8975558711, &
      84.0390907769, 87.1806298436, 90.3221726372, 93.4637187819, &
      96.6052679510, 99.7468198587, 102.888374254, 106.029930916, &
      109.171489649, 112.313050280, 115.454612653, 118.596176630, &
      121.737742088, 124.879308913, 128.020877005, 131.162446275, &
      134.304016638, 137.445588020, 140.587160352, 143.728733573, &
      146.870307625, 150.011882457, 153.153458019, 156.295034268 /
      real(8):: dlt
      integer(4):: jhe,jho
      real(gfsio_dblekind),parameter :: pi=3.14159265358979,c=(1.-(2./pi)**2)*0.25
      real(gfsio_dblekind) r
      integer jh,n,j
!c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!c  gaussian latitudes
      if(idrt.eq.4) then
        jh=jmax/2
        jhe=(jmax+1)/2
        r=1.d0/sqrt((jmax+0.5d0)**2+c)
        do j=1,min(jh,jz)
          aslatd(j)=cos(bz(j)*r)
        enddo
        do j=jz+1,jh
          aslatd(j)=cos((bz(jz)+(j-jz)*pi)*r)
        enddo
        spmax=1.d0
        do while(spmax.gt.eps)
          spmax=0.d0
          do j=1,jh
            pkm1(j)=1.d0
            pk(j)=aslatd(j)
          enddo
          do n=2,jmax
            do j=1,jh
              pkm2(j)=pkm1(j)
              pkm1(j)=pk(j)
              pk(j)=((2*n-1)*aslatd(j)*pkm1(j)-(n-1)*pkm2(j))/n
            enddo
          enddo
          do j=1,jh
            sp=pk(j)*(1.d0-aslatd(j)**2)/(jmax*(pkm1(j)-aslatd(j)*pk(j)))
            aslatd(j)=aslatd(j)-sp
            spmax=max(spmax,abs(sp))
          enddo
        enddo
!cdir$ ivdep
        do j=1,jh
          aslat(j)=aslatd(j)
          aslat(jmax+1-j)=-aslat(j)
        enddo
        if(jhe.gt.jh) then
          aslat(jhe)=0.d0
        endif
!c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!c  equally-spaced latitudes including poles
      elseif(idrt.eq.0) then
        jh=jmax/2
        jhe=(jmax+1)/2
        jho=jhe-1
        dlt=pi/(jmax-1)
        aslat(1)=1.d0
        do j=2,jh
          aslat(j)=cos((j-1)*dlt)
        enddo
!cdir$ ivdep
        do j=1,jh
          aslat(jmax+1-j)=-aslat(j)
        enddo
        if(jhe.gt.jh) then
          aslat(jhe)=0.d0
        endif
!c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!c  equally-spaced latitudes excluding poles
      elseif(idrt.eq.256) then
        jh=jmax/2
        jhe=(jmax+1)/2
        jho=jhe
        dlt=pi/jmax
        aslat(1)=1.d0
        do j=1,jh
          aslat(j)=cos((j-0.5d0)*dlt)
        enddo
!cdir$ ivdep
        do j=1,jh
          aslat(jmax+1-j)=-aslat(j)
        enddo
        if(jhe.gt.jh) then
          aslat(jhe)=0.d0
        endif
      endif
!c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     end subroutine gfsio_splat8
!----------------------------------------------------------------------
  end module gfsio_module

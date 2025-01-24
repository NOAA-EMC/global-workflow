!-------------------------------------------------------------------------------
module nstio_module
!$$$  Module Documentation Block
!
! Module:    nstio_module    API for global spectral nst file I/O
!   Prgmmr: Xu Li (modified from sfcio_modul)         Org: w/nx23     date: 2007-10-26
!
! Abstract: This module provides an Application Program Interface
!   for performing I/O on the nst restart file of the global nst diurnal warming and sub-layer cooling models.
!   Functions include opening, reading, writing, and closing as well as
!   allocating and deallocating data buffers sed in the transfers.
!   The I/O performed here is sequential.
!   The transfers are limited to header records or data records.
!   
! Program History Log:
!   2007-10-26  Xu Li
!   2008-03-25  Xu Li: add surface mask field
!   2009-06-30  Xu Li: modified for NCEP DTM-1p
!
! Public Variables:
!   nstio_lhead1      Integer parameter length of first header record (=32)
!   nstio_intkind     Integer parameter kind or length of passed integers (=4)
!   nstio_realkind    Integer parameter kind or length of passed reals (=4)
!   nstio_dblekind    Integer parameter kind or length of passed longreals (=8)
!   nstio_realfill    Real(nstio_realkind) fill value (=-9999.)
!   nstio_dblefill    Real(nstio_dblekind) fill value (=-9999.)
!
! Public Defined Types:
!   nstio_head        nst file header information
!     clabnst           Character(nstio_lhead1) ON85 label
!     fhour             Real(nstio_realkind) forecast hour
!     idate             Integer(nstio_intkind)(4) initial date
!                       (hour, month, day, 4-digit year)
!     latb              Integer(nstio_intkind) latitudes
!     lonb              Integer(nstio_intkind) longitudes
!     ivo               Integer(nstio_intkind) version number
!     lsea              Integer(nstio_intkind) sea levels
!     irealf            Integer(sigio_intkind) floating point flag
!                       (=1 for 4-byte ieee, =2 for 8-byte ieee)
!     lpl               Integer(nstio_intkind)(latb/2) lons per lat
!     zsea              Real(nstio_realkind) sea depths (meter)
!
!   nstio_data        nst file data fields
!     slmsk             Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       surface mask: 0 = water; 1 = land; 2 = ice
!     xt                Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       heat content in DTL                                  (M*K)
!     xs                Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       salinity content in DTL                              (M*ppt)
!     xu                Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       u-current content in DTL                             (M*M/S)
!     xv                Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       v-current content in DTL                             (M*M/S)
!     xz                Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       DTL thickness                                        (M)
!     zm                Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       MXL thickness                                        (M)
!     xtts              Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       d(xt)/d(Ts)                                          (1/M)
!     xzts              Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       d(xz)/d(Ts)                                          (M/K)
!     dt_cool           Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       sea surface cooling amount by sub-layer cooling effect
!     z_c               Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       sea sub-layer depth in m
!     c_0               Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       coefficient to calculate d(Tz)/d(tr) in dimensionless
!     c_d               Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       coefficient to calculate d(Tz)/d(tr) in               (1/M)
!     w_0               Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       coefficient to calculate d(Tz)/d(tr) in dimensionless
!     w_d               Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       coefficient to calculate d(Tz)/d(tr)                  (1/M)
!     d_conv            Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       FCL thickness                                          (M)
!     ifd               Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       index of time integral started mode: 0 = not yet; 1 = started already
!     Tref              Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       reference temperature                                  (K)
!     Qrain             Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       sensible heat flux due to rainfall                     (W*M^-2)
!                       
!   nstio_dbta        nst file longreal data fields
!                       
! Public Subprograms:
!   nstio_sropen      Open nst file for sequential reading
!     lu                Integer(nstio_intkind) input logical unit
!     cfname            Character(*) input filename
!     iret              Integer(nstio_intkind) output return code
!
!   nstio_swopen      Open nst file for sequential writing
!     lu                Integer(nstio_intkind) input logical unit
!     cfname            Character(*) input filename
!     iret              Integer(nstio_intkind) output return code
!
!   nstio_srclose      Close nst file for sequential I/O
!     lu                Integer(nstio_intkind) input logical unit
!     iret              Integer(nstio_intkind) output return code
!
!   nstio_srhead      Read header information with sequential I/O
!     lu                Integer(nstio_intkind) input logical unit
!     head              Type(nstio_head) output header information
!     iret              Integer(nstio_intkind) output return code
!
!   nstio_swhead      Write header information with sequential I/O
!     lu                Integer(nstio_intkind) input logical unit
!     head              Type(nstio_head) input header information
!     iret              Integer(nstio_intkind) output return code
!
!   nstio_alhead      Allocate head allocatables
!     head              Type(nstio_head) input/output header information
!     iret              Integer(nstio_intkind) output return code
!     latb              Integer(nstio_intkind) optional latitudes
!     lsea             Integer(nstio_intkind) optional sea levels
!
!   nstio_aldata      Allocate data fields
!     head              Type(nstio_head) input header information
!     data              Type(nstio_data) output data fields
!     iret              Integer(nstio_intkind) output return code
!
!   nstio_axdata      Deallocate data fields
!     data              Type(nstio_data) output data fields
!     iret              Integer(nstio_intkind) output return code
!
!   nstio_srdata      Read data fields with sequential I/O
!     lu                Integer(nstio_intkind) input logical unit
!     head              Type(nstio_head) input header information
!     data              Type(nstio_data) output data fields
!     iret              Integer(nstio_intkind) output return code
!
!   nstio_swdata      Write data fields with sequential I/O
!     lu                Integer(nstio_intkind) input logical unit
!     head              Type(nstio_head) input header information
!     data              Type(nstio_data) input data fields
!     iret              Integer(nstio_intkind) output return code
!
!   nstio_srohdc      Open, read header & data and close with sequential I/O
!     lu                Integer(nstio_intkind) input logical unit
!     cfname            Character(*) input filename
!     head              Type(nstio_head) output header information
!     data              Type(nstio_data) output data fields
!     iret              Integer(nstio_intkind) output return code
!
!   nstio_swohdc      Open, write header & data and close with sequential I/O
!     lu                Integer(nstio_intkind) input logical unit
!     cfname            Character(*) input filename
!     head              Type(nstio_head) input header information
!     data              Type(nstio_data) input data fields
!     iret              Integer(nstio_intkind) output return code
!
!   nstio_aldbta      Allocate longreal data fields
!     head              Type(nstio_head) input header information
!     dbta              Type(nstio_dbta) output longreal data fields
!     iret              Integer(nstio_intkind) output return code
!
!   nstio_axdbta      Deallocate longreal data fields
!     dbta              Type(nstio_dbta) output longreal data fields
!     iret              Integer(nstio_intkind) output return code
!
!   nstio_srdbta      Read longreal data fields with sequential I/O
!     lu                Integer(nstio_intkind) input logical unit
!     head              Type(nstio_head) input header information
!     dbta              Type(nstio_dbta) output longreal data fields
!     iret              Integer(nstio_intkind) output return code
!
!   nstio_swdbta      Write longreal data fields with sequential I/O
!     lu                Integer(nstio_intkind) input logical unit
!     head              Type(nstio_head) input header information
!     dbta              Type(nstio_dbta) input longreal data fields
!     iret              Integer(nstio_intkind) output return code
!
! Remarks:
!   (1) Here's the supported nst file formats.
!       For ivo=200907 
!         Label containing
!           'GFS ','NST ',ivo,nhead,ndata,reserved(3) (8 4-byte words)
!         Header records
!           lhead(nhead),ldata(ndata) (nhead+ndata 4-byte words)
!           fhour, idate(4), lonb, latb, lsea, irealf,
!             reserved(16)  (25 4-byte words)
!           lpl  (latb/2 4-byte words)
!           zsea  (lsea 4-byte words)
!         Data records
!           slmsk    (lonb*latb 4-byte words)
!           xt       (lonb*latb 4-byte words)
!           xs       (lonb*latb 4-byte words)
!           xu       (lonb*latb 4-byte words)
!           xv       (lonb*latb 4-byte words)
!           xz       (lonb*latb 4-byte words)
!           zm       (lonb*latb 4-byte words)
!           xtts     (lonb*latb 4-byte words)
!           xzts     (lonb*latb 4-byte words)
!           dt_cool  (lonb*latb 4-byte words)
!           z_c      (lonb*latb 4-byte words)
!           c_0      (lonb*latb 4-byte words)
!           c_d      (lonb*latb 4-byte words)
!           w_0      (lonb*latb 4-byte words)
!           w_d      (lonb*latb 4-byte words)
!           d_conv   (lonb*latb 4-byte words)
!           ifd      (lonb*latb 4-byte words)
!           Tref     (lonb*latb 4-byte words)
!           Qrain    (lonb*latb 4-byte words)
!
!   (2) Possible return codes:
!          0   Successful call
!         -1   Open or close I/O error
!         -2   Header record I/O error or unrecognized version
!         -3   Allocation or deallocation error
!         -4   Data record I/O error
!         -5   Insufficient data dimensions allocated
!
! Examples:
!   (1) Read the entire nst file 'nstf24' and
!       print out the northernmost nst temperature at greenwich.
!
!     use nstio_module
!     type(nstio_head):: head
!     type(nstio_data):: data
!     call nstio_srohdc(11,'nstf24',head,data,iret)
!     print '(f8.2)',data%tref(1,1)
!     end
!
! Attributes:
!   Language: Fortran 90
!
!$$$
  implicit none
  private
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Public Variables
  integer,parameter,public:: ngrids_nst=19
  integer,parameter,public:: nstio_lhead1=32
  integer,parameter,public:: nstio_intkind=4,nstio_realkind=4,nstio_dblekind=8
  real(nstio_realkind),parameter,public:: nstio_realfill=-9999.
  real(nstio_dblekind),parameter,public:: nstio_dblefill=nstio_realfill
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Public Types
  type,public:: nstio_head
    character(nstio_lhead1):: clabnst='                                '
    real(nstio_realkind):: fhour=0.
    integer(nstio_intkind):: idate(4)=(/0,0,0,0/),latb=0,lonb=0,lsea=0,ivo=0
    integer(nstio_intkind):: irealf=1
    integer(nstio_intkind),allocatable:: lpl(:)
    real(nstio_realkind),allocatable:: zsea(:)
  end type
  type,public:: nstio_data
    real(nstio_realkind),pointer:: slmsk    (:,:)=>null()
    real(nstio_realkind),pointer:: xt       (:,:)=>null()
    real(nstio_realkind),pointer:: xs       (:,:)=>null()
    real(nstio_realkind),pointer:: xu       (:,:)=>null()
    real(nstio_realkind),pointer:: xv       (:,:)=>null()
    real(nstio_realkind),pointer:: xz       (:,:)=>null()
    real(nstio_realkind),pointer:: zm       (:,:)=>null()
    real(nstio_realkind),pointer:: xtts     (:,:)=>null()
    real(nstio_realkind),pointer:: xzts     (:,:)=>null()
    real(nstio_realkind),pointer:: dt_cool  (:,:)=>null()
    real(nstio_realkind),pointer:: z_c      (:,:)=>null()
    real(nstio_realkind),pointer:: c_0      (:,:)=>null()
    real(nstio_realkind),pointer:: c_d      (:,:)=>null()
    real(nstio_realkind),pointer:: w_0      (:,:)=>null()
    real(nstio_realkind),pointer:: w_d      (:,:)=>null()
    real(nstio_realkind),pointer:: d_conv   (:,:)=>null()
    real(nstio_realkind),pointer:: ifd      (:,:)=>null()
    real(nstio_realkind),pointer:: tref     (:,:)=>null()
    real(nstio_realkind),pointer:: Qrain    (:,:)=>null()
  end type
  type,public:: nstio_dbta
    real(nstio_dblekind),pointer:: slmsk    (:,:)=>null()
    real(nstio_dblekind),pointer:: xt       (:,:)=>null()
    real(nstio_dblekind),pointer:: xs       (:,:)=>null()
    real(nstio_dblekind),pointer:: xu       (:,:)=>null()
    real(nstio_dblekind),pointer:: xv       (:,:)=>null()
    real(nstio_dblekind),pointer:: xz       (:,:)=>null()
    real(nstio_dblekind),pointer:: zm       (:,:)=>null()
    real(nstio_dblekind),pointer:: xtts     (:,:)=>null()
    real(nstio_dblekind),pointer:: xzts     (:,:)=>null()
    real(nstio_dblekind),pointer:: dt_cool  (:,:)=>null()
    real(nstio_dblekind),pointer:: z_c      (:,:)=>null()
    real(nstio_dblekind),pointer:: c_0      (:,:)=>null()
    real(nstio_dblekind),pointer:: c_d      (:,:)=>null()
    real(nstio_dblekind),pointer:: w_0      (:,:)=>null()
    real(nstio_dblekind),pointer:: w_d      (:,:)=>null()
    real(nstio_dblekind),pointer:: d_conv   (:,:)=>null()
    real(nstio_dblekind),pointer:: ifd      (:,:)=>null()
    real(nstio_dblekind),pointer:: tref     (:,:)=>null()
    real(nstio_dblekind),pointer:: Qrain    (:,:)=>null()
  end type
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Public Subprograms
  public nstio_sropen,nstio_swopen,nstio_srclose,nstio_srhead,nstio_swhead
  public nstio_alhead,nstio_aldata,nstio_axdata,nstio_srdata,nstio_swdata
  public nstio_aldbta,nstio_axdbta,nstio_srdbta,nstio_swdbta
  public nstio_srohdc,nstio_swohdc
  interface nstio_srohdc
  module procedure nstio_srohdca,nstio_srohdcb
  end interface
  interface nstio_swohdc
  module procedure nstio_swohdca,nstio_swohdcb
  end interface
contains
!-------------------------------------------------------------------------------
  subroutine nstio_sropen(lu,cfname,iret)
    implicit none
    integer(nstio_intkind),intent(in):: lu
    character*(*),intent(in):: cfname
    integer(nstio_intkind),intent(out):: iret
    integer ios
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    open(lu,file=cfname,form='unformatted',&
         status='old',action='read',iostat=ios)
!   write(*,*) ' successfully opened : ',cfname, ios
    iret=ios
    if(iret.ne.0) iret=-1
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine nstio_swopen(lu,cfname,iret)
    implicit none
    integer(nstio_intkind),intent(in):: lu
    character*(*),intent(in):: cfname
    integer(nstio_intkind),intent(out):: iret
    integer ios
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    open(lu,file=cfname,form='unformatted',&
         status='unknown',action='readwrite',iostat=ios)
    iret=ios
    if(iret.ne.0) iret=-1
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine nstio_srclose(lu,iret)
    implicit none
    integer(nstio_intkind),intent(in):: lu
    integer(nstio_intkind),intent(out):: iret
    integer ios
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    close(lu,iostat=ios)
    iret=ios
    if(iret.ne.0) iret=-1
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine nstio_srhead(lu,head,iret)
    implicit none
    integer(nstio_intkind),intent(in):: lu
    type(nstio_head),intent(out):: head
    integer(nstio_intkind),intent(out):: iret
    integer:: ios
    character(4):: cgfs,cnst
    integer(nstio_intkind):: nhead,nresv(3)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=-2
    rewind lu
    read(lu,iostat=ios) head%clabnst(1:8)
!   write(*,*) ' head%clabnst done, ios : ',head%clabnst(1:8), ios
    if(ios.ne.0) return
    if(head%clabnst(1:8).eq.'GFS NST ') then  ! modern nst file
      rewind lu
      read(lu,iostat=ios) cgfs,cnst,head%ivo,nhead,nresv
!     write(*,*) ' cgfs,cnst done, ios : ',cgfs,cnst, ios,head%ivo,nhead
      if(ios.ne.0) return
      if(head%ivo.eq.200907) then
        read(lu,iostat=ios)
        if(ios.ne.0) return
        read(lu,iostat=ios) head%fhour,head%idate,head%lonb,head%latb,&
                            head%lsea,head%irealf
        if(ios.ne.0) return
        call nstio_alhead(head,ios)
        if(ios.ne.0) return
        read(lu,iostat=ios) head%lpl
        if(ios.ne.0) return
        read(lu,iostat=ios) head%zsea
        if(ios.ne.0) return
      else
        return
      endif
    endif
    iret=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine nstio_swhead(lu,head,iret)
    implicit none
    integer(nstio_intkind),intent(in):: lu
    type(nstio_head),intent(in):: head
    integer(nstio_intkind),intent(out):: iret
    integer:: ios
    integer i
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=-2
    if(head%ivo.eq.200907) then
      rewind lu
      write(lu,iostat=ios) 'GFS NST ',head%ivo,ngrids_nst+4*head%lsea,0,0,0
      if(ios.ne.0) return
      write(lu,iostat=ios) 4*(/8,ngrids_nst+4*head%lsea,25,head%latb/2,head%lsea/),&
                           4*head%irealf*(/(head%lonb*head%latb,&
                                            i=1,ngrids_nst+4*head%lsea)/)
      if(ios.ne.0) return
      write(lu,iostat=ios) head%fhour,head%idate,head%lonb,head%latb,&
                           head%lsea,head%irealf,(0,i=1,16)
      if(ios.ne.0) return
      write(lu,iostat=ios) head%lpl
      if(ios.ne.0) return
      write(lu,iostat=ios) head%zsea
      if(ios.ne.0) return
      iret=0
    endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine nstio_alhead(head,iret,latb,lsea)
    implicit none
    type(nstio_head),intent(inout):: head
    integer(nstio_intkind),intent(out):: iret
    integer(nstio_intkind),optional,intent(in):: latb,lsea
    integer dim1l,dim1z
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(present(latb)) then
      dim1l=latb/2
    else
      dim1l=head%latb/2
    endif
    if(present(lsea)) then
      dim1z=lsea
    else
      dim1z=head%lsea
    endif
    if(allocated(head%lpl)) deallocate(head%lpl)
    if(allocated(head%zsea)) deallocate(head%zsea)
    allocate(head%lpl(dim1l),head%zsea(dim1z),stat=iret)
    if(iret.eq.0) then
      head%lpl=0
      head%zsea=nstio_realfill
    endif
    if(iret.ne.0) then
      iret=-3
      write(*,*) ' fail to allocate nstio%head, iret = ',iret
    endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine nstio_aldata(head,data,iret)
    implicit none
    type(nstio_head),intent(in):: head
    type(nstio_data),intent(inout):: data
    integer(nstio_intkind),intent(out):: iret
    integer dim1,dim2,dim3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call nstio_axdata(data,iret)
    dim1=head%lonb
    dim2=head%latb
    dim3=head%lsea
!   write(*,*) 'in nstio_aldata, dim1, dim2, dim3 : ', dim1, dim2, dim3
    allocate(&
      data%slmsk(dim1,dim2),&
      data%xt(dim1,dim2),&
      data%xs(dim1,dim2),&
      data%xu(dim1,dim2),&
      data%xv(dim1,dim2),&
      data%xz(dim1,dim2),&
      data%zm(dim1,dim2),&
      data%xtts(dim1,dim2),&
      data%xzts(dim1,dim2),&
      data%dt_cool(dim1,dim2),&
      data%z_c(dim1,dim2),&
      data%c_0(dim1,dim2),&
      data%c_d(dim1,dim2),&
      data%w_0(dim1,dim2),&
      data%w_d(dim1,dim2),&
      data%d_conv(dim1,dim2),&
      data%ifd(dim1,dim2),&
      data%tref(dim1,dim2),&
      data%Qrain(dim1,dim2),&
      stat=iret)
    if(iret.ne.0) then
      iret=-3
      write(*,*) ' fail to allocate nstio%data, iret = ',iret
    endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine nstio_axdata(data,iret)
    implicit none
    type(nstio_data),intent(inout):: data
    integer(nstio_intkind),intent(out):: iret
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    deallocate(&
      data%slmsk,&
      data%xt,&
      data%xs,&
      data%xu,&
      data%xv,&
      data%xz,&
      data%zm,&
      data%xtts,&
      data%xzts,&
      data%dt_cool,&
      data%z_c,&
      data%c_0,&
      data%c_d,&
      data%w_0,&
      data%w_d,&
      data%d_conv,&
      data%ifd,&
      data%tref,&
      data%Qrain,&
      stat=iret)
    nullify(&
      data%slmsk,&
      data%xt,&
      data%xs,&
      data%xu,&
      data%xv,&
      data%xz,&
      data%zm,&
      data%xtts,&
      data%xzts,&
      data%dt_cool,&
      data%z_c,&
      data%c_0,&
      data%c_d,&
      data%w_0,&
      data%w_d,&
      data%d_conv,&
      data%ifd,&
      data%tref,&
      data%Qrain)
    if(iret.ne.0) iret=-3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine nstio_srdata(lu,head,data,iret)
    implicit none
    integer(nstio_intkind),intent(in):: lu
    type(nstio_head),intent(in):: head
    type(nstio_data),intent(inout):: data
    integer(nstio_intkind),intent(out):: iret
    integer:: dim1,dim2,dim3,mdim1,mdim2,mdim3
    integer:: ios
    type(nstio_dbta) dbta
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    dim1=head%lonb
    dim2=head%latb
    dim3=head%lsea

    mdim1=min(&
      size(data%slmsk,1),&
      size(data%xt,1),&
      size(data%xs,1),&
      size(data%xu,1),&
      size(data%xv,1),&
      size(data%xz,1),&
      size(data%zm,1),&
      size(data%xtts,1),&
      size(data%xzts,1),&
      size(data%dt_cool,1),&
      size(data%z_c,1),&
      size(data%c_0,1),&
      size(data%c_d,1),&
      size(data%w_0,1),&
      size(data%w_d,1),&
      size(data%d_conv,1),&
      size(data%ifd,1),&
      size(data%tref,1),&
      size(data%Qrain,1))
    mdim2=min(&
      size(data%slmsk,2),&
      size(data%xt,2),&
      size(data%xs,2),&
      size(data%xu,2),&
      size(data%xv,2),&
      size(data%xz,2),&
      size(data%zm,2),&
      size(data%xtts,2),&
      size(data%xzts,2),&
      size(data%dt_cool,2),&
      size(data%z_c,2),&
      size(data%c_0,2),&
      size(data%c_d,2),&
      size(data%w_0,2),&
      size(data%w_d,2),&
      size(data%d_conv,2),&
      size(data%ifd,2),&
      size(data%tref,2),&
      size(data%Qrain,2))
    mdim3=0
    iret=-5
    if(mdim1.lt.dim1.or.&
       mdim2.lt.dim2.or.&
       mdim3.lt.dim3) return
    iret=-4
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(head%ivo.eq.200907) then
      if(head%irealf.ne.2) then
        read(lu,iostat=ios)    data%slmsk(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios)       data%xt(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios)       data%xs(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios)       data%xu(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios)       data%xv(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios)       data%xz(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios)       data%zm(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios)     data%xtts(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios)     data%xzts(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios)  data%dt_cool(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios)      data%z_c(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios)      data%c_0(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios)      data%c_d(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios)      data%w_0(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios)      data%w_d(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios)   data%d_conv(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios)      data%ifd(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios)     data%tref(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios)    data%Qrain(:dim1,:dim2)
        if(ios.ne.0) return
      else
        call nstio_aldbta(head,dbta,iret)
        if(iret.ne.0) return
        call nstio_srdbta(lu,head,dbta,iret)
        if(iret.ne.0) return
        data%slmsk(:dim1,:dim2)   = dbta%slmsk(:dim1,:dim2)
        data%xt(:dim1,:dim2)      = dbta%xt(:dim1,:dim2)
        data%xs(:dim1,:dim2)      = dbta%xs(:dim1,:dim2)
        data%xu(:dim1,:dim2)      = dbta%xu(:dim1,:dim2)
        data%xv(:dim1,:dim2)      = dbta%xv(:dim1,:dim2)
        data%xz(:dim1,:dim2)      = dbta%xz(:dim1,:dim2)
        data%zm(:dim1,:dim2)      = dbta%zm(:dim1,:dim2)
        data%xtts(:dim1,:dim2)    = dbta%xtts(:dim1,:dim2)
        data%xzts(:dim1,:dim2)    = dbta%xzts(:dim1,:dim2)
        data%dt_cool(:dim1,:dim2) = dbta%dt_cool(:dim1,:dim2)
        data%z_c(:dim1,:dim2)     = dbta%z_c(:dim1,:dim2)
        data%c_0(:dim1,:dim2)     = dbta%c_0(:dim1,:dim2)
        data%c_d(:dim1,:dim2)     = dbta%c_d(:dim1,:dim2)
        data%w_0(:dim1,:dim2)     = dbta%w_0(:dim1,:dim2)
        data%w_d(:dim1,:dim2)     = dbta%w_d(:dim1,:dim2)
        data%d_conv(:dim1,:dim2)  = dbta%d_conv(:dim1,:dim2)
        data%ifd(:dim1,:dim2)     = dbta%ifd(:dim1,:dim2)
        data%tref(:dim1,:dim2)    = dbta%tref(:dim1,:dim2)
        data%Qrain(:dim1,:dim2)   = dbta%Qrain(:dim1,:dim2)
        call nstio_axdbta(dbta,iret)
      endif
    endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=0
  end subroutine
!-------------------------------------------------------------------------------
  subroutine nstio_swdata(lu,head,data,iret)
    implicit none
    integer(nstio_intkind),intent(in):: lu
    type(nstio_head),intent(in):: head
    type(nstio_data),intent(in):: data
    integer(nstio_intkind),intent(out):: iret
    integer:: dim1,dim2,dim3,mdim1,mdim2,mdim3
    integer:: ios
    type(nstio_dbta) dbta
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    dim1=head%lonb
    dim2=head%latb
    dim3=head%lsea
    mdim1=min(&
      size(data%slmsk,1),&
      size(data%xt,1),&
      size(data%xs,1),&
      size(data%xu,1),&
      size(data%xv,1),&
      size(data%xz,1),&
      size(data%zm,1),&
      size(data%xtts,1),&
      size(data%xzts,1),&
      size(data%dt_cool,1),&
      size(data%z_c,1),&
      size(data%c_0,1),&
      size(data%c_d,1),&
      size(data%w_0,1),&
      size(data%w_d,1),&
      size(data%d_conv,1),&
      size(data%ifd,1),&
      size(data%tref,1),&
      size(data%Qrain,1))
    mdim2=min(&
      size(data%slmsk,2),&
      size(data%xt,2),&
      size(data%xs,2),&
      size(data%xu,2),&
      size(data%xv,2),&
      size(data%xz,2),&
      size(data%zm,2),&
      size(data%xtts,2),&
      size(data%xzts,2),&
      size(data%dt_cool,2),&
      size(data%z_c,2),&
      size(data%c_0,2),&
      size(data%c_d,2),&
      size(data%w_0,2),&
      size(data%w_d,2),&
      size(data%d_conv,2),&
      size(data%ifd,2),&
      size(data%tref,2))
    mdim3=0
    iret=-5
    if(mdim1.lt.dim1.or.&
       mdim2.lt.dim2.or.&
       mdim3.lt.dim3) return
    iret=-4
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(head%ivo.eq.200907) then
      if(head%irealf.ne.2) then
        write(lu,iostat=ios) data%slmsk(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%xt(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%xs(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%xu(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%xv(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%xz(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%zm(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%xtts(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%xzts(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%dt_cool(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%z_c(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%c_0(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%c_d(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%w_0(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%w_d(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%d_conv(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%ifd(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%tref(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%Qrain(:dim1,:dim2)
        if(ios.ne.0) return
      else
        call nstio_aldbta(head,dbta,iret)
        if(iret.ne.0) return
        dbta%slmsk(:dim1,:dim2)    = data%slmsk(:dim1,:dim2)
        dbta%xt(:dim1,:dim2)       = data%xt(:dim1,:dim2)
        dbta%xs(:dim1,:dim2)       = data%xs(:dim1,:dim2)
        dbta%xu(:dim1,:dim2)       = data%xu(:dim1,:dim2)
        dbta%xv(:dim1,:dim2)       = data%xv(:dim1,:dim2)
        dbta%xz(:dim1,:dim2)       = data%xz(:dim1,:dim2)
        dbta%zm(:dim1,:dim2)       = data%zm(:dim1,:dim2)
        dbta%xtts(:dim1,:dim2)     = data%xtts(:dim1,:dim2)
        dbta%xzts(:dim1,:dim2)     = data%xzts(:dim1,:dim2)
        dbta%dt_cool(:dim1,:dim2)  = data%dt_cool(:dim1,:dim2)
        dbta%z_c(:dim1,:dim2)      = data%z_c(:dim1,:dim2)
        dbta%c_0(:dim1,:dim2)      = data%c_0(:dim1,:dim2)
        dbta%c_d(:dim1,:dim2)      = data%c_d(:dim1,:dim2)
        dbta%w_0(:dim1,:dim2)      = data%w_0(:dim1,:dim2)
        dbta%w_d(:dim1,:dim2)      = data%w_d(:dim1,:dim2)
        dbta%d_conv(:dim1,:dim2)   = data%d_conv(:dim1,:dim2)
        dbta%ifd(:dim1,:dim2)      = data%ifd(:dim1,:dim2)
        dbta%tref(:dim1,:dim2)     = data%tref(:dim1,:dim2)
        dbta%Qrain(:dim1,:dim2)    = data%Qrain(:dim1,:dim2)
        call nstio_swdbta(lu,head,dbta,iret)
        if(iret.ne.0) return
        call nstio_axdbta(dbta,iret)
      endif
    endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=0
  end subroutine
!-------------------------------------------------------------------------------
  subroutine nstio_srohdca(lu,cfname,head,data,iret)
    implicit none
    integer(nstio_intkind),intent(in):: lu
    character*(*),intent(in):: cfname
    type(nstio_head),intent(inout):: head
    type(nstio_data),intent(inout):: data
    integer(nstio_intkind),intent(out):: iret
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call nstio_sropen(lu,cfname,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call nstio_srhead(lu,head,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call nstio_aldata(head,data,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call nstio_srdata(lu,head,data,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call nstio_srclose(lu,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine nstio_swohdca(lu,cfname,head,data,iret)
    implicit none
    integer(nstio_intkind),intent(in):: lu
    character*(*),intent(in):: cfname
    type(nstio_head),intent(in):: head
    type(nstio_data),intent(in):: data
    integer(nstio_intkind),intent(out):: iret
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call nstio_swopen(lu,cfname,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call nstio_swhead(lu,head,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call nstio_swdata(lu,head,data,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call nstio_srclose(lu,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine nstio_aldbta(head,dbta,iret)
    implicit none
    type(nstio_head),intent(in):: head
    type(nstio_dbta),intent(inout):: dbta
    integer(nstio_intkind),intent(out):: iret
    integer dim1,dim2,dim3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call nstio_axdbta(dbta,iret)
    dim1=head%lonb
    dim2=head%latb
    dim3=head%lsea
    allocate(&
      dbta%slmsk(dim1,dim2),&
      dbta%xt(dim1,dim2),&
      dbta%xs(dim1,dim2),&
      dbta%xu(dim1,dim2),&
      dbta%xv(dim1,dim2),&
      dbta%xz(dim1,dim2),&
      dbta%zm(dim1,dim2),&
      dbta%xtts(dim1,dim2),&
      dbta%xzts(dim1,dim2),&
      dbta%dt_cool(dim1,dim2),&
      dbta%z_c(dim1,dim2),&
      dbta%c_0(dim1,dim2),&
      dbta%c_d(dim1,dim2),&
      dbta%w_0(dim1,dim2),&
      dbta%w_d(dim1,dim2),&
      dbta%d_conv(dim1,dim2),&
      dbta%ifd(dim1,dim2),&
      dbta%tref(dim1,dim2),&
      dbta%Qrain(dim1,dim2),&
      stat=iret)
    if(iret.ne.0) iret=-3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine nstio_axdbta(dbta,iret)
    implicit none
    type(nstio_dbta),intent(inout):: dbta
    integer(nstio_intkind),intent(out):: iret
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    deallocate(&
      dbta%slmsk,&
      dbta%xt,&
      dbta%xs,&
      dbta%xu,&
      dbta%xv,&
      dbta%xz,&
      dbta%zm,&
      dbta%xtts,&
      dbta%xzts,&
      dbta%dt_cool,&
      dbta%z_c,&
      dbta%c_0,&
      dbta%c_d,&
      dbta%w_0,&
      dbta%w_d,&
      dbta%d_conv,&
      dbta%ifd,&
      dbta%tref,&
      dbta%Qrain,&
      stat=iret)
    nullify(&
      dbta%slmsk,&
      dbta%xt,&
      dbta%xs,&
      dbta%xu,&
      dbta%xv,&
      dbta%xz,&
      dbta%zm,&
      dbta%xtts,&
      dbta%xzts,&
      dbta%dt_cool,&
      dbta%z_c,&
      dbta%c_0,&
      dbta%c_d,&
      dbta%w_0,&
      dbta%w_d,&
      dbta%d_conv,&
      dbta%ifd,&
      dbta%tref)
    if(iret.ne.0) iret=-3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine nstio_srdbta(lu,head,dbta,iret)
    implicit none
    integer(nstio_intkind),intent(in):: lu
    type(nstio_head),intent(in):: head
    type(nstio_dbta),intent(inout):: dbta
    integer(nstio_intkind),intent(out):: iret
    integer:: dim1,dim2,dim3,mdim1,mdim2,mdim3
    integer:: ios
    type(nstio_data):: data
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    dim1=head%lonb
    dim2=head%latb
    dim3=head%lsea
    mdim1=min(&
      size(dbta%slmsk,1),&
      size(dbta%xt,1),&
      size(dbta%xs,1),&
      size(dbta%xu,1),&
      size(dbta%xv,1),&
      size(dbta%xz,1),&
      size(dbta%zm,1),&
      size(dbta%xtts,1),&
      size(dbta%xzts,1),&
      size(dbta%dt_cool,1),&
      size(dbta%z_c,1),&
      size(dbta%c_0,1),&
      size(dbta%c_d,1),&
      size(dbta%w_0,1),&
      size(dbta%w_d,1),&
      size(dbta%d_conv,1),&
      size(dbta%ifd,1),&
      size(dbta%tref,1),&
      size(dbta%Qrain,1))
    mdim2=min(&
      size(dbta%slmsk,2),&
      size(dbta%xt,2),&
      size(dbta%xs,2),&
      size(dbta%xu,2),&
      size(dbta%xv,2),&
      size(dbta%xz,2),&
      size(dbta%zm,2),&
      size(dbta%xtts,2),&
      size(dbta%xzts,2),&
      size(dbta%dt_cool,2),&
      size(dbta%z_c,2),&
      size(dbta%c_0,2),&
      size(dbta%c_d,2),&
      size(dbta%w_0,2),&
      size(dbta%w_d,2),&
      size(dbta%d_conv,2),&
      size(dbta%ifd,2),&
      size(dbta%tref,2),&
      size(dbta%Qrain,2))
    mdim3=0
    iret=-5
    if(mdim1.lt.dim1.or.&
       mdim2.lt.dim2.or.&
       mdim3.lt.dim3) return
    iret=-4
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(head%irealf.ne.2) then
      call nstio_aldata(head,data,iret)
      if(iret.ne.0) return
      call nstio_srdata(lu,head,data,iret)
      if(iret.ne.0) return
        dbta%slmsk(:dim1,:dim2)    = data%slmsk(:dim1,:dim2)
        dbta%xt(:dim1,:dim2)       = data%xt(:dim1,:dim2)
        dbta%xs(:dim1,:dim2)       = data%xs(:dim1,:dim2)
        dbta%xu(:dim1,:dim2)       = data%xu(:dim1,:dim2)
        dbta%xv(:dim1,:dim2)       = data%xv(:dim1,:dim2)
        dbta%xz(:dim1,:dim2)       = data%xz(:dim1,:dim2)
        dbta%zm(:dim1,:dim2)       = data%zm(:dim1,:dim2)
        dbta%xtts(:dim1,:dim2)     = data%xtts(:dim1,:dim2)
        dbta%xzts(:dim1,:dim2)     = data%xzts(:dim1,:dim2)
        dbta%dt_cool(:dim1,:dim2)  = data%dt_cool(:dim1,:dim2)
        dbta%z_c(:dim1,:dim2)      = data%z_c(:dim1,:dim2)
        dbta%c_0(:dim1,:dim2)      = data%c_0(:dim1,:dim2)
        dbta%c_d(:dim1,:dim2)      = data%c_d(:dim1,:dim2)
        dbta%w_0(:dim1,:dim2)      = data%w_0(:dim1,:dim2)
        dbta%w_d(:dim1,:dim2)      = data%w_d(:dim1,:dim2)
        dbta%d_conv(:dim1,:dim2)   = data%d_conv(:dim1,:dim2)
        dbta%ifd(:dim1,:dim2)      = data%ifd(:dim1,:dim2)
        dbta%tref(:dim1,:dim2)     = data%tref(:dim1,:dim2)
        dbta%Qrain(:dim1,:dim2)    = data%Qrain(:dim1,:dim2)
      call nstio_axdata(data,iret)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    elseif(head%ivo == 200907) then
      read(lu,iostat=ios) dbta%slmsk(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%xt(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%xs(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%xu(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%xv(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%xz(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%zm(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%xtts(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%xzts(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%dt_cool(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%z_c(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%c_0(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%c_d(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%w_0(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%w_d(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%d_conv(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%ifd(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%tref(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%Qrain(:dim1,:dim2)
      if(ios.ne.0) return
    endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=0
  end subroutine
!-------------------------------------------------------------------------------
  subroutine nstio_swdbta(lu,head,dbta,iret)
    implicit none
    integer(nstio_intkind),intent(in):: lu
    type(nstio_head),intent(in):: head
    type(nstio_dbta),intent(in):: dbta
    integer(nstio_intkind),intent(out):: iret
    integer:: dim1,dim2,dim3,mdim1,mdim2,mdim3
    integer:: ios
    type(nstio_data):: data
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    dim1=head%lonb
    dim2=head%latb
    dim3=head%lsea
    mdim1=min(&
      size(dbta%slmsk,1),&
      size(dbta%xt,1),&
      size(dbta%xs,1),&
      size(dbta%xu,1),&
      size(dbta%xv,1),&
      size(dbta%xz,1),&
      size(dbta%zm,1),&
      size(dbta%xtts,1),&
      size(dbta%xzts,1),&
      size(dbta%dt_cool,1),&
      size(dbta%z_c,1),&
      size(dbta%c_0,1),&
      size(dbta%c_d,1),&
      size(dbta%w_0,1),&
      size(dbta%w_d,1),&
      size(dbta%d_conv,1),&
      size(dbta%ifd,1),&
      size(dbta%tref,1),&
      size(dbta%Qrain,1))
    mdim2=min(&
      size(dbta%slmsk,2),&
      size(dbta%xt,2),&
      size(dbta%xs,2),&
      size(dbta%xu,2),&
      size(dbta%xv,2),&
      size(dbta%xz,2),&
      size(dbta%zm,2),&
      size(dbta%xtts,2),&
      size(dbta%xzts,2),&
      size(dbta%dt_cool,2),&
      size(dbta%z_c,2),&
      size(dbta%c_0,2),&
      size(dbta%c_d,2),&
      size(dbta%w_0,2),&
      size(dbta%w_d,2),&
      size(dbta%d_conv,2),&
      size(dbta%ifd,2),&
      size(dbta%tref,2),&
      size(dbta%Qrain,2))
    mdim3=0
    iret=-5
    if(mdim1.lt.dim1.or.&
       mdim2.lt.dim2.or.&
       mdim3.lt.dim3) return
    iret=-4
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(head%irealf.ne.2) then
      call nstio_aldata(head,data,iret)
      if(iret.ne.0) return
        data%slmsk(:dim1,:dim2)   = dbta%slmsk(:dim1,:dim2)
        data%xt(:dim1,:dim2)      = dbta%xt(:dim1,:dim2)
        data%xs(:dim1,:dim2)      = dbta%xs(:dim1,:dim2)
        data%xu(:dim1,:dim2)      = dbta%xu(:dim1,:dim2)
        data%xv(:dim1,:dim2)      = dbta%xv(:dim1,:dim2)
        data%xz(:dim1,:dim2)      = dbta%xz(:dim1,:dim2)
        data%zm(:dim1,:dim2)      = dbta%zm(:dim1,:dim2)
        data%xtts(:dim1,:dim2)    = dbta%xtts(:dim1,:dim2)
        data%xzts(:dim1,:dim2)    = dbta%xzts(:dim1,:dim2)
        data%dt_cool(:dim1,:dim2) = dbta%dt_cool(:dim1,:dim2)
        data%z_c(:dim1,:dim2)     = dbta%z_c(:dim1,:dim2)
        data%c_0(:dim1,:dim2)     = dbta%c_0(:dim1,:dim2)
        data%c_d(:dim1,:dim2)     = dbta%c_d(:dim1,:dim2)
        data%w_0(:dim1,:dim2)     = dbta%w_0(:dim1,:dim2)
        data%w_d(:dim1,:dim2)     = dbta%w_d(:dim1,:dim2)
        data%d_conv(:dim1,:dim2)  = dbta%d_conv(:dim1,:dim2)
        data%ifd(:dim1,:dim2)     = dbta%ifd(:dim1,:dim2)
        data%tref(:dim1,:dim2)    = dbta%tref(:dim1,:dim2)
        data%Qrain(:dim1,:dim2)   = dbta%Qrain(:dim1,:dim2)
      call nstio_swdata(lu,head,data,iret)
      if(iret.ne.0) return
      call nstio_axdata(data,iret)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    elseif(head%ivo == 200907) then
      write(lu,iostat=ios) dbta%slmsk(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%xt(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%xs(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%xu(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%xv(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%xz(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%zm(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%xtts(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%xzts(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%dt_cool(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%z_c(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%c_0(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%c_d(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%w_0(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%w_d(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%d_conv(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%ifd(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%tref(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%Qrain(:dim1,:dim2)
      if(ios.ne.0) return
    endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=0
  end subroutine
!-------------------------------------------------------------------------------
  subroutine nstio_srohdcb(lu,cfname,head,dbta,iret)
    implicit none
    integer(nstio_intkind),intent(in):: lu
    character*(*),intent(in):: cfname
    type(nstio_head),intent(inout):: head
    type(nstio_dbta),intent(inout):: dbta
    integer(nstio_intkind),intent(out):: iret
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call nstio_sropen(lu,cfname,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call nstio_srhead(lu,head,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call nstio_aldbta(head,dbta,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call nstio_srdbta(lu,head,dbta,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call nstio_srclose(lu,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine nstio_swohdcb(lu,cfname,head,dbta,iret)
    implicit none
    integer(nstio_intkind),intent(in):: lu
    character*(*),intent(in):: cfname
    type(nstio_head),intent(in):: head
    type(nstio_dbta),intent(in):: dbta
    integer(nstio_intkind),intent(out):: iret
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call nstio_swopen(lu,cfname,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call nstio_swhead(lu,head,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call nstio_swdbta(lu,head,dbta,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call nstio_srclose(lu,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
end module

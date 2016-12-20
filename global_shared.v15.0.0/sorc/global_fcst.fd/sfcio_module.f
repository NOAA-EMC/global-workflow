!-------------------------------------------------------------------------------
module sfcio_module
!$$$  module documentation block
!
! module:    sfcio_module    api for global spectral surface file i/o
!   prgmmr: iredell          org: w/nx23     date: 1999-01-18
!
! abstract: this module provides an application program interface
!   for performing i/o on the surface restart file of the global spectral model.
!   functions include opening, reading, writing, and closing as well as
!   allocating and deallocating data buffers used in the transfers.
!   the i/o performed here is sequential.
!   the transfers are limited to header records or data records.
!   
! program history log:
!   1999-01-18  mark iredell
!
! public variables:
!   sfcio_lhead1      integer parameter length of first header record (=32)
!   sfcio_intkind     integer parameter kind or length of passed integers (=4)
!   sfcio_realkind    integer parameter kind or length of passed reals (=4)
!   sfcio_dblekind    integer parameter kind or length of passed longreals (=8)
!   sfcio_realfill    real(sfcio_realkind) fill value (=-9999.)
!   sfcio_dblefill    real(sfcio_dblekind) fill value (=-9999.)
!
! public defined types:
!   sfcio_head        surface file header information
!     clabsfc           character(sfcio_lhead1) on85 label
!     fhour             real(sfcio_realkind) forecast hour
!     idate             integer(sfcio_intkind)(4) initial date
!                       (hour, month, day, 4-digit year)
!     latb              integer(sfcio_intkind) latitudes
!     lonb              integer(sfcio_intkind) longitudes
!     ivs               integer(sfcio_intkind) version number
!     lsoil             integer(sfcio_intkind) soil levels
!     irealf            integer(sigio_intkind) floating point flag
!                       (=1 for 4-byte ieee, =2 for 8-byte ieee)
!     lpl               integer(sfcio_intkind)(latb/2) lons per lat
!     zsoil             real(sfcio_realkind) soil depths (meter)
!
!   sfcio_data        surface file data fields
!     tsea              real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       surface temperature in k
!     smc               real(sfcio_realkind)(:,:,:) pointer to lonb*latb*lsoil
!                       soil volumetric water content in fraction
!     sheleg            real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       snow depth in m
!     stc               real(sfcio_realkind)(:,:,:) pointer to lonb*latb*lsoil
!                       soil temperature in k
!     tg3               real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       deep soil temperature in k
!     zorl              real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       roughness in cm
!     cv                real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       convective cloud cover in fraction
!     cvb               real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       convective cloud bottom in kpa
!     cvt               real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       convective cloud top in kpa
!     alvsf             real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       albedo for visible scattered in fraction
!     alvwf             real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       albedo for visible beam in fraction
!     alnsf             real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       albedo for near-ir scattered in fraction
!     alnwf             real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       albedo for near-ir beam in fraction
!     slmsk             real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       sea-land-ice mask (0-sea, 1-land, 2-ice)
!     vfrac             real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       vegetation fraction in fraction
!     canopy            real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       canopy water in m
!     f10m              real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       10-meter wind speed over lowest model wind speed
!     t2m               real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       2-meter temperature in k
!     q2m               real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       2-meter specific humidity in kg/kg
!     vtype             real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       vegetation type in integer 1-13
!     stype             real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       soil type in integer 1-9
!     facsf             real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in fraction
!     facwf             real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in fraction
!     uustar            real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     ffmm              real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     ffhh              real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     hice              real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     fice              real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     tisfc             real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     tprcp             real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     srflag            real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     snwdph            real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     slc               real(sfcio_realkind)(:,:,:) pointer to lonb*latb*lsoil
!                       xxx in xxx
!     shdmin            real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     shdmax            real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     slope             real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     snoalb            real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in xxx
!     orog              real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       orography in m
!                       
!   sfcio_dbta        surface file longreal data fields
!                       
! public subprograms:
!   sfcio_sropen      open surface file for sequential reading
!     lu                integer(sfcio_intkind) input logical unit
!     cfname            character(*) input filename
!     iret              integer(sfcio_intkind) output return code
!
!   sfcio_swopen      open surface file for sequential writing
!     lu                integer(sfcio_intkind) input logical unit
!     cfname            character(*) input filename
!     iret              integer(sfcio_intkind) output return code
!
!   sfcio_sclose      close surface file for sequential i/o
!     lu                integer(sfcio_intkind) input logical unit
!     iret              integer(sfcio_intkind) output return code
!
!   sfcio_srhead      read header information with sequential i/o
!     lu                integer(sfcio_intkind) input logical unit
!     head              type(sfcio_head) output header information
!     iret              integer(sfcio_intkind) output return code
!
!   sfcio_swhead      write header information with sequential i/o
!     lu                integer(sfcio_intkind) input logical unit
!     head              type(sfcio_head) input header information
!     iret              integer(sfcio_intkind) output return code
!
!   sfcio_alhead      allocate head allocatables
!     head              type(sfcio_head) input/output header information
!     iret              integer(sfcio_intkind) output return code
!     latb              integer(sfcio_intkind) optional latitudes
!     lsoil             integer(sfcio_intkind) optional soil levels
!
!   sfcio_aldata      allocate data fields
!     head              type(sfcio_head) input header information
!     data              type(sfcio_data) output data fields
!     iret              integer(sfcio_intkind) output return code
!
!   sfcio_axdata      deallocate data fields
!     data              type(sfcio_data) output data fields
!     iret              integer(sfcio_intkind) output return code
!
!   sfcio_srdata      read data fields with sequential i/o
!     lu                integer(sfcio_intkind) input logical unit
!     head              type(sfcio_head) input header information
!     data              type(sfcio_data) output data fields
!     iret              integer(sfcio_intkind) output return code
!
!   sfcio_swdata      write data fields with sequential i/o
!     lu                integer(sfcio_intkind) input logical unit
!     head              type(sfcio_head) input header information
!     data              type(sfcio_data) input data fields
!     iret              integer(sfcio_intkind) output return code
!
!   sfcio_srohdc      open, read header & data and close with sequential i/o
!     lu                integer(sfcio_intkind) input logical unit
!     cfname            character(*) input filename
!     head              type(sfcio_head) output header information
!     data              type(sfcio_data) output data fields
!     iret              integer(sfcio_intkind) output return code
!
!   sfcio_swohdc      open, write header & data and close with sequential i/o
!     lu                integer(sfcio_intkind) input logical unit
!     cfname            character(*) input filename
!     head              type(sfcio_head) input header information
!     data              type(sfcio_data) input data fields
!     iret              integer(sfcio_intkind) output return code
!
!   sfcio_aldbta      allocate longreal data fields
!     head              type(sfcio_head) input header information
!     dbta              type(sfcio_dbta) output longreal data fields
!     iret              integer(sfcio_intkind) output return code
!
!   sfcio_axdbta      deallocate longreal data fields
!     dbta              type(sfcio_dbta) output longreal data fields
!     iret              integer(sfcio_intkind) output return code
!
!   sfcio_srdbta      read longreal data fields with sequential i/o
!     lu                integer(sfcio_intkind) input logical unit
!     head              type(sfcio_head) input header information
!     dbta              type(sfcio_dbta) output longreal data fields
!     iret              integer(sfcio_intkind) output return code
!
!   sfcio_swdbta      write longreal data fields with sequential i/o
!     lu                integer(sfcio_intkind) input logical unit
!     head              type(sfcio_head) input header information
!     dbta              type(sfcio_dbta) input longreal data fields
!     iret              integer(sfcio_intkind) output return code
!
! remarks:
!   (1) here's the supported surface file formats.
!       for ivs=199802 (read-only):
!         on85 label (32 bytes)
!         header information record containing
!           fhour, idate, lonb, latb, ivs (8 4-byte words)
!         tsea (lonb*latb 4-byte words)
!         smc (lonb*latb*lsoil 4-byte words)
!         sheleg (lonb*latb 4-byte words)
!         stc (lonb*latb*lsoil 4-byte words)
!         tg3 (lonb*latb 4-byte words)
!         zorl (lonb*latb 4-byte words)
!         cv (lonb*latb 4-byte words)
!         cvb (lonb*latb 4-byte words)
!         cvt (lonb*latb 4-byte words)
!         alvsf,alvwf,alnsf,alnwf (lonb*latb*4 4-byte words)
!         slmsk (lonb*latb 4-byte words)
!         vfrac (lonb*latb 4-byte words)
!         canopy (lonb*latb 4-byte words)
!         f10m (lonb*latb 4-byte words)
!         vtype (lonb*latb 4-byte words)
!         stype (lonb*latb 4-byte words)
!         facsf,facwf (lonb*latb*2 4-byte words)
!         uustar (lonb*latb 4-byte words)
!         ffmm (lonb*latb 4-byte words)
!         ffhh (lonb*latb 4-byte words)
!       for ivs=200004:
!         on85 label (32 bytes)
!         header information record containing
!           fhour, idate, lonb, latb, ivs, lpl (8+latb/2 4-byte words)
!         tsea (lonb*latb 4-byte words)
!         smc (lonb*latb*lsoil 4-byte words)
!         sheleg (lonb*latb 4-byte words)
!         stc (lonb*latb*lsoil 4-byte words)
!         tg3 (lonb*latb 4-byte words)
!         zorl (lonb*latb 4-byte words)
!         cv (lonb*latb 4-byte words)
!         cvb (lonb*latb 4-byte words)
!         cvt (lonb*latb 4-byte words)
!         alvsf,alvwf,alnsf,alnwf (lonb*latb*4 4-byte words)
!         slmsk (lonb*latb 4-byte words)
!         vfrac (lonb*latb 4-byte words)
!         canopy (lonb*latb 4-byte words)
!         f10m (lonb*latb 4-byte words)
!         vtype (lonb*latb 4-byte words)
!         stype (lonb*latb 4-byte words)
!         facsf,facwf (lonb*latb*2 4-byte words)
!         uustar (lonb*latb 4-byte words)
!         ffmm (lonb*latb 4-byte words)
!         ffhh (lonb*latb 4-byte words)
!       for ivs=200412 (read-only):
!         on85 label (32 bytes)
!         header information record containing
!           fhour, idate, lonb, latb, ivs, lpl (8+latb/2 4-byte words)
!         tsea (lonb*latb 4-byte words)
!         smc (lonb*latb*lsoil 4-byte words)
!         sheleg (lonb*latb 4-byte words)
!         stc (lonb*latb*lsoil 4-byte words)
!         tg3 (lonb*latb 4-byte words)
!         zorl (lonb*latb 4-byte words)
!         cv (lonb*latb 4-byte words)
!         cvb (lonb*latb 4-byte words)
!         cvt (lonb*latb 4-byte words)
!         alvsf,alvwf,alnsf,alnwf (lonb*latb*4 4-byte words)
!         slmsk (lonb*latb 4-byte words)
!         vfrac (lonb*latb 4-byte words)
!         canopy (lonb*latb 4-byte words)
!         f10m (lonb*latb 4-byte words)
!         vtype (lonb*latb 4-byte words)
!         stype (lonb*latb 4-byte words)
!         facsf,facwf (lonb*latb*2 4-byte words)
!         uustar (lonb*latb 4-byte words)
!         ffmm (lonb*latb 4-byte words)
!         ffhh (lonb*latb 4-byte words)
!         hice (lonb*latb 4-byte words)
!         fice (lonb*latb 4-byte words)
!         tisfc (lonb*latb 4-byte words)
!         tprcp (lonb*latb 4-byte words)
!         srflag (lonb*latb 4-byte words)
!         snwdph (lonb*latb 4-byte words)
!         slc (lonb*latb*lsoil 4-byte words)
!         shdmin (lonb*latb 4-byte words)
!         shdmax (lonb*latb 4-byte words)
!         slope (lonb*latb 4-byte words)
!         snoalb (lonb*latb 4-byte words)
!       for ivs=200501:
!         label containing
!           'gfs ','sfc ',ivs,nhead,ndata,reserved(3) (8 4-byte words)
!         header records
!           lhead(nhead),ldata(ndata) (nhead+ndata 4-byte words)
!           fhour, idate(4), lonb, latb, lsoil  (8 4-byte words)
!           lpl  (latb/2 4-byte words)
!           zsoil  (lsoil 4-byte words)
!         data records
!           slmsk (lonb*latb 4-byte words)
!           orog (lonb*latb 4-byte words)
!           tsea (lonb*latb 4-byte words)
!           sheleg (lonb*latb 4-byte words)
!           tg3 (lonb*latb 4-byte words)
!           zorl (lonb*latb 4-byte words)
!           alvsf (lonb*latb 4-byte words)
!           alvwf (lonb*latb 4-byte words)
!           alnsf (lonb*latb 4-byte words)
!           alnwf (lonb*latb 4-byte words)
!           vfrac (lonb*latb 4-byte words)
!           canopy (lonb*latb 4-byte words)
!           f10m (lonb*latb 4-byte words)
!           vtype (lonb*latb 4-byte words)
!           stype (lonb*latb 4-byte words)
!           facsf (lonb*latb 4-byte words)
!           facwf (lonb*latb 4-byte words)
!           uustar (lonb*latb 4-byte words)
!           ffmm (lonb*latb 4-byte words)
!           ffhh (lonb*latb 4-byte words)
!           hice (lonb*latb 4-byte words)
!           fice (lonb*latb 4-byte words)
!           tprcp (lonb*latb 4-byte words)
!           srflag (lonb*latb 4-byte words)
!           snwdph (lonb*latb 4-byte words)
!           shdmin (lonb*latb 4-byte words)
!           shdmax (lonb*latb 4-byte words)
!           slope (lonb*latb 4-byte words)
!           snoalb (lonb*latb 4-byte words)
!           lsoil stc (lonb*latb 4-byte words)
!           lsoil smc (lonb*latb 4-byte words)
!           lsoil slc (lonb*latb 4-byte words)
!       for ivs=200509:
!         label containing
!           'gfs ','sfc ',ivs,nhead,ndata,reserved(3) (8 4-byte words)
!         header records
!           lhead(nhead),ldata(ndata) (nhead+ndata 4-byte words)
!           fhour, idate(4), lonb, latb, lsoil, irealf,
!             reserved(16)  (25 4-byte words)
!           lpl  (latb/2 4-byte words)
!           zsoil  (lsoil 4-byte words)
!         data records
!           slmsk (lonb*latb 4-byte words)
!           orog (lonb*latb 4-byte words)
!           tsea (lonb*latb 4-byte words)
!           sheleg (lonb*latb 4-byte words)
!           tg3 (lonb*latb 4-byte words)
!           zorl (lonb*latb 4-byte words)
!           alvsf (lonb*latb 4-byte words)
!           alvwf (lonb*latb 4-byte words)
!           alnsf (lonb*latb 4-byte words)
!           alnwf (lonb*latb 4-byte words)
!           vfrac (lonb*latb 4-byte words)
!           canopy (lonb*latb 4-byte words)
!           f10m (lonb*latb 4-byte words)
!           t2m (lonb*latb 4-byte words)
!           q2m (lonb*latb 4-byte words)
!           vtype (lonb*latb 4-byte words)
!           stype (lonb*latb 4-byte words)
!           facsf (lonb*latb 4-byte words)
!           facwf (lonb*latb 4-byte words)
!           uustar (lonb*latb 4-byte words)
!           ffmm (lonb*latb 4-byte words)
!           ffhh (lonb*latb 4-byte words)
!           hice (lonb*latb 4-byte words)
!           fice (lonb*latb 4-byte words)
!           tisfc (lonb*latb 4-byte words)
!           tprcp (lonb*latb 4-byte words)
!           srflag (lonb*latb 4-byte words)
!           snwdph (lonb*latb 4-byte words)
!           shdmin (lonb*latb 4-byte words)
!           shdmax (lonb*latb 4-byte words)
!           slope (lonb*latb 4-byte words)
!           snoalb (lonb*latb 4-byte words)
!           lsoil stc (lonb*latb 4-byte words)
!           lsoil smc (lonb*latb 4-byte words)
!           lsoil slc (lonb*latb 4-byte words)
!
!   (2) possible return codes:
!          0   successful call
!         -1   open or close i/o error
!         -2   header record i/o error or unrecognized version
!         -3   allocation or deallocation error
!         -4   data record i/o error
!         -5   insufficient data dimensions allocated
!
! examples:
!   (1) read the entire surface file 'sfcf24' and
!       print out the northernmost surface temperature at greenwich.
!
!     use sfcio_module
!     type(sfcio_head):: head
!     type(sfcio_data):: data
!     call sfcio_srohdc(11,'sfcf24',head,data,iret)
!     print '(f8.2)',data%tsea(1,1)
!     end
!
! attributes:
!   language: fortran 90
!
!$$$
  implicit none
  private
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! public variables
  integer,parameter,public:: sfcio_lhead1=32
  integer,parameter,public:: sfcio_intkind=4,sfcio_realkind=4,sfcio_dblekind=8
  real(sfcio_realkind),parameter,public:: sfcio_realfill=-9999.
  real(sfcio_dblekind),parameter,public:: sfcio_dblefill=sfcio_realfill
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! public types
  type,public:: sfcio_head
    character(sfcio_lhead1):: clabsfc='                                '
    real(sfcio_realkind):: fhour=0.
    integer(sfcio_intkind):: idate(4)=(/0,0,0,0/),latb=0,lonb=0,lsoil=0,ivs=0
    integer(sfcio_intkind):: irealf=1
    integer(sfcio_intkind),allocatable:: lpl(:)
    real(sfcio_realkind),allocatable:: zsoil(:)
  end type
  type,public:: sfcio_data
    real(sfcio_realkind),pointer:: tsea(:,:)=>null()
    real(sfcio_realkind),pointer:: smc(:,:,:)=>null()
    real(sfcio_realkind),pointer:: sheleg(:,:)=>null()
    real(sfcio_realkind),pointer:: stc(:,:,:)=>null()
    real(sfcio_realkind),pointer:: tg3(:,:)=>null()
    real(sfcio_realkind),pointer:: zorl(:,:)=>null()
    real(sfcio_realkind),pointer:: cv(:,:)=>null()
    real(sfcio_realkind),pointer:: cvb(:,:)=>null()
    real(sfcio_realkind),pointer:: cvt(:,:)=>null()
    real(sfcio_realkind),pointer:: alvsf(:,:)=>null()
    real(sfcio_realkind),pointer:: alvwf(:,:)=>null()
    real(sfcio_realkind),pointer:: alnsf(:,:)=>null()
    real(sfcio_realkind),pointer:: alnwf(:,:)=>null()
    real(sfcio_realkind),pointer:: slmsk(:,:)=>null()
    real(sfcio_realkind),pointer:: vfrac(:,:)=>null()
    real(sfcio_realkind),pointer:: canopy(:,:)=>null()
    real(sfcio_realkind),pointer:: f10m(:,:)=>null()
    real(sfcio_realkind),pointer:: t2m(:,:)=>null()
    real(sfcio_realkind),pointer:: q2m(:,:)=>null()
    real(sfcio_realkind),pointer:: vtype(:,:)=>null()
    real(sfcio_realkind),pointer:: stype(:,:)=>null()
    real(sfcio_realkind),pointer:: facsf(:,:)=>null()
    real(sfcio_realkind),pointer:: facwf(:,:)=>null()
    real(sfcio_realkind),pointer:: uustar(:,:)=>null()
    real(sfcio_realkind),pointer:: ffmm(:,:)=>null()
    real(sfcio_realkind),pointer:: ffhh(:,:)=>null()
    real(sfcio_realkind),pointer:: hice(:,:)=>null()
    real(sfcio_realkind),pointer:: fice(:,:)=>null()
    real(sfcio_realkind),pointer:: tisfc(:,:)=>null()
    real(sfcio_realkind),pointer:: tprcp(:,:)=>null()
    real(sfcio_realkind),pointer:: srflag(:,:)=>null()
    real(sfcio_realkind),pointer:: snwdph(:,:)=>null()
    real(sfcio_realkind),pointer:: slc(:,:,:)=>null()
    real(sfcio_realkind),pointer:: shdmin(:,:)=>null()
    real(sfcio_realkind),pointer:: shdmax(:,:)=>null()
    real(sfcio_realkind),pointer:: slope(:,:)=>null()
    real(sfcio_realkind),pointer:: snoalb(:,:)=>null()
    real(sfcio_realkind),pointer:: orog(:,:)=>null()
  end type
  type,public:: sfcio_dbta
    real(sfcio_dblekind),pointer:: tsea(:,:)=>null()
    real(sfcio_dblekind),pointer:: smc(:,:,:)=>null()
    real(sfcio_dblekind),pointer:: sheleg(:,:)=>null()
    real(sfcio_dblekind),pointer:: stc(:,:,:)=>null()
    real(sfcio_dblekind),pointer:: tg3(:,:)=>null()
    real(sfcio_dblekind),pointer:: zorl(:,:)=>null()
    real(sfcio_dblekind),pointer:: cv(:,:)=>null()
    real(sfcio_dblekind),pointer:: cvb(:,:)=>null()
    real(sfcio_dblekind),pointer:: cvt(:,:)=>null()
    real(sfcio_dblekind),pointer:: alvsf(:,:)=>null()
    real(sfcio_dblekind),pointer:: alvwf(:,:)=>null()
    real(sfcio_dblekind),pointer:: alnsf(:,:)=>null()
    real(sfcio_dblekind),pointer:: alnwf(:,:)=>null()
    real(sfcio_dblekind),pointer:: slmsk(:,:)=>null()
    real(sfcio_dblekind),pointer:: vfrac(:,:)=>null()
    real(sfcio_dblekind),pointer:: canopy(:,:)=>null()
    real(sfcio_dblekind),pointer:: f10m(:,:)=>null()
    real(sfcio_dblekind),pointer:: t2m(:,:)=>null()
    real(sfcio_dblekind),pointer:: q2m(:,:)=>null()
    real(sfcio_dblekind),pointer:: vtype(:,:)=>null()
    real(sfcio_dblekind),pointer:: stype(:,:)=>null()
    real(sfcio_dblekind),pointer:: facsf(:,:)=>null()
    real(sfcio_dblekind),pointer:: facwf(:,:)=>null()
    real(sfcio_dblekind),pointer:: uustar(:,:)=>null()
    real(sfcio_dblekind),pointer:: ffmm(:,:)=>null()
    real(sfcio_dblekind),pointer:: ffhh(:,:)=>null()
    real(sfcio_dblekind),pointer:: hice(:,:)=>null()
    real(sfcio_dblekind),pointer:: fice(:,:)=>null()
    real(sfcio_dblekind),pointer:: tisfc(:,:)=>null()
    real(sfcio_dblekind),pointer:: tprcp(:,:)=>null()
    real(sfcio_dblekind),pointer:: srflag(:,:)=>null()
    real(sfcio_dblekind),pointer:: snwdph(:,:)=>null()
    real(sfcio_dblekind),pointer:: slc(:,:,:)=>null()
    real(sfcio_dblekind),pointer:: shdmin(:,:)=>null()
    real(sfcio_dblekind),pointer:: shdmax(:,:)=>null()
    real(sfcio_dblekind),pointer:: slope(:,:)=>null()
    real(sfcio_dblekind),pointer:: snoalb(:,:)=>null()
    real(sfcio_dblekind),pointer:: orog(:,:)=>null()
  end type
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! public subprograms
  public sfcio_sropen,sfcio_swopen,sfcio_sclose,sfcio_srhead,sfcio_swhead
  public sfcio_alhead,sfcio_aldata,sfcio_axdata,sfcio_srdata,sfcio_swdata
  public sfcio_aldbta,sfcio_axdbta,sfcio_srdbta,sfcio_swdbta
  public sfcio_srohdc,sfcio_swohdc
  interface sfcio_srohdc
    module procedure sfcio_srohdca,sfcio_srohdcb
  end interface
  interface sfcio_swohdc
    module procedure sfcio_swohdca,sfcio_swohdcb
  end interface
contains
!-------------------------------------------------------------------------------
  subroutine sfcio_sropen(lu,cfname,iret)
    implicit none
    integer(sfcio_intkind),intent(in):: lu
    character*(*),intent(in):: cfname
    integer(sfcio_intkind),intent(out):: iret
    integer ios
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    open(lu,file=cfname,form='unformatted',&
         status='old',action='read',iostat=ios)
    iret=ios
    if(iret.ne.0) iret=-1
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sfcio_swopen(lu,cfname,iret)
    implicit none
    integer(sfcio_intkind),intent(in):: lu
    character*(*),intent(in):: cfname
    integer(sfcio_intkind),intent(out):: iret
    integer ios
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    open(lu,file=cfname,form='unformatted',&
         status='unknown',action='readwrite',iostat=ios)
    iret=ios
    if(iret.ne.0) iret=-1
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sfcio_sclose(lu,iret)
    implicit none
    integer(sfcio_intkind),intent(in):: lu
    integer(sfcio_intkind),intent(out):: iret
    integer ios
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    close(lu,iostat=ios)
    iret=ios
    if(iret.ne.0) iret=-1
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sfcio_srhead(lu,head,iret)
    implicit none
    integer(sfcio_intkind),intent(in):: lu
    type(sfcio_head),intent(out):: head
    integer(sfcio_intkind),intent(out):: iret
    integer:: ios
    character(4):: cgfs,csfc
    integer(sfcio_intkind):: nhead,ndata,nresv(3)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=-2
    rewind lu
    read(lu,iostat=ios) head%clabsfc
    if(ios.ne.0) return
     if(head%clabsfc(1:8).eq.'GFS SFC ' .or. head%clabsfc(1:8).eq.' SFG CFS') then  ! modern surface file
!port    if(head%clabsfc(1:8).eq.'gfs sfc ') then  ! modern surface file
! commented by moorthi
!     if(head%clabsfc(1:8).eq.'gfs sfc ')  then
!        print *,'  modern big endian surface file'
!     else
!        print *,'  modern little endian surface file'
!     endif

      rewind lu
      read(lu,iostat=ios) cgfs,csfc,head%ivs,nhead,ndata,nresv
      if(ios.ne.0) return
      if(head%ivs.eq.200509) then
        read(lu,iostat=ios)
        if(ios.ne.0) return
        read(lu,iostat=ios) head%fhour,head%idate,head%lonb,head%latb,&
                            head%lsoil,head%irealf
        if(ios.ne.0) return
        call sfcio_alhead(head,ios)
        if(ios.ne.0) return
        read(lu,iostat=ios) head%lpl
        if(ios.ne.0) return
        read(lu,iostat=ios) head%zsoil
        if(ios.ne.0) return
      elseif(head%ivs.eq.200501) then
        read(lu,iostat=ios)
        if(ios.ne.0) return
        read(lu,iostat=ios) head%fhour,head%idate,head%lonb,head%latb,head%lsoil
        if(ios.ne.0) return
        call sfcio_alhead(head,ios)
        if(ios.ne.0) return
        read(lu,iostat=ios) head%lpl
        if(ios.ne.0) return
        read(lu,iostat=ios) head%zsoil
        if(ios.ne.0) return
      else
        return
      endif
    else
      read(lu,iostat=ios) head%fhour,head%idate,head%lonb,head%latb,head%ivs
      if(ios.ne.0) return
      if(head%ivs.eq.199802) then
        head%lsoil=2
        call sfcio_alhead(head,ios)
        if(ios.ne.0) return
        head%lpl=head%lonb
        head%zsoil=(/-0.1,-2.0/)
      elseif(head%ivs.eq.200004) then
        head%lsoil=2
        call sfcio_alhead(head,ios)
        if(ios.ne.0) return
        rewind lu
        read(lu) head%clabsfc
        read(lu,iostat=ios) head%fhour,head%idate,head%lonb,head%latb,head%ivs,&
                            head%lpl
        if(ios.ne.0) return
        head%zsoil=(/-0.1,-2.0/)
      elseif(head%ivs.eq.200412) then
        head%lsoil=4
        call sfcio_alhead(head,ios)
        if(ios.ne.0) return
        rewind lu
        read(lu) head%clabsfc
        read(lu,iostat=ios) head%fhour,head%idate,head%lonb,head%latb,head%ivs,&
                            head%lpl
        if(ios.ne.0) return
        head%zsoil=(/-0.1,-0.4,-1.0,-2.0/)
      else
        return
      endif
      iret=0
    endif
    iret=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sfcio_swhead(lu,head,iret)
    implicit none
    integer(sfcio_intkind),intent(in):: lu
    type(sfcio_head),intent(in):: head
    integer(sfcio_intkind),intent(out):: iret
    integer:: ios
    integer i
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=-2
    if(head%ivs.eq.200509) then
      rewind lu
      write(lu,iostat=ios) 'GFS SFC ',head%ivs,5,29+3*head%lsoil,0,0,0
      if(ios.ne.0) return
      write(lu,iostat=ios) 4*(/8,5+29+3*head%lsoil,25,head%latb/2,head%lsoil/),&
                           4*head%irealf*(/(head%lonb*head%latb,&
                                            i=1,29+3*head%lsoil)/)
      if(ios.ne.0) return
      write(lu,iostat=ios) head%fhour,head%idate,head%lonb,head%latb,&
                           head%lsoil,head%irealf,(0,i=1,16)
      if(ios.ne.0) return
      write(lu,iostat=ios) head%lpl
      if(ios.ne.0) return
      write(lu,iostat=ios) head%zsoil
      if(ios.ne.0) return
      iret=0
    elseif(head%ivs.eq.200501) then
      rewind lu
      write(lu,iostat=ios) 'GFS SFC ',head%ivs,5,29+3*head%lsoil,0,0,0
      if(ios.ne.0) return
      write(lu,iostat=ios) 4*(/8,5+29+3*head%lsoil,8,head%latb/2,head%lsoil/),&
                           4*(/(head%lonb*head%latb,i=1,29+3*head%lsoil)/)
      if(ios.ne.0) return
      write(lu,iostat=ios) head%fhour,head%idate,head%lonb,head%latb,head%lsoil
      if(ios.ne.0) return
      write(lu,iostat=ios) head%lpl
      if(ios.ne.0) return
      write(lu,iostat=ios) head%zsoil
      if(ios.ne.0) return
      iret=0
    elseif(head%ivs.eq.200004.and.head%lsoil.eq.2) then
      rewind lu
      write(lu,iostat=ios) head%clabsfc
      if(ios.ne.0) return
      write(lu,iostat=ios) head%fhour,head%idate,head%lonb,head%latb,head%ivs,&
                           head%lpl
      if(ios.ne.0) return
      iret=0
    endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sfcio_alhead(head,iret,latb,lsoil)
    implicit none
    type(sfcio_head),intent(inout):: head
    integer(sfcio_intkind),intent(out):: iret
    integer(sfcio_intkind),optional,intent(in):: latb,lsoil
    integer dim1l,dim1z
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(present(latb)) then
      dim1l=(latb+1)/2
    else
      dim1l=(head%latb+1)/2
    endif
    if(present(lsoil)) then
      dim1z=lsoil
    else
      dim1z=head%lsoil
    endif
    if(allocated(head%lpl)) deallocate(head%lpl)
    if(allocated(head%zsoil)) deallocate(head%zsoil)
    allocate(head%lpl(dim1l),head%zsoil(dim1z),stat=iret)
    if(iret.eq.0) then
      head%lpl=0
      head%zsoil=sfcio_realfill
    endif
    if(iret.ne.0) iret=-3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sfcio_aldata(head,data,iret)
    implicit none
    type(sfcio_head),intent(in):: head
    type(sfcio_data),intent(inout):: data
    integer(sfcio_intkind),intent(out):: iret
    integer dim1,dim2,dim3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sfcio_axdata(data,iret)
    dim1=head%lonb
    dim2=head%latb
    dim3=head%lsoil
    allocate(&
      data%tsea(dim1,dim2),&
      data%smc(dim1,dim2,dim3),&
      data%sheleg(dim1,dim2),&
      data%stc(dim1,dim2,dim3),&
      data%tg3(dim1,dim2),&
      data%zorl(dim1,dim2),&
      data%cv(dim1,dim2),&
      data%cvb(dim1,dim2),&
      data%cvt(dim1,dim2),&
      data%alvsf(dim1,dim2),&
      data%alvwf(dim1,dim2),&
      data%alnsf(dim1,dim2),&
      data%alnwf(dim1,dim2),&
      data%slmsk(dim1,dim2),&
      data%vfrac(dim1,dim2),&
      data%canopy(dim1,dim2),&
      data%f10m(dim1,dim2),&
      data%t2m(dim1,dim2),&
      data%q2m(dim1,dim2),&
      data%vtype(dim1,dim2),&
      data%stype(dim1,dim2),&
      data%facsf(dim1,dim2),&
      data%facwf(dim1,dim2),&
      data%uustar(dim1,dim2),&
      data%ffmm(dim1,dim2),&
      data%ffhh(dim1,dim2),&
      data%hice(dim1,dim2),&
      data%fice(dim1,dim2),&
      data%tisfc(dim1,dim2),&
      data%tprcp(dim1,dim2),&
      data%srflag(dim1,dim2),&
      data%snwdph(dim1,dim2),&
      data%slc(dim1,dim2,dim3),&
      data%shdmin(dim1,dim2),&
      data%shdmax(dim1,dim2),&
      data%slope(dim1,dim2),&
      data%snoalb(dim1,dim2),&
      data%orog(dim1,dim2),&
      stat=iret)
    if(iret.ne.0) iret=-3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sfcio_axdata(data,iret)
    implicit none
    type(sfcio_data),intent(inout):: data
    integer(sfcio_intkind),intent(out):: iret
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    deallocate(&
      data%tsea,&
      data%smc,&
      data%sheleg,&
      data%stc,&
      data%tg3,&
      data%zorl,&
      data%cv,&
      data%cvb,&
      data%cvt,&
      data%alvsf,&
      data%alvwf,&
      data%alnsf,&
      data%alnwf,&
      data%slmsk,&
      data%vfrac,&
      data%canopy,&
      data%f10m,&
      data%t2m,&
      data%q2m,&
      data%vtype,&
      data%stype,&
      data%facsf,&
      data%facwf,&
      data%uustar,&
      data%ffmm,&
      data%ffhh,&
      data%hice,&
      data%fice,&
      data%tisfc,&
      data%tprcp,&
      data%srflag,&
      data%snwdph,&
      data%slc,&
      data%shdmin,&
      data%shdmax,&
      data%slope,&
      data%snoalb,&
      data%orog,&
      stat=iret)
    nullify(&
      data%tsea,&
      data%smc,&
      data%sheleg,&
      data%stc,&
      data%tg3,&
      data%zorl,&
      data%cv,&
      data%cvb,&
      data%cvt,&
      data%alvsf,&
      data%alvwf,&
      data%alnsf,&
      data%alnwf,&
      data%slmsk,&
      data%vfrac,&
      data%canopy,&
      data%f10m,&
      data%t2m,&
      data%q2m,&
      data%vtype,&
      data%stype,&
      data%facsf,&
      data%facwf,&
      data%uustar,&
      data%ffmm,&
      data%ffhh,&
      data%hice,&
      data%fice,&
      data%tisfc,&
      data%tprcp,&
      data%srflag,&
      data%snwdph,&
      data%slc,&
      data%shdmin,&
      data%shdmax,&
      data%slope,&
      data%snoalb,&
      data%orog)
    if(iret.ne.0) iret=-3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sfcio_srdata(lu,head,data,iret)
    implicit none
    integer(sfcio_intkind),intent(in):: lu
    type(sfcio_head),intent(in):: head
    type(sfcio_data),intent(inout):: data
    integer(sfcio_intkind),intent(out):: iret
    integer:: dim1,dim2,dim3,mdim1,mdim2,mdim3
    integer:: ios
    integer i
    type(sfcio_dbta) dbta
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    dim1=head%lonb
    dim2=head%latb
    dim3=head%lsoil
    mdim1=min(&
      size(data%tsea,1),&
      size(data%smc,1),&
      size(data%sheleg,1),&
      size(data%stc,1),&
      size(data%tg3,1),&
      size(data%zorl,1),&
      size(data%alvsf,1),&
      size(data%alvwf,1),&
      size(data%alnsf,1),&
      size(data%alnwf,1),&
      size(data%slmsk,1),&
      size(data%vfrac,1),&
      size(data%canopy,1),&
      size(data%f10m,1),&
      size(data%t2m,1),&
      size(data%q2m,1),&
      size(data%vtype,1),&
      size(data%stype,1),&
      size(data%facsf,1),&
      size(data%facwf,1),&
      size(data%uustar,1),&
      size(data%ffmm,1),&
      size(data%ffhh,1),&
      size(data%hice,1),&
      size(data%fice,1),&
      size(data%tisfc,1),&
      size(data%tprcp,1),&
      size(data%srflag,1),&
      size(data%snwdph,1),&
      size(data%slc,1),&
      size(data%shdmin,1),&
      size(data%shdmax,1),&
      size(data%slope,1),&
      size(data%snoalb,1),&
      size(data%orog,1))
    mdim2=min(&
      size(data%tsea,2),&
      size(data%smc,2),&
      size(data%sheleg,2),&
      size(data%stc,2),&
      size(data%tg3,2),&
      size(data%zorl,2),&
      size(data%alvsf,2),&
      size(data%alvwf,2),&
      size(data%alnsf,2),&
      size(data%alnwf,2),&
      size(data%slmsk,2),&
      size(data%vfrac,2),&
      size(data%canopy,2),&
      size(data%f10m,2),&
      size(data%t2m,2),&
      size(data%q2m,2),&
      size(data%vtype,2),&
      size(data%stype,2),&
      size(data%facsf,2),&
      size(data%facwf,2),&
      size(data%uustar,2),&
      size(data%ffmm,2),&
      size(data%ffhh,2),&
      size(data%hice,2),&
      size(data%fice,2),&
      size(data%tisfc,2),&
      size(data%tprcp,2),&
      size(data%srflag,2),&
      size(data%snwdph,2),&
      size(data%slc,2),&
      size(data%shdmin,2),&
      size(data%shdmax,2),&
      size(data%slope,2),&
      size(data%snoalb,2),&
      size(data%orog,2))
    mdim3=min(&
      size(data%smc,3),&
      size(data%stc,3),&
      size(data%slc,3))
    iret=-5
    if(mdim1.lt.dim1.or.&
       mdim2.lt.dim2.or.&
       mdim3.lt.dim3) return
    iret=-4
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    data%t2m(:dim1,:dim2)=sfcio_realfill
    data%q2m(:dim1,:dim2)=sfcio_realfill
    data%tisfc(:dim1,:dim2)=sfcio_realfill
    if(head%ivs.eq.200509) then
      if(head%irealf.ne.2) then
        read(lu,iostat=ios) data%slmsk(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios) data%orog(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios) data%tsea(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios) data%sheleg(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios) data%tg3(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios) data%zorl(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios) data%alvsf(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios) data%alvwf(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios) data%alnsf(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios) data%alnwf(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios) data%vfrac(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios) data%canopy(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios) data%f10m(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios) data%t2m(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios) data%q2m(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios) data%vtype(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios) data%stype(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios) data%facsf(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios) data%facwf(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios) data%uustar(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios) data%ffmm(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios) data%ffhh(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios) data%hice(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios) data%fice(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios) data%tisfc(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios) data%tprcp(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios) data%srflag(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios) data%snwdph(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios) data%shdmin(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios) data%shdmax(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios) data%slope(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios) data%snoalb(:dim1,:dim2)
        if(ios.ne.0) return
        do i=1,head%lsoil
          read(lu,iostat=ios) data%stc(:dim1,:dim2,i)
          if(ios.ne.0) return
        enddo
        do i=1,head%lsoil
          read(lu,iostat=ios) data%smc(:dim1,:dim2,i)
          if(ios.ne.0) return
        enddo
        do i=1,head%lsoil
          read(lu,iostat=ios) data%slc(:dim1,:dim2,i)
          if(ios.ne.0) return
        enddo
        data%cv(:dim1,:dim2)=sfcio_realfill
        data%cvb(:dim1,:dim2)=sfcio_realfill
        data%cvt(:dim1,:dim2)=sfcio_realfill
      else
        call sfcio_aldbta(head,dbta,iret)
        if(iret.ne.0) return
        call sfcio_srdbta(lu,head,dbta,iret)
        if(iret.ne.0) return
        data%tsea(:dim1,:dim2)=dbta%tsea(:dim1,:dim2)
        data%smc(:dim1,:dim2,:dim3)=dbta%smc(:dim1,:dim2,:dim3)
        data%sheleg(:dim1,:dim2)=dbta%sheleg(:dim1,:dim2)
        data%stc(:dim1,:dim2,:dim3)=dbta%stc(:dim1,:dim2,:dim3)
        data%tg3(:dim1,:dim2)=dbta%tg3(:dim1,:dim2)
        data%zorl(:dim1,:dim2)=dbta%zorl(:dim1,:dim2)
        data%cv(:dim1,:dim2)=dbta%cv(:dim1,:dim2)
        data%cvb(:dim1,:dim2)=dbta%cvb(:dim1,:dim2)
        data%cvt(:dim1,:dim2)=dbta%cvt(:dim1,:dim2)
        data%alvsf(:dim1,:dim2)=dbta%alvsf(:dim1,:dim2)
        data%alvwf(:dim1,:dim2)=dbta%alvwf(:dim1,:dim2)
        data%alnsf(:dim1,:dim2)=dbta%alnsf(:dim1,:dim2)
        data%alnwf(:dim1,:dim2)=dbta%alnwf(:dim1,:dim2)
        data%slmsk(:dim1,:dim2)=dbta%slmsk(:dim1,:dim2)
        data%vfrac(:dim1,:dim2)=dbta%vfrac(:dim1,:dim2)
        data%canopy(:dim1,:dim2)=dbta%canopy(:dim1,:dim2)
        data%f10m(:dim1,:dim2)=dbta%f10m(:dim1,:dim2)
        data%t2m(:dim1,:dim2)=dbta%t2m(:dim1,:dim2)
        data%q2m(:dim1,:dim2)=dbta%q2m(:dim1,:dim2)
        data%vtype(:dim1,:dim2)=dbta%vtype(:dim1,:dim2)
        data%stype(:dim1,:dim2)=dbta%stype(:dim1,:dim2)
        data%facsf(:dim1,:dim2)=dbta%facsf(:dim1,:dim2)
        data%facwf(:dim1,:dim2)=dbta%facwf(:dim1,:dim2)
        data%uustar(:dim1,:dim2)=dbta%uustar(:dim1,:dim2)
        data%ffmm(:dim1,:dim2)=dbta%ffmm(:dim1,:dim2)
        data%ffhh(:dim1,:dim2)=dbta%ffhh(:dim1,:dim2)
        data%hice(:dim1,:dim2)=dbta%hice(:dim1,:dim2)
        data%fice(:dim1,:dim2)=dbta%fice(:dim1,:dim2)
        data%tisfc(:dim1,:dim2)=dbta%tisfc(:dim1,:dim2)
        data%tprcp(:dim1,:dim2)=dbta%tprcp(:dim1,:dim2)
        data%srflag(:dim1,:dim2)=dbta%srflag(:dim1,:dim2)
        data%snwdph(:dim1,:dim2)=dbta%snwdph(:dim1,:dim2)
        data%slc(:dim1,:dim2,:dim3)=dbta%slc(:dim1,:dim2,:dim3)
        data%shdmin(:dim1,:dim2)=dbta%shdmin(:dim1,:dim2)
        data%shdmax(:dim1,:dim2)=dbta%shdmax(:dim1,:dim2)
        data%slope(:dim1,:dim2)=dbta%slope(:dim1,:dim2)
        data%snoalb(:dim1,:dim2)=dbta%snoalb(:dim1,:dim2)
        data%orog(:dim1,:dim2)=dbta%orog(:dim1,:dim2)
        call sfcio_axdbta(dbta,iret)
      endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    elseif(head%ivs.eq.200501.and.head%irealf.ne.2) then
      read(lu,iostat=ios) data%slmsk(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) data%orog(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) data%tsea(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) data%sheleg(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) data%tg3(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) data%zorl(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) data%alvsf(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) data%alvwf(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) data%alnsf(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) data%alnwf(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) data%vfrac(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) data%canopy(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) data%f10m(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) data%vtype(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) data%stype(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) data%facsf(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) data%facwf(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) data%uustar(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) data%ffmm(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) data%ffhh(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) data%hice(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) data%fice(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) data%tprcp(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) data%srflag(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) data%snwdph(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) data%shdmin(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) data%shdmax(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) data%slope(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) data%snoalb(:dim1,:dim2)
      if(ios.ne.0) return
      do i=1,head%lsoil
        read(lu,iostat=ios) data%stc(:dim1,:dim2,i)
        if(ios.ne.0) return
      enddo
      do i=1,head%lsoil
        read(lu,iostat=ios) data%smc(:dim1,:dim2,i)
        if(ios.ne.0) return
      enddo
      do i=1,head%lsoil
        read(lu,iostat=ios) data%slc(:dim1,:dim2,i)
        if(ios.ne.0) return
      enddo
      data%cv(:dim1,:dim2)=sfcio_realfill
      data%cvb(:dim1,:dim2)=sfcio_realfill
      data%cvt(:dim1,:dim2)=sfcio_realfill
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    else
      read(lu,iostat=ios) data%tsea(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) data%smc(:dim1,:dim2,:dim3)
      if(ios.ne.0) return
      read(lu,iostat=ios) data%sheleg(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) data%stc(:dim1,:dim2,:dim3)
      if(ios.ne.0) return
      read(lu,iostat=ios) data%tg3(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) data%zorl(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) data%cv(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) data%cvb(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) data%cvt(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) data%alvsf(:dim1,:dim2),&
                          data%alvwf(:dim1,:dim2),&
                          data%alnsf(:dim1,:dim2),&
                          data%alnwf(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) data%slmsk(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) data%vfrac(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) data%canopy(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) data%f10m(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) data%vtype(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) data%stype(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) data%facsf(:dim1,:dim2),&
                          data%facwf(:dim1,:dim2)
      if(ios.ne.0) return
      if(head%ivs.ge.200004) then
        read(lu,iostat=ios) data%uustar(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios) data%ffmm(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios) data%ffhh(:dim1,:dim2)
        if(ios.ne.0) return
      else
        data%uustar(:dim1,:dim2)=sfcio_realfill
        data%ffmm(:dim1,:dim2)=sfcio_realfill
        data%ffhh(:dim1,:dim2)=sfcio_realfill
      endif
      if(head%ivs.eq.200412) then
        read(lu,iostat=ios) data%hice(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios) data%fice(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios) data%tprcp(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios) data%srflag(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios) data%snwdph(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios) data%slc(:dim1,:dim2,:dim3)
        if(ios.ne.0) return
        read(lu,iostat=ios) data%shdmin(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios) data%shdmax(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios) data%slope(:dim1,:dim2)
        if(ios.ne.0) return
        read(lu,iostat=ios) data%snoalb(:dim1,:dim2)
        if(ios.ne.0) return
        data%orog(:dim1,:dim2)=sfcio_realfill
      else
        data%hice(:dim1,:dim2)=sfcio_realfill
        data%fice(:dim1,:dim2)=sfcio_realfill
        data%tprcp(:dim1,:dim2)=sfcio_realfill
        data%srflag(:dim1,:dim2)=sfcio_realfill
        data%snwdph(:dim1,:dim2)=sfcio_realfill
        data%slc(:dim1,:dim2,:dim3)=sfcio_realfill
        data%shdmin(:dim1,:dim2)=sfcio_realfill
        data%shdmax(:dim1,:dim2)=sfcio_realfill
        data%slope(:dim1,:dim2)=sfcio_realfill
        data%snoalb(:dim1,:dim2)=sfcio_realfill
        data%orog(:dim1,:dim2)=sfcio_realfill
      endif
    endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=0
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sfcio_swdata(lu,head,data,iret)
    implicit none
    integer(sfcio_intkind),intent(in):: lu
    type(sfcio_head),intent(in):: head
    type(sfcio_data),intent(in):: data
    integer(sfcio_intkind),intent(out):: iret
    integer:: dim1,dim2,dim3,mdim1,mdim2,mdim3
    integer:: ios
    integer i
    type(sfcio_dbta) dbta
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    dim1=head%lonb
    dim2=head%latb
    dim3=head%lsoil
    mdim1=min(&
      size(data%tsea,1),&
      size(data%smc,1),&
      size(data%sheleg,1),&
      size(data%stc,1),&
      size(data%tg3,1),&
      size(data%zorl,1),&
      size(data%alvsf,1),&
      size(data%alvwf,1),&
      size(data%alnsf,1),&
      size(data%alnwf,1),&
      size(data%slmsk,1),&
      size(data%vfrac,1),&
      size(data%canopy,1),&
      size(data%f10m,1),&
      size(data%t2m,1),&
      size(data%q2m,1),&
      size(data%vtype,1),&
      size(data%stype,1),&
      size(data%facsf,1),&
      size(data%facwf,1),&
      size(data%uustar,1),&
      size(data%ffmm,1),&
      size(data%ffhh,1),&
      size(data%hice,1),&
      size(data%fice,1),&
      size(data%tisfc,1),&
      size(data%tprcp,1),&
      size(data%srflag,1),&
      size(data%snwdph,1),&
      size(data%slc,1),&
      size(data%shdmin,1),&
      size(data%shdmax,1),&
      size(data%slope,1),&
      size(data%snoalb,1),&
      size(data%orog,1))
    mdim2=min(&
      size(data%tsea,2),&
      size(data%smc,2),&
      size(data%sheleg,2),&
      size(data%stc,2),&
      size(data%tg3,2),&
      size(data%zorl,2),&
      size(data%alvsf,2),&
      size(data%alvwf,2),&
      size(data%alnsf,2),&
      size(data%alnwf,2),&
      size(data%slmsk,2),&
      size(data%vfrac,2),&
      size(data%canopy,2),&
      size(data%f10m,2),&
      size(data%t2m,2),&
      size(data%q2m,2),&
      size(data%vtype,2),&
      size(data%stype,2),&
      size(data%facsf,2),&
      size(data%facwf,2),&
      size(data%uustar,2),&
      size(data%ffmm,2),&
      size(data%ffhh,2),&
      size(data%hice,2),&
      size(data%fice,2),&
      size(data%tisfc,2),&
      size(data%tprcp,2),&
      size(data%srflag,2),&
      size(data%snwdph,2),&
      size(data%slc,2),&
      size(data%shdmin,2),&
      size(data%shdmax,2),&
      size(data%slope,2),&
      size(data%snoalb,2),&
      size(data%orog,2))
    mdim3=min(&
      size(data%smc,3),&
      size(data%stc,3),&
      size(data%slc,3))
    iret=-5
    if(mdim1.lt.dim1.or.&
       mdim2.lt.dim2.or.&
       mdim3.lt.dim3) return
    iret=-4
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(head%ivs.eq.200509) then
      if(head%irealf.ne.2) then
        write(lu,iostat=ios) data%slmsk(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%orog(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%tsea(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%sheleg(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%tg3(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%zorl(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%alvsf(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%alvwf(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%alnsf(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%alnwf(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%vfrac(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%canopy(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%f10m(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%t2m(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%q2m(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%vtype(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%stype(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%facsf(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%facwf(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%uustar(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%ffmm(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%ffhh(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%hice(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%fice(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%tisfc(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%tprcp(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%srflag(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%snwdph(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%shdmin(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%shdmax(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%slope(:dim1,:dim2)
        if(ios.ne.0) return
        write(lu,iostat=ios) data%snoalb(:dim1,:dim2)
        if(ios.ne.0) return
        do i=1,head%lsoil
          write(lu,iostat=ios) data%stc(:dim1,:dim2,i)
          if(ios.ne.0) return
        enddo
        do i=1,head%lsoil
          write(lu,iostat=ios) data%smc(:dim1,:dim2,i)
          if(ios.ne.0) return
        enddo
        do i=1,head%lsoil
          write(lu,iostat=ios) data%slc(:dim1,:dim2,i)
          if(ios.ne.0) return
        enddo
      else
        call sfcio_aldbta(head,dbta,iret)
        if(iret.ne.0) return
        dbta%tsea(:dim1,:dim2)=data%tsea(:dim1,:dim2)
        dbta%smc(:dim1,:dim2,:dim3)=data%smc(:dim1,:dim2,:dim3)
        dbta%sheleg(:dim1,:dim2)=data%sheleg(:dim1,:dim2)
        dbta%stc(:dim1,:dim2,:dim3)=data%stc(:dim1,:dim2,:dim3)
        dbta%tg3(:dim1,:dim2)=data%tg3(:dim1,:dim2)
        dbta%zorl(:dim1,:dim2)=data%zorl(:dim1,:dim2)
        dbta%cv(:dim1,:dim2)=data%cv(:dim1,:dim2)
        dbta%cvb(:dim1,:dim2)=data%cvb(:dim1,:dim2)
        dbta%cvt(:dim1,:dim2)=data%cvt(:dim1,:dim2)
        dbta%alvsf(:dim1,:dim2)=data%alvsf(:dim1,:dim2)
        dbta%alvwf(:dim1,:dim2)=data%alvwf(:dim1,:dim2)
        dbta%alnsf(:dim1,:dim2)=data%alnsf(:dim1,:dim2)
        dbta%alnwf(:dim1,:dim2)=data%alnwf(:dim1,:dim2)
        dbta%slmsk(:dim1,:dim2)=data%slmsk(:dim1,:dim2)
        dbta%vfrac(:dim1,:dim2)=data%vfrac(:dim1,:dim2)
        dbta%canopy(:dim1,:dim2)=data%canopy(:dim1,:dim2)
        dbta%f10m(:dim1,:dim2)=data%f10m(:dim1,:dim2)
        dbta%t2m(:dim1,:dim2)=data%t2m(:dim1,:dim2)
        dbta%q2m(:dim1,:dim2)=data%q2m(:dim1,:dim2)
        dbta%vtype(:dim1,:dim2)=data%vtype(:dim1,:dim2)
        dbta%stype(:dim1,:dim2)=data%stype(:dim1,:dim2)
        dbta%facsf(:dim1,:dim2)=data%facsf(:dim1,:dim2)
        dbta%facwf(:dim1,:dim2)=data%facwf(:dim1,:dim2)
        dbta%uustar(:dim1,:dim2)=data%uustar(:dim1,:dim2)
        dbta%ffmm(:dim1,:dim2)=data%ffmm(:dim1,:dim2)
        dbta%ffhh(:dim1,:dim2)=data%ffhh(:dim1,:dim2)
        dbta%hice(:dim1,:dim2)=data%hice(:dim1,:dim2)
        dbta%fice(:dim1,:dim2)=data%fice(:dim1,:dim2)
        dbta%tisfc(:dim1,:dim2)=data%tisfc(:dim1,:dim2)
        dbta%tprcp(:dim1,:dim2)=data%tprcp(:dim1,:dim2)
        dbta%srflag(:dim1,:dim2)=data%srflag(:dim1,:dim2)
        dbta%snwdph(:dim1,:dim2)=data%snwdph(:dim1,:dim2)
        dbta%slc(:dim1,:dim2,:dim3)=data%slc(:dim1,:dim2,:dim3)
        dbta%shdmin(:dim1,:dim2)=data%shdmin(:dim1,:dim2)
        dbta%shdmax(:dim1,:dim2)=data%shdmax(:dim1,:dim2)
        dbta%slope(:dim1,:dim2)=data%slope(:dim1,:dim2)
        dbta%snoalb(:dim1,:dim2)=data%snoalb(:dim1,:dim2)
        dbta%orog(:dim1,:dim2)=data%orog(:dim1,:dim2)
        call sfcio_swdbta(lu,head,dbta,iret)
        if(iret.ne.0) return
        call sfcio_axdbta(dbta,iret)
      endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    elseif(head%ivs.eq.200501.and.head%irealf.ne.2) then
      write(lu,iostat=ios) data%slmsk(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) data%orog(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) data%tsea(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) data%sheleg(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) data%tg3(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) data%zorl(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) data%alvsf(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) data%alvwf(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) data%alnsf(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) data%alnwf(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) data%vfrac(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) data%canopy(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) data%f10m(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) data%vtype(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) data%stype(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) data%facsf(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) data%facwf(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) data%uustar(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) data%ffmm(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) data%ffhh(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) data%hice(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) data%fice(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) data%tprcp(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) data%srflag(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) data%snwdph(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) data%shdmin(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) data%shdmax(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) data%slope(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) data%snoalb(:dim1,:dim2)
      if(ios.ne.0) return
      do i=1,head%lsoil
        write(lu,iostat=ios) data%stc(:dim1,:dim2,i)
        if(ios.ne.0) return
      enddo
      do i=1,head%lsoil
        write(lu,iostat=ios) data%smc(:dim1,:dim2,i)
        if(ios.ne.0) return
      enddo
      do i=1,head%lsoil
        write(lu,iostat=ios) data%slc(:dim1,:dim2,i)
        if(ios.ne.0) return
      enddo
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    elseif(head%ivs.eq.200004.and.head%lsoil.eq.2) then
      write(lu,iostat=ios) data%tsea(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) data%smc(:dim1,:dim2,:dim3)
      if(ios.ne.0) return
      write(lu,iostat=ios) data%sheleg(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) data%stc(:dim1,:dim2,:dim3)
      if(ios.ne.0) return
      write(lu,iostat=ios) data%tg3(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) data%zorl(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) data%cv(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) data%cvb(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) data%cvt(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) data%alvsf(:dim1,:dim2),&
                           data%alvwf(:dim1,:dim2),&
                           data%alnsf(:dim1,:dim2),&
                           data%alnwf(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) data%slmsk(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) data%vfrac(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) data%canopy(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) data%f10m(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) data%vtype(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) data%stype(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) data%facsf(:dim1,:dim2),&
                           data%facwf(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) data%uustar(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) data%ffmm(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) data%ffhh(:dim1,:dim2)
      if(ios.ne.0) return
    endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=0
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sfcio_srohdca(lu,cfname,head,data,iret)
    implicit none
    integer(sfcio_intkind),intent(in):: lu
    character*(*),intent(in):: cfname
    type(sfcio_head),intent(inout):: head
    type(sfcio_data),intent(inout):: data
    integer(sfcio_intkind),intent(out):: iret
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sfcio_sropen(lu,cfname,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sfcio_srhead(lu,head,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sfcio_aldata(head,data,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sfcio_srdata(lu,head,data,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sfcio_sclose(lu,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sfcio_swohdca(lu,cfname,head,data,iret)
    implicit none
    integer(sfcio_intkind),intent(in):: lu
    character*(*),intent(in):: cfname
    type(sfcio_head),intent(in):: head
    type(sfcio_data),intent(in):: data
    integer(sfcio_intkind),intent(out):: iret
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sfcio_swopen(lu,cfname,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sfcio_swhead(lu,head,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sfcio_swdata(lu,head,data,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sfcio_sclose(lu,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sfcio_aldbta(head,dbta,iret)
    implicit none
    type(sfcio_head),intent(in):: head
    type(sfcio_dbta),intent(inout):: dbta
    integer(sfcio_intkind),intent(out):: iret
    integer dim1,dim2,dim3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sfcio_axdbta(dbta,iret)
    dim1=head%lonb
    dim2=head%latb
    dim3=head%lsoil
    allocate(&
      dbta%tsea(dim1,dim2),&
      dbta%smc(dim1,dim2,dim3),&
      dbta%sheleg(dim1,dim2),&
      dbta%stc(dim1,dim2,dim3),&
      dbta%tg3(dim1,dim2),&
      dbta%zorl(dim1,dim2),&
      dbta%cv(dim1,dim2),&
      dbta%cvb(dim1,dim2),&
      dbta%cvt(dim1,dim2),&
      dbta%alvsf(dim1,dim2),&
      dbta%alvwf(dim1,dim2),&
      dbta%alnsf(dim1,dim2),&
      dbta%alnwf(dim1,dim2),&
      dbta%slmsk(dim1,dim2),&
      dbta%vfrac(dim1,dim2),&
      dbta%canopy(dim1,dim2),&
      dbta%f10m(dim1,dim2),&
      dbta%t2m(dim1,dim2),&
      dbta%q2m(dim1,dim2),&
      dbta%vtype(dim1,dim2),&
      dbta%stype(dim1,dim2),&
      dbta%facsf(dim1,dim2),&
      dbta%facwf(dim1,dim2),&
      dbta%uustar(dim1,dim2),&
      dbta%ffmm(dim1,dim2),&
      dbta%ffhh(dim1,dim2),&
      dbta%hice(dim1,dim2),&
      dbta%fice(dim1,dim2),&
      dbta%tisfc(dim1,dim2),&
      dbta%tprcp(dim1,dim2),&
      dbta%srflag(dim1,dim2),&
      dbta%snwdph(dim1,dim2),&
      dbta%slc(dim1,dim2,dim3),&
      dbta%shdmin(dim1,dim2),&
      dbta%shdmax(dim1,dim2),&
      dbta%slope(dim1,dim2),&
      dbta%snoalb(dim1,dim2),&
      dbta%orog(dim1,dim2),&
      stat=iret)
    if(iret.ne.0) iret=-3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sfcio_axdbta(dbta,iret)
    implicit none
    type(sfcio_dbta),intent(inout):: dbta
    integer(sfcio_intkind),intent(out):: iret
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    deallocate(&
      dbta%tsea,&
      dbta%smc,&
      dbta%sheleg,&
      dbta%stc,&
      dbta%tg3,&
      dbta%zorl,&
      dbta%cv,&
      dbta%cvb,&
      dbta%cvt,&
      dbta%alvsf,&
      dbta%alvwf,&
      dbta%alnsf,&
      dbta%alnwf,&
      dbta%slmsk,&
      dbta%vfrac,&
      dbta%canopy,&
      dbta%f10m,&
      dbta%t2m,&
      dbta%q2m,&
      dbta%vtype,&
      dbta%stype,&
      dbta%facsf,&
      dbta%facwf,&
      dbta%uustar,&
      dbta%ffmm,&
      dbta%ffhh,&
      dbta%hice,&
      dbta%fice,&
      dbta%tisfc,&
      dbta%tprcp,&
      dbta%srflag,&
      dbta%snwdph,&
      dbta%slc,&
      dbta%shdmin,&
      dbta%shdmax,&
      dbta%slope,&
      dbta%snoalb,&
      dbta%orog,&
      stat=iret)
    nullify(&
      dbta%tsea,&
      dbta%smc,&
      dbta%sheleg,&
      dbta%stc,&
      dbta%tg3,&
      dbta%zorl,&
      dbta%cv,&
      dbta%cvb,&
      dbta%cvt,&
      dbta%alvsf,&
      dbta%alvwf,&
      dbta%alnsf,&
      dbta%alnwf,&
      dbta%slmsk,&
      dbta%vfrac,&
      dbta%canopy,&
      dbta%f10m,&
      dbta%t2m,&
      dbta%q2m,&
      dbta%vtype,&
      dbta%stype,&
      dbta%facsf,&
      dbta%facwf,&
      dbta%uustar,&
      dbta%ffmm,&
      dbta%ffhh,&
      dbta%hice,&
      dbta%fice,&
      dbta%tisfc,&
      dbta%tprcp,&
      dbta%srflag,&
      dbta%snwdph,&
      dbta%slc,&
      dbta%shdmin,&
      dbta%shdmax,&
      dbta%slope,&
      dbta%snoalb,&
      dbta%orog)
    if(iret.ne.0) iret=-3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sfcio_srdbta(lu,head,dbta,iret)
    implicit none
    integer(sfcio_intkind),intent(in):: lu
    type(sfcio_head),intent(in):: head
    type(sfcio_dbta),intent(inout):: dbta
    integer(sfcio_intkind),intent(out):: iret
    integer:: dim1,dim2,dim3,mdim1,mdim2,mdim3
    integer:: ios
    integer i
    type(sfcio_data):: data
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    dim1=head%lonb
    dim2=head%latb
    dim3=head%lsoil
    mdim1=min(&
      size(dbta%tsea,1),&
      size(dbta%smc,1),&
      size(dbta%sheleg,1),&
      size(dbta%stc,1),&
      size(dbta%tg3,1),&
      size(dbta%zorl,1),&
      size(dbta%alvsf,1),&
      size(dbta%alvwf,1),&
      size(dbta%alnsf,1),&
      size(dbta%alnwf,1),&
      size(dbta%slmsk,1),&
      size(dbta%vfrac,1),&
      size(dbta%canopy,1),&
      size(dbta%f10m,1),&
      size(dbta%t2m,1),&
      size(dbta%q2m,1),&
      size(dbta%vtype,1),&
      size(dbta%stype,1),&
      size(dbta%facsf,1),&
      size(dbta%facwf,1),&
      size(dbta%uustar,1),&
      size(dbta%ffmm,1),&
      size(dbta%ffhh,1),&
      size(dbta%hice,1),&
      size(dbta%fice,1),&
      size(dbta%tisfc,1),&
      size(dbta%tprcp,1),&
      size(dbta%srflag,1),&
      size(dbta%snwdph,1),&
      size(dbta%slc,1),&
      size(dbta%shdmin,1),&
      size(dbta%shdmax,1),&
      size(dbta%slope,1),&
      size(dbta%snoalb,1),&
      size(dbta%orog,1))
    mdim2=min(&
      size(dbta%tsea,2),&
      size(dbta%smc,2),&
      size(dbta%sheleg,2),&
      size(dbta%stc,2),&
      size(dbta%tg3,2),&
      size(dbta%zorl,2),&
      size(dbta%alvsf,2),&
      size(dbta%alvwf,2),&
      size(dbta%alnsf,2),&
      size(dbta%alnwf,2),&
      size(dbta%slmsk,2),&
      size(dbta%vfrac,2),&
      size(dbta%canopy,2),&
      size(dbta%f10m,2),&
      size(dbta%t2m,2),&
      size(dbta%q2m,2),&
      size(dbta%vtype,2),&
      size(dbta%stype,2),&
      size(dbta%facsf,2),&
      size(dbta%facwf,2),&
      size(dbta%uustar,2),&
      size(dbta%ffmm,2),&
      size(dbta%ffhh,2),&
      size(dbta%hice,2),&
      size(dbta%fice,2),&
      size(dbta%tisfc,2),&
      size(dbta%tprcp,2),&
      size(dbta%srflag,2),&
      size(dbta%snwdph,2),&
      size(dbta%slc,2),&
      size(dbta%shdmin,2),&
      size(dbta%shdmax,2),&
      size(dbta%slope,2),&
      size(dbta%snoalb,2),&
      size(dbta%orog,2))
    mdim3=min(&
      size(dbta%smc,3),&
      size(dbta%stc,3),&
      size(dbta%slc,3))
    iret=-5
    if(mdim1.lt.dim1.or.&
       mdim2.lt.dim2.or.&
       mdim3.lt.dim3) return
    iret=-4
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(head%irealf.ne.2) then
      call sfcio_aldata(head,data,iret)
      if(iret.ne.0) return
      call sfcio_srdata(lu,head,data,iret)
      if(iret.ne.0) return
      dbta%tsea(:dim1,:dim2)=data%tsea(:dim1,:dim2)
      dbta%smc(:dim1,:dim2,:dim3)=data%smc(:dim1,:dim2,:dim3)
      dbta%sheleg(:dim1,:dim2)=data%sheleg(:dim1,:dim2)
      dbta%stc(:dim1,:dim2,:dim3)=data%stc(:dim1,:dim2,:dim3)
      dbta%tg3(:dim1,:dim2)=data%tg3(:dim1,:dim2)
      dbta%zorl(:dim1,:dim2)=data%zorl(:dim1,:dim2)
      dbta%cv(:dim1,:dim2)=data%cv(:dim1,:dim2)
      dbta%cvb(:dim1,:dim2)=data%cvb(:dim1,:dim2)
      dbta%cvt(:dim1,:dim2)=data%cvt(:dim1,:dim2)
      dbta%alvsf(:dim1,:dim2)=data%alvsf(:dim1,:dim2)
      dbta%alvwf(:dim1,:dim2)=data%alvwf(:dim1,:dim2)
      dbta%alnsf(:dim1,:dim2)=data%alnsf(:dim1,:dim2)
      dbta%alnwf(:dim1,:dim2)=data%alnwf(:dim1,:dim2)
      dbta%slmsk(:dim1,:dim2)=data%slmsk(:dim1,:dim2)
      dbta%vfrac(:dim1,:dim2)=data%vfrac(:dim1,:dim2)
      dbta%canopy(:dim1,:dim2)=data%canopy(:dim1,:dim2)
      dbta%f10m(:dim1,:dim2)=data%f10m(:dim1,:dim2)
      dbta%t2m(:dim1,:dim2)=data%t2m(:dim1,:dim2)
      dbta%q2m(:dim1,:dim2)=data%q2m(:dim1,:dim2)
      dbta%vtype(:dim1,:dim2)=data%vtype(:dim1,:dim2)
      dbta%stype(:dim1,:dim2)=data%stype(:dim1,:dim2)
      dbta%facsf(:dim1,:dim2)=data%facsf(:dim1,:dim2)
      dbta%facwf(:dim1,:dim2)=data%facwf(:dim1,:dim2)
      dbta%uustar(:dim1,:dim2)=data%uustar(:dim1,:dim2)
      dbta%ffmm(:dim1,:dim2)=data%ffmm(:dim1,:dim2)
      dbta%ffhh(:dim1,:dim2)=data%ffhh(:dim1,:dim2)
      dbta%hice(:dim1,:dim2)=data%hice(:dim1,:dim2)
      dbta%fice(:dim1,:dim2)=data%fice(:dim1,:dim2)
      dbta%tisfc(:dim1,:dim2)=data%tisfc(:dim1,:dim2)
      dbta%tprcp(:dim1,:dim2)=data%tprcp(:dim1,:dim2)
      dbta%srflag(:dim1,:dim2)=data%srflag(:dim1,:dim2)
      dbta%snwdph(:dim1,:dim2)=data%snwdph(:dim1,:dim2)
      dbta%slc(:dim1,:dim2,:dim3)=data%slc(:dim1,:dim2,:dim3)
      dbta%shdmin(:dim1,:dim2)=data%shdmin(:dim1,:dim2)
      dbta%shdmax(:dim1,:dim2)=data%shdmax(:dim1,:dim2)
      dbta%slope(:dim1,:dim2)=data%slope(:dim1,:dim2)
      dbta%snoalb(:dim1,:dim2)=data%snoalb(:dim1,:dim2)
      dbta%orog(:dim1,:dim2)=data%orog(:dim1,:dim2)
      call sfcio_axdata(data,iret)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    elseif(head%ivs == 200509) then
      read(lu,iostat=ios) dbta%slmsk(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%orog(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%tsea(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%sheleg(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%tg3(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%zorl(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%alvsf(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%alvwf(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%alnsf(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%alnwf(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%vfrac(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%canopy(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%f10m(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%t2m(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%q2m(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%vtype(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%stype(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%facsf(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%facwf(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%uustar(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%ffmm(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%ffhh(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%hice(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%fice(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%tisfc(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%tprcp(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%srflag(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%snwdph(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%shdmin(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%shdmax(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%slope(:dim1,:dim2)
      if(ios.ne.0) return
      read(lu,iostat=ios) dbta%snoalb(:dim1,:dim2)
      if(ios.ne.0) return
      do i=1,head%lsoil
        read(lu,iostat=ios) dbta%stc(:dim1,:dim2,i)
        if(ios.ne.0) return
      enddo
      do i=1,head%lsoil
        read(lu,iostat=ios) dbta%smc(:dim1,:dim2,i)
        if(ios.ne.0) return
      enddo
      do i=1,head%lsoil
        read(lu,iostat=ios) dbta%slc(:dim1,:dim2,i)
        if(ios.ne.0) return
      enddo
      dbta%cv(:dim1,:dim2)=sfcio_realfill
      dbta%cvb(:dim1,:dim2)=sfcio_realfill
      dbta%cvt(:dim1,:dim2)=sfcio_realfill
    endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=0
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sfcio_swdbta(lu,head,dbta,iret)
    implicit none
    integer(sfcio_intkind),intent(in):: lu
    type(sfcio_head),intent(in):: head
    type(sfcio_dbta),intent(in):: dbta
    integer(sfcio_intkind),intent(out):: iret
    integer:: dim1,dim2,dim3,mdim1,mdim2,mdim3
    integer:: ios
    integer i
    type(sfcio_data):: data
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    dim1=head%lonb
    dim2=head%latb
    dim3=head%lsoil
    mdim1=min(&
      size(dbta%tsea,1),&
      size(dbta%smc,1),&
      size(dbta%sheleg,1),&
      size(dbta%stc,1),&
      size(dbta%tg3,1),&
      size(dbta%zorl,1),&
      size(dbta%alvsf,1),&
      size(dbta%alvwf,1),&
      size(dbta%alnsf,1),&
      size(dbta%alnwf,1),&
      size(dbta%slmsk,1),&
      size(dbta%vfrac,1),&
      size(dbta%canopy,1),&
      size(dbta%f10m,1),&
      size(dbta%t2m,1),&
      size(dbta%q2m,1),&
      size(dbta%vtype,1),&
      size(dbta%stype,1),&
      size(dbta%facsf,1),&
      size(dbta%facwf,1),&
      size(dbta%uustar,1),&
      size(dbta%ffmm,1),&
      size(dbta%ffhh,1),&
      size(dbta%hice,1),&
      size(dbta%fice,1),&
      size(dbta%tisfc,1),&
      size(dbta%tprcp,1),&
      size(dbta%srflag,1),&
      size(dbta%snwdph,1),&
      size(dbta%slc,1),&
      size(dbta%shdmin,1),&
      size(dbta%shdmax,1),&
      size(dbta%slope,1),&
      size(dbta%snoalb,1),&
      size(dbta%orog,1))
    mdim2=min(&
      size(dbta%tsea,2),&
      size(dbta%smc,2),&
      size(dbta%sheleg,2),&
      size(dbta%stc,2),&
      size(dbta%tg3,2),&
      size(dbta%zorl,2),&
      size(dbta%alvsf,2),&
      size(dbta%alvwf,2),&
      size(dbta%alnsf,2),&
      size(dbta%alnwf,2),&
      size(dbta%slmsk,2),&
      size(dbta%vfrac,2),&
      size(dbta%canopy,2),&
      size(dbta%f10m,2),&
      size(dbta%t2m,2),&
      size(dbta%q2m,2),&
      size(dbta%vtype,2),&
      size(dbta%stype,2),&
      size(dbta%facsf,2),&
      size(dbta%facwf,2),&
      size(dbta%uustar,2),&
      size(dbta%ffmm,2),&
      size(dbta%ffhh,2),&
      size(dbta%hice,2),&
      size(dbta%fice,2),&
      size(dbta%tisfc,2),&
      size(dbta%tprcp,2),&
      size(dbta%srflag,2),&
      size(dbta%snwdph,2),&
      size(dbta%slc,2),&
      size(dbta%shdmin,2),&
      size(dbta%shdmax,2),&
      size(dbta%slope,2),&
      size(dbta%snoalb,2),&
      size(dbta%orog,2))
    mdim3=min(&
      size(dbta%smc,3),&
      size(dbta%stc,3),&
      size(dbta%slc,3))
    iret=-5
    if(mdim1.lt.dim1.or.&
       mdim2.lt.dim2.or.&
       mdim3.lt.dim3) return
    iret=-4
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(head%irealf.ne.2) then
      call sfcio_aldata(head,data,iret)
      if(iret.ne.0) return
      data%tsea(:dim1,:dim2)=dbta%tsea(:dim1,:dim2)
      data%smc(:dim1,:dim2,:dim3)=dbta%smc(:dim1,:dim2,:dim3)
      data%sheleg(:dim1,:dim2)=dbta%sheleg(:dim1,:dim2)
      data%stc(:dim1,:dim2,:dim3)=dbta%stc(:dim1,:dim2,:dim3)
      data%tg3(:dim1,:dim2)=dbta%tg3(:dim1,:dim2)
      data%zorl(:dim1,:dim2)=dbta%zorl(:dim1,:dim2)
      data%cv(:dim1,:dim2)=dbta%cv(:dim1,:dim2)
      data%cvb(:dim1,:dim2)=dbta%cvb(:dim1,:dim2)
      data%cvt(:dim1,:dim2)=dbta%cvt(:dim1,:dim2)
      data%alvsf(:dim1,:dim2)=dbta%alvsf(:dim1,:dim2)
      data%alvwf(:dim1,:dim2)=dbta%alvwf(:dim1,:dim2)
      data%alnsf(:dim1,:dim2)=dbta%alnsf(:dim1,:dim2)
      data%alnwf(:dim1,:dim2)=dbta%alnwf(:dim1,:dim2)
      data%slmsk(:dim1,:dim2)=dbta%slmsk(:dim1,:dim2)
      data%vfrac(:dim1,:dim2)=dbta%vfrac(:dim1,:dim2)
      data%canopy(:dim1,:dim2)=dbta%canopy(:dim1,:dim2)
      data%f10m(:dim1,:dim2)=dbta%f10m(:dim1,:dim2)
      data%t2m(:dim1,:dim2)=dbta%t2m(:dim1,:dim2)
      data%q2m(:dim1,:dim2)=dbta%q2m(:dim1,:dim2)
      data%vtype(:dim1,:dim2)=dbta%vtype(:dim1,:dim2)
      data%stype(:dim1,:dim2)=dbta%stype(:dim1,:dim2)
      data%facsf(:dim1,:dim2)=dbta%facsf(:dim1,:dim2)
      data%facwf(:dim1,:dim2)=dbta%facwf(:dim1,:dim2)
      data%uustar(:dim1,:dim2)=dbta%uustar(:dim1,:dim2)
      data%ffmm(:dim1,:dim2)=dbta%ffmm(:dim1,:dim2)
      data%ffhh(:dim1,:dim2)=dbta%ffhh(:dim1,:dim2)
      data%hice(:dim1,:dim2)=dbta%hice(:dim1,:dim2)
      data%fice(:dim1,:dim2)=dbta%fice(:dim1,:dim2)
      data%tisfc(:dim1,:dim2)=dbta%tisfc(:dim1,:dim2)
      data%tprcp(:dim1,:dim2)=dbta%tprcp(:dim1,:dim2)
      data%srflag(:dim1,:dim2)=dbta%srflag(:dim1,:dim2)
      data%snwdph(:dim1,:dim2)=dbta%snwdph(:dim1,:dim2)
      data%slc(:dim1,:dim2,:dim3)=dbta%slc(:dim1,:dim2,:dim3)
      data%shdmin(:dim1,:dim2)=dbta%shdmin(:dim1,:dim2)
      data%shdmax(:dim1,:dim2)=dbta%shdmax(:dim1,:dim2)
      data%slope(:dim1,:dim2)=dbta%slope(:dim1,:dim2)
      data%snoalb(:dim1,:dim2)=dbta%snoalb(:dim1,:dim2)
      data%orog(:dim1,:dim2)=dbta%orog(:dim1,:dim2)
      call sfcio_swdata(lu,head,data,iret)
      if(iret.ne.0) return
      call sfcio_axdata(data,iret)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    elseif(head%ivs == 200509) then
      write(lu,iostat=ios) dbta%slmsk(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%orog(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%tsea(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%sheleg(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%tg3(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%zorl(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%alvsf(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%alvwf(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%alnsf(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%alnwf(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%vfrac(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%canopy(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%f10m(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%t2m(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%q2m(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%vtype(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%stype(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%facsf(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%facwf(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%uustar(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%ffmm(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%ffhh(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%hice(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%fice(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%tisfc(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%tprcp(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%srflag(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%snwdph(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%shdmin(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%shdmax(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%slope(:dim1,:dim2)
      if(ios.ne.0) return
      write(lu,iostat=ios) dbta%snoalb(:dim1,:dim2)
      if(ios.ne.0) return
      do i=1,head%lsoil
        write(lu,iostat=ios) dbta%stc(:dim1,:dim2,i)
        if(ios.ne.0) return
      enddo
      do i=1,head%lsoil
        write(lu,iostat=ios) dbta%smc(:dim1,:dim2,i)
        if(ios.ne.0) return
      enddo
      do i=1,head%lsoil
        write(lu,iostat=ios) dbta%slc(:dim1,:dim2,i)
        if(ios.ne.0) return
      enddo
    endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=0
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sfcio_srohdcb(lu,cfname,head,dbta,iret)
    implicit none
    integer(sfcio_intkind),intent(in):: lu
    character*(*),intent(in):: cfname
    type(sfcio_head),intent(inout):: head
    type(sfcio_dbta),intent(inout):: dbta
    integer(sfcio_intkind),intent(out):: iret
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sfcio_sropen(lu,cfname,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sfcio_srhead(lu,head,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sfcio_aldbta(head,dbta,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sfcio_srdbta(lu,head,dbta,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sfcio_sclose(lu,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sfcio_swohdcb(lu,cfname,head,dbta,iret)
    implicit none
    integer(sfcio_intkind),intent(in):: lu
    character*(*),intent(in):: cfname
    type(sfcio_head),intent(in):: head
    type(sfcio_dbta),intent(in):: dbta
    integer(sfcio_intkind),intent(out):: iret
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sfcio_swopen(lu,cfname,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sfcio_swhead(lu,head,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sfcio_swdbta(lu,head,dbta,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sfcio_sclose(lu,iret)
    if(iret.ne.0) return
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
end module

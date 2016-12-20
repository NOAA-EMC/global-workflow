      subroutine fill_psetfld(param_ofld,param_afld)
!
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    READCNTRLgrb2_xml  READS POST xml CONTROL FILE
!   PRGRMMR: J. WANG         ORG: NCEP/EMC   DATE: 12-01-27       
!     
! ABSTRACT:
!     THIS ROUTINE SET THE OUTPUT FIELD GRIB2 INFORMATION SUCH    
!     AS PARAMETER NAME, LEVEL TYPE ETC FROM POST AVAILABLE FIELD
!     TABLE
!     
! PROGRAM HISTORY LOG:
!   01_27_2012  Jun Wang - INITIAL CODE
!   04_03_2012  Jun Wang - Add table info
!   03_10_2015  Lin Gan  - Using flat file data
!     
! USAGE:    CALL READCNTRL_XML(kth,kpv,pv)
!   INPUT ARGUMENT LIST:
!     param_ofld: output field
!     param_afld: available field in POST
!
!   OUTPUT ARGUMENT LIST: 
!     param_ofld: output field
!     
!   OUTPUT FILES:
!     NONE
!     
!   SUBPROGRAMS CALLED:
!     UTILITIES:
!
!     LIBRARY:
!       MODULE:   - xml_data_post_t
!     
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN
!     MACHINE : IBM      
!$$$  
!
!     
!     INCLUDE ETA GRID DIMENSIONS.  SET/DERIVE PARAMETERS.
!
      use xml_perl_data, only: param_t
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       implicit none
!
!     DECLARE VARIABLES.
!     
      type(param_t) ,intent(inout) :: param_ofld
      type(param_t) ,intent(in)    :: param_afld
!
      integer :: i,j
!
!******************************************************************************
!     START HERE.
!     
!     GET field information
!
! pds template
      if(trim(param_afld%pdstmpl)/='tmpl4_0'.and.trim(param_ofld%pdstmpl)=='tmpl4_0') then
         param_ofld%pdstmpl=param_afld%pdstmpl
      endif
! pname
      if(trim(param_ofld%pname)==''.and.trim(param_afld%pname)/='') then
         param_ofld%pname=param_afld%pname
      endif
! stats_proc
      if(trim(param_ofld%stats_proc)==''.and.trim(param_afld%stats_proc)/='') then
         param_ofld%stats_proc=param_afld%stats_proc
      endif
! table_info
      if(trim(param_ofld%table_info)==''.and.trim(param_afld%table_info)/='') then
         param_ofld%table_info=param_afld%table_info
      endif
!
! fixed_sfc1_type
      if(trim(param_ofld%fixed_sfc1_type)==''.and.trim(param_afld%fixed_sfc1_type)/='') then
         param_ofld%fixed_sfc1_type=param_afld%fixed_sfc1_type
      endif
! scale_fact_fixed_sfc1
      if(size(param_ofld%scale_fact_fixed_sfc1)==0.and.size(param_afld%scale_fact_fixed_sfc1)/=0) then
        print *,'scale_fact,fld=',trim(param_ofld%shortname),size(param_afld%scale_fact_fixed_sfc1)
         nullify(param_ofld%scale_fact_fixed_sfc1)
         allocate(param_ofld%scale_fact_fixed_sfc1(1))
         param_ofld%scale_fact_fixed_sfc1(1)=param_afld%scale_fact_fixed_sfc1(1)
      endif
! level
      if(size(param_ofld%level)==0.and.size(param_afld%level)/=0) then
         nullify(param_ofld%level) 
         allocate(param_ofld%level(1))
         param_ofld%level(1)=param_afld%level(1)
      endif
! fixed_sfc2_type
      if(trim(param_ofld%fixed_sfc2_type)==''.and.trim(param_afld%fixed_sfc2_type)/='') then
         param_ofld%fixed_sfc2_type=param_afld%fixed_sfc2_type
      endif
! scale_fact_fixed_sfc2
      if(size(param_ofld%scale_fact_fixed_sfc2)==0.and.size(param_afld%scale_fact_fixed_sfc2)/=0) then
         nullify(param_ofld%scale_fact_fixed_sfc2)
         allocate(param_ofld%scale_fact_fixed_sfc2(1))
         param_ofld%scale_fact_fixed_sfc2(1)=param_afld%scale_fact_fixed_sfc2(1)
      endif
! level2
      if(size(param_ofld%level2)==0.and.size(param_afld%level2)/=0) then
         nullify(param_ofld%level2) 
         allocate(param_ofld%level2(1))
         param_ofld%level2(1)=param_afld%level2(1)
      endif
! aerosol type
      if(trim(param_ofld%aerosol_type)==''.and.trim(param_afld%aerosol_type)/='') then
         param_ofld%aerosol_type=param_afld%aerosol_type
      endif
! typ_intvl_size
      if(trim(param_ofld%typ_intvl_size)==''.and.trim(param_afld%typ_intvl_size)/='') then
         param_ofld%typ_intvl_size=param_afld%typ_intvl_size
      endif
! scale_fact_1st_size
      if(param_ofld%scale_fact_1st_size==0.and.param_afld%scale_fact_1st_size/=0) then
         param_ofld%scale_fact_1st_size=param_afld%scale_fact_1st_size
      endif
! scale_val_1st_size
      if(param_ofld%scale_val_1st_size==0.and.param_afld%scale_val_1st_size/=0) then
         param_ofld%scale_val_1st_size=param_afld%scale_val_1st_size
      endif
! scale_fact_2nd_size
      if(param_ofld%scale_fact_2nd_size==0.and.param_afld%scale_fact_2nd_size/=0) then
         param_ofld%scale_fact_2nd_size=param_afld%scale_fact_2nd_size
      endif
! scale_val_2nd_size
      if(param_ofld%scale_val_2nd_size==0.and.param_afld%scale_val_2nd_size/=0) then
         param_ofld%scale_val_2nd_size=param_afld%scale_val_2nd_size
      endif

! typ_intvl_wvlen
      if(trim(param_ofld%typ_intvl_wvlen)==''.and.trim(param_afld%typ_intvl_wvlen)/='') then
         param_ofld%typ_intvl_wvlen=param_afld%typ_intvl_wvlen
      endif
! scale_fact_1st_wvlen
      if(param_ofld%scale_fact_1st_wvlen==0.and.param_afld%scale_fact_1st_wvlen/=0) then
         param_ofld%scale_fact_1st_wvlen=param_afld%scale_fact_1st_wvlen
      endif
! scale_val_1st_wvlen
      if(param_ofld%scale_val_1st_wvlen==0.and.param_afld%scale_val_1st_wvlen/=0) then
         param_ofld%scale_val_1st_wvlen=param_afld%scale_val_1st_wvlen
      endif
! scale_fact_2nd_wvlen
      if(param_ofld%scale_fact_2nd_wvlen==0.and.param_afld%scale_fact_2nd_wvlen/=0) then
         param_ofld%scale_fact_2nd_wvlen=param_afld%scale_fact_2nd_wvlen
      endif
! scale_val_2nd_wvlen
      if(param_ofld%scale_val_2nd_wvlen==0.and.param_afld%scale_val_2nd_wvlen/=0) then
         param_ofld%scale_val_2nd_wvlen=param_afld%scale_val_2nd_wvlen
      endif
!
! scale
      if(size(param_ofld%scale)==0.and.size(param_afld%scale)/=0) then
        nullify(param_ofld%scale) 
        allocate(param_ofld%scale(size(param_afld%scale)))
        param_ofld%scale=param_afld%scale
      endif

!
      RETURN
      end subroutine fill_psetfld

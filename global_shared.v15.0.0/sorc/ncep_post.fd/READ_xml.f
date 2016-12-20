      SUBROUTINE READ_xml()
!
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    READCNTRLgrb2_xml  READS POST xml CONTROL FILE
!   PRGRMMR: J. WANG         ORG: NCEP/EMC   DATE: 12-01-27       
!     
! ABSTRACT:
!     THIS ROUTINE READS THE POST AVAILABLE FIELD XML FILE and 
!       POST CONTROL XML FILE. EACH SET OF OUTPUT FIELDS GOING TO ONE 
!       OUTPUT FILE WILL WILL BE SAVED AND PROCESSED LATER. IN OTHER
!       WORDS, POST CONTROL FILE WILL BE READ IN WHOLE ONCE.
!     
! PROGRAM HISTORY LOG:
!   01_27_2012  Jun Wang - INITIAL CODE
!   03_10_2015  Lin Gan  - Replace XML file with flat file implementation
!                           with parameter marshalling
!   07_08_2016 J. Carley - Clean up prints 
!     
! USAGE:    CALL READ_XML()
!   INPUT ARGUMENT LIST:
!     NONE
!
!   OUTPUT ARGUMENT LIST: 
!     NONE     - 
!     
!   OUTPUT FILES:
!     NONE
!     
!   SUBPROGRAMS CALLED:
!     UTILITIES:
!
!     LIBRARY:
!       COMMON   - RQSTFLDGRB2
!                  CTLBLK
!     
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN
!     MACHINE : IBM      
!$$$  
!
!     
!     INCLUDE ETA GRID DIMENSIONS.  SET/DERIVE PARAMETERS.
!
       use xml_perl_data,only: post_avblflds,paramset,read_postxconfig
       use grib2_module, only: num_pset
       use rqstfld_mod,only: num_post_afld,MXLVL,lvlsxml
       use CTLBLK_mod, only: me
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       implicit none
!
!     DECLARE VARIABLES.
!     
!******************************************************************************
!     START READCNTRL_XML HERE.
!     
!     READ post available field table
      if (me==0) write(0,*)'in readxml,bf readxml,size(post_avblflds%param)=', &
                            size(post_avblflds%param)
      call read_postxconfig()
      num_post_afld=size(paramset(1)%param)
      num_pset=size(paramset)
      if (me==0) write(0,*)'in readxml, aft read flat file.xml,num_post_afld=', &
                            num_post_afld
      if (me==0) write(0,*)'in readxml, aft read flat file.xml,num_pset=',num_pset


! LinGan below line removed because now we only read one flat file
!
!      if(size(post_avblflds%param)==0) then
!        call read_xml_file_post_t( 'post_avblflds.xml')
!        num_post_afld=size(post_avblflds%param)
!        allocate(lvlsxml(MXLVL,num_post_afld))
!      write(0,*)'in readxml, aft read post_avblflds.xml,num_post_afld=',num_post_afld
!      endif
!
!     READ post cntrl file
!      write(0,*)'in readxml,bf readxml,size(paramset)=',size(paramset)
!      if(size(paramset)==0) then
!        call read_xml_file_post_t( 'postcntrl.xml')
!        num_pset=size(paramset)
!        write(0,*)'in readxml, aft read postcntrl.xml,num_pset=',num_pset
!      endif
!

      RETURN
      end subroutine read_xml


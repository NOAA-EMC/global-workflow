        module xml_perl_data
!------------------------------------------------------------------------
!
! This module read in Perl XML processed flat file and 
!   handle parameter marshalling for existing POST program
!
! program log:
!   March, 2015    Lin Gan    Initial Code
!   July,  2016    J. Carley  Clean up prints 
!   
!------------------------------------------------------------------------
        implicit none
	  type param_t
	    integer                              :: post_avblfldidx=-9999
	    character(len=80)                    :: shortname=''
	    character(len=300)                   :: longname=''
	    integer                              :: mass_windpoint=1
	    character(len=30)                    :: pdstmpl='tmpl4_0'
	    character(len=30)                    :: pname=''
	    character(len=10)                    :: table_info=''
	    character(len=80)                    :: stats_proc=''
	    character(len=80)                    :: fixed_sfc1_type=''
            integer, dimension(:), pointer       :: scale_fact_fixed_sfc1 => null()
	    real, dimension(:), pointer          :: level => null()
	    character(len=80)                    :: fixed_sfc2_type=''
	    integer, dimension(:), pointer       :: scale_fact_fixed_sfc2 => null()
	    real, dimension(:), pointer          :: level2 => null()
	    character(len=80)                    :: aerosol_type=''
	    character(len=80)                    :: typ_intvl_size=''
 	    integer                              :: scale_fact_1st_size=0
	    real                                 :: scale_val_1st_size=0.0
	    integer                              :: scale_fact_2nd_size=0
	    real                                 :: scale_val_2nd_size=0.0
	    character(len=80)                    :: typ_intvl_wvlen=''
	    integer                              :: scale_fact_1st_wvlen=0
	    real                                 :: scale_val_1st_wvlen=0.0
	    integer                              :: scale_fact_2nd_wvlen=0
	    real                                 :: scale_val_2nd_wvlen=0.0
	    real, dimension(:), pointer          :: scale => null()
	    integer                              :: stat_miss_val=0
	    integer                              :: leng_time_range_prev=0
	    integer                              :: time_inc_betwn_succ_fld=0
	    character(len=80)                    :: type_of_time_inc=''
	    character(len=20)                    :: stat_unit_time_key_succ=''
	    character(len=20)                    :: bit_map_flag=''
          end type param_t

          type paramset_t
	    character(len=6)                     :: datset=''
	    integer                              :: grid_num=255
	    character(len=20)                    :: sub_center=''
	    character(len=20)                    :: version_no=''
	    character(len=20)                    :: local_table_vers_no=''
	    character(len=20)                    :: sigreftime=''
	    character(len=20)                    :: prod_status=''
	    character(len=20)                    :: data_type=''
	    character(len=20)                    :: gen_proc_type=''
	    character(len=30)                    :: time_range_unit=''
	    character(len=50)                    :: orig_center=''
	    character(len=30)                    :: gen_proc=''
	    character(len=50)                    :: packing_method=''
	    character(len=30)                    :: order_of_sptdiff='1st_ord_sptdiff'
	    character(len=20)                    :: field_datatype=''
	    character(len=30)                    :: comprs_type=''
            character(len=50)                    :: type_ens_fcst=''
            character(len=50)                    :: type_derived_fcst=''
            type(param_t), dimension(:), pointer :: param => null()
          end type paramset_t
 
          type post_avblfld_t
            type(param_t), dimension(:), pointer :: param => null()
          end type post_avblfld_t

          type (paramset_t), dimension(:), pointer :: paramset
          type (post_avblfld_t),save               :: post_avblflds

        contains
        subroutine read_postxconfig()

         use rqstfld_mod,only: num_post_afld,MXLVL,lvlsxml
         use CTLBLK_mod, only:tprec,tclod,trdlw,trdsw,tsrfc &
                              ,tmaxmin,td3d,me

! Read in the flat file postxconfig-NT.txt
! for current working parameters and param
	integer   paramset_count, param_count

! temp array count
        integer   cc
        integer   level_array_count
        integer   cv
        integer   level2_array_count
        integer   scale_array_count
        integer   i,j

! evil for empty default char "?"
        character(len=80)   testcharname
        character           dummy_char
        integer             testintname

! open the Post flat file
        open(UNIT=22,file="postxconfig-NT.txt",     &
             form="formatted", access="sequential", &
             status="old", position="rewind")

! Take the first line as paramset_count
	read(22,*)paramset_count

        if(me==0)write(0,*)'xml_perl_data read Post flat file'

! Allocate paramset array size
        if(me==0)write(0,*)'allocate paramset to :', paramset_count

        allocate(paramset(paramset_count))

! Take the second line as param_count (on n..1 down loop)
! stored as FILO

! Initialize num_post_afld here
     num_post_afld = 0

        do i = paramset_count, 1, -1
          read(22,*)param_count
          if(me==0)write(0,*)'allocate param to :', param_count

          allocate(paramset(i)%param(param_count))

! LinGan lvlsxml is now a sum of flat file read out
! Also allocate lvlsxml for rqstfld_mod
          num_post_afld = num_post_afld + param_count
          if(me==0)write(0,*)'sum num_post_afld :', num_post_afld

        end do
        
        if(allocated(lvlsxml)) deallocate(lvlsxml)
        allocate(lvlsxml(MXLVL,num_post_afld))

! For each paramset_count to read in all 16 control contain
        do i = 1, paramset_count
! allocate array size from param for current paramset
! filter_char_inp is to check if "?" is found 
!   then replace to empty string because it means no input. 

          read(22,*)paramset(i)%datset
          call filter_char_inp(paramset(i)%datset)

          param_count = size (paramset(i)%param)

          read(22,*)paramset(i)%grid_num
          read(22,*)paramset(i)%sub_center
            call filter_char_inp(paramset(i)%sub_center)
          read(22,*)paramset(i)%version_no
            call filter_char_inp(paramset(i)%version_no)
          read(22,*)paramset(i)%local_table_vers_no
            call filter_char_inp(paramset(i)%local_table_vers_no)
          read(22,*)paramset(i)%sigreftime
            call filter_char_inp(paramset(i)%sigreftime)
          read(22,*)paramset(i)%prod_status
            call filter_char_inp(paramset(i)%prod_status)
          read(22,*)paramset(i)%data_type
            call filter_char_inp(paramset(i)%data_type)
          read(22,*)paramset(i)%gen_proc_type
            call filter_char_inp(paramset(i)%gen_proc_type)
          if(me==0)print*,'gen_proc_type= ',paramset(i)%gen_proc_type
          read(22,*)paramset(i)%time_range_unit
            call filter_char_inp(paramset(i)%time_range_unit)
          read(22,*)paramset(i)%orig_center
            call filter_char_inp(paramset(i)%orig_center)
          read(22,*)paramset(i)%gen_proc
            call filter_char_inp(paramset(i)%gen_proc)
          read(22,*)paramset(i)%packing_method
            call filter_char_inp(paramset(i)%packing_method)
          read(22,*)paramset(i)%order_of_sptdiff
          read(22,*)paramset(i)%field_datatype
            call filter_char_inp(paramset(i)%field_datatype)
          read(22,*)paramset(i)%comprs_type
            call filter_char_inp(paramset(i)%comprs_type)
          if(me==0)print*,'finish reading comprs_type'
          if(paramset(i)%gen_proc_type=='ens_fcst')then
            read(22,*)paramset(i)%type_ens_fcst
            call filter_char_inp(paramset(i)%type_ens_fcst)
            tprec   = 6  ! always 6 hr bucket for gefs
            tclod   = tprec
            trdlw   = tprec
            trdsw   = tprec
            tsrfc   = tprec
            tmaxmin = tprec
            td3d    = tprec
          end if          
          if(me==0)print*,'type_ens_fcst= ',paramset(i)%type_ens_fcst 
! Loop param_count (param datas 161) for gfsprs
	  do j = 1, param_count
	    read(22,*)paramset(i)%param(j)%post_avblfldidx
            read(22,*)paramset(i)%param(j)%shortname
            read(22,'(A300)')paramset(i)%param(j)%longname
              call filter_char_inp(paramset(i)%param(j)%longname)

            read(22,*)paramset(i)%param(j)%mass_windpoint
            read(22,*)paramset(i)%param(j)%pdstmpl
            read(22,*)paramset(i)%param(j)%pname
              call filter_char_inp(paramset(i)%param(j)%pname)

            read(22,*)paramset(i)%param(j)%table_info
              call filter_char_inp(paramset(i)%param(j)%table_info)
            read(22,*)paramset(i)%param(j)%stats_proc
              call filter_char_inp(paramset(i)%param(j)%stats_proc)
            read(22,*)paramset(i)%param(j)%fixed_sfc1_type
              call filter_char_inp(paramset(i)%param(j)%fixed_sfc1_type)
! Read array count for scale_fact_fixed_sfc1
            read(22,*)cc
!
            allocate( paramset(i)%param(j)%scale_fact_fixed_sfc1(1))

            if (cc .gt. 0) then 
!  
              deallocate( paramset(i)%param(j)%scale_fact_fixed_sfc1)

              allocate( paramset(i)%param(j)%scale_fact_fixed_sfc1(cc))
              read(22,*)paramset(i)%param(j)%scale_fact_fixed_sfc1
            else
! If array count is zero dummy out the line
!
              paramset(i)%param(j)%scale_fact_fixed_sfc1(1)=0

              read(22,*)dummy_char
            endif

            read(22,*)level_array_count
            allocate( paramset(i)%param(j)%level(1))
            if (level_array_count .gt. 0) then
              deallocate( paramset(i)%param(j)%level)
              allocate( paramset(i)%param(j)%level(level_array_count))
              read(22,*)paramset(i)%param(j)%level
            else
              paramset(i)%param(j)%level(1)=0
              read(22,*)dummy_char
            endif

            read(22,*)paramset(i)%param(j)%fixed_sfc2_type
              call filter_char_inp(paramset(i)%param(j)%fixed_sfc2_type)
            read(22,*)cv
         allocate( paramset(i)%param(j)%scale_fact_fixed_sfc2(1))
            if (cv .gt. 0) then
              deallocate(paramset(i)%param(j)%scale_fact_fixed_sfc2)
              allocate(paramset(i)%param(j)%scale_fact_fixed_sfc2(cv))
              read(22,*)paramset(i)%param(j)%scale_fact_fixed_sfc2
            else
              paramset(i)%param(j)%scale_fact_fixed_sfc2(1)=0
              read(22,*)dummy_char
            endif

            read(22,*)level2_array_count
            if (level2_array_count .gt. 0) then
              allocate(paramset(i)%param(j)%level2(level2_array_count))
              read(22,*)paramset(i)%param(j)%level2
            else
              read(22,*)dummy_char
            endif

            read(22,*)paramset(i)%param(j)%aerosol_type
              call filter_char_inp(paramset(i)%param(j)%aerosol_type)
            read(22,*)paramset(i)%param(j)%typ_intvl_size
              call filter_char_inp(paramset(i)%param(j)%typ_intvl_size)

            read(22,*)paramset(i)%param(j)%scale_fact_1st_size
            read(22,*)paramset(i)%param(j)%scale_val_1st_size
            read(22,*)paramset(i)%param(j)%scale_fact_2nd_size
            read(22,*)paramset(i)%param(j)%scale_val_2nd_size
            read(22,*)paramset(i)%param(j)%typ_intvl_wvlen
              call filter_char_inp(paramset(i)%param(j)%typ_intvl_wvlen)
 
            read(22,*)paramset(i)%param(j)%scale_fact_1st_wvlen
            read(22,*)paramset(i)%param(j)%scale_val_1st_wvlen
            read(22,*)paramset(i)%param(j)%scale_fact_2nd_wvlen
            read(22,*)paramset(i)%param(j)%scale_val_2nd_wvlen
            read(22,*)scale_array_count
            allocate(paramset(i)%param(j)%scale(1))
            if (scale_array_count .gt. 0) then
              deallocate(paramset(i)%param(j)%scale)
              allocate(paramset(i)%param(j)%scale(scale_array_count))
              read(22,*)paramset(i)%param(j)%scale
            else
              paramset(i)%param(j)%scale(1)=0
              read(22,*)dummy_char
            endif  
            read(22,*)paramset(i)%param(j)%stat_miss_val
            read(22,*)paramset(i)%param(j)%leng_time_range_prev
            read(22,*)paramset(i)%param(j)%time_inc_betwn_succ_fld
            read(22,*)paramset(i)%param(j)%type_of_time_inc

              call filter_char_inp(paramset(i)%param(j)%type_of_time_inc)
            read(22,*)paramset(i)%param(j)%stat_unit_time_key_succ
              call filter_char_inp(paramset(i)%param(j)%stat_unit_time_key_succ)
            read(22,*)paramset(i)%param(j)%bit_map_flag
              call filter_char_inp(paramset(i)%param(j)%bit_map_flag)

! End of reading param
          end do

        post_avblflds%param => paramset(i)%param

! End of reading paramset
        end do
        close (UNIT=22)

        end subroutine read_postxconfig


        subroutine filter_char_inp (inpchar)
          character, intent(inout)    :: inpchar
          if (inpchar .eq. "?") then
            inpchar = ""
          endif
        end subroutine filter_char_inp

        end module


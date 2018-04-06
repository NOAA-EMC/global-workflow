!-----------------------------------------------------------------------
!
    module module_wrt_grid_comp
!
!-----------------------------------------------------------------------
!***  This module includes the funcationailty of write gridded component.
!-----------------------------------------------------------------------
!***  At intialization step, write grid is defined. The firecast field
!***  bundle is mirrored and output field information inside the field 
!***  bundle is used to create ESMF field on the write grid and added in
!***  the mirrror field bundle on write grid component. Also the IO_BaseTime
!***  is set to the intial clock time.
!***  At the run step, output time is set from the write grid comp clock 
!***  the ESMF field bundles that contains the data on write grid are 
!***  writen out through ESMF field bundle write to netcdf files.
!***  The ESMF field bundle write uses parallel write, so if output grid
!***  is cubed sphere grid, the  six tiles file will be written out at
!***  same time.
!-----------------------------------------------------------------------
!***
!***  Revision history
!***
!     Jul 2017:  J. Wang/G. Theurich  - initial code for fv3 write grid component
!     Aug 2017:  J. Wang              - add nemsio binary output for Gaussian grid
!
!---------------------------------------------------------------------------------
!
      use esmf
      use write_internal_state
      use module_fv3_io_def, only : num_pes_fcst,lead_wrttask, last_wrttask,  &
                                    n_group, num_files, &
                                    filename_base, output_grid, output_file,  &
                                    imo, jmo, write_nemsioflip
      use module_write_nemsio, only : nemsio_first_call, write_nemsio
      use module_write_netcdf, only : write_netcdf
!
!-----------------------------------------------------------------------
!
      implicit none
!
      include 'mpif.h'
!
!-----------------------------------------------------------------------

      private
!
!-----------------------------------------------------------------------
!
      integer,parameter :: filename_maxstr=255
      real(8),parameter :: pi=3.14159265358979d0
      real, parameter   :: rdgas=287.04, grav=9.80
      real, parameter   :: stndrd_atmos_ps = 101325.
      real, parameter   :: stndrd_atmos_lapse = 0.0065
!
      integer,save      :: lead_write_task                                !<-- Rank of the first write task in the write group
      integer,save      :: last_write_task                                !<-- Rank of the last write task in the write group
      integer,save      :: ntasks                                         !<-- # of write tasks in the current group 

      integer,save      :: mytile                                         !<-- the tile number in write task
      integer,save      :: wrt_mpi_comm                                   !<-- the mpi communicator in the write comp
      integer,save      :: idate(7)
      logical,save      :: first_init=.false.
      logical,save      :: first_run=.false.
      logical,save      :: first_getlatlon=.true.
!
!-----------------------------------------------------------------------
!
      type(wrt_internal_state),pointer :: wrt_int_state                 ! The internal state pointer.
      type(ESMF_FieldBundle)           :: gridFB
      integer                          :: FBcount
      character(len=80),allocatable    :: fcstItemNameList(:)
!
!-----------------------------------------------------------------------
      REAL(KIND=8)             :: btim,btim0
      REAL(KIND=8),PUBLIC,SAVE :: write_init_tim, write_run_tim  
!-----------------------------------------------------------------------
!
      public SetServices
!
      interface splat
        module procedure splat4
        module procedure splat8
      end interface splat
!
      type optimizeT
        type(ESMF_State)      :: state
        type(ESMF_GridComp), allocatable   :: comps(:)
      end type

      contains
!
!-----------------------------------------------------------------------
!#######################################################################
!-----------------------------------------------------------------------
!
      subroutine SetServices(wrt_comp, rc)
        type(ESMF_GridComp)  :: wrt_comp
        integer, intent(out) :: rc

        rc = ESMF_SUCCESS

        call ESMF_GridCompSetEntryPoint(wrt_comp, ESMF_METHOD_INITIALIZE, &
             userRoutine=wrt_initialize, rc=rc)
        if(rc/=0) write(*,*)'Error: write grid comp, initial'
!
        call ESMF_GridCompSetEntryPoint(wrt_comp, ESMF_METHOD_RUN, &
             userRoutine=wrt_run, rc=rc)
        if(rc/=0) write(*,*)'Error: write grid comp, run'
!
        call ESMF_GridCompSetEntryPoint(wrt_comp, ESMF_METHOD_FINALIZE, &
             userRoutine=wrt_finalize, rc=rc)
        if(rc/=0) write(*,*)'Error: write grid comp, run'

      end subroutine SetServices
!
!-----------------------------------------------------------------------
!#######################################################################
!-----------------------------------------------------------------------
!
      subroutine wrt_initialize(wrt_comp, imp_state_write, exp_state_write, clock, rc)
!
!-----------------------------------------------------------------------
!***  INITIALIZE THE WRITE GRIDDED COMPONENT.
!-----------------------------------------------------------------------
!
      type(esmf_GridComp)               :: wrt_comp
      type(ESMF_State)                  :: imp_state_write, exp_state_write
      type(esmf_Clock)                  :: clock
      integer,intent(out)               :: rc
!
!***  LOCAL VARIABLES
!
      TYPE(ESMF_VM)                           :: VM
      type(write_wrap)                        :: WRAP
      type(wrt_internal_state),pointer        :: wrt_int_state

      integer                                 :: ISTAT, tl, i, j, n, k,date(6)
      integer,dimension(2,6)                  :: decomptile
      integer                                 :: fieldCount
      integer                                 :: vm_mpi_comm
      character(40)                           :: fieldName, axesname,longname
      type(ESMF_Grid)                         :: wrtGrid, fcstGrid
      type(ESMF_Array)                        :: array_work, array
      type(ESMF_FieldBundle)                  :: fieldbdl_work
      type(ESMF_Field)                        :: field_work, field

      character(len=80)                       :: attrValueSList(2)
      type(ESMF_StateItem_Flag), allocatable  :: fcstItemTypeList(:)
      type(ESMF_FieldBundle)                  :: fcstFB, wrtFB, fieldbundle
      type(ESMF_Field),          allocatable  :: fcstField(:)
      type(ESMF_TypeKind_Flag)                :: typekind
      character(len=80),         allocatable  :: fieldnamelist(:)
      integer                                 :: fieldDimCount, gridDimCount
      integer,                   allocatable  :: gridToFieldMap(:)
      integer,                   allocatable  :: ungriddedLBound(:)
      integer,                   allocatable  :: ungriddedUBound(:)
      character(len=80)                       :: attName
      character(len=80),         allocatable  :: attNameList(:),attNameList2(:)
      type(ESMF_TypeKind_Flag),  allocatable  :: typekindList(:)
      character(len=80)                       :: valueS
      integer                                 :: valueI4
      real(ESMF_KIND_R4)                      :: valueR4
      real(ESMF_KIND_R8)                      :: valueR8

      integer :: attCount, axeslen, jidx, noutfile
      character(128) :: FBlist_outfilename(100), outfile_name
      character(128),dimension(:,:), allocatable    :: outfilename
      real, dimension(:), allocatable               :: slat, lat, lon, axesdata
      real(ESMF_KIND_R8), dimension(:,:), pointer   :: lonPtr, latPtr
      type(ESMF_DataCopy_Flag) :: copyflag=ESMF_DATACOPY_REFERENCE
!
      logical,save                            :: first=.true.
!test
      integer myattCount
!
!----------------------------------------------------------------------- 
!*********************************************************************** 
!----------------------------------------------------------------------- 
!
      rc = ESMF_SUCCESS
!
!----------------------------------------------------------------------- 
!***  initialize the write component timers.
!----------------------------------------------------------------------- 
!
      write_init_tim = 0.
      write_run_tim  = 0.
      btim0 = MPI_Wtime()
!
!----------------------------------------------------------------------- 
!***  set the write component's internal state.
!----------------------------------------------------------------------- 
!
      allocate(wrt_int_state,stat=RC)
      wrap%write_int_state => wrt_int_state
      call ESMF_GridCompSetInternalState(wrt_comp, wrap, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out
      allocate(wrt_int_state%wrtFB(num_files))
!
      call ESMF_VMGetCurrent(vm=VM,rc=RC)        
      call ESMF_VMGet(vm=VM, localPet=wrt_int_state%mype,               &
           petCount=wrt_int_state%petcount,mpiCommunicator=vm_mpi_comm,rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out
      call mpi_comm_dup(vm_mpi_comm,wrt_mpi_comm,rc)
      ntasks = wrt_int_state%petcount
      jidx = wrt_int_state%petcount/6
      lead_write_task = 0
      last_write_task = ntasks -1
!      print *,'in wrt, lead_write_task=', &
!         lead_write_task,'last_write_task=',last_write_task, &
!         'mype=',wrt_int_state%mype,'jidx=',jidx,' comm=',wrt_mpi_comm
!
!-----------------------------------------------------------------------
!*** Create the cubed sphere grid with field on PETs
!*** first try: Create cubed sphere grid from file
!-----------------------------------------------------------------------
!
      if ( trim(output_grid) == 'cubed_sphere_grid' ) then

        mytile = mod(wrt_int_state%mype,ntasks)+1
        do tl=1,6
          decomptile(1,tl) = 1
          decomptile(2,tl) = jidx
        enddo
        wrtgrid = ESMF_GridCreateMosaic(filename='INPUT/grid_spec.nc',      &
                              regDecompPTile=decomptile,tileFilePath='INPUT/', &
                              staggerlocList=(/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER/), &
                              name='wrt_grid', rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      else if ( trim(output_grid) == 'gaussian_grid') then
        
        wrtgrid = ESMF_GridCreate1PeriDim(minIndex=(/1,1/), &
                  maxIndex=(/imo,jmo/), regDecomp=(/1,ntasks/), &
                  indexflag=ESMF_INDEX_GLOBAL, &
                  name='wrt_grid',rc=rc)
!                  indexflag=ESMF_INDEX_GLOBAL, coordSys=ESMF_COORDSYS_SPH_DEG

        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        call ESMF_GridAddCoord(wrtgrid, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        call ESMF_GridGetCoord(wrtgrid, coordDim=1, farrayPtr=lonPtr, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        call ESMF_GridGetCoord(wrtgrid, coordDim=2, farrayPtr=latPtr, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
!
        allocate(slat(jmo),lat(jmo), lon(imo))
        call splat(4,jmo, slat)
        if(write_nemsioflip) then
          do j=1,jmo
            lat(j) = asin(slat(j)) * 180./pi
          enddo
        else
          do j=1,jmo
            lat(jmo-j+1) = asin(slat(j)) * 180./pi
          enddo
        endif
        do j=1,imo
          lon(j) = 360./real(imo) *real(j-1)
        enddo
        do j=lbound(lonPtr,2),ubound(lonPtr,2)
          do i=lbound(lonPtr,1),ubound(lonPtr,1)
            lonPtr(i,j) = 360./real(imo) * (i-1)
            latPtr(i,j) = lat(j)
          enddo
        enddo 
!        print *,'aft wrtgrd, Gaussian, dimi,i=',lbound(lonPtr,1),ubound(lonPtr,1), &
!         ' j=',lbound(lonPtr,2),ubound(lonPtr,2),'imo=',imo,'jmo=',jmo
!        print *,'aft wrtgrd, lon=',lonPtr(lbound(lonPtr,1),lbound(lonPtr,2)), &
!        lonPtr(lbound(lonPtr,1),ubound(lonPtr,2)),'lat=',latPtr(lbound(lonPtr,1),lbound(lonPtr,2)), &
!        latPtr(lbound(lonPtr,1),ubound(lonPtr,2))
        deallocate(slat)
      else if ( trim(output_grid) == 'latlon_grid') then
        wrtgrid = ESMF_GridCreate1PeriDimUfrm(maxIndex=(/imo, jmo/), &
               minCornerCoord=(/0._ESMF_KIND_R8, -80._ESMF_KIND_R8/), &
               maxCornerCoord=(/360._ESMF_KIND_R8, 80._ESMF_KIND_R8/), &
               staggerLocList=(/ESMF_STAGGERLOC_CENTER, ESMF_STAGGERLOC_CORNER/), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
!        if(wrt_int_state%mype == lead_write_task) print *,'af wrtgrd, latlon,rc=',rc, &
!         'imo=',imo,' jmo=',jmo
      endif


! Create field bundle
!-------------------------------------------------------------------
!
!---  check grid dim count first
      call ESMF_GridGet(wrtgrid, dimCount=gridDimCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
!
!--- Look at the incoming FieldBundles in the imp_state_write, and mirror them
!
      call ESMF_StateGet(imp_state_write, itemCount=FBCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      wrt_int_state%FBCount = FBCount
!      if(wrt_int_state%mype == lead_write_task) print *,'in wrt,fcst FBCount=',FBCount

      allocate(fcstItemNameList(FBCount), fcstItemTypeList(FBCount))
      allocate(wrt_int_state%wrtFB_names(FBCount))
      allocate(wrt_int_state%ncount_fields(FBCount),wrt_int_state%ncount_attribs(FBCount))
      allocate(wrt_int_state%field_names(2000,FBCount))
      allocate(outfilename(2000,FBcount))
      outfilename=''

      call ESMF_StateGet(imp_state_write, itemNameList=fcstItemNameList, &
           itemTypeList=fcstItemTypeList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

!loop over all items in the imp_state_write and collect all FieldBundles
      do i=1, FBcount

          if (fcstItemTypeList(i)==ESMF_STATEITEM_FIELDBUNDLE) then

             call ESMF_StateGet(imp_state_write, itemName=fcstItemNameList(i), &
                  fieldbundle=fcstFB, rc=rc)
             if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
               line=__LINE__, &
               file=__FILE__)) &
               return  ! bail out

! create a mirror FieldBundle and add it to importState
             fieldbundle = ESMF_FieldBundleCreate(name="mirror_"//trim(fcstItemNameList(i)), rc=rc)
             if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
               line=__LINE__, &
               file=__FILE__)) &
               return  ! bail out
             call ESMF_StateAdd(imp_state_write, (/fieldbundle/), rc=rc)
             if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
               line=__LINE__, &
               file=__FILE__)) &
               return  ! bail out

! copy the fcstFB Attributes to the mirror FieldBundle
             call ESMF_AttributeCopy(fcstFB, fieldbundle,                   &
                   attcopy=ESMF_ATTCOPY_REFERENCE, rc=rc)
             if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
               line=__LINE__, &
               file=__FILE__)) &
               return  ! bail out
!
             call ESMF_FieldBundleGet(fcstFB, fieldCount=fieldCount, rc=rc)
             if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
               line=__LINE__, &
               file=__FILE__)) &
               return  ! bail out
             wrt_int_state%ncount_fields(i) = fieldCount

             allocate(fcstField(fieldCount))
             call ESMF_FieldBundleGet(fcstFB, fieldList=fcstField,     &
                  itemorderflag=ESMF_ITEMORDER_ADDORDER, rc=rc)
             if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
               line=__LINE__, &
               file=__FILE__)) &
               return  ! bail out

             do j=1, fieldCount

               call ESMF_FieldGet(fcstField(j), typekind=typekind, &
                    dimCount=fieldDimCount, name=fieldName, grid=fcstGrid, rc=rc)
               if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                 line=__LINE__, &
                 file=__FILE__)) &
                 return  ! bail out

               allocate(gridToFieldMap(gridDimCount))
               allocate(ungriddedLBound(fieldDimCount-gridDimCount))
               allocate(ungriddedUBound(fieldDimCount-gridDimCount))
               call ESMF_FieldGet(fcstField(j), gridToFieldMap=gridToFieldMap, &
                    ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
                    rc=rc)
               CALL ESMF_LogWrite("after field create on wrt comp",ESMF_LOGMSG_INFO,rc=RC)
!      if(wrt_int_state%mype == lead_write_task) print *,'in wrt,fcstfld,fieldname=',&
!         trim(fieldname),'fieldDimCount=',fieldDimCount,'gridDimCount=',gridDimCount, &
!          'gridToFieldMap=',gridToFieldMap,'ungriddedLBound=',ungriddedLBound, &
!           'ungriddedUBound=',ungriddedUBound,'rc=',rc
               if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                 line=__LINE__, &
                 file=__FILE__)) &
                 return  ! bail out

! create the mirror field

               CALL ESMF_LogWrite("call field create on wrt comp",ESMF_LOGMSG_INFO,rc=RC)
                field_work = ESMF_FieldCreate(wrtGrid, typekind, name=fieldName, &
                       gridToFieldMap=gridToFieldMap, ungriddedLBound=ungriddedLBound, &
                       ungriddedUBound=ungriddedUBound, rc=rc)
               CALL ESMF_LogWrite("aft call field create on wrt comp",ESMF_LOGMSG_INFO,rc=RC)
               if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                 line=__LINE__, &
                 file=__FILE__)) &
                 return  ! bail out
               wrt_int_state%field_names(j,i) = trim(fieldName)

               call ESMF_AttributeCopy(fcstField(j), field_work, &
                    attcopy=ESMF_ATTCOPY_REFERENCE, rc=rc)
               if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                 line=__LINE__, &
                 file=__FILE__)) &
                 return  ! bail out
!
! get output file name
               call ESMF_AttributeGet(fcstField(i), convention="NetCDF", purpose="FV3", &
                    name="output_file", value=outfile_name, rc=rc)
               if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                 line=__LINE__, &
                 file=__FILE__)) &
                 return  ! bail out
               CALL ESMF_LogWrite("bf fcstfield, get output_file"//trim(outfile_name)//trim(fieldName),ESMF_LOGMSG_INFO,rc=RC)
               if( trim(outfile_name) /= '') then
                 outfilename(j,i) = trim(outfile_name)
               endif
               CALL ESMF_LogWrite("af fcstfield, get output_file",ESMF_LOGMSG_INFO,rc=RC)


! add the mirror field to the mirror FieldBundle
               call ESMF_FieldBundleAdd(fieldbundle, (/field_work/), rc=rc)
               if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                 line=__LINE__, &
                 file=__FILE__)) &
                 return  ! bail out

! local garbage collection
               deallocate(gridToFieldMap, ungriddedLBound, ungriddedUBound)
             enddo
!
             if (fieldCount>0) then
                call ESMF_AttributeCopy(fcstGrid, wrtGrid, &
                     attcopy=ESMF_ATTCOPY_REFERENCE, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, &
                  file=__FILE__)) &
                  return  ! bail out
             endif
             deallocate(fcstField)

          else
! anything but a FieldBundle in the state is unexpected here
             call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
                  msg="Only FieldBundles supported in fcstState.", &
                  line=__LINE__, file=__FILE__)
             return  ! bail out
          endif


!end FBCount
      enddo
!
!loop over all items in the imp_state_write and count output FieldBundles
    call get_outfile(FBcount, outfilename,FBlist_outfilename,noutfile)
    wrt_int_state%FBCount = noutfile
!
!create output field bundles
    do i=1, wrt_int_state%FBcount

       wrt_int_state%wrtFB_names(i) = trim(FBlist_outfilename(i))
       wrt_int_state%wrtFB(i) = ESMF_FieldBundleCreate(name=trim(wrt_int_state%wrtFB_names(i)), rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

       do n=1, FBcount

         call ESMF_StateGet(imp_state_write, itemName="mirror_"//trim(fcstItemNameList(n)), &
             fieldbundle=fcstFB, rc=rc)

!       if(wrt_int_state%mype == lead_write_task) print *,'in wrt,fcstItemNameList(n)=', &
!         trim(fcstItemNameList(n)),' FBlist_outfilename=',trim(FBlist_outfilename(i))

         if( index(trim(fcstItemNameList(n)),trim(FBlist_outfilename(i))) > 0 ) then
!
! copy the mirror fcstfield bundle Attributes to the output field bundle
           call ESMF_AttributeCopy(fcstFB,  wrt_int_state%wrtFB(i), &
                attcopy=ESMF_ATTCOPY_REFERENCE, rc=rc)
           if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
             line=__LINE__, &
             file=__FILE__)) &
             return  ! bail out
!
           call ESMF_FieldBundleGet(fcstFB, fieldCount=fieldCount, rc=rc)
           if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
             line=__LINE__, &
             file=__FILE__)) &
             return  ! bail out

          allocate(fcstField(fieldCount),fieldnamelist(fieldCount))
          call ESMF_FieldBundleGet(fcstFB, fieldList=fcstField, fieldNameList=fieldnamelist,   &
            itemorderflag=ESMF_ITEMORDER_ADDORDER, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

          do j=1, fieldCount

            call ESMF_AttributeGet(fcstField(j),convention="NetCDF", purpose="FV3", &
              name='output_file',value=outfile_name, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out

!       if(wrt_int_state%mype == lead_write_task) print *,'in wrt,add field,i=',i,'n=',n,' j=',j, &
!            'fieldname=',trim(fieldnamelist(j)), ' outfile_name=',trim(outfile_name), &
!            ' field bundle name, FBlist_outfilename(i)=',trim(FBlist_outfilename(i))

            if( trim(outfile_name) == trim(FBlist_outfilename(i))) then
              call ESMF_FieldBundleAdd(wrt_int_state%wrtFB(i), (/fcstField(j)/), rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                   line=__LINE__, &
                   file=__FILE__)) &
                   return  ! bail out
            endif

          enddo
          deallocate(fcstField, fieldnamelist)

        endif
!end FBcount
      enddo

!end wrt FBcount
    enddo
!
! add time Attribute
! look at the importState attributes and copy those starting with "time"

    call ESMF_AttributeGet(imp_state_write, convention="NetCDF", purpose="FV3", &
      attnestflag=ESMF_ATTNEST_OFF, count=attCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_LogWrite("Write component AttributeGet, attCount ", ESMF_LOGMSG_INFO, rc=rc)

! prepare the lists needed to transfer attributes
    allocate(attNameList(attCount), attNameList2(attCount))
    allocate(typekindList(attCount))

! loop over all the attributes on importState within AttPack
    j=1
    k=1
    do i=1, attCount
      call ESMF_AttributeGet(imp_state_write, convention="NetCDF", purpose="FV3", &
        attnestflag=ESMF_ATTNEST_OFF, attributeIndex=i, name=attName, &
        typekind=typekind, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

! test for name starting with "time"
      if (index(trim(attName), "time")==1) then

! add this attribute to the list of transfers
        attNameList(j)=attName
        typekindList(j)=typekind
        j=j+1
        if (index(trim(attName), "time:")==1) then
          ! store names of attributes starting with "time:" for later use
          attNameList2(k)=attName
          k=k+1
        endif
      endif
    enddo

! add the transfer attributes from importState to grid
    call ESMF_AttributeAdd(wrtgrid, convention="NetCDF", purpose="FV3", &
      attrList=attNameList(1:j-1), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

! loop over the added attributes, access the value (only scalar allowed),
! and set them on the grid
    do i=1, j-1
      if (typekindList(i)==ESMF_TYPEKIND_CHARACTER) then
        call ESMF_AttributeGet(imp_state_write, &
          convention="NetCDF", purpose="FV3", &
          name=trim(attNameList(i)), value=valueS, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        call ESMF_AttributeSet(wrtgrid, convention="NetCDF", purpose="FV3", &
          name=trim(attNameList(i)), value=valueS, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      else if (typekindList(i)==ESMF_TYPEKIND_I4) then
        call ESMF_AttributeGet(imp_state_write, &
          convention="NetCDF", purpose="FV3", &
          name=trim(attNameList(i)), value=valueI4, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        call ESMF_AttributeSet(wrtgrid, convention="NetCDF", purpose="FV3", &
          name=trim(attNameList(i)), value=valueI4, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      else if (typekindList(i)==ESMF_TYPEKIND_R4) then
        call ESMF_AttributeGet(imp_state_write, &
          convention="NetCDF", purpose="FV3", &
          name=trim(attNameList(i)), value=valueR4, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        call ESMF_AttributeSet(wrtgrid, convention="NetCDF", purpose="FV3", &
          name=trim(attNameList(i)), value=valueR4, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      else if (typekindList(i)==ESMF_TYPEKIND_R8) then
        call ESMF_AttributeGet(imp_state_write, &
          convention="NetCDF", purpose="FV3", &
          name=trim(attNameList(i)), value=valueR8, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        call ESMF_AttributeSet(wrtgrid, convention="NetCDF", purpose="FV3", &
          name=trim(attNameList(i)), value=valueR8, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
    enddo

! Add special attribute that holds names of "time" related attributes
! for faster access during Run().
    call ESMF_AttributeAdd(wrtgrid, convention="NetCDF", purpose="FV3", &
      attrList=(/"TimeAttributes"/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_AttributeSet(wrtgrid, convention="NetCDF", purpose="FV3", &
      name="TimeAttributes", valueList=attNameList2(1:k-1), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    deallocate(attNameList, attNameList2, typekindList)

!
!*** create temporary field bundle for  axes information
! write the Grid coordinate arrays into the output files via temporary FB
      gridFB = ESMF_FieldBundleCreate(rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      call ESMF_AttributeGet(wrtGrid, convention="NetCDF", purpose="FV3", &
      name="ESMF:gridded_dim_labels", valueList=attrValueSList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      call ESMF_GridGetCoord(wrtGrid, coordDim=1, &
        staggerloc=ESMF_STAGGERLOC_CENTER, array=array, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
!      print *,'create gridFB,fieldname=',trim(attrValueSList(1)),trim(attrValueSList(2)), &
!         'lon value=',array(1:5)

      field = ESMF_FieldCreate(wrtGrid, array, name=trim(attrValueSList(1)), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

!add attribute info
! long name
      call ESMF_AttributeAdd(field,convention="NetCDF",purpose="FV3",  &
                            attrList=(/'long_name'/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_AttributeSet(field,convention="NetCDF",purpose="FV3",name='long_name', &
                              value="T-cell longitude", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
! units
      call ESMF_AttributeAdd(field,convention="NetCDF",purpose="FV3",  &
                            attrList=(/'units'/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_AttributeSet(field,convention="NetCDF",purpose="FV3",name='units', &
                             value="degrees_E", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
! cartesian_axis
      call ESMF_AttributeAdd(field,convention="NetCDF",purpose="FV3",  &
                            attrList=(/'cartesian_axis'/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_AttributeSet(field,convention="NetCDF",purpose="FV3",name='cartesian_axis', &
                            value="X", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

! add field to bundle
      call ESMF_FieldBundleAdd(gridFB, (/field/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
!
! get 2nd dimension
      call ESMF_GridGetCoord(wrtGrid, coordDim=2, &
        staggerloc=ESMF_STAGGERLOC_CENTER, array=array, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
!      print *,'create gridFB,fieldname=',trim(attrValueSList(1)),trim(attrValueSList(2)), &
!         'lat value=',array(1:5,1),array(1,1:5)

      field = ESMF_FieldCreate(wrtGrid, array, name=trim(attrValueSList(2)), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
!add attribute info
! long name
      call ESMF_AttributeAdd(field,convention="NetCDF",purpose="FV3",  &
                            attrList=(/'long_name'/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_AttributeSet(field,convention="NetCDF",purpose="FV3",name='long_name', &
                              value="T-cell latitude", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
! units
      call ESMF_AttributeAdd(field,convention="NetCDF",purpose="FV3",  &
                            attrList=(/'units'/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_AttributeSet(field,convention="NetCDF",purpose="FV3",name='units', &
                             value="degrees_N", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
! cartesian_axis
      call ESMF_AttributeAdd(field,convention="NetCDF",purpose="FV3",  &
                            attrList=(/'cartesian_axis'/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_AttributeSet(field,convention="NetCDF",purpose="FV3",name='cartesian_axis', &
                            value="Y", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      call ESMF_FieldBundleAdd(gridFB, (/field/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
!
!-----------------------------------------------------------------------
!***  SET THE IO_BaseTime TO THE INITIAL CLOCK TIME.
!-----------------------------------------------------------------------
!
      call ESMF_ClockGet(clock    =CLOCK                                &  !<-- The ESMF Clock
                        ,startTime=wrt_int_state%IO_BASETIME            &  !<-- The Clock's starting time
                        ,rc       =RC)

      call ESMF_TimeGet(time=wrt_int_state%IO_BASETIME,yy=date(1),mm=date(2),dd=date(3),h=date(4), &
                        m=date(5),s=date(6),rc=rc)
      if(wrt_int_state%mype == lead_write_task) print *,'in wrt initial, io_baseline time=',date,'rc=',rc
      idate(1:6) = date(1:6)
      idate(7) = 1
!
!-----------------------------------------------------------------------
!***  SET THE FIRST HISTORY FILE'S TIME INDEX.
!-----------------------------------------------------------------------
!
      wrt_int_state%NFHOUR = 0
!
!-----------------------------------------------------------------------
!***  Initialize for nemsio file
!-----------------------------------------------------------------------
!
      call ESMF_LogWrite("before initialize for nemsio file", ESMF_LOGMSG_INFO, rc=rc)
      if(trim(output_grid) == 'gaussian_grid' .and. trim(output_file) == 'nemsio') then
        do i= 1, wrt_int_state%FBcount
          call nemsio_first_call(wrt_int_state%wrtFB(i), imo, jmo, &
             wrt_int_state%mype, ntasks, wrt_mpi_comm, wrt_int_state%FBcount, i, idate, &
             lat, lon,rc) 
        enddo
      endif
      call ESMF_LogWrite("after initialize for nemsio file", ESMF_LOGMSG_INFO, rc=rc)
!
!-----------------------------------------------------------------------
!
      IF(RC /= ESMF_SUCCESS) THEN
        WRITE(0,*)"FAIL: Write_Initialize."
!      ELSE
!        WRITE(0,*)"PASS: Write_Initialize."
      ENDIF
!
!      write_init_tim = MPI_Wtime() - btim0
!
!----------------------------------------------------------------------- 
!
      end subroutine wrt_initialize
!
!----------------------------------------------------------------------- 
!####################################################################### 
!----------------------------------------------------------------------- 
!
      subroutine wrt_run(wrt_comp, imp_state_write, exp_state_write,clock,rc)
!
!----------------------------------------------------------------------- 
!***  the run step for the write gridded component.  
!----------------------------------------------------------------------- 
!
!
      type(ESMF_GridComp)            :: wrt_comp
      type(ESMF_State)               :: imp_state_write, exp_state_write
      type(ESMF_Clock)               :: clock
      integer,intent(out)            :: rc 
!
!-----------------------------------------------------------------------
!***  local variables
!
      TYPE(ESMF_VM)                         :: VM
      type(ESMF_FieldBundle)                :: file_bundle 
      type(ESMF_Time)                       :: currtime
      type(ESMF_TypeKind_Flag)              :: datatype
      type(ESMF_Field)                      :: field_work
      type(ESMF_Grid)                       :: grid_work, fbgrid
      type(ESMF_Array)                      :: array_work
      type(ESMF_State),save                 :: stateGridFB
      type(optimizeT), save                 :: optimize(4)
      type(ESMF_GridComp), save, allocatable   :: compsGridFB(:)
!
      type(write_wrap)                      :: wrap
      type(wrt_internal_state),pointer      :: wrt_int_state
!
      integer,dimension(:),allocatable,save :: ih_int  &
                                              ,ih_real
!
      INTEGER,SAVE                          :: NPOSN_1,NPOSN_2
!
      integer                               :: i,j,n,mype,nolog
!
      integer                               :: nf_hours,nf_seconds, nf_minutes,     &
                                               nseconds,nseconds_num,nseconds_den
!
      integer                               :: id
      integer                               :: nbdl, idx, date(6)
      integer                               :: step=1
!
      REAL                                  :: DEGRAD
!
      logical                               :: opened
      logical,save                          :: first=.true.
      logical,save                          :: file_first=.true.
!
      character(filename_maxstr)            :: filename,compname,bundle_name
      character(3)                          :: cfhour
      character(10)                         :: stepString
      character(80)                         :: attrValueS
      integer                               :: attrValueI
      real                                  :: attrValueR
      real(ESMF_KIND_R8)                    :: time

!
!-----------------------------------------------------------------------
!
      real(kind=8) :: wait_time, MPI_Wtime
      real(kind=8)         :: times,times2,etim
      character(10)        :: timeb
      real(kind=8) :: tbeg,tend
      real(kind=8) :: wbeg,wend

      integer fieldcount, dimCount
      real(kind=ESMF_KIND_R8), dimension(:,:,:), pointer   :: datar8
      real(kind=ESMF_KIND_R8), dimension(:,:), pointer     :: datar82d
!
      integer myattCount
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      tbeg = MPI_Wtime()
      rc     = esmf_success
!
!-----------------------------------------------------------------------
!***  get the current write grid comp name, id, and internal state
!
      call ESMF_GridCompGet(wrt_comp, name=compname, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
!      print *,'in wrt run. compname=',trim(compname),' rc=',rc

! instance id from name
       read(compname(10:11),"(I2)") id

! Provide log message indicating which wrtComp is active
       call ESMF_LogWrite("Write component activated: "//trim(compname), &
         ESMF_LOGMSG_INFO, rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out

! access the internal state
       call ESMF_GridCompGetInternalState(wrt_Comp, wrap, rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out
       wrt_int_state => wrap%write_int_state

       call ESMF_VMGetCurrent(VM,rc=RC)
       mype = wrt_int_state%mype
!    print *,'in wrt run, mype=',mype,'lead_write_task=',lead_write_task
!
!-----------------------------------------------------------------------
!*** get current time and elapsed forecast time

       call ESMF_ClockGet(clock=CLOCK, currTime=CURRTIME, rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out
       call ESMF_TimeGet(time=currTime,yy=date(1),mm=date(2),dd=date(3),h=date(4), &
                      m=date(5),s=date(6),rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out
!    if(mype == lead_write_task) print *,'in wrt run, curr time=',date
!
       call ESMF_TimeGet(time=wrt_int_state%IO_BASETIME,yy=date(1),mm=date(2),dd=date(3),h=date(4), &
                      m=date(5),s=date(6),rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out
!    print *,'in wrt run, io_baseline time=',date
!
       wrt_int_state%IO_CURRTIMEDIFF = CURRTIME-wrt_int_state%IO_BASETIME
!
       call ESMF_TimeIntervalGet(timeinterval=wrt_int_state%IO_CURRTIMEDIFF &
                                   ,h           =nf_hours               &  !<-- Hours of elapsed time
                                   ,m           =nf_minutes             &  !<-- Minutes of elapsed time
                                   ,s           =nseconds               &  !<-- Seconds of elapsed time
                                   ,sN          =nseconds_num           &  !<-- Numerator of fractional elapsed seconds
                                   ,sD          =nseconds_den           &  !<-- denominator of fractional elapsed seconds
                                   ,rc          =RC)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out
!    if(mype == lead_write_task) print *,'in wrt run, nf_hours=',nf_hours,nf_minutes,nseconds, &
!       'nseconds_num=',nseconds_num,nseconds_den
!
       nf_seconds = nf_hours*3600+nf_minuteS*60+nseconds+real(nseconds_num)/real(nseconds_den)
       wrt_int_state%nfhour = nf_seconds/3600.
       nf_hours   = int(nf_seconds/3600.)
       nf_minutes = int((nf_seconds-nf_hours*3600.)/60.)
       nseconds   = int(nf_seconds-nf_hours*3600.-nf_minutes*60.)
       write(cfhour,'(I3.3)')int(wrt_int_state%nfhour)
    if(mype == lead_write_task) print *,'in wrt run, 2, nf_hours=',nf_hours,nf_minutes,nseconds, &
       'nseconds_num=',nseconds_num,nseconds_den,' FBCount=',FBCount

!    if(mype == lead_write_task) print *,'in wrt run, cfhour=',cfhour, &
!     print *,'in wrt run, cfhour=',cfhour, &
!        ' nf_seconds=',nf_seconds,wrt_int_state%nfhour

! access the time Attribute which is updated by the driver each time
       call ESMF_LogWrite("before Write component get time", ESMF_LOGMSG_INFO, rc=rc)
       call ESMF_AttributeGet(imp_state_write, convention="NetCDF", purpose="FV3", &
         name="time", value=time, rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out
       call ESMF_LogWrite("before Write component af get time", ESMF_LOGMSG_INFO, rc=rc)
!
!-----------------------------------------------------------------------
!*** loop on the files that need to write out
!-----------------------------------------------------------------------

       do i=1, FBCount
         call ESMF_LogWrite("before Write component get mirror file bundle", ESMF_LOGMSG_INFO, rc=rc)
         call ESMF_StateGet(imp_state_write, itemName="mirror_"//trim(fcstItemNameList(i)), &
              fieldbundle=file_bundle, rc=rc)
         if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, file=__FILE__)) &
           return  ! bail out
         call ESMF_LogWrite("before Write component af get mirror file bundle", ESMF_LOGMSG_INFO, rc=rc)
!recover fields from cartesian vector and sfc pressure
         call recover_fields(file_bundle,rc)
       enddo
!
! ** now loop through output field bundle

       file_loop_all: do nbdl=1, wrt_int_state%FBCount
!
         if(step == 1) then
           file_bundle = wrt_int_state%wrtFB(nbdl)
         endif

         if ( trim(output_file) == 'nemsio' ) then
            filename = trim(wrt_int_state%wrtFB_names(nbdl))//'f'//cfhour//'.nemsio'
         else
            filename = trim(wrt_int_state%wrtFB_names(nbdl))//'f'//cfhour//'.nc'
         endif
!         if(mype == lead_write_task) print *,'in wrt run,filename=',trim(filename)

!
! set the time Attribute on the grid to carry it into the lower levels
         call ESMF_FieldBundleGet(file_bundle, grid=fbgrid, rc=rc)
         if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out
         call ESMF_AttributeSet(fbgrid, convention="NetCDF", purpose="FV3", &
           name="time", value=real(wrt_int_state%nfhour,ESMF_KIND_R8), rc=rc)
         if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out

!*** write out grid bundle:
! Provide log message indicating which wrtComp is active
         call ESMF_LogWrite("before Write component before gridFB ", ESMF_LOGMSG_INFO, rc=rc)
         if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, file=__FILE__)) &
           return  ! bail out

         if (trim(output_grid) == 'cubed_sphere_grid') then

          wbeg = MPI_Wtime()
          call ESMFproto_FieldBundleWrite(gridFB, filename=trim(filename), &
               convention="NetCDF", purpose="FV3", &
               status=ESMF_FILESTATUS_REPLACE, state=stateGridFB, comps=compsGridFB,rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) &
            return  ! bail out

          call ESMFproto_FieldBundleWrite(wrt_int_state%wrtFB(nbdl), &
               filename=trim(filename), convention="NetCDF", purpose="FV3",   &
               status=ESMF_FILESTATUS_OLD, timeslice=step, state=optimize(nbdl)%state, &
               comps=optimize(nbdl)%comps, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
          wend = MPI_Wtime()
          if (mype == lead_write_task) then
            write(*,'(A,F10.5,A,I2.2,A,I2.2)')' actual    netcdf Write Time is ',wend-wbeg &
                     ,' at Fcst ',NF_HOURS,':',NF_MINUTES
          endif

        else if (trim(output_grid) == 'gaussian_grid') then

           if (trim(output_file) == 'nemsio') then

             wbeg = MPI_Wtime()
             call write_nemsio(file_bundle,trim(filename),nf_hours, nf_minutes, &
                               nseconds, nseconds_num, nseconds_den,nbdl, rc)
             wend = MPI_Wtime()
             if (mype == lead_write_task) then
               write(*,'(A,F10.5,A,I2.2,A,I2.2)')' nemsio      Write Time is ',wend-wbeg  &
                        ,' at Fcst ',NF_HOURS,':',NF_MINUTES
             endif

           else if (trim(output_file) == 'netcdf') then

             wbeg = MPI_Wtime()
             call write_netcdf(file_bundle,wrt_int_state%wrtFB(nbdl),trim(filename), &
                               wrt_mpi_comm,wrt_int_state%mype,imo,jmo,rc)
             wend = MPI_Wtime()
             if (mype == lead_write_task) then
               write(*,'(A,F10.5,A,I2.2,A,I2.2)')' netcdf      Write Time is ',wend-wbeg  &
                        ,' at Fcst ',NF_HOURS,':',NF_MINUTES
             endif

           else if (trim(output_file) == 'netcdf_esmf') then

             wbeg = MPI_Wtime()
             call ESMFproto_FieldBundleWrite(gridFB, filename=trim(filename), &
                  convention="NetCDF", purpose="FV3", &
                  status=ESMF_FILESTATUS_REPLACE, state=stateGridFB, comps=compsGridFB,rc=rc)
             if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
               line=__LINE__, file=__FILE__)) &
               return  ! bail out

             call ESMFproto_FieldBundleWrite(wrt_int_state%wrtFB(nbdl), &
                  filename=trim(filename), convention="NetCDF", purpose="FV3",   &
                  status=ESMF_FILESTATUS_OLD, timeslice=step, state=optimize(nbdl)%state, &
                  comps=optimize(nbdl)%comps, rc=rc)
             if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                 line=__LINE__, &
                 file=__FILE__)) &
                 return  ! bail out
             wend = MPI_Wtime()
             if (mype == lead_write_task) then
               write(*,'(A,F10.5,A,I2.2,A,I2.2)')' netcdf_esmf Write Time is ',wend-wbeg &
                        ,' at Fcst ',NF_HOURS,':',NF_MINUTES
             endif

           else ! unknown output_file

             call ESMF_LogWrite("wrt_run: Unknown output_file",ESMF_LOGMSG_ERROR,rc=RC)

           endif

        else ! unknown output_grid

          call ESMF_LogWrite("wrt_run: Unknown output_grid",ESMF_LOGMSG_ERROR,rc=RC)

        endif

      enddo file_loop_all
!
!** write out log file
!
    if(mype == lead_write_task) then
      do n=701,900
        inquire(n,opened=OPENED)
        if(.not.opened)then
          nolog=n
          exit
        endif
      enddo
!
      open(nolog,file='logf'//cfhour,form='FORMATTED')
        write(nolog,100)wrt_int_state%nfhour,idate(1:6)
100     format(' completed fv3gfs fhour=',f10.3,2x,6(i4,2x))
      close(nolog)
    endif
!
!
!-----------------------------------------------------------------------
!
      call ESMF_VMBarrier(VM, rc=rc)
!
      write_run_tim=MPI_Wtime()-tbeg
!
      IF(mype == lead_write_task)THEN
        WRITE(*,'(A,F10.5,A,I2.2,A,I2.2)')' total            Write Time is ',write_run_tim  &
                 ,' at Fcst ',NF_HOURS,':',NF_MINUTES
      ENDIF
!
      IF(RC /= ESMF_SUCCESS) THEN
        WRITE(0,*)"FAIL: WRITE_RUN"
!     ELSE
!       WRITE(0,*)"PASS: WRITE_RUN"
      ENDIF
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE wrt_run
!
!-----------------------------------------------------------------------
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!-----------------------------------------------------------------------
!
      subroutine wrt_finalize(wrt_comp, imp_state_write, exp_state_write, clock, rc)
!
!-----------------------------------------------------------------------
!***  finalize the write gridded component.
!-----------------------------------------------------------------------
!
!***  HISTORY
!       Feb 2017:  J. Wang  - deallocate for fv3
!
!-----------------------------------------------------------------------
!
      type(ESMF_GridComp)            :: wrt_comp
      type(ESMF_State)               :: imp_state_write, exp_state_write
      type(ESMF_Clock)               :: clock
      integer,intent(out)            :: rc
!
!***  local variables
!
      integer :: stat
!
      type(write_wrap)                  :: wrap
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      rc=ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!***  retrieve the write component's esmf internal state(used later for 
!***  post finalization)
!-----------------------------------------------------------------------
!
      call ESMF_GridCompGetInternalState(wrt_comp, wrap, rc)  
      deallocate(wrap%write_int_state,stat=stat)  
!
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
          msg="Deallocation of internal state memory failed.", &
          line=__LINE__, &
          file=__FILE__)) &
        return  ! bail out
!
!-----------------------------------------------------------------------
!
      IF(RC /= ESMF_SUCCESS)THEN
        WRITE(0,*)'FAIL: Write_Finalize.'
!      ELSE
!        WRITE(0,*)'PASS: Write_Finalize.'
      ENDIF
!
!-----------------------------------------------------------------------
!
    end subroutine wrt_finalize
!
!-----------------------------------------------------------------------
!
   subroutine recover_fields(file_bundle,rc)

     type(ESMF_FieldBundle), intent(in)              :: file_bundle
     integer,                intent(out),   optional :: rc
!
     integer i,j,k,ifld,fieldCount,nstt,nend,fieldDimCount,gridDimCount
     integer istart,iend,jstart,jend,kstart,kend,km
     logical uPresent, vPresent
     type(ESMF_Grid)  fieldGrid
     type(ESMF_Field)  ufield, vfield
     type(ESMF_TypeKind_Flag) typekind
     character(100) fieldName,uwindname,vwindname
     type(ESMF_Field),   allocatable  :: fcstField(:)
     real(ESMF_KIND_R8), dimension(:,:),     pointer  :: lon, lat
     real(ESMF_KIND_R4), dimension(:,:),     pointer  :: pressfc
     real(ESMF_KIND_R4), dimension(:,:),     pointer  :: uwind2dr4,vwind2dr4
     real(ESMF_KIND_R4), dimension(:,:,:),   pointer  :: uwind3dr4,vwind3dr4
     real(ESMF_KIND_R4), dimension(:,:,:),   pointer  :: cart3dPtr2dr4
     real(ESMF_KIND_R4), dimension(:,:,:,:), pointer  :: cart3dPtr3dr4
     real(ESMF_KIND_R8), dimension(:,:,:,:), pointer  :: cart3dPtr3dr8
     save lon, lat
!
! get filed count
     call ESMF_FieldBundleGet(file_bundle, fieldCount=fieldCount, &
         grid=fieldGrid, rc=rc)
!
     CALL ESMF_LogWrite("call recover field on wrt comp",ESMF_LOGMSG_INFO,rc=RC)
     call ESMF_GridGet(fieldgrid, dimCount=gridDimCount, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

     if( first_getlatlon ) then
     CALL ESMF_LogWrite("call recover field get coord 1",ESMF_LOGMSG_INFO,rc=RC)
     call ESMF_GridGetCoord(fieldgrid, coordDim=1, farrayPtr=lon, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
     lon = lon * pi/180.
!     print *,'in 3DCartesian2wind, lon dim=',lbound(lon,1),ubound(lon,1),lbound(lon,2),ubound(lon,2), &
!       'lon=',lon(lbound(lon,1),lbound(lon,2)), lon(ubound(lon,1),ubound(lon,2))
     CALL ESMF_LogWrite("call recover field get coord 2",ESMF_LOGMSG_INFO,rc=RC)
     call ESMF_GridGetCoord(fieldgrid, coordDim=2, farrayPtr=lat, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
     lat = lat * pi/180.
!     print *,'in 3DCartesian2wind, lat dim=',lbound(lat,1),ubound(lat,1),lbound(lat,2),ubound(lat,2), &
!       'lat=',lat(lbound(lon,1),lbound(lon,2)), lat(ubound(lon,1),ubound(lon,2))
     first_getlatlon = .false.
     endif
!
     allocate(fcstField(fieldCount))
     CALL ESMF_LogWrite("call recover field get fcstField",ESMF_LOGMSG_INFO,rc=RC)
     call ESMF_FieldBundleGet(file_bundle, fieldList=fcstField, itemorderflag=ESMF_ITEMORDER_ADDORDER, rc=rc)
!
     do ifld=1,fieldCount

       CALL ESMF_LogWrite("call recover field get fieldname, type dimcount",ESMF_LOGMSG_INFO,rc=RC)
       call ESMF_FieldGet(fcstField(ifld),name=fieldName,typekind=typekind,dimCount=fieldDimCount, rc=rc)

! convert back wind
       if(index(trim(fieldName),"vector")>0) then
         nstt = index(trim(fieldName),"wind")+4
         nend = index(trim(fieldName),"vector")-1
         if( nend>nstt ) then
           uwindname = 'u'//fieldName(nstt+1:nend)
           vwindname = 'v'//fieldName(nstt+1:nend)
         else
           uwindname = 'ugrd'
           vwindname = 'vgrd'
         endif
!         print *,'in get 3D vector wind, uwindname=',trim(uwindname),' v=', trim(vwindname),' fieldname=',trim(fieldname)
! get u , v wind
         CALL ESMF_LogWrite("call recover field get u, v field",ESMF_LOGMSG_INFO,rc=RC)
         call ESMF_FieldBundleGet(file_bundle,trim(uwindname),field=ufield,isPresent=uPresent,rc=rc)
         call ESMF_FieldBundleGet(file_bundle,trim(vwindname),field=vfield,isPresent=vPresent,rc=rc)
         if(.not. uPresent .or. .not.vPresent) then
           rc=990
           print *,' ERROR ,the local wind is not present! rc=', rc
           exit
         endif

! get field data
         if ( typekind == ESMF_TYPEKIND_R4 ) then
           if( fieldDimCount > gridDimCount+1 ) then
             CALL ESMF_LogWrite("call recover field get 3d card wind farray",ESMF_LOGMSG_INFO,rc=RC)
             call ESMF_FieldGet(fcstField(ifld), localDe=0, farrayPtr=cart3dPtr3dr4, rc=rc)
               if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                 line=__LINE__, file=__FILE__)) return  ! bail out
             if( ubound(cart3dPtr3dr4,1)-lbound(cart3dPtr3dr4,1)+1/=3) then
               rc=991
               print *,'ERROR, 3D the vector dimension /= 3, rc=',rc
               exit
             endif
             iend   = ubound(cart3dPtr3dr4,2)
             istart = lbound(cart3dPtr3dr4,2)
             iend   = ubound(cart3dPtr3dr4,2)
             jstart = lbound(cart3dPtr3dr4,3)
             jend   = ubound(cart3dPtr3dr4,3)
             kstart = lbound(cart3dPtr3dr4,4)
             kend   = ubound(cart3dPtr3dr4,4)
             call ESMF_FieldGet(ufield, localDe=0, farrayPtr=uwind3dr4,rc=rc)
             call ESMF_FieldGet(vfield, localDe=0, farrayPtr=vwind3dr4,rc=rc)
! update u , v wind
             do k=kstart,kend
               do j=jstart, jend
                 do i=istart, iend
                  uwind3dr4(i,j,k) = cart3dPtr3dr4(1,i,j,k) * cos(lon(i,j))+ &
                                     cart3dPtr3dr4(2,i,j,k) * sin(lon(i,j))
                  vwind3dr4(i,j,k) =-cart3dPtr3dr4(1,i,j,k) * sin(lat(i,j))*sin(lon(i,j))+ &
                                     cart3dPtr3dr4(2,i,j,k) * sin(lat(i,j))*cos(lon(i,j))+ &
                                     cart3dPtr3dr4(3,i,j,k) * cos(lat(i,j))
                 enddo
               enddo
             enddo
           else
             call ESMF_FieldGet(fcstField(ifld), localDe=0, farrayPtr=cart3dPtr2dr4, rc=rc)
               if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                 line=__LINE__, file=__FILE__)) return  ! bail out
             if( ubound(cart3dPtr2dr4,1)-lbound(cart3dPtr2dr4,1)+1 /= 3) then
               rc=991
               print *,'ERROR, 2D the vector dimension /= 3, rc=',rc
               exit
             endif
             istart = lbound(cart3dPtr2dr4,2)
             iend   = ubound(cart3dPtr2dr4,2)
             jstart = lbound(cart3dPtr2dr4,3)
             jend   = ubound(cart3dPtr2dr4,3)

             call ESMF_FieldGet(ufield, localDe=0, farrayPtr=uwind2dr4,rc=rc)
             call ESMF_FieldGet(vfield, localDe=0, farrayPtr=vwind2dr4,rc=rc)
              ! update u , v wind
             do j=jstart, jend
               do i=istart, iend
                  uwind2dr4(i,j) = cart3dPtr2dr4(1,i,j) * cos(lon(i,j))+ &
                                   cart3dPtr2dr4(2,i,j) * sin(lon(i,j))
                  vwind2dr4(i,j) =-cart3dPtr2dr4(1,i,j) * sin(lat(i,j))*sin(lon(i,j))+ &
                                   cart3dPtr2dr4(2,i,j) * sin(lat(i,j))*cos(lon(i,j))+ &
                                   cart3dPtr2dr4(3,i,j) * cos(lat(i,j))
               enddo
             enddo
           endif
         endif
!     print *,'in 3DCartesian2wind, uwindname=', trim(uwindname),'uPresent =',uPresent, 'vPresent=',vPresent,'fieldDimCount=', &
!       fieldDimCount,'gridDimCount=',gridDimCount

! convert back surface pressure
       else if(index(trim(fieldName),"pressfc")>0) then
         call ESMF_FieldGet(fcstField(ifld),localDe=0, farrayPtr=pressfc, rc=rc)
         istart = lbound(pressfc,1)
         iend   = ubound(pressfc,1)
         jstart = lbound(pressfc,2)
         jend   = ubound(pressfc,2)
         do j=jstart, jend
           do i=istart, iend
             pressfc(i,j) = pressfc(i,j)**(grav/(rdgas*stndrd_atmos_lapse))*stndrd_atmos_ps
           enddo
         enddo
       endif
     enddo
!
     deallocate(fcstField)
     rc = 0

   end subroutine recover_fields
!
!-----------------------------------------------------------------------
!

  subroutine ESMFproto_FieldBundleWrite(fieldbundle, fileName, &
    convention, purpose, status, timeslice, state, comps, rc)
    type(ESMF_FieldBundle),     intent(in)              :: fieldbundle
    character(*),               intent(in)              :: fileName
    character(*),               intent(in),    optional :: convention
    character(*),               intent(in),    optional :: purpose
    type(ESMF_FileStatus_Flag), intent(in),    optional :: status
    integer,                    intent(in),    optional :: timeslice
    type(ESMF_State),           intent(inout), optional :: state
    type(ESMF_GridComp), allocatable, intent(inout), optional :: comps(:)
    integer,                    intent(out),   optional :: rc
    
    ! Prototype multi-tile implementation for FieldBundleWrite().
    ! Produces as many output files as there are tiles. The naming of the 
    ! output files is such that the string in the fileName argument is used
    ! as the basis. If fileName ends with ".nc", then this suffix is replaced
    ! by ".tileN.nc", where "N" is the tile number. If fileName does not 
    ! end in ".nc", then ".tileN.nc" will simply be appended.
    !
    ! Restrictions:
    !   - All Fields in the FieldBundle must have the same tileCount
    
    integer                       :: i, j, ind
    integer                       :: fieldCount, tileCount, itemCount
    type(ESMF_Field), allocatable :: fieldList(:), tileFieldList(:)
    type(ESMF_Grid)               :: grid
    type(ESMF_Array)              :: array
    type(ESMF_DistGrid)           :: distgrid
    type(ESMF_DELayout)           :: delayout
    type(ESMF_FieldBundle)        :: wrtTileFB
    type(ESMF_FieldBundle), allocatable :: wrtTileFBList(:)
    character(len=80), allocatable      :: itemNameList(:)
    logical                             :: stateIsEmpty
    character(len=80)             :: tileFileName
    character(len=80)             :: statusStr
    integer, allocatable          :: petList(:)
    character(1024)               :: msgString
    type(ESMF_State), allocatable :: ioState(:)
    integer                       :: timesliceOpt
    integer                       :: urc

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_VMLogMemInfo("Entering ESMFproto_FieldBundleWrite",rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! query number of fields in fieldbundle
    call ESMF_FieldBundleGet(fieldbundle, fieldCount=fieldCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! early successful exit if there are no fields present
    if (fieldCount==0) return
    ! obtain list of fields in the fieldbundle
    allocate(fieldList(fieldCount), tileFieldList(fieldCount))
    call ESMF_FieldBundleGet(fieldbundle, fieldList=fieldList, &
      itemorderflag=ESMF_ITEMORDER_ADDORDER, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! determine tileCount by looking at first field
    call ESMF_FieldGet(fieldList(1), array=array, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ArrayGet(array, tileCount=tileCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! deal with optional state argument
    stateIsEmpty = .true.
    if (present(state)) then
      if (.not.ESMF_StateIsCreated(state, rc=rc)) then
        state = ESMF_StateCreate(rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      call ESMF_StateGet(state, itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      if (itemCount /= 0) then
        stateIsEmpty = .false.
        if (itemCount /= tileCount) then
          call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
            msg="Number of items in state must match number of tiles.", &
            line=__LINE__, &
            file=__FILE__, &
            rcToReturn=rc)
          return  ! bail out
        endif
        allocate(itemNameList(itemCount),wrtTileFBList(itemCount))
        call ESMF_StateGet(state, itemNameList=itemNameList, &
          itemorderflag=ESMF_ITEMORDER_ADDORDER, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        do i=1, itemCount
          call ESMF_StateGet(state, itemName=itemNameList(i), &
            fieldbundle=wrtTileFBList(i), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        enddo
      endif
    endif
    
    ! loop over all the tiles and construct a tile specific fieldbundle
    call ESMF_LogWrite("In ESMFproto_FieldBundleWrite() before tileCount-loop",&
      ESMF_LOGMSG_INFO, rc=rc)
      
    !TODO: remove this once comps is hidden within state
    if (present(comps)) then
      allocate(ioState(tileCount))
      if (.not.allocated(comps)) then
        ! first-write
        allocate(comps(tileCount))
        do i=1, tileCount
          call ESMF_LogWrite("In ESMFproto_FieldBundleWrite() before "// &
            "ESMFproto_FieldMakeSingleTile() w/ petList", &
            ESMF_LOGMSG_INFO, rc=rc)
          do j=1, fieldCount
            ! access only tile specific part of field
            call ESMFproto_FieldMakeSingleTile(fieldList(j), tile=i, &
              tileField=tileFieldList(j), petList=petList, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
          enddo
!          write(msgString, *) petList
!          call ESMF_LogWrite("In ESMFproto_FieldBundleWrite() after "// &
!            "ESMFproto_FieldMakeSingleTile(), petList:"//trim(msgString), &
!            ESMF_LOGMSG_INFO, rc=rc)
          ! create component to handle this tile I/O
          call ESMF_LogWrite("In ESMFproto_FieldBundleWrite() before "// &
            "tile-component creation", ESMF_LOGMSG_INFO, rc=rc)
          comps(i) = ESMF_GridCompCreate(petList=petList, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          ! convention
          call ESMF_AttributeSet(comps(i), name="convention", &
            value=convention, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          ! purpose
          call ESMF_AttributeSet(comps(i), name="purpose", &
            value=purpose, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          ! timeslice
          timesliceOpt = -1 ! init
          if (present(timeslice)) timesliceOpt = timeslice
          call ESMF_AttributeSet(comps(i), name="timeslice", &
            value=timesliceOpt, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          ! status
          statusStr="ESMF_FILESTATUS_UNKNOWN" ! default
          if (present(status)) then
            if (status==ESMF_FILESTATUS_UNKNOWN) then
              statusStr="ESMF_FILESTATUS_UNKNOWN" ! default
            else if (status==ESMF_FILESTATUS_NEW) then
              statusStr="ESMF_FILESTATUS_NEW" ! default
            else if (status==ESMF_FILESTATUS_OLD) then
              statusStr="ESMF_FILESTATUS_OLD" ! default
            else if (status==ESMF_FILESTATUS_REPLACE) then
              statusStr="ESMF_FILESTATUS_REPLACE" ! default
            endif
          endif
          call ESMF_AttributeSet(comps(i), name="status", &
            value=statusStr, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          call ESMF_GridCompSetServices(comps(i), ioCompSS, userRc=urc, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__, rcToReturn=rc)) &
            return  ! bail out
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          call ESMF_LogWrite("In ESMFproto_FieldBundleWrite() after "// &
            "tile-component creation", ESMF_LOGMSG_INFO, rc=rc)
        enddo
      endif
    endif
      
    do i=1, tileCount
      if (stateIsEmpty) then
        ! loop over all the fields and add tile specific part to fieldbundle
        call ESMF_LogWrite("In ESMFproto_FieldBundleWrite() before "// &
          "ESMFproto_FieldMakeSingleTile()", ESMF_LOGMSG_INFO, rc=rc)
        do j=1, fieldCount
          ! access only tile specific part of field
          call ESMFproto_FieldMakeSingleTile(fieldList(j), tile=i, &
            tileField=tileFieldList(j), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        enddo
        call ESMF_LogWrite("In ESMFproto_FieldBundleWrite() after "// &
          "ESMFproto_FieldMakeSingleTile()", ESMF_LOGMSG_INFO, rc=rc)
        ! create tile specific fieldbundle
        wrtTileFB = ESMF_FieldBundleCreate(fieldList=tileFieldList, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        ! ensure global attributes on the fieldbundle are passed on by reference
        call ESMF_AttributeCopy(fieldbundle, wrtTileFB, &
          attcopy=ESMF_ATTCOPY_REFERENCE, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        ! store this fieldbundle in state if present
        if (present(state)) then
          call ESMF_StateAdd(state, fieldbundleList=(/wrtTileFB/), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        endif
      else
        ! state brought in existing fieldbundles
        wrtTileFB = wrtTileFBList(i)
      endif
      ! write out the tile specific fieldbundle
      if(tileCount>1) then
        ind=min(index(trim(fileName),".nc",.true.)-1,len_trim(fileName))
        write(tileFileName, "(A,A,I1,A)") fileName(1:ind), ".tile", i, ".nc"
      else
        tileFileName=trim(fileName)
      endif
      if (present(comps)) then
        ioState(i) = ESMF_StateCreate(rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        call ESMF_StateAdd(ioState(i), (/wrtTileFB/), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        call ESMF_AttributeSet(comps(i), name="tileFileName", &
          value=tileFileName, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        call ESMF_GridCompRun(comps(i), importState=ioState(i), &
          userRc=urc, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__, rcToReturn=rc)) &
          return  ! bail out
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        call ESMF_StateDestroy(ioState(i), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      else
        call ESMF_LogWrite("In ESMFproto_FieldBundleWrite() before "// &
          "ESMF_FieldBundleWrite(): "//trim(tileFileName), &
          ESMF_LOGMSG_INFO, rc=rc)
        call ESMF_FieldBundleWrite(fieldbundle=wrtTileFB, fileName=tileFileName, &
          convention=convention, purpose=purpose, status=status, &
          timeslice=timeslice, overwrite=.true., rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        call ESMF_LogWrite("In ESMFproto_FieldBundleWrite() after "// &
          "ESMF_FieldBundleWrite()", ESMF_LOGMSG_INFO, rc=rc)
      endif
      if (.not.present(state)) then
        ! local garbage collection of fields
        do j=1, fieldCount
          call ESMF_FieldGet(tileFieldList(j), array=array, grid=grid, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          call ESMF_FieldDestroy(tileFieldList(j), noGarbage=.true., rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          call ESMF_GridDestroy(grid, noGarbage=.true., rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          call ESMF_ArrayGet(array, distgrid=distgrid, delayout=delayout, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          call ESMF_ArrayDestroy(array, noGarbage=.true., rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          call ESMF_DistGridDestroy(distgrid, noGarbage=.true., rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          call ESMF_DELayoutDestroy(delayout, noGarbage=.true., rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        enddo
        ! destroy tile specific fieldbundle
        call ESMF_FieldBundleDestroy(wrtTileFB, noGarbage=.true., rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
    enddo
    if (present(comps)) then
      deallocate(ioState)
    endif
    call ESMF_LogWrite("In ESMFproto_FieldBundleWrite() after tileCount-loop",&
      ESMF_LOGMSG_INFO, rc=rc)
    
    ! deallocate temporary lists
    deallocate(fieldList, tileFieldList)

    call ESMF_VMLogMemInfo("Exiting ESMFproto_FieldBundleWrite",rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine ESMFproto_FieldBundleWrite

  !-----------------------------------------------------------------------------

  subroutine ioCompSS(comp, rc)
    type(ESMF_GridComp)   :: comp
    integer, intent(out)  :: rc

    rc = ESMF_SUCCESS
  
    call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_RUN, &
      userRoutine=ioCompRun, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine ioCompRun(comp, importState, exportState, clock, rc)
    use netcdf
    type(ESMF_GridComp)   :: comp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    type(ESMF_FieldBundle)        :: wrtTileFB
    character(len=80)             :: tileFileName
    character(len=80)             :: convention
    character(len=80)             :: purpose
    integer                       :: timeslice
    character(len=80)             :: statusStr
    type(ESMF_FileStatus_Flag)    :: status
    character(len=80)             :: itemNameList(1)

    integer                       :: localPet, i, j, k, ind
    type(ESMF_Grid)               :: grid
    real(ESMF_KIND_R4), allocatable  :: valueListr4(:)
    real(ESMF_KIND_R8), allocatable  :: valueListr8(:)
    integer                       :: valueCount, fieldCount, udimCount
    character(80), allocatable    :: udimList(:)
    integer                       :: ncerr, ncid, dimid, varid
    type(ESMF_Field), allocatable :: fieldList(:)
    type(ESMF_Field)              :: field
    logical                       :: isPresent
    real(ESMF_KIND_R8)            :: time
    integer                       :: itemCount, attCount
    character(len=80),         allocatable  :: attNameList(:)
    character(len=80)             :: attName
    type(ESMF_TypeKind_Flag)      :: typekind
    character(len=80)             :: valueS
    integer                       :: valueI4
    real(ESMF_KIND_R4)            :: valueR4
    real(ESMF_KIND_R8)            :: valueR8
    logical                       :: thereAreVerticals

    rc = ESMF_SUCCESS

    ! Access the FieldBundle
    call ESMF_StateGet(importState, itemNameList=itemNameList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_StateGet(importState, itemName=itemNameList(1), &
      fieldBundle=wrtTileFB, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! Access attributes on the component and use as parameters for Write()
    call ESMF_AttributeGet(comp, name="tileFileName", value=tileFileName, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_AttributeGet(comp, name="convention", value=convention, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_AttributeGet(comp, name="purpose", value=purpose, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_AttributeGet(comp, name="timeslice", value=timeslice, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_AttributeGet(comp, name="status", value=statusStr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (trim(statusStr)=="ESMF_FILESTATUS_UNKNOWN") then
      status=ESMF_FILESTATUS_UNKNOWN
    else if (trim(statusStr)=="ESMF_FILESTATUS_NEW") then
      status=ESMF_FILESTATUS_NEW
    else if (trim(statusStr)=="ESMF_FILESTATUS_OLD") then
      status=ESMF_FILESTATUS_OLD
    else if (trim(statusStr)=="ESMF_FILESTATUS_REPLACE") then
      status=ESMF_FILESTATUS_REPLACE
    endif

    call ESMF_LogWrite("In ioCompRun() before writing to: "// &
      trim(tileFileName), ESMF_LOGMSG_INFO, rc=rc)

    if (status==ESMF_FILESTATUS_OLD) then
      ! This writes the vectical coordinates and the time dimension into the
      ! file. Doing this before the large data sets are written, assuming that
      ! the first time coming into ioCompRun() with this tileFileName, only
      ! the grid info is written. Second time in, with ESMF_FILESTATUS_OLD,
      ! the large data sets are written. That is when vertical and time info
      ! is also written.
      ! Hoping for better performance because file exists, but is still small.
      call ESMF_GridCompGet(comp, localPet=localPet, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      if (localPet==0) then
        ! do this work only on the root pet
        call ESMF_FieldBundleGet(wrtTileFB, grid=grid, fieldCount=fieldCount, &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        allocate(fieldList(fieldCount))
        call ESMF_FieldBundleGet(wrtTileFB, fieldList=fieldList, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        ! open this tile's NetCDF file
        ncerr = nf90_open(tileFileName, NF90_WRITE, ncid=ncid)
        if (ESMF_LogFoundNetCDFError(ncerr, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__, rcToReturn=rc)) &
          return  ! bail out
        ! loop over all the fields in the bundle and handle their vectical dims
        thereAreVerticals=.false.
        do i=1, fieldCount
          field = fieldList(i)
          call ESMF_AttributeGetAttPack(field, &
            convention="NetCDF", purpose="FV3", isPresent=isPresent, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          if (.not.isPresent) cycle ! field does not have the AttPack
          call ESMF_AttributeGet(field, convention="NetCDF", purpose="FV3", &
            name="ESMF:ungridded_dim_labels", isPresent=isPresent, &
            itemCount=udimCount, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          if (udimCount==0 .or. .not.isPresent) cycle ! nothing there to do
          thereAreVerticals=.true.
          allocate(udimList(udimCount))
          call ESMF_AttributeGet(field, convention="NetCDF", purpose="FV3", &
            name="ESMF:ungridded_dim_labels", valueList=udimList, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          ! loop over all ungridded dimension labels
          do k=1, udimCount
            call write_out_ungridded_dim_atts(dimLabel=trim(udimList(k)), rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
          enddo
          deallocate(udimList)
        enddo ! fieldCount
        deallocate(fieldList)
        if (thereAreVerticals) then
          ! see if the vertical_dim_labels attribute exists on the grid, and
          ! if so access it and write out vecticals accordingly
          call ESMF_AttributeGet(grid, convention="NetCDF", purpose="FV3", &
            name="vertical_dim_labels", isPresent=isPresent, &
            itemCount=udimCount, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          if (isPresent .and. (udimCount>0) ) then
            allocate(udimList(udimCount))
            call ESMF_AttributeGet(grid, convention="NetCDF", purpose="FV3", &
              name="vertical_dim_labels", valueList=udimList, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
            ! loop over all ungridded dimension labels
            do k=1, udimCount
              call write_out_ungridded_dim_atts(dimLabel=trim(udimList(k)), rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
            enddo
            deallocate(udimList)
          endif
        endif
        ! inquire if NetCDF file already contains the "time" variable
        ncerr = nf90_inq_varid(ncid, "time", varid=varid)
        if (ncerr /= NF90_NOERR) then
          ! the variable does not exist in the NetCDF file yet -> add it
          ! access the "time" attribute on the grid
          call ESMF_AttributeGet(grid, convention="NetCDF", purpose="FV3", &
            name="time", value=time, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          ncerr = nf90_redef(ncid=ncid)
          if (ESMF_LogFoundNetCDFError(ncerr, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__, rcToReturn=rc)) &
            return  ! bail out
          ncerr = nf90_inq_dimid(ncid, "time", dimid=dimid)
          if (ncerr /= NF90_NOERR) then
            ! "time" dimension does not yet exist, define as unlimited dim
            ncerr = nf90_def_dim(ncid, "time", NF90_UNLIMITED, dimid=dimid)
            if (ESMF_LogFoundNetCDFError(ncerr, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__, rcToReturn=rc)) &
              return  ! bail out
          endif
          ncerr = nf90_def_var(ncid, "time", NF90_DOUBLE, &
            dimids=(/dimid/), varid=varid)
          if (ESMF_LogFoundNetCDFError(ncerr, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__, rcToReturn=rc)) &
            return  ! bail out
          ncerr = nf90_enddef(ncid=ncid)
          if (ESMF_LogFoundNetCDFError(ncerr, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__, rcToReturn=rc)) &
            return  ! bail out
          ncerr = nf90_put_var(ncid, varid, values=time)
          if (ESMF_LogFoundNetCDFError(ncerr, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__, rcToReturn=rc)) &
            return  ! bail out
          ! loop over all the grid attributes that start with "time:", and
          ! put them on the "time" variable in the NetCDF file
          call ESMF_AttributeGet(grid, convention="NetCDF", purpose="FV3", &
            name="TimeAttributes", itemCount=itemCount, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          if (itemCount>0) then
            ncerr = nf90_redef(ncid=ncid)
            if (ESMF_LogFoundNetCDFError(ncerr, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__, rcToReturn=rc)) &
              return  ! bail out
            allocate(attNameList(itemCount))
            call ESMF_AttributeGet(grid, convention="NetCDF", purpose="FV3", &
              name="TimeAttributes", valueList=attNameList, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
            do i=1, itemCount
              attName=attNameList(i)
              call ESMF_AttributeGet(grid, convention="NetCDF", purpose="FV3", &
                name=trim(attNameList(i)), typekind=typekind, rc=rc)
!                print *,'in esmf call, att name=',trim(attNameList(i))
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              if (typekind==ESMF_TYPEKIND_CHARACTER) then
                call ESMF_AttributeGet(grid, &
                  convention="NetCDF", purpose="FV3", &
                  name=trim(attNameList(i)), value=valueS, rc=rc)
!                print *,'in esmf call, att string value=',trim(valueS)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, &
                  file=__FILE__)) &
                  return  ! bail out
                ncerr = nf90_put_att(ncid, varid, &
                  trim(attName(6:len(attName))), values=valueS)
                if (ESMF_LogFoundNetCDFError(ncerr, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, &
                  file=__FILE__, rcToReturn=rc)) &
                  return  ! bail out
              else if (typekind==ESMF_TYPEKIND_I4) then
                call ESMF_AttributeGet(grid, &
                  convention="NetCDF", purpose="FV3", &
                  name=trim(attNameList(i)), value=valueI4, rc=rc)
!                print *,'in esmf call, att I4 value=',valueR8
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, &
                  file=__FILE__)) &
                  return  ! bail out
                ncerr = nf90_put_att(ncid, varid, &
                  trim(attName(6:len(attName))), values=valueI4)
                if (ESMF_LogFoundNetCDFError(ncerr, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, &
                  file=__FILE__, rcToReturn=rc)) &
                  return  ! bail out
              else if (typekind==ESMF_TYPEKIND_R4) then
                call ESMF_AttributeGet(grid, &
                  convention="NetCDF", purpose="FV3", &
                  name=trim(attNameList(i)), value=valueR4, rc=rc)
!                print *,'in esmf call, att r4 value=',valueR8
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, &
                  file=__FILE__)) &
                  return  ! bail out
                ncerr = nf90_put_att(ncid, varid, &
                  trim(attName(6:len(attName))), values=valueR4)
                if (ESMF_LogFoundNetCDFError(ncerr, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, &
                  file=__FILE__, rcToReturn=rc)) &
                  return  ! bail out
              else if (typekind==ESMF_TYPEKIND_R8) then
                call ESMF_AttributeGet(grid, &
                  convention="NetCDF", purpose="FV3", &
                  name=trim(attNameList(i)), value=valueR8, rc=rc)
!                print *,'in esmf call, att r8 value=',valueR8
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, &
                  file=__FILE__)) &
                  return  ! bail out
                ncerr = nf90_put_att(ncid, varid, &
                  trim(attName(6:len(attName))), values=valueR8)
                if (ESMF_LogFoundNetCDFError(ncerr, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, &
                  file=__FILE__, rcToReturn=rc)) &
                  return  ! bail out
              endif
            enddo
            deallocate(attNameList)
            ncerr = nf90_enddef(ncid=ncid)
            if (ESMF_LogFoundNetCDFError(ncerr, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__, rcToReturn=rc)) &
              return  ! bail out
          endif
        endif
        ! close the NetCDF file
        ncerr = nf90_close(ncid=ncid)
        if (ESMF_LogFoundNetCDFError(ncerr, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__, rcToReturn=rc)) &
          return  ! bail out
      endif
      call ESMF_LogWrite("In ioCompRun() after "// &
        "writing vectical and time dimensions.", ESMF_LOGMSG_INFO, rc=rc)
    endif

    !TODO: remove this block once the ESMF_FieldBundleWrite() below allows to
    !TODO: specify the NetCDF 64-bit-offset file format.
    if (status==ESMF_FILESTATUS_REPLACE) then
      ! First time in with this filename (therefore 'replace'), create the
      ! file with 64bit-offset format in order to accommodate larger data
      ! volume.
      call ESMF_GridCompGet(comp, localPet=localPet, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      if (localPet==0) then
        ! only single PET to deal with NetCDF
        ncerr = nf90_create(tileFileName, &
          cmode=or(nf90_clobber,nf90_64bit_offset), ncid=ncid)
        if (ESMF_LogFoundNetCDFError(ncerr, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__, rcToReturn=rc)) &
          return  ! bail out
        ncerr = nf90_close(ncid=ncid)
        if (ESMF_LogFoundNetCDFError(ncerr, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__, rcToReturn=rc)) &
          return  ! bail out
      endif
      status=ESMF_FILESTATUS_OLD  ! switch status to 'OLD' to not overwrite
      call ESMF_LogWrite("In ioCompRun() after creating the NetCDF file", &
        ESMF_LOGMSG_INFO, rc=rc)
    endif

    if (timeslice==-1) then
      call ESMF_FieldBundleWrite(fieldbundle=wrtTileFB, fileName=tileFileName, &
        convention=convention, purpose=purpose, status=status, &
        overwrite=.true., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    else
      call ESMF_FieldBundleWrite(fieldbundle=wrtTileFB, fileName=tileFileName, &
        convention=convention, purpose=purpose, status=status, &
        timeslice=timeslice, overwrite=.true., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    call ESMF_LogWrite("In ioCompRun() after "// &
      "ESMF_FieldBundleWrite()", ESMF_LOGMSG_INFO, rc=rc)

  contains

    subroutine write_out_ungridded_dim_atts(dimLabel, rc)
      character(len=*)      :: dimLabel
      integer, intent(out)  :: rc

      ! inquire if NetCDF file already contains this ungridded dimension
      ncerr = nf90_inq_varid(ncid, trim(dimLabel), varid=varid)
      if (ncerr == NF90_NOERR) return
      ! the variable does not exist in the NetCDF file yet -> add it
      ! access the undistributed dimension attribute on the grid
      call ESMF_AttributeGet(grid, convention="NetCDF", purpose="FV3", &
        name=trim(dimLabel), itemCount=valueCount, typekind=typekind, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
      return  ! bail out
      if( typekind == ESMF_TYPEKIND_R4 ) then
        allocate(valueListr4(valueCount))
        call ESMF_AttributeGet(grid, convention="NetCDF", purpose="FV3", &
          name=trim(dimLabel), valueList=valueListr4, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      else if ( typekind == ESMF_TYPEKIND_R8) then
        allocate(valueListr8(valueCount))
        call ESMF_AttributeGet(grid, convention="NetCDF", purpose="FV3", &
          name=trim(dimLabel), valueList=valueListr8, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
      ! now add it to the NetCDF file
      ncerr = nf90_redef(ncid=ncid)
      if (ESMF_LogFoundNetCDFError(ncerr, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__, rcToReturn=rc)) &
        return  ! bail out
      ncerr = nf90_inq_dimid(ncid, trim(dimLabel), dimid=dimid)
      if (ncerr /= NF90_NOERR) then
        ! dimension does not yet exist, and must be defined
        ncerr = nf90_def_dim(ncid, trim(dimLabel), valueCount, &
          dimid=dimid)
        if (ESMF_LogFoundNetCDFError(ncerr, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__, rcToReturn=rc)) &
          return  ! bail out
      endif
      if( typekind == ESMF_TYPEKIND_R4 ) then
        ncerr = nf90_def_var(ncid, trim(dimLabel), NF90_FLOAT, &
          dimids=(/dimid/), varid=varid)
        if (ESMF_LogFoundNetCDFError(ncerr, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__, rcToReturn=rc)) &
          return  ! bail out
        ncerr = nf90_enddef(ncid=ncid)
        if (ESMF_LogFoundNetCDFError(ncerr, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__, rcToReturn=rc)) &
          return  ! bail out
        ncerr = nf90_put_var(ncid, varid, values=valueListr4)
        if (ESMF_LogFoundNetCDFError(ncerr, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__, rcToReturn=rc)) &
          return  ! bail out
        deallocate(valueListr4)
      else if(typekind == ESMF_TYPEKIND_R8) then
        ncerr = nf90_def_var(ncid, trim(dimLabel), NF90_DOUBLE, &
          dimids=(/dimid/), varid=varid)
        if (ESMF_LogFoundNetCDFError(ncerr, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__, rcToReturn=rc)) &
          return  ! bail out
        ncerr = nf90_enddef(ncid=ncid)
        if (ESMF_LogFoundNetCDFError(ncerr, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__, rcToReturn=rc)) &
          return  ! bail out
        ncerr = nf90_put_var(ncid, varid, values=valueListr8)
        if (ESMF_LogFoundNetCDFError(ncerr, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__, rcToReturn=rc)) &
          return  ! bail out
        deallocate(valueListr8)
      endif
      ! add attributes to this vertical variable
      call ESMF_AttributeGet(grid, convention="NetCDF", purpose="FV3", &
        attnestflag=ESMF_ATTNEST_OFF, count=attCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      if (attCount>0) then
        ncerr = nf90_redef(ncid=ncid)
        if (ESMF_LogFoundNetCDFError(ncerr, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__, rcToReturn=rc)) &
          return  ! bail out
      endif
      ! loop over all the attributes
      do j=1, attCount
        call ESMF_AttributeGet(grid, convention="NetCDF", purpose="FV3", &
          attnestflag=ESMF_ATTNEST_OFF, attributeIndex=j, name=attName, &
          typekind=typekind, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        ! test for name starting with trim(dimLabel)":"
        if (index(trim(attName), trim(dimLabel)//":")==1) then
          ind = len(trim(dimLabel)//":")
          ! found a matching attributes
          if (typekind==ESMF_TYPEKIND_CHARACTER) then
            call ESMF_AttributeGet(grid, &
              convention="NetCDF", purpose="FV3", &
              name=trim(attName), value=valueS, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
            ncerr = nf90_put_att(ncid, varid, &
              trim(attName(ind+1:len(attName))), values=valueS)
            if (ESMF_LogFoundNetCDFError(ncerr, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__, rcToReturn=rc)) &
              return  ! bail out
          else if (typekind==ESMF_TYPEKIND_I4) then
            call ESMF_AttributeGet(grid, &
              convention="NetCDF", purpose="FV3", &
              name=trim(attName), value=valueI4, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
            ncerr = nf90_put_att(ncid, varid, &
              trim(attName(ind+1:len(attName))), values=valueI4)
            if (ESMF_LogFoundNetCDFError(ncerr, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__, rcToReturn=rc)) &
              return  ! bail out
          else if (typekind==ESMF_TYPEKIND_R4) then
            call ESMF_AttributeGet(grid, &
              convention="NetCDF", purpose="FV3", &
              name=trim(attName), value=valueR4, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
            ncerr = nf90_put_att(ncid, varid, &
              trim(attName(ind+1:len(attName))), values=valueR4)
            if (ESMF_LogFoundNetCDFError(ncerr, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__, rcToReturn=rc)) &
              return  ! bail out
          else if (typekind==ESMF_TYPEKIND_R8) then
            call ESMF_AttributeGet(grid, &
              convention="NetCDF", purpose="FV3", &
              name=trim(attName), value=valueR8, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
            ncerr = nf90_put_att(ncid, varid, &
              trim(attName(ind+1:len(attName))), values=valueR8)
            if (ESMF_LogFoundNetCDFError(ncerr, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__, rcToReturn=rc)) &
              return  ! bail out
          endif
        endif
      enddo
      if (attCount>0) then
        ncerr = nf90_enddef(ncid=ncid)
        if (ESMF_LogFoundNetCDFError(ncerr, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__, rcToReturn=rc)) &
          return  ! bail out
      endif
    end subroutine

  end subroutine ioCompRun

  !-----------------------------------------------------------------------------

  subroutine ESMFproto_FieldMakeSingleTile(field, tile, tileField, petList, rc)
    type(ESMF_Field),     intent(in)              :: field
    integer,              intent(in)              :: tile
    type(ESMF_Field),     intent(out)             :: tileField
    integer, allocatable, intent(inout), optional :: petList(:)
    integer,              intent(out),   optional :: rc
    
    ! Take in a field on a multi-tile grid and return a field that only 
    ! references a single tile.
    
    ! This routine only works with references, no data copies are being 
    ! made. The single tile field that is returned points to the original
    ! field allocation. 
    
    ! The original field passed in remains valid.

    type(ESMF_TypeKind_Flag)                :: typekind
    type(ESMF_Index_Flag)                   :: indexflag
    type(ESMF_Grid)                         :: grid, tileGrid
    type(ESMF_Array)                        :: array
    type(ESMF_DistGrid)                     :: distgrid
    type(ESMF_DELayout)                     :: delayout
    character(40)                           :: fieldName
    integer                                 :: fieldDimCount, gridDimCount
    integer                                 :: undistDims
    integer                                 :: localDeCount, deCount, tileCount
    integer,                   allocatable  :: gridToFieldMap(:)
    integer,                   allocatable  :: ungriddedLBound(:)
    integer,                   allocatable  :: ungriddedUBound(:)
    integer,                   allocatable  :: localDeToDeMap(:), deToTileMap(:)
    type(ESMF_LocalArray),     allocatable  :: lArrayList(:)
    integer                                 :: i
    integer                                 :: tileDeCount, tileLocalDeCount
    integer,                   allocatable  :: distgridToArrayMap(:)
    integer,                   allocatable  :: undistLBound(:), undistUBound(:)
    integer,                   allocatable  :: petMap(:), tilePetMap(:)
    integer,                   allocatable  :: minIndexPDe(:,:)
    integer,                   allocatable  :: maxIndexPDe(:,:)
    integer,                   allocatable  :: minIndexPTile(:,:)
    integer,                   allocatable  :: maxIndexPTile(:,:)
    integer,                   allocatable  :: deBlockList(:,:,:)

    character(800)                          :: msg

    if (present(rc)) rc = ESMF_SUCCESS

    ! access information from the incoming field
    call ESMF_FieldGet(field, array=array, typekind=typekind, &
      dimCount=fieldDimCount, name=fieldName, grid=grid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! access information from the associated grid
    call ESMF_GridGet(grid, dimCount=gridDimCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#if 0
write(msg,*) "fieldDimCount=",fieldDimCount,"gridDimCount=",gridDimCount
call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
#endif
    ! access list type information from the incoming field
    allocate(gridToFieldMap(gridDimCount))
    undistDims = fieldDimCount-gridDimCount
    if (undistDims < 0) undistDims = 0  ! this supports replicated dimensions
    allocate(ungriddedLBound(undistDims))
    allocate(ungriddedUBound(undistDims))
    call ESMF_FieldGet(field, gridToFieldMap=gridToFieldMap, &
      ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
    return  ! bail out
    ! access information from associated array
    call ESMF_ArrayGet(array, distgrid=distgrid, delayout=delayout, &
      indexflag=indexflag, localDeCount=localDeCount, deCount=deCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! access list type information from associated array
    allocate(localDeToDeMap(localDeCount), deToTileMap(deCount))
    allocate(distgridToArrayMap(gridDimCount))
    allocate(undistLBound(undistDims))
    allocate(undistUBound(undistDims))
    call ESMF_ArrayGet(array, tileCount=tileCount, &
      localDeToDeMap=localDeToDeMap, deToTileMap=deToTileMap, &
      distgridToArrayMap=distgridToArrayMap, &
      undistLBound=undistLBound, undistUBound=undistUBound, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! access list type information from associated distgrid
    allocate(minIndexPDe(gridDimCount,deCount))
    allocate(maxIndexPDe(gridDimCount,deCount))
    allocate(minIndexPTile(gridDimCount,tileCount))
    allocate(maxIndexPTile(gridDimCount,tileCount))
    call ESMF_DistGridGet(distgrid, &
      minIndexPDe=minIndexPDe, maxIndexPDe=maxIndexPDe, &
      minIndexPTile=minIndexPTile, maxIndexPTile=maxIndexPTile, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! access list type information from associated delayout
    allocate(petMap(deCount))
    call ESMF_DELayoutGet(delayout, petMap=petMap, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! construct data structures selecting specific tile
    allocate(lArrayList(localDeCount))
    tileLocalDeCount=0
    do i=1, localDeCount
      if (deToTileMap(localDeToDeMap(i)+1)==tile) then
        ! localDe is on tile
        tileLocalDeCount=tileLocalDeCount+1
        call ESMF_ArrayGet(array, localDe=i-1, &
          localarray=lArrayList(tileLocalDeCount), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
    enddo
    allocate(tilePetMap(deCount))
    allocate(deBlockList(gridDimCount,2,deCount))
    tileDeCount=0
    do i=1, deCount
      if (deToTileMap(i)==tile) then
        ! DE is on tile
        tileDeCount = tileDeCount+1
        tilePetMap(tileDeCount) = petMap(i)
        deBlockList(:,1,tileDeCount) = minIndexPDe(:,i)
        deBlockList(:,2,tileDeCount) = maxIndexPDe(:,i)
      endif
    enddo
    if (present(petList)) then
      if (.not.allocated(petList)) then
        allocate(petList(tileDeCount))
      else if (size(petList)/=tileDeCount) then
        deallocate(petList)
        allocate(petList(tileDeCount))
      endif
      petList(:) = tilePetMap(1:tileDeCount)
    endif
    ! create DELayout and DistGrid that only contain the single tile
    delayout = ESMF_DELayoutCreate(tilePetMap(1:tileDeCount), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    distgrid = ESMF_DistGridCreate(minIndex=minIndexPTile(:,tile), &
      maxIndex=maxIndexPTile(:,tile), delayout=delayout, &
      deBlockList=deBlockList(:,:,1:tileDeCount), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! create an Array that only holds tile specific allocations
    if (tileLocalDeCount>0) then
      array = ESMF_ArrayCreate(distgrid, lArrayList(1:tileLocalDeCount), &
        indexflag=indexflag, &
        distgridToArrayMap=distgridToArrayMap, undistLBound=undistLBound, &
        undistUBound=undistUBound, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    else
      array = ESMF_ArrayCreate(distgrid, typekind, indexflag=indexflag, &
        distgridToArrayMap=distgridToArrayMap, undistLBound=undistLBound, &
        undistUBound=undistUBound, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    endif
    ! create a grid on the new distgrid
    tileGrid = ESMF_GridCreate(distgrid, indexflag=indexflag, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! alias the Attributes on grid level
    call ESMF_AttributeCopy(grid, tileGrid, attcopy=ESMF_ATTCOPY_REFERENCE, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! create the tile specific field from the array
    tileField = ESMF_FieldCreate(tileGrid, array=array, name=fieldName, &
      gridToFieldMap=gridToFieldMap, ungriddedLBound=ungriddedLBound, &
      ungriddedUBound=ungriddedUBound, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! alias the Attributes on field level
    call ESMF_AttributeCopy(field, tileField, attcopy=ESMF_ATTCOPY_REFERENCE, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! local garbage collection
    deallocate(localDeToDeMap, deToTileMap)
    deallocate(petMap, tilePetMap)
    deallocate(minIndexPDe, maxIndexPDe)
    deallocate(minIndexPTile, maxIndexPTile)
    deallocate(gridToFieldMap, ungriddedLBound, ungriddedUBound)
    deallocate(deBlockList)

  end subroutine ESMFproto_FieldMakeSingleTile

!
!-----------------------------------------------------------------------
  subroutine splat4(idrt,jmax,aslat)

      implicit none
      integer,intent(in)  :: idrt,jmax
      real(4),intent(out) :: aslat(jmax)
!
      integer,parameter   :: KD=SELECTED_REAL_KIND(15,45)
      real(kind=KD)       :: pk(jmax/2),pkm1(jmax/2),pkm2(jmax/2)
      real(kind=KD)       :: aslatd(jmax/2),sp,spmax,eps=10.d0*epsilon(sp)
      integer,PARAMETER   :: JZ=50
      real(8) bz(jz)
      data bz        / 2.4048255577d0,  5.5200781103d0, &
       8.6537279129d0, 11.7915344391d0, 14.9309177086d0, 18.0710639679d0, &
      21.2116366299d0, 24.3524715308d0, 27.4934791320d0, 30.6346064684d0, &
      33.7758202136d0, 36.9170983537d0, 40.0584257646d0, 43.1997917132d0, &
      46.3411883717d0, 49.4826098974d0, 52.6240518411d0, 55.7655107550d0, &
      58.9069839261d0, 62.0484691902d0, 65.1899648002d0, 68.3314693299d0, &
      71.4729816036d0, 74.6145006437d0, 77.7560256304d0, 80.8975558711d0, &
      84.0390907769d0, 87.1806298436d0, 90.3221726372d0, 93.4637187819d0, &
      96.6052679510d0, 99.7468198587d0, 102.888374254d0, 106.029930916d0, &
      109.171489649d0, 112.313050280d0, 115.454612653d0, 118.596176630d0, &
      121.737742088d0, 124.879308913d0, 128.020877005d0, 131.162446275d0, &
      134.304016638d0, 137.445588020d0, 140.587160352d0, 143.728733573d0, &
      146.870307625d0, 150.011882457d0, 153.153458019d0, 156.295034268d0 /
      real(8)           :: dlt,d1=1.d0
      integer           :: jhe,jho,j0=0
      real(8),parameter :: PI=3.14159265358979d0,C=(1.d0-(2.d0/PI)**2)*0.25d0
      real(8) r
      integer jh,js,n,j
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  GAUSSIAN LATITUDES
      IF(IDRT.EQ.4) THEN
        JH=JMAX/2
        JHE=(JMAX+1)/2
        R=1.d0/SQRT((JMAX+0.5d0)**2+C)
        DO J=1,MIN(JH,JZ)
          ASLATD(J)=COS(BZ(J)*R)
        ENDDO
        DO J=JZ+1,JH
          ASLATD(J)=COS((BZ(JZ)+(J-JZ)*PI)*R)
        ENDDO
        SPMAX=1.d0
        DO WHILE(SPMAX.GT.EPS)
          SPMAX=0.d0
          DO J=1,JH
            PKM1(J)=1.d0
            PK(J)=ASLATD(J)
          ENDDO
          DO N=2,JMAX
            DO J=1,JH
              PKM2(J)=PKM1(J)
              PKM1(J)=PK(J)
              PK(J)=((2*N-1)*ASLATD(J)*PKM1(J)-(N-1)*PKM2(J))/N
            ENDDO
          ENDDO
          DO J=1,JH
            SP=PK(J)*(1.d0-ASLATD(J)**2)/(JMAX*(PKM1(J)-ASLATD(J)*PK(J)))
            ASLATD(J)=ASLATD(J)-SP
            SPMAX=MAX(SPMAX,ABS(SP))
          ENDDO
        ENDDO
!
        DO J=1,JH
          ASLAT(J)=ASLATD(J)
          ASLAT(JMAX+1-J)=-ASLAT(J)
        ENDDO
        IF(JHE.GT.JH) THEN
          ASLAT(JHE)=0.d0
        ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  EQUALLY-SPACED LATITUDES INCLUDING POLES
      ELSEIF(IDRT.EQ.0) THEN
        JH=JMAX/2
        JHE=(JMAX+1)/2
        JHO=JHE-1
        DLT=PI/(JMAX-1)
        ASLAT(1)=1.d0
        DO J=2,JH
          ASLAT(J)=COS((J-1)*DLT)
        ENDDO
!
        DO J=1,JH
          ASLAT(JMAX+1-J)=-ASLAT(J)
        ENDDO
        IF(JHE.GT.JH) THEN
          ASLAT(JHE)=0.d0
        ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  EQUALLY-SPACED LATITUDES EXCLUDING POLES
      ELSEIF(IDRT.EQ.256) THEN
        JH=JMAX/2
        JHE=(JMAX+1)/2
        JHO=JHE
        DLT=PI/JMAX
        ASLAT(1)=1.d0
        DO J=1,JH
          ASLAT(J)=COS((J-0.5)*DLT)
        ENDDO

        DO J=1,JH
          ASLAT(JMAX+1-J)=-ASLAT(J)
        ENDDO
        IF(JHE.GT.JH) THEN
          ASLAT(JHE)=0.d0
        ENDIF
      ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     end subroutine splat4
!----------------------------------------------------------------------
     subroutine splat8(idrt,jmax,aslat)
!$$$
      implicit none
      integer,intent(in)  :: idrt,jmax
      real(8),intent(out) :: aslat(jmax)
!
      integer,parameter   :: KD=SELECTED_REAL_KIND(15,45)
      real(kind=KD)       :: pk(jmax/2),pkm1(jmax/2),pkm2(jmax/2)
      real(kind=KD)       :: aslatd(jmax/2),sp,spmax,eps=10.d0*epsilon(sp)
      integer,parameter   :: jz=50
      real(8) bz(jz)
      data bz        / 2.4048255577d0,  5.5200781103d0, &
       8.6537279129d0, 11.7915344391d0, 14.9309177086d0, 18.0710639679d0, &
      21.2116366299d0, 24.3524715308d0, 27.4934791320d0, 30.6346064684d0, &
      33.7758202136d0, 36.9170983537d0, 40.0584257646d0, 43.1997917132d0, &
      46.3411883717d0, 49.4826098974d0, 52.6240518411d0, 55.7655107550d0, &
      58.9069839261d0, 62.0484691902d0, 65.1899648002d0, 68.3314693299d0, &
      71.4729816036d0, 74.6145006437d0, 77.7560256304d0, 80.8975558711d0, &
      84.0390907769d0, 87.1806298436d0, 90.3221726372d0, 93.4637187819d0, &
      96.6052679510d0, 99.7468198587d0, 102.888374254d0, 106.029930916d0, &
      109.171489649d0, 112.313050280d0, 115.454612653d0, 118.596176630d0, &
      121.737742088d0, 124.879308913d0, 128.020877005d0, 131.162446275d0, &
      134.304016638d0, 137.445588020d0, 140.587160352d0, 143.728733573d0, &
      146.870307625d0, 150.011882457d0, 153.153458019d0, 156.295034268d0 /
      real(8)           :: dlt,d1=1.d0
      integer(4)        :: jhe,jho,j0=0
      real(8),parameter :: PI=3.14159265358979d0,C=(1.d0-(2.d0/PI)**2)*0.25d0
      real(8) r
      integer jh,js,n,j
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  GAUSSIAN LATITUDES
      IF(IDRT.EQ.4) THEN
        JH=JMAX/2
        JHE=(JMAX+1)/2
        R=1.d0/SQRT((JMAX+0.5d0)**2+C)
        DO J=1,MIN(JH,JZ)
          ASLATD(J)=COS(BZ(J)*R)
        ENDDO
        DO J=JZ+1,JH
          ASLATD(J)=COS((BZ(JZ)+(J-JZ)*PI)*R)
        ENDDO
        SPMAX=1.d0
        DO WHILE(SPMAX.GT.EPS)
          SPMAX=0.d0
          DO J=1,JH
            PKM1(J)=1.d0
            PK(J)=ASLATD(J)
          ENDDO
          DO N=2,JMAX
            DO J=1,JH
              PKM2(J)=PKM1(J)
              PKM1(J)=PK(J)
              PK(J)=((2*N-1)*ASLATD(J)*PKM1(J)-(N-1)*PKM2(J))/N
            ENDDO
          ENDDO
          DO J=1,JH
            SP=PK(J)*(1.d0-ASLATD(J)**2)/(JMAX*(PKM1(J)-ASLATD(J)*PK(J)))
            ASLATD(J)=ASLATD(J)-SP
            SPMAX=MAX(SPMAX,ABS(SP))
          ENDDO
        ENDDO
!
        DO J=1,JH
          ASLAT(J)=ASLATD(J)
          ASLAT(JMAX+1-J)=-ASLAT(J)
        ENDDO
        IF(JHE.GT.JH) THEN
          ASLAT(JHE)=0.d0
        ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  EQUALLY-SPACED LATITUDES INCLUDING POLES
      ELSEIF(IDRT.EQ.0) THEN
        JH=JMAX/2
        JHE=(JMAX+1)/2
        JHO=JHE-1
        DLT=PI/(JMAX-1)
        ASLAT(1)=1.d0
        DO J=2,JH
          ASLAT(J)=COS((J-1)*DLT)
        ENDDO
        DO J=1,JH
          ASLAT(JMAX+1-J)=-ASLAT(J)
        ENDDO
        IF(JHE.GT.JH) THEN
          ASLAT(JHE)=0.d0
        ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  EQUALLY-SPACED LATITUDES EXCLUDING POLES
      ELSEIF(IDRT.EQ.256) THEN
        JH=JMAX/2
        JHE=(JMAX+1)/2
        JHO=JHE
        DLT=PI/JMAX
        ASLAT(1)=1.d0
        DO J=1,JH
          ASLAT(J)=COS((J-0.5d0)*DLT)
        ENDDO
!
        DO J=1,JH
          ASLAT(JMAX+1-J)=-ASLAT(J)
        ENDDO
        IF(JHE.GT.JH) THEN
          ASLAT(JHE)=0.d0
        ENDIF
      ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     end subroutine splat8
!-----------------------------------------------------------------------
!
     subroutine get_outfile(nfl, filename, outfile_name,noutfile)
       integer, intent(in)          :: nfl
       character(*), intent(in)     :: filename(:,:)
       character(*), intent(inout)  :: outfile_name(:)
       integer, intent(inout)       :: noutfile

       integer        :: i,j,n,idx
       logical        :: found
!
       noutfile = 0
       do i=1,nfl

         loopj: do j=1, 2000

           if( trim(filename(j,i)) == '') exit loopj
           if( trim(filename(j,i)) == 'none') cycle

           found = .false.
           loopn: do n=1, noutfile
             if(trim(filename(j,i)) == trim(outfile_name(n))) then
               found = .true.
               exit loopn
             endif
           enddo loopn

           if (.not.found) then
             noutfile = noutfile + 1
             outfile_name(noutfile) = trim(filename(j,i))
!             print *,'in get outfile,noutfile=', noutfile,' outfile=',trim(filename(j,i))
           endif
         enddo loopj
!
       enddo

     end subroutine get_outfile
!
!-----------------------------------------------------------------------
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!-----------------------------------------------------------------------
!
    end module  module_wrt_grid_comp
!
!-----------------------------------------------------------------------

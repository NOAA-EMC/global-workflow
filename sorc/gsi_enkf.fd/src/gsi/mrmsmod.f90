module mrmsmod 
implicit none
private
public l_mrms_run
public l_mrms_sparse_netcdf 
public mrms_listfile
public l_new_cldvar,l_ens_dbz_clip 
public load_mrms_data_info

logical l_mrms_run
logical l_mrms_sparse_netcdf
logical,save::l_ens_dbz_clip=.false.
logical,save::l_new_cldvar=.false.
character(len=*),parameter:: mrms_listfile='mrms_listfile'
contains

subroutine load_mrms_data_info (mrms_listfile,nrows0,ntot_mrms,nrows_mrms,nrows,obsfile_all,dfile,dtype,ditype,dplat,dsis,dval,dthin,ipoint,dsfcalc,time_window,rcname)

   use kinds, only: r_kind,i_kind
   use file_utility, only : get_lun
   use mpeu_util, only: gettablesize
   use mpeu_util, only: gettable
   use mpeu_util, only: getindex

   implicit none

   integer(i_kind),parameter::nobstype_mrms=2 ! first for vr, and the second for ref
   integer(i_kind),intent(in):: nrows0,ntot_mrms,nrows_mrms,nrows
   character(len=*),intent(in),optional ::mrms_listfile, rcname ! input filename
   character(len=*),intent(inout),dimension(nrows):: dtype
   character(len=*),intent(inout),dimension(nrows):: ditype
   character(len=*),intent(inout),dimension(nrows):: dplat
   character(20),intent(inout),dimension(nrows):: obsfile_all
   character(*),intent(inout),dimension(nrows):: dfile
   character(20),intent(inout),dimension(nrows):: dsis
   real(r_kind) ,intent(inout),dimension(nrows):: dval
   integer(i_kind),intent(inout),dimension(nrows):: dsfcalc,dthin,ipoint
   real(r_kind) ,intent(inout),dimension(nrows):: time_window

   character(len=*),parameter:: tbname_mrms='OBS_INPUT_MRMS::'
   integer(i_kind) luin_mrms,ii0,itype_mrms
   character(len=256),allocatable,dimension(:):: utable_mrms_list
   character(len=256),allocatable,dimension(:):: utable_mrms


   real(r_kind),allocatable,dimension(:):: dmesh_mrms
   character(10),allocatable,dimension(:):: dtype_mrms,ditype_mrms
   character(11),allocatable,dimension(:):: dplat_mrms
   character(120),allocatable,dimension(:):: dfile_mrms
   character(20),allocatable,dimension(:):: dsis_mrms
   real(r_kind) ,allocatable,dimension(:):: dval_mrms
   integer(i_kind) ,allocatable,dimension(:):: dsfcalc_mrms,dthin_mrms,ipoint_mrms
   real(r_kind) ,allocatable,dimension(:):: time_window_mrms
   real(r_kind) ,save:: time_window_mrms_max=3.0_r_kind

   integer(i_kind):: ii,ier

   luin_mrms=get_lun()
   open(luin_mrms,file=trim(mrms_listfile),form='formatted',iostat=ier)
   allocate(utable_mrms_list(nrows_mrms))
   call gettable(mrms_listfile,luin_mrms,ntot_mrms,nrows_mrms,utable_mrms_list)
   if(luin_mrms/=5) close(luin_mrms )
   allocate(dfile_mrms(nobstype_mrms),dtype_mrms(nobstype_mrms),dplat_mrms(nobstype_mrms),&
            dsis_mrms(nobstype_mrms),dval_mrms(nobstype_mrms),dthin_mrms(nobstype_mrms),dsfcalc_mrms(nobstype_mrms),&
            dmesh_mrms(nobstype_mrms), &
            time_window_mrms(nobstype_mrms))
   allocate(ditype_mrms(nobstype_mrms),ipoint_mrms(nobstype_mrms))
   ! variables participating in state vector
   if (present(rcname)) then
      luin_mrms=get_lun()
      open(luin_mrms,file=trim(rcname),form='formatted')
   else
      luin_mrms=5
   endif
   allocate(utable_mrms(nobstype_mrms))
   call gettable(tbname_mrms,luin_mrms,nobstype_mrms,nobstype_mrms,utable_mrms)
   if(luin_mrms/=5) close(luin_mrms)

   do ii=1,nobstype_mrms

      read(utable_mrms(ii),*)dmesh_mrms(ii), dfile_mrms(ii),& ! local file name from which to read observatinal data
                         dtype_mrms(ii),& ! character string identifying type of observatio
                         dplat_mrms(ii),& ! currently contains satellite id (no meaning for non-sat data)
                         dsis_mrms(ii), & ! sensor/instrument/satellite identifier for info files
                         dval_mrms(ii), & !
                         dthin_mrms(ii),& ! thinning flag (1=thinning on; otherwise off)
                         dsfcalc_mrms(ii) ! use orig bilinear FOV surface calculation (routine deter_sfc)

    if(trim(dplat_mrms(ii))=='null') dplat_mrms(ii)=' '
      ditype_mrms(ii)= ' '                    ! character string identifying group type of ob (see read_obs)
      ipoint_mrms(ii)= 0                      ! default pointer (values set in gsisub) _RT: This is never needed
      time_window_mrms(ii) = time_window_mrms_max  ! default to maximum time window

   enddo

   deallocate(utable_mrms)
   do ii=1,nrows_mrms
      ii0=nrows0+ii

      read(utable_mrms_list(ii),*) dfile(ii0) ! ! local file name from which to read observatinal data
      if(index(dfile(ii0),'vr') > 0) then
        itype_mrms=1 ! for vr
      elseif(index(dfile(ii0),'ref') > 0) then
        itype_mrms=2 ! for ref
      else
        write(6,*) 'the mrms files to be read not recognizable, stop'
        call stop2(255)
      endif

      dtype(ii0)=  dtype_mrms(itype_mrms) ! ! character string identifying type of observatio
      dplat(ii0)=dplat_mrms(itype_mrms) ! currently contains satellite id (no meaning for non-sat data)
      dsis (ii0)=          dsis_mrms(itype_mrms) !  sensor/instrument/satellite identifier for info files
      dval(ii0)=dval_mrms(itype_mrms)  !
      dthin(ii0)= dthin_mrms(itype_mrms) ! thinning flag (1=thinning on; otherwise off)
      dsfcalc(ii0)=dsfcalc_mrms(itype_mrms) ! use orig bilinear FOV surface calculation (routine deter_sfc)
      ditype(ii0)=ditype_mrms(itype_mrms)
      ipoint(ii0)=ipoint_mrms(itype_mrms)
      time_window(ii0)=time_window_mrms(itype_mrms)
      write(obsfile_all(ii0),'(a,i4.4)') 'obs_input.', ii0      ! name of scratch file to hold obs data
   enddo

   deallocate(utable_mrms_list)
   deallocate(dfile_mrms,dtype_mrms,dplat_mrms,&
           dsis_mrms,dval_mrms,dthin_mrms,dsfcalc_mrms,dmesh_mrms,&
           time_window_mrms)
   deallocate(ditype_mrms,ipoint_mrms)




end subroutine load_mrms_data_info
end module mrmsmod

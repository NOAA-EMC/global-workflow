!-------------------------------------------------------------------------------
module sfc_fields
!
 use machine, only : kind_phys
  type,public:: sfc_fields
    real(kind=kind_phys),pointer:: tsea(:,:)=>null()
    real(kind=kind_phys),pointer:: smc(:,:,:)=>null()
    real(kind=kind_phys),pointer:: sheleg(:,:)=>null()
    real(kind=kind_phys),pointer:: stc(:,:,:)=>null()
    real(kind=kind_phys),pointer:: tg3(:,:)=>null()
    real(kind=kind_phys),pointer:: zorl(:,:)=>null()
    real(kind=kind_phys),pointer:: cv(:,:)=>null()
    real(kind=kind_phys),pointer:: cvb(:,:)=>null()
    real(kind=kind_phys),pointer:: cvt(:,:)=>null()
    real(kind=kind_phys),pointer:: alvsf(:,:)=>null()
    real(kind=kind_phys),pointer:: alvwf(:,:)=>null()
    real(kind=kind_phys),pointer:: alnsf(:,:)=>null()
    real(kind=kind_phys),pointer:: alnwf(:,:)=>null()
    real(kind=kind_phys),pointer:: slmsk(:,:)=>null()
    real(kind=kind_phys),pointer:: vfrac(:,:)=>null()
    real(kind=kind_phys),pointer:: canopy(:,:)=>null()
    real(kind=kind_phys),pointer:: f10m(:,:)=>null()
    real(kind=kind_phys),pointer:: t2m(:,:)=>null()
    real(kind=kind_phys),pointer:: q2m(:,:)=>null()
    real(kind=kind_phys),pointer:: vtype(:,:)=>null()
    real(kind=kind_phys),pointer:: stype(:,:)=>null()
    real(kind=kind_phys),pointer:: facsf(:,:)=>null()
    real(kind=kind_phys),pointer:: facwf(:,:)=>null()
    real(kind=kind_phys),pointer:: uustar(:,:)=>null()
    real(kind=kind_phys),pointer:: ffmm(:,:)=>null()
    real(kind=kind_phys),pointer:: ffhh(:,:)=>null()
    real(kind=kind_phys),pointer:: hice(:,:)=>null()
    real(kind=kind_phys),pointer:: fice(:,:)=>null()
    real(kind=kind_phys),pointer:: tisfc(:,:)=>null()
    real(kind=kind_phys),pointer:: tprcp(:,:)=>null()
    real(kind=kind_phys),pointer:: srflag(:,:)=>null()
    real(kind=kind_phys),pointer:: snwdph(:,:)=>null()
    real(kind=kind_phys),pointer:: slc(:,:,:)=>null()
    real(kind=kind_phys),pointer:: shdmin(:,:)=>null()
    real(kind=kind_phys),pointer:: shdmax(:,:)=>null()
    real(kind=kind_phys),pointer:: slope(:,:)=>null()
    real(kind=kind_phys),pointer:: snoalb(:,:)=>null()
    real(kind=kind_phys),pointer:: orog(:,:)=>null()
  end type
  contains
  subroutine sfc_aldata(data,dim1,dim2,dim3,iret)
    implicit none
    type(sfc_fields),intent(inout):: data
    integer,intent(in):: dim1, dim2, dim3
    integer,intent(out):: iret
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call sfc_axdata(data,iret)
    allocate(&
      data%tsea(dim1,dim2),      &
      data%smc(dim1,dim2,dim3),  &
      data%sheleg(dim1,dim2),    &
      data%stc(dim1,dim2,dim3),  &
      data%tg3(dim1,dim2),       &
      data%zorl(dim1,dim2),      &
      data%cv(dim1,dim2),        &
      data%cvb(dim1,dim2),       &
      data%cvt(dim1,dim2),       &
      data%alvsf(dim1,dim2),     &
      data%alvwf(dim1,dim2),     &
      data%alnsf(dim1,dim2),     &
      data%alnwf(dim1,dim2),     &
      data%slmsk(dim1,dim2),     &
      data%vfrac(dim1,dim2),     &
      data%canopy(dim1,dim2),    &
      data%f10m(dim1,dim2),      &
      data%t2m(dim1,dim2),       &
      data%q2m(dim1,dim2),       &
      data%vtype(dim1,dim2),     &
      data%stype(dim1,dim2),     &
      data%facsf(dim1,dim2),     &
      data%facwf(dim1,dim2),     &
      data%uustar(dim1,dim2),    &
      data%ffmm(dim1,dim2),      &
      data%ffhh(dim1,dim2),      &
      data%hice(dim1,dim2),      &
      data%fice(dim1,dim2),      &
      data%tisfc(dim1,dim2),     &
      data%tprcp(dim1,dim2),     &
      data%srflag(dim1,dim2),    &
      data%snwdph(dim1,dim2),    &
      data%slc(dim1,dim2,dim3),  &
      data%shdmin(dim1,dim2),    &
      data%shdmax(dim1,dim2),    &
      data%slope(dim1,dim2),     &
      data%snoalb(dim1,dim2),    &
      data%orog(dim1,dim2),      &
      stat=iret)
    if(iret.ne.0) iret=-3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine sfc_axdata(data,iret)
    implicit none
    type(sfc_fields),intent(inout):: data
    integer,intent(out):: iret
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    deallocate(                   &
      data%tsea,                  &
      data%smc,                   &
      data%sheleg,                &
      data%stc,                   &
      data%tg3,                   &
      data%zorl,                  &
      data%cv,                    &
      data%cvb,                   &
      data%cvt,                   &
      data%alvsf,                 &
      data%alvwf,                 &
      data%alnsf,                 &
      data%alnwf,                 &
      data%slmsk,                 &
      data%vfrac,                 &
      data%canopy,                &
      data%f10m,                  &
      data%t2m,                   &
      data%q2m,                   &
      data%vtype,                 &
      data%stype,                 &
      data%facsf,                 &
      data%facwf,                 &
      data%uustar,                &
      data%ffmm,                  &
      data%ffhh,                  &
      data%hice,                  &
      data%fice,                  &
      data%tisfc,                 &
      data%tprcp,                 &
      data%srflag,                &
      data%snwdph,                &
      data%slc,                   &
      data%shdmin,                &
      data%shdmax,                &
      data%slope,                 &
      data%snoalb,                &
      data%orog,                  &
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
end module

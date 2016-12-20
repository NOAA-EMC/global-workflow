!
! !MODULE: Sfc_Flx_ESMFMod  ---              Definition of the surface
!                                            fields in the ESMF internal state.
!
! !DESCRIPTION: Sfc_Flx_ESMFMod ---            Define the surfacee  variables
!                                              in the ESMF internal state.
!---------------------------------------------------------------------------
! !REVISION HISTORY:
!
!  March 2007      Shrinivas Moorthi Initial code.
!  March 2008      Y.-T. Hou    add Sunshine_Duration (suntim) to Flx_Var_Data
!  Jan 2009        Moorthi      add Ho Chun's changes
!  Apr 2009        Y.-T. Hou    add surface lw emissivity (sfcemis)
!  ??  2009        J Wang       add spfhmax/spfhmin
!  June 2013       H Chuang     add sr 
!  March 2014      F Yang       removed pointers for fixing digital filter
!
! !INTERFACE:
!
 MODULE Sfc_Flx_ESMFMod

 use machine , only : kind_phys

 IMPLICIT none

 TYPE Sfc_Var_Data
    real(kind=kind_phys),allocatable:: tsea(:,:)
    real(kind=kind_phys),allocatable:: smc(:,:,:)
    real(kind=kind_phys),allocatable:: sheleg(:,:)
    real(kind=kind_phys),allocatable:: sncovr(:,:)
    real(kind=kind_phys),allocatable:: stc(:,:,:)
    real(kind=kind_phys),allocatable:: tg3(:,:)
    real(kind=kind_phys),allocatable:: zorl(:,:)
    real(kind=kind_phys),allocatable:: cv(:,:)
    real(kind=kind_phys),allocatable:: cvb(:,:)
    real(kind=kind_phys),allocatable:: cvt(:,:)
    real(kind=kind_phys),allocatable:: alvsf(:,:)
    real(kind=kind_phys),allocatable:: alvwf(:,:)
    real(kind=kind_phys),allocatable:: alnsf(:,:)
    real(kind=kind_phys),allocatable:: alnwf(:,:)
    real(kind=kind_phys),allocatable:: slmsk(:,:)
    real(kind=kind_phys),allocatable:: vfrac(:,:)
    real(kind=kind_phys),allocatable:: canopy(:,:)
    real(kind=kind_phys),allocatable:: f10m(:,:)
    real(kind=kind_phys),allocatable:: t2m(:,:)
    real(kind=kind_phys),allocatable:: q2m(:,:)
    real(kind=kind_phys),allocatable:: vtype(:,:)
    real(kind=kind_phys),allocatable:: stype(:,:)
    real(kind=kind_phys),allocatable:: facsf(:,:)
    real(kind=kind_phys),allocatable:: facwf(:,:)
    real(kind=kind_phys),allocatable:: uustar(:,:)
    real(kind=kind_phys),allocatable:: ffmm(:,:)
    real(kind=kind_phys),allocatable:: ffhh(:,:)
    real(kind=kind_phys),allocatable:: hice(:,:)
    real(kind=kind_phys),allocatable:: fice(:,:)
    real(kind=kind_phys),allocatable:: tisfc(:,:)
    real(kind=kind_phys),allocatable:: tprcp(:,:)
    real(kind=kind_phys),allocatable:: srflag(:,:)
    real(kind=kind_phys),allocatable:: snwdph(:,:)
    real(kind=kind_phys),allocatable:: slc(:,:,:)
    real(kind=kind_phys),allocatable:: shdmin(:,:)
    real(kind=kind_phys),allocatable:: shdmax(:,:)
    real(kind=kind_phys),allocatable:: slope(:,:)
    real(kind=kind_phys),allocatable:: snoalb(:,:)
    real(kind=kind_phys),allocatable:: oro(:,:)
    real(kind=kind_phys),allocatable:: oro_uf(:,:)
 end type Sfc_Var_Data
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 TYPE Flx_Var_Data
    real(kind=kind_phys),allocatable:: SFCDSW(:,:)
    real(kind=kind_phys),allocatable:: COSZEN(:,:)
    real(kind=kind_phys),allocatable:: PWAT(:,:)
    real(kind=kind_phys),allocatable:: TMPMIN(:,:)
    real(kind=kind_phys),allocatable:: TMPMAX(:,:)
    real(kind=kind_phys),allocatable:: SPFHMIN(:,:)
    real(kind=kind_phys),allocatable:: SPFHMAX(:,:)
    real(kind=kind_phys),allocatable:: DUSFC(:,:)
    real(kind=kind_phys),allocatable:: DVSFC(:,:)
    real(kind=kind_phys),allocatable:: DTSFC(:,:)
    real(kind=kind_phys),allocatable:: DQSFC(:,:)
    real(kind=kind_phys),allocatable:: DLWSFC(:,:)
    real(kind=kind_phys),allocatable:: ULWSFC(:,:)
    real(kind=kind_phys),allocatable:: GFLUX(:,:)
    real(kind=kind_phys),allocatable:: RUNOFF(:,:)
    real(kind=kind_phys),allocatable:: EP(:,:)
    real(kind=kind_phys),allocatable:: CLDWRK(:,:)
    real(kind=kind_phys),allocatable:: DUGWD(:,:)
    real(kind=kind_phys),allocatable:: DVGWD(:,:)
    real(kind=kind_phys),allocatable:: PSMEAN(:,:)
    real(kind=kind_phys),allocatable:: GESHEM(:,:)
    real(kind=kind_phys),allocatable:: BENGSH(:,:)
    real(kind=kind_phys),allocatable:: SFCNSW(:,:)
    real(kind=kind_phys),allocatable:: SFCDLW(:,:)
    real(kind=kind_phys),allocatable:: TSFLW(:,:)
    real(kind=kind_phys),allocatable:: PSURF(:,:)
    real(kind=kind_phys),allocatable:: U10M(:,:)
    real(kind=kind_phys),allocatable:: V10M(:,:)
    real(kind=kind_phys),allocatable:: HPBL(:,:)
    real(kind=kind_phys),allocatable:: CHH(:,:)
    real(kind=kind_phys),allocatable:: CMM(:,:)
    real(kind=kind_phys),allocatable:: EPI(:,:)
    real(kind=kind_phys),allocatable:: DLWSFCI(:,:)
    real(kind=kind_phys),allocatable:: ULWSFCI(:,:)
    real(kind=kind_phys),allocatable:: USWSFCI(:,:)
    real(kind=kind_phys),allocatable:: DSWSFCI(:,:)
    real(kind=kind_phys),allocatable:: DTSFCI(:,:)
    real(kind=kind_phys),allocatable:: DQSFCI(:,:)
    real(kind=kind_phys),allocatable:: GFLUXI(:,:)
    real(kind=kind_phys),allocatable:: SRUNOFF(:,:)
    real(kind=kind_phys),allocatable:: T1(:,:)
    real(kind=kind_phys),allocatable:: Q1(:,:)
    real(kind=kind_phys),allocatable:: U1(:,:)
    real(kind=kind_phys),allocatable:: V1(:,:)
    real(kind=kind_phys),allocatable:: ZLVL(:,:)
    real(kind=kind_phys),allocatable:: EVBSA(:,:)
    real(kind=kind_phys),allocatable:: EVCWA(:,:)
    real(kind=kind_phys),allocatable:: TRANSA(:,:)
    real(kind=kind_phys),allocatable:: SBSNOA(:,:)
    real(kind=kind_phys),allocatable:: SNOWCA(:,:)
    real(kind=kind_phys),allocatable:: SOILM(:,:)
    real(kind=kind_phys),allocatable:: SNOHFA(:,:)
    real(kind=kind_phys),allocatable:: SMCWLT2(:,:)
    real(kind=kind_phys),allocatable:: SMCREF2(:,:)
    real(kind=kind_phys),allocatable:: suntim(:,:)       ! sunshine duration time
    real(kind=kind_phys),allocatable:: sfcemis(:,:)      ! surface emissivity
    real(kind=kind_phys),allocatable:: sr(:,:)
 end type Flx_Var_Data
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    contains
    subroutine sfcvar_aldata(dim1,dim2,dim3,data,iret)
       implicit none
       integer, intent(in)              :: dim1, dim2, dim3
       type(sfc_var_data),intent(inout) :: data
       integer, intent(out)             :: iret
!
allocate(                          &
      data%tsea   (dim1,dim2),     &
      data%smc    (dim1,dim3,dim2),&
      data%sheleg (dim1,dim2),     &
      data%sncovr (dim1,dim2),     &
      data%stc    (dim1,dim3,dim2),&
      data%tg3    (dim1,dim2),     &
      data%zorl   (dim1,dim2),     &
      data%cv     (dim1,dim2),     &
      data%cvb    (dim1,dim2),     &
      data%cvt    (dim1,dim2),     &
      data%alvsf  (dim1,dim2),     &
      data%alvwf  (dim1,dim2),     &
      data%alnsf  (dim1,dim2),     &
      data%alnwf  (dim1,dim2),     &
      data%slmsk  (dim1,dim2),     &
      data%vfrac  (dim1,dim2),     &
      data%canopy (dim1,dim2),     &
      data%f10m   (dim1,dim2),     &
      data%t2m    (dim1,dim2),     &
      data%q2m    (dim1,dim2),     &
      data%vtype  (dim1,dim2),     &
      data%stype  (dim1,dim2),     &
      data%facsf  (dim1,dim2),     &
      data%facwf  (dim1,dim2),     &
      data%uustar (dim1,dim2),     &
      data%ffmm   (dim1,dim2),     &
      data%ffhh   (dim1,dim2),     &
      data%hice   (dim1,dim2),     &
      data%fice   (dim1,dim2),     &
      data%tisfc  (dim1,dim2),     &
      data%tprcp  (dim1,dim2),     &
      data%srflag (dim1,dim2),     &
      data%snwdph (dim1,dim2),     &
      data%slc    (dim1,dim3,dim2),&
      data%shdmin (dim1,dim2),     &
      data%shdmax (dim1,dim2),     &
      data%slope  (dim1,dim2),     &
      data%snoalb (dim1,dim2),     &
      data%oro    (dim1,dim2),     &
      data%oro_uf (dim1,dim2),     &
      stat=iret)

      data%tsea   = 0.0
      data%smc    = 0.0
      data%sheleg = 0.0
      data%sncovr = 0.0
      data%stc    = 0.0
      data%tg3    = 0.0
      data%zorl   = 0.0
      data%cv     = 0.0
      data%cvb    = 0.0
      data%cvt    = 0.0
      data%alvsf  = 0.0
      data%alvwf  = 0.0
      data%alnsf  = 0.0
      data%alnwf  = 0.0
      data%slmsk  = 0.0
      data%vfrac  = 0.0
      data%canopy = 0.0
      data%f10m   = 0.0
      data%t2m    = 0.0
      data%q2m    = 0.0
      data%vtype  = 0.0
      data%stype  = 0.0
      data%facsf  = 0.0
      data%facwf  = 0.0
      data%uustar = 0.0
      data%ffmm   = 0.0
      data%ffhh   = 0.0
      data%hice   = 0.0
      data%fice   = 0.0
      data%tisfc  = 0.0
      data%tprcp  = 0.0
      data%srflag = 0.0
      data%snwdph = 0.0
      data%slc    = 0.0
      data%shdmin = 0.0
      data%shdmax = 0.0
      data%slope  = 0.0
      data%snoalb = 0.0
      data%oro    = 0.0
      data%oro_uf = 0.0
    if(iret.ne.0) iret=-3
    return
  end subroutine
    subroutine flxvar_aldata(dim1,dim2,data,iret)
       implicit none
       integer, intent(in)              :: dim1, dim2
       type(flx_var_data),intent(inout) :: data
       integer, intent(out)             :: iret
!
!   allocate(data%SFCDSW  (dim1,dim2))
    allocate(                  &
     data%SFCDSW  (dim1,dim2), &
     data%COSZEN  (dim1,dim2), &
     data%PWAT    (dim1,dim2), &
     data%TMPMIN  (dim1,dim2), &
     data%TMPMAX  (dim1,dim2), &
     data%SPFHMIN (dim1,dim2), &
     data%SPFHMAX (dim1,dim2), &
     data%DUSFC   (dim1,dim2), &
     data%DVSFC   (dim1,dim2), &
     data%DTSFC   (dim1,dim2), &
     data%DQSFC   (dim1,dim2), &
     data%DLWSFC  (dim1,dim2), &
     data%ULWSFC  (dim1,dim2), &
     data%GFLUX   (dim1,dim2), &
     data%RUNOFF  (dim1,dim2), &
     data%EP      (dim1,dim2), &
     data%CLDWRK  (dim1,dim2), &
     data%DUGWD   (dim1,dim2), &
     data%DVGWD   (dim1,dim2), &
     data%PSMEAN  (dim1,dim2), &
     data%GESHEM  (dim1,dim2), &
     data%BENGSH  (dim1,dim2), &
     data%SFCNSW  (dim1,dim2), &
     data%SFCDLW  (dim1,dim2), &
     data%TSFLW   (dim1,dim2), &
     data%PSURF   (dim1,dim2), &
     data%U10M    (dim1,dim2), &
     data%V10M    (dim1,dim2), &
     data%HPBL    (dim1,dim2), &
     data%CHH     (dim1,dim2), &
     data%CMM     (dim1,dim2), &
     data%EPI     (dim1,dim2), &
     data%DLWSFCI (dim1,dim2), &
     data%ULWSFCI (dim1,dim2), &
     data%USWSFCI (dim1,dim2), &
     data%DSWSFCI (dim1,dim2), &
     data%DTSFCI  (dim1,dim2), &
     data%DQSFCI  (dim1,dim2), &
     data%GFLUXI  (dim1,dim2), &
     data%SRUNOFF (dim1,dim2), &
     data%T1      (dim1,dim2), &
     data%Q1      (dim1,dim2), &
     data%U1      (dim1,dim2), &
     data%V1      (dim1,dim2), &
     data%ZLVL    (dim1,dim2), &
     data%EVBSA   (dim1,dim2), &
     data%EVCWA   (dim1,dim2), &
     data%TRANSA  (dim1,dim2), &
     data%SBSNOA  (dim1,dim2), &
     data%SNOWCA  (dim1,dim2), &
     data%SOILM   (dim1,dim2), &
     data%SNOHFA  (dim1,dim2), &
     data%SMCWLT2 (dim1,dim2), &
     data%SMCREF2 (dim1,dim2), &
     data%suntim  (dim1,dim2), &                
     data%sfcemis (dim1,dim2), &               
     data%sr      (dim1,dim2), &
     stat=iret)

     data%SFCDSW  = 0.0
     data%COSZEN  = 0.0
     data%PWAT    = 0.0
     data%SFCNSW  = 0.0
     data%SFCDLW  = 0.0
     data%TSFLW   = 0.0
     data%PSURF   = 0.0
     data%U10M    = 0.0
     data%V10M    = 0.0
     data%HPBL    = 0.0
     data%CHH     = 0.0
     data%CMM     = 0.0
     data%EPI     = 0.0
     data%DLWSFCI = 0.0
     data%ULWSFCI = 0.0
     data%USWSFCI = 0.0
     data%DSWSFCI = 0.0
     data%DTSFCI  = 0.0
     data%DQSFCI  = 0.0
     data%GFLUXI  = 0.0
     data%T1      = 0.0
     data%Q1      = 0.0
     data%U1      = 0.0
     data%V1      = 0.0
     data%ZLVL    = 0.0
     data%SOILM   = 0.0
     data%SMCWLT2 = 0.0
     data%SMCREF2 = 0.0
     data%sfcemis = 0.0               

    if(iret.ne.0) iret=-4
    return
  end subroutine
    subroutine flx_init(data,iret)
       implicit none
       type(flx_var_data),intent(inout) :: data
       integer, intent(out)             :: iret
!
       iret         = 0                           
       data%TMPMIN  = 1.e10
       data%TMPMAX  = 0.
       data%SPFHMIN = 1.e10
       data%SPFHMAX = 0.
       data%GESHEM  = 0.
       data%BENGSH  = 0.
       data%DUSFC   = 0.
       data%DVSFC   = 0.
       data%DTSFC   = 0.
       data%DQSFC   = 0.
       data%DLWSFC  = 0.
       data%ULWSFC  = 0.
       data%suntim  = 0.
       data%GFLUX   = 0.
!
       data%RUNOFF  = 0.
       data%EP      = 0.
       data%CLDWRK  = 0.
       data%DUGWD   = 0.
       data%DVGWD   = 0.
       data%PSMEAN  = 0.
!
       data%EVBSA   = 0.
       data%EVCWA   = 0.
       data%TRANSA  = 0.
       data%SBSNOA  = 0.
       data%SNOWCA  = 0.
       data%SRUNOFF = 0.
       data%SNOHFA  = 0.
       data%sr      = 0.


    if(iret.ne.0) iret=-4
    return
     return
  end subroutine
 END MODULE Sfc_Flx_ESMFMod

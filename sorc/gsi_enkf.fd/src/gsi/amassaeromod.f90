module amassaeromod 

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    amass2aero_tl and amass2aero_ad 
!                These routines are for fv3_cmaq_regional DA with control varialbes
!                of total mass per mode (I,J,K).
!                amass2aero_tl is the linear process to partion perturbation of total mass 
!                to individual aerosol specie. amass2aero_ad is the adjoint
!                process.
!                 
       
! program history log:
!   2022-05-24  H.Wang  


   use kinds, only: r_kind,i_kind
   use constants, only: zero,one,r0_05,t0c,fv,max_varname_length
   use gridmod, only: lat2,lon2,nsig
   use mpeu_util, only: die
   use guess_grids, only: ntguessig
   use gsi_bundlemod, only: gsi_bundle
   use gsi_bundlemod, only: gsi_bundlegetpointer
   use gsi_metguess_mod, only: gsi_metguess_bundle
   use gsi_chemguess_mod, only: gsi_chemguess_bundle
   use gridmod, only: fv3_cmaq_regional
   use chemmod, only: naero_cmaq_fv3,aeronames_cmaq_fv3,imodes_cmaq_fv3,laeroana_fv3cmaq

   implicit none

   PRIVATE
   PUBLIC amass2aero_tl
   PUBLIC amass2aero_ad
   integer(i_kind) :: istatus


   contains

subroutine amass2aero_tl(sval,wbundle,aerosols,naerosols)
   implicit none

! Declare passed variables
   type(gsi_bundle),intent(inout):: sval
   type(gsi_bundle),intent(in):: wbundle
   character(len=24),parameter :: myname = 'amass2aero_tl'
   integer(i_kind),intent(in) :: naerosols
   character(len=max_varname_length),intent(in):: aerosols(naerosols)

! Declare local variables
   integer(i_kind) i,j,k,ic,istatus,it,ier
   real(r_kind),pointer,dimension(:,:,:) :: sv_rank3

   real(r_kind),pointer,dimension(:,:,:) :: cv_amassi,cv_amassj,cv_amassk

   real(r_kind),pointer,dimension(:,:,:) :: ges_aero=>NULL()

   real(r_kind),pointer,dimension(:,:,:) :: ges_amassi=>NULL()
   real(r_kind),pointer,dimension(:,:,:) :: ges_amassj=>NULL()
   real(r_kind),pointer,dimension(:,:,:) :: ges_amassk=>NULL()
  
    
! Get background amassi,amassj,amassk 
   ier = 0
   it= ntguessig
   call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it),'amassi',ges_amassi,istatus );ier=ier+istatus
   call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it),'amassj',ges_amassj,istatus );ier=ier+istatus
   call GSI_BundleGetPointer ( GSI_ChemGuess_Bundle(it),'amassk',ges_amassk,istatus );ier=ier+istatus

   if (ier/=0) call die(trim(myname),'cannot get first guess amassi-k ier =',ier)

! Get pointer to required control variable
   call gsi_bundlegetpointer (wbundle,'amassi',cv_amassi,istatus)
   call gsi_bundlegetpointer (wbundle,'amassj',cv_amassj,istatus)
   call gsi_bundlegetpointer (wbundle,'amassk',cv_amassk,istatus)

! Split total amass into aerosol species 
   do ic=1,naerosols
      call GSI_BundleGetPointer (GSI_ChemGuess_Bundle(it),trim(aerosols(ic)),ges_aero,istatus )
      call gsi_bundlegetpointer (sval,trim(aerosols(ic)),sv_rank3,istatus)
      if (istatus/=0) cycle
      sv_rank3=zero
      if (imodes_cmaq_fv3(ic)==1) then
        do k=1,nsig
          do j=1,lon2
            do i=1,lat2
              sv_rank3(i,j,k)=cv_amassi(i,j,k)*ges_aero(i,j,k)/ges_amassi(i,j,k)
            end do
          end do
        end do
      elseif (imodes_cmaq_fv3(ic)==2) then
        do k=1,nsig
          do j=1,lon2
            do i=1,lat2
              sv_rank3(i,j,k)=cv_amassj(i,j,k)*ges_aero(i,j,k)/ges_amassj(i,j,k)
            end do
          end do
        end do
      else
        do k=1,nsig
          do j=1,lon2
            do i=1,lat2
              sv_rank3(i,j,k)=cv_amassk(i,j,k)*ges_aero(i,j,k)/ges_amassk(i,j,k)
            end do
          end do
        end do
      end if
   end do

return
end subroutine amass2aero_tl

subroutine amass2aero_ad(rval,wbundle,aerosols,naerosols)
   implicit none

! Declare passed variables
   type(gsi_bundle),intent(in):: rval
   type(gsi_bundle),intent(inout):: wbundle
   integer(i_kind),intent(in) :: naerosols
   character(len=max_varname_length),intent(in):: aerosols(naerosols)

! Declare local variables
   integer(i_kind) i,j,k,ic,istatus,it,ier
   real(r_kind),pointer,dimension(:,:,:) :: sv_rank3

   real(r_kind),pointer,dimension(:,:,:) :: cv_amassi,cv_amassj,cv_amassk

   real(r_kind),pointer,dimension(:,:,:) :: ges_aero=>NULL()

   real(r_kind),pointer,dimension(:,:,:) :: ges_amassi=>NULL()
   real(r_kind),pointer,dimension(:,:,:) :: ges_amassj=>NULL()
   real(r_kind),pointer,dimension(:,:,:) :: ges_amassk=>NULL()


   ier = 0
   it= ntguessig
   call GSI_BundleGetPointer (GSI_ChemGuess_Bundle(it),'amassi',ges_amassi,istatus );ier=ier+istatus
   call GSI_BundleGetPointer (GSI_ChemGuess_Bundle(it),'amassj',ges_amassj,istatus );ier=ier+istatus
   call GSI_BundleGetPointer (GSI_ChemGuess_Bundle(it),'amassk',ges_amassk,istatus );ier=ier+istatus

   if (ier/=0) then
      write(6,*),"get first guess amassi-k failed in amassaeromod.f90 ! Stop"
      stop
   end if

! Get pointer to required control variable
   call gsi_bundlegetpointer (wbundle,'amassi',cv_amassi,istatus)
   call gsi_bundlegetpointer (wbundle,'amassj',cv_amassj,istatus)
   call gsi_bundlegetpointer (wbundle,'amassk',cv_amassk,istatus)

! ad of partitioning amass into aerosol speccies 
   do ic=1,naerosols
      ier = 0
      call GSI_BundleGetPointer (GSI_ChemGuess_Bundle(it),trim(aerosols(ic)),ges_aero,istatus );ier=ier+istatus
      call gsi_bundlegetpointer (rval,trim(aerosols(ic)),sv_rank3,istatus);ier=ier+istatus
      if (ier/=0) cycle

      if (imodes_cmaq_fv3(ic)==1) then
        do k=1,nsig
          do j=1,lon2
            do i=1,lat2
              cv_amassi(i,j,k)=cv_amassi(i,j,k)+sv_rank3(i,j,k)*ges_aero(i,j,k)/ges_amassi(i,j,k)
            end do
          end do
        end do
      elseif (imodes_cmaq_fv3(ic)==2) then
        do k=1,nsig
          do j=1,lon2
            do i=1,lat2
              cv_amassj(i,j,k)=cv_amassj(i,j,k)+sv_rank3(i,j,k)*ges_aero(i,j,k)/ges_amassj(i,j,k)
            end do
          end do
        end do
      else
        do k=1,nsig
          do j=1,lon2
            do i=1,lat2
              cv_amassk(i,j,k)=cv_amassk(i,j,k)+sv_rank3(i,j,k)*ges_aero(i,j,k)/ges_amassk(i,j,k)
            end do
          end do
        end do
      end if
   end do

return
end subroutine amass2aero_ad


end module amassaeromod

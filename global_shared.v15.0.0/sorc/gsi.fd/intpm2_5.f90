module intpm2_5mod

!$$$ module documentation block
!           .      .    .                                       .
! module:   intpm2_5mod    module for intq and its tangent linear intq_tl
!   prgmmr:
!
! abstract: module for intq and its tangent linear intq_tl
!
! program history log:
!   2010-10-15  pagowski - use for in-situ pm2_5
!
! subroutines included:
!   sub intpm2_5_
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none
  
  private
  public intpm2_5
  
  interface intpm2_5; module procedure &
       intpm2_5_
  end interface
  
contains
  
  subroutine intpm2_5_(pm2_5head,rval,sval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intpm2_5        apply nonlin qc obs operator for q 
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: apply observation operator and adjoint for q with
!             nonlinear qc operator
!
! program history log:
!   2010-10-15  pagowski  - convert for in-situ pm2_5
!   2015-12-03  pagowski  - modify to use bundle
!
!   input argument list:
!     pm2_5head    - obs type pointer to obs structure
!     spm2_5       - pm2_5 increment in grid space
!     rpm2_5
!
!   output argument list:
!     rpm2_5       - results from pm2_5 observation operator 
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use kinds, only: r_kind,i_kind
    use constants, only: half,one,tiny_r_kind,cg_term,max_varname_length
    use obsmod, only: pm2_5_ob_type,lsaveobsens,l_do_adjoint,luse_obsdiag
    use qcmod, only: nlnqc_iter,varqc_iter
    use jfunc, only: jiter
    use gsi_bundlemod, only: gsi_bundle
    use gsi_bundlemod, only: gsi_bundlegetpointer
    use gsi_4dvar, only: ladtest_obs
    use gridmod, only: cmaq_regional,wrf_mass_regional
    use chemmod, only: s_2_5,d_2_5,nh4_mfac,oc_mfac,laeroana_gocart


    implicit none
    
! declare passed variables
    type(pm2_5_ob_type),pointer,intent(in   ) :: pm2_5head
    type(gsi_bundle)       ,intent(in   ) :: sval
    type(gsi_bundle)       ,intent(inout) :: rval
    
! declare local variables  
    integer(i_kind) j1,j2,j3,j4,j5,j6,j7,j8,ier,istatus
    real(r_kind) w1,w2,w3,w4,w5,w6,w7,w8
! real(r_kind) penalty
    real(r_kind) cg_pm2_5,val,p0,grad,wnotgross,wgross,pm2_5_pg
    real(r_kind),pointer,dimension(:) :: spm2_5
    real(r_kind),pointer,dimension(:) :: rpm2_5
    type(pm2_5_ob_type), pointer :: pm2_5ptr

    character(len=max_varname_length) :: aeroname
    
!   If no pm2_5 obs return
    if(.not. associated(pm2_5head))return
! retrieve pointers
! simply return if any pointer not found
    ier=0

    if (cmaq_regional .or. (wrf_mass_regional .and. .not. laeroana_gocart)) then

       call gsi_bundlegetpointer(sval,'pm2_5',spm2_5,istatus);ier=istatus+ier
       call gsi_bundlegetpointer(rval,'pm2_5',rpm2_5,istatus);ier=istatus+ier

       if(ier/=0) return

       pm2_5ptr => pm2_5head
       do while (associated(pm2_5ptr))
          j1=pm2_5ptr%ij(1)
          j2=pm2_5ptr%ij(2)
          j3=pm2_5ptr%ij(3)
          j4=pm2_5ptr%ij(4)
          j5=pm2_5ptr%ij(5)
          j6=pm2_5ptr%ij(6)
          j7=pm2_5ptr%ij(7)
          j8=pm2_5ptr%ij(8)
          w1=pm2_5ptr%wij(1)
          w2=pm2_5ptr%wij(2)
          w3=pm2_5ptr%wij(3)
          w4=pm2_5ptr%wij(4)
          w5=pm2_5ptr%wij(5)
          w6=pm2_5ptr%wij(6)
          w7=pm2_5ptr%wij(7)
          w8=pm2_5ptr%wij(8)

!    forward model
          val=w1* spm2_5(j1)+w2* spm2_5(j2)+w3* spm2_5(j3)+w4* spm2_5(j4)+ &
               w5* spm2_5(j5)+w6* spm2_5(j6)+w7* spm2_5(j7)+w8* spm2_5(j8)

          if(luse_obsdiag)then
             if (lsaveobsens) then
                pm2_5ptr%diags%obssen(jiter) = val*pm2_5ptr%raterr2*pm2_5ptr%err2
             else
                if (pm2_5ptr%luse) pm2_5ptr%diags%tldepart(jiter)=val
             endif
          endif

          if (l_do_adjoint) then
             if (lsaveobsens) then
                grad = pm2_5ptr%diags%obssen(jiter)

             else
                if( .not. ladtest_obs ) val=val-pm2_5ptr%res

!          gradient of nonlinear operator

                if (nlnqc_iter .and. pm2_5ptr%pg > tiny_r_kind .and.  &
                     pm2_5ptr%b  > tiny_r_kind) then
                   pm2_5_pg=pm2_5ptr%pg*varqc_iter
                   cg_pm2_5=cg_term/pm2_5ptr%b
                   wnotgross= one-pm2_5_pg
                   wgross =pm2_5_pg*cg_pm2_5/wnotgross              ! wgross is gama in the reference by enderson
                   p0=wgross/(wgross+exp(-half*pm2_5ptr%err2*val**2))  ! p0 is p in the reference by enderson
                   val=val*(one-p0)                         ! term is wqc in the referenc by enderson
                endif

                if( ladtest_obs ) then
                   grad = val
                else
                   grad     = val*pm2_5ptr%raterr2*pm2_5ptr%err2
                end if
             endif

!       adjoint
             rpm2_5(j1)=rpm2_5(j1)+w1*grad
             rpm2_5(j2)=rpm2_5(j2)+w2*grad
             rpm2_5(j3)=rpm2_5(j3)+w3*grad
             rpm2_5(j4)=rpm2_5(j4)+w4*grad
             rpm2_5(j5)=rpm2_5(j5)+w5*grad
             rpm2_5(j6)=rpm2_5(j6)+w6*grad
             rpm2_5(j7)=rpm2_5(j7)+w7*grad
             rpm2_5(j8)=rpm2_5(j8)+w8*grad

          endif

          pm2_5ptr => pm2_5ptr%llpoint

       end do

    endif

    if (wrf_mass_regional .and. laeroana_gocart) then

       pm2_5ptr => pm2_5head
       do while (associated(pm2_5ptr))
          j1=pm2_5ptr%ij(1)
          j2=pm2_5ptr%ij(2)
          j3=pm2_5ptr%ij(3)
          j4=pm2_5ptr%ij(4)
          j5=pm2_5ptr%ij(5)
          j6=pm2_5ptr%ij(6)
          j7=pm2_5ptr%ij(7)
          j8=pm2_5ptr%ij(8)
          w1=pm2_5ptr%wij(1)
          w2=pm2_5ptr%wij(2)
          w3=pm2_5ptr%wij(3)
          w4=pm2_5ptr%wij(4)
          w5=pm2_5ptr%wij(5)
          w6=pm2_5ptr%wij(6)
          w7=pm2_5ptr%wij(7)
          w8=pm2_5ptr%wij(8)

          aeroname='bc1'
          call gsi_bundlegetpointer(sval,trim(aeroname),spm2_5,istatus)
          if(istatus /= 0) then
             write(6,*) 'error gsi_bundlegetpointer in intpm2_5 for ',&
                  aeroname
             call stop2(454)
          endif

          val= w1* spm2_5(j1)+w2* spm2_5(j2)+ &
               w3* spm2_5(j3)+w4* spm2_5(j4)+ &
               w5* spm2_5(j5)+w6* spm2_5(j6)+ &
               w7* spm2_5(j7)+w8* spm2_5(j8)

          nullify(spm2_5)
          
          aeroname='bc2'
          call gsi_bundlegetpointer(sval,trim(aeroname),spm2_5,istatus)
          if(istatus /= 0) then
             write(6,*) 'error gsi_bundlegetpointer in intpm2_5 for ',&
                  aeroname
             call stop2(454)
          endif

          val= w1* spm2_5(j1)+w2* spm2_5(j2)+ &
               w3* spm2_5(j3)+w4* spm2_5(j4)+ &
               w5* spm2_5(j5)+w6* spm2_5(j6)+ &
               w7* spm2_5(j7)+w8* spm2_5(j8)+ &
               val
          
          nullify(spm2_5)

          aeroname='sulf'
          call gsi_bundlegetpointer(sval,trim(aeroname),spm2_5,istatus)
          if(istatus /= 0) then
             write(6,*) 'error gsi_bundlegetpointer in intpm2_5 for ',&
                  aeroname
             call stop2(454)
          endif

          val=(w1* spm2_5(j1)+w2* spm2_5(j2)+ &
               w3* spm2_5(j3)+w4* spm2_5(j4)+ &
               w5* spm2_5(j5)+w6* spm2_5(j6)+ &
               w7* spm2_5(j7)+w8* spm2_5(j8)) * nh4_mfac + &
               val
          
          nullify(spm2_5)

          aeroname='p25'
          call gsi_bundlegetpointer(sval,trim(aeroname),spm2_5,istatus)
          if(istatus /= 0) then
             write(6,*) 'error gsi_bundlegetpointer in intpm2_5 for ',&
                  aeroname
             call stop2(454)
          endif
          
          val= w1* spm2_5(j1)+w2* spm2_5(j2)+ &
               w3* spm2_5(j3)+w4* spm2_5(j4)+ &
               w5* spm2_5(j5)+w6* spm2_5(j6)+ &
               w7* spm2_5(j7)+w8* spm2_5(j8)+ &
               val
          
          nullify(spm2_5)

          aeroname='oc1'
          call gsi_bundlegetpointer(sval,trim(aeroname),spm2_5,istatus)
          if(istatus /= 0) then
             write(6,*) 'error gsi_bundlegetpointer in intpm2_5 for ',&
                  aeroname
             call stop2(454)
          endif

          val=(w1* spm2_5(j1)+w2* spm2_5(j2)+ &
               w3* spm2_5(j3)+w4* spm2_5(j4)+ &
               w5* spm2_5(j5)+w6* spm2_5(j6)+ &
               w7* spm2_5(j7)+w8* spm2_5(j8)) *oc_mfac+ &
               val

          nullify(spm2_5)

          aeroname='oc2'
          call gsi_bundlegetpointer(sval,trim(aeroname),spm2_5,istatus)
          if(istatus /= 0) then
             write(6,*) 'error gsi_bundlegetpointer in intpm2_5 for ',&
                  aeroname
             call stop2(454)
          endif

          val=(w1* spm2_5(j1)+w2* spm2_5(j2)+ &
               w3* spm2_5(j3)+w4* spm2_5(j4)+ &
               w5* spm2_5(j5)+w6* spm2_5(j6)+ &
               w7* spm2_5(j7)+w8* spm2_5(j8)) *oc_mfac+ &
               val
          
          nullify(spm2_5)

          aeroname='seas1'
          call gsi_bundlegetpointer(sval,trim(aeroname),spm2_5,istatus)
          if(istatus /= 0) then
             write(6,*) 'error gsi_bundlegetpointer in intpm2_5 for ',&
                  aeroname
             call stop2(454)
          endif
          
          val= w1* spm2_5(j1)+w2* spm2_5(j2)+ &
               w3* spm2_5(j3)+w4* spm2_5(j4)+ &
               w5* spm2_5(j5)+w6* spm2_5(j6)+ &
               w7* spm2_5(j7)+w8* spm2_5(j8)+ &
               val
          
          nullify(spm2_5)

          aeroname='seas2'
          call gsi_bundlegetpointer(sval,trim(aeroname),spm2_5,istatus)
          if(istatus /= 0) then
             write(6,*) 'error gsi_bundlegetpointer in intpm2_5 for ',&
                  aeroname
             call stop2(454)
          endif
          
          val=(w1* spm2_5(j1)+w2* spm2_5(j2)+ &
               w3* spm2_5(j3)+w4* spm2_5(j4)+ &
               w5* spm2_5(j5)+w6* spm2_5(j6)+ &
               w7* spm2_5(j7)+w8* spm2_5(j8)) * s_2_5 + &
               val

          nullify(spm2_5)

          aeroname='dust1'
          call gsi_bundlegetpointer(sval,trim(aeroname),spm2_5,istatus)
          if(istatus /= 0) then
             write(6,*) 'error gsi_bundlegetpointer in intpm2_5 for ',&
                  aeroname
             call stop2(454)
          endif

          val= w1* spm2_5(j1)+w2* spm2_5(j2)+ &
               w3* spm2_5(j3)+w4* spm2_5(j4)+ &
               w5* spm2_5(j5)+w6* spm2_5(j6)+ &
               w7* spm2_5(j7)+w8* spm2_5(j8)+ &
               val
          
          nullify(spm2_5)

          aeroname='dust2'
          call gsi_bundlegetpointer(sval,trim(aeroname),spm2_5,istatus)
          if(istatus /= 0) then
             write(6,*) 'error gsi_bundlegetpointer in intpm2_5 for ',&
                  aeroname
             call stop2(454)
          endif

          val=(w1* spm2_5(j1)+w2* spm2_5(j2)+ &
               w3* spm2_5(j3)+w4* spm2_5(j4)+ &
               w5* spm2_5(j5)+w6* spm2_5(j6)+ &
               w7* spm2_5(j7)+w8* spm2_5(j8)) * d_2_5 + &
               val
          
          nullify(spm2_5)

          if(luse_obsdiag)then
             if (lsaveobsens) then
                pm2_5ptr%diags%obssen(jiter) = val*pm2_5ptr%raterr2*pm2_5ptr%err2
             else
                if (pm2_5ptr%luse) pm2_5ptr%diags%tldepart(jiter)=val
             endif
          endif

          if (l_do_adjoint) then
             if (lsaveobsens) then
                grad = pm2_5ptr%diags%obssen(jiter)

             else
                if( .not. ladtest_obs ) val=val-pm2_5ptr%res

!          gradient of nonlinear operator

                if (nlnqc_iter .and. pm2_5ptr%pg > tiny_r_kind .and.  &
                     pm2_5ptr%b  > tiny_r_kind) then
                   pm2_5_pg=pm2_5ptr%pg*varqc_iter
                   cg_pm2_5=cg_term/pm2_5ptr%b
                   wnotgross= one-pm2_5_pg
                   wgross =pm2_5_pg*cg_pm2_5/wnotgross              ! wgross is gama in the reference by enderson
                   p0=wgross/(wgross+exp(-half*pm2_5ptr%err2*val**2))  ! p0 is p in the reference by enderson
                   val=val*(one-p0)                         ! term is wqc in the referenc by enderson
                endif

                if( ladtest_obs ) then
                   grad = val
                else
                   grad     = val*pm2_5ptr%raterr2*pm2_5ptr%err2
                end if
             endif

!       adjoint

             aeroname='bc1'
             call gsi_bundlegetpointer(rval,trim(aeroname),rpm2_5,istatus)
             if(istatus /= 0) then
                write(6,*) 'error gsi_bundlegetpointer in intpm2_5 for ',&
                     aeroname
                call stop2(455)
             endif
             
             rpm2_5(j1)=rpm2_5(j1)+w1*grad
             rpm2_5(j2)=rpm2_5(j2)+w2*grad
             rpm2_5(j3)=rpm2_5(j3)+w3*grad
             rpm2_5(j4)=rpm2_5(j4)+w4*grad
             rpm2_5(j5)=rpm2_5(j5)+w5*grad
             rpm2_5(j6)=rpm2_5(j6)+w6*grad
             rpm2_5(j7)=rpm2_5(j7)+w7*grad
             rpm2_5(j8)=rpm2_5(j8)+w8*grad
             nullify(rpm2_5)

             aeroname='bc2'
             call gsi_bundlegetpointer(rval,trim(aeroname),rpm2_5,istatus)
             if(istatus /= 0) then
                write(6,*) 'error gsi_bundlegetpointer in intpm2_5 for ',&
                     aeroname
                call stop2(455)
             endif
             
             rpm2_5(j1)=rpm2_5(j1)+w1*grad
             rpm2_5(j2)=rpm2_5(j2)+w2*grad
             rpm2_5(j3)=rpm2_5(j3)+w3*grad
             rpm2_5(j4)=rpm2_5(j4)+w4*grad
             rpm2_5(j5)=rpm2_5(j5)+w5*grad
             rpm2_5(j6)=rpm2_5(j6)+w6*grad
             rpm2_5(j7)=rpm2_5(j7)+w7*grad
             rpm2_5(j8)=rpm2_5(j8)+w8*grad
             nullify(rpm2_5)

             aeroname='sulf'
             call gsi_bundlegetpointer(rval,trim(aeroname),rpm2_5,istatus)
             if(istatus /= 0) then
                write(6,*) 'error gsi_bundlegetpointer in intpm2_5 for ',&
                     aeroname
                call stop2(455)
             endif
             
             rpm2_5(j1)=rpm2_5(j1)+w1*grad
             rpm2_5(j2)=rpm2_5(j2)+w2*grad
             rpm2_5(j3)=rpm2_5(j3)+w3*grad
             rpm2_5(j4)=rpm2_5(j4)+w4*grad
             rpm2_5(j5)=rpm2_5(j5)+w5*grad
             rpm2_5(j6)=rpm2_5(j6)+w6*grad
             rpm2_5(j7)=rpm2_5(j7)+w7*grad
             rpm2_5(j8)=rpm2_5(j8)+w8*grad
             nullify(rpm2_5)

             aeroname='p25'
             call gsi_bundlegetpointer(rval,trim(aeroname),rpm2_5,istatus)
             if(istatus /= 0) then
                write(6,*) 'error gsi_bundlegetpointer in intpm2_5 for ',&
                     aeroname
                call stop2(455)
             endif
             
             rpm2_5(j1)=rpm2_5(j1)+w1*grad
             rpm2_5(j2)=rpm2_5(j2)+w2*grad
             rpm2_5(j3)=rpm2_5(j3)+w3*grad
             rpm2_5(j4)=rpm2_5(j4)+w4*grad
             rpm2_5(j5)=rpm2_5(j5)+w5*grad
             rpm2_5(j6)=rpm2_5(j6)+w6*grad
             rpm2_5(j7)=rpm2_5(j7)+w7*grad
             rpm2_5(j8)=rpm2_5(j8)+w8*grad
             nullify(rpm2_5)

             aeroname='oc1'
             call gsi_bundlegetpointer(rval,trim(aeroname),rpm2_5,istatus)
             if(istatus /= 0) then
                write(6,*) 'error gsi_bundlegetpointer in intpm2_5 for ',&
                     aeroname
                call stop2(455)
             endif
             
             rpm2_5(j1)=rpm2_5(j1)+w1*grad
             rpm2_5(j2)=rpm2_5(j2)+w2*grad
             rpm2_5(j3)=rpm2_5(j3)+w3*grad
             rpm2_5(j4)=rpm2_5(j4)+w4*grad
             rpm2_5(j5)=rpm2_5(j5)+w5*grad
             rpm2_5(j6)=rpm2_5(j6)+w6*grad
             rpm2_5(j7)=rpm2_5(j7)+w7*grad
             rpm2_5(j8)=rpm2_5(j8)+w8*grad
             nullify(rpm2_5)

             aeroname='oc2'
             call gsi_bundlegetpointer(rval,trim(aeroname),rpm2_5,istatus)
             if(istatus /= 0) then
                write(6,*) 'error gsi_bundlegetpointer in intpm2_5 for ',&
                     aeroname
                call stop2(455)
             endif
             
             rpm2_5(j1)=rpm2_5(j1)+w1*grad
             rpm2_5(j2)=rpm2_5(j2)+w2*grad
             rpm2_5(j3)=rpm2_5(j3)+w3*grad
             rpm2_5(j4)=rpm2_5(j4)+w4*grad
             rpm2_5(j5)=rpm2_5(j5)+w5*grad
             rpm2_5(j6)=rpm2_5(j6)+w6*grad
             rpm2_5(j7)=rpm2_5(j7)+w7*grad
             rpm2_5(j8)=rpm2_5(j8)+w8*grad
             nullify(rpm2_5)


             aeroname='seas1'
             call gsi_bundlegetpointer(rval,trim(aeroname),rpm2_5,istatus)
             if(istatus /= 0) then
                write(6,*) 'error gsi_bundlegetpointer in intpm2_5 for ',&
                     aeroname
                call stop2(455)
             endif
             
             rpm2_5(j1)=rpm2_5(j1)+w1*grad
             rpm2_5(j2)=rpm2_5(j2)+w2*grad
             rpm2_5(j3)=rpm2_5(j3)+w3*grad
             rpm2_5(j4)=rpm2_5(j4)+w4*grad
             rpm2_5(j5)=rpm2_5(j5)+w5*grad
             rpm2_5(j6)=rpm2_5(j6)+w6*grad
             rpm2_5(j7)=rpm2_5(j7)+w7*grad
             rpm2_5(j8)=rpm2_5(j8)+w8*grad
             nullify(rpm2_5)

             aeroname='seas2'
             call gsi_bundlegetpointer(rval,trim(aeroname),rpm2_5,istatus)
             if(istatus /= 0) then
                write(6,*) 'error gsi_bundlegetpointer in intpm2_5 for ',&
                     aeroname
                call stop2(455)
             endif
             
             rpm2_5(j1)=rpm2_5(j1)+w1*grad
             rpm2_5(j2)=rpm2_5(j2)+w2*grad
             rpm2_5(j3)=rpm2_5(j3)+w3*grad
             rpm2_5(j4)=rpm2_5(j4)+w4*grad
             rpm2_5(j5)=rpm2_5(j5)+w5*grad
             rpm2_5(j6)=rpm2_5(j6)+w6*grad
             rpm2_5(j7)=rpm2_5(j7)+w7*grad
             rpm2_5(j8)=rpm2_5(j8)+w8*grad
             nullify(rpm2_5)

             aeroname='dust1'
             call gsi_bundlegetpointer(rval,trim(aeroname),rpm2_5,istatus)
             if(istatus /= 0) then
                write(6,*) 'error gsi_bundlegetpointer in intpm2_5 for ',&
                     aeroname
                call stop2(455)
             endif
             
             rpm2_5(j1)=rpm2_5(j1)+w1*grad
             rpm2_5(j2)=rpm2_5(j2)+w2*grad
             rpm2_5(j3)=rpm2_5(j3)+w3*grad
             rpm2_5(j4)=rpm2_5(j4)+w4*grad
             rpm2_5(j5)=rpm2_5(j5)+w5*grad
             rpm2_5(j6)=rpm2_5(j6)+w6*grad
             rpm2_5(j7)=rpm2_5(j7)+w7*grad
             rpm2_5(j8)=rpm2_5(j8)+w8*grad
             nullify(rpm2_5)

             aeroname='dust2'
             call gsi_bundlegetpointer(rval,trim(aeroname),rpm2_5,istatus)
             if(istatus /= 0) then
                write(6,*) 'error gsi_bundlegetpointer in intpm2_5 for ',&
                     aeroname
                call stop2(455)
             endif
             
             rpm2_5(j1)=rpm2_5(j1)+w1*grad
             rpm2_5(j2)=rpm2_5(j2)+w2*grad
             rpm2_5(j3)=rpm2_5(j3)+w3*grad
             rpm2_5(j4)=rpm2_5(j4)+w4*grad
             rpm2_5(j5)=rpm2_5(j5)+w5*grad
             rpm2_5(j6)=rpm2_5(j6)+w6*grad
             rpm2_5(j7)=rpm2_5(j7)+w7*grad
             rpm2_5(j8)=rpm2_5(j8)+w8*grad
             nullify(rpm2_5)

          endif

          pm2_5ptr => pm2_5ptr%llpoint

       end do

    endif

    return
  end subroutine intpm2_5_
  
end module intpm2_5mod

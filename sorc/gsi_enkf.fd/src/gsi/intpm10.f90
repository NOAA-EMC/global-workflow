module intpm10mod

!$$$ module documentation block
!           .      .    .                                       .
! module:   intpm10mod    module for intpm10 and its tangent linear intpm10_tl
!   prgmmr:
!
! abstract: module for intpm10 and its tangent linear intpm10_tl
!
! program history log:
!   2016-02-20  pagowski - based on intpm2_5 but for pm10 
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!
! subroutines included:
!   sub intpm10_
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use m_obsNode , only: obsNode
  use m_pm10Node, only: pm10Node
  use m_pm10Node, only: pm10Node_typecast
  use m_pm10Node, only: pm10Node_nextcast
  use m_obsdiagNode, only: obsdiagNode_get
  use m_obsdiagNode, only: obsdiagNode_set
  implicit none
  
  private
  public intpm10
  
  interface intpm10; module procedure &
       intpm10_
  end interface
  
contains
  
  subroutine intpm10_(pm10head,rval,sval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intpm10        apply nonlin qc obs operator for q 
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: apply observation operator and adjoint for q with
!             nonlinear qc operator
!
! program history log:
! 2016-02-20  pagowski - based on intpm2_5 but for pm10         
!
!   input argument list:
!     pm10head    - obs type pointer to obs structure
!     spm10       - pm10 increment in grid space
!     rpm10
!
!   output argument list:
!     rpm10       - results from pm10 observation operator 
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use kinds, only: r_kind,i_kind
    use constants, only: half,one,tiny_r_kind,cg_term,max_varname_length
    use obsmod, only: lsaveobsens,l_do_adjoint,luse_obsdiag
    use qcmod, only: nlnqc_iter,varqc_iter
    use jfunc, only: jiter
    use gsi_bundlemod, only: gsi_bundle
    use gsi_bundlemod, only: gsi_bundlegetpointer
    use gsi_4dvar, only: ladtest_obs
    use gridmod, only: cmaq_regional,wrf_mass_regional
    use chemmod, only: d_10,nh4_mfac,oc_mfac


    implicit none
    
! declare passed variables
    class(obsNode), pointer,intent(in   ) :: pm10head
    type(gsi_bundle)       ,intent(in   ) :: sval
    type(gsi_bundle)       ,intent(inout) :: rval
    
! declare local variables  
    integer(i_kind) j1,j2,j3,j4,j5,j6,j7,j8,ier,istatus
    real(r_kind) w1,w2,w3,w4,w5,w6,w7,w8
! real(r_kind) penalty
    real(r_kind) cg_pm10,val,p0,grad,wnotgross,wgross,pm10_pg
    real(r_kind),pointer,dimension(:) :: spm10
    real(r_kind),pointer,dimension(:) :: rpm10
    type(pm10Node), pointer :: pm10ptr

    character(len=max_varname_length) :: aeroname
    
!   If no pm10 obs return
    if(.not. associated(pm10head))return
! retrieve pointers
! simply return if any pointer not found
    ier=0

    if (cmaq_regional) then

       write(6,*)'pm10 assimilation for cmaq not implemented'
       call stop2(455)

    endif

    if (wrf_mass_regional) then

       !pm10ptr => pm10head
       pm10ptr => pm10Node_typecast(pm10head)
       do while (associated(pm10ptr))
          j1=pm10ptr%ij(1)
          j2=pm10ptr%ij(2)
          j3=pm10ptr%ij(3)
          j4=pm10ptr%ij(4)
          j5=pm10ptr%ij(5)
          j6=pm10ptr%ij(6)
          j7=pm10ptr%ij(7)
          j8=pm10ptr%ij(8)
          w1=pm10ptr%wij(1)
          w2=pm10ptr%wij(2)
          w3=pm10ptr%wij(3)
          w4=pm10ptr%wij(4)
          w5=pm10ptr%wij(5)
          w6=pm10ptr%wij(6)
          w7=pm10ptr%wij(7)
          w8=pm10ptr%wij(8)

          aeroname='bc1'
          call gsi_bundlegetpointer(sval,trim(aeroname),spm10,istatus)
          if(istatus /= 0) then
             write(6,*) 'error gsi_bundlegetpointer in intpm10 for ',&
                  aeroname
             call stop2(454)
          endif

          val= w1* spm10(j1)+w2* spm10(j2)+ &
               w3* spm10(j3)+w4* spm10(j4)+ &
               w5* spm10(j5)+w6* spm10(j6)+ &
               w7* spm10(j7)+w8* spm10(j8)

          nullify(spm10)
          
          aeroname='bc2'
          call gsi_bundlegetpointer(sval,trim(aeroname),spm10,istatus)
          if(istatus /= 0) then
             write(6,*) 'error gsi_bundlegetpointer in intpm10 for ',&
                  aeroname
             call stop2(454)
          endif

          val= w1* spm10(j1)+w2* spm10(j2)+ &
               w3* spm10(j3)+w4* spm10(j4)+ &
               w5* spm10(j5)+w6* spm10(j6)+ &
               w7* spm10(j7)+w8* spm10(j8)+ &
               val
          
          nullify(spm10)

          aeroname='sulf'
          call gsi_bundlegetpointer(sval,trim(aeroname),spm10,istatus)
          if(istatus /= 0) then
             write(6,*) 'error gsi_bundlegetpointer in intpm10 for ',&
                  aeroname
             call stop2(454)
          endif

          val=(w1* spm10(j1)+w2* spm10(j2)+ &
               w3* spm10(j3)+w4* spm10(j4)+ &
               w5* spm10(j5)+w6* spm10(j6)+ &
               w7* spm10(j7)+w8* spm10(j8)) * nh4_mfac + &
               val
          
          nullify(spm10)

          aeroname='p25'
          call gsi_bundlegetpointer(sval,trim(aeroname),spm10,istatus)
          if(istatus /= 0) then
             write(6,*) 'error gsi_bundlegetpointer in intpm10 for ',&
                  aeroname
             call stop2(454)
          endif
          
          val= w1* spm10(j1)+w2* spm10(j2)+ &
               w3* spm10(j3)+w4* spm10(j4)+ &
               w5* spm10(j5)+w6* spm10(j6)+ &
               w7* spm10(j7)+w8* spm10(j8)+ &
               val
          
          nullify(spm10)

          aeroname='oc1'
          call gsi_bundlegetpointer(sval,trim(aeroname),spm10,istatus)
          if(istatus /= 0) then
             write(6,*) 'error gsi_bundlegetpointer in intpm10 for ',&
                  aeroname
             call stop2(454)
          endif

          val=(w1* spm10(j1)+w2* spm10(j2)+ &
               w3* spm10(j3)+w4* spm10(j4)+ &
               w5* spm10(j5)+w6* spm10(j6)+ &
               w7* spm10(j7)+w8* spm10(j8)) *oc_mfac+ &
               val

          nullify(spm10)

          aeroname='oc2'
          call gsi_bundlegetpointer(sval,trim(aeroname),spm10,istatus)
          if(istatus /= 0) then
             write(6,*) 'error gsi_bundlegetpointer in intpm10 for ',&
                  aeroname
             call stop2(454)
          endif

          val=(w1* spm10(j1)+w2* spm10(j2)+ &
               w3* spm10(j3)+w4* spm10(j4)+ &
               w5* spm10(j5)+w6* spm10(j6)+ &
               w7* spm10(j7)+w8* spm10(j8)) *oc_mfac+ &
               val
          
          nullify(spm10)

          aeroname='seas1'
          call gsi_bundlegetpointer(sval,trim(aeroname),spm10,istatus)
          if(istatus /= 0) then
             write(6,*) 'error gsi_bundlegetpointer in intpm10 for ',&
                  aeroname
             call stop2(454)
          endif
          
          val= w1* spm10(j1)+w2* spm10(j2)+ &
               w3* spm10(j3)+w4* spm10(j4)+ &
               w5* spm10(j5)+w6* spm10(j6)+ &
               w7* spm10(j7)+w8* spm10(j8)+ &
               val
          
          nullify(spm10)

          aeroname='seas2'
          call gsi_bundlegetpointer(sval,trim(aeroname),spm10,istatus)
          if(istatus /= 0) then
             write(6,*) 'error gsi_bundlegetpointer in intpm10 for ',&
                  aeroname
             call stop2(454)
          endif
          
          val=(w1* spm10(j1)+w2* spm10(j2)+ &
               w3* spm10(j3)+w4* spm10(j4)+ &
               w5* spm10(j5)+w6* spm10(j6)+ &
               w7* spm10(j7)+w8* spm10(j8)) + &
               val

          nullify(spm10)

          aeroname='seas3'
          call gsi_bundlegetpointer(sval,trim(aeroname),spm10,istatus)
          if(istatus /= 0) then
             write(6,*) 'error gsi_bundlegetpointer in intpm10 for ',&
                  aeroname
             call stop2(454)
          endif
          
          val=(w1* spm10(j1)+w2* spm10(j2)+ &
               w3* spm10(j3)+w4* spm10(j4)+ &
               w5* spm10(j5)+w6* spm10(j6)+ &
               w7* spm10(j7)+w8* spm10(j8)) + &
               val

          nullify(spm10)

          aeroname='dust1'
          call gsi_bundlegetpointer(sval,trim(aeroname),spm10,istatus)
          if(istatus /= 0) then
             write(6,*) 'error gsi_bundlegetpointer in intpm10 for ',&
                  aeroname
             call stop2(454)
          endif

          val= w1* spm10(j1)+w2* spm10(j2)+ &
               w3* spm10(j3)+w4* spm10(j4)+ &
               w5* spm10(j5)+w6* spm10(j6)+ &
               w7* spm10(j7)+w8* spm10(j8)+ &
               val
          
          nullify(spm10)

          aeroname='dust2'
          call gsi_bundlegetpointer(sval,trim(aeroname),spm10,istatus)
          if(istatus /= 0) then
             write(6,*) 'error gsi_bundlegetpointer in intpm10 for ',&
                  aeroname
             call stop2(454)
          endif

          val=(w1* spm10(j1)+w2* spm10(j2)+ &
               w3* spm10(j3)+w4* spm10(j4)+ &
               w5* spm10(j5)+w6* spm10(j6)+ &
               w7* spm10(j7)+w8* spm10(j8)) + &
               val
          
          nullify(spm10)


          aeroname='dust3'
          call gsi_bundlegetpointer(sval,trim(aeroname),spm10,istatus)
          if(istatus /= 0) then
             write(6,*) 'error gsi_bundlegetpointer in intpm10 for ',&
                  aeroname
             call stop2(454)
          endif

          val=(w1* spm10(j1)+w2* spm10(j2)+ &
               w3* spm10(j3)+w4* spm10(j4)+ &
               w5* spm10(j5)+w6* spm10(j6)+ &
               w7* spm10(j7)+w8* spm10(j8)) + &
               val
          
          nullify(spm10)


          aeroname='dust4'
          call gsi_bundlegetpointer(sval,trim(aeroname),spm10,istatus)
          if(istatus /= 0) then
             write(6,*) 'error gsi_bundlegetpointer in intpm10 for ',&
                  aeroname
             call stop2(454)
          endif

          val=(w1* spm10(j1)+w2* spm10(j2)+ &
               w3* spm10(j3)+w4* spm10(j4)+ &
               w5* spm10(j5)+w6* spm10(j6)+ &
               w7* spm10(j7)+w8* spm10(j8)) * d_10 + &
               val
          
          nullify(spm10)

          if(luse_obsdiag)then
             if (lsaveobsens) then
                !-- pm10ptr%diags%obssen(jiter) = val*pm10ptr%raterr2*pm10ptr%err2
                call obsdiagNode_set(pm10ptr%diags,jiter=jiter,obssen=val*pm10ptr%raterr2*pm10ptr%err2)
             else
                !-- if (pm10ptr%luse) pm10ptr%diags%tldepart(jiter)=val
                if (pm10ptr%luse) call obsdiagNode_set(pm10ptr%diags,jiter=jiter,tldepart=val)
             endif
          endif

          if (l_do_adjoint) then
             if (lsaveobsens) then
                !-- grad = pm10ptr%diags%obssen(jiter)
                call obsdiagNode_get(pm10ptr%diags,jiter=jiter,obssen=grad)

             else
                if( .not. ladtest_obs ) val=val-pm10ptr%res

!          gradient of nonlinear operator

                if (nlnqc_iter .and. pm10ptr%pg > tiny_r_kind .and.  &
                     pm10ptr%b  > tiny_r_kind) then
                   pm10_pg=pm10ptr%pg*varqc_iter
                   cg_pm10=cg_term/pm10ptr%b
                   wnotgross= one-pm10_pg
                   wgross =pm10_pg*cg_pm10/wnotgross              ! wgross is gama in the reference by enderson
                   p0=wgross/(wgross+exp(-half*pm10ptr%err2*val**2))  ! p0 is p in the reference by enderson
                   val=val*(one-p0)                         ! term is wqc in the referenc by enderson
                endif

                if( ladtest_obs ) then
                   grad = val
                else
                   grad     = val*pm10ptr%raterr2*pm10ptr%err2
                end if
             endif

!       adjoint

             aeroname='bc1'
             call gsi_bundlegetpointer(rval,trim(aeroname),rpm10,istatus)
             if(istatus /= 0) then
                write(6,*) 'error gsi_bundlegetpointer in intpm10 for ',&
                     aeroname
                call stop2(455)
             endif
             
             rpm10(j1)=rpm10(j1)+w1*grad
             rpm10(j2)=rpm10(j2)+w2*grad
             rpm10(j3)=rpm10(j3)+w3*grad
             rpm10(j4)=rpm10(j4)+w4*grad
             rpm10(j5)=rpm10(j5)+w5*grad
             rpm10(j6)=rpm10(j6)+w6*grad
             rpm10(j7)=rpm10(j7)+w7*grad
             rpm10(j8)=rpm10(j8)+w8*grad
             nullify(rpm10)

             aeroname='bc2'
             call gsi_bundlegetpointer(rval,trim(aeroname),rpm10,istatus)
             if(istatus /= 0) then
                write(6,*) 'error gsi_bundlegetpointer in intpm10 for ',&
                     aeroname
                call stop2(455)
             endif
             
             rpm10(j1)=rpm10(j1)+w1*grad
             rpm10(j2)=rpm10(j2)+w2*grad
             rpm10(j3)=rpm10(j3)+w3*grad
             rpm10(j4)=rpm10(j4)+w4*grad
             rpm10(j5)=rpm10(j5)+w5*grad
             rpm10(j6)=rpm10(j6)+w6*grad
             rpm10(j7)=rpm10(j7)+w7*grad
             rpm10(j8)=rpm10(j8)+w8*grad
             nullify(rpm10)

             aeroname='sulf'
             call gsi_bundlegetpointer(rval,trim(aeroname),rpm10,istatus)
             if(istatus /= 0) then
                write(6,*) 'error gsi_bundlegetpointer in intpm10 for ',&
                     aeroname
                call stop2(455)
             endif
             
             rpm10(j1)=rpm10(j1)+w1*grad
             rpm10(j2)=rpm10(j2)+w2*grad
             rpm10(j3)=rpm10(j3)+w3*grad
             rpm10(j4)=rpm10(j4)+w4*grad
             rpm10(j5)=rpm10(j5)+w5*grad
             rpm10(j6)=rpm10(j6)+w6*grad
             rpm10(j7)=rpm10(j7)+w7*grad
             rpm10(j8)=rpm10(j8)+w8*grad
             nullify(rpm10)

             aeroname='p25'
             call gsi_bundlegetpointer(rval,trim(aeroname),rpm10,istatus)
             if(istatus /= 0) then
                write(6,*) 'error gsi_bundlegetpointer in intpm10 for ',&
                     aeroname
                call stop2(455)
             endif
             
             rpm10(j1)=rpm10(j1)+w1*grad
             rpm10(j2)=rpm10(j2)+w2*grad
             rpm10(j3)=rpm10(j3)+w3*grad
             rpm10(j4)=rpm10(j4)+w4*grad
             rpm10(j5)=rpm10(j5)+w5*grad
             rpm10(j6)=rpm10(j6)+w6*grad
             rpm10(j7)=rpm10(j7)+w7*grad
             rpm10(j8)=rpm10(j8)+w8*grad
             nullify(rpm10)

             aeroname='oc1'
             call gsi_bundlegetpointer(rval,trim(aeroname),rpm10,istatus)
             if(istatus /= 0) then
                write(6,*) 'error gsi_bundlegetpointer in intpm10 for ',&
                     aeroname
                call stop2(455)
             endif
             
             rpm10(j1)=rpm10(j1)+w1*grad
             rpm10(j2)=rpm10(j2)+w2*grad
             rpm10(j3)=rpm10(j3)+w3*grad
             rpm10(j4)=rpm10(j4)+w4*grad
             rpm10(j5)=rpm10(j5)+w5*grad
             rpm10(j6)=rpm10(j6)+w6*grad
             rpm10(j7)=rpm10(j7)+w7*grad
             rpm10(j8)=rpm10(j8)+w8*grad
             nullify(rpm10)

             aeroname='oc2'
             call gsi_bundlegetpointer(rval,trim(aeroname),rpm10,istatus)
             if(istatus /= 0) then
                write(6,*) 'error gsi_bundlegetpointer in intpm10 for ',&
                     aeroname
                call stop2(455)
             endif
             
             rpm10(j1)=rpm10(j1)+w1*grad
             rpm10(j2)=rpm10(j2)+w2*grad
             rpm10(j3)=rpm10(j3)+w3*grad
             rpm10(j4)=rpm10(j4)+w4*grad
             rpm10(j5)=rpm10(j5)+w5*grad
             rpm10(j6)=rpm10(j6)+w6*grad
             rpm10(j7)=rpm10(j7)+w7*grad
             rpm10(j8)=rpm10(j8)+w8*grad
             nullify(rpm10)


             aeroname='seas1'
             call gsi_bundlegetpointer(rval,trim(aeroname),rpm10,istatus)
             if(istatus /= 0) then
                write(6,*) 'error gsi_bundlegetpointer in intpm10 for ',&
                     aeroname
                call stop2(455)
             endif
             
             rpm10(j1)=rpm10(j1)+w1*grad
             rpm10(j2)=rpm10(j2)+w2*grad
             rpm10(j3)=rpm10(j3)+w3*grad
             rpm10(j4)=rpm10(j4)+w4*grad
             rpm10(j5)=rpm10(j5)+w5*grad
             rpm10(j6)=rpm10(j6)+w6*grad
             rpm10(j7)=rpm10(j7)+w7*grad
             rpm10(j8)=rpm10(j8)+w8*grad
             nullify(rpm10)

             aeroname='seas2'
             call gsi_bundlegetpointer(rval,trim(aeroname),rpm10,istatus)
             if(istatus /= 0) then
                write(6,*) 'error gsi_bundlegetpointer in intpm10 for ',&
                     aeroname
                call stop2(455)
             endif
             
             rpm10(j1)=rpm10(j1)+w1*grad
             rpm10(j2)=rpm10(j2)+w2*grad
             rpm10(j3)=rpm10(j3)+w3*grad
             rpm10(j4)=rpm10(j4)+w4*grad
             rpm10(j5)=rpm10(j5)+w5*grad
             rpm10(j6)=rpm10(j6)+w6*grad
             rpm10(j7)=rpm10(j7)+w7*grad
             rpm10(j8)=rpm10(j8)+w8*grad
             nullify(rpm10)

             aeroname='seas3'
             call gsi_bundlegetpointer(rval,trim(aeroname),rpm10,istatus)
             if(istatus /= 0) then
                write(6,*) 'error gsi_bundlegetpointer in intpm10 for ',&
                     aeroname
                call stop2(455)
             endif
             
             rpm10(j1)=rpm10(j1)+w1*grad
             rpm10(j2)=rpm10(j2)+w2*grad
             rpm10(j3)=rpm10(j3)+w3*grad
             rpm10(j4)=rpm10(j4)+w4*grad
             rpm10(j5)=rpm10(j5)+w5*grad
             rpm10(j6)=rpm10(j6)+w6*grad
             rpm10(j7)=rpm10(j7)+w7*grad
             rpm10(j8)=rpm10(j8)+w8*grad
             nullify(rpm10)

             aeroname='dust1'
             call gsi_bundlegetpointer(rval,trim(aeroname),rpm10,istatus)
             if(istatus /= 0) then
                write(6,*) 'error gsi_bundlegetpointer in intpm10 for ',&
                     aeroname
                call stop2(455)
             endif
             
             rpm10(j1)=rpm10(j1)+w1*grad
             rpm10(j2)=rpm10(j2)+w2*grad
             rpm10(j3)=rpm10(j3)+w3*grad
             rpm10(j4)=rpm10(j4)+w4*grad
             rpm10(j5)=rpm10(j5)+w5*grad
             rpm10(j6)=rpm10(j6)+w6*grad
             rpm10(j7)=rpm10(j7)+w7*grad
             rpm10(j8)=rpm10(j8)+w8*grad
             nullify(rpm10)

             aeroname='dust2'
             call gsi_bundlegetpointer(rval,trim(aeroname),rpm10,istatus)
             if(istatus /= 0) then
                write(6,*) 'error gsi_bundlegetpointer in intpm10 for ',&
                     aeroname
                call stop2(455)
             endif
             
             rpm10(j1)=rpm10(j1)+w1*grad
             rpm10(j2)=rpm10(j2)+w2*grad
             rpm10(j3)=rpm10(j3)+w3*grad
             rpm10(j4)=rpm10(j4)+w4*grad
             rpm10(j5)=rpm10(j5)+w5*grad
             rpm10(j6)=rpm10(j6)+w6*grad
             rpm10(j7)=rpm10(j7)+w7*grad
             rpm10(j8)=rpm10(j8)+w8*grad
             nullify(rpm10)

             aeroname='dust3'
             call gsi_bundlegetpointer(rval,trim(aeroname),rpm10,istatus)
             if(istatus /= 0) then
                write(6,*) 'error gsi_bundlegetpointer in intpm10 for ',&
                     aeroname
                call stop2(455)
             endif
             
             rpm10(j1)=rpm10(j1)+w1*grad
             rpm10(j2)=rpm10(j2)+w2*grad
             rpm10(j3)=rpm10(j3)+w3*grad
             rpm10(j4)=rpm10(j4)+w4*grad
             rpm10(j5)=rpm10(j5)+w5*grad
             rpm10(j6)=rpm10(j6)+w6*grad
             rpm10(j7)=rpm10(j7)+w7*grad
             rpm10(j8)=rpm10(j8)+w8*grad
             nullify(rpm10)

             aeroname='dust4'
             call gsi_bundlegetpointer(rval,trim(aeroname),rpm10,istatus)
             if(istatus /= 0) then
                write(6,*) 'error gsi_bundlegetpointer in intpm10 for ',&
                     aeroname
                call stop2(455)
             endif
             
             rpm10(j1)=rpm10(j1)+w1*grad
             rpm10(j2)=rpm10(j2)+w2*grad
             rpm10(j3)=rpm10(j3)+w3*grad
             rpm10(j4)=rpm10(j4)+w4*grad
             rpm10(j5)=rpm10(j5)+w5*grad
             rpm10(j6)=rpm10(j6)+w6*grad
             rpm10(j7)=rpm10(j7)+w7*grad
             rpm10(j8)=rpm10(j8)+w8*grad
             nullify(rpm10)

          endif

          !pm10ptr => pm10ptr%llpoint
          pm10ptr => pm10Node_nextcast(pm10ptr)

       end do

    endif

    return
  end subroutine intpm10_
  
end module intpm10mod

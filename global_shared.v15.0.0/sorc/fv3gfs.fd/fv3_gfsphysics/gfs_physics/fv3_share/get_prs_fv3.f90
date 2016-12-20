module gfs_fv3_needs

   use machine,  only: kind_phys
   use physcons, only: con_fvirt

!--- public declarations
   public get_prs_fv3, get_phi_fv3

!--- local variables
   real(kind=kind_phys), parameter :: zero = 0.0_kind_phys
   real(kind=kind_phys), parameter :: half = 0.5_kind_phys

contains

   subroutine get_prs_fv3(ix, levs, ntrac, phii, prsi, tgrs, qgrs, del, del_gz)
     integer, intent(in) :: ix, levs, ntrac
     real(kind=kind_phys), dimension(ix,levs+1),     intent(in)    :: phii
     real(kind=kind_phys), dimension(ix,levs+1),     intent(in)    :: prsi
     real(kind=kind_phys), dimension(ix,levs),       intent(in)    :: tgrs
     real(kind=kind_phys), dimension(ix,levs,ntrac), intent(in)    :: qgrs
     real(kind=kind_phys), dimension(ix,levs),       intent(inout) :: del
     real(kind=kind_phys), dimension(ix,levs+1),     intent(inout) :: del_gz

! SJL: Adjust the geopotential height hydrostatically in a way consistent with FV3 discretization
! del_gz is a temp array recording the old info before (t,q) are adjusted
     do k=1,levs
       do i=1,ix
            del(i,k) = prsi(i,k) - prsi(i,k+1)
         del_gz(i,k) = (phii(i,k+1) - phii(i,k)) /                    &
                        (tgrs(i,k)*(1.+con_fvirt*max(zero,qgrs(i,k,1))))
       enddo
     enddo

   end subroutine get_prs_fv3


   subroutine get_phi_fv3(ix, levs, ntrac, gt0, gq0, del_gz, phii, phil)
     integer, intent(in) :: ix, levs, ntrac
     real(kind=kind_phys), dimension(ix,levs),       intent(in)    :: gt0
     real(kind=kind_phys), dimension(ix,levs,ntrac), intent(in)    :: gq0
     real(kind=kind_phys), dimension(ix,levs+1),     intent(inout) :: del_gz
     real(kind=kind_phys), dimension(ix,levs+1),     intent(inout) :: phii
     real(kind=kind_phys), dimension(ix,levs),       intent(inout) :: phil

! SJL: Adjust the heighz hydrostatically in a way consistent with FV3 discretization
     do i=1,ix
        phii(i,1) = zero
     enddo
     do k=1,levs
       do i=1,ix
         del_gz(i,k) = del_gz(i,k)*gt0(i,k) *                          &
     &                 (1.+con_fvirt*max(zero,gq0(i,k,1)))
         phii(i,k+1) = phii(i,k) + del_gz(i,k)
         phil(i,k)   = half*(phii(i,k) + phii(i,k+1))
       enddo
     enddo

   end subroutine get_phi_fv3

end module gfs_fv3_needs

subroutine expand_ens(neig,nanals,ens_orig,ens_expanded,evectors)
  ! create modulated ensemble in model space with vertical
  ! localization 'baked in'.
  ! input:
  !    neig - number of localization eigenvectors
  !    nanals - number of original ensemble members
  !    ens_orig - original ensemble perturbations (dimension nanals)
  !    evectors - localization eigenvectors (dimension neig)
  ! output:
  !    ens_expanded - expanded modulated ensemble (dimension nanals*neig)
  ! Note:  evectors should be normalized so variance of modulated ensemble
  ! can be computed via sum(ens_expanded**2)/(nanals-1).  
  use kinds, only: r_single,r_kind, r_double
  integer, intent(in) :: neig,nanals
  real(r_single), intent(in) :: ens_orig(nanals)
  real(r_single), intent(out) :: ens_expanded(neig*nanals)
  real(r_double), intent(in) :: evectors(neig)
  integer nanalo,nanal,ne
  do nanal=1,nanals
     do ne = 1, neig
        nanalo = neig*(nanal-1) + ne 
        ens_expanded(nanalo) = ens_orig(nanal)*evectors(ne)
     enddo
  enddo
end subroutine expand_ens

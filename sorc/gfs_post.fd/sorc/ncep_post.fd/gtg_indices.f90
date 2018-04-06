module  gtg_indices
contains
  subroutine indices_gtg()
    print *, "Stub code for GTG protection but to make UPP public to work"
  end subroutine indices_gtg
end module gtg_indices

  subroutine gtg_algo(hgt,gust,qitfax,catonly,mwt)
    use ctlblk_mod, only: me,mpi_comm_comp,jsta_2l, jend_2u, jsta, jend, IM,JM,LM,SPVAL
    real, intent(in) :: hgt(im,jsta_2l:jend_2u)    ! terrain avg. ht in grid box (m)
    real, intent(in) :: gust(im,jsta_2l:jend_2u)  ! surface max gust (m/s)
    real, intent(inout) :: qitfax(IM,jsta_2l:jend_2u,LM)
    real, intent(inout),dimension(IM,jsta_2l:jend_2u,LM) :: catonly,mwt

    print *, "Stub code for GTG protection but to make UPP public to work"

    qitfax=SPVAL
    catonly=SPVAL
    mwt=SPVAL
    return
  end subroutine gtg_algo

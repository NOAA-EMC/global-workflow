module abstract_read_wrf_nmm_files_mod
  type, abstract :: abstract_read_wrf_nmm_files_class
  contains
    procedure(read_wrf_nmm_files), deferred, pass(this) :: read_wrf_nmm_files 
    procedure(read_nems_nmmb_files), deferred, pass(this) :: read_nems_nmmb_files 
  end type abstract_read_wrf_nmm_files_class

  abstract interface
  subroutine read_wrf_nmm_files(this,mype)
    use kinds, only: i_kind
    import abstract_read_wrf_nmm_files_class
    implicit none
    class(abstract_read_wrf_nmm_files_class),intent(inout) :: this
    integer(i_kind),intent(in):: mype
  end subroutine read_wrf_nmm_files
  end interface
  abstract interface
  subroutine read_nems_nmmb_files(this,mype)
    use kinds, only: i_kind
    import abstract_read_wrf_nmm_files_class
    implicit none
    class(abstract_read_wrf_nmm_files_class),intent(inout) :: this
    integer(i_kind),intent(in):: mype
  end subroutine read_nems_nmmb_files
  end interface

end module abstract_read_wrf_nmm_files_mod

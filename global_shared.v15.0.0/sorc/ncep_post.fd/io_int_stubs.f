! Stubs version of wrf io spi subroutines
!
!--- get_dom_ti_real
SUBROUTINE ext_int_get_dom_ti_real ( DataHandle,Element,   Data, Count, Outcount, Status )
  INTEGER ,       INTENT(IN)  :: DataHandle
  CHARACTER*(*) :: Element
  REAL ,          INTENT(OUT) :: Data(*)
  INTEGER ,       INTENT(IN)  :: Count
  INTEGER ,       INTENT(OUT) :: Outcount
  INTEGER ,       INTENT(OUT) :: Status

RETURN
END SUBROUTINE ext_int_get_dom_ti_real 


SUBROUTINE ext_int_get_dom_ti_integer ( DataHandle,Element,   Data, Count, Outcount, Status )

RETURN
END SUBROUTINE ext_int_get_dom_ti_integer 


!--- get_dom_ti_char
SUBROUTINE ext_int_get_dom_ti_char ( DataHandle,Element,   Data,  Status )

RETURN
END SUBROUTINE ext_int_get_dom_ti_char 


!--- get_var_info
SUBROUTINE ext_int_get_var_info ( DataHandle , VarName , NDim , MemoryOrder , Stagger , &
                              DomainStart , DomainEnd , WrfType, Status )

RETURN
END SUBROUTINE ext_int_get_var_info


!--- read_field
SUBROUTINE ext_int_read_field ( DataHandle , DateStr , VarName , Field , FieldType , Comm , IOComm, &
                            DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                            DomainStart , DomainEnd ,                                    &
                            MemoryStart , MemoryEnd ,                                    &
                            PatchStart , PatchEnd ,                                      &
                            Status )
  RETURN

END SUBROUTINE ext_int_read_field


!--- close
SUBROUTINE ext_int_ioclose ( DataHandle, Status )

  RETURN
END SUBROUTINE ext_int_ioclose


!--- initialize
SUBROUTINE ext_int_ioinit( SysDepInfo, Status )

END SUBROUTINE ext_int_ioinit



!--- open_for_read 
SUBROUTINE ext_int_open_for_read ( FileName , Comm_compute, Comm_io, SysDepInfo, &
                               DataHandle , Status )

  RETURN  
END SUBROUTINE ext_int_open_for_read



SUBROUTINE int_get_ti_header_c ( hdrbuf, hdrbufsize, n, itypesize, typesize, &
                           DataHandle, Data, Count, code )

RETURN
END SUBROUTINE int_get_ti_header_c	


! NETCDF STUBS
SUBROUTINE ext_ncd_ioinit(SysDepInfo, Status)

RETURN
END SUBROUTINE ext_ncd_ioinit


subroutine ext_ncd_open_for_read(DatasetName, Comm1, Comm2, SysDepInfo, DataHandle, Status)

RETURN 
END subroutine ext_ncd_open_for_read


subroutine ext_ncd_get_dom_ti_integer(DataHandle,Element,Data,Count,OutCount,Status)

RETURN
END subroutine ext_ncd_get_dom_ti_integer


subroutine ext_ncd_ioclose(DataHandle, Status)

  return
end subroutine ext_ncd_ioclose


subroutine ext_ncd_get_dom_ti_char(DataHandle,Element,Data,Status)

  return
end subroutine ext_ncd_get_dom_ti_char


subroutine ext_ncd_get_dom_ti_real(DataHandle,Element,Data,Count,Status)

  return
end subroutine ext_ncd_get_dom_ti_real


subroutine ext_ncd_get_var_info(DataHandle,Name,NDim,MemoryOrder, &
 Stagger,DomainStart,DomainEnd,WrfType,Status)
 
   return
end subroutine ext_ncd_get_var_info


subroutine ext_ncd_read_field(DataHandle,DateStr,Var,Field,FieldType,Comm,  &
  IOComm, DomainDesc, MemoryOrdIn, Stagger, DimNames,                       &
  DomainStart,DomainEnd,MemoryStart,MemoryEnd,PatchStart,PatchEnd,Status)
  
 return
end subroutine ext_ncd_read_field


subroutine wrf_error_fatal(massage)

stop
end subroutine wrf_error_fatal 


subroutine int_gen_ti_header_c ( hdrbuf, hdrbufsize, itypesize, typesize, &
                           DataHandle, Data, Count, code )
 RETURN
END SUBROUTINE int_gen_ti_header_c






		   

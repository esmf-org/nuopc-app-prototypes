module OceanCommon

  use ESMF
  use NUOPC
  
  use subModule1
  use subModule2
  
  implicit none
  
  private
  
  public procedure_ocnCommon
  
  contains
  
  subroutine procedure_ocnCommon(rc)
    integer, intent(out) :: rc
    print *, "Executing procedure_ocnCommon()"
    call procedure_subModule1()
    call procedure_subModule2()
    rc=ESMF_SUCCESS
  end subroutine
  
end module

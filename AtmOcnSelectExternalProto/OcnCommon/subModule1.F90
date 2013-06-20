module subModule1

  implicit none
  
  private
  
  public procedure_subModule1
  
  contains
  
  subroutine procedure_subModule1()
    print *, "Executing procedure_subModule1()"
  end subroutine
  
end module

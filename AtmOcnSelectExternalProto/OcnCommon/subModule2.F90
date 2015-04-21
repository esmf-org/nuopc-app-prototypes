module subModule2

  implicit none
  
  private
  
  public procedure_subModule2
  
  contains
  
  subroutine procedure_subModule2()
    print *, "Executing procedure_subModule2()"
  end subroutine
  
end module

!==============================================================================
! Earth System Modeling Framework
! Copyright 2002-2018, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================

module subModule2

  implicit none
  
  private
  
  public procedure_subModule2
  
  contains
  
  subroutine procedure_subModule2()
    print *, "Executing procedure_subModule2()"
  end subroutine
  
end module

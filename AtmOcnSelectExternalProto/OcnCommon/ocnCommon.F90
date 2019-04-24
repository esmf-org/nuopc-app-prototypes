!==============================================================================
! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================

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

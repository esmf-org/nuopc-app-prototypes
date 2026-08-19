cmake_minimum_required(VERSION 3.22)

# Project
project(ExternalDriverAPIProto
        VERSION 1.0.0
        LANGUAGES Fortran CXX C
        )

# Find ESMF
find_package(ESMF 9.0.0 REQUIRED)

# Find required packages
find_package(OpenMP REQUIRED)

# CMake has a bug with NAG and OpenMP:
#   https://gitlab.kitware.com/cmake/cmake/-/issues/21280
# so we work around it ... credit to https://github.com/mathomp4
if (OpenMP_Fortran_FOUND AND CMAKE_Fortran_COMPILER_ID STREQUAL "NAG")
  message(WARNING "NAG Fortran detected, resetting OpenMP flags to avoid CMake bug")
  set_property(TARGET OpenMP::OpenMP_Fortran PROPERTY INTERFACE_LINK_LIBRARIES "")
  set_property(TARGET OpenMP::OpenMP_Fortran PROPERTY INTERFACE_LINK_OPTIONS "-openmp")
endif()

# Add ESMX driver
add_subdirectory(${ESMF_ESMXDIR}/Driver ./ESMX_Driver)

# Create executable
add_executable(externalApp externalApp.F90)
target_include_directories(externalApp PUBLIC ${PROJECT_BINARY_DIR})
target_link_libraries(externalApp PUBLIC esmx_driver)
set_target_properties(externalApp PROPERTIES INSTALL_RPATH_USE_LINK_PATH TRUE)

# Install executable
install(
  TARGETS externalApp
  EXPORT externalApp
  RUNTIME DESTINATION bin
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib)

cmake_minimum_required(VERSION 3.22)

find_package(ESMF 8.9.0 MODULE REQUIRED)

if(NOT DEFINED CMAKE_Fortran_COMPILER)
  set(CMAKE_Fortran_COMPILER        "${ESMF_F90COMPILER}")
endif()

project(Lumo
        VERSION 1.0
        LANGUAGES Fortran)

find_package(OpenMP REQUIRED)

if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  set(CMAKE_Fortran_FLAGS "-ffree-line-length-none -Wall -Wextra -Wconversion -Wno-unused -Wno-unused-dummy-argument")
  set(CMAKE_Fortran_FLAGS_RELEASE "-Ofast")
  set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-g -fbacktrace -Ofast")
  set(CMAKE_Fortran_FLAGS_DEBUG "-g -fbacktrace -O0 -fcheck=all -ffpe-trap=invalid,zero,overflow,underflow")
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
  set(CMAKE_Fortran_FLAGS_RELEASE "-Ofast")
  set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-g -traceback -Ofast")
  set(CMAKE_Fortran_FLAGS_DEBUG "-g -traceback -O0 -check all -fpe0 -ftrapuv -init=snan,arrays")
else()
  message(WARNING "${CMAKE_Fortran_COMPILER_ID} Fortran compiler will be used with default options")
endif()

# CMake has a bug with NAG and OpenMP:
#   https://gitlab.kitware.com/cmake/cmake/-/issues/21280
# so we work around it ... credit to https://github.com/mathomp4
if (OpenMP_Fortran_FOUND AND CMAKE_Fortran_COMPILER_ID STREQUAL "NAG")
  message(WARNING "NAG Fortran detected, resetting OpenMP flags to avoid CMake bug")
  set_property(TARGET OpenMP::OpenMP_Fortran PROPERTY INTERFACE_LINK_LIBRARIES "")
  set_property(TARGET OpenMP::OpenMP_Fortran PROPERTY INTERFACE_LINK_OPTIONS "-openmp")
endif()

add_library(Lumo lumo.F90)
target_include_directories(Lumo
  INTERFACE $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
)
target_link_libraries(Lumo PUBLIC ESMF::ESMF)
target_link_libraries(Lumo PUBLIC OpenMP::OpenMP_Fortran)

###############################################################################
### Install
###############################################################################

install(
  TARGETS Lumo
  EXPORT  Lumo
)

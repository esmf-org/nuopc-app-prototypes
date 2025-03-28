cmake_minimum_required(VERSION 3.22)

find_package(ESMF 8.9.0 MODULE REQUIRED)

if(NOT DEFINED CMAKE_Fortran_COMPILER)
  set(CMAKE_Fortran_COMPILER "${ESMF_F90COMPILER}")
endif()

project(SiMoCo
        VERSION 1.0
        LANGUAGES Fortran)

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

add_library(SiMoCo SHARED simoco.F90)
target_include_directories(SiMoCo
  INTERFACE $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
)
target_link_libraries(SiMoCo PUBLIC ESMF::ESMF)

###############################################################################
### Install
###############################################################################

install(
  TARGETS SiMoCo
  EXPORT  SiMoCo
)

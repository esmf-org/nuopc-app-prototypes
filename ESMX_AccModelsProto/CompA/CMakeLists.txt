cmake_minimum_required(VERSION 3.22)

find_package(ESMF 8.9.0 MODULE REQUIRED)

if(NOT DEFINED CMAKE_Fortran_COMPILER)
  set(CMAKE_Fortran_COMPILER "${ESMF_F90COMPILER}")
endif()

project(CompA
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
#  set(CMAKE_Fortran_FLAGS "-Minfo=all -stdpar=gpu -gpu=mem:separate") # cuMemFree errors for managed memory
  set(CMAKE_Fortran_FLAGS "-Minfo=all -stdpar=multicore") # generates gpu and multicore code!
endif()

# Use STATIC library because SHARED can cause problems with memory management
#add_library(CompA SHARED compA.F90)
add_library(CompA STATIC compA.F90)

target_include_directories(CompA
  INTERFACE $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
)
target_link_libraries(CompA PUBLIC ESMF::ESMF)

option(USE_OPENACC "Enable OpenACC" OFF)
message(STATUS "Enable OpenACC: ${USE_OPENACC}")
if(USE_OPENACC)
  find_package(OpenACC REQUIRED)
  target_link_libraries(CompA PUBLIC OpenACC::OpenACC_Fortran)
endif()

###############################################################################
### Install (needed for SHARED lib version)
###############################################################################

#install(
#  TARGETS CompA
#  EXPORT  CompA
#  LIBRARY DESTINATION lib
#  ARCHIVE DESTINATION lib
#)

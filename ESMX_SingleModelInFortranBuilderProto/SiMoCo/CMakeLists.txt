cmake_minimum_required(VERSION 3.22)

project(SiMoCo
        VERSION 1.0
        LANGUAGES Fortran)

find_package(ESMF 9.0.0 REQUIRED)

if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  set(CMAKE_Fortran_FLAGS "-ffree-line-length-none -Wall -Wextra -Wconversion -Wno-unused -Wno-unused-dummy-argument")
  set(CMAKE_Fortran_FLAGS_RELEASE "-Ofast")
  set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-g -fbacktrace -Ofast")
  set(CMAKE_Fortran_FLAGS_DEBUG "-g -fbacktrace -O0 -fcheck=all -ffpe-trap=invalid,zero,overflow,underflow")
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
  set(CMAKE_Fortran_FLAGS_RELEASE "-Ofast")
  set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-g -traceback -Ofast")
  set(CMAKE_Fortran_FLAGS_DEBUG "-g -traceback -O0 -check all -fpe0 -ftrapuv -init=snan,arrays")
endif()

if(APPLE AND CMAKE_Fortran_COMPILER_ID STREQUAL "LLVMFlang")
    message(STATUS "Enabling CXX language for Darwin LLVMFlang linker workaround")
    enable_language(CXX)
endif()

add_library(SiMoCo SHARED simoco.F90)
target_include_directories(SiMoCo
  INTERFACE $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
)
target_link_libraries(SiMoCo PUBLIC ESMF::ESMF)

if(APPLE AND CMAKE_Fortran_COMPILER_ID STREQUAL "LLVMFlang")
    set_target_properties(SiMoCo PROPERTIES LINKER_LANGUAGE CXX)
endif()

###############################################################################
### Install
###############################################################################

install(
  TARGETS SiMoCo
  EXPORT  SiMoCo
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib
)

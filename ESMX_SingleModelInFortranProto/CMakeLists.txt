# Set the minimum CMake version
cmake_minimum_required(VERSION 3.22)

# Where to look for the local Find<Package>.cmake files
list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/SiMoCo/cmake")

# Find ESMF
find_package(ESMF 8.9.0 MODULE REQUIRED)

# Set compilers consistent with ESMF unless already set
if(NOT DEFINED CMAKE_Fortran_COMPILER)
  set(CMAKE_Fortran_COMPILER        "${ESMF_F90COMPILER}")
endif()
if(NOT DEFINED CMAKE_CXX_COMPILER)
  set(CMAKE_CXX_COMPILER            "${ESMF_CXXCOMPILER}")
endif()
if(NOT DEFINED CMAKE_C_COMPILER)
  set(CMAKE_C_COMPILER              "${ESMF_CCOMPILER}")
endif()

# Project setting
project(SiMoCoApp
        VERSION 1.0
        LANGUAGES Fortran CXX C)

# Define standard installation directories
include(GNUInstallDirs)

# Good practice to compile everything PIC
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

# Add SiMoCo
add_subdirectory(SiMoCo)

# Add ESMX
add_subdirectory(${ESMF_ESMXDIR} ${CMAKE_BINARY_DIR}/ESMX)

cmake_minimum_required(VERSION 3.12)
enable_language(Fortran)

# Where to look for Find<Package>.cmake files
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")

# Find ESMF
find_package(ESMF REQUIRED)

# Set compilers per ESMFMKFILE
set(CMAKE_CXX_COMPILER ${ESMF_CXXCOMPILER})
set(CMAKE_Fortran_COMPILER ${ESMF_F90LINKER})

# Specific project settings
project(TAWAS VERSION 1.0.0 DESCRIPTION "The Absolute Worst Atmosphere Stand-in")
add_library(tawas tawas.F90)
target_link_libraries(tawas ESMF)
set_target_properties(tawas PROPERTIES
  VERSION ${PROJECT_VERSION}
  SOVERSION 1)

# Set the minimum CMake version
cmake_minimum_required(VERSION 3.22)

# Project setting
project(SiMoCoApp
        VERSION 1.0
        LANGUAGES Fortran CXX C)

# Find ESMF
find_package(ESMF 9.0.0 REQUIRED)

# Define standard installation directories
include(GNUInstallDirs)

# Good practice to compile everything PIC
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

# Add SiMoCo
add_subdirectory(SiMoCo)

# Add ESMX
add_subdirectory(${ESMF_ESMXDIR} ${CMAKE_BINARY_DIR}/ESMX)

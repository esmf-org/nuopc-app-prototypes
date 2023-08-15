cmake_minimum_required(VERSION 3.22)

# Where to look for the local Find<Package>.cmake files
list(APPEND CMAKE_MODULE_PATH "${ESMF_ESMXDIR}/Driver/cmake")

# Find ESMF
find_package(ESMF 8.6.0 MODULE REQUIRED)

# Set compilers consistent with ESMF
set(CMAKE_Fortran_COMPILER        "${ESMF_F90COMPILER}")
set(CMAKE_CXX_COMPILER            "${ESMF_CXXCOMPILER}")
set(CMAKE_C_COMPILER              "${ESMF_CCOMPILER}")

# Project
project(ExternalDriverAPIProto
        VERSION 1.0.0
        LANGUAGES Fortran CXX C
        )

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

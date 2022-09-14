cmake_minimum_required(VERSION 3.5.2)
enable_language(Fortran)

add_subdirectory(${ESMF_ESMXDIR}/Driver ./ESMX_Driver)

# Specific project settings
project(ExternalDriverAPIProto VERSION 0.1.0)
add_executable(externalApp externalApp.F90)
target_include_directories(externalApp PUBLIC ${PROJECT_BINARY_DIR})
target_link_libraries(externalApp PUBLIC esmx_driver)


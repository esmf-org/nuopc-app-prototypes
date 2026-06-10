cmake_minimum_required(VERSION 3.22)

project(SiMoCoC
        VERSION 1.0
        LANGUAGES C)

find_package(ESMF 9.0.0 REQUIRED)

if(CMAKE_C_COMPILER_ID STREQUAL "GNU")
  set(CMAKE_C_FLAGS "-Wall -Wextra -Wconversion -Wno-unused")
  set(CMAKE_C_FLAGS_RELEASE "-Ofast")
  set(CMAKE_C_FLAGS_RELWITHDEBINFO "-g -fbacktrace -Ofast")
  set(CMAKE_C_FLAGS_DEBUG "-g -fbacktrace -O0 -fcheck=all")
elseif(CMAKE_C_COMPILER_ID STREQUAL "Intel")
  set(CMAKE_C_FLAGS_RELEASE "-Ofast")
  set(CMAKE_C_FLAGS_RELWITHDEBINFO "-g -traceback -Ofast")
  set(CMAKE_C_FLAGS_DEBUG "-g -traceback -O0 -check all -fpe0 -ftrapuv -init=snan,arrays")
else()
  message(WARNING "${CMAKE_C_COMPILER_ID} C compiler will be used with default options")
endif()

add_library(SiMoCoC SHARED simococ.c)
target_include_directories(SiMoCoC
  INTERFACE $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
)
target_link_libraries(SiMoCoC PUBLIC ESMF::ESMC)

###############################################################################
### Install
###############################################################################

install(
  TARGETS SiMoCoC
  EXPORT  SiMoCoC
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib
)

cmake_minimum_required(VERSION 3.19)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)
find_package(ESMF 8.6.0 MODULE REQUIRED)

set(CMAKE_C_COMPILER "${ESMF_CCOMPILER}")

project(Catmos
        VERSION 1.0
        LANGUAGES C)

if(CMAKE_C_COMPILER_ID MATCHES "GNU")
  set(CMAKE_C_FLAGS "-Wall -Wextra -Wconversion -Wno-unused")
  set(CMAKE_C_FLAGS_RELEASE "-Ofast")
  set(CMAKE_C_FLAGS_RELWITHDEBINFO "-g -fbacktrace -Ofast")
  set(CMAKE_C_FLAGS_DEBUG "-g -fbacktrace -O0 -fcheck=all")
elseif(CMAKE_C_COMPILER_ID MATCHES "Intel")
  set(CMAKE_C_FLAGS_RELEASE "-Ofast")
  set(CMAKE_C_FLAGS_RELWITHDEBINFO "-g -traceback -Ofast")
  set(CMAKE_C_FLAGS_DEBUG "-g -traceback -O0 -check all -fpe0 -ftrapuv -init=snan,arrays")
else()
  message(WARNING "${CMAKE_C_COMPILER_ID} C compiler will be used with default options")
endif()

add_library(Catmos SHARED catmos.c)
target_link_libraries(Catmos PUBLIC ESMF)

install(TARGETS Catmos
        EXPORT Catmos
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib
        INCLUDES DESTINATION mod
)
install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/
        DESTINATION mod
        FILES_MATCHING
        PATTERN "*.mod"
        PATTERN "CMakeFiles" EXCLUDE
)
install(EXPORT Catmos DESTINATION cmake)

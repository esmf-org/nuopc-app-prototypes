# Specific project settings
project(TAWAS VERSION 1.0.0 DESCRIPTION "The Absolute Worst Atmosphere Stand-in")
add_library(tawas tawas.F90)
target_link_libraries(tawas ESMF)
set_target_properties(tawas PROPERTIES
  VERSION ${PROJECT_VERSION}
  SOVERSION 1)

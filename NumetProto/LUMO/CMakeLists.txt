# Specific project settings
project(lumo VERSION 1.0.0 DESCRIPTION "Least Useful Model of the Ocean")
add_library(lumo lumo.F90)
target_link_libraries(lumo ESMF)
set_target_properties(lumo PROPERTIES
  VERSION ${PROJECT_VERSION}
  SOVERSION 1)

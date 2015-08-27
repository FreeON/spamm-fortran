add_library(spamm
  spamm.F90
  spamm_types.F90
  spamm_types_2d.F90)

set_target_properties(spamm
  PROPERTIES
  COMPILE_DEFINITIONS "${SPAMM_COMPILE_DEFINITIONS}")

install(TARGETS spamm
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib)

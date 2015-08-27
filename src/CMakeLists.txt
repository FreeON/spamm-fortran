add_library(spamm
  spamm.F90
  spamm_types.F90
  spamm_types_2d.F90)

install(TARGETS spamm
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib)
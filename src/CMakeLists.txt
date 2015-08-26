add_library(spamm
  spamm.F90)
install(TARGETS spamm
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib)
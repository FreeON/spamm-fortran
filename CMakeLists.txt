cmake_minimum_required(VERSION 2.8)
project(spamm C Fortran)

include(FindOpenMP)

add_subdirectory(src)

set(SPAMM_TESTING FALSE
  CACHE BOOL "Whether to build the test-suite.")
if(SPAMM_TESTING)
  enable_testing()
  add_subdirectory(tests)
endif()

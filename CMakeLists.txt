cmake_minimum_required(VERSION 2.8)
project(spamm C CXX Fortran)

include(FindOpenMP)

include(FindDoxygen)
if(DOXYGEN_FOUND)
  configure_file(docs/Doxygen.in docs/Doxygen)
  add_custom_target(docs
    ${DOXYGEN_EXECUTABLE} ${CMAKE_BINARY_DIR}/docs/Doxygen)
endif()

add_subdirectory(src)

set(SPAMM_TESTING FALSE
  CACHE BOOL "Whether to build the test-suite.")
if(SPAMM_TESTING)
  enable_testing()
  add_subdirectory(tests)
endif()
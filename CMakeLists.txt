cmake_minimum_required(VERSION 2.8)
project(spamm Fortran)

include(FindDoxygen)
if(DOXYGEN_FOUND)
  configure_file(docs/Doxygen.in docs/Doxygen)
  add_custom_target(docs
    ${DOXYGEN_EXECUTABLE} ${CMAKE_BINARY_DIR}/docs/Doxygen)
endif()

unset(SPAMM_COMPILE_DEFINITIONS)
set(SPAMM_BLOCK_SIZE 4
  CACHE STRING "The smallest submatrix size.")
list(APPEND SPAMM_COMPILE_DEFINITIONS "SPAMM_BLOCK_SIZE=${SPAMM_BLOCK_SIZE}")

set(SPAMM_CHUNK_SIZE 32
  CACHE STRING "The matrix size of a chunk.")
list(APPEND SPAMM_COMPILE_DEFINITIONS "SPAMM_CHUNK_SIZE=${SPAMM_CHUNK_SIZE}")

mark_as_advanced(SPAMM_CHUNK_BLOCKS)
math(EXPR SPAMM_CHUNK_BLOCKS "${SPAMM_CHUNK_SIZE}/${SPAMM_BLOCK_SIZE}")
set(SPAMM_CHUNK_BLOCKS ${SPAMM_CHUNK_BLOCKS}
  CACHE STRING "The number of submatrix blocks in a chunk.")
list(APPEND SPAMM_COMPILE_DEFINITIONS "SPAMM_CHUNK_BLOCKS=${SPAMM_CHUNK_BLOCKS}")

mark_as_advanced(SPAMM_CHUNK_NODES)
math(EXPR SPAMM_CHUNK_NODES "(4*${SPAMM_CHUNK_BLOCKS}*${SPAMM_CHUNK_BLOCKS}-1)/3.")
set(SPAMM_CHUNK_NODES ${SPAMM_CHUNK_NODES}
  CACHE STRING "The number of tree nodes inside a chunk.")
list(APPEND SPAMM_COMPILE_DEFINITIONS "SPAMM_CHUNK_NODES=${SPAMM_CHUNK_NODES}")

add_subdirectory(src)

set(SPAMM_TESTING FALSE
  CACHE BOOL "Whether to build the test-suite.")
if(SPAMM_TESTING)
  enable_testing()
  add_subdirectory(tests)
endif()

cmake_minimum_required(VERSION 2.8)
project(spamm Fortran)

set(GNU_Fortran_FLAGS_DEBUG "-O0 -g")
set(GNU_Fortran_FLAGS_RELEASE "-O2 -g")
set(Intel_Fortran_FLAGS_DEBUG "-O0 -g -debug all -check all -traceback")
set(Intel_Fortran_FLAGS_RELEASE "-O2 -g")

# Make sure we get the right RPATH. We want the RPATH to be the build
# path and after installing, the actual installed library path.
set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)
list(FIND CMAKE_PLATFORM_IMPLICIT_LINK_DIRECTORIES "${CMAKE_INSTALL_PREFIX}/lib" isSystemDir)
if("${isSystemDir}" STREQUAL "-1")
  set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")
endif()

if(NOT CMAKE_Fortran_FLAGS)
  if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU" OR
      CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
    set(CMAKE_Fortran_FLAGS_DEBUG ${${CMAKE_Fortran_COMPILER_ID}_Fortran_FLAGS_DEBUG})
    set(CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_COMPILER_ID}_Fortran_FLAGS_RELEASE)
  else()
    message(STATUS "Unknown compiler ${CMAKE_Fortran_COMPILER_ID}")
  endif()
endif()

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

include_directories(${CMAKE_BINARY_DIR}/src)

add_executable(spamm-single-multiply single_multiply.F90)
target_link_libraries(spamm-single-multiply spamm)

install(TARGETS spamm-single-multiply
  RUNTIME DESTINATION bin)
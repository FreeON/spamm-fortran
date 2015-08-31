add_test(NAME true COMMAND /bin/true)

set(TESTS
  convert_2d.F90
  multiply_2d.F90)

include_directories(${CMAKE_BINARY_DIR}/src)
foreach(file ${TESTS})
  get_filename_component(TEST ${file} NAME_WE)
  add_executable(${TEST} ${file})
  target_link_libraries(${TEST} spamm)
  add_test(${TEST} ${TEST})
endforeach()

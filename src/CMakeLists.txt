set(SPAMM_SOURCES
  spamm.F90
  spamm_check.F90
  spamm_check_2d.F90
  spamm_constructor.F90
  spamm_constructor_2d.F90
  spamm_convert.F90
  spamm_convert_2d.F90
  spamm_dense_matrix.F90
  spamm_multiply.F90
  spamm_multiply_2d.F90
  spamm_strings.F90
  spamm_types.F90
  spamm_types_chunk_2d.F90
  spamm_types_tree_2d.F90)

foreach(f ${SPAMM_SOURCES})
  get_filename_component(MOD ${f} NAME_WE)
  install(FILES ${CMAKE_CURRENT_BINARY_DIR}/${MOD}.mod
    DESTINATION include)
endforeach()

add_library(spamm ${SPAMM_SOURCES})
set_target_properties(spamm
  PROPERTIES
  COMPILE_DEFINITIONS "${SPAMM_COMPILE_DEFINITIONS}")

install(TARGETS spamm
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib)

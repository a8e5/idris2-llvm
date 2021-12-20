message(VERBOSE "find Idris2 $ENV{HOME}")

find_program(
  IDRIS2_EXECUTABLE
  idris2
  PATHS "$ENV{HOME}/.idris2/bin"
  )

message(VERBOSE "idris2 found: ${IDRIS2_EXECUTABLE}")

function(add_idris_package TARGET_NAME IPKG_FILE)
  set(IDRIS_BUILD_DIR "${CMAKE_BINARY_DIR}/build/${TARGET_NAME}")
  set(IDRIS_OUTPUT_DIR "${IDRIS_BUILD_DIR}/exec")

  set(TARGET_EXECUTABLE_PATH "${IDRIS_OUTPUT_DIR}/${TARGET_NAME}")

  add_custom_command(
    OUTPUT "${TARGET_EXECUTABLE_PATH}"
    COMMAND ${IDRIS2_EXECUTABLE}
    ARGS
      # put each target in its own subdirectory
      # this can result in single modules being compiled multiple times, but
      # enables building several targets in parallel
      "--build-dir" "${IDRIS_BUILD_DIR}"
      "--build" ${IPKG_FILE}
    WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
    DEPENDS ${ARGN}
    )
  add_custom_target(
    "${TARGET_NAME}" ALL
    DEPENDS "${TARGET_EXECUTABLE_PATH}"
    )

  set_target_properties(
    "${TARGET_NAME}" PROPERTIES
    ADDITIONAL_CLEAN_FILES "${IDRIS_BUILD_DIR}")

  set(${TARGET_NAME}_EXECUTABLE "${IDRIS_OUTPUT_DIR}/${TARGET_NAME}"
    PARENT_SCOPE)
endfunction()

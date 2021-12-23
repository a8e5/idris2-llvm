set(RTS_SOURCES
  builtin.c
  gc.c
  getline.c
  memory.c
  net.c
  object.c
  rts.c
  time.c
  ../external/llvm-statepoint-utils/build/statepoint.c
  vendor/hashset.c
  )

set(RTS_BITCODE)
set(RAPID_RTS_LIB "${RAPID_SUPPORT_BINARIES}/rapid/runtime.bc")
set(RAPID_PLATFORM_LIB "${RAPID_SUPPORT_BINARIES}/rapid/platform.a")

set(RTS_INCLUDES
  "."
  "/usr/local/include"
  "${CMAKE_CURRENT_SOURCE_DIR}/../external/llvm-statepoint-utils/dist"
  )

set(RTS_CFLAGS)

if (APPLE)
  list(APPEND RTS_CFLAGS "-D_DARWIN_C_SOURCE")
else()
  list(APPEND RTS_CFLAGS "-D_GNU_SOURCE")
endif()
list(APPEND RTS_CFLAGS "-Wall" "-Wpedantic" "-Werror" "-Wno-error=format")

set(RAPID_GC_STATS_ENABLED OFF CACHE BOOL "Print GC stats at program exit")
if (RAPID_GC_STATS_ENABLED)
  list(APPEND RTS_CFLAGS "-DRAPID_GC_STATS_ENABLED")
endif()

foreach(FILE ${RTS_INCLUDES})
  list(APPEND RTS_INCLUDE_FLAGS "-I" "${FILE}")
endforeach()

foreach(C_SOURCE ${RTS_SOURCES} "entry.c")
  string(REGEX REPLACE "\.c$" ".bc" BC_TARGET ${C_SOURCE})
  add_custom_command(
    OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/${BC_TARGET}"
    COMMAND clang ARGS "-flto" "-g" "-O3" "-c" "-MMD" "-MF${CMAKE_CURRENT_BINARY_DIR}/${BC_TARGET}.d"
    ${RTS_CFLAGS} ${RTS_INCLUDE_FLAGS} "-std=c11" "-o" "${CMAKE_CURRENT_BINARY_DIR}/${BC_TARGET}" "${CMAKE_CURRENT_SOURCE_DIR}/${C_SOURCE}"
    DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/${C_SOURCE}"
    DEPFILE "${CMAKE_CURRENT_BINARY_DIR}/${BC_TARGET}.d"
    )
  set(RTS_BITCODE "${RTS_BITCODE};${CMAKE_CURRENT_BINARY_DIR}/${BC_TARGET}")
endforeach()

add_custom_command(
  OUTPUT "${RAPID_RTS_LIB}"
  COMMAND llvm-link ARGS -o "${RAPID_RTS_LIB}"
  ${RTS_BITCODE}
  DEPENDS ${RTS_BITCODE}
  )

set(RAPID_PLATFORM_SOURCE "${CMAKE_CURRENT_SOURCE_DIR}/arch_x86-64.S")
add_custom_command(
  OUTPUT "${RAPID_PLATFORM_LIB}"
  COMMAND clang ARGS "-c" "-o" "${RAPID_PLATFORM_LIB}.o" ${RAPID_PLATFORM_SOURCE}
  COMMAND llvm-ar ARGS rcsu "${RAPID_PLATFORM_LIB}" "${RAPID_PLATFORM_LIB}.o"
  DEPENDS ${RAPID_PLATFORM_SOURCE}
  BYPRODUCTS "${RAPID_PLATFORM_LIB}.o"
  )

add_custom_target(rapid_rts ALL
  DEPENDS "${RAPID_RTS_LIB}" "${RAPID_PLATFORM_LIB}"
  )

#######
# tests
#######

add_executable(rapid_rts_test
  test/test_main.c
  ${RTS_SOURCES}
  )
target_compile_options(rapid_rts_test PUBLIC -g ${RTS_CFLAGS})
target_include_directories(rapid_rts_test PUBLIC ${RTS_INCLUDES})
target_link_directories(rapid_rts_test PUBLIC
  "/usr/local/lib"
  )
target_link_libraries(rapid_rts_test PUBLIC
  gc
  gmp
  m
  "${RAPID_PLATFORM_LIB}"
  )

add_custom_target(rapid_test_rts
                  COMMAND rapid_rts_test
                  WORKING_DIRECTORY .
                  DEPENDS rapid_rts_test)

if (NOT TARGET test)
  add_custom_target(test)
endif()
add_dependencies(test rapid_test_rts rapid_rts)

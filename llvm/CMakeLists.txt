cmake_minimum_required(VERSION 3.12)
project(RapidLLVMPasses)

find_package(LLVM REQUIRED CONFIG)
find_program(LLVMLIT NAMES lit llvm-lit llvm-lit10 llvm-lit11 llvm-lit12 REQUIRED)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

if (${LLVM_ENABLE_RTTI})
else()
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-rtti")
endif()

add_library(rapid MODULE Passes/LowerRapidIntrinsics.cpp)

set_property(TARGET rapid PROPERTY CXX_STANDARD 17)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Allow undefined symbols
target_link_libraries(rapid
  "$<$<PLATFORM_ID:Darwin>:-undefined dynamic_lookup>")

add_custom_command(
  OUTPUT "${RAPID_SUPPORT_BINARIES}/rapid/librapid.so"
  COMMAND "cmake" ARGS "-E" copy_if_different $<TARGET_FILE:rapid> "${RAPID_SUPPORT_BINARIES}/rapid"
  DEPENDS rapid
  )

# workaround for older CMake version
if (${RAPID_SUPPORT_BINARIES} MATCHES .)
  add_custom_target(rapid_install_llvm_passes ALL
    DEPENDS "${RAPID_SUPPORT_BINARIES}/rapid/librapid.so"
    )
endif()

add_custom_target(rapid_test_llvm_passes
                  COMMAND ${LLVMLIT} -v "${CMAKE_CURRENT_SOURCE_DIR}/test"
                  DEPENDS rapid)

if (NOT TARGET test)
  add_custom_target(test)
endif()
add_dependencies(test rapid_test_llvm_passes)

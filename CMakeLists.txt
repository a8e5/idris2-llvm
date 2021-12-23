cmake_minimum_required(VERSION 3.18)

project(rapid)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")

find_package(Idris2)

set(RAPID_SUPPORT_BINARIES "${CMAKE_BINARY_DIR}/build/support")
add_custom_target(test)

add_idris_package(rapid-lite rapid-lite.ipkg
  src/Compiler/GenLLVMIR.idr
  src/Compiler/LLVM/Instruction.idr
  src/Compiler/LLVM/IR.idr
  src/Compiler/LLVM/Rapid/Builtin.idr
  src/Compiler/LLVM/Rapid/Foreign.idr
  src/Compiler/LLVM/Rapid/Integer.idr
  src/Compiler/LLVM/Rapid/Object.idr
  src/Compiler/Optimize.idr
  src/Compiler/PrepareCode.idr
  src/Compiler/VMCodeSexp.idr
  src/Control/Codegen.idr
  src/Data/Sexp.idr
  src/Data/Sexp/Lexer.idr
  src/Data/Sexp/Parser.idr
  src/Data/Utils.idr
  src/Rapid/Common.idr
  src/Rapid/Driver.idr
  src/Rapid/Lite.idr
  )

add_idris_package(rapidc rapidc.ipkg
  src/Compiler/Codegen/LLVM.idr
  src/Compiler/Codegen/VmcodeSexp.idr
  src/Compiler/GenLLVMIR.idr
  src/Compiler/LLVM/Instruction.idr
  src/Compiler/LLVM/IR.idr
  src/Compiler/LLVM/Rapid/Builtin.idr
  src/Compiler/LLVM/Rapid/Foreign.idr
  src/Compiler/LLVM/Rapid/Integer.idr
  src/Compiler/LLVM/Rapid/Object.idr
  src/Compiler/PrepareCode.idr
  src/Compiler/VMCodeSexp.idr
  src/Control/Codegen.idr
  src/Data/Sexp.idr
  src/Data/Utils.idr
  src/Rapid/Common.idr
  src/Rapid/Driver.idr
  src/Rapid/Main.idr
)

add_subdirectory(llvm)
add_subdirectory(rts)

add_custom_target(rapidc_test
  COMMAND cmake -E env "IDRIS2_DATA=${RAPID_SUPPORT_BINARIES}:${CMAKE_CURRENT_SOURCE_DIR}/support:$ENV{IDRIS2_DATA}" rapidc=${rapidc_EXECUTABLE} ./runtests.sh --good
  DEPENDS rapidc rapid_rts
  WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
  USES_TERMINAL
  )
add_dependencies(test rapidc_test)

include_directories( ${CMAKE_CURRENT_BINARY_DIR}/..
                     ${CMAKE_CURRENT_SOURCE_DIR}/.. )

add_llvm_library(LLVMGBZ80Info
  GBZ80TargetInfo.cpp
)

add_dependencies(LLVMGBZ80Info GBZ80CommonTableGen)


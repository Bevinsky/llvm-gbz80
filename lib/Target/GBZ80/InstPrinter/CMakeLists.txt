include_directories( ${CMAKE_CURRENT_BINARY_DIR}/..
                     ${CMAKE_CURRENT_SOURCE_DIR}/.. )

add_llvm_library(LLVMGBZ80AsmPrinter
  GBZ80InstPrinter.cpp
  )

add_dependencies(LLVMGBZ80AsmPrinter GBZ80CommonTableGen)
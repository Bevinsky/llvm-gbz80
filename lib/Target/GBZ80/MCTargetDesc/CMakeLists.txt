add_llvm_library(LLVMGBZ80Desc
  GBZ80AsmBackend.cpp
  GBZ80ELFObjectWriter.cpp
  GBZ80ELFStreamer.cpp
  GBZ80MCAsmInfo.cpp
  GBZ80MCCodeEmitter.cpp
  GBZ80MCExpr.cpp
  GBZ80MCTargetDesc.cpp
  GBZ80TargetStreamer.cpp
)

add_dependencies(LLVMGBZ80Desc GBZ80CommonTableGen)


set(LLVM_TARGET_DEFINITIONS GBZ80.td)

tablegen(LLVM GBZ80GenRegisterInfo.inc -gen-register-info)
tablegen(LLVM GBZ80GenInstrInfo.inc -gen-instr-info)
tablegen(LLVM GBZ80GenMCCodeEmitter.inc -gen-emitter)
tablegen(LLVM GBZ80GenAsmWriter.inc -gen-asm-writer)
tablegen(LLVM GBZ80GenDAGISel.inc -gen-dag-isel)
tablegen(LLVM GBZ80GenCallingConv.inc -gen-callingconv)
tablegen(LLVM GBZ80GenDAGISel.inc -gen-dag-isel)
tablegen(LLVM GBZ80GenInstrInfo.inc -gen-instr-info)
tablegen(LLVM GBZ80GenRegisterInfo.inc -gen-register-info)
tablegen(LLVM GBZ80GenSubtargetInfo.inc -gen-subtarget)
add_public_tablegen_target(GBZ80CommonTableGen)

add_llvm_target(GBZ80CodeGen
  GBZ80AsmPrinter.cpp
  GBZ80FrameLowering.cpp
  GBZ80InstrInfo.cpp
  GBZ80ISelDAGToDAG.cpp
  GBZ80ISelLowering.cpp
  GBZ80MCInstLower.cpp
  GBZ80PostISel.cpp
  GBZ80PreRA.cpp
  GBZ80PostRA.cpp
  GBZ80PostPEI.cpp
  GBZ80PreEmit.cpp
  GBZ80RegisterInfo.cpp
  GBZ80Subtarget.cpp
  GBZ80TargetMachine.cpp
  GBZ80TargetObjectFile.cpp

  DEPENDS
  intrinsics_gen
)

add_subdirectory(InstPrinter)
add_subdirectory(MCTargetDesc)
add_subdirectory(TargetInfo)

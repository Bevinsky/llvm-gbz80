//===-- GBZ80MCTargetDesc.cpp - GBZ80 Target Descriptions ---------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file provides GBZ80 specific target descriptions.
//
//===----------------------------------------------------------------------===//

#include "GBZ80MCTargetDesc.h"
#include "GBZ80ELFStreamer.h"
#include "GBZ80MCAsmInfo.h"
#include "GBZ80TargetStreamer.h"
#include "InstPrinter/GBZ80InstPrinter.h"

#include "llvm/MC/MCELFStreamer.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/Support/TargetRegistry.h"

#define GET_INSTRINFO_MC_DESC
#include "GBZ80GenInstrInfo.inc"

#define GET_SUBTARGETINFO_MC_DESC
#include "GBZ80GenSubtargetInfo.inc"

#define GET_REGINFO_MC_DESC
#include "GBZ80GenRegisterInfo.inc"

using namespace llvm;

static MCInstrInfo *createGBZ80MCInstrInfo() {
  MCInstrInfo *X = new MCInstrInfo();
  InitGBZ80MCInstrInfo(X);

  return X;
}

static MCRegisterInfo *createGBZ80MCRegisterInfo(const Triple &TT) {
  MCRegisterInfo *X = new MCRegisterInfo();
  InitGBZ80MCRegisterInfo(X, 0);

  return X;
}

static MCSubtargetInfo *createGBZ80MCSubtargetInfo(const Triple &TT,
                                                 StringRef CPU, StringRef FS) {
  return createGBZ80MCSubtargetInfoImpl(TT, CPU, FS);
}

static MCInstPrinter *createGBZ80MCInstPrinter(const Triple &T,
                                             unsigned SyntaxVariant,
                                             const MCAsmInfo &MAI,
                                             const MCInstrInfo &MII,
                                             const MCRegisterInfo &MRI) {
  if (SyntaxVariant == 0) {
    return new GBZ80InstPrinter(MAI, MII, MRI);
  }

  return nullptr;
}

static MCStreamer *createMCStreamer(const Triple &T, MCContext &Context,
                                    MCAsmBackend &MAB, raw_pwrite_stream &OS,
                                    MCCodeEmitter *Emitter, bool RelaxAll) {
  return nullptr;
  //return createELFStreamer(Context, MAB, OS, Emitter, RelaxAll); // $HACK $MS: commented
}

static MCTargetStreamer *
createGBZ80ObjectTargetStreamer(MCStreamer &S, const MCSubtargetInfo &STI) {
  return new GBZ80ELFStreamer(S, STI);
}

static MCTargetStreamer *createMCAsmTargetStreamer(MCStreamer &S,
                                                   formatted_raw_ostream &OS,
                                                   MCInstPrinter *InstPrint,
                                                   bool isVerboseAsm) {
  return new GBZ80TargetAsmStreamer(S);
}

extern "C" void LLVMInitializeGBZ80TargetMC() {
  // Register the MC asm info.
  RegisterMCAsmInfo<GBZ80MCAsmInfo> X(getTheGBZ80Target());

  // Register the MC instruction info.
  TargetRegistry::RegisterMCInstrInfo(getTheGBZ80Target(), createGBZ80MCInstrInfo);

  // Register the MC register info.
  TargetRegistry::RegisterMCRegInfo(getTheGBZ80Target(), createGBZ80MCRegisterInfo);

  // Register the MC subtarget info.
  TargetRegistry::RegisterMCSubtargetInfo(getTheGBZ80Target(),
                                          createGBZ80MCSubtargetInfo);

  // Register the MCInstPrinter.
  TargetRegistry::RegisterMCInstPrinter(getTheGBZ80Target(),
                                        createGBZ80MCInstPrinter);

  // Register the MC Code Emitter
  TargetRegistry::RegisterMCCodeEmitter(getTheGBZ80Target(), createGBZ80MCCodeEmitter);

  // Register the ELF streamer
  //TargetRegistry::RegisterELFStreamer(getTheGBZ80Target(), createMCStreamer); // $HACK $MS: commented

  // Register the obj target streamer.
  //TargetRegistry::RegisterObjectTargetStreamer(getTheGBZ80Target(), // $HACK $MS: commented
  //                                             createGBZ80ObjectTargetStreamer);

  // Register the asm target streamer.
  TargetRegistry::RegisterAsmTargetStreamer(getTheGBZ80Target(),
                                            createMCAsmTargetStreamer);

  // Register the asm backend (as little endian).
  // TargetRegistry::RegisterMCAsmBackend(getTheGBZ80Target(), createGBZ80AsmBackend); // $HACK $MS: commented
}


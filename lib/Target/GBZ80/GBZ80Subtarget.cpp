//===-- GBZ80Subtarget.cpp - GBZ80 Subtarget Information ----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the GBZ80 specific subclass of TargetSubtargetInfo.
//
//===----------------------------------------------------------------------===//

#include "GBZ80Subtarget.h"

#include "llvm/BinaryFormat/ELF.h"
#include "llvm/Support/TargetRegistry.h"

#include "GBZ80.h"
#include "GBZ80TargetMachine.h"
#include "MCTargetDesc/GBZ80MCTargetDesc.h"

#define DEBUG_TYPE "GBZ80-subtarget"

#define GET_SUBTARGETINFO_TARGET_DESC
#define GET_SUBTARGETINFO_CTOR
#include "GBZ80GenSubtargetInfo.inc"

namespace llvm {

GBZ80Subtarget::GBZ80Subtarget(const Triple &TT, const std::string &CPU,
                           const std::string &FS, GBZ80TargetMachine &TM)
    : GBZ80GenSubtargetInfo(TT, CPU, FS), InstrInfo(), FrameLowering(),
      TLInfo(TM), TSInfo(),

      // Subtarget features
      m_MBC1(false), m_MBC2(false), m_MBC3(false), m_RAM(false),
      m_Battery(false), m_RTC(false) {
  // Parse features string.
  ParseSubtargetFeatures(CPU, FS);
  assert((m_MBC1 + m_MBC2 + m_MBC3) <= 1 && "Multiple MBCs?");
}

} // end of namespace llvm

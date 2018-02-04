//===-- GBZ80AsmBackend.h - GBZ80 Asm Backend  --------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// \file The GBZ80 assembly backend implementation.
//
//===----------------------------------------------------------------------===//
//

#ifndef LLVM_GBZ80_ASM_BACKEND_H
#define LLVM_GBZ80_ASM_BACKEND_H

#include "MCTargetDesc/GBZ80FixupKinds.h"

#include "llvm/ADT/Triple.h"
#include "llvm/MC/MCAsmBackend.h"

namespace llvm {

class MCAssembler;
class MCObjectWriter;
class Target;

struct MCFixupKindInfo;

/// Utilities for manipulating generated GBZ80 machine code.
class GBZ80AsmBackend : public MCAsmBackend {
public:

  GBZ80AsmBackend(Triple::OSType OSType)
      : MCAsmBackend(), OSType(OSType) {}

  void adjustFixupValue(const MCFixup &Fixup, const MCValue &Target,
                        uint64_t &Value, MCContext *Ctx = nullptr) const;

  MCObjectWriter *createObjectWriter(raw_pwrite_stream &OS) const override;

  void applyFixup(const MCAssembler &Asm, const MCFixup &Fixup,
                  const MCValue &Target, MutableArrayRef<char> Data,
                  uint64_t Value, bool IsPCRel) const override;

  const MCFixupKindInfo &getFixupKindInfo(MCFixupKind Kind) const override;

  unsigned getNumFixupKinds() const override {
    return GB::NumTargetFixupKinds;
  }

  bool mayNeedRelaxation(const MCInst &Inst) const override { return false; }

  bool fixupNeedsRelaxation(const MCFixup &Fixup, uint64_t Value,
                            const MCRelaxableFragment *DF,
                            const MCAsmLayout &Layout) const override {
    llvm_unreachable("RelaxInstruction() unimplemented");
    return false;
  }

  void relaxInstruction(const MCInst &Inst, const MCSubtargetInfo &STI,
                        MCInst &Res) const override {}

  bool writeNopData(uint64_t Count, MCObjectWriter *OW) const override;

  bool shouldForceRelocation(const MCAssembler &Asm, const MCFixup &Fixup,
                             const MCValue &Target) override;

private:
  Triple::OSType OSType;
};

} // end namespace llvm

#endif // LLVM_GBZ80_ASM_BACKEND_H


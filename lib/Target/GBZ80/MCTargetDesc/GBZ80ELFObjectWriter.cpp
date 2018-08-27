//===-- GBZ80ELFObjectWriter.cpp - GBZ80 ELF Writer ---------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/GBZ80FixupKinds.h"
#include "MCTargetDesc/GBZ80MCTargetDesc.h"

#include "llvm/MC/MCAssembler.h"
#include "llvm/MC/MCELFObjectWriter.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCSection.h"
#include "llvm/MC/MCValue.h"
#include "llvm/Support/ErrorHandling.h"

namespace llvm {

/// TODO: Remove all of this. We don't support ELF.
class GBZ80ELFObjectWriter : public MCELFObjectTargetWriter {
public:
  GBZ80ELFObjectWriter(uint8_t OSABI);

  virtual ~GBZ80ELFObjectWriter() {}

  unsigned getRelocType(MCContext &Ctx,
                        const MCValue &Target,
                        const MCFixup &Fixup,
                        bool IsPCRel) const override;
};

GBZ80ELFObjectWriter::GBZ80ELFObjectWriter(uint8_t OSABI)
    : MCELFObjectTargetWriter(false, OSABI, ELF::EM_NONE, true/*, false*/) {}

unsigned GBZ80ELFObjectWriter::getRelocType(MCContext &Ctx,
                                          const MCValue &Target,
                                          const MCFixup &Fixup,
                                          bool IsPCRel) const {

  llvm_unreachable("invalid fixup kind!");
}

std::unique_ptr<MCObjectWriter> createGBZ80ELFObjectWriter(raw_pwrite_stream &OS, uint8_t OSABI) {
  return createELFObjectWriter(llvm::make_unique<GBZ80ELFObjectWriter>(OSABI), OS, true);
}

} // end of namespace llvm


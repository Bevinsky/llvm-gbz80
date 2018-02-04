//===-- GBZ80MCCodeEmitter.cpp - Convert GBZ80 Code to Machine Code -----------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the GBZ80MCCodeEmitter class.
//
//===----------------------------------------------------------------------===//

#include "GBZ80MCCodeEmitter.h"

#include "MCTargetDesc/GBZ80MCExpr.h"
#include "MCTargetDesc/GBZ80MCTargetDesc.h"

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCFixup.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"

#define DEBUG_TYPE "mccodeemitter"

#define GET_INSTRMAP_INFO
#include "GBZ80GenInstrInfo.inc"
#undef GET_INSTRMAP_INFO

namespace llvm {

unsigned GBZ80MCCodeEmitter::getExprOpValue(const MCExpr *Expr,
                                          SmallVectorImpl<MCFixup> &Fixups,
                                          const MCSubtargetInfo &STI) const {

  MCExpr::ExprKind Kind = Expr->getKind();

  if (Kind == MCExpr::Binary) {
    Expr = static_cast<const MCBinaryExpr *>(Expr)->getLHS();
    Kind = Expr->getKind();
  }

  if (Kind == MCExpr::Target) {
    GBZ80MCExpr const *GBZ80Expr = cast<GBZ80MCExpr>(Expr);
    int64_t Result;
    if (GBZ80Expr->evaluateAsConstant(Result)) {
      return Result;
    }

    MCFixupKind FixupKind = static_cast<MCFixupKind>(GBZ80Expr->getFixupKind());
    Fixups.push_back(MCFixup::create(0, GBZ80Expr, FixupKind));
    return 0;
  }

  assert(Kind == MCExpr::SymbolRef);
  return 0;
}

unsigned GBZ80MCCodeEmitter::getMachineOpValue(const MCInst &MI,
                                             const MCOperand &MO,
                                             SmallVectorImpl<MCFixup> &Fixups,
                                             const MCSubtargetInfo &STI) const {
  if (MO.isReg()) return Ctx.getRegisterInfo()->getEncodingValue(MO.getReg());
  if (MO.isImm()) return static_cast<unsigned>(MO.getImm());

  if (MO.isFPImm())
    return static_cast<unsigned>(APFloat(MO.getFPImm())
                                     .bitcastToAPInt()
                                     .getHiBits(32)
                                     .getLimitedValue());

  // MO must be an Expr.
  assert(MO.isExpr());

  return getExprOpValue(MO.getExpr(), Fixups, STI);
}

void GBZ80MCCodeEmitter::emitInstruction(uint64_t Val, unsigned Size,
                                       const MCSubtargetInfo &STI,
                                       raw_ostream &OS) const {
  const uint16_t *Words = reinterpret_cast<uint16_t const *>(&Val);
  size_t WordCount = Size / 2;

  for (int64_t i = WordCount - 1; i >= 0; --i) {
    uint16_t Word = Words[i];

    OS << (uint8_t) ((Word & 0x00ff) >> 0);
    OS << (uint8_t) ((Word & 0xff00) >> 8);
  }
}

void GBZ80MCCodeEmitter::encodeInstruction(const MCInst &MI, raw_ostream &OS,
                                         SmallVectorImpl<MCFixup> &Fixups,
                                         const MCSubtargetInfo &STI) const {
  const MCInstrDesc &Desc = MCII.get(MI.getOpcode());

  // Get byte count of instruction
  unsigned Size = Desc.getSize();

  assert(Size > 0 && "Instruction size cannot be zero");

  uint64_t BinaryOpCode = getBinaryCodeForInstr(MI, Fixups, STI);
  emitInstruction(BinaryOpCode, Size, STI, OS);
}

MCCodeEmitter *createGBZ80MCCodeEmitter(const MCInstrInfo &MCII,
                                      const MCRegisterInfo &MRI,
                                      MCContext &Ctx) {
  return new GBZ80MCCodeEmitter(MCII, Ctx);
}

#include "GBZ80GenMCCodeEmitter.inc"

} // end of namespace llvm

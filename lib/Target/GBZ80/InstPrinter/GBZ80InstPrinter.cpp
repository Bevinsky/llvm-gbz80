//===-- GBZ80InstPrinter.cpp - Convert GBZ80 MCInst to assembly syntax --------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This class prints an GBZ80 MCInst to a .s file.
//
//===----------------------------------------------------------------------===//

#include "GBZ80InstPrinter.h"
#include "GBZ80InstrInfo.h"

#include "MCTargetDesc/GBZ80MCTargetDesc.h"

#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstrDesc.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FormattedStream.h"

#include <cstring>

#define DEBUG_TYPE "asm-printer"

namespace llvm {

// Include the auto-generated portion of the assembly writer.
#define PRINT_ALIAS_INSTR
#include "GBZ80GenAsmWriter.inc"

void GBZ80InstPrinter::printInst(const MCInst *MI, raw_ostream &O,
                               StringRef Annot, const MCSubtargetInfo &STI) {
  unsigned Opcode = MI->getOpcode();

  switch (Opcode) {
  default:
    if (!printAliasInstr(MI, O))
      printInstruction(MI, O);

    printAnnotation(O, Annot);
    break;
  }
}

void GBZ80InstPrinter::printCondCode(const MCInst *MI, unsigned OpNo,
  raw_ostream &O) {
  switch ((GBCC::CondCodes)MI->getOperand(OpNo).getImm()) {
  default:
    llvm_unreachable("Invalid condcode!");
  case GBCC::COND_Z:
    O << "Z";
    break;
  case GBCC::COND_NZ:
    O << "NZ";
    break;
  case GBCC::COND_C:
    O << "C";
    break;
  case GBCC::COND_NC:
    O << "NC";
    break;
  }
}

void GBZ80InstPrinter::printOperand(const MCInst *MI, unsigned OpNo,
                                  raw_ostream &O) {
  const MCOperand &Op = MI->getOperand(OpNo);
  const MCOperandInfo &MOI = this->MII.get(MI->getOpcode()).OpInfo[OpNo];

  if (Op.isReg()) {
    O << getRegisterName(Op.getReg());
  } else if (Op.isImm()) {
    O << Op.getImm();
  } else {
    assert(Op.isExpr() && "Unknown operand kind in printOperand");
    O << *Op.getExpr();
  }
}

const char *GBZ80InstPrinter::getPrettyRegisterName(unsigned RegNo) {
  return getRegisterName(RegNo);
}

/// This is used to print an immediate value that ends up
/// being encoded as a pc-relative value.
void GBZ80InstPrinter::printPCRelImm(const MCInst *MI, unsigned OpNo,
                                   raw_ostream &O) {
  const MCOperand &Op = MI->getOperand(OpNo);

  if (Op.isImm()) {
    int64_t Imm = Op.getImm();
    // Print a position sign if needed.
    // Negative values have their sign printed automatically.
    if (Imm >= 0)
      O << '+';
    O << Imm;
  } else {
    assert(Op.isExpr() && "Unknown pcrel immediate operand");
    O << *Op.getExpr();
  }
}
#if 0
void GBZ80InstPrinter::printMemri(const MCInst *MI, unsigned OpNo,
                                raw_ostream &O) {
  assert(MI->getOperand(OpNo).isReg() && "Expected a register for the first operand");

  const MCOperand &OffsetOp = MI->getOperand(OpNo + 1);

  // Print the register.
  printOperand(MI, OpNo, O);

  // Print the {+,-}offset.
  if (OffsetOp.isImm()) {
    int64_t Offset = OffsetOp.getImm();

    if (Offset >= 0)
      O << '+';

    O << Offset;
  } else if (OffsetOp.isExpr()) {
    O << *OffsetOp.getExpr();
  } else {
    llvm_unreachable("unknown type for offset");
  }
}
#endif

} // end of namespace llvm


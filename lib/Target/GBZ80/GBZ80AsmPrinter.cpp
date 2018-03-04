//===-- GBZ80AsmPrinter.cpp - GBZ80 LLVM assembly writer ----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains a printer that converts from our internal representation
// of machine-dependent LLVM code to GAS-format GBZ80 assembly language.
//
//===----------------------------------------------------------------------===//

#include "GBZ80.h"
#include "GBZ80MCInstLower.h"
#include "GBZ80Subtarget.h"
#include "InstPrinter/GBZ80InstPrinter.h"

#include "llvm/CodeGen/AsmPrinter.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/IR/Mangler.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetRegisterInfo.h"
#include "llvm/Target/TargetLoweringObjectFile.h"
#include "llvm/Target/TargetSubtargetInfo.h"

#define DEBUG_TYPE "GBZ80-asm-printer"

namespace llvm {

/// An GBZ80 assembly code printer.
class GBZ80AsmPrinter : public AsmPrinter {
public:
  GBZ80AsmPrinter(TargetMachine &TM,
                std::unique_ptr<MCStreamer> Streamer)
      : AsmPrinter(TM, std::move(Streamer)), MRI(*TM.getMCRegisterInfo()) { }

  StringRef getPassName() const override { return "GBZ80 Assembly Printer"; }

  void printOperand(const MachineInstr *MI, unsigned OpNo, raw_ostream &O,
                    const char *Modifier = 0);

  bool PrintAsmOperand(const MachineInstr *MI, unsigned OpNum,
                       unsigned AsmVariant, const char *ExtraCode,
                       raw_ostream &O) override;

  bool PrintAsmMemoryOperand(const MachineInstr *MI, unsigned OpNum,
                             unsigned AsmVariant, const char *ExtraCode,
                             raw_ostream &O) override;

  void EmitFunctionBodyStart() override;

  void EmitInstruction(const MachineInstr *MI) override;

  void EmitLinkage(const GlobalValue *GV, MCSymbol *GVSym) const;

private:
  const MCRegisterInfo &MRI;

  void EmitFunctionHeader() override;
};

void GBZ80AsmPrinter::printOperand(const MachineInstr *MI, unsigned OpNo,
                                 raw_ostream &O, const char *Modifier) {
  const MachineOperand &MO = MI->getOperand(OpNo);

  switch (MO.getType()) {
  case MachineOperand::MO_Register:
    O << GBZ80InstPrinter::getPrettyRegisterName(OpNo);
    break;
  case MachineOperand::MO_Immediate:
    O << MO.getImm();
    break;
  case MachineOperand::MO_GlobalAddress:
    O << getSymbol(MO.getGlobal());
    break;
  case MachineOperand::MO_ExternalSymbol:
    O << *GetExternalSymbolSymbol(MO.getSymbolName());
    break;
  case MachineOperand::MO_MachineBasicBlock:
    O << *MO.getMBB()->getSymbol();
    break;
  default:
    llvm_unreachable("Not implemented yet!");
  }
}

bool GBZ80AsmPrinter::PrintAsmOperand(const MachineInstr *MI, unsigned OpNum,
                                    unsigned AsmVariant, const char *ExtraCode,
                                    raw_ostream &O) {
  // Default asm printer can only deal with some extra codes,
  // so try it first.
  bool Error = AsmPrinter::PrintAsmOperand(MI, OpNum, AsmVariant, ExtraCode, O);
  if (Error)
    printOperand(MI, OpNum, O);

  return false;
}

bool GBZ80AsmPrinter::PrintAsmMemoryOperand(const MachineInstr *MI,
                                          unsigned OpNum, unsigned AsmVariant,
                                          const char *ExtraCode,
                                          raw_ostream &O) {
#if 0
  if (ExtraCode && ExtraCode[0]) {
    llvm_unreachable("This branch is not implemented yet");
  }

  const MachineOperand &MO = MI->getOperand(OpNum);
  (void)MO;
  assert(MO.isReg() && "Unexpected inline asm memory operand");

  // TODO: We should be able to look up the alternative name for
  // the register if it's given.
  // TableGen doesn't expose a way of getting retrieving names
  // for registers.
  if (MI->getOperand(OpNum).getReg() == GB::R31R30) {
    O << "Z";
  } else {
    assert(MI->getOperand(OpNum).getReg() == GB::R29R28 &&
           "Wrong register class for memory operand.");
    O << "Y";
  }

  // If NumOpRegs == 2, then we assume it is product of a FrameIndex expansion
  // and the second operand is an Imm.
  unsigned OpFlags = MI->getOperand(OpNum - 1).getImm();
  unsigned NumOpRegs = InlineAsm::getNumOperandRegisters(OpFlags);

  if (NumOpRegs == 2) {
    O << '+' << MI->getOperand(OpNum + 1).getImm();
  }
#endif
  return false;
}

void GBZ80AsmPrinter::EmitFunctionBodyStart() {
  const MachineRegisterInfo &MRI = MF->getRegInfo();

  SmallString<128> C;
  raw_svector_ostream O(C);
  O << " ===== Function " << MF->getName() << " =====";
  OutStreamer->emitRawComment(C, false);
  C.clear();
  O << " Arguments: ";
  if (!MRI.livein_empty()) {
    for (MachineRegisterInfo::livein_iterator
      I = MRI.livein_begin(), E = MRI.livein_end(); I != E; ++I) {
      O << PrintReg(I->first, MRI.getTargetRegisterInfo());
      if (std::next(I) != E)
        O << ", ";
    }
  }
  OutStreamer->emitRawComment(C, false);
  C.clear();
  O << " Returns:   TODO";
  OutStreamer->emitRawComment(C, false);
  C.clear();
  O << " =====================";
  for (unsigned i = 0; i < MF->getName().size(); i++)
    O << "=";
  OutStreamer->emitRawComment(C, false);
}

void GBZ80AsmPrinter::EmitInstruction(const MachineInstr *MI) {
  GBZ80MCInstLower MCInstLowering(OutContext, *this);

  MCInst I;
  MCInstLowering.lowerInstruction(*MI, I);
  EmitToStreamer(*OutStreamer, I);
}

void GBZ80AsmPrinter::EmitLinkage(const GlobalValue *GV,
    MCSymbol *GVSym) const {
  GlobalValue::LinkageTypes Linkage = GV->getLinkage();
  switch (Linkage) {
  case GlobalValue::ExternalLinkage:
    OutStreamer->EmitSymbolAttribute(GVSym, MCSA_Global);
    break;
  case GlobalValue::PrivateLinkage:
  case GlobalValue::InternalLinkage:
    // Emit nothing for private and internal.
    break;
  case GlobalValue::CommonLinkage:
  case GlobalValue::LinkOnceAnyLinkage:
  case GlobalValue::LinkOnceODRLinkage:
  case GlobalValue::WeakAnyLinkage:
  case GlobalValue::WeakODRLinkage:
    llvm_unreachable("Unsupported linkage types!");
  case GlobalValue::AppendingLinkage:
  case GlobalValue::AvailableExternallyLinkage:
  case GlobalValue::ExternalWeakLinkage:
    llvm_unreachable("Should never emit this");
  }
}

void GBZ80AsmPrinter::EmitFunctionHeader() {
  const Function *F = MF->getFunction();

  if (isVerbose())
    OutStreamer->GetCommentOS()
    << "-- Begin function "
    << GlobalValue::dropLLVMManglingEscape(F->getName()) << '\n';

  // Print out constants referenced by the function
  EmitConstantPool();

  // Print the 'header' of function.
  OutStreamer->SwitchSection(getObjFileLowering().SectionForGlobal(F, TM));

  EmitLinkage(F, CurrentFnSym);

  if (isVerbose()) {
    F->printAsOperand(OutStreamer->GetCommentOS(),
      /*PrintType=*/false, F->getParent());
    OutStreamer->GetCommentOS() << '\n';
  }

  // Emit the prefix data.
  if (F->hasPrefixData()) {
    if (MAI->hasSubsectionsViaSymbols()) {
      // Preserving prefix data on platforms which use subsections-via-symbols
      // is a bit tricky. Here we introduce a symbol for the prefix data
      // and use the .alt_entry attribute to mark the function's real entry point
      // as an alternative entry point to the prefix-data symbol.
      MCSymbol *PrefixSym = OutContext.createLinkerPrivateTempSymbol();
      OutStreamer->EmitLabel(PrefixSym);

      EmitGlobalConstant(F->getParent()->getDataLayout(), F->getPrefixData());

      // Emit an .alt_entry directive for the actual function symbol.
      OutStreamer->EmitSymbolAttribute(CurrentFnSym, MCSA_AltEntry);
    } else {
      EmitGlobalConstant(F->getParent()->getDataLayout(), F->getPrefixData());
    }
  }

  // Emit the CurrentFnSym.  This is a virtual function to allow targets to
  // do their wild and crazy things as required.
  EmitFunctionEntryLabel();

  // If the function had address-taken blocks that got deleted, then we have
  // references to the dangling symbols.  Emit them at the start of the function
  // so that we don't get references to undefined symbols.
  std::vector<MCSymbol*> DeadBlockSyms;
  MMI->takeDeletedSymbolsForFunction(F, DeadBlockSyms);
  for (unsigned i = 0, e = DeadBlockSyms.size(); i != e; ++i) {
    OutStreamer->AddComment("Address taken block that was later removed");
    OutStreamer->EmitLabel(DeadBlockSyms[i]);
  }

  // Emit the prologue data.
  if (F->hasPrologueData())
    EmitGlobalConstant(F->getParent()->getDataLayout(), F->getPrologueData());
}

} // end of namespace llvm

extern "C" void LLVMInitializeGBZ80AsmPrinter() {
  llvm::RegisterAsmPrinter<llvm::GBZ80AsmPrinter> X(llvm::getTheGBZ80Target());
}


//===-- GBZ80RelaxMemOperations.cpp - Relax out of range loads/stores -------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains a pass which relaxes out of range memory operations into
// equivalent operations which handle bigger addresses.
//
//===----------------------------------------------------------------------===//

#include "GBZ80.h"
#include "GBZ80InstrInfo.h"
#include "GBZ80TargetMachine.h"
#include "MCTargetDesc/GBZ80MCTargetDesc.h"

#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/Target/TargetRegisterInfo.h"

using namespace llvm;

#define GBZ80_RELAX_MEM_OPS_NAME "GBZ80 memory operation relaxation pass"

namespace {

class GBZ80RelaxMem : public MachineFunctionPass {
public:
  static char ID;

  GBZ80RelaxMem() : MachineFunctionPass(ID) {
    initializeGBZ80RelaxMemPass(*PassRegistry::getPassRegistry());
  }

  bool runOnMachineFunction(MachineFunction &MF) override;

  StringRef getPassName() const override { return GBZ80_RELAX_MEM_OPS_NAME; }

private:
  typedef MachineBasicBlock Block;
  typedef Block::iterator BlockIt;

  const TargetInstrInfo *TII;

  template <unsigned OP> bool relax(Block &MBB, BlockIt MBBI);

  bool runOnBasicBlock(Block &MBB);
  bool runOnInstruction(Block &MBB, BlockIt MBBI);

  MachineInstrBuilder buildMI(Block &MBB, BlockIt MBBI, unsigned Opcode) {
    return BuildMI(MBB, MBBI, MBBI->getDebugLoc(), TII->get(Opcode));
  }
};

char GBZ80RelaxMem::ID = 0;

bool GBZ80RelaxMem::runOnMachineFunction(MachineFunction &MF) {
  bool Modified = false;

  const GBZ80Subtarget &STI = MF.getSubtarget<GBZ80Subtarget>();
  TII = STI.getInstrInfo();

  for (Block &MBB : MF) {
    bool BlockModified = runOnBasicBlock(MBB);
    Modified |= BlockModified;
  }

  return Modified;
}

bool GBZ80RelaxMem::runOnBasicBlock(Block &MBB) {
  bool Modified = false;

  BlockIt MBBI = MBB.begin(), E = MBB.end();
  while (MBBI != E) {
    BlockIt NMBBI = std::next(MBBI);
    Modified |= runOnInstruction(MBB, MBBI);
    MBBI = NMBBI;
  }

  return Modified;
}
#if 0
template <>
bool GBZ80RelaxMem::relax<GB::STDWPtrQRr>(Block &MBB, BlockIt MBBI) {
  MachineInstr &MI = *MBBI;

  MachineOperand &Ptr = MI.getOperand(0);
  MachineOperand &Src = MI.getOperand(2);
  int64_t Imm = MI.getOperand(1).getImm();

  // We can definitely optimise this better.
  if (Imm > 63) {
    // Push the previous state of the pointer register.
    // This instruction must preserve the value.
    buildMI(MBB, MBBI, GB::PUSHWRr)
      .addReg(Ptr.getReg());

    // Add the immediate to the pointer register.
    buildMI(MBB, MBBI, GB::SBCIWRdK)
      .addReg(Ptr.getReg(), RegState::Define)
      .addReg(Ptr.getReg())
      .addImm(-Imm);

    // Store the value in the source register to the address
    // pointed to by the pointer register.
    buildMI(MBB, MBBI, GB::STWPtrRr)
      .addReg(Ptr.getReg())
      .addReg(Src.getReg(), getKillRegState(Src.isKill()));

    // Pop the original state of the pointer register.
    buildMI(MBB, MBBI, GB::POPWRd)
      .addReg(Ptr.getReg(), getKillRegState(Ptr.isKill()));

    MI.removeFromParent();
  }

  return false;
}
#endif
bool GBZ80RelaxMem::runOnInstruction(Block &MBB, BlockIt MBBI) {
  MachineInstr &MI = *MBBI;
  int Opcode = MBBI->getOpcode();

#define RELAX(Op)                \
  case Op:                       \
    return relax<Op>(MBB, MI)

  switch (Opcode) {
    /*RELAX(GB::STDWPtrQRr);*/
  }
#undef RELAX
  return false;
}

} // end of anonymous namespace

INITIALIZE_PASS(GBZ80RelaxMem, "GBZ80-relax-mem",
                GBZ80_RELAX_MEM_OPS_NAME, false, false)

namespace llvm {

FunctionPass *createGBZ80RelaxMemPass() { return new GBZ80RelaxMem(); }

} // end of namespace llvm

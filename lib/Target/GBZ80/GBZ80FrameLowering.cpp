//===-- GBZ80FrameLowering.cpp - GBZ80 Frame Information ----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the GBZ80 implementation of TargetFrameLowering class.
//
//===----------------------------------------------------------------------===//

#include "GBZ80FrameLowering.h"

#include "GBZ80.h"
#include "GBZ80InstrInfo.h"
#include "GBZ80MachineFunctionInfo.h"
#include "GBZ80TargetMachine.h"
#include "MCTargetDesc/GBZ80MCTargetDesc.h"

#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/IR/Function.h"

#include <vector>

namespace llvm {

GBZ80FrameLowering::GBZ80FrameLowering()
    : TargetFrameLowering(TargetFrameLowering::StackGrowsDown, 1, 0, 1, false) {}

bool GBZ80FrameLowering::canSimplifyCallFramePseudos(
    const MachineFunction &MF) const {
  // We cannot simplify, because we do not have a frame pointer and the
  // stack pointer might change between the call pseudos.
  // ...although, if this isn't true, PEI won't actually remove the pseudos.
  return true;
}

bool GBZ80FrameLowering::noFramePointerElim(const MachineFunction &MF) const {
  return false;
}

bool GBZ80FrameLowering::hasReservedCallFrame(const MachineFunction &MF) const {
  // I think this should be false?
  return false;
}

void GBZ80FrameLowering::emitPrologue(MachineFunction &MF,
                                    MachineBasicBlock &MBB) const {
  MachineBasicBlock::iterator MBBI = MBB.begin();
  CallingConv::ID CallConv = MF.getFunction()->getCallingConv();
  DebugLoc DL = (MBBI != MBB.end()) ? MBBI->getDebugLoc() : DebugLoc();
  const GBZ80Subtarget &STI = MF.getSubtarget<GBZ80Subtarget>();
  const GBZ80InstrInfo &TII = *STI.getInstrInfo();
  bool HasFP = hasFP(MF);

  const MachineFrameInfo &MFI = MF.getFrameInfo();
  const GBZ80MachineFunctionInfo *AFI = MF.getInfo<GBZ80MachineFunctionInfo>();
  unsigned FrameSize = MFI.getStackSize() - AFI->getCalleeSavedFrameSize();

  // Skip the callee-saved push instructions.
  while (
      (MBBI != MBB.end()) && MBBI->getFlag(MachineInstr::FrameSetup) &&
      (MBBI->getOpcode() == GB::PUSH)) {
    ++MBBI;
  }

  // No need to mark the stack pointer as live in, since it's reserved.

  if (!FrameSize) {
    return;
  }

  assert(FrameSize <= 127 && "Cannot handle frames larger than 127 bytes!");

  // Reserve the necessary frame memory by doing SP + -Size
  MachineInstr *MI = BuildMI(MBB, MBBI, DL, TII.get(GB::ADD_SP_e))
                         .addImm(-FrameSize)
                         .setMIFlag(MachineInstr::FrameSetup);
  // The flag operand is dead.
  MI->findRegisterDefOperand(GB::rF)->setIsDead();
}

void GBZ80FrameLowering::emitEpilogue(MachineFunction &MF,
                                    MachineBasicBlock &MBB) const {
  CallingConv::ID CallConv = MF.getFunction()->getCallingConv();
 
  MachineBasicBlock::iterator MBBI = MBB.getLastNonDebugInstr();
  assert(MBBI->getDesc().isReturn() &&
         "Can only insert epilog into returning blocks");

  DebugLoc DL = MBBI->getDebugLoc();
  const MachineFrameInfo &MFI = MF.getFrameInfo();
  const GBZ80MachineFunctionInfo *AFI = MF.getInfo<GBZ80MachineFunctionInfo>();
  unsigned FrameSize = MFI.getStackSize() - AFI->getCalleeSavedFrameSize();
  const GBZ80Subtarget &STI = MF.getSubtarget<GBZ80Subtarget>();
  const GBZ80InstrInfo &TII = *STI.getInstrInfo();

  // Early exit if there is no need to adjust the stack pointer.
  if (!FrameSize) {
    return;
  }

  // Skip the callee-saved pop instructions.
  while (MBBI != MBB.begin()) {
    MachineBasicBlock::iterator PI = std::prev(MBBI);
    int Opc = PI->getOpcode();

    if (Opc != GB::POP && !PI->isTerminator()) {
      break;
    }

    --MBBI;
  }

  // XXX: This sort of code does not work with conditional return.

  // Restore the frame pointer by doing FP += <size>.
  MachineInstr *MI = BuildMI(MBB, MBBI, DL, TII.get(GB::ADD_SP_e))
                         .addImm(FrameSize);
  // The flag operand is dead.
  MI->findRegisterDefOperand(GB::rF)->setIsDead();
}

// Our frame pointer is the stack pointer, so we do not have a frame pointer.
bool GBZ80FrameLowering::hasFP(const MachineFunction &MF) const {
  return false;
}

bool GBZ80FrameLowering::spillCalleeSavedRegisters(
    MachineBasicBlock &MBB, MachineBasicBlock::iterator MI,
    const std::vector<CalleeSavedInfo> &CSI,
    const TargetRegisterInfo *TRI) const {
  if (CSI.empty()) {
    return false;
  }

  unsigned CalleeFrameSize = 0;
  DebugLoc DL = MBB.findDebugLoc(MI);
  MachineFunction &MF = *MBB.getParent();
  const GBZ80Subtarget &STI = MF.getSubtarget<GBZ80Subtarget>();
  const TargetInstrInfo &TII = *STI.getInstrInfo();
  GBZ80MachineFunctionInfo *GBZ80FI = MF.getInfo<GBZ80MachineFunctionInfo>();

  for (const CalleeSavedInfo &CCSI : reverse(CSI)) {
    unsigned Reg = CCSI.getReg();
    bool IsNotLiveIn = !MBB.isLiveIn(Reg);

    assert(TRI->getRegSizeInBits(*TRI->getMinimalPhysRegClass(Reg)) == 16 &&
           "Invalid register size");

    // Add the callee-saved register as live-in only if it is not already a
    // live-in register, this usually happens with arguments that are passed
    // through callee-saved registers.
    if (IsNotLiveIn) {
      MBB.addLiveIn(Reg);
    }

    // Do not kill the register when it is an input argument.
    BuildMI(MBB, MI, DL, TII.get(GB::PUSH))
        .addReg(Reg, getKillRegState(IsNotLiveIn))
        .setMIFlag(MachineInstr::FrameSetup);
    CalleeFrameSize += 2;
  }

  GBZ80FI->setCalleeSavedFrameSize(CalleeFrameSize);

  return true;
}

bool GBZ80FrameLowering::restoreCalleeSavedRegisters(
    MachineBasicBlock &MBB, MachineBasicBlock::iterator MI,
    const std::vector<CalleeSavedInfo> &CSI,
    const TargetRegisterInfo *TRI) const {
  if (CSI.empty()) {
    return false;
  }

  DebugLoc DL = MBB.findDebugLoc(MI);
  const MachineFunction &MF = *MBB.getParent();
  const GBZ80Subtarget &STI = MF.getSubtarget<GBZ80Subtarget>();
  const TargetInstrInfo &TII = *STI.getInstrInfo();

  for (const CalleeSavedInfo &CCSI : CSI) {
    unsigned Reg = CCSI.getReg();

    assert(TRI->getRegSizeInBits(*TRI->getMinimalPhysRegClass(Reg)) == 16 &&
           "Invalid register size");

    BuildMI(MBB, MI, DL, TII.get(GB::POP), Reg);
  }

  return true;
}

/// Replace pseudo store instructions that pass arguments through the stack with
/// real instructions. If insertPushes is true then all instructions are
/// replaced with push instructions, otherwise regular std instructions are
/// inserted.
static void fixStackStores(MachineBasicBlock &MBB,
                           MachineBasicBlock::iterator MI,
                           const TargetInstrInfo &TII, bool insertPushes) {
  const GBZ80Subtarget &STI = MBB.getParent()->getSubtarget<GBZ80Subtarget>();
  const TargetRegisterInfo &TRI = *STI.getRegisterInfo();


#if 0
  // Iterate through the BB until we hit a call instruction or we reach the end.
  for (auto I = MI, E = MBB.end(); I != E && !I->isCall();) {
    MachineBasicBlock::iterator NextMI = std::next(I);
    MachineInstr &MI = *I;
    unsigned Opcode = I->getOpcode();

    // Only care of pseudo store instructions where SP is the base pointer.
    if (Opcode != GB::STDSPQRr && Opcode != GB::STDWSPQRr) {
      I = NextMI;
      continue;
    }

    assert(MI.getOperand(0).getReg() == GB::SP &&
           "Invalid register, should be SP!");
    if (insertPushes) {
      // Replace this instruction with a push.
      unsigned SrcReg = MI.getOperand(2).getReg();
      bool SrcIsKill = MI.getOperand(2).isKill();

      // We can't use PUSHWRr here because when expanded the order of the new
      // instructions are reversed from what we need. Perform the expansion now.
      if (Opcode == GB::STDWSPQRr) {
        BuildMI(MBB, I, MI.getDebugLoc(), TII.get(GB::PUSHRr))
            .addReg(TRI.getSubReg(SrcReg, GB::sub_hi),
                    getKillRegState(SrcIsKill));
        BuildMI(MBB, I, MI.getDebugLoc(), TII.get(GB::PUSHRr))
            .addReg(TRI.getSubReg(SrcReg, GB::sub_lo),
                    getKillRegState(SrcIsKill));
      } else {
        BuildMI(MBB, I, MI.getDebugLoc(), TII.get(GB::PUSHRr))
            .addReg(SrcReg, getKillRegState(SrcIsKill));
      }

      MI.eraseFromParent();
      I = NextMI;
      continue;
    }

    // Replace this instruction with a regular store. Use Y as the base
    // pointer since it is guaranteed to contain a copy of SP.
    unsigned STOpc =
        (Opcode == GB::STDWSPQRr) ? GB::STDWPtrQRr : GB::STDPtrQRr;

    MI.setDesc(TII.get(STOpc));
    MI.getOperand(0).setReg(GB::R29R28);

    I = NextMI;
  }
#endif
}

MachineBasicBlock::iterator GBZ80FrameLowering::eliminateCallFramePseudoInstr(
    MachineFunction &MF, MachineBasicBlock &MBB,
    MachineBasicBlock::iterator MI) const {
  const GBZ80Subtarget &STI = MF.getSubtarget<GBZ80Subtarget>();
  const TargetFrameLowering &TFI = *STI.getFrameLowering();
  const GBZ80InstrInfo &TII = *STI.getInstrInfo();

  DebugLoc DL = MI->getDebugLoc();
  unsigned int Opcode = MI->getOpcode();
  int Amount = TII.getFrameSize(*MI);
  assert(Amount == 0 && "call frame with stack not yet implemented!");
#if 0
  // Adjcallstackup does not need to allocate stack space for the call, instead
  // we insert push instructions that will allocate the necessary stack.
  // For adjcallstackdown we convert it into an 'adiw reg, <amt>' handling
  // the read and write of SP in I/O space.
  if (Amount != 0) {
    assert(TFI.getStackAlignment() == 1 && "Unsupported stack alignment");

    if (Opcode == TII.getCallFrameSetupOpcode()) {
      fixStackStores(MBB, MI, TII, true);
    } else {
      assert(Opcode == TII.getCallFrameDestroyOpcode());

      // Select the best opcode to adjust SP based on the offset size.
      unsigned addOpcode;
      if (isUInt<6>(Amount)) {
        addOpcode = GB::ADIWRdK;
      } else {
        addOpcode = GB::SUBIWRdK;
        Amount = -Amount;
      }

      // Build the instruction sequence.
      BuildMI(MBB, MI, DL, TII.get(GB::SPREAD), GB::R31R30).addReg(GB::SP);

      MachineInstr *New = BuildMI(MBB, MI, DL, TII.get(addOpcode), GB::R31R30)
                              .addReg(GB::R31R30, RegState::Kill)
                              .addImm(Amount);
      New->getOperand(3).setIsDead();

      BuildMI(MBB, MI, DL, TII.get(GB::SPWRITE), GB::SP)
          .addReg(GB::R31R30, RegState::Kill);
    }
  }
#endif
  return MBB.erase(MI);
}

void GBZ80FrameLowering::determineCalleeSaves(MachineFunction &MF,
                                            BitVector &SavedRegs,
                                            RegScavenger *RS) const {
  TargetFrameLowering::determineCalleeSaves(MF, SavedRegs, RS);
}

// We probably don't need this stuff.
#if 0

/// The frame analyzer pass.
///
/// Scans the function for allocas and used arguments
/// that are passed through the stack.
struct GBZ80FrameAnalyzer : public MachineFunctionPass {
  static char ID;
  GBZ80FrameAnalyzer() : MachineFunctionPass(ID) {}

  bool runOnMachineFunction(MachineFunction &MF) {
    const MachineFrameInfo &MFI = MF.getFrameInfo();
    GBZ80MachineFunctionInfo *FuncInfo = MF.getInfo<GBZ80MachineFunctionInfo>();

    // If there are no fixed frame indexes during this stage it means there
    // are allocas present in the function.
    if (MFI.getNumObjects() != MFI.getNumFixedObjects()) {
      // Check for the type of allocas present in the function. We only care
      // about fixed size allocas so do not give false positives if only
      // variable sized allocas are present.
      for (unsigned i = 0, e = MFI.getObjectIndexEnd(); i != e; ++i) {
        // Variable sized objects have size 0.
        if (MFI.getObjectSize(i)) {
          FuncInfo->setHasAllocas(true);
          break;
        }
      }
    }

    // If there are fixed frame indexes present, scan the function to see if
    // they are really being used.
    if (MFI.getNumFixedObjects() == 0) {
      return false;
    }

    // Ok fixed frame indexes present, now scan the function to see if they
    // are really being used, otherwise we can ignore them.
    for (const MachineBasicBlock &BB : MF) {
      for (const MachineInstr &MI : BB) {
        int Opcode = MI.getOpcode();

        if ((Opcode != GB::LDDRdPtrQ) && (Opcode != GB::LDDWRdPtrQ) &&
            (Opcode != GB::STDPtrQRr) && (Opcode != GB::STDWPtrQRr)) {
          continue;
        }

        for (const MachineOperand &MO : MI.operands()) {
          if (!MO.isFI()) {
            continue;
          }

          if (MFI.isFixedObjectIndex(MO.getIndex())) {
            FuncInfo->setHasStackArgs(true);
            return false;
          }
        }
      }
    }

    return false;
  }

  StringRef getPassName() const { return "GBZ80 Frame Analyzer"; }
};

char GBZ80FrameAnalyzer::ID = 0;

/// Creates instance of the frame analyzer pass.
FunctionPass *createGBZ80FrameAnalyzerPass() { return new GBZ80FrameAnalyzer(); }

/// Create the Dynalloca Stack Pointer Save/Restore pass.
/// Insert a copy of SP before allocating the dynamic stack memory and restore
/// it in function exit to restore the original SP state. This avoids the need
/// of reserving a register pair for a frame pointer.
struct GBZ80DynAllocaSR : public MachineFunctionPass {
  static char ID;
  GBZ80DynAllocaSR() : MachineFunctionPass(ID) {}

  bool runOnMachineFunction(MachineFunction &MF) {
    // Early exit when there are no variable sized objects in the function.
    if (!MF.getFrameInfo().hasVarSizedObjects()) {
      return false;
    }

    const GBZ80Subtarget &STI = MF.getSubtarget<GBZ80Subtarget>();
    const TargetInstrInfo &TII = *STI.getInstrInfo();
    MachineBasicBlock &EntryMBB = MF.front();
    MachineBasicBlock::iterator MBBI = EntryMBB.begin();
    DebugLoc DL = EntryMBB.findDebugLoc(MBBI);

    unsigned SPCopy =
        MF.getRegInfo().createVirtualRegister(&GB::DREGSRegClass);

    // Create a copy of SP in function entry before any dynallocas are
    // inserted.
    BuildMI(EntryMBB, MBBI, DL, TII.get(GB::COPY), SPCopy).addReg(GB::SP);

    // Restore SP in all exit basic blocks.
    for (MachineBasicBlock &MBB : MF) {
      // If last instruction is a return instruction, add a restore copy.
      if (!MBB.empty() && MBB.back().isReturn()) {
        MBBI = MBB.getLastNonDebugInstr();
        DL = MBBI->getDebugLoc();
        BuildMI(MBB, MBBI, DL, TII.get(GB::COPY), GB::SP)
            .addReg(SPCopy, RegState::Kill);
      }
    }

    return true;
  }

  StringRef getPassName() const {
    return "GBZ80 dynalloca stack pointer save/restore";
  }
};

char GBZ80DynAllocaSR::ID = 0;

/// createGBZ80DynAllocaSRPass - returns an instance of the dynalloca stack
/// pointer save/restore pass.
FunctionPass *createGBZ80DynAllocaSRPass() { return new GBZ80DynAllocaSR(); }
#endif
} // end of namespace llvm


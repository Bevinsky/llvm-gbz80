//===-- GBZ80InstrInfo.cpp - GBZ80 Instruction Information --------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the GBZ80 implementation of the TargetInstrInfo class.
//
//===----------------------------------------------------------------------===//

#include "GBZ80InstrInfo.h"

#include "llvm/ADT/STLExtras.h"
#include "llvm/CodeGen/MachineConstantPool.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineMemOperand.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/MC/MCContext.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/TargetRegistry.h"

#include "GBZ80.h"
#include "GBZ80MachineFunctionInfo.h"
#include "GBZ80RegisterInfo.h"
#include "GBZ80TargetMachine.h"
#include "MCTargetDesc/GBZ80MCTargetDesc.h"

#define GET_INSTRINFO_CTOR_DTOR
#include "GBZ80GenInstrInfo.inc"

namespace llvm {

GBZ80InstrInfo::GBZ80InstrInfo()
    : GBZ80GenInstrInfo(GB::ADJCALLSTACKDOWN, GB::ADJCALLSTACKUP), RI() {}

void GBZ80InstrInfo::copyPhysReg(MachineBasicBlock &MBB,
                               MachineBasicBlock::iterator MI,
                               const DebugLoc &DL, unsigned DestReg,
                               unsigned SrcReg, bool KillSrc) const {
  const GBZ80Subtarget &STI = MBB.getParent()->getSubtarget<GBZ80Subtarget>();
  const GBZ80RegisterInfo &TRI = *STI.getRegisterInfo();
  unsigned Opc;

  if (GB::R16RegClass.contains(DestReg, SrcReg)) {
    // 16-to-16 copy. Use two reg-LDs.
    unsigned DestLo, DestHi, SrcLo, SrcHi;

    TRI.splitReg(DestReg, DestLo, DestHi);
    TRI.splitReg(SrcReg, SrcLo, SrcHi);

    BuildMI(MBB, MI, DL, get(GB::LD_r_r), DestLo)
      .addReg(SrcLo, getKillRegState(KillSrc));
    BuildMI(MBB, MI, DL, get(GB::LD_r_r), DestHi)
      .addReg(SrcHi, getKillRegState(KillSrc));
  }
  else if (GB::R8RegClass.contains(DestReg, SrcReg)) {
    BuildMI(MBB, MI, DL, get(GB::LD_r_r), DestReg)
      .addReg(SrcReg, getKillRegState(KillSrc));
  }
  else if (DestReg == GB::SP && SrcReg == GB::RHL) {
    BuildMI(MBB, MI, DL, get(GB::LD_SP_HL), DestReg)
      .addReg(SrcReg, getKillRegState(KillSrc));
  }
  // TODO: Others?
  else {
    llvm_unreachable("Impossible reg-to-reg copy");
  }
}

unsigned GBZ80InstrInfo::isLoadFromStackSlot(const MachineInstr &MI,
                                           int &FrameIndex) const {
  // TODO: add the 16-bit one too
  switch (MI.getOpcode()) {
  case GB::FRMIDX_Load8:
  /*case GB::FRMIDX_Load16:*/ {
    if (MI.getOperand(1).isFI()) {
      // XXX: this might break if 'side effects' includes clobbering.
      FrameIndex = MI.getOperand(1).getIndex();
      return MI.getOperand(0).getReg();
    }
    break;
  }
  default:
    break;
  }

  return 0;
}

unsigned GBZ80InstrInfo::isStoreToStackSlot(const MachineInstr &MI,
                                          int &FrameIndex) const {
  switch (MI.getOpcode()) {
  case GB::FRMIDX_Store8:
    /*case GB::FRMIDX_Load16:*/ {
    if (MI.getOperand(1).isFI()) {
      // XXX: this might break if 'side effects' includes clobbering.
      FrameIndex = MI.getOperand(0).getIndex();
      return MI.getOperand(1).getReg();
    }
    break;
  }
  default:
    break;
  }

  return 0;
}

void GBZ80InstrInfo::storeRegToStackSlot(MachineBasicBlock &MBB,
                                       MachineBasicBlock::iterator MI,
                                       unsigned SrcReg, bool isKill,
                                       int FrameIndex,
                                       const TargetRegisterClass *RC,
                                       const TargetRegisterInfo *TRI) const {
  MachineFunction &MF = *MBB.getParent();
  GBZ80MachineFunctionInfo *AFI = MF.getInfo<GBZ80MachineFunctionInfo>();

  AFI->setHasSpills(true);

  DebugLoc DL;
  if (MI != MBB.end()) {
    DL = MI->getDebugLoc();
  }

  const MachineFrameInfo &MFI = MF.getFrameInfo();

  MachineMemOperand *MMO = MF.getMachineMemOperand(
      MachinePointerInfo::getFixedStack(MF, FrameIndex),
      MachineMemOperand::MOStore, MFI.getObjectSize(FrameIndex),
      MFI.getObjectAlignment(FrameIndex));

  unsigned Opcode = 0;
  if (TRI->isTypeLegalForClass(*RC, MVT::i8)) {
    Opcode = GB::FRMIDX_Store8;
  } else if (TRI->isTypeLegalForClass(*RC, MVT::i16)) {
    assert(false && "not implemented");
  } else {
    llvm_unreachable("Cannot store this register into a stack slot!");
  }

  BuildMI(MBB, MI, DL, get(Opcode))
      .addFrameIndex(FrameIndex)
      .addReg(SrcReg, getKillRegState(isKill))
      .addMemOperand(MMO);
}

void GBZ80InstrInfo::loadRegFromStackSlot(MachineBasicBlock &MBB,
                                        MachineBasicBlock::iterator MI,
                                        unsigned DestReg, int FrameIndex,
                                        const TargetRegisterClass *RC,
                                        const TargetRegisterInfo *TRI) const {
  DebugLoc DL;
  if (MI != MBB.end()) {
    DL = MI->getDebugLoc();
  }

  MachineFunction &MF = *MBB.getParent();
  const MachineFrameInfo &MFI = MF.getFrameInfo();

  MachineMemOperand *MMO = MF.getMachineMemOperand(
      MachinePointerInfo::getFixedStack(MF, FrameIndex),
      MachineMemOperand::MOLoad, MFI.getObjectSize(FrameIndex),
      MFI.getObjectAlignment(FrameIndex));

  unsigned Opcode = 0;
  if (TRI->isTypeLegalForClass(*RC, MVT::i8)) {
    Opcode = GB::FRMIDX_Load8;
  } else if (TRI->isTypeLegalForClass(*RC, MVT::i16)) {
    assert(false && "not implemented");
  } else {
    llvm_unreachable("Cannot load this register from a stack slot!");
  }

  BuildMI(MBB, MI, DL, get(Opcode), DestReg)
      .addFrameIndex(FrameIndex)
      .addMemOperand(MMO);
}


GBCC::CondCodes GBZ80InstrInfo::getOppositeCondition(GBCC::CondCodes CC) const {
  switch (CC) {
  default:
    llvm_unreachable("Invalid condition!");
  case GBCC::COND_C:
    return GBCC::COND_NC;
  case GBCC::COND_NC:
    return GBCC::COND_C;
  case GBCC::COND_Z:
    return GBCC::COND_NZ;
  case GBCC::COND_NZ:
    return GBCC::COND_Z;
  }
}

bool GBZ80InstrInfo::invertCondition(SmallVectorImpl<MachineOperand> &Cond) const {
  // Don't bother with more complex conditions...
  // TODO: Or?
  if (Cond.size() > 4)
    return false;
  switch (Cond.size()) {
  default:
    llvm_unreachable("Invalid condition vector!");
  case 2: {
    // Inverting a single condition simply involves reversing the CC.
    GBCC::CondCodes CC = (GBCC::CondCodes)Cond[0].getImm();
    Cond[0].setImm((int)getOppositeCondition(CC));
    break;
  }
  case 4:
    // Inverting two conditions involves inverting the first block and
    // the second CC.
    //   JR  C1, T
    //   JR  C2, T
    //    ->
    //   JR  C1, F
    //   JR !C2, T
    bool FirstBlockIsT = Cond[3].getImm();
    Cond[3].setImm(!FirstBlockIsT);
    GBCC::CondCodes CC = (GBCC::CondCodes)Cond[0].getImm();
    Cond[0].setImm((int)getOppositeCondition(CC));
    break;
  }
  return true;
}

bool GBZ80InstrInfo::swapCondition(SmallVectorImpl<MachineOperand> &Cond) const {
  if (Cond.size() > 4)
    return false;
  SmallVector<MachineOperand, 4> Copy(Cond.begin(), Cond.end());
  Cond.clear();
  CCPair CondTuple;
  if (Cond.size() == 2)
    CondTuple = std::make_tuple(
      (GBCC::CondCodes)Copy[0].getImm(),
      (bool)Copy[1].getImm(),
      GBCC::COND_INVALID,
      false);
  else if (Cond.size() == 4)
    CondTuple = std::make_tuple(
      (GBCC::CondCodes)Copy[2].getImm(),
      (bool)Copy[3].getImm(),
      (GBCC::CondCodes)Copy[0].getImm(),
      (bool)Copy[1].getImm());
  unsigned CPClass = CCPair2CPClass.at(CondTuple);
  unsigned SwappedClass = swapCPClass(CPClass);
  // We could just skip the swap if the classes are the same, but we want to
  // get the most optimal class here.
  CondTuple = CPClass2CCPairs.at(SwappedClass).at(0);

  if (std::get<2>(CondTuple) != GBCC::COND_INVALID) {
    Cond.push_back(MachineOperand::CreateImm(std::get<2>(CondTuple)));
    Cond.push_back(MachineOperand::CreateImm(std::get<3>(CondTuple)));
  }
  Cond.push_back(MachineOperand::CreateImm(std::get<0>(CondTuple)));
  Cond.push_back(MachineOperand::CreateImm(std::get<1>(CondTuple)));
  return true;
}

bool GBZ80InstrInfo::isConditionalBranch(const MachineInstr &I) const {
  // This does not include BR16 and other strange codes.
  return I.getOpcode() == GB::JP_cc_nn || I.getOpcode() == GB::JR_cc_e;
}

bool GBZ80InstrInfo::isUnconditionalBranch(const MachineInstr &I) const {
  return I.getOpcode() == GB::JP_nn || I.getOpcode() == GB::JR_e;
}

bool GBZ80InstrInfo::isReallyTriviallyReMaterializable(const MachineInstr &MI,
  AliasAnalysis *AI) const {
  switch (MI.getOpcode()) {
  default: return false;
  case GB::LD_r_n:
  case GB::LD_dd_nn: // These are always rematable.
    return true;
  }
}

void GBZ80InstrInfo::reMaterialize(MachineBasicBlock &MBB,
  MachineBasicBlock::iterator I, unsigned DestReg,
  unsigned SubIdx, const MachineInstr &Orig,
  const TargetRegisterInfo &TRI) const {
  // TODO: This will likely break if we remat things that set flags.

  MachineInstr *MI = MBB.getParent()->CloneMachineInstr(&Orig);
  MBB.insert(I, MI);

  MachineInstr &NewMI = *std::prev(I);
  NewMI.substituteRegister(Orig.getOperand(0).getReg(), DestReg, SubIdx, TRI);
}

bool safeToCommute(MachineInstr &MI, unsigned OpIdx1, unsigned OpIdx2) {
  MachineRegisterInfo &MRI = MI.getParent()->getParent()->getRegInfo();
  const TargetRegisterInfo *TRI = MRI.getTargetRegisterInfo();
  MachineBasicBlock &MBB = *MI.getParent();
  MachineInstr *LHSDef = nullptr;
  MachineInstr *RHSDef = nullptr;
  unsigned LHSReg = MI.getOperand(OpIdx1).getReg();
  unsigned RHSReg = MI.getOperand(OpIdx2).getReg();
  unsigned LHSSub = MI.getOperand(OpIdx1).getSubReg();
  unsigned RHSSub = MI.getOperand(OpIdx2).getSubReg();

  // Temporarily.
  return false;
#if 0
  // Looking for defines of LHSReg and RHSReg. Search upwards.
  MachineBasicBlock::reverse_iterator Begin = MI, End = MBB.rend();
  ++Begin;
  while (Begin != End) {
    if (!LHSDef && Begin->definesRegister(LHSReg, TRI))
      LHSDef = &*Begin;
    if (!RHSDef && Begin->definesRegister(RHSReg, TRI))
      RHSDef = &*Begin;
    if (LHSDef && RHSDef)
      break;
    Begin++;
  }

  if (!LHSDef || !RHSDef)
    return false;

  if (LHSDef->getOpcode() != GB::COPY || RHSDef->getOpcode() != GB::COPY)
    return false;

  bool LHSIsSafe = !TRI->isPhysicalRegister(LHSDef->getOperand(1).getReg()) &&
    (MRI.getRegClass(LHSDef->getOperand(1).getReg()) == &GB::R8RegClass ||
     MRI.getRegClass(LHSDef->getOperand(1).getReg())->hasSuperClassEq(&GB::R16RegClass));

  bool RHSIsSafe =
    (TRI->isPhysicalRegister(RHSDef->getOperand(1).getReg()) &&
     RHSDef->getOperand(1).getReg() == GB::rA) ||
    (!TRI->isPhysicalRegister(RHSDef->getOperand(1).getReg()) &&
     MRI.getRegClass(RHSDef->getOperand(1).getReg()) == &GB::ARegRegClass);

  return LHSIsSafe && RHSIsSafe;
  #endif
}

MachineInstr *
GBZ80InstrInfo::commuteInstructionImpl(MachineInstr &MI, bool NewMI,
                                       unsigned OpIdx1, unsigned OpIdx2) const{
  MachineRegisterInfo &MRI = MI.getParent()->getParent()->getRegInfo();

  // Commuting an arithmetic instruction where the LHS is a copy from GPR
  // and the RHS is a copy from A is beneficial.
  if (MRI.getRegClass(MI.getOperand(OpIdx2).getReg())->hasSuperClassEq(&GB::R8RegClass)) {
    if (!safeToCommute(MI, OpIdx1, OpIdx2))
      return nullptr;
  }
  return TargetInstrInfo::commuteInstructionImpl(MI, NewMI, OpIdx1, OpIdx2);
}

bool GBZ80InstrInfo::analyzeBranch(MachineBasicBlock &MBB,
                                 MachineBasicBlock *&TBB,
                                 MachineBasicBlock *&FBB,
                                 SmallVectorImpl<MachineOperand> &Cond,
                                 bool AllowModify) const {
  // Start from the bottom of the block and work up, examining the
  // terminator instructions.
  MachineBasicBlock::iterator I = MBB.end();
  MachineBasicBlock::iterator UnCondBrIter = MBB.end();

  while (I != MBB.begin()) {
    --I;
    if (I->isDebugValue()) {
      continue;
    }

    // Working from the bottom, when we see a non-terminator
    // instruction, we're done.
    if (!isUnpredicatedTerminator(*I)) {
      break;
    }

    // A terminator that isn't a branch can't easily be handled
    // by this analysis.
    if (!isUnconditionalBranch(*I) && !isConditionalBranch(*I)) {
      return true;
    }

    // Handle unconditional branches.
    if (isUnconditionalBranch(*I)) {
      UnCondBrIter = I;

      if (!AllowModify) {
        TBB = I->getOperand(0).getMBB();
        continue;
      }

      // If the block has any instructions after a JMP, delete them.
      while (std::next(I) != MBB.end()) {
        std::next(I)->eraseFromParent();
      }

      Cond.clear();
      FBB = 0;

      // Delete the JMP if it's equivalent to a fall-through.
      if (MBB.isLayoutSuccessor(I->getOperand(0).getMBB())) {
        TBB = 0;
        I->eraseFromParent();
        I = MBB.end();
        UnCondBrIter = MBB.end();
        continue;
      }

      // TBB is used to indicate the unconditinal destination.
      TBB = I->getOperand(0).getMBB();
      continue;
    }

    // Handle conditional branches.
    assert(isConditionalBranch(*I) && "Expected conditional branch!");
    GBCC::CondCodes BranchCode = (GBCC::CondCodes)I->getOperand(1).getImm();
    if (BranchCode == GBCC::COND_INVALID) {
      return true; // Not sure if this is possible, but we can't handle it.
    }

    // Working from the bottom, handle the first conditional branch.
    if (Cond.empty()) {
      MachineBasicBlock *TargetBB = I->getOperand(0).getMBB();
      if (AllowModify && UnCondBrIter != MBB.end() &&
          MBB.isLayoutSuccessor(TargetBB)) {
        // If we can modify the code and it ends in something like:
        //
        //     jCC L1
        //     jmp L2
        //   L1:
        //     ...
        //   L2:
        //
        // Then we can change this to:
        //
        //     jnCC L2
        //   L1:
        //     ...
        //   L2:
        //
        // Which is a bit more efficient.
        // We conditionally jump to the fall-through block.
        BranchCode = getOppositeCondition(BranchCode);
        MachineBasicBlock::iterator OldInst = I;

        BuildMI(MBB, UnCondBrIter, MBB.findDebugLoc(I), get(GB::JP_cc_nn))
            .addMBB(UnCondBrIter->getOperand(0).getMBB())
            .addImm(BranchCode);
        BuildMI(MBB, UnCondBrIter, MBB.findDebugLoc(I), get(GB::JP_nn))
            .addMBB(TargetBB);

        OldInst->eraseFromParent();
        UnCondBrIter->eraseFromParent();

        // Restart the analysis.
        UnCondBrIter = MBB.end();
        I = MBB.end();
        continue;
      }

      FBB = TBB;
      TBB = I->getOperand(0).getMBB();
      // Conditions go in reverse order (bottommost condition pair first)
      Cond.push_back(MachineOperand::CreateImm(BranchCode));
      Cond.push_back(MachineOperand::CreateImm(1)); // True branch.
      continue;
    }

    // Handle subsequent conditional branches.
    //assert(Cond.size() == 1);
    assert(TBB);
    

    GBCC::CondCodes CC = (GBCC::CondCodes)I->getOperand(1).getImm();
    const MachineBasicBlock *Dest = I->getOperand(0).getMBB();
    // Only handle the case where all conditional branches branch to either
    // the false block or the true block.
    assert(FBB && "Is this possible?");
    if (Dest != TBB && Dest != FBB) {
      return true;
    }
                                                                               
    // If the condition is the same as a later condition, that instruction can
    // be removed. The branch targets don't matter.
    // XXX: Can we do similar things in other cases?
    if (AllowModify) {
      MachineBasicBlock::iterator OldI = UnCondBrIter;
      ++OldI; // jump one instr back.
      for ( ; OldI != I; OldI++)
        if (OldI->getOperand(1).getImm() == (int)CC)
          break;
      if (OldI != I) {
        // We found a later condition that's the same as this one. Remove it.
        // Move the uncond iter back one step to compensate (?).
        OldI->eraseFromParent();
        
        // Restart the analysis.
        Cond.clear();
        I = MBB.end();
        UnCondBrIter = MBB.end();
        continue;
      }
    }
    
    Cond.push_back(MachineOperand::CreateImm(BranchCode));
    Cond.push_back(MachineOperand::CreateImm(Dest == TBB));

    continue;
  }

  return false;
}

unsigned GBZ80InstrInfo::insertBranch(MachineBasicBlock &MBB,
                                    MachineBasicBlock *TBB,
                                    MachineBasicBlock *FBB,
                                    ArrayRef<MachineOperand> Cond,
                                    const DebugLoc &DL,
                                    int *BytesAdded) const {
  if (BytesAdded) *BytesAdded = 0;

  // Shouldn't be a fall through.
  assert(TBB && "insertBranch must not be told to insert a fallthrough");
  assert((Cond.size() % 2 == 0) &&
         "GBZ80 branch conditions must have a multiple of two components!");

  if (Cond.empty()) {
    assert(!FBB && "Unconditional branch with multiple successors!");
    auto &MI = *BuildMI(&MBB, DL, get(GB::JP_nn)).addMBB(TBB);
    if (BytesAdded)
      *BytesAdded += getInstSizeInBytes(MI);
    return 1;
  }

  // Conditional branches.
  unsigned Count = 0;
  // The condition list is in reverse (first condition is last), so iterate
  // backwards through the conditions, adding them one after another.
  for (auto I = Cond.rbegin(); I != Cond.rend(); ++I) {
    bool IsTrueBranch = I->getImm();
    ++I;
    GBCC::CondCodes CC = (GBCC::CondCodes)I->getImm();
    MachineBasicBlock *Dest = IsTrueBranch ? TBB : FBB;
    // If Dest is null, this means we must jump to the fallthrough.
    if (!Dest) {
      Dest = MBB.getNextNode();
      assert(std::next(MBB.getIterator()) != MBB.getParent()->end() &&
            "Fallthrough at the end of a function?");
    }

    auto &CondMI = *BuildMI(&MBB, DL, get(GB::JP_cc_nn))
      .addMBB(Dest)
      .addImm((int)CC);

    if (BytesAdded) *BytesAdded += getInstSizeInBytes(CondMI);
    ++Count;
  }

  if (FBB) {
    // Two-way Conditional branch. Insert the second branch.
    auto &MI = *BuildMI(&MBB, DL, get(GB::JP_nn)).addMBB(FBB);
    if (BytesAdded) *BytesAdded += getInstSizeInBytes(MI);
    ++Count;
  }

  return Count;
}

unsigned GBZ80InstrInfo::removeBranch(MachineBasicBlock &MBB,
                                    int *BytesRemoved) const {
  if (BytesRemoved) *BytesRemoved = 0;

  MachineBasicBlock::iterator I = MBB.end();
  unsigned Count = 0;

  while (I != MBB.begin()) {
    --I;
    if (I->isDebugValue()) {
      continue;
    }
    // Don't bother removing indirect jumps and such.
    if (!isUnconditionalBranch(*I) && !isConditionalBranch(*I)) {
      break;
    }

    // Remove the branch.
    if (BytesRemoved) *BytesRemoved += getInstSizeInBytes(*I);
    I->eraseFromParent();
    I = MBB.end();
    ++Count;
  }

  return Count;
}

bool GBZ80InstrInfo::reverseBranchCondition(
    SmallVectorImpl<MachineOperand> &Cond) const {
  assert((Cond.size() % 2 == 0) && "Invalid GBZ80 branch condition!");

  return !invertCondition(Cond);
}

bool GBZ80InstrInfo::analyzeBranchPredicate(MachineBasicBlock &MBB,
  MachineBranchPredicate &MBP,
  bool AllowModify) const {
  SmallVector<MachineOperand, 4> Cond;
  MachineBasicBlock *FBB = nullptr, *TBB = nullptr;
  if (analyzeBranch(MBB, TBB, FBB, Cond, AllowModify))
    return true;

  // TODO: do some kind of condition analysis when we have it

  return true;
}

unsigned GBZ80InstrInfo::getInstSizeInBytes(const MachineInstr &MI) const {
  unsigned Opcode = MI.getOpcode();

  switch (Opcode) {
  // A regular instruction
  default: {
    // XXX: Pseudos?
    const MCInstrDesc &Desc = get(Opcode);
    return Desc.getSize();
  }
  case TargetOpcode::EH_LABEL:
  case TargetOpcode::IMPLICIT_DEF:
  case TargetOpcode::KILL:
  case TargetOpcode::DBG_VALUE:
    return 0;
#if 0
  case TargetOpcode::INLINEASM: {
    const MachineFunction &MF = *MI.getParent()->getParent();
    const GBZ80TargetMachine &TM = static_cast<const GBZ80TargetMachine&>(MF.getTarget());
    const GBZ80Subtarget &STI = MF.getSubtarget<GBZ80Subtarget>();
    const TargetInstrInfo &TII = *STI.getInstrInfo();

    return TII.getInlineAsmLength(MI.getOperand(0).getSymbolName(),
                                  *TM.getMCAsmInfo());
  }
#endif
  }
}

MachineBasicBlock *
GBZ80InstrInfo::getBranchDestBlock(const MachineInstr &MI) const {
  switch (MI.getOpcode()) {
  default:
    llvm_unreachable("unexpected opcode!");
  case GB::JP_cc_nn:
  case GB::JP_nn:
  case GB::JR_cc_e:
  case GB::JR_e:
  case GB::BR16: // Can't do much with this though.
    return MI.getOperand(0).getMBB();
  }
}

bool GBZ80InstrInfo::isBranchOffsetInRange(unsigned BranchOp,
                                           int64_t BrOffset) const {

  switch (BranchOp) {
  default:
    llvm_unreachable("unexpected opcode!");
  case GB::CALL_cc_nn:
  case GB::CALL_nn:
  case GB::JP_cc_nn:
  case GB::JP_nn:
    return true;
  case GB::JR_cc_e:
  case GB::JR_e:
    return BrOffset >= -127 && BrOffset <= 129;
  }
}

int GBZ80InstrInfo::getSPAdjust(const MachineInstr &MI) const {
  switch (MI.getOpcode()) {
  case GB::PUSH:
    return -2;
  case GB::POP:
    return 2;
  case GB::ADD_SP_e:
    return (int8_t)MI.getOperand(0).getImm();
  }
  return TargetInstrInfo::getSPAdjust(MI);
}

unsigned GBZ80InstrInfo::insertIndirectBranch(MachineBasicBlock &MBB,
  MachineBasicBlock &NewDestBB,
  const DebugLoc &DL,
  int64_t BrOffset,
  RegScavenger *RS) const {
  // This method inserts a *direct* branch (JMP), despite its name.
  // LLVM calls this method to fixup unconditional branches; it never calls
  // insertBranch or some hypothetical "insertDirectBranch".
  // See lib/CodeGen/RegisterRelaxation.cpp for details.
  // We end up here when a jump is too long for a RJMP instruction.
  auto &MI = *BuildMI(&MBB, DL, get(GB::JP_nn)).addMBB(&NewDestBB);

  return getInstSizeInBytes(MI);
}

std::map<CCPair, unsigned> GBZ80InstrInfo::CCPair2CPClass = {
  {{GBCC::COND_Z, true, GBCC::COND_INVALID, false}, 0b010},
  {{GBCC::COND_C, false, GBCC::COND_Z, true}, 0b010},
  {{GBCC::COND_NZ, false, GBCC::COND_Z, true}, 0b010},
  {{GBCC::COND_NZ, false, GBCC::COND_NC, true}, 0b010},
  {{GBCC::COND_Z, true, GBCC::COND_Z, false}, 0b010},
  {{GBCC::COND_Z, true, GBCC::COND_C, false}, 0b010},
  {{GBCC::COND_Z, true, GBCC::COND_NZ, false}, 0b010},
  {{GBCC::COND_Z, true, GBCC::COND_NC, false}, 0b010},
  {{GBCC::COND_Z, true, GBCC::COND_Z, true}, 0b010},
  {{GBCC::COND_NC, true, GBCC::COND_INVALID, false}, 0b011},
  {{GBCC::COND_C, false, GBCC::COND_NC, true}, 0b011},
  {{GBCC::COND_NC, true, GBCC::COND_Z, false}, 0b011},
  {{GBCC::COND_NC, true, GBCC::COND_C, false}, 0b011},
  {{GBCC::COND_NC, true, GBCC::COND_NZ, false}, 0b011},
  {{GBCC::COND_NC, true, GBCC::COND_NC, false}, 0b011},
  {{GBCC::COND_Z, true, GBCC::COND_NC, true}, 0b011},
  {{GBCC::COND_NC, true, GBCC::COND_Z, true}, 0b011},
  {{GBCC::COND_NC, true, GBCC::COND_NC, true}, 0b011},
  {{GBCC::COND_C, true, GBCC::COND_INVALID, false}, 0b100},
  {{GBCC::COND_Z, false, GBCC::COND_C, true}, 0b100},
  {{GBCC::COND_NC, false, GBCC::COND_C, true}, 0b100},
  {{GBCC::COND_NC, false, GBCC::COND_NZ, true}, 0b100},
  {{GBCC::COND_C, true, GBCC::COND_Z, false}, 0b100},
  {{GBCC::COND_C, true, GBCC::COND_C, false}, 0b100},
  {{GBCC::COND_C, true, GBCC::COND_NZ, false}, 0b100},
  {{GBCC::COND_C, true, GBCC::COND_NC, false}, 0b100},
  {{GBCC::COND_C, true, GBCC::COND_C, true}, 0b100},
  {{GBCC::COND_Z, true, GBCC::COND_C, true}, 0b110},
  {{GBCC::COND_C, true, GBCC::COND_Z, true}, 0b110},
  {{GBCC::COND_Z, false, GBCC::COND_Z, true}, 0b000},
  {{GBCC::COND_C, false, GBCC::COND_C, true}, 0b000},
  {{GBCC::COND_NZ, false, GBCC::COND_C, true}, 0b000},
  {{GBCC::COND_NZ, false, GBCC::COND_NZ, true}, 0b000},
  {{GBCC::COND_NC, false, GBCC::COND_Z, true}, 0b000},
  {{GBCC::COND_NC, false, GBCC::COND_NC, true}, 0b000},
  {{GBCC::COND_Z, true, GBCC::COND_NZ, true}, 0b111},
  {{GBCC::COND_C, true, GBCC::COND_NC, true}, 0b111},
  {{GBCC::COND_NZ, true, GBCC::COND_Z, true}, 0b111},
  {{GBCC::COND_NZ, true, GBCC::COND_NC, true}, 0b111},
  {{GBCC::COND_NC, true, GBCC::COND_C, true}, 0b111},
  {{GBCC::COND_NC, true, GBCC::COND_NZ, true}, 0b111},
  {{GBCC::COND_NZ, true, GBCC::COND_INVALID, false}, 0b101},
  {{GBCC::COND_Z, false, GBCC::COND_NZ, true}, 0b101},
  {{GBCC::COND_NZ, true, GBCC::COND_Z, false}, 0b101},
  {{GBCC::COND_NZ, true, GBCC::COND_C, false}, 0b101},
  {{GBCC::COND_NZ, true, GBCC::COND_NZ, false}, 0b101},
  {{GBCC::COND_NZ, true, GBCC::COND_NC, false}, 0b101},
  {{GBCC::COND_C, true, GBCC::COND_NZ, true}, 0b101},
  {{GBCC::COND_NZ, true, GBCC::COND_C, true}, 0b101},
  {{GBCC::COND_NZ, true, GBCC::COND_NZ, true}, 0b101},
  {{GBCC::COND_Z, false, GBCC::COND_NC, true}, 0b001},
  {{GBCC::COND_C, false, GBCC::COND_NZ, true}, 0b001},
};

std::map<unsigned, std::vector<CCPair>> GBZ80InstrInfo::CPClass2CCPairs = {
{0b010,{
  {GBCC::COND_Z, true, GBCC::COND_INVALID, false},
  {GBCC::COND_C, false, GBCC::COND_Z, true},
  {GBCC::COND_NZ, false, GBCC::COND_Z, true},
  {GBCC::COND_NZ, false, GBCC::COND_NC, true},
  {GBCC::COND_Z, true, GBCC::COND_Z, false},
  {GBCC::COND_Z, true, GBCC::COND_C, false},
  {GBCC::COND_Z, true, GBCC::COND_NZ, false},
  {GBCC::COND_Z, true, GBCC::COND_NC, false},
  {GBCC::COND_Z, true, GBCC::COND_Z, true},
}},
{0b011,{
  {GBCC::COND_NC, true, GBCC::COND_INVALID, false},
  {GBCC::COND_C, false, GBCC::COND_NC, true},
  {GBCC::COND_NC, true, GBCC::COND_Z, false},
  {GBCC::COND_NC, true, GBCC::COND_C, false},
  {GBCC::COND_NC, true, GBCC::COND_NZ, false},
  {GBCC::COND_NC, true, GBCC::COND_NC, false},
  {GBCC::COND_Z, true, GBCC::COND_NC, true},
  {GBCC::COND_NC, true, GBCC::COND_Z, true},
  {GBCC::COND_NC, true, GBCC::COND_NC, true},
}},
{0b100,{
  {GBCC::COND_C, true, GBCC::COND_INVALID, false},
  {GBCC::COND_Z, false, GBCC::COND_C, true},
  {GBCC::COND_NC, false, GBCC::COND_C, true},
  {GBCC::COND_NC, false, GBCC::COND_NZ, true},
  {GBCC::COND_C, true, GBCC::COND_Z, false},
  {GBCC::COND_C, true, GBCC::COND_C, false},
  {GBCC::COND_C, true, GBCC::COND_NZ, false},
  {GBCC::COND_C, true, GBCC::COND_NC, false},
  {GBCC::COND_C, true, GBCC::COND_C, true},
}},
{0b110,{
  {GBCC::COND_Z, true, GBCC::COND_C, true},
  {GBCC::COND_C, true, GBCC::COND_Z, true},
}},
{0b000,{
  {GBCC::COND_Z, false, GBCC::COND_Z, true},
  {GBCC::COND_C, false, GBCC::COND_C, true},
  {GBCC::COND_NZ, false, GBCC::COND_C, true},
  {GBCC::COND_NZ, false, GBCC::COND_NZ, true},
  {GBCC::COND_NC, false, GBCC::COND_Z, true},
  {GBCC::COND_NC, false, GBCC::COND_NC, true},
}},
{0b111,{
  {GBCC::COND_Z, true, GBCC::COND_NZ, true},
  {GBCC::COND_C, true, GBCC::COND_NC, true},
  {GBCC::COND_NZ, true, GBCC::COND_Z, true},
  {GBCC::COND_NZ, true, GBCC::COND_NC, true},
  {GBCC::COND_NC, true, GBCC::COND_C, true},
  {GBCC::COND_NC, true, GBCC::COND_NZ, true},
}},
{0b101,{
  {GBCC::COND_NZ, true, GBCC::COND_INVALID, false},
  {GBCC::COND_Z, false, GBCC::COND_NZ, true},
  {GBCC::COND_NZ, true, GBCC::COND_Z, false},
  {GBCC::COND_NZ, true, GBCC::COND_C, false},
  {GBCC::COND_NZ, true, GBCC::COND_NZ, false},
  {GBCC::COND_NZ, true, GBCC::COND_NC, false},
  {GBCC::COND_C, true, GBCC::COND_NZ, true},
  {GBCC::COND_NZ, true, GBCC::COND_C, true},
  {GBCC::COND_NZ, true, GBCC::COND_NZ, true},
}},
{0b001,{
  {GBCC::COND_Z, false, GBCC::COND_NC, true},
  {GBCC::COND_C, false, GBCC::COND_NZ, true},
}}
};

} // end of namespace llvm


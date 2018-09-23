//===-- GBZ80ISelLowering.cpp - GBZ80 DAG Lowering Implementation -------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines the interfaces that GBZ80 uses to lower LLVM code into a
// selection DAG.
//
//===----------------------------------------------------------------------===//

#include "GBZ80ISelLowering.h"

#include "llvm/ADT/StringSwitch.h"
#include "llvm/CodeGen/CallingConvLower.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/SelectionDAG.h"
#include "llvm/CodeGen/TargetLoweringObjectFileImpl.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/ErrorHandling.h"

#include "GBZ80.h"
#include "GBZ80MachineFunctionInfo.h"
#include "GBZ80TargetMachine.h"
#include "MCTargetDesc/GBZ80MCTargetDesc.h"

namespace llvm {

GBZ80TargetLowering::GBZ80TargetLowering(GBZ80TargetMachine &tm)
    : TargetLowering(tm) {
  // Set up the register classes.
  addRegisterClass(MVT::i8, &GB::R8RegClass);
  addRegisterClass(MVT::i16, &GB::R16RegClass);

  // Compute derived properties from the register classes.
  computeRegisterProperties(tm.getSubtargetImpl()->getRegisterInfo());

  setBooleanContents(ZeroOrOneBooleanContent);
  setBooleanVectorContents(ZeroOrOneBooleanContent);
  setSchedulingPreference(Sched::RegPressure);
  setStackPointerRegisterToSaveRestore(GB::SP);

  setOperationAction(ISD::GlobalAddress, MVT::i16, Custom);
  setOperationAction(ISD::BlockAddress, MVT::i16, Custom);

  setOperationAction(ISD::STACKSAVE, MVT::Other, Expand);
  setOperationAction(ISD::STACKRESTORE, MVT::Other, Expand);
  setOperationAction(ISD::DYNAMIC_STACKALLOC, MVT::i8, Expand);
  setOperationAction(ISD::DYNAMIC_STACKALLOC, MVT::i16, Expand);

  for (MVT VT : MVT::integer_valuetypes()) {
    for (auto N : {ISD::EXTLOAD, ISD::SEXTLOAD, ISD::ZEXTLOAD}) {
      setLoadExtAction(N, VT, MVT::i1, Promote);
      setLoadExtAction(N, VT, MVT::i8, Expand);
    }
  }

  setTruncStoreAction(MVT::i16, MVT::i8, Expand);

  // sub (x, imm) gets canonicalized to add (x, -imm), so for illegal types
  // revert into a sub since we don't have an add with immediate instruction.
  setOperationAction(ISD::ADD, MVT::i32, Custom);
  setOperationAction(ISD::ADD, MVT::i64, Custom);

  // our shift instructions are only able to shift 1 bit at a time, so handle
  // this in a custom way.
  setOperationAction(ISD::SRA, MVT::i8, Custom);
  setOperationAction(ISD::SHL, MVT::i8, Custom);
  setOperationAction(ISD::SRL, MVT::i8, Custom);
  setOperationAction(ISD::SRA, MVT::i16, Custom);
  setOperationAction(ISD::SHL, MVT::i16, Custom);
  setOperationAction(ISD::SRL, MVT::i16, Custom);
  setOperationAction(ISD::SHL_PARTS, MVT::i16, Expand);
  setOperationAction(ISD::SRA_PARTS, MVT::i16, Expand);
  setOperationAction(ISD::SRL_PARTS, MVT::i16, Expand);

  setOperationAction(ISD::ROTL, MVT::i8, Custom);
  setOperationAction(ISD::ROTL, MVT::i16, Custom);
  setOperationAction(ISD::ROTR, MVT::i8, Custom);
  setOperationAction(ISD::ROTR, MVT::i16, Custom);
  
  setOperationAction(ISD::BR_CC, MVT::i8, Custom);
  setOperationAction(ISD::BR_CC, MVT::i16, Custom);
  setOperationAction(ISD::BR_CC, MVT::i32, Custom);
  setOperationAction(ISD::BR_CC, MVT::i64, Custom);
  setOperationAction(ISD::BRCOND, MVT::Other, Expand);

  setOperationAction(ISD::SELECT_CC, MVT::i8, Custom);
  setOperationAction(ISD::SELECT_CC, MVT::i16, Custom);
  setOperationAction(ISD::SELECT_CC, MVT::i32, Expand);
  setOperationAction(ISD::SELECT_CC, MVT::i64, Expand);
  setOperationAction(ISD::SETCC, MVT::i8, Custom);
  setOperationAction(ISD::SETCC, MVT::i16, Custom);
  setOperationAction(ISD::SETCC, MVT::i32, Custom);
  setOperationAction(ISD::SETCC, MVT::i64, Custom);
  setOperationAction(ISD::SELECT, MVT::i8, Expand);
  setOperationAction(ISD::SELECT, MVT::i16, Expand);

  setOperationAction(ISD::BSWAP, MVT::i16, Expand);

  // Add support for postincrement and predecrement load/stores.

  //setIndexedLoadAction(ISD::POST_INC, MVT::i8, Legal);
  //setIndexedStoreAction(ISD::POST_INC, MVT::i8, Legal);
  //setIndexedLoadAction(ISD::POST_DEC, MVT::i8, Legal);
  //setIndexedStoreAction(ISD::POST_DEC, MVT::i8, Legal);
  //setIndexedLoadAction(ISD::POST_INC, MVT::i16, Legal);
  //setIndexedLoadAction(ISD::PRE_DEC, MVT::i8, Legal);
  //setIndexedLoadAction(ISD::PRE_DEC, MVT::i16, Legal);
  
  //setIndexedStoreAction(ISD::POST_INC, MVT::i16, Legal);
  //setIndexedStoreAction(ISD::PRE_DEC, MVT::i8, Legal);
  //setIndexedStoreAction(ISD::PRE_DEC, MVT::i16, Legal);

  setOperationAction(ISD::BR_JT, MVT::Other, Expand);

  setOperationAction(ISD::VASTART, MVT::Other, Custom);
  setOperationAction(ISD::VAEND, MVT::Other, Expand);
  setOperationAction(ISD::VAARG, MVT::Other, Expand);
  setOperationAction(ISD::VACOPY, MVT::Other, Expand);
  /*
  // Atomic operations which must be lowered to rtlib calls
  for (MVT VT : MVT::integer_valuetypes()) {
    setOperationAction(ISD::ATOMIC_SWAP, VT, Expand);
    setOperationAction(ISD::ATOMIC_CMP_SWAP, VT, Expand);
    setOperationAction(ISD::ATOMIC_LOAD_NAND, VT, Expand);
    setOperationAction(ISD::ATOMIC_LOAD_MAX, VT, Expand);
    setOperationAction(ISD::ATOMIC_LOAD_MIN, VT, Expand);
    setOperationAction(ISD::ATOMIC_LOAD_UMAX, VT, Expand);
    setOperationAction(ISD::ATOMIC_LOAD_UMIN, VT, Expand);
  }
  */
  // Division/remainder
  setOperationAction(ISD::UDIV, MVT::i8, Expand);
  setOperationAction(ISD::UDIV, MVT::i16, Expand);
  setOperationAction(ISD::UREM, MVT::i8, Expand);
  setOperationAction(ISD::UREM, MVT::i16, Expand);
  setOperationAction(ISD::SDIV, MVT::i8, Expand);
  setOperationAction(ISD::SDIV, MVT::i16, Expand);
  setOperationAction(ISD::SREM, MVT::i8, Expand);
  setOperationAction(ISD::SREM, MVT::i16, Expand);

  // Make division and modulus custom
  for (MVT VT : MVT::integer_valuetypes()) {
    setOperationAction(ISD::UDIVREM, VT, Custom);
    setOperationAction(ISD::SDIVREM, VT, Custom);
  }

  // Do not use MUL. The GBZ80 instructions are closer to SMUL_LOHI &co.
  setOperationAction(ISD::MUL, MVT::i8, Expand);
  setOperationAction(ISD::MUL, MVT::i16, Expand);

  // Expand 16 bit multiplications.
  setOperationAction(ISD::SMUL_LOHI, MVT::i16, Expand);
  setOperationAction(ISD::UMUL_LOHI, MVT::i16, Expand);

  for (MVT VT : MVT::integer_valuetypes()) {
    setOperationAction(ISD::MULHS, VT, Expand);
    setOperationAction(ISD::MULHU, VT, Expand);
  }

  for (MVT VT : MVT::integer_valuetypes()) {
    setOperationAction(ISD::CTPOP, VT, Expand);
    setOperationAction(ISD::CTLZ, VT, Expand);
    setOperationAction(ISD::CTTZ, VT, Expand);
  }

  for (MVT VT : MVT::integer_valuetypes()) {
    setOperationAction(ISD::SIGN_EXTEND_INREG, VT, Expand);
    // TODO: The generated code is pretty poor. Investigate using the
    // same "shift and subtract with carry" trick that we do for
    // extending 8-bit to 16-bit. This may require infrastructure
    // improvements in how we treat 16-bit "registers" to be feasible.
  }

  // Division rtlib functions (not supported)
  setLibcallName(RTLIB::SDIV_I8, nullptr);
  setLibcallName(RTLIB::SDIV_I16, nullptr);
  setLibcallName(RTLIB::SDIV_I32, nullptr);
  setLibcallName(RTLIB::SDIV_I64, nullptr);
  setLibcallName(RTLIB::SDIV_I128, nullptr);
  setLibcallName(RTLIB::UDIV_I8, nullptr);
  setLibcallName(RTLIB::UDIV_I16, nullptr);
  setLibcallName(RTLIB::UDIV_I32, nullptr);
  setLibcallName(RTLIB::UDIV_I64, nullptr);
  setLibcallName(RTLIB::UDIV_I128, nullptr);

  // Modulus rtlib functions (not supported)
  setLibcallName(RTLIB::SREM_I8, nullptr);
  setLibcallName(RTLIB::SREM_I16, nullptr);
  setLibcallName(RTLIB::SREM_I32, nullptr);
  setLibcallName(RTLIB::SREM_I64, nullptr);
  setLibcallName(RTLIB::SREM_I128, nullptr);
  setLibcallName(RTLIB::UREM_I8, nullptr);
  setLibcallName(RTLIB::UREM_I16, nullptr);
  setLibcallName(RTLIB::UREM_I32, nullptr);
  setLibcallName(RTLIB::UREM_I64, nullptr);
  setLibcallName(RTLIB::UREM_I128, nullptr);

  // Division and modulus rtlib functions
  setLibcallName(RTLIB::SDIVREM_I8, "__divmodqi4");
  setLibcallName(RTLIB::SDIVREM_I16, "__divmodhi4");
  setLibcallName(RTLIB::SDIVREM_I32, "__divmodsi4");
  setLibcallName(RTLIB::SDIVREM_I64, "__divmoddi4");
  setLibcallName(RTLIB::SDIVREM_I128, "__divmodti4");
  setLibcallName(RTLIB::UDIVREM_I8, "__udivmodqi4");
  setLibcallName(RTLIB::UDIVREM_I16, "__udivmodhi4");
  setLibcallName(RTLIB::UDIVREM_I32, "__udivmodsi4");
  setLibcallName(RTLIB::UDIVREM_I64, "__udivmoddi4");
  setLibcallName(RTLIB::UDIVREM_I128, "__udivmodti4");

  // Several of the runtime library functions use a special calling conv
  /*
  setLibcallCallingConv(RTLIB::SDIVREM_I8, CallingConv::GBZ80_BUILTIN);
  setLibcallCallingConv(RTLIB::SDIVREM_I16, CallingConv::GBZ80_BUILTIN);
  setLibcallCallingConv(RTLIB::UDIVREM_I8, CallingConv::GBZ80_BUILTIN);
  setLibcallCallingConv(RTLIB::UDIVREM_I16, CallingConv::GBZ80_BUILTIN);
  */
  // Trigonometric rtlib functions
  setLibcallName(RTLIB::SIN_F32, "sin");
  setLibcallName(RTLIB::COS_F32, "cos");

  setMinFunctionAlignment(1);
  setMinimumJumpTableEntries(INT_MAX);
}

const char *GBZ80TargetLowering::getTargetNodeName(unsigned Opcode) const {
#define NODE(name)       \
  case GBISD::name:     \
    return #name

  switch (Opcode) {
  default:
    return nullptr;
    NODE(RET_FLAG);
    NODE(RETI_FLAG);
    NODE(CALL);
    NODE(WRAPPER);
    NODE(LSL);
    NODE(LSR);
    NODE(ROL);
    NODE(ROR);
    NODE(ASR);
    NODE(LSLLOOP);
    NODE(LSRLOOP);
    NODE(ASRLOOP);
    NODE(BRCOND);
    NODE(CMP);
    NODE(CMPC);
    NODE(TST);
    NODE(SELECT_CC);
    NODE(SELECT_BR);
    NODE(BR16);
#undef NODE
  }
}

EVT GBZ80TargetLowering::getSetCCResultType(const DataLayout &DL, LLVMContext &,
                                          EVT VT) const {
  assert(!VT.isVector() && "No GBZ80 SetCC type for vectors!");
  return MVT::i8;
}

SDValue GBZ80TargetLowering::LowerShifts(SDValue Op, SelectionDAG &DAG) const {
  //:TODO: this function has to be completely rewritten to produce optimal
  // code, for now it's producing very long but correct code.
  unsigned Opc8;
  const SDNode *N = Op.getNode();
  EVT VT = Op.getValueType();
  SDLoc dl(N);

  // Expand non-constant shifts to loops.
  if (!isa<ConstantSDNode>(N->getOperand(1))) {
    switch (Op.getOpcode()) {
    default:
      llvm_unreachable("Invalid shift opcode!");
    case ISD::SHL:
      return DAG.getNode(GBISD::LSLLOOP, dl, VT, N->getOperand(0),
                         N->getOperand(1));
    case ISD::SRL:
      return DAG.getNode(GBISD::LSRLOOP, dl, VT, N->getOperand(0),
                         N->getOperand(1));
    case ISD::ROTL:
      return DAG.getNode(GBISD::ROLLOOP, dl, VT, N->getOperand(0),
                         N->getOperand(1));
    case ISD::ROTR:
      return DAG.getNode(GBISD::RORLOOP, dl, VT, N->getOperand(0),
                         N->getOperand(1));
    case ISD::SRA:
      return DAG.getNode(GBISD::ASRLOOP, dl, VT, N->getOperand(0),
                         N->getOperand(1));
    }
  }

  uint64_t ShiftAmount = cast<ConstantSDNode>(N->getOperand(1))->getZExtValue();
  SDValue Victim = N->getOperand(0);

  switch (Op.getOpcode()) {
  case ISD::SRA:
    Opc8 = GBISD::ASR;
    break;
  case ISD::ROTL:
    Opc8 = GBISD::ROL;
    break;
  case ISD::ROTR:
    Opc8 = GBISD::ROR;
    break;
  case ISD::SRL:
    Opc8 = GBISD::LSR;
    break;
  case ISD::SHL:
    Opc8 = GBISD::LSL;
    break;
  default:
    llvm_unreachable("Invalid shift opcode");
  }

  while (ShiftAmount--) {
    Victim = DAG.getNode(Opc8, dl, VT, Victim);
  }

  return Victim;
}

SDValue GBZ80TargetLowering::LowerDivRem(SDValue Op, SelectionDAG &DAG) const {
  unsigned Opcode = Op->getOpcode();
  assert((Opcode == ISD::SDIVREM || Opcode == ISD::UDIVREM) &&
         "Invalid opcode for Div/Rem lowering");
  bool IsSigned = (Opcode == ISD::SDIVREM);
  EVT VT = Op->getValueType(0);
  Type *Ty = VT.getTypeForEVT(*DAG.getContext());

  RTLIB::Libcall LC;
  switch (VT.getSimpleVT().SimpleTy) {
  default:
    llvm_unreachable("Unexpected request for libcall!");
  case MVT::i8:
    LC = IsSigned ? RTLIB::SDIVREM_I8 : RTLIB::UDIVREM_I8;
    break;
  case MVT::i16:
    LC = IsSigned ? RTLIB::SDIVREM_I16 : RTLIB::UDIVREM_I16;
    break;
  case MVT::i32:
    LC = IsSigned ? RTLIB::SDIVREM_I32 : RTLIB::UDIVREM_I32;
    break;
  case MVT::i64:
    LC = IsSigned ? RTLIB::SDIVREM_I64 : RTLIB::UDIVREM_I64;
    break;
  }

  SDValue InChain = DAG.getEntryNode();

  TargetLowering::ArgListTy Args;
  TargetLowering::ArgListEntry Entry;
  for (SDValue const &Value : Op->op_values()) {
    Entry.Node = Value;
    Entry.Ty = Value.getValueType().getTypeForEVT(*DAG.getContext());
    Entry.IsSExt = IsSigned;
    Entry.IsZExt = !IsSigned;
    Args.push_back(Entry);
  }

  SDValue Callee = DAG.getExternalSymbol(getLibcallName(LC),
                                         getPointerTy(DAG.getDataLayout()));

  Type *RetTy = (Type *)StructType::get(Ty, Ty);

  SDLoc dl(Op);
  TargetLowering::CallLoweringInfo CLI(DAG);
  CLI.setDebugLoc(dl)
      .setChain(InChain)
      .setLibCallee(getLibcallCallingConv(LC), RetTy, Callee, std::move(Args))
      .setInRegister()
      .setSExtResult(IsSigned)
      .setZExtResult(!IsSigned);

  std::pair<SDValue, SDValue> CallInfo = LowerCallTo(CLI);
  return CallInfo.first;
}

SDValue GBZ80TargetLowering::LowerGlobalAddress(SDValue Op,
                                              SelectionDAG &DAG) const {
  auto DL = DAG.getDataLayout();

  const GlobalValue *GV = cast<GlobalAddressSDNode>(Op)->getGlobal();
  int64_t Offset = cast<GlobalAddressSDNode>(Op)->getOffset();

  // Create the TargetGlobalAddress node, folding in the constant offset.
  SDValue Result =
      DAG.getTargetGlobalAddress(GV, SDLoc(Op), getPointerTy(DL), Offset);
  return DAG.getNode(GBISD::WRAPPER, SDLoc(Op), getPointerTy(DL), Result);
}

SDValue GBZ80TargetLowering::LowerBlockAddress(SDValue Op,
                                             SelectionDAG &DAG) const {
  auto DL = DAG.getDataLayout();
  const BlockAddress *BA = cast<BlockAddressSDNode>(Op)->getBlockAddress();

  SDValue Result = DAG.getTargetBlockAddress(BA, getPointerTy(DL));

  return DAG.getNode(GBISD::WRAPPER, SDLoc(Op), getPointerTy(DL), Result);
}

/// IntCCToGBCC - Convert a DAG integer condition code to an GBZ80 CC.
static GBCC::CondCodes intCCToGBCC(ISD::CondCode CC) {
  switch (CC) {
  default:
    llvm_unreachable("Unknown condition code!");
  case ISD::SETEQ:
    return GBCC::COND_Z;
  case ISD::SETNE:
    return GBCC::COND_NZ;
  case ISD::SETUGE:
    return GBCC::COND_NC;
  case ISD::SETULT:
    return GBCC::COND_C;
  }
}

static SDValue geti8CC(SDValue LHS, SDValue RHS, ISD::CondCode CC,
  SDValue &GBCC, SelectionDAG &DAG,
  SDLoc DL) {

   /*
  i8:
    EQ: CP X, Y
        JP Z
    
    NE: CP X, Y
        JP NZ
 
    GE : Add 0x80
    UGE: CP X, Y
         JP NC
    
    LE : Add 0x80
    ULE: CP Y, X
         JP NC
         If the LHS has more uses, it might be better to emit something else.

    LT : Add 0x80
    ULT: CP X, Y
         JP C
    
    GT : Add 0x80
    UGT: CP Y, X
         JP C
         If the LHS has more uses, it might be better to emit something else.

  */

  // When doing lhs > -1 use a tst instruction on the top part of lhs
  // and use brpl instead of using a chain of cp/cpc.

  // Turn lhs < rhs with lhs constant into rhs >= lhs+1, this allows
  // us to  fold the constant into the cmp instruction.

  // Turn lhs < 1 into 0 >= lhs since 0 can be materialized with
  // __zero_reg__ in lhs.

  // When doing lhs < 0 use a tst instruction on the top part of lhs
  // and use brmi instead of using a chain of cp/cpc.

  // Turn lhs < rhs with lhs constant into rhs >= lhs+1, this allows us to
  // fold the constant into the cmp instruction.

  // TODO: optimize edge cases for signed
  switch (CC) {
  case ISD::SETGE:
    LHS = DAG.getNode(ISD::ADD, DL, MVT::i8, LHS,
      DAG.getConstant(0x80, DL, MVT::i8));
    RHS = DAG.getNode(ISD::ADD, DL, MVT::i8, RHS,
      DAG.getConstant(0x80, DL, MVT::i8));
    CC = ISD::SETUGE;
    break;
  case ISD::SETLE:
    LHS = DAG.getNode(ISD::ADD, DL, MVT::i8, LHS,
      DAG.getConstant(0x80, DL, MVT::i8));
    RHS = DAG.getNode(ISD::ADD, DL, MVT::i8, RHS,
      DAG.getConstant(0x80, DL, MVT::i8));
    // fallthrough
  case ISD::SETULE:
    std::swap(LHS, RHS);
    CC = ISD::SETUGE;
    break;
  case ISD::SETGT:
    LHS = DAG.getNode(ISD::ADD, DL, MVT::i8, LHS,
      DAG.getConstant(0x80, DL, MVT::i8));
    RHS = DAG.getNode(ISD::ADD, DL, MVT::i8, RHS,
      DAG.getConstant(0x80, DL, MVT::i8));
    // fallthrough
  case ISD::SETUGT:
    std::swap(LHS, RHS);
    CC = ISD::SETULT;
    break;
  case ISD::SETLT:
    LHS = DAG.getNode(ISD::ADD, DL, MVT::i8, LHS,
      DAG.getConstant(0x80, DL, MVT::i8));
    RHS = DAG.getNode(ISD::ADD, DL, MVT::i8, RHS,
      DAG.getConstant(0x80, DL, MVT::i8));
    CC = ISD::SETULT;
    break;
  default:
    break;
  }

  GBCC = DAG.getConstant(intCCToGBCC(CC), DL, MVT::i8);

  return DAG.getNode(GBISD::CMP, DL, MVT::Glue, LHS, RHS);
}

SDValue GBZ80TargetLowering::LowerBR_CC(SDValue Op, SelectionDAG &DAG) const {
  SDValue Chain = Op.getOperand(0);
  ISD::CondCode CC = cast<CondCodeSDNode>(Op.getOperand(1))->get();
  SDValue LHS = Op.getOperand(2);
  SDValue RHS = Op.getOperand(3);
  SDValue Dest = Op.getOperand(4);
  SDLoc dl(Op);

  SDValue TargetCC;
  SDValue Cmp;

  if (LHS.getValueType() == MVT::i8) {
    Cmp = geti8CC(LHS, RHS, CC, TargetCC, DAG, dl);
    return DAG.getNode(GBISD::BRCOND, dl, MVT::Other, Chain, Dest, TargetCC, Cmp);
  }
  else {
    assert(LHS.getValueType() == MVT::i16);
    TargetCC = DAG.getConstant(CC, dl, MVT::i8, true);
    return SDValue(DAG.getMachineNode((unsigned)GB::BR16, dl, MVT::Other,
      {Chain, Dest, Op.getOperand(1), LHS, RHS}), 0);
  }
}

SDValue GBZ80TargetLowering::LowerSELECT_CC(SDValue Op, SelectionDAG &DAG) const {
  SDValue LHS = Op.getOperand(0);
  SDValue RHS = Op.getOperand(1);
  SDValue TrueV = Op.getOperand(2);
  SDValue FalseV = Op.getOperand(3);
  ISD::CondCode CC = cast<CondCodeSDNode>(Op.getOperand(4))->get();
  SDLoc dl(Op);

  SDValue TargetCC = DAG.getTargetConstant(CC, dl, MVT::i8);
  SDValue Cmp;

  // For a comparison of i8, emit an i8 cmp and a SELECT_CC that depends on it.
  if (LHS.getValueType() == MVT::i8) {
    Cmp = geti8CC(LHS, RHS, CC, TargetCC, DAG, dl);
    return DAG.getNode(GBISD::SELECT_CC, dl, MVT::i8, TrueV, FalseV, TargetCC, Cmp);
  }
  assert(LHS.getValueType() == MVT::i16 && "not i6 select_CC?");
  //SDVTList VTs = DAG.getVTList(Op.getValueType(), MVT::Glue);
  SDValue Ops[] = { TrueV, FalseV, TargetCC, LHS, RHS };
  return DAG.getNode(GBISD::SELECT_BR, dl, Op.getValueType(), Ops);
}

SDValue GBZ80TargetLowering::LowerSETCC(SDValue Op, SelectionDAG &DAG) const {
  SDValue LHS = Op.getOperand(0);
  SDValue RHS = Op.getOperand(1);
  ISD::CondCode CC = cast<CondCodeSDNode>(Op.getOperand(2))->get();
  SDLoc dl(Op);

  SDValue TargetCC = DAG.getTargetConstant(CC, dl, MVT::i8);
  SDValue TrueV = DAG.getConstant(1, dl, Op.getValueType());
  SDValue FalseV = DAG.getConstant(0, dl, Op.getValueType());

  if (LHS.getValueType() == MVT::i8) {
    SDValue Cmp = geti8CC(LHS, RHS, CC, TargetCC, DAG, dl);
    SDValue Ops[] = {TrueV, FalseV, TargetCC, Cmp};
    return DAG.getNode(GBISD::SELECT_CC, dl, Op.getValueType(), Ops);
  }
  assert(LHS.getValueType() == MVT::i16);
  SDValue Ops[] = {TrueV, FalseV, TargetCC, LHS, RHS};
  return DAG.getNode(GBISD::SELECT_BR, dl, Op.getValueType(), Ops);
}

SDValue GBZ80TargetLowering::LowerVASTART(SDValue Op, SelectionDAG &DAG) const {
  const MachineFunction &MF = DAG.getMachineFunction();
  const GBZ80MachineFunctionInfo *AFI = MF.getInfo<GBZ80MachineFunctionInfo>();
  const Value *SV = cast<SrcValueSDNode>(Op.getOperand(2))->getValue();
  auto DL = DAG.getDataLayout();
  SDLoc dl(Op);

  // Vastart just stores the address of the VarArgsFrameIndex slot into the
  // memory location argument.
  SDValue FI = DAG.getFrameIndex(AFI->getVarArgsFrameIndex(), getPointerTy(DL));

  return DAG.getStore(Op.getOperand(0), dl, FI, Op.getOperand(1),
                      MachinePointerInfo(SV), 0);
}

SDValue GBZ80TargetLowering::LowerOperation(SDValue Op, SelectionDAG &DAG) const {
  switch (Op.getOpcode()) {
  default:
    llvm_unreachable("Don't know how to custom lower this!");
  case ISD::SHL:
  case ISD::SRA:
  case ISD::SRL:
  case ISD::ROTL:
  case ISD::ROTR:
    return LowerShifts(Op, DAG);
  case ISD::GlobalAddress:
    return LowerGlobalAddress(Op, DAG);
  case ISD::BlockAddress:
    return LowerBlockAddress(Op, DAG);
  case ISD::BR_CC:
    return LowerBR_CC(Op, DAG);
  case ISD::SELECT_CC:
    return LowerSELECT_CC(Op, DAG);
  case ISD::SETCC:
    return LowerSETCC(Op, DAG);
  case ISD::VASTART:
    return LowerVASTART(Op, DAG);
  case ISD::SDIVREM:
  case ISD::UDIVREM:
    return LowerDivRem(Op, DAG);
  }

  return SDValue();
}

/// Replace a node with an illegal result type
/// with a new node built out of custom code.
void GBZ80TargetLowering::ReplaceNodeResults(SDNode *N,
                                           SmallVectorImpl<SDValue> &Results,
                                           SelectionDAG &DAG) const {
  SDLoc DL(N);

  switch (N->getOpcode()) {
  case ISD::ADD: {
    // Convert add (x, imm) into sub (x, -imm).
    if (const ConstantSDNode *C = dyn_cast<ConstantSDNode>(N->getOperand(1))) {
      SDValue Sub = DAG.getNode(
          ISD::SUB, DL, N->getValueType(0), N->getOperand(0),
          DAG.getConstant(-C->getAPIntValue(), DL, C->getValueType(0)));
      Results.push_back(Sub);
    }
    break;
  }
  default: {
    SDValue Res = LowerOperation(SDValue(N, 0), DAG);

    for (unsigned I = 0, E = Res->getNumValues(); I != E; ++I)
      Results.push_back(Res.getValue(I));

    break;
  }
  }
}

/// Return true if the addressing mode represented
/// by AM is legal for this target, for a load/store of the specified type.
bool GBZ80TargetLowering::isLegalAddressingMode(const DataLayout &DL,
                                              const AddrMode &AM, Type *Ty,
                                              unsigned AS,
                                              Instruction *I /*= nullptr*/) const {
  int64_t Offs = AM.BaseOffs;

  // Allow absolute addresses.
  if (AM.BaseGV && !AM.HasBaseReg && AM.Scale == 0 && Offs == 0) {
    return true;
  }

  // Allow simple pointers.
  // FIXME: Is this bad? This also matches multiple base registers.
  if (!AM.BaseGV && AM.HasBaseReg && AM.Scale == 0 && Offs == 0) {
    return true;
  }

  // Allow single scale. Does this make sense?
  if (!AM.BaseGV && !AM.HasBaseReg && AM.Scale == 1 && Offs == 0) {
    return true;
  }

  // Allow 0xFF00-0xFFFF, with or without scale of 1.
  if (!AM.BaseGV && !AM.HasBaseReg && Offs >= 0xFF00) {
    return (AM.Scale == 1 && Offs == 0xFF00) ||
      (AM.Scale == 0 && Offs <= 0xFFFF);
  }

  return false;
}

/// Returns true by value, base pointer and
/// offset pointer and addressing mode by reference if this node can be
/// combined with a load / store to form a post-indexed load / store.
bool GBZ80TargetLowering::getPostIndexedAddressParts(SDNode *N, SDNode *Op,
                                                   SDValue &Base,
                                                   SDValue &Offset,
                                                   ISD::MemIndexedMode &AM,
                                                   SelectionDAG &DAG) const {
  EVT VT;
  SDLoc DL(N);
  return false;
  /*
  if (const LoadSDNode *LD = dyn_cast<LoadSDNode>(N)) {
    VT = LD->getMemoryVT();
    if (LD->getExtensionType() != ISD::NON_EXTLOAD)
      return false;
  } else if (const StoreSDNode *ST = dyn_cast<StoreSDNode>(N)) {
    VT = ST->getMemoryVT();
    if (GB::isProgramMemoryAccess(ST)) {
      return false;
    }
  } else {
    return false;
  }

  if (VT != MVT::i8 && VT != MVT::i16) {
    return false;
  }

  if (Op->getOpcode() != ISD::ADD && Op->getOpcode() != ISD::SUB) {
    return false;
  }

  if (const ConstantSDNode *RHS = dyn_cast<ConstantSDNode>(Op->getOperand(1))) {
    int RHSC = RHS->getSExtValue();
    if (Op->getOpcode() == ISD::SUB)
      RHSC = -RHSC;
    if ((VT == MVT::i16 && RHSC != 2) || (VT == MVT::i8 && RHSC != 1)) {
      return false;
    }

    Base = Op->getOperand(0);
    Offset = DAG.getConstant(RHSC, DL, MVT::i8);
    AM = ISD::POST_INC;

    return true;
  }

  return false;
  */
}

bool GBZ80TargetLowering::isOffsetFoldingLegal(
    const GlobalAddressSDNode *GA) const {
  return true;
}

//===----------------------------------------------------------------------===//
//             Formal Arguments Calling Convention Implementation
//===----------------------------------------------------------------------===//

#include "GBZ80GenCallingConv.inc"

/// For each argument in a function store the number of pieces it is composed
/// of.
static void parseFunctionArgs(const Function *F, const DataLayout *TD,
                              SmallVectorImpl<unsigned> &Out) {
  for (Argument const &Arg : F->args()) {
    unsigned Bytes = (TD->getTypeSizeInBits(Arg.getType()) + 7) / 8;
    Out.push_back((Bytes + 1) / 2);
  }
}

/// For external symbols there is no function prototype information so we
/// have to rely directly on argument sizes.
static void parseExternFuncCallArgs(const SmallVectorImpl<ISD::OutputArg> &In,
                                    SmallVectorImpl<unsigned> &Out) {
  for (unsigned i = 0, e = In.size(); i != e;) {
    unsigned Size = 0;
    unsigned Offset = 0;
    while ((i != e) && (In[i].PartOffset == Offset)) {
      Offset += In[i].VT.getStoreSize();
      ++i;
      ++Size;
    }
    Out.push_back(Size);
  }
}

static StringRef getFunctionName(TargetLowering::CallLoweringInfo &CLI) {
  SDValue Callee = CLI.Callee;

  if (const ExternalSymbolSDNode *G = dyn_cast<ExternalSymbolSDNode>(Callee)) {
    return G->getSymbol();
  }

  if (const GlobalAddressSDNode *G = dyn_cast<GlobalAddressSDNode>(Callee)) {
    return G->getGlobal()->getName();
  }

  llvm_unreachable("don't know how to get the name for this callee");
}

/// Analyze incoming and outgoing function arguments. We need custom C++ code
/// to handle special constraints in the ABI like reversing the order of the
/// pieces of splitted arguments. In addition, all pieces of a certain argument
/// have to be passed either using registers or the stack but never mixing both.
static void analyzeStandardArguments(TargetLowering::CallLoweringInfo *CLI,
                                     const Function *F, const DataLayout *TD,
                                     const SmallVectorImpl<ISD::OutputArg> *Outs,
                                     const SmallVectorImpl<ISD::InputArg> *Ins,
                                     CallingConv::ID CallConv,
                                     SmallVectorImpl<CCValAssign> &ArgLocs,
                                     CCState &CCInfo, bool IsCall, bool IsVarArg) {
  static const MCPhysReg RegList8[] =
  { GB::RA, GB::RB, GB::RC, GB::RD, GB::RE, GB::RH, GB::RL };
  static const MCPhysReg RegList16[] =
  { GB::RBC, GB::RHL, GB::RDE };
  if (IsVarArg) {
    // Variadic functions do not need all the analisys below.
    if (IsCall) {
      CCInfo.AnalyzeCallOperands(*Outs, ArgCC_GBZ80_Vararg);
    } else {
      CCInfo.AnalyzeFormalArguments(*Ins, ArgCC_GBZ80_Vararg);
    }
    return;
  }

  // Fill in the Args array which will contain original argument sizes.
  SmallVector<unsigned, 8> Args;
  if (IsCall) {
    parseExternFuncCallArgs(*Outs, Args);
  } else {
    assert(F != nullptr && "function should not be null");
    parseFunctionArgs(F, TD, Args);
  }

  unsigned ValNo = 0;
  // Variadic functions always use the stack.
  bool UsesStack = false;
  for (unsigned i = 0, pos = 0, e = Args.size(); i != e; ++i) {
    unsigned Size = Args[i];

    if (Size == 0) continue;

    MVT LocVT = (IsCall) ? (*Outs)[pos].VT : (*Ins)[pos].VT;

    // If this is i8 or 16, try allocating it in registers.
    if (LocVT == MVT::i8 || LocVT == MVT::i16) {
      const MCPhysReg *RegList = (LocVT == MVT::i16) ? RegList16 : RegList8;
      unsigned RegListSize = (LocVT == MVT::i16) ? array_lengthof(RegList16)
        : array_lengthof(RegList8);

      unsigned Reg = CCInfo.AllocateReg(
        ArrayRef<MCPhysReg>(RegList, RegListSize));
      if (Reg) {
        CCInfo.addLoc(
          CCValAssign::getReg(ValNo++, LocVT, Reg, LocVT, CCValAssign::Full));
        pos += Size;
        continue;
      }
    }
    // Otherwise, use the stack.
    UsesStack = true;
    for (unsigned j = 0; j != Size; ++j) {
      unsigned Offset = CCInfo.AllocateStack(
          TD->getTypeAllocSize(EVT(LocVT).getTypeForEVT(CCInfo.getContext())),
          TD->getABITypeAlignment(
              EVT(LocVT).getTypeForEVT(CCInfo.getContext())));
      CCInfo.addLoc(CCValAssign::getMem(ValNo++, LocVT, Offset, LocVT,
                                        CCValAssign::Full));
    }
    pos += Size;
  }
}

static void analyzeBuiltinArguments(TargetLowering::CallLoweringInfo &CLI,
                                    const Function *F, const DataLayout *TD,
                                    const SmallVectorImpl<ISD::OutputArg> *Outs,
                                    const SmallVectorImpl<ISD::InputArg> *Ins,
                                    CallingConv::ID CallConv,
                                    SmallVectorImpl<CCValAssign> &ArgLocs,
                                    CCState &CCInfo, bool IsCall, bool IsVarArg) {
  StringRef FuncName = getFunctionName(CLI);

  if (FuncName.startswith("__udivmod") || FuncName.startswith("__divmod")) {
    //CCInfo.AnalyzeCallOperands(*Outs, ArgCC_GBZ80_BUILTIN_DIV);
  } else {
    analyzeStandardArguments(&CLI, F, TD, Outs, Ins,
                             CallConv, ArgLocs, CCInfo,
                             IsCall, IsVarArg);
  }
}

static void analyzeArguments(TargetLowering::CallLoweringInfo *CLI,
                             const Function *F, const DataLayout *TD,
                             const SmallVectorImpl<ISD::OutputArg> *Outs,
                             const SmallVectorImpl<ISD::InputArg> *Ins,
                             CallingConv::ID CallConv,
                             SmallVectorImpl<CCValAssign> &ArgLocs,
                             CCState &CCInfo, bool IsCall, bool IsVarArg) {
  switch (CallConv) {
   /* case CallingConv::GBZ80_BUILTIN: {
      analyzeBuiltinArguments(*CLI, F, TD, Outs, Ins,
                              CallConv, ArgLocs, CCInfo,
                              IsCall, IsVarArg);
      return;
    }*/
    default: {
      analyzeStandardArguments(CLI, F, TD, Outs, Ins,
                               CallConv, ArgLocs, CCInfo,
                               IsCall, IsVarArg);
      return;
    }
  }
}

SDValue GBZ80TargetLowering::LowerFormalArguments(
    SDValue Chain, CallingConv::ID CallConv, bool isVarArg,
    const SmallVectorImpl<ISD::InputArg> &Ins, const SDLoc &dl, SelectionDAG &DAG,
    SmallVectorImpl<SDValue> &InVals) const {
  MachineFunction &MF = DAG.getMachineFunction();
  MachineFrameInfo &MFI = MF.getFrameInfo();
  auto DL = DAG.getDataLayout();

  // Assign locations to all of the incoming arguments.
  SmallVector<CCValAssign, 16> ArgLocs;
  CCState CCInfo(CallConv, isVarArg, DAG.getMachineFunction(), ArgLocs,
                 *DAG.getContext());

  analyzeArguments(nullptr, &MF.getFunction(), &DL, 0, &Ins, CallConv, ArgLocs, CCInfo,
                   false, isVarArg);

  SDValue ArgValue;
  for (CCValAssign &VA : ArgLocs) {

    // Arguments stored on registers.
    if (VA.isRegLoc()) {
      EVT RegVT = VA.getLocVT();
      const TargetRegisterClass *RC;
      if (RegVT == MVT::i8) {
        RC = &GB::R8RegClass;
      } else if (RegVT == MVT::i16) {
        RC = &GB::R16RegClass;
      } else {
        llvm_unreachable("Unknown argument type!");
      }

      unsigned Reg = MF.addLiveIn(VA.getLocReg(), RC);
      ArgValue = DAG.getCopyFromReg(Chain, dl, Reg, RegVT);

      // :NOTE: Clang should not promote any i8 into i16 but for safety the
      // following code will handle zexts or sexts generated by other
      // front ends. Otherwise:
      // If this is an 8 bit value, it is really passed promoted
      // to 16 bits. Insert an assert[sz]ext to capture this, then
      // truncate to the right size.
      switch (VA.getLocInfo()) {
      default:
        llvm_unreachable("Unknown loc info!");
      case CCValAssign::Full:
        break;
      case CCValAssign::BCvt:
        ArgValue = DAG.getNode(ISD::BITCAST, dl, VA.getValVT(), ArgValue);
        break;
      case CCValAssign::SExt:
        ArgValue = DAG.getNode(ISD::AssertSext, dl, RegVT, ArgValue,
                               DAG.getValueType(VA.getValVT()));
        ArgValue = DAG.getNode(ISD::TRUNCATE, dl, VA.getValVT(), ArgValue);
        break;
      case CCValAssign::ZExt:
        ArgValue = DAG.getNode(ISD::AssertZext, dl, RegVT, ArgValue,
                               DAG.getValueType(VA.getValVT()));
        ArgValue = DAG.getNode(ISD::TRUNCATE, dl, VA.getValVT(), ArgValue);
        break;
      }

      InVals.push_back(ArgValue);
    } else {
      // Sanity check.
      assert(VA.isMemLoc());

      EVT LocVT = VA.getLocVT();

      // Create the frame index object for this incoming parameter.
      int FI = MFI.CreateFixedObject(LocVT.getSizeInBits() / 8,
                                     VA.getLocMemOffset(), true);

      // Create the SelectionDAG nodes corresponding to a load
      // from this parameter.
      SDValue FIN = DAG.getFrameIndex(FI, getPointerTy(DL));
      InVals.push_back(DAG.getLoad(LocVT, dl, Chain, FIN,
                                   MachinePointerInfo::getFixedStack(MF, FI),
                                   0));
    }
  }

  // If the function takes variable number of arguments, make a frame index for
  // the start of the first vararg value... for expansion of llvm.va_start.
  if (isVarArg) {
    unsigned StackSize = CCInfo.getNextStackOffset();
    GBZ80MachineFunctionInfo *AFI = MF.getInfo<GBZ80MachineFunctionInfo>();

    AFI->setVarArgsFrameIndex(MFI.CreateFixedObject(2, StackSize, true));
  }

  return Chain;
}

//===----------------------------------------------------------------------===//
//                  Call Calling Convention Implementation
//===----------------------------------------------------------------------===//

SDValue GBZ80TargetLowering::LowerCall(TargetLowering::CallLoweringInfo &CLI,
                                     SmallVectorImpl<SDValue> &InVals) const {
  SelectionDAG &DAG = CLI.DAG;
  SDLoc &DL = CLI.DL;
  SmallVectorImpl<ISD::OutputArg> &Outs = CLI.Outs;
  SmallVectorImpl<SDValue> &OutVals = CLI.OutVals;
  SmallVectorImpl<ISD::InputArg> &Ins = CLI.Ins;
  SDValue Chain = CLI.Chain;
  SDValue Callee = CLI.Callee;
  bool &isTailCall = CLI.IsTailCall;
  CallingConv::ID CallConv = CLI.CallConv;
  bool isVarArg = CLI.IsVarArg;

  MachineFunction &MF = DAG.getMachineFunction();

  // GBZ80 does not yet support tail call optimization.
  isTailCall = false;

  // Analyze operands of the call, assigning locations to each operand.
  SmallVector<CCValAssign, 16> ArgLocs;
  CCState CCInfo(CallConv, isVarArg, DAG.getMachineFunction(), ArgLocs,
                 *DAG.getContext());

  // If the callee is a GlobalAddress/ExternalSymbol node (quite common, every
  // direct call is) turn it into a TargetGlobalAddress/TargetExternalSymbol
  // node so that legalize doesn't hack it.
  const Function *F = nullptr;
  if (const GlobalAddressSDNode *G = dyn_cast<GlobalAddressSDNode>(Callee)) {
    const GlobalValue *GV = G->getGlobal();

    F = cast<Function>(GV);
    Callee =
        DAG.getTargetGlobalAddress(GV, DL, getPointerTy(DAG.getDataLayout()));
  } else if (const ExternalSymbolSDNode *ES =
                 dyn_cast<ExternalSymbolSDNode>(Callee)) {
    Callee = DAG.getTargetExternalSymbol(ES->getSymbol(),
                                         getPointerTy(DAG.getDataLayout()));
  }

  analyzeArguments(&CLI, F, &DAG.getDataLayout(), &Outs, 0, CallConv, ArgLocs, CCInfo,
                   true, isVarArg);

  // Get a count of how many bytes are to be pushed on the stack.
  unsigned NumBytes = CCInfo.getNextStackOffset();

  Chain = DAG.getCALLSEQ_START(Chain, NumBytes, 0, DL);

  SmallVector<std::pair<unsigned, SDValue>, 8> RegsToPass;
  SmallVector<unsigned, 4> StackLocs;

  // First, walk the register assignments, inserting copies.
  unsigned AI, AE;
  bool HasStackArgs = false;
  for (AI = 0, AE = ArgLocs.size(); AI != AE; ++AI) {
    CCValAssign &VA = ArgLocs[AI];
    EVT RegVT = VA.getLocVT();
    SDValue Arg = OutVals[AI];

    // Promote the value if needed. With Clang this should not happen.
    switch (VA.getLocInfo()) {
    default:
      llvm_unreachable("Unknown loc info!");
    case CCValAssign::Full:
      break;
    case CCValAssign::SExt:
      Arg = DAG.getNode(ISD::SIGN_EXTEND, DL, RegVT, Arg);
      break;
    case CCValAssign::ZExt:
      Arg = DAG.getNode(ISD::ZERO_EXTEND, DL, RegVT, Arg);
      break;
    case CCValAssign::AExt:
      Arg = DAG.getNode(ISD::ANY_EXTEND, DL, RegVT, Arg);
      break;
    case CCValAssign::BCvt:
      Arg = DAG.getNode(ISD::BITCAST, DL, RegVT, Arg);
      break;
    }

    if (VA.isMemLoc()) {
      // Keep track of stack arguments.
      StackLocs.push_back(AI);
    }
    else {
      // Arguments that can be passed on registers must be kept in the RegsToPass
      // vector.
      RegsToPass.push_back(std::make_pair(VA.getLocReg(), Arg));
    }
  }

  // Second, stack arguments have to walked in reverse order by inserting
  // chained stores, this ensures their order is not changed by the scheduler
  // and that the push instruction sequence generated is correct, otherwise they
  // can be freely intermixed.

  // FIXME: This should be pushes of 16-bit words. Unsure how this will actually work.
  for (unsigned AI : reverse(StackLocs)) {
    CCValAssign &VA = ArgLocs[AI];
    SDValue Arg = OutVals[AI];

    assert(VA.isMemLoc());

    // SP points to one stack slot further so add one to adjust it.
    SDValue PtrOff = DAG.getNode(
        ISD::ADD, DL, getPointerTy(DAG.getDataLayout()),
        DAG.getRegister(GB::SP, getPointerTy(DAG.getDataLayout())),
        DAG.getIntPtrConstant(VA.getLocMemOffset() + 1, DL));

    Chain =
        DAG.getStore(Chain, DL, Arg, PtrOff,
                      MachinePointerInfo::getStack(MF, VA.getLocMemOffset()),
                      0);
  }

  // Build a sequence of copy-to-reg nodes chained together with token chain and
  // flag operands which copy the outgoing args into registers.  The InFlag in
  // necessary since all emited instructions must be stuck together.
  SDValue InFlag;
  for (auto Reg : RegsToPass) {
    Chain = DAG.getCopyToReg(Chain, DL, Reg.first, Reg.second, InFlag);
    InFlag = Chain.getValue(1);
  }

  // Returns a chain & a flag for retval copy to use.
  SDVTList NodeTys = DAG.getVTList(MVT::Other, MVT::Glue);
  SmallVector<SDValue, 8> Ops;
  Ops.push_back(Chain);
  Ops.push_back(Callee);

  // Add argument registers to the end of the list so that they are known live
  // into the call.
  for (auto Reg : RegsToPass) {
    Ops.push_back(DAG.getRegister(Reg.first, Reg.second.getValueType()));
  }

  // Add a register mask operand representing the call-preserved registers.
  const GBZ80TargetMachine &TM = (const GBZ80TargetMachine &)getTargetMachine();
  const TargetRegisterInfo *TRI = TM.getSubtargetImpl()->getRegisterInfo();
  const uint32_t *Mask =
      TRI->getCallPreservedMask(DAG.getMachineFunction(), CallConv);
  assert(Mask && "Missing call preserved mask for calling convention");
  Ops.push_back(DAG.getRegisterMask(Mask));

  if (InFlag.getNode()) {
    Ops.push_back(InFlag);
  }

  Chain = DAG.getNode(GBISD::CALL, DL, NodeTys, Ops);
  InFlag = Chain.getValue(1);

  // Create the CALLSEQ_END node.
  Chain = DAG.getCALLSEQ_END(Chain, DAG.getIntPtrConstant(NumBytes, DL, true),
                             DAG.getIntPtrConstant(0, DL, true), InFlag, DL);

  if (!Ins.empty()) {
    InFlag = Chain.getValue(1);
  }

  // Handle result values, copying them out of physregs into vregs that we
  // return.
  return LowerCallResult(Chain, InFlag, CallConv, isVarArg, Ins, DL, DAG,
                         InVals);
}

/// Lower the result values of a call into the
/// appropriate copies out of appropriate physical registers.
///
SDValue GBZ80TargetLowering::LowerCallResult(
    SDValue Chain, SDValue InFlag, CallingConv::ID CallConv, bool isVarArg,
    const SmallVectorImpl<ISD::InputArg> &Ins, const SDLoc &dl, SelectionDAG &DAG,
    SmallVectorImpl<SDValue> &InVals) const {

  // Assign locations to each value returned by this call.
  SmallVector<CCValAssign, 16> RVLocs;
  CCState CCInfo(CallConv, isVarArg, DAG.getMachineFunction(), RVLocs,
                 *DAG.getContext());

  // Handle runtime calling convs.
  auto CCFunction = CCAssignFnForReturn(CallConv);
  CCInfo.AnalyzeCallResult(Ins, CCFunction);

  if (RVLocs.size() > 1) {
    // Reverse splitted return values to get the "big endian" format required
    // to agree with the calling convention ABI.
    std::reverse(RVLocs.begin(), RVLocs.end());
  }

  // Copy all of the result registers out of their specified physreg.
  for (CCValAssign const &RVLoc : RVLocs) {
    Chain = DAG.getCopyFromReg(Chain, dl, RVLoc.getLocReg(), RVLoc.getValVT(),
                               InFlag)
                .getValue(1);
    InFlag = Chain.getValue(2);
    InVals.push_back(Chain.getValue(0));
  }

  return Chain;
}

//===----------------------------------------------------------------------===//
//               Return Value Calling Convention Implementation
//===----------------------------------------------------------------------===//

CCAssignFn *GBZ80TargetLowering::CCAssignFnForReturn(CallingConv::ID CC) const {
  switch (CC) {
  default:
    return RetCC_GBZ80;
  }
}

bool
GBZ80TargetLowering::CanLowerReturn(CallingConv::ID CallConv,
                                  MachineFunction &MF, bool isVarArg,
                                  const SmallVectorImpl<ISD::OutputArg> &Outs,
                                  LLVMContext &Context) const
{
  SmallVector<CCValAssign, 16> RVLocs;
  CCState CCInfo(CallConv, isVarArg, MF, RVLocs, Context);

  auto CCFunction = CCAssignFnForReturn(CallConv);
  return CCInfo.CheckReturn(Outs, CCFunction);
}

SDValue
GBZ80TargetLowering::LowerReturn(SDValue Chain, CallingConv::ID CallConv,
                               bool isVarArg,
                               const SmallVectorImpl<ISD::OutputArg> &Outs,
                               const SmallVectorImpl<SDValue> &OutVals,
                               const SDLoc &dl, SelectionDAG &DAG) const {
  // CCValAssign - represent the assignment of the return value to locations.
  SmallVector<CCValAssign, 16> RVLocs;

  // CCState - Info about the registers and stack slot.
  CCState CCInfo(CallConv, isVarArg, DAG.getMachineFunction(), RVLocs,
                 *DAG.getContext());

  // Analyze return values.
  auto CCFunction = CCAssignFnForReturn(CallConv);
  CCInfo.AnalyzeReturn(Outs, CCFunction);

  // If this is the first return lowered for this function, add the regs to
  // the liveout set for the function.
  MachineFunction &MF = DAG.getMachineFunction();
  unsigned e = RVLocs.size();

  // Reverse splitted return values to get the "big endian" format required
  // to agree with the calling convention ABI.
  if (e > 1) {
    std::reverse(RVLocs.begin(), RVLocs.end());
  }

  SDValue Flag;
  SmallVector<SDValue, 4> RetOps(1, Chain);
  // Copy the result values into the output registers.
  for (unsigned i = 0; i != e; ++i) {
    CCValAssign &VA = RVLocs[i];
    assert(VA.isRegLoc() && "Can only return in registers!");

    Chain = DAG.getCopyToReg(Chain, dl, VA.getLocReg(), OutVals[i], Flag);

    // Guarantee that all emitted copies are stuck together with flags.
    Flag = Chain.getValue(1);
    RetOps.push_back(DAG.getRegister(VA.getLocReg(), VA.getLocVT()));
  }

  // Don't emit the ret/reti instruction when the naked attribute is present in
  // the function being compiled.
  if (MF.getFunction().getAttributes().hasAttribute(
          AttributeList::FunctionIndex, Attribute::Naked)) {
    return Chain;
  }

  // TODO: Emit RETI for interrupt handlers.
  unsigned RetOpc =
      /*(CallConv == CallingConv::GBZ80_INTR || CallConv == CallingConv::GBZ80_SIGNAL)
          ? GBISD::RETI_FLAG
          :*/ GBISD::RET_FLAG;

  RetOps[0] = Chain; // Update chain.

  if (Flag.getNode()) {
    RetOps.push_back(Flag);
  }

  return DAG.getNode(RetOpc, dl, MVT::Other, RetOps);
}

//===----------------------------------------------------------------------===//
//  Custom Inserters
//===----------------------------------------------------------------------===//

/*
static bool isCopyMulResult(MachineBasicBlock::iterator const &I) {
  if (I->getOpcode() == GB::COPY) {
    unsigned SrcReg = I->getOperand(1).getReg();
    return (SrcReg == GB::R0 || SrcReg == GB::R1);
  }

  return false;
}
*/

// The mul instructions wreak havock on our zero_reg R1. We need to clear it
// after the result has been evacuated. This is probably not the best way to do
// it, but it works for now.
MachineBasicBlock *GBZ80TargetLowering::insertMul(MachineInstr &MI,
                                                MachineBasicBlock *BB) const {
  /*
  const GBZ80TargetMachine &TM = (const GBZ80TargetMachine &)getTargetMachine();
  const TargetInstrInfo &TII = *TM.getSubtargetImpl()->getInstrInfo();
  MachineBasicBlock::iterator I(MI);
  ++I; // in any case insert *after* the mul instruction
  if (isCopyMulResult(I))
    ++I;
  if (isCopyMulResult(I))
    ++I;
  BuildMI(*BB, I, MI.getDebugLoc(), TII.get(GB::EORRdRr), GB::R1)
      .addReg(GB::R1)
      .addReg(GB::R1);
      */
  return BB;
}

MachineBasicBlock *
GBZ80TargetLowering::EmitInstrWithCustomInserter(MachineInstr &MI,
                                               MachineBasicBlock *MBB) const {
  llvm_unreachable("Custom inserters are disabled.");
  return nullptr;
}

//===----------------------------------------------------------------------===//
//  Inline Asm Support
//===----------------------------------------------------------------------===//
/*
GBZ80TargetLowering::ConstraintType
GBZ80TargetLowering::getConstraintType(StringRef Constraint) const {
 
  return TargetLowering::getConstraintType(Constraint);
}

unsigned
GBZ80TargetLowering::getInlineAsmMemConstraint(StringRef ConstraintCode) const {
  
  return TargetLowering::getInlineAsmMemConstraint(ConstraintCode);
}

GBZ80TargetLowering::ConstraintWeight
GBZ80TargetLowering::getSingleConstraintMatchWeight(
    AsmOperandInfo &info, const char *constraint) const {
  ConstraintWeight weight = CW_Invalid;

  return weight;
}

std::pair<unsigned, const TargetRegisterClass *>
GBZ80TargetLowering::getRegForInlineAsmConstraint(const TargetRegisterInfo *TRI,
                                                StringRef Constraint,
                                                MVT VT) const {

  return TargetLowering::getRegForInlineAsmConstraint(STI->getRegisterInfo(),
                                                      Constraint, VT);
}

void GBZ80TargetLowering::LowerAsmOperandForConstraint(SDValue Op,
                                                     std::string &Constraint,
                                                     std::vector<SDValue> &Ops,
                                                     SelectionDAG &DAG) const {

  return TargetLowering::LowerAsmOperandForConstraint(Op, Constraint, Ops, DAG);
}

unsigned GBZ80TargetLowering::getRegisterByName(const char *RegName,
                                              EVT VT,
                                              SelectionDAG &DAG) const {
  unsigned Reg;

  if (VT == MVT::i8) {
    Reg = StringSwitch<unsigned>(RegName)
      .Case("r0", GB::R0).Case("r1", GB::R1).Case("r2", GB::R2)
      .Case("r3", GB::R3).Case("r4", GB::R4).Case("r5", GB::R5)
      .Case("r6", GB::R6).Case("r7", GB::R7).Case("r8", GB::R8)
      .Case("r9", GB::R9).Case("r10", GB::R10).Case("r11", GB::R11)
      .Case("r12", GB::R12).Case("r13", GB::R13).Case("r14", GB::R14)
      .Case("r15", GB::R15).Case("r16", GB::R16).Case("r17", GB::R17)
      .Case("r18", GB::R18).Case("r19", GB::R19).Case("r20", GB::R20)
      .Case("r21", GB::R21).Case("r22", GB::R22).Case("r23", GB::R23)
      .Case("r24", GB::R24).Case("r25", GB::R25).Case("r26", GB::R26)
      .Case("r27", GB::R27).Case("r28", GB::R28).Case("r29", GB::R29)
      .Case("r30", GB::R30).Case("r31", GB::R31)
      .Case("X", GB::R27R26).Case("Y", GB::R29R28).Case("Z", GB::R31R30)
      .Default(0);
  } else {
    Reg = StringSwitch<unsigned>(RegName)
      .Case("r0", GB::R1R0).Case("r2", GB::R3R2)
      .Case("r4", GB::R5R4).Case("r6", GB::R7R6)
      .Case("r8", GB::R9R8).Case("r10", GB::R11R10)
      .Case("r12", GB::R13R12).Case("r14", GB::R15R14)
      .Case("r16", GB::R17R16).Case("r18", GB::R19R18)
      .Case("r20", GB::R21R20).Case("r22", GB::R23R22)
      .Case("r24", GB::R25R24).Case("r26", GB::R27R26)
      .Case("r28", GB::R29R28).Case("r30", GB::R31R30)
      .Case("X", GB::R27R26).Case("Y", GB::R29R28).Case("Z", GB::R31R30)
      .Default(0);
  }

  if (Reg)
    return Reg;

  report_fatal_error("Invalid register name global variable");
}
*/
} // end of namespace llvm

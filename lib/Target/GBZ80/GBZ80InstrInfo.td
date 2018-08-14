//===-- GBZ80InstrInfo.td - GBZ80 Instruction defs -------------*- tablegen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file describes the GBZ80 instructions in TableGen format.
//
//===----------------------------------------------------------------------===//

include "GBZ80InstrFormats.td"

//===----------------------------------------------------------------------===//
// GBZ80 Type Profiles
//===----------------------------------------------------------------------===//

def SDT_GBZ80CallSeqStart : SDCallSeqStart<[SDTCisVT<0, i16>, SDTCisVT<1, i16>]>;
def SDT_GBZ80CallSeqEnd : SDCallSeqEnd<[SDTCisVT<0, i16>, SDTCisVT<1, i16>]>;
def SDT_GBZ80Call : SDTypeProfile<0, -1, [SDTCisVT<0, iPTR>]>;
def SDT_GBZ80Wrapper : SDTypeProfile<1, 1, [SDTCisSameAs<0, 1>, SDTCisPtrTy<0>]>;
def SDT_GBZ80Brcond : SDTypeProfile<0, 2,
                                  [SDTCisVT<0, OtherVT>, SDTCisVT<1, i8>]>;
def SDT_GBZ80Br16 : SDTypeProfile<0, 4,
                                  [SDTCisVT<0, OtherVT>, SDTCisVT<1, i8>,
                                   SDTCisSameAs<2, 3>]>;
def SDT_GBZ80Cmp : SDTypeProfile<0, 2, [SDTCisSameAs<0, 1>]>;
def SDT_GBZ80Tst : SDTypeProfile<0, 1, [SDTCisInt<0>]>;

// Select-branch. This takes (true, false, cc) and also takes a condcode from a
// compare.
def SDT_GBZ80SelectCC : SDTypeProfile<1, 3, [SDTCisSameAs<0, 1>,
                                    SDTCisSameAs<1, 2>, SDTCisVT<3, i8>]>;

// Select-and-compare. This takes (true, false, cc, lhs, rhs).
// Expanded after ISEL.
def SDT_GBZ80SelectBR : SDTypeProfile<1, 5, [SDTCisSameAs<0, 1>,
                                    SDTCisSameAs<1, 2>, SDTCisVT<3, i8>,
                                    SDTCisSameAs<4, 5>]>;

//===----------------------------------------------------------------------===//
// GBZ80 Specific Node Definitions
//===----------------------------------------------------------------------===//

def GBZ80retflag : SDNode<"GBISD::RET_FLAG", SDTNone,
                        [SDNPHasChain, SDNPOptInGlue, SDNPVariadic]>;
def GBZ80retiflag : SDNode<"GBISD::RETI_FLAG", SDTNone,
                         [SDNPHasChain, SDNPOptInGlue, SDNPVariadic]>;

def GBZ80callseq_start : SDNode<"ISD::CALLSEQ_START", SDT_GBZ80CallSeqStart,
                              [SDNPHasChain, SDNPOutGlue]>;
def GBZ80callseq_end : SDNode<"ISD::CALLSEQ_END", SDT_GBZ80CallSeqEnd,
                            [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue]>;

def GBZ80call : SDNode<"GBISD::CALL", SDT_GBZ80Call,
                     [SDNPHasChain, SDNPOutGlue, SDNPOptInGlue, SDNPVariadic]>;

def GBZ80Wrapper : SDNode<"GBISD::WRAPPER", SDT_GBZ80Wrapper>;

def GBZ80brcond : SDNode<"GBISD::BRCOND", SDT_GBZ80Brcond,
                       [SDNPHasChain, SDNPInGlue]>;
def GBZ80br16 : SDNode<"GBISD::BR16", SDT_GBZ80Br16,
                       [SDNPHasChain]>;
def GBZ80cmp : SDNode<"GBISD::CMP", SDT_GBZ80Cmp, [SDNPOutGlue]>;
def GBZ80tst : SDNode<"GBISD::TST", SDT_GBZ80Tst, [SDNPOutGlue]>;
def GBZ80selectcc: SDNode<"GBISD::SELECT_CC", SDT_GBZ80SelectCC, [SDNPInGlue]>;
def GBZ80selectbr: SDNode<"GBISD::SELECT_BR", SDT_GBZ80SelectBR>;

// Shift nodes.
def GBZ80lsl : SDNode<"GBISD::LSL", SDTIntUnaryOp>;
def GBZ80lsr : SDNode<"GBISD::LSR", SDTIntUnaryOp>;
def GBZ80asr : SDNode<"GBISD::ASR", SDTIntUnaryOp>;
def GBZ80rol : SDNode<"GBISD::ROL", SDTIntUnaryOp>;
def GBZ80ror : SDNode<"GBISD::ROR", SDTIntUnaryOp>;


// Pseudo shift nodes for non-constant shift amounts.
def GBZ80lslLoop : SDNode<"GBISD::LSLLOOP", SDTIntShiftOp>;
def GBZ80lsrLoop : SDNode<"GBISD::LSRLOOP", SDTIntShiftOp>;
def GBZ80asrLoop : SDNode<"GBISD::ASRLOOP", SDTIntShiftOp>;
def GBZ80rolLoop : SDNode<"GBISD::ROLLOOP", SDTIntShiftOp>;
def GBZ80rorLoop : SDNode<"GBISD::RORLOOP", SDTIntShiftOp>;

//===----------------------------------------------------------------------===//
// GBZ80 Operands, Complex Patterns and Transformations Definitions.
//===----------------------------------------------------------------------===//

def imm8_neg_XFORM : SDNodeXForm<imm,
[{
  return CurDAG->getTargetConstant(-N->getAPIntValue(), SDLoc(N), MVT::i8);
}]>;

def imm16_neg_XFORM : SDNodeXForm<imm,
[{
  return CurDAG->getTargetConstant(-N->getAPIntValue(), SDLoc(N), MVT::i16);
}]>;

def imm0_63_neg : PatLeaf<(imm),
[{
  int64_t val = -N->getSExtValue();
  return val >= 0 && val < 64;
}], imm16_neg_XFORM>;

let PrintMethod = "printUImm8" in {
  def uimm3 : PatLeaf<(imm), [{ return isUInt<3>(N->getZExtValue()); }]>,
              Operand<i8>;
  def imm8 : PatLeaf<(imm), [{ return isUInt<8>(N->getZExtValue()) ||
                                      isInt<8>(N->getSExtValue()); }]>,
             Operand<i8>;
}
def simm8 : PatLeaf<(imm), [{ return isInt<8>(N->getSExtValue()); }]>, Operand<i8>;

let PrintMethod = "printUImm16" in {
  def uimm16 : PatLeaf<(imm), [{ return isUInt<16>(N->getZExtValue()); }]>,
               Operand<i16>;
  def imm16 : PatLeaf<(imm), [{ return isUInt<16>(N->getZExtValue()) ||
                                       isInt<16>(N->getSExtValue()); }]>,
              Operand<i16>;
}

def uimm8p2_XFORM : SDNodeXForm<imm,
[{
  return CurDAG->getTargetConstant(Log2_32(uint8_t(N->getZExtValue())),
                                   SDLoc(N), MVT::i8);
}]>;
def uimm8p2 : PatLeaf<(imm), [{
  return isPowerOf2_32(uint8_t(N->getZExtValue()));
}], uimm8p2_XFORM>, Operand<i8>;

def uimm8p2n_XFORM : SDNodeXForm<imm,
[{
  return CurDAG->getTargetConstant(Log2_32(uint8_t(~N->getZExtValue())),
                                   SDLoc(N), MVT::i8);
}]>;
def uimm8p2n : PatLeaf<(imm), [{
  return isPowerOf2_32(uint8_t(~N->getZExtValue()));
}], uimm8p2n_XFORM>, Operand<i8>;



def uimm16hip2_XFORM : SDNodeXForm<imm,
[{
  return CurDAG->getTargetConstant(Log2_32(uint16_t(N->getZExtValue())) - 8,
                                   SDLoc(N), MVT::i8);
}]>;
def uimm16hip2 : PatLeaf<(imm), [{
  return isPowerOf2_32(uint16_t(N->getZExtValue())) &&
         Log2_32(uint16_t(N->getZExtValue())) >= 8;
}], uimm16hip2_XFORM>, Operand<i16>;

def uimm16lop2_XFORM : SDNodeXForm<imm,
[{
  return CurDAG->getTargetConstant(Log2_32(uint16_t(N->getZExtValue())),
                                   SDLoc(N), MVT::i8);
}]>;
def uimm16lop2 : PatLeaf<(imm), [{
  return isPowerOf2_32(uint16_t(N->getZExtValue())) &&
         Log2_32(uint16_t(N->getZExtValue())) < 8;
}], uimm16lop2_XFORM>, Operand<i16>;
def uimm16hip2n_XFORM : SDNodeXForm<imm,
[{
  return CurDAG->getTargetConstant(Log2_32(uint16_t(~N->getZExtValue())) - 8,
                                   SDLoc(N), MVT::i8);
}]>;
def uimm16hip2n : PatLeaf<(imm), [{
  return isPowerOf2_32(uint16_t(~N->getZExtValue())) &&
         Log2_32(uint16_t(~N->getZExtValue())) >= 8;
}], uimm16hip2n_XFORM>, Operand<i16>;

def uimm16lop2n_XFORM : SDNodeXForm<imm,
[{
  return CurDAG->getTargetConstant(Log2_32(uint16_t(~N->getZExtValue())),
                                   SDLoc(N), MVT::i8);
}]>;
def uimm16lop2n : PatLeaf<(imm), [{
  return isPowerOf2_32(uint16_t(~N->getZExtValue())) &&
         Log2_32(uint16_t(N->getZExtValue())) < 8;
}], uimm16lop2n_XFORM>, Operand<i16>;




// Relative jump target, either a signed immediate or a basic block.
def relbrtarget8 : Operand<OtherVT> {
  let PrintMethod   = "printPCRelImm";
  //let EncoderMethod = "encodeRelCondBrTarget<AVR::fixup_7_pcrel>";
}

// Absolute jump target, either an address or a basic block.
def brtarget16 : Operand<OtherVT> {
  let PrintMethod   = "printPCRelImm";
  //let EncoderMethod = "encodeRelCondBrTarget<AVR::fixup_13_pcrel>";
}

def calltarget : Operand<i16> {
//  let EncoderMethod = "encodeCallTarget";
}

// Condcode.
def condcode : PatLeaf<(imm)>, Operand<i8> {
  let PrintMethod   = "printCondCode";
}


def HRAMImmAddr_XFORM : SDNodeXForm<imm,
[{
  return CurDAG->getTargetConstant(N->getZExtValue() - 0xFF00,
                                   SDLoc(N), MVT::i8);
}]>;

def HRAMImmAddr : PatLeaf<(imm),
[{
  uint64_t val = N->getZExtValue();
  return val >= 0xFF00 && val <= 0xFFFF;
}], HRAMImmAddr_XFORM>;

def iaddr : ComplexPattern<i16, 1, "SelectIAddr", [], []>; 

// Match for HRAM indirect reg. e.g. LD [$FF00+C]
//def HRAMRegAddr : ComplexPattern<iPTR, 1, "SelectHRAMRegAddr", [add], [SDNPWantRoot]>;


// Output pattern fragments.
def toHi      : OutPatFrag<(ops node:$in),
  (i8 (EXTRACT_SUBREG (i16 $in), sub_hi))>;
def toLo      : OutPatFrag<(ops node:$in),
  (i8 (EXTRACT_SUBREG (i16 $in), sub_lo))>;
def buildPair : OutPatFrag<(ops node:$hi, node:$lo),
  (i16 (REG_SEQUENCE Pairs, (i8 $hi), sub_hi, (i8 $lo), sub_lo))>;


//===----------------------------------------------------------------------===//
// GBZ80 predicates for subtarget features
//===----------------------------------------------------------------------===//



// GBZ80 specific condition code. These correspond to GBZ80_*_COND in
// GBZ80InstrInfo.td. They must be kept in synch.
def GBZ80_COND_NZ : PatLeaf<(i8 0)>;
def GBZ80_COND_Z  : PatLeaf<(i8 1)>;
def GBZ80_COND_NC : PatLeaf<(i8 2)>;
def GBZ80_COND_C  : PatLeaf<(i8 3)>;

//===----------------------------------------------------------------------===//
//===----------------------------------------------------------------------===//
// GBZ80 Instruction list
//===----------------------------------------------------------------------===//
//===----------------------------------------------------------------------===//

// ADJCALLSTACKDOWN/UP implicitly use/def SP because they may be expanded into
// a stack adjustment and the codegen must know that they may modify the stack
// pointer before prolog-epilog rewriting occurs.
// Pessimistically assume ADJCALLSTACKDOWN / ADJCALLSTACKUP will become
// sub / add which can clobber SREG.
let Defs = [SP, rF],
Uses = [SP] in
{
  def ADJCALLSTACKDOWN : Pseudo<(outs),
                                (ins i16imm:$amt, i16imm:$amt2),
                                "#ADJCALLSTACKDOWN",
                                [(GBZ80callseq_start timm:$amt, timm:$amt2)]>;

  def ADJCALLSTACKUP : Pseudo<(outs),
                              (ins i16imm:$amt1, i16imm:$amt2),
                              "#ADJCALLSTACKUP",
                              [(GBZ80callseq_end timm:$amt1, timm:$amt2)]>;
}

///===============================
/// Moves
///===============================

let isAsCheapAsAMove = 1 in
def LD_r_r : I8<(outs GPR8:$dst), (ins GPR8:$src),
  "LD\t$dst, $src",
  []>;

let isMoveImm = 1, isReMaterializable = 1 in {
  def LD_r_n : I16<(outs GPR8:$dst), (ins imm8:$imm),
    "LD\t$dst, $imm",
    [(set GPR8:$dst, imm8:$imm)]>;

  def LD_dd_nn : I24<(outs DDPairs:$dst), (ins imm16:$imm),
    "LD\t$dst, $imm",
    [(set DDPairs:$dst, imm16:$imm)]>;
}

let Defs = [SP], Uses = [rHL] in
def LD_SP_HL : I8<(outs), (ins),
  "LD\tSP, HL",
  []>;

///===============================
/// Loads
///===============================

let mayLoad = 1 in {
  def LD8 : Pseudo<(outs AReg:$dst), (ins Pairs:$ptr),
    "LD8\t$dst, [$ptr]",
    [(set i8:$dst, (load i16:$ptr))]>;

  let Constraints = "$ptr = $post" in {
    def LD8_INC : Pseudo<(outs AReg:$dst, Pairs:$post), (ins Pairs:$ptr),
      "LD8\t$dst, [$ptr+]",
      []>;

    def LD8_DEC : Pseudo<(outs AReg:$dst, Pairs:$post), (ins Pairs:$ptr),
      "LD8\t$dst, [$ptr-]",
      []>;
  }
}

// Real instructions below.

let mayLoad = 1 in
def LD_r_HL : I8<(outs GPR8:$dst), (ins HLPairs:$src),
  "LD\t$dst, [HL]",
  []>;

let mayLoad = 1 in
def LD_A_dd : I8<(outs AReg:$dst), (ins BCDEPairs:$ptr),
  "LD\tA, [$ptr]",
  []>;

let mayLoad = 1 in
def LDH_A_C : I8<(outs AReg:$dst), (ins CReg:$reg),
  "LD\t$dst, [$$FF00+C]",
  []>;

let mayLoad = 1 in
def LDH_A_n : I16<(outs AReg:$dst), (ins imm8:$offs),
  "LDH\tA, [$$FF00+$offs]",
  [(set AReg:$dst, (load HRAMImmAddr:$offs))]>;

let mayLoad = 1 in
def LD_A_nn : I24<(outs AReg:$dst), (ins uimm16:$addr),
  "LD\tA, [$addr]",
  [(set AReg:$dst, (load imm:$addr))]>;

let Constraints = "$ptr = $post", mayLoad = 1 in {
  def LD_A_HLI : I8<(outs AReg:$dst, HLPairs:$post), (ins HLPairs:$ptr),
    "LD\tA, [HL+]",
    []>;
  def LD_A_HLD : I8<(outs AReg:$dst, HLPairs:$post), (ins HLPairs:$ptr),
    "LD\tA, [HL-]",
    []>;
}




///===============================
/// Stores
///===============================

let mayStore = 1 in {
  def ST8 : Pseudo<(outs), (ins AReg:$src, Pairs:$ptr),
    "ST8\t[$ptr], $src",
    [(store i8:$src, i16:$ptr)]>;

  let Constraints = "$ptr = $post" in {
    def ST8_INC : Pseudo<(outs Pairs:$post), (ins AReg:$src, Pairs:$ptr),
      "ST8\t[$ptr+], $src",
      []>;

    def ST8_DEC : Pseudo<(outs Pairs:$post), (ins AReg:$src, Pairs:$ptr),
      "ST8\t[$ptr-], $src",
      []>;
  }
}


let mayStore = 1 in
def LD_HL_r : I8<(outs), (ins GPR8:$src, HLPairs:$ptr),
  "LD\t[HL], $src",
  []>;

let mayStore = 1 in
def LD_HL_n : I16<(outs), (ins imm8:$imm, HLPairs:$ptr),
  "LD\t[HL], $imm",
  []>;

let mayStore = 1 in
def LDH_C_A : I8<(outs), (ins AReg:$src, CReg:$reg),
  "LD\t[$$FF00+C], A",
  []>;

let mayStore = 1 in
def LDH_n_A : I16<(outs), (ins AReg:$src, imm8:$offs),
  "LDH\t[$$FF00+$offs], A",
  [(store AReg:$src, HRAMImmAddr:$offs)]>;

let mayStore = 1 in
def LD_nn_A : I24<(outs), (ins AReg:$src, uimm16:$addr),
  "LD\t[$addr], A",
  [(store AReg:$src, imm:$addr)]>;

let mayStore = 1 in
def LD_dd_A : I8<(outs), (ins AReg:$src, BCDEPairs:$ptr),
  "LD\t[$ptr], A",
  []>;

let Uses = [SP], mayStore = 1 in
def LD_nn_SP : I24<(outs), (ins uimm16:$addr),
  "LD\t[$addr], SP",
  []>;

let Constraints = "$ptr = $post", mayStore = 1 in {
  def LD_HLI_A : I8<(outs HLPairs:$post), (ins AReg:$src, HLPairs:$ptr),
    "LD\t[HL+], A",
    []>;
  def LD_HLD_A : I8<(outs HLPairs:$post), (ins AReg:$src, HLPairs:$ptr),
    "LD\t[HL-], A",
    []>;
}

///===============================
/// Push and pop
///===============================

let Defs = [SP], Uses = [SP] in {
  let mayStore = 1 in
  def PUSH : I8<(outs), (ins AllPairs:$src),
    "PUSH\t$src",
    []>;
  
  let mayLoad = 1 in
  def POP : I8<(outs AllPairs:$dst), (ins),
    "POP\t$dst",
    []>;
}










///===============================
/// Arithmetic operations 
///===============================

/// It also defines a set of pseudoops for the operation, which take generic 8-
/// and 16-bit registers and immediates. These pseudos are two-address.
///   OP8r
///   OP8i
///   OP16r
///   OP16i
multiclass ALUOp<string Name, SDNode MatchOp, list<Register> ExtraUses = [],
                 bit IsComm = 0> {
  let Uses = ExtraUses, Defs = [rF], Constraints = "$dst = $a" in {
    let isCommutable = IsComm in
    def _r : I8<(outs AReg:$dst), (ins AReg:$a, GPR8:$src),
      !strconcat(Name, "\tA, $src"),
      [(set AReg:$dst, (MatchOp AReg:$a, GPR8:$src))]>;
    def _n : I16<(outs AReg:$dst), (ins AReg:$a, imm8:$imm),
      !strconcat(Name, "\tA, $imm"),
      [(set AReg:$dst, (MatchOp AReg:$a, imm8:$imm))]>;
    let mayLoad = 1 in
    def _HL : I8<(outs AReg:$dst), (ins AReg:$a, HLPairs:$ptr),
      !strconcat(Name, "\tA, [HL]"),
      []>;
  }

  let Uses = ExtraUses, Defs = [rF, rA], Constraints = "$dst = $src1" in {
    let isCommutable = IsComm in
    def 16r : Pseudo<(outs Pairs:$dst), (ins Pairs:$src1, Pairs:$src2),
      !strconcat(Name, "16r\t$dst, $src2"),
      [(set Pairs:$dst, (MatchOp Pairs:$src1, Pairs:$src2))]>;

    def 16i : Pseudo<(outs Pairs:$dst), (ins Pairs:$src1, imm16:$imm),
      !strconcat(Name, "16i\t$dst, $imm"),
      [(set Pairs:$dst, (MatchOp Pairs:$src1, imm16:$imm))]>;
  }
}

defm ADD : ALUOp<"ADD", add,  [],   1>;
defm ADC : ALUOp<"ADC", adde, [rF], 1>;
defm SUB : ALUOp<"SUB", sub>;
defm SBC : ALUOp<"SBC", sube, [rF]>;
defm AND : ALUOp<"AND", and,  [],   1>;
defm OR  : ALUOp<"OR",  or,   [],   1>;
defm XOR : ALUOp<"XOR", xor,  [],   1>;

// TODO: Make pseudos for cp when the format is decided.
let Defs = [rF] in {
  def CP_r : I8<(outs), (ins AReg:$a, GPR8:$rhs),
   "CP\tA, $rhs",
    [(GBZ80cmp i8:$a, i8:$rhs)]>;
  def CP_n : I16<(outs), (ins AReg:$a, imm8:$imm),
    "CP\tA, $imm",
    [(GBZ80cmp i8:$a, imm8:$imm)]>;
  let mayLoad = 1 in
  def CP_HL : I8<(outs), (ins AReg:$a, HLPairs:$ptr),
    "CP\tA, [HL]",
    []>;
}

/*
// Skip this for now
let Defs = []
def MUL8 : Pseudo<(outs GPR8:$dst), (ins GPR8:$src1, GPR8:$src2),
  "MUL8\t$dst, $src",
  []>;
*/

///===============================
/// Increment / Decrement
///===============================
foreach Name = ["INC", "DEC"] in {
  // 8-bit
  let Defs = [rF], Constraints = "$dst = $src" in
  def Name##_r : I8<(outs GPR8:$dst), (ins GPR8:$src),
    !strconcat(Name, "\t$src"),
    []>;
  let Uses = [rHL], Defs = [rF, rA], mayLoad = 1 in
  def Name##_HL : I8<(outs), (ins),
    !strconcat(Name, "\t[HL]"),
    []>;

  // 16-bit
  let Constraints = "$dst = $src" in
  def Name##_ss : I8<(outs Pairs:$dst), (ins Pairs:$src),
    !strconcat(Name, "\t$src"),
    []>;
}

///===============================
/// Other 16-bit arithmetic
///===============================

// This can be thought of as a specialization of the pseudo
// ADD16r when dst is HL.
// Use for MUL too
let Defs = [rF], Constraints = "$dst = $hl" in
def ADD_HL_ss : I8<(outs HLPairs:$dst), (ins HLPairs:$hl, DDPairs:$src),
  "ADD\tHL, $src",
  []>;

let Uses = [SP], Defs = [SP, rF] in
def ADD_SP_e : I16<(outs), (ins simm8:$imm),
  "ADD\tSP, $imm",
  []>;

// Should this format the +/-?
let Uses = [SP], Defs = [rF] in
def LDHL_SP_e : I16<(outs HLPairs:$dst), (ins simm8:$imm),
  "LD\tHL, SP+$imm",
  []>;

// This pseudo will load the frame index given in idx. It does this
// by summoning a demon that brutally murders everyone. kill me
//
// The offset will always be positive because the stack pointer is
// also the frame pointer, so it always points to the end of the
// current AR.
//
// PUSH HL (if HL is not the def)
// LDHL SP+offset
// if offset > 127 (?)
//  SUBW/ADDW HL, remaining offset (which is more efficient/possible?)
//    (perhaps use DEC for a small difference? what difference?)
// COPY $dst, HL (if HL is not the def)
// POP HL (if HL is not the def)
//
// This clobbers F, since we use the LDHL instruction. It can also
// clobber A, since we might need to add an extra offset to the
// pointer
// XXX: Let's skip pointer correction for now.
let Uses = [SP], Defs = [rF], mayStore = 1, mayLoad = 1 in
def FRMIDX : Pseudo<(outs Pairs:$dst), (ins i16imm:$idx, i16imm:$offs),
  "FRMIDX\t$dst, $idx, $offs",
  []>;

// Load/store a value from the stack at the given IDX. This expands to
// code similar to FRMIDX, except it doesn't emit a pointer.
//
// Load Example:
// PUSH HL
// LDHL SP+offset
// if offset > 127 (?)
//  SUBW/ADDW HL, remaining offset (which is more efficient/possible?)
//    (perhaps use DEC for a small difference? what difference?)
// LD A, [HL] (do we know if H or L are dst? if they aren't, emit directly)
// POP HL
// LD $dst, A (if H or L are dst)
let Uses = [SP], Defs = [rF, rA], mayStore = 1, mayLoad = 1 in
def FRMIDX_Load8 : Pseudo<(outs GPR8:$dst), (ins i16imm:$idx, i16imm:$offs),
  "FRMIDX_L8\t$dst, $idx, $offs",
  []>;
let Uses = [SP], Defs = [rF, rA], mayStore = 1, mayLoad = 1 in
def FRMIDX_Store8 : Pseudo<(outs), (ins i16imm:$idx, i16imm:$offs, GPR8:$src),
  "FRMIDX_S8\t$src, $idx, $offs",
  []>;

// TODO: 16-bit frame index load/store.

// TODO: frame index increment/decrement? identify if this is worth it

///===============================
/// Rotations and shifts
///===============================

let Constraints = "$dst = $src" in {
  let Defs = [rF] in
  def RLCA : I8<(outs AReg:$dst), (ins AReg:$src),
    "RLCA",
    []>;
  let Defs = [rF], Uses = [rF] in
  def RLA : I8<(outs AReg:$dst), (ins AReg:$src),
    "RLA",
    []>;
  let Defs = [rF] in
  def RRCA : I8<(outs AReg:$dst), (ins AReg:$src),
    "RRCA",
    []>;
  let Defs = [rF], Uses = [rF] in
  def RRA : I8<(outs AReg:$dst), (ins AReg:$src),
    "RRA",
    []>;

  let Defs = [rF] in
  def RLC_r : I16<(outs GPR8:$dst), (ins GPR8:$src),
    "RLC\t$dst",
    [(set GPR8:$dst, (GBZ80rol GPR8:$src))]>;

  let Uses = [rF], Defs = [rF] in
  def RL_r : I16<(outs GPR8:$dst), (ins GPR8:$src),
    "RL\t$dst",
    []>;

  let Defs = [rF] in
  def RRC_r : I16<(outs GPR8:$dst), (ins GPR8:$src),
    "RRC\t$dst",
    [(set GPR8:$dst, (GBZ80ror GPR8:$src))]>;

  let Uses = [rF], Defs = [rF] in
  def RR_r : I16<(outs GPR8:$dst), (ins GPR8:$src),
    "RR\t$dst",
    []>;

  let Defs = [rF] in 
  def SLA_r : I16<(outs GPR8:$dst), (ins GPR8:$src),
    "SLA\t$dst",
    [(set GPR8:$dst, (GBZ80lsl GPR8:$src))]>;

  let Defs = [rF] in 
  def SRA_r : I16<(outs GPR8:$dst), (ins GPR8:$src),
    "SRA\t$dst",
    [(set GPR8:$dst, (GBZ80asr GPR8:$src))]>;

  let Defs = [rF] in 
  def SRL_r : I16<(outs GPR8:$dst), (ins GPR8:$src),
    "SRL\t$dst",
    [(set GPR8:$dst, (GBZ80lsr GPR8:$src))]>;

  let Defs = [rF] in 
  def SWAP_r : I16<(outs GPR8:$dst), (ins GPR8:$src),
    "SWAP\t$dst",
    [(set GPR8:$dst, (bswap GPR8:$src))]>;
}

let Defs = [rF], mayLoad = 1, mayStore = 1 in {
  def RLC_HL : I16<(outs), (ins HLPairs:$ptr),
    "RLC\t[HL]",
    []>;

  let Uses = [rF] in
  def RL_HL : I16<(outs), (ins HLPairs:$ptr),
    "RL\t[HL]",
    []>;

  def RRC_HL : I16<(outs), (ins HLPairs:$ptr),
    "RRC\t[HL]",
    []>;

  let Uses = [rF] in
  def RR_HL : I16<(outs), (ins HLPairs:$ptr),
    "RR\t[HL]",
    []>;

  def SLA_HL : I16<(outs), (ins HLPairs:$ptr),
    "SLA\t[HL]",
    []>;

  def SRA_HL : I16<(outs), (ins HLPairs:$ptr),
    "SRA\t[HL]",
    []>;

  def SRL_HL : I16<(outs), (ins HLPairs:$ptr),
    "SRL\t[HL]",
    []>;
  
  def SWAP_HL : I16<(outs), (ins HLPairs:$ptr),
    "SWAP\t[HL]",
    []>;
}

///===============================
/// ISel shift pseudos
///===============================

def IselLsl8 : ShiftPseudo<
  (outs GPR8:$dst),
  (ins GPR8:$src, GPR8:$cnt),
  "# Lsl8 PSEUDO",
  [(set i8:$dst, (GBZ80lslLoop i8:$src, i8:$cnt))]
>;

def IselLsl16 : ShiftPseudo<
  (outs Pairs:$dst),
  (ins Pairs:$src, GPR8:$cnt),
  "# Lsl16 PSEUDO",
  [(set i16:$dst, (GBZ80lslLoop i16:$src, i8:$cnt))]
>;

def IselLsr8 : ShiftPseudo<
  (outs GPR8:$dst),
  (ins GPR8:$src, GPR8:$cnt),
  "# Lsr8 PSEUDO",
  [(set i8:$dst, (GBZ80lsrLoop i8:$src, i8:$cnt))]
>;

def IselLsr16 : ShiftPseudo<
  (outs Pairs:$dst),
   (ins Pairs:$src, GPR8:$cnt),
   "# Lsr16 PSEUDO",
   [(set i16:$dst, (GBZ80lsrLoop i16:$src, i8:$cnt))]
>;

def IselRol8 : ShiftPseudo<
  (outs GPR8:$dst),
  (ins GPR8:$src, GPR8:$cnt),
  "# Rol8 PSEUDO",
  [(set i8:$dst, (GBZ80rolLoop i8:$src, i8:$cnt))]
>;

def IselRol16 : ShiftPseudo<
  (outs Pairs:$dst),
  (ins Pairs:$src, GPR8:$cnt),
  "# Rol16 PSEUDO",
  [(set i16:$dst, (GBZ80rolLoop i16:$src, i8:$cnt))]
>;

def IselRor8 : ShiftPseudo<
  (outs GPR8:$dst),
  (ins GPR8:$src, GPR8:$cnt),
  "# Ror8 PSEUDO",
  [(set i8:$dst, (GBZ80rorLoop i8:$src, i8:$cnt))]
>;

def IselRor16 : ShiftPseudo<
  (outs Pairs:$dst),
  (ins Pairs:$src, GPR8:$cnt),
  "# Ror16 PSEUDO",
  [(set i16:$dst, (GBZ80rorLoop i16:$src, i8:$cnt))]
>;

def IselAsr8 : ShiftPseudo<
  (outs GPR8:$dst),
  (ins GPR8:$src, GPR8:$cnt),
  "# Asr8 PSEUDO",
  [(set i8:$dst, (GBZ80asrLoop i8:$src, i8:$cnt))]
>;

def IselAsr16 : ShiftPseudo<
  (outs Pairs:$dst),
   (ins Pairs:$src, GPR8:$cnt),
   "# Asr16 PSEUDO",
   [(set i16:$dst, (GBZ80asrLoop i16:$src, i8:$cnt))]
>;

///===============================
/// Post-ISel 16-bit shift pseudos
///===============================

let Constraints = "$dst = $src" in {
  // Given a pair reg of AB:

  //  SLA B
  //  RL A
  def SHL16 : Pseudo<(outs Pairs:$dst), (ins Pairs:$src),
    "SHL16\t$dst",
    []>;

  //  SRL A
  //  RR B
  def LSR16 : Pseudo<(outs Pairs:$dst), (ins Pairs:$src),
    "LSR16\t$dst",
    []>;

  // ??
  def ROL16 : Pseudo<(outs Pairs:$dst), (ins Pairs:$src),
    "ROL16\t$dst",
    []>;

  // ??
  def ROR16 : Pseudo<(outs Pairs:$dst), (ins Pairs:$src),
    "ROR16\t$dst",
    []>;

  //  SRA A
  //  RL B
  def ASR16 : Pseudo<(outs Pairs:$dst), (ins Pairs:$src),
    "ASR16\t$dst",
    []>;

}

///===============================
/// Bit operations
///===============================

let Defs = [rF] in
def BIT_r : I16<(outs), (ins GPR8:$src, uimm3:$imm),
  "BIT\t$imm, $src",
  []>;

let Constraints = "$dst = $src" in {
  def SET_r : I16<(outs GPR8:$dst), (ins GPR8:$src, uimm8p2:$imm),
    "SET\t$imm, $dst",
    []>;

  def RES_r : I16<(outs GPR8:$dst), (ins GPR8:$src, uimm8p2:$imm),
    "RES\t$imm, $dst",
    []>;
}

let mayLoad = 1, mayStore = 1 in {
  def SET_HL : I16<(outs), (ins imm8:$imm, HLPairs:$ptr),
    "SET\t$imm, [HL]",
    []>;

  def RES_HL : I16<(outs), (ins imm8:$imm, HLPairs:$ptr),
    "RES\t$imm, [HL]",
    []>;
}

///===============================
/// Extension
///===============================

let Defs = [rF] in {
  def SEXT : ExtensionPseudo<(outs Pairs:$dst), (ins GPR8:$src),
    "SEXT\t$dst, $src",
    [(set i16:$dst, (sext i8:$src))]>;

  def ZEXT : ExtensionPseudo<(outs Pairs:$dst), (ins GPR8:$src),
    "ZEXT\t$dst, $src",
    [(set i16:$dst, (zext i8:$src))]>;
}

def : Pat<(i16 (anyext i8:$src)),
          (INSERT_SUBREG (i16 (IMPLICIT_DEF)), i8:$src, sub_lo)>;

def : Pat<(i8 (trunc i16:$src)),
          (toLo $src)>;

def : Pat<(sext_inreg i16:$src, i8),
          (SEXT (i8 (toLo $src)))>;

def : Pat<(i8 (trunc (GBZ80lsr (GBZ80lsr (GBZ80lsr (GBZ80lsr (GBZ80lsr (GBZ80lsr (GBZ80lsr (GBZ80lsr i16:$src)))))))))),
  (toHi $src)>;

def : Pat<(i8 (trunc (GBZ80asr (GBZ80asr (GBZ80asr (GBZ80asr (GBZ80asr (GBZ80asr (GBZ80asr (GBZ80asr i16:$src)))))))))),
  (toHi $src)>;

///===============================
/// Jumps
///===============================

let isBranch = 1, isTerminator = 1 in {
  let isBarrier = 1 in {
    def JP_nn : I24<(outs), (ins brtarget16:$targ),
      "JP\t$targ",
      []>;

    def JR_e : I16<(outs), (ins relbrtarget8:$targ),
      "JR\t$targ",
      [(br bb:$targ)]>;

    // indirectbr? Should this have a real input instead of an impdef?
    // nah
    let isIndirectBranch = 1 in
    def JP_HL : I8<(outs), (ins HLPairs:$ptr),
      "JP\tHL",
      []>;
  }

  // TODO: print routines for the codes? how do we print?
  let Uses = [rF] in
  def JP_cc_nn : I24<(outs), (ins brtarget16:$targ, condcode:$cc),
    "JP\t$cc, $targ",
    []>;
  
  let Uses = [rF] in
  def JR_cc_e : I16<(outs), (ins relbrtarget8:$targ, condcode:$cc),
    "JR\t$cc, $targ",
    [(GBZ80brcond bb:$targ, condcode:$cc)]>;

  // Pseudo compare+jump for i16. Expand after isel.
  def BR16 : Pseudo<(outs), (ins brtarget16:$targ, condcode:$cc, Pairs:$lhs, Pairs:$rhs),
    "BR16\t$targ, $lhs, $rhs",
    [(GBZ80br16 bb:$targ, condcode:$cc, Pairs:$lhs, Pairs:$rhs)]>;
}

///===============================
/// Select pseudos
///===============================

let Defs = [rF] in {
  // Select an i8 from an i8 comparison.
  def Select8_8 : Pseudo<
    (outs GPR8:$dst),
    (ins GPR8:$src, GPR8:$src2, i8imm:$cc),
    "# Select8_8 PSEUDO",
    [(set i8:$dst, (GBZ80selectcc i8:$src, i8:$src2, imm:$cc))]
  >;
  // Select an i16 from an i8 comparison.
  def Select16_8 : Pseudo<
    (outs Pairs:$dst),
    (ins Pairs:$src, Pairs:$src2, i8imm:$cc),
    "# Select16_8 PSEUDO",
    [(set i16:$dst, (GBZ80selectcc i16:$src, i16:$src2, imm:$cc))]
  >;
}

let Defs = [rF, rA] in {
  // Select an i16 from an i16 comparison.
  def Select16_16 : Pseudo<
    (outs Pairs:$dst),
    (ins Pairs:$src, Pairs:$src2, i8imm:$cc, Pairs:$lhs, Pairs:$rhs),
    "# Select16_16 PSEUDO",
    [(set i16:$dst, (GBZ80selectbr i16:$src, i16:$src2, timm:$cc, i16:$lhs, i16:$rhs))]
  >;
  // Select an i8 from an i16 comparison.
  def Select8_16 : Pseudo<
    (outs GPR8:$dst),
    (ins GPR8:$src, GPR8:$src2, i8imm:$cc, Pairs:$lhs, Pairs:$rhs),
    "# Select8_16 PSEUDO",
    [(set i8:$dst, (GBZ80selectbr i8:$src, i8:$src2, timm:$cc, i16:$lhs, i16:$rhs))]
  >;
}

///===============================
/// Calls
///===============================

let isCall = 1 in {
  let Uses = [SP, IME], Defs = [IME, rA, rF, rAF, rB, rC, rBC] in
  def CALL_nn : I24<(outs), (ins calltarget:$targ),
    "CALL\t$targ",
    [(GBZ80call imm:$targ)]>;
  
  let Uses = [SP, IME, rF], Defs = [IME, rA, rF, rAF, rH, rL, rHL] in
  def CALL_cc_nn : I24<(outs), (ins calltarget:$targ, condcode:$cc),
    "CALL\t$cc, $targ",
    []>;

  let Uses = [SP, IME], Defs = [IME, rA, rF, rAF, rH, rL, rHL] in 
  def RST_t : I8<(outs), (ins imm8:$t),
    "RST\t$t",
    []>;
}

///===============================
/// Returns
///===============================

let isReturn = 1, isTerminator = 1 in {
  let isBarrier = 1 in {
    def RET : I8<(outs), (ins),
      "RET",
      [(GBZ80retflag)]>;
    let Uses = [IME], Defs = [IME] in
    def RETI : I8<(outs), (ins),
      "RETI",
      []>;
  }
  let Uses = [rF] in
  def RET_cc : I8<(outs), (ins condcode:$cc),
    "RET\t$cc",
    []>;
}

///===============================
/// Other instructions
///===============================

let Defs = [rF], Uses = [rF], Constraints = "$dst = $src" in
def DAA : I8<(outs AReg:$dst), (ins AReg:$src),
  "DAA",
  []>;

let Defs = [rF], Constraints = "$dst = $src" in
def CPL : I8<(outs AReg:$dst), (ins AReg:$src),
  "CPL",
  [(set i8:$dst, (not i8:$src))]>;

// It does nothing
def NOP : I8<(outs), (ins),
  "NOP",
  []>;

let Defs = [rF], Uses = [rF] in
def CCF : I8<(outs), (ins),
  "CCF",
  []>;

let Defs = [rF] in
def SCF : I8<(outs), (ins),
  "SCF",
  []>;

let Defs = [IME], Uses = [IME], hasSideEffects = 1 in {
  def DI : I8<(outs), (ins),
    "DI",
    []>;

  def EI : I8<(outs), (ins),
    "EI",
    []>;
}

let hasSideEffects = 1 in {
  def HALT : I8<(outs), (ins),
    "HALT",
    []>;

  def STOP : I8<(outs), (ins),
    "STOP",
    []>;
}

///===============================
/// Patterns
///===============================


// Load of symbol address.
def : Pat<(i16 (GBZ80Wrapper tglobaladdr:$dst)),
          (LD_dd_nn tglobaladdr:$dst)>;

// Call of global.
def : Pat<(GBZ80call (i16 tglobaladdr:$dst)),
          (CALL_nn tglobaladdr:$dst)>;
def : Pat<(GBZ80call (i16 texternalsym:$dst)),
          (CALL_nn texternalsym:$dst)>;

// Store to global.
def : Pat<(store i8:$src, (GBZ80Wrapper tglobaladdr:$addr)),
          (LD_nn_A $src, $addr)>;
def : Pat<(store i8:$src, (GBZ80Wrapper texternalsym:$addr)),
          (LD_nn_A $src, $addr)>;

// Load from global.
def : Pat<(i8 (load (GBZ80Wrapper tglobaladdr:$addr))),
          (LD_A_nn $addr)>;
def : Pat<(i8 (load (GBZ80Wrapper texternalsym:$addr))),
          (LD_A_nn $addr)>;

// Inc/dec of i8 and i16.
def : Pat<(add GPR8:$src, 1),  (INC_r $src)>;
def : Pat<(add GPR8:$src, -1), (DEC_r $src)>;
def : Pat<(sub GPR8:$src, 1),  (DEC_r $src)>;

def : Pat<(add Pairs:$src, 1),  (INC_ss $src)>;
def : Pat<(add Pairs:$src, -1), (DEC_ss $src)>;
def : Pat<(sub Pairs:$src, 1),  (DEC_ss $src)>;

// Simplification for negation.
def : Pat<(ineg GPR8:$src),
  (INC_r (XOR_n $src, 0xFF))>;
def : Pat<(ineg Pairs:$src),
  (INC_ss (XOR16i $src, 0xFFFF))>;

// Patterns for RES and SET on i8.
let AddedComplexity = 10 in {
// Match an OR with a power of 2. XFORM the immediate to a log2.
def : Pat<(or GPR8:$src, uimm8p2:$imm),
  (SET_r $src, (uimm8p2_XFORM imm:$imm))>;
// Match an AND with an inverse of a power of two. XFORM the immediate.
def : Pat<(and GPR8:$src, uimm8p2n:$imm),
  (RES_r $src, (uimm8p2n_XFORM imm:$imm))>;
}

let AddedComplexity = 10 in {
def : Pat<(or Pairs:$src, uimm16hip2:$imm),
  (buildPair (SET_r (toHi $src), (uimm16hip2_XFORM imm:$imm)),
             (toLo $src))>;
def : Pat<(or Pairs:$src, uimm16lop2:$imm),
  (buildPair (toHi $src),
             (SET_r (toLo $src), (uimm16lop2_XFORM imm:$imm)))>;
}

/*
def : Pat<(add i16:$src, (AVRWrapper tglobaladdr:$src2)),
          (ADD16r i16:$src, tglobaladdr:$src2)>;
def : Pat<(i8 (load (AVRWrapper tglobaladdr:$dst))),
          (LD_ tglobaladdr:$dst)>;
def : Pat<(i16 (load (AVRWrapper tglobaladdr:$dst))),
          (LDSWRdK tglobaladdr:$dst)>;
def : Pat<(store i8:$src, (i16 (AVRWrapper tglobaladdr:$dst))),
          (STSKRr tglobaladdr:$dst, i8:$src)>;
def : Pat<(store i16:$src, (i16 (AVRWrapper tglobaladdr:$dst))),
          (STSWKRr tglobaladdr:$dst, i16:$src)>;
*/

// Add patterns for LDH_C_A and LDH_A_C.
//===-- GBZ80InstrInfo.td - GBZ80 Instruction Formats ----------*- tablegen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// GBZ80 Instruction Format Definitions.
//
//===----------------------------------------------------------------------===//

// A generic GBZ80 instruction.
class GBZ80Inst<dag outs, dag ins, string asmstr, list<dag> pattern> : Instruction
{
  let Namespace = "GB";

  dag OutOperandList = outs;
  dag InOperandList = ins;
  let AsmString = asmstr;
  let Pattern = pattern;
}

class I8<dag outs, dag ins, string asmstr, list<dag> pattern>
  : GBZ80Inst<outs, ins, asmstr, pattern>
{
  field bits<8> Inst;

  let Size = 1;
}

class I16<dag outs, dag ins, string asmstr, list<dag> pattern>
  : GBZ80Inst<outs, ins, asmstr, pattern>
{
  field bits<16> Inst;

  let Size = 2;
}

class I24<dag outs, dag ins, string asmstr, list<dag> pattern>
  : GBZ80Inst<outs, ins, asmstr, pattern>
{
  field bits<24> Inst;

  let Size = 3;
}

// A class for pseudo instructions.
// Psuedo instructions are not real GBZ80 instructions. The DAG stores
// psuedo instructions which are replaced by real GBZ80 instructions by
// GBZ80ExpandPseudoInsts.cpp.
//
// For example, the ADDW (add wide, as in add 16 bit values) instruction
// is defined as a pseudo instruction. In GBZ80ExpandPseudoInsts.cpp,
// the instruction is then replaced by two add instructions - one for each byte.
class Pseudo<dag outs, dag ins, string asmstr, list<dag> pattern>
  : GBZ80Inst<outs, ins, asmstr, pattern>
{
  let Pattern = pattern;

  let isPseudo = 1;
  let isCodeGenOnly = 1;
}

class ExtensionPseudo<dag outs, dag ins, string asmstr, list<dag> pattern>
  : Pseudo<outs, ins, asmstr, pattern>
{
  let Defs = [rF];
}

class StorePseudo<dag outs, dag ins, string asmstr, list<dag> pattern>
  : Pseudo<outs, ins, asmstr, pattern>
{
  let Defs = [SP];
}

class SelectPseudo<dag outs, dag ins, string asmstr, list<dag> pattern>
  : Pseudo<outs, ins, asmstr, pattern>
{
  let usesCustomInserter = 1;

  let Uses = [rF];
  let Defs = [rF]; 
}

class ShiftPseudo<dag outs, dag ins, string asmstr, list<dag> pattern>
  : Pseudo<outs, ins, asmstr, pattern>
{
  let usesCustomInserter = 1;

  let Defs = [rF];
}


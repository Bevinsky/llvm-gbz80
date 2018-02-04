//===-- GBZ80CallingConv.td - Calling Conventions for GBZ80 ----*- tablegen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
// This describes the calling conventions for GBZ80 architecture.
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// GBZ80 Return Value Calling Convention
//===----------------------------------------------------------------------===//

def RetCC_GBZ80 : CallingConv
<[
  // i8 is returned in A.
  CCIfType<[i8], CCAssignToReg<[rA]>>,

  // i16 are returned in HL.
  CCIfType<[i16], CCAssignToReg<[rHL]>>
]>;

//===----------------------------------------------------------------------===//
// GBZ80 Argument Calling Conventions
//===----------------------------------------------------------------------===//

def ArgCC_GBZ80 : CallingConv<[
  CCIfType<[i8], CCAssignToReg<[rA, rB, rC, rD, rE, rH, rL]>>,
  CCIfType<[i16], CCAssignToReg<[rBC, rDE, rHL]>>,
  CCAssignToStack<2, 1>
]>;

// Calling convention for variadic functions.
def ArgCC_GBZ80_Vararg : CallingConv
<[
  // i16 are always passed through the stack with an alignment of 1.
  CCAssignToStack<2, 1>
]>;

//===----------------------------------------------------------------------===//
// Callee-saved register lists.
//===----------------------------------------------------------------------===//

def CSR_Normal : CalleeSavedRegs<(add rBC, rDE)>;
def CSR_Interrupts : CalleeSavedRegs<(add rAF, rBC, rDE, rHL)>;

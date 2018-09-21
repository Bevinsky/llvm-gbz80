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
  // i8 is returned in B.
  CCIfType<[i8], CCAssignToReg<[RB]>>,

  // i16 are returned in BC.
  CCIfType<[i16], CCAssignToReg<[RBC]>>
]>;

//===----------------------------------------------------------------------===//
// GBZ80 Argument Calling Conventions
//===----------------------------------------------------------------------===//

// Calling convention for variadic functions.
def ArgCC_GBZ80_Vararg : CallingConv
<[
  // i16 are always passed through the stack with an alignment of 1.
  CCAssignToStack<2, 1>
]>;

//===----------------------------------------------------------------------===//
// Callee-saved register lists.
//===----------------------------------------------------------------------===//

def CSR_Normal : CalleeSavedRegs<(add RDE, RHL)>;
def CSR_Interrupts : CalleeSavedRegs<(add RAF, RBC, RDE, RHL)>;

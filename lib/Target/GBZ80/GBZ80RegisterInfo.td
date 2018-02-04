//===-- GBZ80RegisterInfo.td - GBZ80 Register defs -------------*- tablegen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
//  Declarations that describe the GBZ80 register file
//===----------------------------------------------------------------------===//

// General purpose register definition.
class GBZ80Reg<bits<16> num,
             string name,
             list<Register> subregs = [],
             list<string> altNames = []>
  : RegisterWithSubRegs<name, subregs>
{
  field bits<16> Num = num;

  let HWEncoding = num;
  let Namespace = "GB";
  let SubRegs = subregs;
  let AltNames = altNames;
}

// Subregister indices.
let Namespace = "GB" in
{
  def sub_lo : SubRegIndex<8>;
  def sub_hi : SubRegIndex<8, 8>;

  def sub_z  : SubRegIndex<1, 7>;
  def sub_n  : SubRegIndex<1, 6>;
  def sub_h  : SubRegIndex<1, 5>;
  def sub_c  : SubRegIndex<1, 4>;
}

// 8-bit registers.
let CostPerUse = 0 in
def rA : GBZ80Reg<7,  "A">;
let CostPerUse = 1 in {
  def rB : GBZ80Reg<0,  "B">;
  def rC : GBZ80Reg<1,  "C">;
  def rD : GBZ80Reg<2,  "D">;
  def rE : GBZ80Reg<3,  "E">;
}
let CostPerUse = 2 in {
  def rH : GBZ80Reg<4,  "H">;
  def rL : GBZ80Reg<5,  "L">;
}

// Flags.
def rF : GBZ80Reg<6,  "F">;

// 16-bit registers.
let SubRegIndices = [sub_hi, sub_lo],
    CoveredBySubRegs = 1 in {
  def rHL : GBZ80Reg<2, "HL", [rH, rL]>;
  let CostPerUse = 1 in {
    def rBC : GBZ80Reg<0, "BC", [rB, rC]>;
    def rDE : GBZ80Reg<1, "DE", [rD, rE]>;
  }
  def rAF : GBZ80Reg<3, "AF", [rA, rF]>;
}

// Stack pointer
def SP : GBZ80Reg<3, "SP">;

// Interrupt master enable.
def IME : GBZ80Reg<0, "IME">;

//===----------------------------------------------------------------------===//
// Register Classes
//===----------------------------------------------------------------------===//

def AReg : RegisterClass<"GB", [i8], 8,
  (add rA)>;
def CReg : RegisterClass<"GB", [i8], 8,
  (add rC)>;
def GPR8 : RegisterClass<"GB", [i8], 8,
  (add rA, rB, rC, rD, rE, rH, rL)>;

def BCDEPairs : RegisterClass<"GB", [i16], 8,
  (add rBC, rDE)>;
def HLPairs : RegisterClass<"GB", [i16], 8,
  (add rHL)>;
def Pairs : RegisterClass<"GB", [i16], 8,
  (add rHL, rBC, rDE)>;

let isAllocatable = 0 in {
  def AllPairs : RegisterClass<"GB", [i16], 8,
    (add rHL, rBC, rDE, rAF)>;
  def DDPairs : RegisterClass<"GB", [i16], 8,
    (add rHL, rBC, rDE, SP)>;
}

let CopyCost = -1 in
def Flag : RegisterClass<"GB", [i8], 8,
  (add rF)>;


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
}

// 8-bit registers.
let CostPerUse = 0 in
def RA : GBZ80Reg<7,  "A">;
let CostPerUse = 1 in {
  def RB : GBZ80Reg<0,  "B">;
  def RC : GBZ80Reg<1,  "C">;
  def RD : GBZ80Reg<2,  "D">;
  def RE : GBZ80Reg<3,  "E">;
}
let CostPerUse = 2 in {
  def RH : GBZ80Reg<4,  "H">;
  def RL : GBZ80Reg<5,  "L">;
}

// Flags.
def RF : GBZ80Reg<6,  "F">;

// 16-bit registers.
let SubRegIndices = [sub_hi, sub_lo],
    CoveredBySubRegs = 1 in {
  def RHL : GBZ80Reg<2, "HL", [RH, RL]>;
  let CostPerUse = 1 in {
    def RBC : GBZ80Reg<0, "BC", [RB, RC]>;
    def RDE : GBZ80Reg<1, "DE", [RD, RE]>;
  }
  def RAF : GBZ80Reg<3, "AF", [RA, RF]>;
}

// Stack pointer
def SP : GBZ80Reg<3, "SP">;

// Interrupt master enable.
def IME : GBZ80Reg<0, "IME">;

//===----------------------------------------------------------------------===//
// Register Classes
//===----------------------------------------------------------------------===//

def R8 : RegisterClass<"GB", [i8], 8,
  (add RA, RB, RC, RD, RE, RH, RL)>;
def R8_GPR : RegisterClass<"GB", [i8], 8,
  (add RB, RC, RD, RE, RH, RL)>;

def R16_BCDE : RegisterClass<"GB", [i16], 8,
  (add RBC, RDE)>;
def R16_HL : RegisterClass<"GB", [i16], 8,
  (add RHL)>;
def R16 : RegisterClass<"GB", [i16], 8,
  (add RHL, RBC, RDE)>;

let isAllocatable = 0 in {
  def R16_All : RegisterClass<"GB", [i16], 8,
    (add RHL, RBC, RDE, RAF)>;
  def R16_DD : RegisterClass<"GB", [i16], 8,
    (add RHL, RBC, RDE, SP)>;
}

let CopyCost = -1 in
def Flag : RegisterClass<"GB", [i8], 8,
  (add RF)>;


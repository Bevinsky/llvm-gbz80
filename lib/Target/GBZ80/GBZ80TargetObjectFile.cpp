//===-- GBZ80TargetObjectFile.cpp - GBZ80 Object Files ------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "GBZ80TargetObjectFile.h"

#include "llvm/BinaryFormat/ELF.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/Mangler.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCSectionELF.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/MathExtras.h"

#include "GBZ80.h"

namespace llvm {

void GBZ80SectionData::getAsString(SmallVectorImpl<char> &Str) const {
  raw_svector_ostream O(Str);
  // XXX: this will break if sectionname has quotes in it.
  if (SectionName.empty())
    O << "SECTION @,";
  else
    O << "SECTION \"" << SectionName << "\",";
  switch (Type) {
  case ST_ROM0:  O << "ROM0"; break;
  case ST_ROMX:  O << "ROMX"; break;
  case ST_VRAM:  O << "VRAM"; break;
  case ST_SRAM:  O << "SRAM"; break;
  case ST_WRAM0: O << "WRAM0"; break;
  case ST_WRAMX: O << "WRAMX"; break;
  case ST_OAM:   O << "OAM"; break;
  case ST_HRAM:  O << "HRAM"; break;
  default: llvm_unreachable("Tried to print an invalid section!");
  }
  if (hasAddress())
    O << "[$" << format("%04x", Address) << "]";
  if (hasBank())
    O << ",BANK[" << Bank << "]";
  // Round alignment up to the nearest log2. This will guarantee at least that
  // alignment. I don't think non-power-of-two alignments are legal anyway.
  if (hasAlignment())
    O << ",ALIGN[" << Log2_32_Ceil(Alignment) << "]";
}

bool GBZ80SectionData::get(StringRef Specifier, GBZ80SectionData &Data) {
  // Set defaults.
  Data.Type = ST_NONE;
  Data.Address = 0;
  Data.Bank = ~0U;
  Data.Alignment = 1;

  // TODO: I guess this isn't needed just yet.
  return false;
}

void GBZ80TargetObjectFile::Initialize(MCContext &Ctx, const TargetMachine &TM) {
  Base::Initialize(Ctx, TM);
  // Make some fake generic sections.
  TextSection = getSection(SectionKind::getText(), StringRef(".text"),
      GBSectionType::ST_ROM0, 0, ~0U, 1, nullptr);
  DataSection = getSection(SectionKind::getText(), StringRef(".data"),
    GBSectionType::ST_WRAM0, 0, ~0U, 1, nullptr);
  BSSSection = getSection(SectionKind::getText(), StringRef(".bss"),
    GBSectionType::ST_WRAM0, 0, ~0U, 1, nullptr);
  ReadOnlySection = getSection(SectionKind::getText(), StringRef(".readonly"),
    GBSectionType::ST_ROM0, 0, ~0U, 1, nullptr);
}

MCSectionGBZ80 *GBZ80TargetObjectFile::getSection(SectionKind Kind,
    StringRef N, GBSectionType T, uint16_t A, unsigned B, unsigned Align,
    const GlobalObject *GO) const {
  GBZ80SectionData Data(N, T, A, B, Align, GO);
  if (Sections.count(Data))
    return Sections[Data];
  // Not allocated with an allocator...
  MCSectionGBZ80 *Section = new MCSectionGBZ80(Kind, Data, nullptr);
  Sections[Data] = Section;
  return Section;
}

MCSection *
GBZ80TargetObjectFile::SelectSectionForGlobal(const GlobalObject *GO,
                                              SectionKind Kind,
                                              const TargetMachine &TM) const {
  // This only gets called for globals that don't have a 'section' attribute.
  // XXX: Could they have custom attributes though? Perhaps only makes sense
  // for constants.
  GBSectionType Type;
  // We need to enforce sections here.
  // Should this verify the section on the actual object?
  if (Kind.isText() || Kind.isReadOnly()) {
    // Code and constants. This goes in ROM0 or ROMX, always. It is an error to
    // put it in any other section. If no section is given, it goes in ROM0.
    // XXX: Make it ROM0 for now.
    Type = ST_ROM0;
  } else if (Kind.isBSS() || Kind.isData()) {
    // This is writeable data.
    // Unsure how to handle this, since it needs to be inited.
    // Special section perhaps? 
    // This can go in any section except for ROM0 and ROMX. Default is WRAM0.
    // XXX: WRAM0 for now.
    Type = ST_WRAM0;
  }

  return getSection(Kind, (Twine(GO->getParent()->getName()) + "_" + GO->getName()).str(),
                    Type, 0, ~0U, GO->getAlignment(), GO);
}
MCSection* 
GBZ80TargetObjectFile::getSectionForConstant(const DataLayout &DL,
    SectionKind Kind, const Constant *C, unsigned &Align) const {
  // This might actually be enough. Any explicit section will end up in
  // getExplicitSectionGlobal.
  // XXX: unless we want to specify deduced constant sections with attrs?
  // XXX: We can't specify a symbol for this unique section. is this a problem?
  assert(Kind.isReadOnly() && "Writeable constant?");
  return getSection(Kind, StringRef(), ST_ROM0, 0, ~0U, Align, nullptr);
}

MCSection*
GBZ80TargetObjectFile::getExplicitSectionGlobal(const GlobalObject * GO,
    SectionKind Kind, const TargetMachine & TM) const {
  // This is supposed to take the explicit section string on the GO and return
  // an MCSection for it. Decide on a format for this.
  // XXX: Is this aware of the subtarget? Could this verify the section?
  llvm_unreachable("getExplicitSectionGlobal not yet implemented!");
}
void MCSectionGBZ80::PrintSwitchToSection(const MCAsmInfo & MAI,
  const Triple & T, raw_ostream & OS, const MCExpr * Subsection) const {
  // This is just as simple as asking the section data for its string and
  // printing it.
  SmallString<40> SecStr;
  Data.getAsString(SecStr);
  OS << SecStr << '\n';
}
bool MCSectionGBZ80::UseCodeAlign() const {
  // RGBDS has support for alignment.
  return false;
}
bool MCSectionGBZ80::isVirtualSection() const {
  // I don't believe RGBDS has any of these.
  // XXX: We might want a fake section I guess? ST_NONE?
  return false;
}
} // end of namespace llvm


//===-- GBZ80MCExpr.cpp - GBZ80 specific MC expression classes ----------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "GBZ80MCExpr.h"

#include "llvm/MC/MCAsmLayout.h"
#include "llvm/MC/MCAssembler.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCValue.h"

namespace llvm {

namespace {

const struct ModifierEntry {
  const char * const Spelling;
  GBZ80MCExpr::VariantKind VariantKind;
} ModifierNames[] = {
    {"lo8", GBZ80MCExpr::VK_GBZ80_LO8},       {"hi8", GBZ80MCExpr::VK_GBZ80_HI8},
    {"hh8", GBZ80MCExpr::VK_GBZ80_HH8}, // synonym with hlo8
    {"hlo8", GBZ80MCExpr::VK_GBZ80_HH8},      {"hhi8", GBZ80MCExpr::VK_GBZ80_HHI8},

    {"pm_lo8", GBZ80MCExpr::VK_GBZ80_PM_LO8}, {"pm_hi8", GBZ80MCExpr::VK_GBZ80_PM_HI8},
    {"pm_hh8", GBZ80MCExpr::VK_GBZ80_PM_HH8},
};

} // end of anonymous namespace

const GBZ80MCExpr *GBZ80MCExpr::create(VariantKind Kind, const MCExpr *Expr,
                                   bool Negated, MCContext &Ctx) {
  return new (Ctx) GBZ80MCExpr(Kind, Expr, Negated);
}

void GBZ80MCExpr::printImpl(raw_ostream &OS, const MCAsmInfo *MAI) const {
  assert(Kind != VK_GBZ80_None);

  if (isNegated())
    OS << '-';

  OS << getName() << '(';
  getSubExpr()->print(OS, MAI);
  OS << ')';
}

bool GBZ80MCExpr::evaluateAsConstant(int64_t &Result) const {
  MCValue Value;

  bool isRelocatable =
      getSubExpr()->evaluateAsRelocatable(Value, nullptr, nullptr);

  if (!isRelocatable)
    return false;

  if (Value.isAbsolute()) {
    Result = evaluateAsInt64(Value.getConstant());
    return true;
  }

  return false;
}

bool GBZ80MCExpr::evaluateAsRelocatableImpl(MCValue &Result,
                                          const MCAsmLayout *Layout,
                                          const MCFixup *Fixup) const {
  MCValue Value;
  bool isRelocatable = SubExpr->evaluateAsRelocatable(Value, Layout, Fixup);

  if (!isRelocatable)
    return false;

  if (Value.isAbsolute()) {
    Result = MCValue::get(evaluateAsInt64(Value.getConstant()));
  } else {
    if (!Layout) return false;

    MCContext &Context = Layout->getAssembler().getContext();
    const MCSymbolRefExpr *Sym = Value.getSymA();
    MCSymbolRefExpr::VariantKind Modifier = Sym->getKind();
    if (Modifier != MCSymbolRefExpr::VK_None)
      return false;

    Sym = MCSymbolRefExpr::create(&Sym->getSymbol(), Modifier, Context);
    Result = MCValue::get(Sym, Value.getSymB(), Value.getConstant());
  }

  return true;
}

int64_t GBZ80MCExpr::evaluateAsInt64(int64_t Value) const {
  if (Negated)
    Value *= -1;

  switch (Kind) {
  case GBZ80MCExpr::VK_GBZ80_LO8:
    break;
  case GBZ80MCExpr::VK_GBZ80_HI8:
    Value >>= 8;
    break;
  case GBZ80MCExpr::VK_GBZ80_HH8:
    Value >>= 16;
    break;
  case GBZ80MCExpr::VK_GBZ80_HHI8:
    Value >>= 24;
    break;
  case GBZ80MCExpr::VK_GBZ80_PM_LO8:
    Value >>= 1;
    break;
  case GBZ80MCExpr::VK_GBZ80_PM_HI8:
    Value >>= 9;
    break;
  case GBZ80MCExpr::VK_GBZ80_PM_HH8:
    Value >>= 17;
    break;

  case GBZ80MCExpr::VK_GBZ80_None:
    llvm_unreachable("Uninitialized expression.");
  }
  return static_cast<uint64_t>(Value) & 0xff;
}

GB::Fixups GBZ80MCExpr::getFixupKind() const {
  GB::Fixups Kind = GB::Fixups::LastTargetFixupKind;

  switch (getKind()) {
  case VK_GBZ80_LO8:
    Kind = isNegated() ? GB::fixup_lo8_ldi_neg : GB::fixup_lo8_ldi;
    break;
  case VK_GBZ80_HI8:
    Kind = isNegated() ? GB::fixup_hi8_ldi_neg : GB::fixup_hi8_ldi;
    break;
  case VK_GBZ80_HH8:
    Kind = isNegated() ? GB::fixup_hh8_ldi_neg : GB::fixup_hh8_ldi;
    break;
  case VK_GBZ80_HHI8:
    Kind = isNegated() ? GB::fixup_ms8_ldi_neg : GB::fixup_ms8_ldi;
    break;

  case VK_GBZ80_PM_LO8:
    Kind = isNegated() ? GB::fixup_lo8_ldi_pm_neg : GB::fixup_lo8_ldi_pm;
    break;
  case VK_GBZ80_PM_HI8:
    Kind = isNegated() ? GB::fixup_hi8_ldi_pm_neg : GB::fixup_hi8_ldi_pm;
    break;
  case VK_GBZ80_PM_HH8:
    Kind = isNegated() ? GB::fixup_hh8_ldi_pm_neg : GB::fixup_hh8_ldi_pm;
    break;

  case VK_GBZ80_None:
    llvm_unreachable("Uninitialized expression");
  }

  return Kind;
}

void GBZ80MCExpr::visitUsedExpr(MCStreamer &Streamer) const {
  Streamer.visitUsedExpr(*getSubExpr());
}

const char *GBZ80MCExpr::getName() const {
  const auto &Modifier = std::find_if(
      std::begin(ModifierNames), std::end(ModifierNames),
      [this](ModifierEntry const &Mod) { return Mod.VariantKind == Kind; });

  if (Modifier != std::end(ModifierNames)) {
    return Modifier->Spelling;
  }
  return nullptr;
}

GBZ80MCExpr::VariantKind GBZ80MCExpr::getKindByName(StringRef Name) {
  const auto &Modifier = std::find_if(
      std::begin(ModifierNames), std::end(ModifierNames),
      [&Name](ModifierEntry const &Mod) { return Mod.Spelling == Name; });

  if (Modifier != std::end(ModifierNames)) {
    return Modifier->VariantKind;
  }
  return VK_GBZ80_None;
}

} // end of namespace llvm


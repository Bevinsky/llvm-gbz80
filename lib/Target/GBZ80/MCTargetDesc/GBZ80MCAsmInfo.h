//===-- GBZ80MCAsmInfo.h - GBZ80 asm properties ---------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the declaration of the GBZ80MCAsmInfo class.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_GBZ80_ASM_INFO_H
#define LLVM_GBZ80_ASM_INFO_H

#include "llvm/MC/MCAsmInfo.h"

namespace llvm {

class Triple;

/// Specifies the format of GBZ80 assembly files.
class GBZ80MCAsmInfo : public MCAsmInfo {
public:
  explicit GBZ80MCAsmInfo(const Triple &TT);
};

} // end namespace llvm

#endif // LLVM_GBZ80_ASM_INFO_H

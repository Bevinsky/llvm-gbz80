//===-- GBZ80MCAsmInfo.cpp - GBZ80 asm properties -----------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the declarations of the GBZ80MCAsmInfo properties.
//
//===----------------------------------------------------------------------===//

#include "GBZ80MCAsmInfo.h"

#include "llvm/ADT/Triple.h"

namespace llvm {

GBZ80MCAsmInfo::GBZ80MCAsmInfo(const Triple &TT) {
  CodePointerSize = 2;
  CalleeSaveStackSlotSize = 2;
  CommentString = ";";
  PrivateGlobalPrefix = ".L";
  UseIntegratedAssembler = false;
  AsciiDirective = "DB ";
  AscizDirective = nullptr; // DB does not zero-terminate.
  MaxInstLength = 3;
  ZeroDirective = nullptr;
  Data8bitsDirective = "DB ";
  Data16bitsDirective = "DW ";
  Data32bitsDirective = "DL ";
  GlobalDirective = "GLOBAL ";
  HasDotTypeDotSizeDirective = false;
  HasSingleParameterDotFile = false;
  WeakDirective = nullptr;
}

} // end of namespace llvm

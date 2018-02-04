//===-- GBZ80TargetStreamer.cpp - GBZ80 Target Streamer Methods ---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file provides GBZ80 specific target streamer methods.
//
//===----------------------------------------------------------------------===//

#include "GBZ80TargetStreamer.h"

#include "llvm/MC/MCContext.h"

namespace llvm {

GBZ80TargetStreamer::GBZ80TargetStreamer(MCStreamer &S) : MCTargetStreamer(S) {}

GBZ80TargetAsmStreamer::GBZ80TargetAsmStreamer(MCStreamer &S)
    : GBZ80TargetStreamer(S) {}

void GBZ80TargetStreamer::finish() {
  MCStreamer &OS = getStreamer();
  MCContext &Context = OS.getContext();

  MCSymbol *DoCopyData = Context.getOrCreateSymbol("__do_copy_data");
  MCSymbol *DoClearBss = Context.getOrCreateSymbol("__do_clear_bss");

  // FIXME: We can disable __do_copy_data if there are no static RAM variables.

  OS.emitRawComment(" Declaring this symbol tells the CRT that it should");
  OS.emitRawComment("copy all variables from program memory to RAM on startup");
  OS.EmitSymbolAttribute(DoCopyData, MCSA_Global);

  OS.emitRawComment(" Declaring this symbol tells the CRT that it should");
  OS.emitRawComment("clear the zeroed data section on startup");
  OS.EmitSymbolAttribute(DoClearBss, MCSA_Global);
}

} // end namespace llvm


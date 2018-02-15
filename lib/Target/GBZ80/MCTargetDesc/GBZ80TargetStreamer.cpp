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

}

} // end namespace llvm


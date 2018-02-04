//===----- GBZ80ELFStreamer.h - GBZ80 Target Streamer --------------*- C++ -*--===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_GBZ80_ELF_STREAMER_H
#define LLVM_GBZ80_ELF_STREAMER_H

#include "GBZ80TargetStreamer.h"

namespace llvm {

/// A target streamer for an GBZ80 ELF object file.
class GBZ80ELFStreamer : public GBZ80TargetStreamer {
public:
  GBZ80ELFStreamer(MCStreamer &S, const MCSubtargetInfo &STI);

  MCELFStreamer &getStreamer() {
    return static_cast<MCELFStreamer &>(Streamer);
  }
};

} // end namespace llvm

#endif

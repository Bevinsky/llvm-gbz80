//===-- GBZ80TargetStreamer.h - GBZ80 Target Streamer --------------*- C++ -*--===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_GBZ80_TARGET_STREAMER_H
#define LLVM_GBZ80_TARGET_STREAMER_H

#include "llvm/MC/MCELFStreamer.h"

namespace llvm {
class MCStreamer;

/// A generic GBZ80 target output stream.
class GBZ80TargetStreamer : public MCTargetStreamer {
public:
  explicit GBZ80TargetStreamer(MCStreamer &S);

  void finish() override;
};

/// A target streamer for textual GBZ80 assembly code.
class GBZ80TargetAsmStreamer : public GBZ80TargetStreamer {
public:
  explicit GBZ80TargetAsmStreamer(MCStreamer &S);
};

} // end namespace llvm

#endif // LLVM_GBZ80_TARGET_STREAMER_H

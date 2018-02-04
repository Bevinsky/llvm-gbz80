//===---------------------------------------------------------------------===//
// GBZ80 Device Definitions
//===---------------------------------------------------------------------===//

def FeatureMBC1    : SubtargetFeature<"mbc1", "m_MBC1", "false",
                                      "MBC1">;

def FeatureMBC3    : SubtargetFeature<"mbc3", "m_MBC3", "false",
                                      "MBC3">;

// TODO: more stuff?

def FeatureRAM     : SubtargetFeature<"ram", "m_RAM", "false",
                                      "Has RAM">;

let Implies = [FeatureRAM] in
def FeatureMBC2    : SubtargetFeature<"mbc2", "m_MBC2", "false",
                                      "MBC2">;

def FeatureBattery : SubtargetFeature<"battery", "m_Battery", "false",
                                      "Has a battery">;

let Implies = [FeatureMBC3, FeatureBattery] in
def FeatureRTC     : SubtargetFeature<"rtc", "m_RTC", "false",
                                      "Has an RTC">;

//===---------------------------------------------------------------------===//
// GBZ80 microcontrollers supported.
//===---------------------------------------------------------------------===//

class GBProcessor<string Name, int Ident, list<SubtargetFeature> F = []>
  : Processor<Name, NoItineraries, F> {
  // We do nothing with ident because there's nowhere to place it.
}

// Only ROM, no MBC.
def : GBProcessor<"gb",               0x00>;

// ROM and RAM.
def : GBProcessor<"gb-ram",           0x08,
  [FeatureRAM]>;
def : GBProcessor<"gb-bram",          0x09,
  [FeatureRAM, FeatureBattery]>;

// MBC1.
def : GBProcessor<"gb-mbc1",          0x01,
  [FeatureMBC1]>;
def : GBProcessor<"gb-mbc1-ram",      0x02,
  [FeatureMBC1, FeatureRAM]>;
def : GBProcessor<"gb-mbc1-bram",     0x03,
  [FeatureMBC1, FeatureRAM, FeatureBattery]>;

// MBC2.
def : GBProcessor<"gb-mbc2",          0x05,
  [FeatureMBC2, FeatureRAM]>;
def : GBProcessor<"gb-mbc2-bram",     0x06,
  [FeatureMBC2, FeatureRAM, FeatureBattery]>;

// MBC3.
def : GBProcessor<"gb-mbc3",          0x11,
  [FeatureMBC3]>;
def : GBProcessor<"gb-mbc3-ram",      0x12,
  [FeatureMBC3, FeatureRAM]>;
def : GBProcessor<"gb-mbc3-bram",     0x13,
  [FeatureMBC3, FeatureRAM, FeatureBattery]>;
def : GBProcessor<"gb-mbc3-rtc",      0x0F,
  [FeatureMBC3, FeatureBattery, FeatureRTC]>;
def : GBProcessor<"gb-mbc3-rtc-bram", 0x10,
  [FeatureMBC3, FeatureRAM, FeatureBattery, FeatureRTC]>;

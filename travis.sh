#!/bin/bash
mkdir -p build
cd build
cmake -G "Unix Makefiles" -DCMAKE_BUILD_TYPE=Debug -DLLVM_TARGETS_TO_BUILD="GBZ80" ../ && \
make -j2 check-llvm


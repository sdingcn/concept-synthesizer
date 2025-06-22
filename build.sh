#!/bin/bash

set -e

LLVM_URL="https://github.com/llvm/llvm-project/archive/refs/tags/llvmorg-19.1.1.zip"
LLVM_ZIP="llvmorg-19.1.1.zip"
LLVM_DIR="llvm-project-llvmorg-19.1.1"

if test -d "$LLVM_DIR"; then
  echo ">>> using LLVM directory $LLVM_DIR"
else
  echo ">>> downloading LLVM source code"
  wget "$LLVM_URL"
  echo ">>> extracting LLVM source code"
  unzip -q "$LLVM_ZIP"
  echo ">>> adding concept-synthesizer source code to LLVM directory"
  mkdir "$LLVM_DIR/clang-tools-extra/concept-synthesizer/"
  cp src/CMakeLists.txt "$LLVM_DIR/clang-tools-extra/concept-synthesizer/"
  cp src/ConceptSynthesizer.cpp "$LLVM_DIR/clang-tools-extra/concept-synthesizer/"
  echo "\nadd_subdirectory(concept-synthesizer)" >> "$LLVM_DIR/clang-tools-extra/CMakeLists.txt"
  echo ">>> making build directory"
  mkdir "$LLVM_DIR/build/"
fi

echo ">>> building the project"
cd "$LLVM_DIR/build/"
cmake -DLLVM_ENABLE_PROJECTS="clang;clang-tools-extra" \
      -DCMAKE_BUILD_TYPE=Release \
      -G "Unix Makefiles" \
      ../llvm
make --quiet concept-synthesizer -j8

#!/usr/bin/env bash

LLVM_URL="https://github.com/llvm/llvm-project/archive/refs/tags/llvmorg-19.1.1.zip"
LLVM_ZIP="llvmorg-19.1.1.zip"
LLVM_DIR="llvm-project-llvmorg-19.1.1"

wget "$LLVM_URL"
unzip "$LLVM_ZIP"
rm "$LLVM_ZIP"
mkdir "$LLVM_DIR/clang-tools-extra/concept-synthesizer/"
cp src/CMakeLists.txt "$LLVM_DIR/clang-tools-extra/concept-synthesizer/"
cp src/ConceptSynthesizer.cpp "$LLVM_DIR/clang-tools-extra/concept-synthesizer/"
echo "" >> "$LLVM_DIR/clang-tools-extra/CMakeLists.txt"
echo "add_subdirectory(concept-synthesizer)" >> "$LLVM_DIR/clang-tools-extra/CMakeLists.txt"
mkdir "$LLVM_DIR/build/"
cd "$LLVM_DIR/build"
cmake -DLLVM_ENABLE_PROJECTS="clang;clang-tools-extra" -DCMAKE_BUILD_TYPE=Release -G "Unix Makefiles" ../llvm
make -j8

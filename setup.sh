#!/bin/bash

set -e

BOOST_URL="https://archives.boost.io/release/1.84.0/source/boost_1_84_0.zip"
BOOST_ZIP="boost_1_84_0.zip"
BOOST_DIR="boost_1_84_0"
LLVM_URL="https://github.com/llvm/llvm-project/archive/refs/tags/llvmorg-19.1.1.zip"
LLVM_ZIP="llvmorg-19.1.1.zip"
LLVM_DIR="llvm-project-llvmorg-19.1.1"

if test -d "$BOOST_DIR"; then
    echo "$BOOST_DIR already exists; please remove it before doing a fresh setup"
    exit 1
fi

if test -d "$LLVM_DIR"; then
    echo "$LLVM_DIR already exists; please remove it before doing a fresh setup"
    exit 1
fi

if test -e "$BOOST_ZIP"; then
    echo ">>> found existing Boost zip"
else
    echo ">>> downloading Boost source code"
    wget "$BOOST_URL"
fi

echo ">>> extracting Boost source code"
unzip -q "$BOOST_ZIP"

if test -e "$LLVM_ZIP"; then
    echo ">>> found existing LLVM zip"
else
    echo ">>> downloading LLVM source code"
    wget "$LLVM_URL"
fi

echo ">>> extracting LLVM source code"
unzip -q "$LLVM_ZIP"

echo ">>> adding concept-synthesizer source code to LLVM directory"
mkdir "$LLVM_DIR/clang-tools-extra/concept-synthesizer/"
cp src/CMakeLists.txt "$LLVM_DIR/clang-tools-extra/concept-synthesizer/"
cp src/ConceptSynthesizer.cpp "$LLVM_DIR/clang-tools-extra/concept-synthesizer/"
echo "" >> "$LLVM_DIR/clang-tools-extra/CMakeLists.txt"
echo "add_subdirectory(concept-synthesizer)" >> "$LLVM_DIR/clang-tools-extra/CMakeLists.txt"

echo ">>> building frontend"
mkdir "$LLVM_DIR/build_frontend/"
cd "$LLVM_DIR/build_frontend/"
cmake -DLLVM_ENABLE_PROJECTS="clang;clang-tools-extra" \
      -DCMAKE_BUILD_TYPE=Release \
      -G "Unix Makefiles" \
      ../llvm
make clang concept-synthesizer -j8
cd ../../

echo ">>> building headers"
mkdir "$LLVM_DIR/build_headers/"
cd "$LLVM_DIR/build_headers/"
cmake -DLLVM_ENABLE_RUNTIMES="libcxx;libcxxabi;libunwind" \
      -DCMAKE_BUILD_TYPE=Release \
      -G "Unix Makefiles" \
      ../runtimes
make cxx -j8
cd ../../

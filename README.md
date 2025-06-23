# concept-synthesizer

automatically synthesizing C++20 template constraints for function templates

## dependencies

+ Platform: MacOS/Linux.
+ Software: python3, wget, unzip, git, cmake, make, clang++/g++.

## setup

Run `./setup.sh` to download LLVM and Boost (only needed for testing) and build the project.

## run

### run on a single file

```
./llvm-project-llvmorg-19.1.1/build_frontend/bin/clang-19 \
  -nostdinc++ \
  -isystem ./llvm-project-llvmorg-19.1.1/build_headers/include/c++/v1 \
  -std=c++20 \
  -E \
  -P \
  -I<include-path-1> -I<include-path-2> ... \
  -o <file-path>.ii \
  <file-path>.cpp

./llvm-project-llvmorg-19.1.1/build_frontend/bin/concept-synthesizer \
  <file-path>.ii \
  -- \
  -x c++-cpp-output \
  -std=c++20
```

The synthesizer prints 5 sections to the standard output.
+ Individual results: source location, synthesized constraints, etc. for each type template parameter
+ Invalid calls: generated invalid calls to these function templates (needed for testing)
+ Statistics: numbers of function templates, type template parameters, etc.
+ Constrained code: the C++ code with constraints added by the synthesizer
+ Resource consumption: the execution time of the synthesizer

### run the automatic testing script

*If you are on MacOS, you may need to run
`export SDKROOT=$(xcrun --sdk macosx --show-sdk-path)`
before invoking the script.*

Run `python3 measure.py` to run the synthesizer on
`test/stl_algorithm.cpp` and `test/boost_special_functions.cpp`,
and report error message reductions.

## miscellaneous

### Docker images

If you want to build the project into a Docker image,
It's recommended to build a linux/amd64 image which can then run on both
amd64 and Apple M1 (arm64) (through Rosetta 2) as described
[here](https://stackoverflow.com/questions/67458621/how-to-run-amd64-docker-image-on-arm64-host-platform).
The reverse direction looks
[more complicated](https://stackoverflow.com/questions/68675532/how-to-run-arm64-docker-images-on-amd64-host-platform).
Multi-platform images also look
[more complicated](https://docs.docker.com/build/building/multi-platform/).

### known issues

It is known that the experimental numbers may change among
different runs. The reason is being investigated, but should be non-essential.

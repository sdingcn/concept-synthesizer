# concept-synthesizer

![](https://github.com/sdingcn/concept-synthesizer/actions/workflows/run_test.yml/badge.svg)

automatically synthesizing C++20 template constraints for function templates

## build

After cloning the repository,
you can either build the project directly on your machine,
or build it inside Docker.

*In either case, you may want to change N_THREADS in `setup.sh` before running it.*

### normal build

+ Platform dependencies: MacOS/Linux.
+ Software dependencies: python3 (>= 3.5), wget, unzip, git,
  cmake, make, Clang (>= 17) / AppleClang (>= 15) / GCC (>= 14).

```
./setup.sh
```

### Docker build

```
docker build -t concept-synthesizer-image .
docker run --rm -ti concept-synthesizer-image /bin/bash
cd workspace/
CC=gcc-14 CXX=g++-14 ./setup.sh
```

## run

*If you are on MacOS, you may need to run
`export SDKROOT=$(xcrun --sdk macosx --show-sdk-path)`
before running the synthesizer. You certainly
don't need this inside the above Docker container.*

### run on a single file

```
./llvm-project-llvmorg-19.1.1/build_frontend/bin/clang-19 \
  -nostdinc++ \
  -isystem ./llvm-project-llvmorg-19.1.1/build_headers/include/c++/v1 \
  -std=c++20 \
  -E \
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

Run `python3 run.py check` to do a quick check of everything.
This is also what the CI currently does.

Run `python3 run.py measure` to run the synthesizer on
`test/stl_algorithm.cpp` and `test/boost_special_functions.cpp`,
and report error message reduction statistics.

# concept-synthesizer

automatically synthesizing C++20 template constraints for function templates

## dependencies

This project was tested on MacOS (Apple M1), but should also work on major Linux platforms.

Requirements include `python3`, `wget`, `unzip`, `git`, `cmake`, `make`, `clang++/g++`.

## setup

Run `./setup.sh` to download LLVM source code and build the project.

## run

### run on a single `.ii` file

```
./llvm-project-llvmorg-19.1.1/build_frontend/bin/concept-synthesizer <c++-file> -- -x c++-cpp-output -std=c++20
```

The synthesizer outputs five "sections" to `stdout`.
+ Individual results: (nontrivial) synthesis information for each type template parameter
+ Invalid calls: generated calls to these function templates (used to test error messages)
+ Statistics: synthesis statistics
+ Constrained code: this is the rewritten C++ code
+ Resource consumption: currently only reports the time taken

### run the automatic error measurement script

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

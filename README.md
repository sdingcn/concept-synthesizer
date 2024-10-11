# concept-synthesizer
Automatically synthesizing C++20 template constraints for function templates

## dependencies

This project was tested on MacOS (Apple M1), but it should also work on Linux platforms.

Requirements include `python3`, `wget`, `unzip`, `git`, `cmake`, `make`,
and a C++ compiler supporting C++20 (such as `clang++`).

Building the synthesizer needs the source code of Clang/LLVM,
which will be downloaded by the build script `build.sh`.

The error message reduction measurement script `measure-error.py`
has two default measurement targets (STL `algorithm` and Boost `special_functions`),
where the Boost library will be automatically downloaded.

## build

```
./build.sh
```

## run

### run the synthesizer and interpret the results

TODO

### run the automatic error message reduction measurement script

```
python3 measure-error.py
```

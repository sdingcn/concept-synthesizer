# concept-synthesizer
Automatically synthesizing C++20 template constraints for function templates

## dependencies

This project was tested on MacOS (Apple M1), but it should also work on Linux platforms.

Requirements include `python3`, `wget`, `unzip`, `git`, `cmake`, `make`, `clang++`.

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

### run the synthesizer on a single C++ file and interpret the result

```
./llvm-project-llvmorg-19.1.1/build/bin/concept-synthesizer <c++-file> -- -std=c++20
```

You may need to tell the synthesizer include search paths for your C++ file, via `-I`.
You can also use a pre-processed C++ file.

The synthesizer outputs five "sections" to `stdout`.
+ Individual results: (nontrivial) synthesis information for each type template parameter
+ Invalid calls: generated calls to these function templates (used to test error messages)
+ Statistics: synthesis statistics
+ Constrained code: this is the rewritten C++ code
+ Resource consumption: currently only reports the time taken

For example, running the synthesizer on `examples/demo.cpp`
outputs the following result (certain details are omitted).

```
[-[Individual results]-]
...
[-[]-]
[-[Invalid calls]-]
...
[-[]-]
[-[Statistics]-]
...
[-[]-]
[-[Constrained code]-]
template <typename T>
// added by concept-synth, original LN: 1
requires
requires (T o) { o.F(); }
void f(T x) {
    x.F();
}

template <typename U>
// added by concept-synth, original LN: 6
requires
requires (U o) { o.G(); }
void g(U x) {
    x.G();
}

template <typename V>
// added by concept-synth, original LN: 11
requires
(
 requires (V o) { o.G(); } &&
 requires (V o) { o.F(); } &&
 requires (V x0) { x0++; }
)
void h(V x) {
    x++;
    if (x) {
        f(x);
    } else {
        g(x);
    }
}

int main() {
}

[-[]-]
[-[Resource consumption]-]
...
[-[]-]
```

### run the automatic error message reduction measurement script

This script assumes there is a command `clang++` on the machine.
So it is testing the error messages of that compiler.

```
python3 measure-error.py
```

### docker images

It's recommended to build a linux/amd64 image which can then be run on both
amd64 and Apple M1 (arm64) (through Rosetta 2) as described [here](https://stackoverflow.com/questions/67458621/how-to-run-amd64-docker-image-on-arm64-host-platform).

The reverse direction looks [more complicated](https://stackoverflow.com/questions/68675532/how-to-run-arm64-docker-images-on-amd64-host-platform).

Multi-platform images also look [more complicated](https://docs.docker.com/build/building/multi-platform/).

### known issues

If you're experiencing compatibility issues
(e.g. you use the `clang++` on your machine to pre-process C++ files
but then use this in-source built synthesizer on the pre-processed files),
you can consider building `clang` and `libcxx` from the source as well.

It is known that the detailed experimental numbers may change among
different runs. The reason is being investigated, but should be non-essential.

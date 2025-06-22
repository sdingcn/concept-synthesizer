# concept-synthesizer

automatically synthesizing C++20 template constraints for function templates

## dependencies

This project was tested on MacOS (Apple M1), but should also work on major Linux platforms.

Requirements include `python3`, `wget`, `unzip`, `git`, `cmake`, `make`, `clang++`.

## build

Run `./build.sh` to download LLVM source code into this repository and build the project.

## run

### run on single file

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

### run the automatic error measurement script

Run `python3 measure.py` to download the Boost library,
run the synthesizer on `test/stl_algorithm.cpp` and `test/boost_special_functions.cpp`,
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

import os.path
import subprocess
import sys
from typing import List, Tuple, Union

def execute(cmd: List[str], i: Union[None, str] = None) -> Tuple[int, str, str]:
    result = subprocess.run(
        cmd,
        text = True,
        input = i,
        capture_output = True
    )
    return (result.returncode, result.stdout, result.stderr)

def call_preprocessor(clang_path: str, fpath: str, ipaths: List[str]) -> str:
    cmd = [clang_path, "-std=c++20", "-E", "-P", fpath]
    for ipath in ipaths:
        cmd.append("-I" + ipath)
    result = execute(cmd)
    if result[0] != 0:
        sys.exit(f"Error: {cmd} failed\n{result[2]}")
    return result[1]

def call_synthesizer(synthesizer_path: str, fpath: str) -> str:
    cmd = [synthesizer_path, fpath, "--", "-std=c++20"]
    result = execute(cmd)
    if result[0] != 0:
        sys.exit(f"Error: {cmd} failed\n{result[2]}")
    return result[1]

def cut_synthesizer_output_section(output: str, section: str) -> str:
    section_header = f'[-[{section}]-]'
    start = output.find(section_header) + len(section_header)
    end = output.find('[-[]-]', start)
    return output[start:end].strip()

def compose_invalid_code(main: str, call: str) -> str:
    lines = main.splitlines()
    lines.insert(-2, "struct S {};")  # insert before
    lines.insert(-1, "S s;")
    lines.insert(-1, call)
    return "\n".join(lines)

def get_error_message(clang_path: str, code: str) -> str:
    cmd = [clang_path, "-x", "c++", "-std=c++20", "-w", "-"]
    result = execute(cmd, code)
    if result[0] == 0:
        sys.exit(f"Error: {cmd} didn't fail as expected\n{result[1]}")
    return result[2]

def get_callee_name(call: str) -> str:
    return call.split("(")[0]

def classify(lst: List[int]) -> List[Tuple[int, int, int]]:
    results = []
    lst.sort()
    n = len(lst)
    i = 0
    start = 0
    step = 50
    cnt = 0
    while i < n:
        if lst[i] < start + step:
            cnt += 1
            i += 1
        else:
            results.append((start, start + step, cnt))
            start = start + step
            cnt = 0
    if cnt > 0:
        results.append((start, start + step, cnt))
    return results

def run_benchmark(fpath: str, includes: List[str], name_prefix: str) -> None:
    print(f"### started error message measurement for {fpath}")
    paths = {
        "clang": "clang++",
        "synthesizer": "./llvm-project-llvmorg-19.1.1/build/bin/concept-synthesizer",
        "source": fpath,
        "preprocessed": fpath + ".preprocessed.cc",
    }
    preprocessed_code = call_preprocessor(paths["clang"], paths["source"], includes)
    with open(paths["preprocessed"], "w") as f:
        f.write(preprocessed_code)
    synthesizer_output = call_synthesizer(paths["synthesizer"], paths["preprocessed"])
    constrained_code = cut_synthesizer_output_section(synthesizer_output, "Constrained code")
    invalid_calls = [
        ic
        for ic in cut_synthesizer_output_section(synthesizer_output, "Invalid calls").splitlines()
        if ic and get_callee_name(ic).startswith(name_prefix)
    ]
    print("[statistics]")
    print(cut_synthesizer_output_section(synthesizer_output, "Statistics"))
    print("[resource consumption]")
    print(cut_synthesizer_output_section(synthesizer_output, "Resource consumption"))
    print("[error reduction measurement]")
    n = len(invalid_calls)
    original_error_lengths = []
    constrained_error_lengths = []
    for (i, call) in enumerate(invalid_calls):
        error1 = get_error_message(paths["clang"], compose_invalid_code(preprocessed_code, call))
        error2 = get_error_message(paths["clang"], compose_invalid_code(constrained_code, call))
        if i % 10 == 0:
            print("* sample {{{")
            print(error1)
            print("* ===")
            print(error2)
            print("* }}}")
        original_error_length = len(error1.splitlines())
        constrained_error_length = len(error2.splitlines())
        print(f"{i + 1}/{n}: original = {original_error_length}\t"
              f"constrained = {constrained_error_length}\t"
              f"name = {get_callee_name(call)}")
        original_error_lengths.append(original_error_length)
        constrained_error_lengths.append(constrained_error_length)
    if n > 0:
        print(f"original average = {round(sum(original_error_lengths) / n, 3)}")
        print(f"constrained average = {round(sum(constrained_error_lengths) / n, 3)}")
        print("original distribution:")
        original_intervals = classify(original_error_lengths)
        for begin, end, count in original_intervals:
            print(f"[{begin}, {end}) = {count}")
        print("constrained distribution:")
        constrained_intervals = classify(constrained_error_lengths)
        for begin, end, count in constrained_intervals:
            print(f"[{begin}, {end}) = {count}")

if __name__ == "__main__":
    if not os.path.exists('./boost_1_84_0/'):
        print("### downloading boost...")
        execute(["wget", "https://archives.boost.io/release/1.84.0/source/boost_1_84_0.zip"])
        print("### unzipping boost...")
        execute(["unzip", "-q", "boost_1_84_0.zip"])
        execute(["rm", "boost_1_84_0.zip"])
    run_benchmark("./examples/stl-algo.cpp", [], "")
    run_benchmark("./examples/boost-sf.cpp", ["./boost_1_84_0/"], "boost")

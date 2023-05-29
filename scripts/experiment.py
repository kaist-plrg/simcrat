#! /usr/bin/env python3

import argparse, itertools, os, subprocess, sys


def split_list(l, ty):
    return [ty(x) for x in l.split()]


def add_list(l1, l2):
    return [x + y for x, y in itertools.zip_longest(l1, l2, fillvalue=0)]


def run(args):
    cmd = default_cmd + args
    res = subprocess.run(cmd, capture_output=True, text=True)
    if log_file:
        with open(log_file, "a") as f:
            f.write(res.stderr.strip())
            f.write("\n")
    return res.stdout.strip()


script_dir = os.path.dirname(os.path.abspath(__file__))

parser = argparse.ArgumentParser()
parser.add_argument("-a")
parser.add_argument("-l")
parser.add_argument("--db-host")
parser.add_argument("--db-port")
parser.add_argument("--db-password")
parser.add_argument("--no-candidate", action="store_true")
parser.add_argument("--no-augmentation", action="store_true")
parser.add_argument("--no-fix", action="store_true")
parser.add_argument("--no-stage", action="store_true")
parser.add_argument("dir")
args = parser.parse_args()

default_cmd = [
    "cargo",
    "run",
    "--release",
    "--manifest-path",
    os.path.join(script_dir, "../Cargo.toml"),
    "--",
    "--db-name",
    "simcrat",
    "-q",
    "--show-error-num",
    "--show-long-num",
    "--show-openai-stat",
    "--show-signature",
]

if args.a:
    default_cmd.extend(["-a", args.a])

if args.l:
    log_file = args.l

if args.db_host:
    default_cmd.extend(["--db-host", args.db_host])

if args.db_port:
    default_cmd.extend(["--db-port", args.db_port])

if args.db_password:
    default_cmd.extend(["--db-password", args.db_password])

if args.no_candidate:
    default_cmd.append("--no-candidate")

if args.no_augmentation:
    default_cmd.append("--no-augmentation")

if args.no_fix:
    default_cmd.append("--no-fix")

if args.no_stage:
    default_cmd.append("--no-stage")

with open(os.path.join(script_dir, "bench_list")) as f:
    bench_list = [x.strip() for x in f.readlines()]

print("long\terrors\ttokens\tsigs\tprogram")

signatures = []

for bench in bench_list:
    try:
        build_file = os.path.join(args.dir, bench + ".json")
        res = run([build_file])
        arr = res.split("\n")
        errors = arr[0]
        long = arr[1]
        tokens = split_list(arr[2], float)
        tokens = int(tokens[0] + tokens[1])
        sigs = split_list(arr[3], int)
        signatures = add_list(signatures, sigs)
        sigs = sigs[0]
        print(f"{long}\t{errors}\t{tokens}\t{sigs}\t{bench}")
    except KeyboardInterrupt:
        sys.exit(1)
    except:
        print(f"\t\t\t\t{bench}")

print("Opt\tTup\tVec\tStr\tFile\tNev\tGen\tVoid\tPtr\tEtc")
print("\t".join([str(x) for x in signatures[1:]]))

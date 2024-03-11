# Tymcrat

**T**ype-**M**igrating **C**-to-**R**ust **A**utomatic **T**ranslator

This repository provides the implementation of the tool Tymcrat of the paper
*Type-Migrating C-to-Rust Translation using a Large Language Model* and the
evaluation scripts.

## Build

The following commands build Tymcrat:

```bash
cd ~
git clone https://github.com/kaist-plrg/simcrat.git tymcrat
cd tymcrat
rustup component add rust-src rustc-dev llvm-tools-preview
cd deps_crate
cargo build
cd ..
cargo build --release
```

## Benchmark Setup

The following commands set up the benchmark programs.

```bash
cd ~
tymcrat/scripts/gnu.sh
```

## Evaluation

The following commands run the evaluation scripts:

```bash
cd ~/tymcrat
scripts/run.sh 0 0
scripts/run.sh 0 1
scripts/run.sh 0 2
scripts/run.sh 0 3
scripts/run.sh 1 0
scripts/run.sh 1 1
scripts/run.sh 1 2
scripts/run.sh 1 3
scripts/run.sh 2 0
scripts/run.sh 2 1
scripts/run.sh 2 2
scripts/run.sh 2 3
scripts/run.sh 3 0
scripts/run.sh 3 1
scripts/run.sh 3 2
scripts/run.sh 3 3
scripts/run.sh 4 0
scripts/run.sh 4 1
scripts/run.sh 4 2
scripts/run.sh 4 3
```

* The first argument decides the number of candidate signatures.
* The second argument decides:
  * 0: fix - yes, augmentation - yes
  * 1: fix - no, augmentation - yes
  * 2: fix - yes, augmentation - no
  * 3: fix - no, augmentation - no

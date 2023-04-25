# Simcrat

**Si**gnature-**M**odernizing **C**-to-**R**ust **A**utomatic **T**ranslator

```bash
git clone https://github.com/kaist-plrg/simcrat.git
cd simcrat
rustup component add rust-src rustc-dev llvm-tools-preview
cargo build --release
cd deps_crate
cargo build
cd ..
cargo test --release
```

#! /bin/bash

if [ $# -ne 1 ]; then
  exit 1
fi

script_dir=$(dirname $0)

while read -r line
do
  loc=`cloc --include-lang=C --csv --hide-rate --quiet "$1/$line" 2> /dev/null | tail -n 1 | awk -F',' '{print $NF}'`
  size=`cargo run --release --manifest-path "$script_dir/../Cargo.toml" -- --parsing-only --show-program-size "$1/$line.json" 2> /dev/null`
  echo -e "$loc\t$size\t$line"
done < $script_dir/bench_list

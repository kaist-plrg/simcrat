#!/bin/bash

# $1: num signatures
# $2: option

if [ $# -ne 2 ]; then
  exit 1
fi

function run(){
  cargo run --release -- --no-stage -q $@
}

OPT="-a .openai_api_key --real-time --show-time"

if [ $1 -eq 0 ]; then
  OPT+="--no-candidate "
else
  OPT+="--num-signatures $1 "
fi

if [ $2 -eq 1 ]; then
  OPT+="--no-fix "
elif [ $2 -eq 2 ]; then
  OPT+="--no-augmentation "
elif [ $2 -eq 3 ]; then
  OPT+="--no-augmentation --no-fix "
fi

run $OPT ~/which-2.21.json
run $OPT ~/ed-1.19.json
run $OPT ~/time-1.9.json
run $OPT ~/libtool-2.4.7.json
run $OPT ~/pexec-1.0rc8.json
run $OPT ~/units-2.22.json
run $OPT ~/pth-2.0.7.json
run $OPT ~/hello-2.12.1.json
run $OPT ~/adns-1.6.0.json
run $OPT ~/bc-1.07.1.json
run $OPT ~/libosip2-5.3.1.json
run $OPT ~/mcsim-6.2.0.json
run $OPT ~/mtools-4.0.43.json
run $OPT ~/indent-2.2.13.json
run $OPT ~/less-633.json
run $OPT ~/cflow-1.7.json
run $OPT ~/gzip-1.12.json
run $OPT ~/dap-3.10.json
run $OPT ~/readline-8.2.json
run $OPT ~/patch-2.7.6.json
run $OPT ~/rcs-5.10.1.json
run $OPT ~/make-4.4.1.json
run $OPT ~/enscript-1.6.6.json
run $OPT ~/cpio-2.14.json
run $OPT ~/screen-4.9.0.json
run $OPT ~/nano-7.2.json
run $OPT ~/sed-4.9.json
run $OPT ~/uucp-1.07.json
run $OPT ~/gprolog-1.5.0.json
run $OPT ~/parted-3.6.json
run $OPT ~/gawk-5.2.2.json
run $OPT ~/bison-3.8.2.json
run $OPT ~/diffutils-3.10.json
run $OPT ~/nettle-3.9.json
run $OPT ~/grep-3.11.json
run $OPT ~/tar-1.34.json
run $OPT ~/glpk-5.0.json
run $OPT ~/m4-1.4.19.json
run $OPT ~/findutils-4.9.0.json
run $OPT ~/wget-1.21.4.json
run $OPT ~/gmp-6.2.1.json

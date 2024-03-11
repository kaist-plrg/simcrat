#!/bin/zsh

set -e

wget http://ftp.kaist.ac.kr/gnu/which/which-2.21.tar.gz
tar -xf which-2.21.tar.gz
rm which-2.21.tar.gz
cd which-2.21
./configure
intercept-build make
mv compile_commands.json ../which-2.21.json
cd ..
wget http://ftp.kaist.ac.kr/gnu/ed/ed-1.19.tar.lz
tar -xf ed-1.19.tar.lz
rm ed-1.19.tar.lz
cd ed-1.19
./configure
intercept-build make
mv compile_commands.json ../ed-1.19.json
cd ..
wget http://ftp.kaist.ac.kr/gnu/time/time-1.9.tar.gz
tar -xf time-1.9.tar.gz
rm time-1.9.tar.gz
cd time-1.9
./configure
intercept-build make
mv compile_commands.json ../time-1.9.json
cd ..
wget http://ftp.kaist.ac.kr/gnu/libtool/libtool-2.4.7.tar.gz
tar -xf libtool-2.4.7.tar.gz
rm libtool-2.4.7.tar.gz
cd libtool-2.4.7
./configure
intercept-build make
mv compile_commands.json ../libtool-2.4.7.json
cd ..
wget http://ftp.kaist.ac.kr/gnu/pexec/pexec-1.0rc8.tar.gz
tar -xf pexec-1.0rc8.tar.gz
rm pexec-1.0rc8.tar.gz
cd pexec-1.0rc8
./configure
intercept-build make
mv compile_commands.json ../pexec-1.0rc8.json
cd ..
wget http://ftp.kaist.ac.kr/gnu/units/units-2.22.tar.gz
tar -xf units-2.22.tar.gz
rm units-2.22.tar.gz
cd units-2.22
./configure
sed -i 's/_Pragma ("GCC diagnostic push")//g' **/*.{c,h}
sed -i 's/_Pragma ("GCC diagnostic pop")//g' **/*.{c,h}
intercept-build make
mv compile_commands.json ../units-2.22.json
cd ..
wget http://ftp.kaist.ac.kr/gnu/pth/pth-2.0.7.tar.gz
tar -xf pth-2.0.7.tar.gz
rm pth-2.0.7.tar.gz
cd pth-2.0.7
./configure
intercept-build make
mv compile_commands.json ../pth-2.0.7.json
cd ..
wget http://ftp.kaist.ac.kr/gnu/hello/hello-2.12.1.tar.gz
tar -xf hello-2.12.1.tar.gz
rm hello-2.12.1.tar.gz
cd hello-2.12.1
./configure
sed -i 's/_GL_ATTRIBUTE_FALLTHROUGH//g' lib/attribute.h
intercept-build make
mv compile_commands.json ../hello-2.12.1.json
cd ..
wget http://ftp.kaist.ac.kr/gnu/adns/adns-1.6.0.tar.gz
tar -xf adns-1.6.0.tar.gz
rm adns-1.6.0.tar.gz
cd adns-1.6.0
./configure
intercept-build make
mv compile_commands.json ../adns-1.6.0.json
cd ..
wget http://ftp.kaist.ac.kr/gnu/bc/bc-1.07.1.tar.gz
tar -xf bc-1.07.1.tar.gz
rm bc-1.07.1.tar.gz
cd bc-1.07.1
./configure
sed -i '315,317s/.*//g' bc/bc.c
sed -i '319s/.*//g' bc/bc.c
intercept-build make
mv compile_commands.json ../bc-1.07.1.json
cd ..
wget http://ftp.kaist.ac.kr/gnu/osip/libosip2-5.3.1.tar.gz
tar -xf libosip2-5.3.1.tar.gz
rm libosip2-5.3.1.tar.gz
cd libosip2-5.3.1
./configure
intercept-build make
mv compile_commands.json ../libosip2-5.3.1.json
cd ..
wget http://ftp.kaist.ac.kr/gnu/mcsim/mcsim-6.2.0.tar.gz
tar -xf mcsim-6.2.0.tar.gz
rm mcsim-6.2.0.tar.gz
cd mcsim-6.2.0
./configure
intercept-build make
mv compile_commands.json ../mcsim-6.2.0.json
cd ..
wget http://ftp.kaist.ac.kr/gnu/mtools/mtools-4.0.43.tar.gz
tar -xf mtools-4.0.43.tar.gz
rm mtools-4.0.43.tar.gz
cd mtools-4.0.43
./configure
sed -i 's/# *pragma GCC diagnostic push//g' **/*.{c,h}
sed -i 's/# *pragma GCC diagnostic pop//g' **/*.{c,h}
sed -i 's/L\x27\\\(.\)\x27/\x27\\\1\x27/g' **/*.{c,h}
intercept-build make
mv compile_commands.json ../mtools-4.0.43.json
cd ..
wget http://ftp.kaist.ac.kr/gnu/indent/indent-2.2.13.tar.gz
tar -xf indent-2.2.13.tar.gz
rm indent-2.2.13.tar.gz
cd indent-2.2.13
./configure
intercept-build make
mv compile_commands.json ../indent-2.2.13.json
cd ..
wget http://ftp.kaist.ac.kr/gnu/less/less-633.tar.gz
tar -xf less-633.tar.gz --no-same-owner
rm less-633.tar.gz
cd less-633
./configure
intercept-build make
mv compile_commands.json ../less-633.json
cd ..
wget http://ftp.kaist.ac.kr/gnu/cflow/cflow-1.7.tar.gz
tar -xf cflow-1.7.tar.gz
rm cflow-1.7.tar.gz
cd cflow-1.7
./configure
sed -i 's/_Pragma ("GCC diagnostic push")//g' **/*.{c,h}
sed -i 's/_Pragma ("GCC diagnostic pop")//g' **/*.{c,h}
sed -i 's/__attribute__ ((__fallthrough__))//g' **/*.{c,h}
sed -i 's/L\x27\(.\)\x27/\x27\1\x27/g' **/*.{c,h}
sed -i 's/L\x27\\\(.\)\x27/\x27\\\1\x27/g' **/*.{c,h}
intercept-build make
mv compile_commands.json ../cflow-1.7.json
cd ..
wget http://ftp.kaist.ac.kr/gnu/gzip/gzip-1.12.tar.gz
tar -xf gzip-1.12.tar.gz
rm gzip-1.12.tar.gz
cd gzip-1.12
./configure --disable-threads
intercept-build make
sed -i '65s/^#if /#if !/g' lib/fpucw.h
sed -i 's/_GL_ATTRIBUTE_FALLTHROUGH//g' lib/attribute.h
sed -i 's/__attribute__ ((__fallthrough__))//g' tailor.h
mv compile_commands.json ../gzip-1.12.json
cd ..
wget http://ftp.kaist.ac.kr/gnu/dap/dap-3.10.tar.gz
tar -xf dap-3.10.tar.gz
rm dap-3.10.tar.gz
cd dap-3.10
./configure
sed -i '345s/\\{/{/g' src/sbstrans3.c
sed -i '1617s/\\%/%/g' src/dap0.c
intercept-build make
mv compile_commands.json ../dap-3.10.json
cd ..
wget http://ftp.kaist.ac.kr/gnu/readline/readline-8.2.tar.gz
tar -xf readline-8.2.tar.gz
rm readline-8.2.tar.gz
cd readline-8.2
./configure
sed -i 's/L\x27\(.\)\x27/\x27\1\x27/g' **/*.{c,h}
sed -i 's/L\x27\\\(.\)\x27/\x27\\\1\x27/g' **/*.{c,h}
intercept-build make
mv compile_commands.json ../readline-8.2.json
cd ..
wget http://ftp.kaist.ac.kr/gnu/patch/patch-2.7.6.tar.gz
tar -xf patch-2.7.6.tar.gz
rm patch-2.7.6.tar.gz
cd patch-2.7.6
./configure
sed -i '828,830s/.*//g' lib/parse-datetime.c
sed -i '832s/.*//g' lib/parse-datetime.c
sed -i 's/__attribute__ ((__fallthrough__))//g' lib/*.c
intercept-build make
mv compile_commands.json ../patch-2.7.6.json
cd ..
wget http://ftp.kaist.ac.kr/gnu/rcs/rcs-5.10.1.tar.lz
tar -xf rcs-5.10.1.tar.lz
rm rcs-5.10.1.tar.lz
cd rcs-5.10.1
./configure
sed -i 's/__attribute__ ((__fallthrough__))//g' **/*.{c,h}
sed -i '65s/^#if /#if !/g' lib/fpucw.h
intercept-build make
mv compile_commands.json ../rcs-5.10.1.json
cd ..
wget http://ftp.kaist.ac.kr/gnu/make/make-4.4.1.tar.gz
tar -xf make-4.4.1.tar.gz
rm make-4.4.1.tar.gz
cd make-4.4.1
./configure
intercept-build make
mv compile_commands.json ../make-4.4.1.json
cd ..
wget http://ftp.kaist.ac.kr/gnu/enscript/enscript-1.6.6.tar.gz
tar -xf enscript-1.6.6.tar.gz
rm enscript-1.6.6.tar.gz
cd enscript-1.6.6
./configure
intercept-build make
mv compile_commands.json ../enscript-1.6.6.json
cd ..
wget http://ftp.kaist.ac.kr/gnu/cpio/cpio-2.14.tar.gz
tar -xf cpio-2.14.tar.gz
rm cpio-2.14.tar.gz
cd cpio-2.14
./configure
sed -i 's/_Pragma ("GCC diagnostic push")//g' **/*.{c,h}
sed -i 's/_Pragma ("GCC diagnostic pop")//g' **/*.{c,h}
sed -i 's/__attribute__ ((__fallthrough__))//g' **/*.{c,h}
sed -i 's/L\x27\\\(.\)\x27/\x27\\\1\x27/g' **/*.{c,h}
sed -i 's/^ *static_assert *(.*);//g' **/*.{c,h}
sed -i '63,64s/.*//g' gnu/strtoimax.c
sed -i '48,49s/.*//g' gnu/argmatch.h
intercept-build make
mv compile_commands.json ../cpio-2.14.json
cd ..
wget http://ftp.kaist.ac.kr/gnu/screen/screen-4.9.0.tar.gz
tar -xf screen-4.9.0.tar.gz
rm screen-4.9.0.tar.gz
cd screen-4.9.0
./autogen.sh
./configure
intercept-build make
mv compile_commands.json ../screen-4.9.0.json
cd ..
wget http://ftp.kaist.ac.kr/gnu/nano/nano-7.2.tar.gz
tar -xf nano-7.2.tar.gz
rm nano-7.2.tar.gz
cd nano-7.2
./configure --disable-threads
sed -i '1331,1334s/.*//g' config.h
sed -i '1336s/.*//g' config.h
sed -i '535,536s/.*//g' lib/regex.h
sed -i '692s/.*//g' lib/regex.h
sed -i '65s/^#if /#if !/g' lib/fpucw.h
sed -i 's/L'"'"'/'"'"'/g' lib/regex_internal.{c,h}
sed -i 's/__attribute__ ((__fallthrough__))//g' lib/*.{c,h}
sed -i 's/_GL_ATTRIBUTE_FALLTHROUGH//g' lib/attribute.h
sed -i '42s/.*//g' lib/malloca.c
intercept-build make
mv compile_commands.json ../nano-7.2.json
cd ..
wget http://ftp.kaist.ac.kr/gnu/sed/sed-4.9.tar.gz
tar -xf sed-4.9.tar.gz
rm sed-4.9.tar.gz
cd sed-4.9
./configure --disable-threads
sed -i '1507,1510s/.*//g' config.h
sed -i '1512s/.*//g' config.h
sed -i '535,536s/.*//g' lib/regex.h
sed -i '692s/.*//g' lib/regex.h
sed -i 's/^ *static_assert *(.*//g' gnulib-tests/*.c
sed -i 's/^ *static_assert *(.*//g' lib/*.c
sed -i '125s/.*//g' lib/localeinfo.c
sed -i 's/__attribute__ ((__fallthrough__))//g' lib/*.{c,h}
sed -i 's/__attribute__ ((__fallthrough__))//g' sed/sed.h
sed -i 's/_GL_ATTRIBUTE_FALLTHROUGH//g' lib/attribute.h
sed -i 's/L'"'"'/'"'"'/g' lib/regex_internal.{c,h}
sed -i '206,207s/.*//g' sed/compile.c
sed -i '209s/.*//g' sed/compile.c
intercept-build make
mv compile_commands.json ../sed-4.9.json
cd ..
wget http://ftp.kaist.ac.kr/gnu/uucp/uucp-1.07.tar.gz
tar -xf uucp-1.07.tar.gz
rm uucp-1.07.tar.gz
cd uucp-1.07
./configure
intercept-build make
mv compile_commands.json ../uucp-1.07.json
cd ..
wget http://ftp.kaist.ac.kr/gnu/gprolog/gprolog-1.5.0.tar.gz
tar -xf gprolog-1.5.0.tar.gz
rm gprolog-1.5.0.tar.gz
cd gprolog-1.5.0/src
./configure
sed -i 's/# *pragma GCC diagnostic push//g' **/*.{c,h}
sed -i 's/# *pragma GCC diagnostic pop//g' **/*.{c,h}
intercept-build make
mv compile_commands.json ../../gprolog-1.5.0.json
cd ../..
wget http://ftp.kaist.ac.kr/gnu/parted/parted-3.6.tar.xz
tar -xf parted-3.6.tar.xz
rm parted-3.6.tar.xz
cd parted-3.6
./configure --disable-device-mapper --without-readline --disable-threads --disable-nls
sed -i 's/__attribute__ ((packed.*))//g' libparted/**/*.{c,h}
sed -i 's/__attribute__ ((packed.*))//g' include/**/*.h
sed -i 's/^ *static_assert *(.*//g' lib/*.c
sed -i '116s/.*//g' parted/parted.c
sed -i '2236s/\?:/? strrchr(name, \x27\/\x27) :/g' libparted/arch/linux.c
sed -i 's/_GL_ATTRIBUTE_FALLTHROUGH//g' lib/attribute.h
sed -i 's/__attribute__ ((__fallthrough__))//g' lib/*.{c,h}
sed -i 's/L'"'"'/'"'"'/g' lib/regex_internal.{c,h}
sed -i 's/L'"'"'/'"'"'/g' parted/table.c
sed -i 's/L'"'"'/'"'"'/g' lib/strtol.c
sed -i '1229,1232s/.*//g' lib/config.h
sed -i '1234s/.*//g' lib/config.h
sed -i '535,536s/.*//g' lib/regex.h
sed -i '692s/.*//g' lib/regex.h
sed -i 's/__attribute__ ((packed,aligned(2)))//g' libparted/labels/atari.c
intercept-build make
mv compile_commands.json ../parted-3.6.json
cd ..
wget http://ftp.kaist.ac.kr/gnu/gawk/gawk-5.2.2.tar.gz
tar -xf gawk-5.2.2.tar.gz
rm gawk-5.2.2.tar.gz
cd gawk-5.2.2
./configure
sed -i 's/_Pragma ("GCC diagnostic push")//g' **/*.{c,h}
sed -i 's/_Pragma ("GCC diagnostic pop")//g' **/*.{c,h}
sed -i 's/# pragma GCC diagnostic push//g' **/*.{c,h}
sed -i 's/# pragma GCC diagnostic pop//g' **/*.{c,h}
sed -i 's/L\x27/\x27/g' node.c support/regex_internal.c interpret.h support/regex_internal.h
sed -i 's/__attribute__ ((__fallthrough__))//g' support/*.{c,h}
intercept-build make
mv compile_commands.json ../gawk-5.2.2.json
cd ..
wget http://ftp.kaist.ac.kr/gnu/bison/bison-3.8.2.tar.gz
tar -xf bison-3.8.2.tar.gz
rm bison-3.8.2.tar.gz
cd bison-3.8.2
./configure --disable-threads
sed -i 's/_Pragma ("GCC diagnostic push")//g' **/*.{c,h}
sed -i 's/_Pragma ("GCC diagnostic pop")//g' **/*.{c,h}
sed -i 's/__attribute__ ((__fallthrough__))//g' **/*.{c,h}
sed -i '65s/^#if /#if !/g' lib/fpucw.h
intercept-build make
mv compile_commands.json ../bison-3.8.2.json
cd ..
wget http://ftp.kaist.ac.kr/gnu/diffutils/diffutils-3.10.tar.xz
tar -xf diffutils-3.10.tar.xz
rm diffutils-3.10.tar.xz
cd diffutils-3.10
./configure --disable-threads
sed -i 's/_Pragma ("GCC diagnostic push")//g' **/*.{c,h}
sed -i 's/_Pragma ("GCC diagnostic pop")//g' **/*.{c,h}
sed -i 's/# pragma GCC diagnostic push//g' **/*.{c,h}
sed -i 's/# pragma GCC diagnostic pop//g' **/*.{c,h}
sed -i 's/__attribute__ ((__fallthrough__))//g' **/*.{c,h}
sed -i 's/L\x27\(.\)\x27/\x27\1\x27/g' **/*.{c,h}
sed -i 's/L\x27\\\(.\)\x27/\x27\\\1\x27/g' **/*.{c,h}
sed -i 's/^ *static_assert *(.*);//g' **/*.{c,h}
sed -i '25s/.*//g' gnulib-tests/test-assert.c
sed -i '28s/.*//g' gnulib-tests/test-limits-h.c
sed -i '57,60s/.*//g' lib/exclude.c
sed -i '63,64s/.*//g' lib/strtoimax.c
sed -i '48,49s/.*//g' lib/argmatch.h
intercept-build make
mv compile_commands.json ../diffutils-3.10.json
cd ..
wget http://ftp.kaist.ac.kr/gnu/nettle/nettle-3.9.tar.gz
tar -xf nettle-3.9.tar.gz
rm nettle-3.9.tar.gz
cd nettle-3.9
./configure
intercept-build make
mv compile_commands.json ../nettle-3.9.json
cd ..
wget http://ftp.kaist.ac.kr/gnu/grep/grep-3.11.tar.gz
tar -xf grep-3.11.tar.gz
rm grep-3.11.tar.gz
cd grep-3.11
./configure --disable-threads
sed -i 's/_Pragma ("GCC diagnostic push")//g' **/*.{c,h}
sed -i 's/_Pragma ("GCC diagnostic pop")//g' **/*.{c,h}
sed -i 's/# pragma GCC diagnostic push//g' **/*.{c,h}
sed -i 's/# pragma GCC diagnostic pop//g' **/*.{c,h}
sed -i 's/__attribute__ ((__fallthrough__))//g' **/*.{c,h}
sed -i 's/L\x27\(.\)\x27/\x27\1\x27/g' **/*.{c,h}
sed -i 's/L\x27\\\(.\)\x27/\x27\\\1\x27/g' **/*.{c,h}
sed -i 's/^ *static_assert *(.*);//g' **/*.{c,h}
sed -i '25s/.*//g' gnulib-tests/test-assert.c
sed -i '28s/.*//g' gnulib-tests/test-limits-h.c
sed -i '57,60s/.*//g' lib/exclude.c
sed -i '124,125s/.*//g' lib/localeinfo.c
sed -i '63,64s/.*//g' lib/strtoimax.c
sed -i '48,49s/.*//g' lib/argmatch.h
intercept-build make
mv compile_commands.json ../grep-3.11.json
cd ..
wget http://ftp.kaist.ac.kr/gnu/tar/tar-1.34.tar.gz
tar -xf tar-1.34.tar.gz
rm tar-1.34.tar.gz
cd tar-1.34
FORCE_UNSAFE_CONFIGURE=1 ./configure
sed -i 's/_Pragma ("GCC diagnostic push")//g' **/*.{c,h}
sed -i 's/_Pragma ("GCC diagnostic pop")//g' **/*.{c,h}
sed -i 's/__attribute__ ((__fallthrough__))//g' **/*.{c,h}
sed -i 's/L\x27\(.\)\x27/\x27\1\x27/g' **/*.{c,h}
sed -i 's/L\x27\\\(.\)\x27/\x27\\\1\x27/g' **/*.{c,h}
intercept-build make
mv compile_commands.json ../tar-1.34.json
cd ..
wget http://ftp.kaist.ac.kr/gnu/glpk/glpk-5.0.tar.gz
tar -xf glpk-5.0.tar.gz
rm glpk-5.0.tar.gz
cd glpk-5.0
./configure
intercept-build make
mv compile_commands.json ../glpk-5.0.json
cd ..
wget http://ftp.kaist.ac.kr/gnu/m4/m4-1.4.19.tar.gz
tar -xf m4-1.4.19.tar.gz
rm m4-1.4.19.tar.gz
cd m4-1.4.19
./configure --disable-threads
sed -i 's/_Pragma ("GCC diagnostic push")//g' **/*.{c,h}
sed -i 's/_Pragma ("GCC diagnostic pop")//g' **/*.{c,h}
sed -i 's/# pragma GCC diagnostic push//g' **/*.{c,h}
sed -i 's/# pragma GCC diagnostic pop//g' **/*.{c,h}
sed -i 's/__attribute__ ((__fallthrough__))//g' **/*.{c,h}
sed -i 's/L\x27\(.\)\x27/\x27\1\x27/g' **/*.{c,h}
sed -i 's/L\x27\\\(.\)\x27/\x27\\\1\x27/g' **/*.{c,h}
sed -i '65s/^#if /#if !/g' lib/fpucw.h
intercept-build make
mv compile_commands.json ../m4-1.4.19.json
cd ..
wget http://ftp.kaist.ac.kr/gnu/findutils/findutils-4.9.0.tar.xz
tar -xf findutils-4.9.0.tar.xz
rm findutils-4.9.0.tar.xz
cd findutils-4.9.0
./configure --disable-threads
sed -i 's/_Pragma ("GCC diagnostic push")//g' **/*.{c,h}
sed -i 's/_Pragma ("GCC diagnostic pop")//g' **/*.{c,h}
sed -i 's/# pragma GCC diagnostic push//g' **/*.{c,h}
sed -i 's/# pragma GCC diagnostic pop//g' **/*.{c,h}
sed -i 's/__attribute__ ((__fallthrough__))//g' **/*.{c,h}
sed -i 's/L\x27\(.\)\x27/\x27\1\x27/g' **/*.{c,h}
sed -i 's/L\x27\\\(.\)\x27/\x27\\\1\x27/g' **/*.{c,h}
sed -i 's/_GL_UNUSED_LABEL;//g' **/*.{c,h}
intercept-build make
mv compile_commands.json ../findutils-4.9.0.json
cd ..
wget http://ftp.kaist.ac.kr/gnu/wget/wget-1.21.4.tar.gz
tar -xf wget-1.21.4.tar.gz
rm wget-1.21.4.tar.gz
cd wget-1.21.4
./configure --disable-threads --with-ssl=no
sed -i 's/_Pragma ("GCC diagnostic push")//g' **/*.{c,h}
sed -i 's/_Pragma ("GCC diagnostic pop")//g' **/*.{c,h}
sed -i 's/# pragma GCC diagnostic push//g' **/*.{c,h}
sed -i 's/# pragma GCC diagnostic pop//g' **/*.{c,h}
sed -i 's/__attribute__ ((__fallthrough__))//g' **/*.{c,h}
sed -i 's/L\x27\(.\)\x27/\x27\1\x27/g' lib/*.{c,h}
sed -i 's/L\x27\\\(.\)\x27/\x27\\\1\x27/g' lib/*.{c,h}
sed -i 's/^ *static_assert *(.*);//g' **/*.{c,h}
intercept-build make
mv compile_commands.json ../wget-1.21.4.json
cd ..
wget http://ftp.kaist.ac.kr/gnu/gmp/gmp-6.2.1.tar.xz
tar -xf gmp-6.2.1.tar.xz
rm gmp-6.2.1.tar.xz
cd gmp-6.2.1
./configure
intercept-build make
mv compile_commands.json ../gmp-6.2.1.json
cd ..

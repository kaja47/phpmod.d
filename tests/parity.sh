# testing parity with PHP source
#
# This script first generate testing C file which checks sizes and offsets of
# structs and their fields by comparing values computed from D implementation
# with results C sizeof and offsetof expressions.
#
# Then compiles the C file with PHP header files. That compute size and offset
# values from PHP C sources. Finally the compiled program is run, reporting any
# discrepancy.
#
# Protip: When in doubt, run this in php source directory
# ./buildconf
# ./configure --disable-all --enable-cli --with-readline

PHPSOURCE=$1
if [ ! -n "$PHPSOURCE" ]; then
  echo "need path to php sources directory"
  exit
fi
PHPMOD="$PWD"

CFLAGS="-D_GNU_SOURCE -UNDEBUG -DZEND_SIGNALS -DZEND_ENABLE_STATIC_TSRMLS_CACHE=1"
CFLAGS="$CFLAGS -I$PHPSOURCE -I$PHPSOURCE/main -I$PHPSOURCE/TSRM -I$PHPSOURCE/Zend -I$PHPSOURCE/ext/ffi/";


echo "PHP 8.3"
cd "$PHPSOURCE"
git checkout PHP-8.3.20
cd "$PHPMOD"
gdc-14 -c -fpreview=all -fversion=TestParityWithPHP -fversion=PHP83 -o _parity.o phpmod.d 2>_parity.c
gcc _parity.c $CFLAGS -o _parity && ./_parity


echo "PHP 8.4"
cd "$PHPSOURCE"
git checkout PHP-8.4.5
cd "$PHPMOD"
gdc-14 -c -fpreview=all -fversion=TestParityWithPHP -fversion=PHP84 -o _parity.o phpmod.d 2>_parity.c
gcc _parity.c $CFLAGS -o _parity && ./_parity


echo "PHP 8.5"
cd "$PHPSOURCE"
git checkout master
cd "$PHPMOD"
gdc-14 -c -fpreview=all -fversion=TestParityWithPHP -fversion=PHP85 -o _parity.o phpmod.d 2>_parity.c
gcc _parity.c $CFLAGS -o _parity && ./_parity


rm _parity.o
rm _parity.c
rm _parity

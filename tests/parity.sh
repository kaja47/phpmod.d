# testing parity with PHP source
#
# This script first generates a C file which checks sizes and offsets of
# structs and their fields by comparing values obtained from D implementation
# with results of C sizeof and offsetof expressions.
#
# Then it compiles the C file with PHP header files.
#
# Finally the compiled program is run, reporting any discrepancy.
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
GDC=${GDC:-gdc-15}

CFLAGS="-D_GNU_SOURCE -UNDEBUG -DZEND_SIGNALS -DZEND_ENABLE_STATIC_TSRMLS_CACHE=1"
CFLAGS="$CFLAGS -I$PHPSOURCE -I$PHPSOURCE/main -I$PHPSOURCE/TSRM -I$PHPSOURCE/Zend -I$PHPSOURCE/ext/ffi/";


versions=(83         84         85        86)
branches=(PHP-8.3.30 PHP-8.4.17 PHP-8.5.2 master)

for i in {0..3}; do
  echo
  echo "PHP ${versions[i]}"
  cd "$PHPSOURCE"
  git checkout "${branches[i]}"
  cd "$PHPMOD"
  $GDC -c -fpreview=dip1008 -fversion=TestParityWithPHP -fversion=PHP"${versions[i]}" -o /dev/null phpmod.d 2>_parity.c

  echo "parity run";
  sed '/\/\/---snip-here---/q' _parity.c > _parity_run.c
  gcc _parity_run.c $CFLAGS -o _parity && ./_parity

  echo "parity compile only";
  sed -n '/\/\/---snip-here---/,$p' _parity.c > _parity_compile.c
  gcc -Wall -Wextra -c _parity_compile.c $CFLAGS -o /dev/null
done
echo


rm _parity
rm _parity.c
rm _parity_run.c
rm _parity_compile.c

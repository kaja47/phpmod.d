<?php

require_once 'lib.php';

echo "=== testing unpack\n";

$str = pack("ccc", 1, 2, 3);
test(__LINE__, unpack1("c", $str, 0) === 1);
test(__LINE__, unpack1("c", $str, 1) === 2);
test(__LINE__, unpack1("c", $str, 2) === 3);
$arr = unpack_array('ccc', $str, 0);
test(__LINE__, $arr === [1, 2, 3]);

$str = pack("vvv", 1111, 2222, 3333);
test(__LINE__, unpack1("v", $str, 0) === 1111);
test(__LINE__, unpack1("v", $str, 2) === 2222);
test(__LINE__, unpack1("v", $str, 4) === 3333);
$arr = unpack_array('vvv', $str, 0);
test(__LINE__, $arr === [1111, 2222, 3333]);

$str = pack("PPP", 111111, 222222, 333333);
test(__LINE__, unpack1("P", $str, 0)  === 111111);
test(__LINE__, unpack1("P", $str, 8)  === 222222);
test(__LINE__, unpack1("P", $str, 16) === 333333);
$arr = unpack_array('PPP', $str, 0);
test(__LINE__, $arr === [111111, 222222, 333333]);

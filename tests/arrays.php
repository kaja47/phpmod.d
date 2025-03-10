<?php

require_once 'lib.php';

echo "=== testing arrays\n";

$a = [1, 'a' => 2, 'b' => 3, 4, 5, 10 => 6];
$b = [100 => 7, 1 => 8, 'z' => 9];

$res = array_merge_values($a, $b);
_test($res === [1,2,3,4,5,6,7,8,9]);;

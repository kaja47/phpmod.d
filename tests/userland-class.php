<?php

require_once "lib.php";

class XYZ {
  public int $x = 1, $y = 2, $z = 3;
  public function meth(int $a): int {
    return $a + 2;
  }
}

$c = new XYZ;

_test(readProperties($c));
//_test(callMethod($c));



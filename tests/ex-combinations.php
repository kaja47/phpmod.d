<?php

require_once "lib.php";

// test for leaks

\Turbo\CombinationsHelper::combinations([[1,2,3], [10,20,30], [100,200,300]]);

$arr = [["".rand(), "".rand()], ["".rand(), "".rand()]];
\Turbo\CombinationsHelper::combinations($arr);

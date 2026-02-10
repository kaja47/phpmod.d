<?php

error_reporting(E_ALL);

function test($name, $x) {
  if ($x === true) {
    echo "pass $name\n";
  } else {
    echo "\e[0;31m";
    echo "fail $name\n";
    var_dump($x);
    echo "\e[0m";
  }
}
function _test($x) {
  if ($x === true) {
    echo "pass\n";
  } else {
    echo "\e[0;31m";
    var_dump($x);
    echo "\e[0m";
  }
}
function _testError($f, $errstr) {
  $__error = "";
  set_error_handler(function ($errno, $errstr) use (&$__error) {
    $__error = $errstr;
  });
  $f();
  _test($__error === $errstr);
  restore_error_handler();
}
function _testThrows($f) {
  $throws = false;
  try {
    $f();
  } catch (Throwable $e) {
    $throws = true;
  }
  _test($throws);
}
function _testThrowsMessage($f, string $regex) {
  $throws = false;
  try {
    $f();
  } catch (Throwable $e) {
    $throws = true;
  }
  _test($throws);
  _test(preg_match($regex, $e->getMessage()) === 1);
}

<?php

error_reporting(E_ALL);

function _test($x) {
  if ($x === true) {
    var_dump($x);
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

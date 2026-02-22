<?php

error_reporting(E_ALL);

$passed = $failed = 0;

function test($line, $x) {
  global $passed, $failed;
  if ($x === true) {
    $passed++;
    //echo "passed $line\n";
  } else {
    echo "\e[0;31m";
    echo "failed line $line\n";
    var_dump($x);
    echo "\e[0m";
    $failed++;
  }
}
function summary() {
  global $passed, $failed;
  if ($failed > 0) {
    echo "$passed passed, $failed failed\n";
  }
  $passed = $failed = 0;
}
function testError($line, $f, $errstr) {
  $__error = "";
  set_error_handler(function ($errno, $errstr) use (&$__error) {
    $__error = $errstr;
  });
  $f();
  test($line, $__error === $errstr);
  restore_error_handler();
}
function testThrows($line, $f) {
  $throws = false;
  try {
    $f();
  } catch (Throwable $e) {
    $throws = true;
  }
  test($line, $throws);
}
function testThrowsMessage($line, $f, string $regex) {
  $throws = false;
  try {
    $f();
  } catch (Throwable $e) {
    $throws = true;
  }
  test($line."throws", $throws);
  test($line."message", preg_match($regex, $e->getMessage()) === 1);
}

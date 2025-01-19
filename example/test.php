<?php

/*
gdc-14 -g -shared -fPIC phpmod.d example/test.d -o test.so && php -d extension=./test.so example/test.php
*/

error_reporting(E_ALL);

$__error = "";
function recordError() {
  global $__error;
  $__error = "";
  set_error_handler(function ($errno, $errstr) use (&$__error) {
    $__error = $errstr;
  });
}
function getError() {
  global $__error;
  restore_error_handler();
  return $__error;
}



var_dump(0.0 === func0());
var_dump(1.0 === func1(1));
var_dump(2.0 === func2(1,1));


echo "=== testing func1\n";
$f = new ReflectionFunction("func1");
var_dump($f->getNumberOfParameters() === 1);
var_dump($f->getNumberOfRequiredParameters() === 0);
var_dump((string)$f->getReturnType() === "float");

$p = $f->getParameters()[0];
var_dump($p->allowsNull() === false);
var_dump($p->canBePassedByValue() === true);
var_dump($p->getDefaultValue() === 1);
var_dump((string)$p->getType() === "int");
var_dump($p->hasType() === true);
var_dump($p->isDefaultValueAvailable() === true);
var_dump($p->isDefaultValueConstant() === false);
var_dump($p->isOptional() === true);
var_dump($p->isPassedByReference() === false);
var_dump($p->isVariadic() === false);


echo "=== testing default values\n";
var_dump(6.0 === func3(1,2,3));
var_dump(6.0 === func3(1,2));
var_dump(6.0 === func3(1));
var_dump(6.0 === func3());
var_dump(6.0 === func3(a:1, b:2, c:3));
var_dump(6.0 === func3(c:3, a:1, b:2));
var_dump(6.0 === func3(c:3));
var_dump(6.0 === func3(b:2));
var_dump(6.0 === func3(a:1));


echo "=== testing resource\n";
$r = makeTestResource();
var_dump(get_resource_type($r) === 'TestResource');



test();


echo "=== testing objects\n";

$t = new Test();
var_dump($t->method() === 10);
var_dump($t->method2(1.0, 1, true) === 3);
var_dump($t->selfType($t) === null);

try {
  $t->neex();
} catch (Throwable $e) {
  var_dump($e->getMessage() === "Call to undefined method Test::neex()");
}

recordError();
var_dump($t->neex === null);
var_dump(getError() === 'Undefined property: Test::$neex');

recordError();
$t->neex = 1;
var_dump($t->neex === 1);
var_dump(getError() === 'Creation of dynamic property Test::$neex is deprecated');



$t = new TestWithConstructor(10, 20);
var_dump($t->method() === 30);

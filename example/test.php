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

function _test($x) {
  if ($x === true) {
    var_dump($x);
  } else {
    echo "\e[0;31m";
    var_dump($x);
    echo "\e[0m";
  }
}



_test(0.0 === func0());
_test(1.0 === func1(1));
_test(2.0 === func2(1,1));


echo "=== testing functions\n";
$f = new ReflectionFunction("func1");
_test($f->getNumberOfParameters() === 1);
_test($f->getNumberOfRequiredParameters() === 0);
_test((string)$f->getReturnType() === "float");

$p = $f->getParameters()[0];
_test($p->allowsNull() === false);
_test($p->canBePassedByValue() === true);
_test($p->getDefaultValue() === 1);
_test((string)$p->getType() === "int");
_test($p->hasType() === true);
_test($p->isDefaultValueAvailable() === true);
_test($p->isDefaultValueConstant() === false);
_test($p->isOptional() === true);
_test($p->isPassedByReference() === false);
_test($p->isVariadic() === false);

_test(funcNullable(null) === true);
_test(funcNullableReturnTypeString() === null);
_test(funcNullableReturnTypeArray()  === null);
_test(funcNullableReturnTypeObject() === null);
_test(funcNullableReturnTypeClass()  === null);


echo "=== testing type hints\n";
$f = new ReflectionFunction("funcArgTypehints");
$params = $f->getParameters();
_test((string)$params[0]->getType() === 'string');
_test((string)$params[1]->getType() === 'array');
_test($params[2]->getType() === null); // resources seems not to be type-hinted
_test((string)$params[3]->getType() === 'object');


echo "=== testing nullable type hints\n";
$f = new ReflectionFunction("funcArgTypehintsNullable");
$params = $f->getParameters();
_test((string)$params[0]->getType() === '?string');
_test((string)$params[1]->getType() === '?array');
_test((string)$params[2]->getType() === ''); // resources are weird
_test((string)$params[3]->getType() === '?object');
_test($params[0]->allowsNull() === true);
_test($params[1]->allowsNull() === true);
_test($params[2]->allowsNull() === true);
_test($params[3]->allowsNull() === true);


echo "=== testing type hints for return types\n";
$f = new ReflectionFunction("funcNullableReturnTypeString");
_test((string)$f->getReturnType() === '?string');
$f = new ReflectionFunction("funcNullableReturnTypeArray");
_test((string)$f->getReturnType() === '?array');
$f = new ReflectionFunction("funcNullableReturnTypeObject");
_test((string)$f->getReturnType() === '?object');
$f = new ReflectionFunction("funcNullableReturnTypeClass");
_test((string)$f->getReturnType() === '?Test');


echo "=== testing type hints for classes\n";
$f = new ReflectionFunction("funcArgTypehintsClasses");
$params = $f->getParameters();
_test((string)$params[0]->getType() === 'Test');
_test((string)$params[1]->getType() === '?Test');
_test((string)$params[2]->getType() === 'TestWithConstructor');
_test((string)$params[3]->getType() === '?TestWithConstructor');


echo "=== testing parameter default values\n";
_test(6.0 === func3(1,2,3));
_test(6.0 === func3(1,2));
_test(6.0 === func3(1));
_test(6.0 === func3());
_test(6.0 === func3(a:1, b:2, c:3));
_test(6.0 === func3(c:3, a:1, b:2));
_test(6.0 === func3(c:3));
_test(6.0 === func3(b:2));
_test(6.0 === func3(a:1));


echo "=== testing resource\n";
$r = makeTestResource();
_test(get_resource_type($r) === 'TestResource');


echo "=== testing objects\n";

$r = new ReflectionClass("Test");
_test($r->getConstructor() === null);
_test(count($r->getMethods()) === 3);

$r = new ReflectionClass("TestWithConstructor");
_test($r->getConstructor() !== null);
_test(count($r->getMethods()) === 3); // ctor+dtor+1

$r = new ReflectionClass("TestWithPHPConstructor");
_test($r->getConstructor() !== null);
_test(count($r->getMethods()) === 3); // ctor+dtor+1

$t = new Test();
_test($t->method() === 10);
_test($t->method2(1.0, 1, true) === 3);
_test($t->selfType($t) === null);

try {
  $t->neex();
} catch (Throwable $e) {
  _test($e->getMessage() === "Call to undefined method Test::neex()");
}

recordError();
_test($t->neex === null);
_test(getError() === 'Undefined property: Test::$neex');

recordError();
$t->neex = 1;
_test($t->neex === 1);
_test(getError() === 'Creation of dynamic property Test::$neex is deprecated');



$t = new TestWithConstructor(10, 20);
_test($t->method() === 30);


$r = new ReflectionClass("TestWithPHPConstructor");
_test(count($r->getMethods()) === 3);

_test(new TestWithPHPConstructor(100)->get() === 100);



echo "=== testing arrays\n";
$arr = [];
for ($i = 0; $i < 5; $i++) {
  $arr[] = rand();
}

_test(testPackedArray($arr));

unset($arr[0]);
_test(testPackedArrayWithHoles($arr));


$arr['x'] = 0;
unset($arr['x']);
_test(testHashArray($arr));
die;



echo "=== testing exceptions\n";
$_thrown = false;
try {
  throwException1();
} catch (Exception $e) {
  _test(str_starts_with($e->getMessage(), 'exception ('));
  $_thrown = true;
} finally {
  if (!$_thrown) {
    _test(false);
  }
}



echo "=== testing refcounts and memory management\n";
function createStringPHP() {
  return (string)rand();
}
_test(rc(createStringPHP()) === rc(createStringNative()));
$a = createStringPHP();
$b = createStringNative();
_test(rc($a) === rc($b));


$arr = null;
$n = memory_get_usage();
$arr = [];
for ($i = 1; $i <= 10; $i++) {
  $arr[] = new C0($i);
}
$arr = null;
_test($n === memory_get_usage());


$n = memory_get_usage();
$arr = [];
for ($i = 1; $i <= 10; $i++) {
  $arr[] = new BigClass($i);
}
$arr = null;
_test($n === memory_get_usage());



//test();

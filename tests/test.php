<?php


require_once "lib.php";



class UserspaceClass {
  public int $a, $b, $c, $d;
  function getInt()    { return 1; }
  function getDouble() { return 1.0; }
  function getArray()  { return []; }
  function getSelf()   { return $this; }
}


_test(testBasics());


echo "=== testing constants\n";

_test(ENUM_CONST === 1);
_test(IMMUTABLE_CONST === 2);
_test(FUNC_CONST === 3);

_test(ClassWithConstants::BOOL_C   === true);
_test(ClassWithConstants::INT_C    === 1);
_test(ClassWithConstants::DOUBLE_C === 1.0);
_test(ClassWithConstants::STR_C    === "1");


echo "=== testing functions\n";

_test(0.0 === func0());
_test(1.0 === func1(1));
_test(2.0 === func2(1,1));

_test(funcReturnVoid() === null);
$f = new ReflectionFunction("funcReturnVoid");
_test($f->getReturnType() === null);


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

$f = new ReflectionFunction("func3");
_test($f->getParameters()[0]->name === 'a');
_test($f->getParameters()[1]->name === 'b');
_test($f->getParameters()[2]->name === 'c');

$f = new ReflectionFunction("funcArgNoName");
_test($f->getParameters()[0]->name === 'arg1');
_test($f->getParameters()[1]->name === 'arg2');
_test($f->getParameters()[2]->name === 'arg3');

$f = new ReflectionFunction("funcMixed");
_test($f->getParameters()[0]->getType() === null);
_test($f->getParameters()[1]->getType() === null);
_test($f->getReturnType() === null);

$f = new ReflectionFunction("funcReturnLongOrBool");
_test((string)$f->getReturnType() === "int|bool");

$f = new ReflectionFunction("funcArgLongOrBool");
_test((string)$f->getParameters()[0]->getType() === "int|bool");
_testThrows(fn() => funcArgLongOrBool());
_testThrows(fn() => funcArgLongOrBool(""));
_testThrows(fn() => funcArgLongOrBool([]));
_testThrows(fn() => funcArgLongOrBool(null));
_test(funcArgLongOrBool(1) === 1);
_test(funcArgLongOrBool(true) === true);

_test(funcNullable(null) === true);
_test(funcNullableReturnTypeString() === null);
_test(funcNullableReturnTypeArray()  === null);
_test(funcNullableReturnTypeObject() === null);
_test(funcNullableReturnTypeClass()  === null);

_test(testAcceptObject(new stdClass));
_test(testAcceptObject(new UserspaceClass));
_test(testAcceptObject(new Test));



echo "=== testing variadic functions\n";

_test(funcVariadic0()      === 0);
_test(funcVariadic0(1)     === 1);
_test(funcVariadic0(1,2)   === 2);
_test(funcVariadic0(1,2,3) === 3);
$f = new ReflectionFunction("funcVariadic0");
_test($f->getParameters()[0]->isVariadic() === true);
_test($f->getParameters()[0]->getType() === null);

$f = new ReflectionFunction("funcVariadicUbyte");
_test($f->getParameters()[0]->isVariadic() === true);
_test($f->getParameters()[0]->getType()->getName() === 'int');

_test(funcVariadic1(false)        === 0);
_test(funcVariadic1(false, 1)     === 1);
_test(funcVariadic1(false, 1,2)   === 2);
_test(funcVariadic1(false, 1,2,3) === 3);

_test(funcVariadicLong() === 0);
_test(funcVariadicLong(1,2,3) === 6);
_test(funcVariadicInt() === 0);
_test(funcVariadicInt(1,2,3) === 6);
_testThrowsMessage(fn() => funcVariadicInt(1, 2, ""), '/Argument #3 expected int, string given/');
_testThrowsMessage(fn() => funcVariadicInt(1,2, 1<<40), '/out of range/');
_test(funcVariadicShort(1,2,3) === 6);
_test(funcVariadicUbyte(1,2,3) === 6);
_test(funcVariadicBool() === 0);
_test(funcVariadicBool(true, false, true, false) === 2);
_test(funcVariadicFloat(1.0, 2.0, 3, 4) === 10.0);
_test(funcVariadicString("a", "b", "c") === 3);
_test(funcVariadicXY(new XY0(1,1), new XY0(2,2), new XY0(3,3)) === 12);

// too much arguments
_testThrows(function () {
  test0(1);
});
_testThrows(function () {
  new C0(1,2,3,4,5);
});



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

_test(funcArgTypehintsNullable(null, null, null, null) === null);


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

$f = new ReflectionFunction("funcArgAutoClass");
_test((string)$f->getParameters()[0]->getType() === 'NativeStruct');


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
test(1, $r->getConstructor() === null);
test(2, count($r->getMethods()) === 7);

$r = new ReflectionClass("TestWithConstructor");
test(3, $r->getConstructor() !== null);
test(4, count($r->getMethods()) === 2); // ctor+1

$r = new ReflectionClass("TestWithPHPConstructor");
test(5, $r->getConstructor() !== null);
test(6, count($r->getMethods()) === 2); // ctor+1

$t = new Test();
test( 7, $t->method() === 10);
test( 8, $t->method2(1.0, 1, true) === 3);
test( 9, $t->selfType($t) === null);
test(10, $t->methodVariadic() === 0);
test(11, $t->methodVariadic(1,2.0,"three") === 3);
test(12, $t->methodVariadic2(0, 1,2.0,"three") === 3);
test(13, $t->methodDefaultArgument() === 1337);
test(14, $t->methodNullableArgument(null) === true);

try {
  $t->neex();
} catch (Throwable $e) {
  _test($e->getMessage() === "Call to undefined method Test::neex()");
}

_testError(function () use ($t) {
  _test($t->neex === null);
}, 'Undefined property: Test::$neex');

_testError(function () use ($t) {
$t->neex = 1;
_test($t->neex === 1);
}, 'Creation of dynamic property Test::$neex is deprecated');


$t = new TestWithConstructor(10, 20);
test('c', $t->method() === 30);


$r = new ReflectionClass("TestWithPHPConstructor");
_test(count($r->getMethods()) === 2); // ctor+1

_test(new TestWithPHPConstructor(100)->get() === 100);


_test(new NativeStruct(1,2,3)->x() === 1);
_test(new NativeStructRenamed(1,2,3) !== null);



class ABC {
  public int $a = 1;
  function fff(int $a) { return $a + 1; }
}
$a = new ABC();
_test(testReadFieldsOfUserspaceObjects($a) === 1);
_test(testReadFieldsOfUserspaceObjects((object)['a' => 1]) === 1);
//_testError(testReadFieldsOfUserspaceObjects(new stdClass));


echo "=== testing objects methods\n";

class ClassWithMethods {
  function noArgs() { return 1; }
  function oneArg(int $x) { return $x+1; }
  function throws() { throw new Exception("bad"); }
}

$obj = new ClassWithMethods;
_test(funcCallNoArgsMethod($obj) === 1);
_test(funcCallNoArgsMethodTyped($obj) === 1);



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


$arr = [1 => '', 'x' => ''];
$packed = [1,2];
_test(testArrayTypedMixedKeys($arr, $packed));

_testThrows(fn() => testArrayIterationIntValues([1.0]));



echo "=== testing exceptions\n";
_testThrowsMessage(function() {
  throwException1();
}, '/^exception \(/');

_testThrowsMessage(function() {
  returnExceptionVoid();
}, "/returnedException/");

_testThrowsMessage(function() {
  returnExceptionInt();
}, "/returnedException/");

_test(returnSuccessInt() === 1);

$f = new ReflectionFunction("returnExceptionVoid");
_test($f->getReturnType() === null);
$f = new ReflectionFunction("returnExceptionInt");
_test((string)$f->getReturnType() === "int");




echo "=== testing refcounts and memory management\n";
function createStringPHP() {
  return (string)rand();
}
_test(rc(createStringPHP()) === rc(createStringNative()));
$a = createStringPHP();
$b = createStringNative();
_test(rc($a) === rc($b));

function passString(string $a) { return $a; }
$a = '_'.$argv[0];
$b = '_'.$argv[0];
_test(rc(passString($a)) === rc(passStringNative($b)));

$a = 'x'.$argv[0];
testAutozvals($a); // should not leak in debug mode
testAutozvals([$argv[0]]);



$arr = null;
$n = memory_get_usage();
$arr = [];
for ($i = 1; $i <= 10; $i++) {
  $arr[] = new C0();
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

<?php


require_once "lib.php";



class UserspaceClass {
  public int $a, $b, $c, $d;
  function getInt()    { return 1; }
  function getDouble() { return 1.0; }
  function getArray()  { return []; }
  function getSelf()   { return $this; }
}


test(__LINE__, testBasics());
summary();


echo "=== testing constants\n";

test(__LINE__, ENUM_CONST === 1);
test(__LINE__, IMMUTABLE_CONST === 2);
test(__LINE__, FUNC_CONST === 3);
test(__LINE__, NS\NS_CONST === "NS");

test(__LINE__, ClassWithConstants::BOOL_C   === true);
test(__LINE__, ClassWithConstants::INT_C    === 1);
test(__LINE__, ClassWithConstants::DOUBLE_C === 1.0);
test(__LINE__, ClassWithConstants::STR_C    === "1");
summary();


echo "=== testing functions\n";

test(__LINE__, 0.0 === func0());
test(__LINE__, 1.0 === func1(1));
test(__LINE__, 2.0 === func2(1,1));
test(__LINE__, \NS\nsFunc() === 1);

test(__LINE__, funcReturnVoid() === null);
$f = new ReflectionFunction("funcReturnVoid");
test(__LINE__, $f->getReturnType() === null);


$f = new ReflectionFunction("func1");
test(__LINE__, $f->getNumberOfParameters() === 1);
test(__LINE__, $f->getNumberOfRequiredParameters() === 0);
test(__LINE__, (string)$f->getReturnType() === "float");

$p = $f->getParameters()[0];
test(__LINE__, $p->allowsNull() === false);
test(__LINE__, $p->canBePassedByValue() === true);
test(__LINE__, $p->getDefaultValue() === 1);
test(__LINE__, (string)$p->getType() === "int");
test(__LINE__, $p->hasType() === true);
test(__LINE__, $p->isDefaultValueAvailable() === true);
test(__LINE__, $p->isDefaultValueConstant() === false);
test(__LINE__, $p->isOptional() === true);
test(__LINE__, $p->isPassedByReference() === false);
test(__LINE__, $p->isVariadic() === false);

$f = new ReflectionFunction("func3");
test(__LINE__, $f->getParameters()[0]->name === 'a');
test(__LINE__, $f->getParameters()[1]->name === 'b');
test(__LINE__, $f->getParameters()[2]->name === 'c');

$f = new ReflectionFunction("funcArgNoName");
test(__LINE__, $f->getParameters()[0]->name === 'arg1');
test(__LINE__, $f->getParameters()[1]->name === 'arg2');
test(__LINE__, $f->getParameters()[2]->name === 'arg3');

$f = new ReflectionFunction("funcMixed");
test(__LINE__, $f->getParameters()[0]->getType() === null);
test(__LINE__, $f->getParameters()[1]->getType() === null);
test(__LINE__, $f->getReturnType() === null);

$f = new ReflectionFunction("funcReturnLongOrBool");
test(__LINE__, (string)$f->getReturnType() === "int|bool");

$f = new ReflectionFunction("funcArgLongOrBool");
test(__LINE__, (string)$f->getParameters()[0]->getType() === "int|bool");
testThrows(72, fn() => funcArgLongOrBool());
testThrows(73, fn() => funcArgLongOrBool(""));
testThrows(74, fn() => funcArgLongOrBool([]));
testThrows(75, fn() => funcArgLongOrBool(null));
test(__LINE__, funcArgLongOrBool(1) === 1);
test(__LINE__, funcArgLongOrBool(true) === true);

test(__LINE__, funcNullable(null) === true);
test(__LINE__, funcNullableReturnTypeString() === null);
test(__LINE__, funcNullableReturnTypeArray()  === null);
test(__LINE__, funcNullableReturnTypeObject() === null);
test(__LINE__, funcNullableReturnTypeClass()  === null);

test(__LINE__, testAcceptObject(new stdClass));
test(__LINE__, testAcceptObject(new UserspaceClass));
test(__LINE__, testAcceptObject(new Test));
summary();



echo "=== testing variadic functions\n";

test(__LINE__, funcVariadic0()      === 0);
test(__LINE__, funcVariadic0(1)     === 1);
test(__LINE__, funcVariadic0(1,2)   === 2);
test(__LINE__, funcVariadic0(1,2,3) === 3);
$f = new ReflectionFunction("funcVariadic0");
test(__LINE__, $f->getParameters()[0]->isVariadic() === true);
test(__LINE__, $f->getParameters()[0]->getType() === null);

$f = new ReflectionFunction("funcVariadicUbyte");
test(__LINE__, $f->getParameters()[0]->isVariadic() === true);
test(__LINE__, $f->getParameters()[0]->getType()->getName() === 'int');

test(__LINE__, funcVariadic1(false)        === 0);
test(__LINE__, funcVariadic1(false, 1)     === 1);
test(__LINE__, funcVariadic1(false, 1,2)   === 2);
test(__LINE__, funcVariadic1(false, 1,2,3) === 3);

test(__LINE__, funcVariadicLong() === 0);
test(__LINE__, funcVariadicLong(1,2,3) === 6);
test(__LINE__, funcVariadicInt() === 0);
test(__LINE__, funcVariadicInt(1,2,3) === 6);
testThrowsMessage(134, fn() => funcVariadicInt(1, 2, ""), '/Argument #3 expected int, string given/');
testThrowsMessage(135, fn() => funcVariadicInt(1,2, 1<<40), '/out of range/');
test(__LINE__, funcVariadicShort(1,2,3) === 6);
test(__LINE__, funcVariadicUbyte(1,2,3) === 6);
test(__LINE__, funcVariadicBool() === 0);
test(__LINE__, funcVariadicBool(true, false, true, false) === 2);
test(__LINE__, funcVariadicFloat(1.0, 2.0, 3, 4) === 10.0);
test(__LINE__, funcVariadicString("a", "b", "c") === 3);
test(__LINE__, funcVariadicXY(new XY0(1,1), new XY0(2,2), new XY0(3,3)) === 12);

// too much arguments
testThrows(150, function () {
  test0(1);
});
testThrows(151, function () {
  new C0(1,2,3,4,5);
});
summary();



echo "=== testing type hints\n";
$f = new ReflectionFunction("funcArgTypehints");
$params = $f->getParameters();
test(__LINE__, (string)$params[0]->getType() === 'string');
test(__LINE__, (string)$params[1]->getType() === 'array');
test(__LINE__, $params[2]->getType() === null); // resources seems not to be type-hinted
test(__LINE__, (string)$params[3]->getType() === 'object');
summary();


echo "=== testing nullable type hints\n";
$f = new ReflectionFunction("funcArgTypehintsNullable");
$params = $f->getParameters();
test(__LINE__, (string)$params[0]->getType() === '?string');
test(__LINE__, (string)$params[1]->getType() === '?array');
test(__LINE__, (string)$params[2]->getType() === ''); // resources are weird
test(__LINE__, (string)$params[3]->getType() === '?object');
test(__LINE__, $params[0]->allowsNull() === true);
test(__LINE__, $params[1]->allowsNull() === true);
test(__LINE__, $params[2]->allowsNull() === true);
test(__LINE__, $params[3]->allowsNull() === true);

test(__LINE__, funcArgTypehintsNullable(null, null, null, null) === null);
summary();


echo "=== testing type hints for return types\n";
$f = new ReflectionFunction("funcNullableReturnTypeString");
test(__LINE__, (string)$f->getReturnType() === '?string');
$f = new ReflectionFunction("funcNullableReturnTypeArray");
test(__LINE__, (string)$f->getReturnType() === '?array');
$f = new ReflectionFunction("funcNullableReturnTypeObject");
test(__LINE__, (string)$f->getReturnType() === '?object');
$f = new ReflectionFunction("funcNullableReturnTypeClass");
test(__LINE__, (string)$f->getReturnType() === '?Test');
summary();


echo "=== testing type hints for classes\n";
$f = new ReflectionFunction("funcArgTypehintsClasses");
$params = $f->getParameters();
test(__LINE__, (string)$params[0]->getType() === 'Test');
test(__LINE__, (string)$params[1]->getType() === '?Test');
test(__LINE__, (string)$params[2]->getType() === 'TestWithConstructor');
test(__LINE__, (string)$params[3]->getType() === '?TestWithConstructor');
summary();


echo "=== testing parameter default values\n";
test(__LINE__, 6.0 === func3(1,2,3));
test(__LINE__, 6.0 === func3(1,2));
test(__LINE__, 6.0 === func3(1));
test(__LINE__, 6.0 === func3());
test(__LINE__, 6.0 === func3(a:1, b:2, c:3));
test(__LINE__, 6.0 === func3(c:3, a:1, b:2));
test(__LINE__, 6.0 === func3(c:3));
test(__LINE__, 6.0 === func3(b:2));
test(__LINE__, 6.0 === func3(a:1));
summary();


echo "=== testing resource\n";
$r = makeTestResource();
test(__LINE__, get_resource_type($r) === 'TestResource');
summary();


echo "=== testing objects\n";

$r = new ReflectionClass("Test");
test(__LINE__, $r->getConstructor() === null);
test(__LINE__, count($r->getMethods()) === 7);

$r = new ReflectionClass("TestWithConstructor");
test(__LINE__, $r->getConstructor() !== null);
test(__LINE__, count($r->getMethods()) === 2); // ctor+1

$r = new ReflectionClass("TestWithPHPConstructor");
test(__LINE__, $r->getConstructor() !== null);
test(__LINE__, count($r->getMethods()) === 2); // ctor+1

$t = new Test();
test(__LINE__, $t->method() === 10);
test(__LINE__, $t->method2(1.0, 1, true) === 3);
test(__LINE__, $t->selfType($t) === null);
test(__LINE__, $t->methodVariadic() === 0);
test(__LINE__, $t->methodVariadic(1,2.0,"three") === 3);
test(__LINE__, $t->methodVariadic2(0, 1,2.0,"three") === 3);
test(__LINE__, $t->methodDefaultArgument() === 1337);
test(__LINE__, $t->methodNullableArgument(null) === true);

try {
  $t->neex();
} catch (Throwable $e) {
  test(__LINE__, $e->getMessage() === "Call to undefined method Test::neex()");
}

testError(241, function () use ($t) {
  test(__LINE__, $t->neex === null);
}, 'Undefined property: Test::$neex');

testError(243, function () use ($t) {
$t->neex = 1;
test(__LINE__, $t->neex === 1);
}, 'Creation of dynamic property Test::$neex is deprecated');


$t = new TestWithConstructor(10, 20);
test(__LINE__, $t->method() === 30);

$r = new ReflectionClass("TestWithPHPConstructor");
test(__LINE__, count($r->getMethods()) === 2); // ctor+1

$obj = new TestWithPHPConstructor(100);
test(__LINE__, $obj->get() === 100);

class ABC {
  public int $a = 1;
  function fff(int $a) { return $a + 1; }
}
$a = new ABC();
test(__LINE__, testReadFieldsOfUserspaceObjects($a) === 1);
test(__LINE__, testReadFieldsOfUserspaceObjects((object)['a' => 1]) === 1);
//testError(testReadFieldsOfUserspaceObjects(new stdClass));
summary();



echo "=== testing default parameters on constructors and methods\n";

$r = new ReflectionClass("TestConstructorWithDefaultParams");
$c = $r->getConstructor();
test(__LINE__, $c->getParameters()[1]->getDefaultValue() === 1);
test(__LINE__, $c->getParameters()[2]->getDefaultValue() === true);
test(__LINE__, $c->getParameters()[3]->getDefaultValue() === 3.0);
test(__LINE__, $c->getParameters()[4]->getDefaultValue() === "asd");
$o = new TestConstructorWithDefaultParams(0);
test(__LINE__, $o->get() === [1, true, 3.0, "asd"]);
$o = new TestConstructorWithDefaultParams(0, d: 0.1);
test(__LINE__, $o->get() === [1, true, 0.1, "asd"]);
$o->set(10, false, 1.0, "XXX");
$o->set();
test(__LINE__, $o->get() === [1, true, 3.0, "asd"]);

$r = new ReflectionClass("TestConstructorWithDefaultParams");
$m = $r->getMethod("set");
test(__LINE__, $m->getParameters()[0]->getDefaultValue() === 1);
test(__LINE__, $m->getParameters()[1]->getDefaultValue() === true);
test(__LINE__, $m->getParameters()[2]->getDefaultValue() === 3.0);
test(__LINE__, $m->getParameters()[3]->getDefaultValue() === "asd");

$r = new ReflectionClass("TestPHPConstructorWithDefaultParams");
$c = $r->getConstructor();
test(__LINE__, $c->getParameters()[0]->getDefaultValue() === 1);
test(__LINE__, $c->getParameters()[1]->getDefaultValue() === true);
test(__LINE__, $c->getParameters()[2]->getDefaultValue() === 3.0);
test(__LINE__, $c->getParameters()[3]->getDefaultValue() === "asd");
$o = new TestPHPConstructorWithDefaultParams();
test(__LINE__, $o->get() === [1, true, 3.0, "asd"]);
$o = new TestPHPConstructorWithDefaultParams(d: 0.1);
test(__LINE__, $o->get() === [1, true, 0.1, "asd"]);



echo "=== testing objects methods\n";

class ClassWithMethods {
  function noArgs() { return 1; }
  function oneArg(int $x) { return $x+1; }
  function throws($x) { throw new Exception("thrown_from_userpace".$x); }
}

$obj = new ClassWithMethods;
test(__LINE__, funcCallNoArgsMethod($obj) === 1);
test(__LINE__, funcCallNoArgsMethodTyped($obj) === 1);
testThrowsMessage(__LINE__, fn() => funcCallThrowsMethod($obj), '/thrown_from_userpace/');
summary();





echo "=== testing arrays\n";
$arr = [];
for ($i = 0; $i < 5; $i++) {
  $arr[] = rand();
}

test(__LINE__, testPackedArray($arr));

unset($arr[0]);
test(__LINE__, testPackedArrayWithHoles($arr));

$arr['x'] = 0;
unset($arr['x']);
test(__LINE__, testHashArray($arr));


$arr = [1 => '', 'x' => ''];
$packed = [1,2];
test(__LINE__, testArrayTypedMixedKeys($arr, $packed));

testThrows(334, fn() => testArrayIterationIntValues([1.0]));
summary();



echo "=== testing exceptions\n";
testThrowsMessage(340, function() {
  throwException1();
}, '/^exception \(/');

testThrowsMessage(341, function() {
  returnExceptionVoid();
}, "/returnedException/");

testThrowsMessage(342, function() {
  returnExceptionInt();
}, "/returnedException/");

test(__LINE__, returnSuccessInt() === 1);

$f = new ReflectionFunction("returnExceptionVoid");
test(__LINE__, $f->getReturnType() === null);
$f = new ReflectionFunction("returnExceptionInt");
test(__LINE__, (string)$f->getReturnType() === "int");
summary();




echo "=== testing refcounts and memory management\n";
function createStringPHP() {
  return (string)rand();
}
test(__LINE__, rc(createStringPHP()) === rc(createStringNative()));
$a = createStringPHP();
$b = createStringNative();
test(__LINE__, rc($a) === rc($b));

function passString(string $a) { return $a; }
$a = '_'.$argv[0];
$b = '_'.$argv[0];
test(__LINE__, rc(passString($a)) === rc(passStringNative($b)));

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
test(__LINE__, $n === memory_get_usage());


$n = memory_get_usage();
$arr = [];
for ($i = 1; $i <= 10; $i++) {
  $arr[] = new BigClass($i);
}
$arr = null;
test(__LINE__, $n === memory_get_usage());
summary();

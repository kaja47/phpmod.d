<?php

require_once "lib.php";


$r = new ReflectionClass("NativeCtor");
_test($r->getConstructor() !== null);
_test(count($r->getMethods()) === 2);
_test(new NativeCtor(1,2)->get() === 3);

$r = new ReflectionClass("PHPCtor");
_test($r->getConstructor() !== null);
_test(count($r->getMethods()) === 2);
_test(new NativeCtor(1,2)->get() === 3);

$r = new ReflectionClass("ClassNativeCtor");
_test($r->getConstructor() !== null);
_test(count($r->getMethods()) === 2);
_test(new NativeCtor(1,2)->get() === 3);

$r = new ReflectionClass("ClassPHPCtor");
_test($r->getConstructor() !== null);
_test(count($r->getMethods()) === 2);
_test(new NativeCtor(1,2)->get() === 3);

$r = new ReflectionClass("StaticMethod");
_test(count($r->getMethods()) === 1);
_test($r->getMethod("get") !== null);
_test($r->getMethod("get")->isStatic() === true);
_test(StaticMethod::get() === 1337);

<?php

require_once "lib.php";


$r = new ReflectionClass("NativeCtor");
test(__LINE__, $r->inNamespace() === false);
test(__LINE__, $r->getConstructor() !== null);
test(__LINE__, count($r->getMethods()) === 2);
$obj = new NativeCtor(1,2);
test(__LINE__, $obj->get() === 3);

$r = new ReflectionClass("PHPCtor");
test(__LINE__, $r->inNamespace() === false);
test(__LINE__, $r->getConstructor() !== null);
test(__LINE__, count($r->getMethods()) === 2);
$obj = new NativeCtor(1,2);
test(__LINE__, $obj->get() === 3);

$r = new ReflectionClass("StaticMethod");
test(__LINE__, $r->inNamespace() === false);
test(__LINE__, count($r->getMethods()) === 1);
test(__LINE__, $r->getMethod("get") !== null);
test(__LINE__, $r->getMethod("get")->isStatic() === true);
test(__LINE__, StaticMethod::get() === 1337);

$r = new ReflectionClass("NS\\NamespacedClass");
test(__LINE__, $r->inNamespace() === true);
test(__LINE__, $r->getNamespaceName() === 'NS');

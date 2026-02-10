<?php

require_once 'lib.php';

echo "=== testing simple\n";

_test(function_exists('popcnt'));
_test(function_exists('lzcnt'));
_test(function_exists('tzcnt'));
_test(function_exists('pdep'));
_test(function_exists('pext'));
_test(!function_exists('notExposed'));

_test(class_exists('ClassA'));
_test(class_exists('ClassB'));
_test(!class_exists('NotClass'));
_test(class_exists('TypeN'));

_test(get_resource_type(makeResource()) === "ResX");

_test(ENUM_CONST === 1);
_test(FUNC_CONST === 2);

_test(isset(get_defined_constants()['NOT_CONST_PRIVATE']) === false);
_test(isset(get_defined_constants()['NOT_CONST_IMMUTABLE']) === false);
_test(isset(get_defined_constants()['notExposed']) === false);

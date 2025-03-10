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

_test(get_resource_type(makeResource()) === "ResX");

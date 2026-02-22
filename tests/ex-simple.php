<?php

require_once 'lib.php';

echo "=== testing simple\n";

test(__LINE__, function_exists('popcnt'));
test(__LINE__, function_exists('lzcnt'));
test(__LINE__, function_exists('tzcnt'));
test(__LINE__, function_exists('pdep'));
test(__LINE__, function_exists('pext'));
test(__LINE__, !function_exists('notExposed'));

test(__LINE__, class_exists('ClassA'));
test(__LINE__, class_exists('ClassB'));
test(__LINE__, !class_exists('NotClass'));

test(__LINE__, get_resource_type(makeResource()) === "ResX");

test(__LINE__, ENUM_CONST === 1);
test(__LINE__, FUNC_CONST === 2);

test(__LINE__, isset(get_defined_constants()['NOT_CONST_PRIVATE']) === false);
test(__LINE__, isset(get_defined_constants()['NOT_CONST_IMMUTABLE']) === false);
test(__LINE__, isset(get_defined_constants()['notExposed']) === false);

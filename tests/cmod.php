<?php

require_once 'lib.php';

test(__LINE__, function_exists('c_function_alias'));
test(__LINE__, function_exists('c_function_extern'));

$r = new ReflectionFunction('c_string_arg');
test(__LINE__, $r->getNumberOfParameters() === 1);

<?php

require_once 'lib.php';

_test(function_exists('c_function_alias'));
_test(function_exists('c_function_extern'));

$r = new ReflectionFunction('c_string_arg');
_test($r->getNumberOfParameters() === 1);

<?php

require_once "lib.php";

$bloom = new Bloom(1000, 0.01);

for ($i = 0; $i < 100; $i++) {
  $bloom->add($i);
  $bloom->add(str_repeat('x', $i));
}

$hits = 0;
for ($i = 0; $i < 100; $i++) {
  $hits += $bloom->contains($i);
  $hits += $bloom->contains(str_repeat('x', $i));
}

test(__LINE__, $hits === 200);

$hits = 0;
for ($i = 0; $i < 100; $i++) {
  $hits += $bloom->contains(1000 + $i);
  $hits += $bloom->contains(str_repeat('y', $i));
}
test(__LINE__, $hits < 3);

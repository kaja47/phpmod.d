<?php

require_once "lib.php";

$ffi = FFI::cdef("
  struct Blob {
    int32_t id;
    int32_t len;
    int16_t w;
    int16_t h;
    int32_t size;
  };

  struct XY { int32_t x, y; };
");

$blobs = $ffi->new('struct Blob[10]');
foreach ($blobs as $i => $m) {
  $m->id   = $i * 1;
  $m->len  = $i * 2;
  $m->w    = $i * 3;
  $m->h    = $i * 4;
  $m->size = $i * 5;
}

$xy = $ffi->new('struct XY');

_test(consumeBlob($blobs[1]) == 1);
_test(consumeBlobArray($blobs, 1, 3) === 1 + 2 + 3);

_testThrows(fn() => consumeBlob($xy));
_testThrows(fn() => consumeBlobArray($blobs[0], 0, 1));

$f = new ReflectionFunction("consumeBlob");
_test((string)$f->getParameters()[0]->getType() === "FFI\\CData");

$f = new ReflectionFunction("consumeBlobArray");
_test((string)$f->getParameters()[0]->getType() === "FFI\\CData");

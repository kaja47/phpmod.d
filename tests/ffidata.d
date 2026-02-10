import phpmod;

mixin mod!ffidata;

@FFI struct Blob {
  int id;
  int len;
  short w;
  short h;
  int size;
}

@FFI struct XY { int x, y; }

long consumeBlob(Blob* arg) {
  return arg.id;
}

long consumeBlobArray(Blob[] arg, int start, int length) {
  long sum;
  foreach (i; start .. start + length)
    sum += arg[i].id;
  return sum;
}

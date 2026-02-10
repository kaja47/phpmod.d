import phpmod;
//@nogc:
nothrow:

mixin mod!noruntime;

void f0(int a, bool b, double c, ZendObject* d, HashTable* e, ZendString* f) {}

Try!int f1() {
  return failure!int("xxx");
}

ZendString* f2() {
  return ZendString.copy("xxx");
}

void f3(HashTable* ht) {
  foreach (k; ht.byKey) {}
  foreach (v; ht.byValue) {}
  foreach (kv; ht.byKeyValue) {}
}

void f4(HashTable* ht) {
  foreach (Try!long v; ht.typed) {}
  foreach (Try!bool v; ht.typed) {}
  foreach (Try!(TypedZval!(bool, long)) v; ht.typed) {}
  foreach (zval v; ht.typed) {}
  foreach (zval* v; ht.typed) {}

  foreach (zval k, zval v; ht.typed) {}
  foreach (Key k, Try!long v; ht.typed) {}
  foreach (Try!long k, zval v; ht.typed) {}
}

@phpClass struct ClassX {
  int a;
  ZendObject std;
}

@phpResource struct ResourceY {
  int a;
}

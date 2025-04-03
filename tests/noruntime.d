import phpmod;
@nogc nothrow:

mixin mod!noruntime;

void f0(int a, bool b, double c, ZendObject* d, HashTable* e, ZendString* f) {}

Try!int f1() {
  return failure!int("xxx");
}

ZendString* f2() {
  return ZendString.copy("xxx");
}

void f3(HashTable* ht) {
  foreach (kv; ht.byKeyValue) {}
}

void f4(ClassX* a, ResourceY* b) {}

@phpClass struct ClassX {
  int a;
  ZendObject std;
}

@phpResource struct ResourceY {
  int a;
}

import phpmod;

mixin mod!arrays;
@nogc:

auto testSeparate() {
  // should not leak in debug mode
  {
    auto a = HashTable.of(1, 2, 3);
    auto b = a.separate();
    assert(a is b);
    assert(a.gc.refcount == 1);
    release(a);
  }
  {
    auto a = HashTable.of(1, 2, 3);
    bump(a);
    assert(a.gc.refcount == 2);

    auto b = a.separate();
    assert(a.gc.refcount == 1);
    assert(b.gc.refcount == 1);
    release(a);
    release(b);
  }
}

import phpmod;

mixin mod!strings;
@nogc:

auto testSeparate() {
  // should not leak in debug mode
  {
    auto s = String.copy("str1");
    auto t = s.separate();
    assert(s is t);
    assert(t.gc.refcount == 1);
    release(t);
  }
  {
    auto s = String.copy("str2");
    bump(s);
    assert(s.gc.refcount == 2);

    auto t = s.separate();
    assert(s.gc.refcount == 1);
    assert(t.gc.refcount == 1);
    release(s);
    release(t);
  }
  {
    auto s = zend_string_init_interned("xyz012", 6, false);
    auto t = s.separate();
    assert(s.isInterned);
    assert(t.gc.refcount == 1);
    release(t);
  }
  {
    auto s = String.staticString!"staticString";
  }

}

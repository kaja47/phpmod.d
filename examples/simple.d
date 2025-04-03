import phpmod;

mixin mod!(simple, "bitops.mod");

private import core.bitop;
private import gcc.builtins;
pragma(inline, true):

auto popcnt(long x)       { return core.bitop.popcnt(x); }
auto lzcnt(long x)        { return __builtin_ia32_lzcnt_u64(x);; }
auto tzcnt(long x)        { return __builtin_ia32_tzcnt_u64(x); }
auto pdep(long x, long y) { return __builtin_ia32_pdep_di(x, y); }
auto pext(long x, long y) { return __builtin_ia32_pext_di(x, y); }

private auto notExposed() => 1;



@phpClass
struct ClassA {
  int a, b, c, d;
  ZendObject std;
}

@phpClass
struct ClassB {
  int a, b, c, d;
  ZendObject std;
}

struct NotClass {
  int a;
}

@phpResource
struct ResX {
  int a, b, c, d;
  // destructor is needed only when some dynamically allocated data need to be
  // freed
  ~this() nothrow @nogc {}
}

ResX* makeResource() {
  return emalloc!ResX;
}

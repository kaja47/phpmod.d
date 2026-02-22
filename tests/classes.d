import phpmod;

mixin mod!classes;
@nogc:


@phpClass struct NativeCtor {
  private int a, b;
  ZendObject obj;
  this(int a, int b) { this.a = a; this.b = b; }
  int get() => a + b;
}

@phpClass struct PHPCtor {
  private int a, b;
  ZendObject obj;
  void __construct(int a, int b) { this.a = a; this.b = b; }
  int get() => a + b;
}

@phpClass struct StaticMethod {
  ZendObject obj;
  static int get() => 1337;
}


@phpClass @namespace("NS")
struct NamespacedClass {
  ZendObject obj;
  static int get() => 1337;
}

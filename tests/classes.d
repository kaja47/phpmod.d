import phpmod;

mixin mod!classes;


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

alias ClassCtor1 = Class!ClassNativeCtor;
struct ClassNativeCtor {
  private int a, b;
  this(int a, int b) { this.a = a; this.b = b; }
  int get() => a + b;
}

alias ClassCtor2 = Class!ClassPHPCtor;
struct ClassPHPCtor {
  private int a, b;
  void __construct(int a, int b) { this.a = a; this.b = b; }
  int get() => a + b;
}

@phpClass struct StaticMethod {
  ZendObject obj;
  static int get() => 1337;
}

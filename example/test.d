import phpmod;
import std.stdio;

ModuleEntry mod = {
  name: "test",
  version_: "0",
  functions: [
    func!(func0),
    func!(func1),
    func!(func2),
    func!(func3),
    func!(test),
    func!(makeTestResource),
    func!(fff),
    func!(ggg),
    zend_function_entry(),
  ],
  moduleStartup: (int, int moduleNumber) {
    phpClassRegistry!Test.register();
    phpClassRegistry!TestWithConstructor.register();
    phpResourceRegistry!TestResource.register(moduleNumber, (Resource*) { });
    return Result.SUCCESS;
  }
};


extern(C) ModuleEntry* get_module() {
  return &mod;
}

double func0() {
  return 0;
}
double func1(uint a = 1) {
  return a;
}
double func2(uint a = 1, int b = 2) {
  return a + b;
}

double func3(uint a = 1, int b = 2, int c = 3) {
  return a + b + c;
}


void test() {
  //writeln(zval(1).type,   " == ", Type.IsLong);
  //writeln(zval(1U).type,  " == ", Type.IsLong);
  //writeln(zval(1L).type,  " == ", Type.IsLong);
  //writeln(zval(1LU).type, " == ", Type.IsLong);
  //
  //writeln(zval(0).type,   " == ", Type.IsLong);
  //writeln(zval(0U).type,  " == ", Type.IsLong);
  //writeln(zval(0L).type,  " == ", Type.IsLong);
  //writeln(zval(0LU).type, " == ", Type.IsLong);
}

@phpResource struct TestResource {
  int a, b, c, d;
}

TestResource* makeTestResource() {
  auto tr = emalloc!TestResource;
  *tr = TestResource(1, 2, 3, 4);
  return tr;
}


@phpClass struct Test {
  int a = 1;
  int b = 2;
  int c = 3;
  int d = 4;
  zend_object std;

  int method() {
    return a + b + c + d;
  }

  long method2(double d, int i, bool b) {
    return (cast(long)d) + i + b;
  }

  void selfType(Test*) {}
}


struct TestWithConstructor {
  int a, b;
  zend_object std;

  this(int a, int b) {
    this.a = a;
    this.b = b;
  }

  int method() {
    return a + b;
  }
}

void fff(@phpClass TestWithConstructor* p) {}
@phpClass TestWithConstructor* ggg() { return null; }



void typedArray(HashTable* ht) {
  foreach (int k, int v; ht.typed) {}
  foreach (zval k, int v; ht.typed) {}
  foreach (scope const(char)[] k, int v; ht.typed) {}
  foreach (scope const(ubyte)[] k, int v; ht.typed) {}
  foreach (String* k, int v; ht.typed) {}
}

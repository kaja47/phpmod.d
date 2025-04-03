import phpmod;

ModuleEntry mod = {
  name: "test",
  version_: "0",
  functions: [
    func!rc,
    func!func0,
    func!func1,
    func!func2,
    func!func3,
    func!funcReturnVoid,
    func!funcArgNoName,
    func!funcMixed,
    func!funcNullable,
    func!funcVariadic0,
    func!funcVariadic1,
    func!funcVariadicLong,
    func!funcNullableReturnTypeString,
    func!funcNullableReturnTypeObject,
    func!funcNullableReturnTypeArray,
    func!funcNullableReturnTypeClass,
    func!funcArgTypehints,
    func!funcArgTypehintsNullable,
    func!funcArgTypehintsClasses,
    func!testAcceptObject,
    func!testReadFieldsOfUserlandObjects,
    func!test,
    func!makeTestResource,
    func!fff,
    func!ggg,
    func!throwException1,
    func!returnExceptionVoid,
    func!returnExceptionInt,
    func!returnSuccessInt,
    func!createStringNative,
    func!passStringNative,
    func!testPackedArray,
    func!testPackedArrayWithHoles,
    func!testHashArray,
    func!testArrayTypedMixedKeys,
    zend_function_entry(),
  ],
  moduleStartup: (int, int moduleNumber) {
    registerClass!Test;
    registerClass!TestWithConstructor;
    registerClass!TestWithPHPConstructor;
    registerClass!C0;
    registerClass!BigClass;
    registerResource!TestResource(moduleNumber);

    return Result.SUCCESS;
  }
};


extern(C) ModuleEntry* get_module() {
  return &mod;
}


int rc(zval* zval) {
  if (String* x = zval.asString()) {
    return x.gc.refcount;
  } else if (zend_object* x = zval.asObject()) {
    return x.gc.refcount;
  } else if (Resource* x = zval.asResource()) {
    return x.gc.refcount;
  } else {
    return -1;
  }
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

void funcReturnVoid() {}

void funcArgNoName(int, int, int) {}

zval funcMixed(zval a, zval* b) { return a; }

bool funcNullable(@nullable Test* obj) {
  return obj == null;
}

bool testAcceptObject(zend_object* obj) { return obj != null; }

long funcVariadic0(zval[] args ...) { return args.length; }
long funcVariadic1(bool, zval[] args ...) { return args.length; }

long funcVariadicLong(long[] args ...) {
  long sum;
  foreach (x; args) sum += x;
  return sum;
}

@nullable String*      funcNullableReturnTypeString() { return null; }
@nullable zend_object* funcNullableReturnTypeObject() { return null; }
@nullable HashTable*   funcNullableReturnTypeArray()  { return null; }
@nullable Test*        funcNullableReturnTypeClass()  { return null; }


void funcArgTypehints(String* str, HashTable* ht, Resource* res, zend_object* obj) {}
void funcArgTypehintsNullable(@nullable String* str, @nullable HashTable* ht, @nullable Resource* res, @nullable zend_object* obj) {}
void funcArgTypehintsClasses(Test* a, @nullable Test* b, @phpClass TestWithConstructor* c, @nullable @phpClass TestWithConstructor* d) {}


void test() {
  //import std.stdio;
  //writeln(zval(1).type,   " == ", Type.Long);
  //writeln(zval(1U).type,  " == ", Type.Long);
  //writeln(zval(1L).type,  " == ", Type.Long);
  //writeln(zval(1LU).type, " == ", Type.Long);
  //
  //writeln(zval(0).type,   " == ", Type.Long);
  //writeln(zval(0U).type,  " == ", Type.Long);
  //writeln(zval(0L).type,  " == ", Type.Long);
  //writeln(zval(0LU).type, " == ", Type.Long);
}

@phpResource struct TestResource {
  int a, b, c, d;
}

TestResource* makeTestResource() {
  auto tr = allocateResource!TestResource();
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

  ~this() {}

  int method() {
    return a + b;
  }
}

void fff(@(1) @phpClass TestWithConstructor* p) {}
@phpClass TestWithConstructor* ggg() { return null; }


@phpClass struct TestWithPHPConstructor {
  int a;
  zend_object std;
  void __construct(int aa) { a = aa; }
  void __destruct() {}
  int get() { return a; }
}


long testReadFieldsOfUserlandObjects(ZendObject* o) {
  zval tmp;
  return o.readProperty("a", &tmp).toLong;
}



// not called from php, just there to check if it compiles
void testArrayCompile(HashTable* ht) {
  foreach (kv; ht.byValue) {}
  foreach (kv; ht.byKey) {}
  foreach (kv; ht.byKeyValue) {}

  const(HashTable)* cht = ht;
  foreach (v;  cht.byValue) {}
  foreach (v;  cht.byKey) {}
  foreach (kv; cht.byKeyValue) {}

  foreach (uint v; ht.typed) {}
  foreach (long v; ht.typed) {}
  foreach (double v; ht.typed) {}
  foreach (float v; ht.typed) {}
  foreach (bool v; ht.typed) {}
  foreach (zend_object* v; ht.typed) {}
  foreach (HashTable* v; ht.typed) {}
  foreach (zend_string* v; ht.typed) {}

  foreach (int k,  int v; ht.typed) {}
  foreach (zval k, int v; ht.typed) {}
  foreach (Key k,  int v; ht.typed) {}
  foreach (scope const(char)[] k, int v; ht.typed) {}
  foreach (scope const(ubyte)[] k, int v; ht.typed) {}
  foreach (String* k, int v; ht.typed) {}

  if (auto z = 1 in *ht) {}
  if (auto z = "asd" in *ht) {}
  zval* _ = (*ht)[1];
        _ = (*ht)["asd"];

  auto newHT = HashTable.copy([1,2,3]);
}

bool testArrayTypedMixedKeys(HashTable* ht, HashTable* packed) {
  assert(!ht.isPacked);
  assert(packed.isPacked);

  bool ex = false;
  try {
    foreach (const(char)[] k, zval v; ht.typed) {}
  } catch (Exception e) {
    ex = true;
  }
  if (!ex) return false;
 
  foreach (const(char)[] k, zval v; ht.typedConvertKeys) {
    if (!(k == "1" || k == "x")) return false;
  }

  foreach (const(char)[] k, zval v; packed.typedConvertKeys) {
    if (!(k == "0" || k == "1")) return false;
  }

  return true;
}


void throwException1() {
  auto nonZeroTerminated = "exceptionbadbadbadbad";
  throw new Exception(nonZeroTerminated[0 .. 9]);
}

Try!void returnExceptionVoid() { return failure("returnedException"); }
Try!int returnExceptionInt()   { return failure!int("returnedException"); }
Try!int returnSuccessInt()     { return success(1); }


String* createStringNative() {
  return String.copy("test");
}

String* passStringNative(String* str) {
  return str.bump();
}



bool testPackedArray(HashTable* ht) {
  if (!ht.isPacked) return false;
  if (ht.hasHoles) return false;
  {
    int n;
    foreach (zval* z; ht.byValue) {
      n++;
      if (z.type != Type.Long) return false;
    }
    if (n != 5) return false;
  }
  {
    int n;
    foreach (kv; ht.byKeyValue) {
      if (kv.key.type != Type.Long) return false;
      if (kv.value.type != Type.Long) return false;
      if (kv.key.lval != n) return false;
      n++;
    }
    if (n != 5) return false;
  }
  return true;
}

bool testPackedArrayWithHoles(HashTable* ht) {
  if (!ht.isPacked) return false;
  if (!ht.hasHoles) return false;
  {
    int n;
    foreach (zval* z; ht.byValue) {
      n++;
      if (z.type != Type.Long) return false;
    }
    if (n != 4) return false;
  }
  {
    int n;
    foreach (kv; ht.byKeyValue) {
      if (kv.key.type != Type.Long) return false;
      if (kv.value.type != Type.Long) return false;
      if (kv.key.lval != n + 1) return false;
      n++;
    }
    if (n != 4) return false;
  }
  return true;
}

bool testHashArray(HashTable* ht) {
  if (ht.isPacked) return false;
  {
    int n;
    foreach (zval* z; ht.byValue) {
      n++;
      if (z.type != Type.Long) return false;
    }
    if (n != 4) return false;
  }
  {
    int n;
    foreach (kv; ht.byKeyValue) {
      if (kv.key.type != Type.Long) return false;
      if (kv.value.type != Type.Long) return false;
      n++;
    }
    if (n != 4) return false;
  }
  return true;
}



@phpClass struct C0 {
  int a, b, c, d;
  zend_object std;

  void __construct() {
    a = 1;
    b = 2;
    c = 3;
    d = 4;
  }
  void __destruct() {
  }
}


@phpClass struct BigClass {
  ubyte* ptr;
  zend_object std;

  void __construct(int i) {
    ptr = cast(ubyte*) _emalloc(1 << 20); // 1MB
  }
  void __destruct() {
    _efree(ptr);
  }
}

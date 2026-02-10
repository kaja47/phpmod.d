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
    func!funcReturnLongOrBool,
    func!funcArgLongOrBool,
    func!funcNullable,
    func!funcVariadic0,
    func!funcVariadic1,
    func!funcVariadicLong,
    func!funcVariadicInt,
    func!funcVariadicShort,
    func!funcVariadicUbyte,
    func!funcVariadicBool,
    func!funcVariadicFloat,
    func!funcVariadicString,
    func!funcVariadicXY,
    func!funcNullableReturnTypeString,
    func!funcNullableReturnTypeObject,
    func!funcNullableReturnTypeArray,
    func!funcNullableReturnTypeClass,
    func!funcArgTypehints,
    func!funcArgTypehintsNullable,
    func!funcArgTypehintsClasses,
    func!funcArgAutoClass,
    func!testAcceptObject,
    func!testReadFieldsOfUserspaceObjects,
    func!funcCallNoArgsMethod,
    func!funcCallNoArgsMethodTyped,
    func!funcCallThrowsMethod,
    func!testBasics,
    func!makeTestResource,
    func!fff,
    func!ggg,
    func!throwException1,
    func!returnExceptionVoid,
    func!returnExceptionInt,
    func!returnSuccessInt,
    func!createStringNative,
    func!passStringNative,
    func!testAutozvals,
    func!testPackedArray,
    func!testPackedArrayWithHoles,
    func!testHashArray,
    func!testArrayTypedMixedKeys,
    func!testArrayIterationIntValues,
    zend_function_entry(),
  ],
  moduleStartup: (int, int moduleNumber) {
    registerClass!Test;
    registerClass!TestWithConstructor;
    registerClass!TestWithPHPConstructor;
    registerClass!C0;
    registerClass!BigClass;
    registerClass!ClassT;
    registerClass!ClassTNamed;
    registerResource!TestResource(moduleNumber);

    registerConstant!ENUM_CONST(moduleNumber);
    registerConstant!IMMUTABLE_CONST(moduleNumber);
    registerConstant!(funcConst, "FUNC_CONST")(moduleNumber);
    registerClass!ClassWithConstants;
    registerClass!XY0;

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


enum ENUM_CONST = 1;
immutable IMMUTABLE_CONST = 2;
auto funcConst() {
  return 3;
}

@phpClass
struct ClassWithConstants {
  enum BOOL_C = true;
  enum INT_C = 1;
  enum DOUBLE_C = 1.0;
  enum STR_C = "1";

  zend_object obj;
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


alias LongOrBool = TypedZval!(long, bool);
LongOrBool funcReturnLongOrBool() {
  // test if all this compiles
  return LongOrBool(1);
  return LongOrBool(ubyte(1));
  return LongOrBool(true);
}
LongOrBool funcArgLongOrBool(LongOrBool arg) nothrow {
  //zval z = arg; // test if compiles
  return arg;
}


bool funcNullable(@nullable Test* obj) {
  return obj == null;
}

bool testAcceptObject(zend_object* obj) { return obj != null; }

long funcVariadic0(zval[] args ...) { return args.length; }
long funcVariadic1(bool, zval[] args ...) { return args.length; }

private auto sum(T)(T[] args) {
  static if (is(T == float)) {
    float res = 0;
  } else {
    long res;
  }
  foreach (x; args) res += x;
  return res;
}

long funcVariadicLong(long[] args ...)   => sum(args);
long funcVariadicInt  (int[] args ...)   => sum(args);
long funcVariadicShort(short[] args ...) => sum(args);
long funcVariadicUbyte(ubyte[] args ...) => sum(args);
long funcVariadicBool (bool[] args ...)  => sum(args);
float funcVariadicFloat(float[] args ...) => sum(args);
long funcVariadicString(zend_string*[] args ...) => args.length;

@phpClass struct XY0 {
  private int x,y; zend_object _z;
  this(int x, int y) { this.x = x; this.y = y; }
}

long funcVariadicXY(XY0*[] args ...) {
  long sum;
  foreach (xy; args) {
    sum += xy.x + xy.y;
  }
  return sum;
}


@nullable String*      funcNullableReturnTypeString() { return null; }
@nullable zend_object* funcNullableReturnTypeObject() { return null; }
@nullable HashTable*   funcNullableReturnTypeArray()  { return null; }
@nullable Test*        funcNullableReturnTypeClass()  { return null; }


void funcArgTypehints(String* str, HashTable* ht, Resource* res, zend_object* obj) {}
void funcArgTypehintsNullable(@nullable String* str, @nullable HashTable* ht, @nullable Resource* res, @nullable zend_object* obj) {}
void funcArgTypehintsClasses(Test* a, @nullable Test* b, TestWithConstructor* c, @nullable TestWithConstructor* d) {}


bool testBasics() {
  assert(zval(1).type        == Type.Long);
  assert(zval(1U).type       == Type.Long);
  assert(zval(1L).type       == Type.Long);
  assert(zval(1LU).type      == Type.Long);
  assert(zval(0).type        == Type.Long);
  assert(zval(0U).type       == Type.Long);
  assert(zval(0L).type       == Type.Long);
  assert(zval(0LU).type      == Type.Long);
  assert(zval(ubyte(1)).type == Type.Long);
  assert(zval(byte(1)).type  == Type.Long);
  assert(zval(true).type     == Type.True);
  assert(zval(false).type    == Type.False);
  assert(zval(0.0f).type     == Type.Double);
  assert(zval(0.0).type      == Type.Double);
  return true;
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

  long methodVariadic(zval[] args ...) { return args.length; }
  long methodVariadic2(int a, zval[] args ...) { return args.length; }
  long methodDefaultArgument(long arg = 1337) { return arg; }
  bool methodNullableArgument(@nullable Test* arg) { return arg == null; }
}

struct NativeStruct {
  private int _x, _y, _z = 1;
  this(int x, int y, int z) {
    _x = x;
    _y = y;
    _z = z;
  }
  long x() => _x;
  long y() => _y;
  long z() => _z;
}

alias ClassT = Class!NativeStruct;
void funcArgAutoClass(ClassT* arg) {}

struct NativeStruct2 { int a; }
alias ClassTNamed = Class!(NativeStruct2, "NativeStructRenamed");


@phpClass
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


long testReadFieldsOfUserspaceObjects(ZendObject* o) {
  zval tmp;
  return o.readProperty("a", &tmp).toLong;
}

zval* funcCallNoArgsMethod(ZendObject* o) {
  zval tmp;
  zval* res = o.callMethod("noArgs", &tmp);
  return res;
}

long funcCallNoArgsMethodTyped(ZendObject* o) {
  return o.call!("noargs", long);
}

ZendString* funcCallThrowsMethod(ZendObject* o) {
  try {
    o.call!("throws", long);
  } catch (PHPException e) {
    import core.stdc.stdio;
    printf("%s\n", e.msg.ptr);
    return bump(e.owner);
  }
  return ZendString.copy("nothrow");
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

  // values
  foreach (uint v; ht.typed) {}
  foreach (long v; ht.typed) {}
  foreach (double v; ht.typed) {}
  foreach (float v; ht.typed) {}
  foreach (bool v; ht.typed) {}
  foreach (scope const(char)[] v; ht.typed) {}
  foreach (scope const(ubyte)[] v; ht.typed) {}
  foreach (zend_object* v; ht.typed) {}
  foreach (HashTable* v; ht.typed) {}
  foreach (zend_string* v; ht.typed) {}
  foreach (Test* v; ht.typed) {}

  // keys
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

void testArrayIterationIntValues(HashTable* ht) {
  foreach (int i; ht.typed) {}
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

void testAutozvals(zval* arg) {
  if (!arg.isRefcounted) throw new Exception("test is useless when zval points to something not refcounted");
  autozval z = arg.bump();
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

import phpmod;

// Mixing in the `mod` template do the magic of auto-extension (template
// parameter is the module, usually the same as the file name) - it exposes
// every public function as a PHP function and every struct annotated by
// @phpClass attribute as PHP class.
// When more control over specifics is desired, it's possible to define
// zend_module_entry structure and get_module() symbol manually (see
// example/popcnt.d and example/writer.d).
mixin mod!api;

// This will be wrapped to a PHP function that accepts integer value and
// returns null. No type conversion are ever performed, PHP value has to be
// long. Bools, floating point values, numeric string (and also references)
// will always lead to error.
void args1(long x) {}

// This will generate PHP function that accepts integer value and checks if
// provided value is in range matching the native type. In this case a call
// ends with error when PHP integer is negative or greater than 2^32.
void args2(uint x) {}

// Accepting unsigned longs (always 64 bits in D) is not allowed because half
// of its possible values is never possible to represent in PHP. In order to
// allow ulong arguments we would need to do implicit casting but that's a bad
// decision which will lead to subtle bugs. If you want to treat provided PHP
// longs as native unsigned longs, specify function parameter as long and do
// casting on your own.
//void f3(ulong x) {}
void args3(long x) { ulong y = cast(ulong) x; }

// PHP represent floating point values always as a double, but when native
// function declares argument as simple float, it's automatically converted. In
// my opinion, loss of some precision is acceptable.
// If PHP code passes integer value into this function, it's accepted and
// converted to floating point value matching behavior of `strict_types`.
// Rationale in this case is that the code calling this function may perform
// some arithmetic operations that almost always evaluate to float, but in some
// rare cases (for example when values in expression `$a / $b` align perfectly)
// they may stay as integers, causing sporadic errors. It matches intuition that
// the range of floats contains all integers (albeit with limited precision),
// but not other way around.
void args4(double a, float b) {}

// Bool as the remaining scalar type is seamlessly translated into native bool.
void args5(bool a) {}

// When you want to work with PHP strings, you can declare parameters as native
// char or byte slices.
// Arguments point inside PHP zend_string structs and they don't own the
// memory. For this reason they have to be marked as const and scope. `const`
// mean we cannot modify contents of the string. It may be shared and it's safe
// to do only when rercount is 1 and the string is not interned. Neither of
// those facts we can check when we see only contents of the string.
// `scope` keyword forces compiler to check we do not keep reference to the
// string when the function exists (it must stay only in the current scope).
// Again that is not safe. Lifetime of the string is managed by PHP refcount
// mechanism and PHP may deallocate the string and suddenly we would have
// dangling reference.
// In order to safely keep a reference to the string argument we need to bump
// the refcount but that's not possible for slice parameters.
void args6(scope const(char)[] a, scope const(ubyte)[] b) {}

// Alternatively we can accept a string argument by declaring two parameters.
// One with type const(char)* for start of the string and second with some
// integral type for string length. This variant is generally not recommended
// and it's there mainly to ease an integration with functionality written in C.
void args7(scope const(char)* str, size_t length) {}

// When we need better control over strings passed as arguments, we can declare
// them as `ZendString*`.
ZendString* args8(ZendString* str) {
  return bump(str);
}

// Same for HashTable (aka PHP array) or ZendObject
void args9(HashTable* ht) {}
void args10(ZendObject* obj) {}

// When parameters are declared as const pointers we can only read their contents
// but never modify it. It's not even possible to return them because we would
// need to increment their refcount and that would modify the object. (Compiler
// will protest.)
// In D language const is transitive meaning any value derived form const
// object is also const. As result we cannot change not only HashTable struct
// itself but also any data structures reachable from this HashTable.
void args11(const(ZendString)* str, const(HashTable)* ht, const(ZendObject)* obj) {}

// For parameters annotated by @nullable attribute the wrapped function accepts PHP
// null and translates it into native null.
void args12(@nullable ZendString* str, @nullable HashTable* ht, @nullable ZendObject* obj) {}

// For cases when we want to accept anything, we can declare parameters as
// `zval` or pointer to `zval`.
void args13(zval* x, zval y) {}

// Default argument values for parameters on our native function are preserved
// and propagated to the PHP runtime.
// The function can be called from PHP as f13(), f13(0) or f13(b: 1) and
// arguments not mentioned get the correct default value from the native
// declaration.
auto args14(int a = 1, int b = 3) {}

// Variadic arguments are also supported.
void args15(zval[] args...) {}

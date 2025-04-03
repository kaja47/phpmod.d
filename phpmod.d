// license: GPL3

/**
 * Compile by `gdc-14 -O2 -shared -fPIC -fpreview=all yourextension.d phpmod.d`.
 *
 * `-fpreview=all` enables all new features in GDC. Only two of them are needed:
 * `-fpreview=dip1000` to enable checking if scope parameters escape and
 * `-fpreview=dip1008` to allow exceptions in @nogc code as we want to avoid
 * D's GC machinery.
 *
 * To compile debug build compatible with PHP debug builds, run
 * `gdc-14 -shared -fPIC -fpreview=all -fversion=ZEND_DEBUG`.
 *
 * It's possible to compile without dependency on D stdlib via
 * `gdc-14 -fno-druntime -shared -fPIC -fpreview=all`.
 *
 * Never supported:
 *  - crummy typing rules and coercions following exact PHP semantic
 *    (strict_types is the only way)
 *  - PHP references.
 *    Internally references are represented as a different type but in PHP
 *    userspace they are indistinguishable from the type they reference. This
 *    poisons the whole PHP codebase where we need to constantly check for
 *    references to unwrap them even though references are almost never used.
 *  - 32-bit platforms
 *    Who cares. Even RPi zero 2 is 64-bit and it costs peanuts. Keeping 32-bit
 *    version is a waste of time.
 *  - windows support
 */
module phpmod;

import core.lifetime : emplace;
import std.meta : anySatisfy, AliasSeq;
import std.traits : isIntegral, Parameters, ReturnType;
import std.typecons : Tuple;
import core.stdc.stdlib;
import core.stdc.stdint : uintptr_t;
import core.stdc.string;


// First we need to decide which exact version of PHP we pretend to support.
// Without any flags we should target the latest stable PHP version in non-debug mode.
version (PHP85) enum PHPVersion = 85;
else version (PHP84) enum PHPVersion = 84;
else version (PHP83) enum PHPVersion = 83;
else version (PHP82) static assert(0, "unsupported version");
else enum PHPVersion = 84;

// debug  build can be enabled by compiling with -fversion=ZEND_DEBUG
version (ZEND_DEBUG) {
  enum PHPDebugBuild = true;
} else {
  enum PHPDebugBuild = false;
}

static if (PHPVersion == 85) {
  enum ZendApi = 20240925;
  enum BuildId = "API20240925,NTS" ~ (PHPDebugBuild ? ",debug" : "");
} else static if (PHPVersion == 84) {
  enum ZendApi = 20240924;
  enum BuildId = "API20240924,NTS" ~ (PHPDebugBuild ? ",debug" : "");
} else static if (PHPVersion == 83) {
  static assert(0, "TODO");
}

// -fno-druntime disables exceptions
private enum ExceptionsAllowed = __traits(compiles, () { try {} catch (Exception ex) {} });



// Import (and document) useful set of symbols and global variables exposed
// from PHP header files.
// Block is __gshared to make all variable true globals and not thread locals.
// Only ZTS build have some use for thread local variables (see ZEND_TLS in PHP source).
extern extern(C) @nogc nothrow __gshared {

  // Absolutely massive struct which I will not reproduce here. Only thing we
  // need is the field pointing to the current exception. See currentException().
  struct zend_executor_globals;
  zend_executor_globals executor_globals;
  alias EG = executor_globals;

  void *tsrm_get_ls_cache();

  version (ZEND_DEBUG) {
    void* _emalloc(size_t size,
        const(char)* filename = __FILE__.ptr, uint lineno = __LINE__,
        const(char)* orig_filename = null, uint orig_lineno = 0
    );
    void* _efree(void* ptr,
        const(char)* filename = __FILE__.ptr, uint lineno = __LINE__,
        const(char)* orig_filename = null, uint orig_lineno = 0
    );
  } else {
    void* _emalloc(size_t size);
    void* _efree(void* ptr);
  }


  // === parameter parsing ===

  @attribute("cold") void zend_wrong_parameters_count_error(uint min_num_args, uint max_num_args);
  @attribute("cold") void zend_argument_type_error(uint arg_num, const char *format, ...);
  @attribute("cold") void zend_type_error(const char *format, ...);
  @attribute("cold") zend_object* zend_throw_exception(zend_class_entry* exception_ce, const(char)* message, long code);
  @attribute("cold") zend_object* zend_throw_exception_ex(zend_class_entry* exception_ce, long code, const(char)* format, ...);
  @attribute("cold") void zend_clear_exception();


  // === zvals ===

  /// decrement refcount (if applicable) and deallocate object/array/string on
  /// RC == 0
  void zval_ptr_dtor(zval *zval_ptr);

  /// converts zval to string
  zend_string* zval_get_string_func(zval *op);


  // === arrays ===

  // note: In PHP source zend_hash_* functions don't convert numeric string
  // keys into integers. That's the responsibility of inline zend_symtable_*
  // functions. They are declared as inline in header file and therefore not
  // exposed as symbols.

  /// Allocate new uninitialized HashTable struct. Memory for key/value data has
  /// to be allocated separately by `zend_hash_real_init` or similar.
  HashTable* _zend_new_array(uint nSize);
  /// Allocate and zero memory for key/value data.
  void zend_hash_real_init(HashTable *ht, bool packed);
  /// ditto
  void zend_hash_real_init_packed(HashTable *ht);
  /// ditto
  void zend_hash_real_init_mixed(HashTable *ht);

  /// Convert hash table representation between hash map and packed array.
  /// Performs no safety checks whether the hashtable has given representation.
  void zend_hash_packed_to_hash(HashTable *ht);
  /// ditto
  void zend_hash_to_packed(HashTable *ht);
  // Safe to use on both hash tables and packed arrays.
  HashTable* zend_array_to_list(const(HashTable) *source);

  /// Grow hash table. Grow only, never shrink.
  void zend_hash_extend(HashTable *ht, uint nSize, bool packed);

  /// Destroy contents of the array `ht`, deallocate memory for key/values and
  /// HashTable struct.
  void zend_array_destroy(HashTable *ht);
  HashTable* zend_array_dup(const(HashTable)* source);
  void zend_hash_rehash(HashTable *ht);

  // Return pointer into table or null if not present.
  zval* zend_hash_find(const(HashTable)* ht, zend_string* key);
  zval* zend_hash_find_known_hash(const(HashTable)* ht, const(zend_string)* key);
  zval* zend_hash_str_find(const(HashTable)* ht, const(char)* key, size_t len);
  zval* zend_hash_index_find(const(HashTable)* ht, ulong h);
  // (only for non-packed hash maps);
  zval* _zend_hash_index_find(const(HashTable)* ht, ulong h);

  /// Create new entry, or fail if it exists.
  zval* zend_hash_add(HashTable *ht, zend_string *key, zval *pData);
  zval* zend_hash_str_add(HashTable *ht, const(char) *str, size_t len, zval *pData);
  zval* zend_hash_index_add(HashTable *ht, ulong h, zval *pData);

  // Create new entry. We know it doesn't exist.
  zval* zend_hash_add_new(HashTable *ht, zend_string *key, zval *pData);
  zval* zend_hash_str_add_new(HashTable *ht, const(char) *str, size_t len, zval *pData);
  zval* zend_hash_index_add_new(HashTable *ht, ulong h, zval *pData);

  /// Create new entry, or update the existing one.
  zval* zend_hash_update(HashTable *ht, zend_string *key, zval *pData);
  zval* zend_hash_str_update(HashTable *ht, const(char) *str, size_t len, zval *pData);
  zval* zend_hash_index_update(HashTable *ht, ulong h, zval *pData);

  /// Look up an existing entry, or create one with a NULL value.
  zval* zend_hash_lookup(HashTable *ht, zend_string *key);
  zval* zend_hash_index_lookup(HashTable *ht, ulong h);

  /// Append to an array.
  zval* zend_hash_next_index_insert(HashTable *ht, zval *pData);
  /// Append to and array + we know this new offset doesn't exist. ???
  zval* zend_hash_next_index_insert_new(HashTable *ht, zval *pData);

  // delete
  zend_result zend_hash_del(HashTable *ht, zend_string *key);
  zend_result zend_hash_str_del(HashTable *ht, const(char) *str, size_t len);
  zend_result zend_hash_index_del(HashTable *ht, ulong h);


  // === strings ===

  // recalculate and update hash value of given string
  ulong zend_string_hash_func(zend_string *str);
  // compute hash value of given string
  ulong zend_hash_func(const(char)* str, size_t len);

  alias zend_string_init_interned_func_t = zend_string * function(const char *str, size_t size, bool permanent);
  zend_string_init_interned_func_t zend_string_init_interned;


  // === resources ===

  int zend_register_list_destructors_ex(rsrc_dtor_func_t ld, rsrc_dtor_func_t pld, const(char) *type_name, int module_number);
  zend_resource* zend_register_resource(void* rsrc_pointer, int rsrc_type);

  struct php_stream;
  int _php_stream_cast(php_stream *stream, int castas, void **ret, int show_err);
  int php_file_le_stream();


  // === classes and objects ===

  /// Allocate and initialize new stdClass object and place it into `arg`.
  void object_init(zval *arg);
  /// Allocates and initializes object of given class (userspace or internal) and
  /// places it into `arg`. Does not call constructor.
  /// May fail when trying to create an instance of interface, trait, enum or
  /// abstract class or when call to zend_update_class_constants fails whatever
  /// that means.
  zend_result object_init_ex(zval *arg, zend_class_entry *class_type);
  /// Similar to object_init_ex, but can initialize some properties for
  /// userspace classes (those without create_object handler). Used only twice
  /// in whole PHP codebase. Most likely useless.
  zend_result object_and_properties_init(zval *arg, zend_class_entry *class_type, HashTable *properties);
  /// Allocates and initializes an object of given class (userspace or internal),
  /// then calls constructor and places the result into `arg`.
  /// May fail, see object_init_ex for details.
  zend_result object_init_with_constructor(zval *arg, zend_class_entry *class_type, uint param_count, zval *params, HashTable *named_params);
  /// Allocate object of given userspace (not internal) class and initialize
  /// its header. Properties (except the guard) are not touched.
  zend_object* zend_objects_new(zend_class_entry *ce);
  /// Initialize declared properties to declared default values.
  void object_properties_init(zend_object *object, zend_class_entry *class_type);
  /// Initialize object header. Declared properties (except the guard) are not
  /// touched.
  void zend_object_std_init(zend_object *object, zend_class_entry *ce);
  /// Destroy and deallocate object.
  void zend_object_std_dtor(zend_object *object);

  alias CacheSlot = void*[3];
  zval *zend_read_property(zend_class_entry *scope_, zend_object *object, const(char)* name, size_t name_length, bool silent, zval *rv);
  void zend_update_property(zend_class_entry *scope_, zend_object *object, const(char)* name, size_t name_length, zval *value);
  void zend_unset_property(zend_class_entry *scope_, zend_object *object, const(char)* name, size_t name_length);

  zval *zend_read_property_ex(zend_class_entry *scope_, zend_object *object, zend_string *name, bool silent, zval *rv);

  zend_class_entry *zend_ce_traversable;
  zend_class_entry *zend_ce_aggregate;
  zend_class_entry *zend_ce_iterator;
  zend_class_entry *zend_ce_arrayaccess;
  zend_class_entry *zend_ce_serializable;
  zend_class_entry *zend_ce_countable;
  zend_class_entry *zend_ce_stringable;
  zend_class_entry *zend_ce_internal_iterator;

  const(zend_object_handlers) std_object_handlers;

  zend_class_entry *zend_register_internal_class_with_flags(zend_class_entry *class_entry, zend_class_entry *parent_ce, uint ce_flags);
  void zend_class_implements(zend_class_entry *class_entry, int num_interfaces, ...);
  bool zend_class_implements_interface(const(zend_class_entry)* class_ce, const(zend_class_entry)* interface_ce);

  struct zend_class_constant;
  zend_class_constant *zend_declare_typed_class_constant(zend_class_entry *ce, zend_string *name, zval *value, int flags, zend_string *doc_comment, zend_type type);



  // === functions ===

  zend_result zend_call_function(zend_fcall_info *fci, zend_fcall_info_cache *fci_cache);
  void zend_release_fcall_info_cache(zend_fcall_info_cache *fcc);
  zend_result zend_fcall_info_init(zval *callable, uint check_flags, zend_fcall_info *fci, zend_fcall_info_cache *fcc, zend_string **callable_name, char **error);


  // === iterators ===

  void zend_iterator_dtor(zend_object_iterator *iter);


  // === constants ===

  zval* zend_get_constant(zend_string* name);
  zval* zend_get_constant_str(const(char)* name, size_t name_len);
  zval* zend_get_constant_ex(zend_string* name, zend_class_entry* scope_, uint flags);
  zval* zend_get_class_constant_ex(zend_string* class_name, zend_string* constant_name, zend_class_entry* scope_, uint flags);
  void zend_register_bool_constant(const(char)* name, size_t name_len, bool bval, int flags, int module_number);
  void zend_register_null_constant(const(char)* name, size_t name_len, int flags, int module_number);
  void zend_register_long_constant(const(char)* name, size_t name_len, long lval, int flags, int module_number);
  void zend_register_double_constant(const(char)* name, size_t name_len, double dval, int flags, int module_number);
  void zend_register_string_constant(const(char)* name, size_t name_len, const(char)* strval, int flags, int module_number);
  void zend_register_stringl_constant(const(char)* name, size_t name_len, const(char)* strval, size_t strlen, int flags, int module_number);

}

alias zend_long  = long;
alias zend_ulong = ulong;

alias rsrc_dtor_func_t = void function(zend_resource*);

// flags for constants
enum CONST_CS            = 0;          /* No longer used -- always case sensitive */
enum CONST_PERSISTENT    = (1<<0);     /* Persistent */
enum CONST_NO_FILE_CACHE = (1<<1);     /* Can't be saved in file cache */
enum CONST_DEPRECATED    = (1<<2);     /* Deprecated */
enum CONST_OWNED         = (1<<3);     /* constant should be destroyed together with class */
enum CONST_RECURSIVE     = (1<<4);     /* Recursion protection for constant evaluation */



version (GNU) {
  import gcc.attributes : attribute;
  import gcc.builtins;
  pragma(inline, true) bool   likely()(bool e) { return !!__builtin_expect(e, 1); };
  pragma(inline, true) bool unlikely()(bool e) { return !!__builtin_expect(e, 0); };
} else {
  struct attribute { string dummy1; string dummy2; }
  pragma(inline, true) bool   likely()(bool e) { return e; };
  pragma(inline, true) bool unlikely()(bool e) { return e; };
}
enum hidden = attribute("visibility", "hidden");





version (ZEND_DEBUG) {
  private void _allocDebugParametersPrototype(
    const(char)* filename = __FILE__.ptr, uint lineno = __LINE__,
    const(char)* orig_filename = null, uint orig_lineno = 0
  );
  static if (is(typeof(_allocDebugParametersPrototype) DebugParams == __parameters)) {}
} else {
  alias DebugParams = AliasSeq!();
}


pragma(inline, true)
T* emalloc(T)(DebugParams _) @nogc nothrow =>
  cast(T*) _emalloc(T.sizeof, _);

pragma(inline, true)
T[] emallocArray(T)(size_t n, DebugParams _) @nogc nothrow =>
  (cast(T*) _emalloc(T.sizeof * n, _))[0 .. n];

void* pemalloc(size_t n, bool persistent) @nogc nothrow =>
  !persistent ? _emalloc(n) : malloc(n);

void pefree(void* ptr, bool persistent) @nogc nothrow =>
  !persistent ? _efree(ptr) : free(ptr);



// === exception support ===


pragma(inline, true)
private zend_object* currentException() nothrow {
  return *cast(zend_object**)((cast(ubyte*) &executor_globals) + _exceptionOffsetInExecGlobals);
}
static if (PHPVersion == 85) {
  private enum _exceptionOffsetInExecGlobals = 960;
} else static if (PHPVersion == 84) {
  private enum _exceptionOffsetInExecGlobals = 936;
} else static if (PHPVersion == 83) {
  private enum _exceptionOffsetInExecGlobals = 864;
} else static assert(0);


/** Variant of exception modified to accept C-style zero terminated string and
 *  optional class entry of the exception that will be rethrown on PHP side.
 *  It's mainly a convenience feature, phpmod.d can catch any native exception and
 *  rethrow it to userspace PHP code. */
static if (ExceptionsAllowed)
class PHPException : Exception {
  const(char)* _msg;
  zend_class_entry* ce;
  zend_string* owner; // owner of `_msg`, may be null.
  pure nothrow @nogc @safe this(
      const(char)* message, zend_class_entry* ce = null, zend_string* owner = null,
      string file = __FILE__, size_t line = __LINE__, Throwable nextInChain = null
  ) {
    super("", file, line, nextInChain);
    this._msg = message;
    this.ce = ce;
  }
  ~this() {
    if (owner) {
      release(owner);
      owner = null;
    }
  }
}

/** Type intended as a result type for code that doesn't want to throw an
 *  exception. */
struct Try(T) {
  pragma(inline, true):
  const(char)* errorMessage;
  static if (!is(T == void)) {
    T result;
  }
}
pragma(inline, true)
Try!T success(T)(T val) { return Try!T(null, val); }
pragma(inline, true)
Try!T failure(T = void)(const(char)* errMsg) { return Try!T(errMsg); }




// === function wrapping ===


/** Wrap a native function 'f' and generate function entry structure needed for
  * PHP to be able to call it. */
const(zend_function_entry) func(alias f, alias phpName = "")() {
  static if (phpName == "") {
    enum _phpName = __traits(identifier, f);
  } else {
    enum _phpName = phpName;
  }
  return makeFunctionEntry!(f, _phpName, true, false);
}


private const(zend_function_entry) makeFunctionEntry(alias _f, alias phpName, alias bool _generateFrameless, alias bool firstArgIsThis)() {
  static if (PHPVersion >= 84) {
    enum generateFrameless = _generateFrameless;
  } else {
    enum generateFrameless = false;
  }

  static assert(!(generateFrameless && firstArgIsThis), "frameless function cannot access $this");
  static assert(__traits(isStaticFunction, _f), "first template argument must be a function");

  alias f = ConvertCStringArgumentsToSlices!_f;
  enum numParams = NumPHPParams!(f, firstArgIsThis);

  immutable static zend_internal_arg_info[numParams + 1] arginfo = [MakeArgInfos!(f, firstArgIsThis, phpName)];

  static if (generateFrameless && numParams < 4 && !IsVariadic!f) {
    enum wff = &wrapFunc!(f, true, false, zval*, zval*, zval*, zval*);
    immutable static zend_frameless_function_info[2] framelessArginfos = [
      immutable(zend_frameless_function_info)(wff, numParams),
      zend_frameless_function_info(),
    ];
    enum wf = &wrapFunc!(f, false, false, zend_execute_data*, zval*);
    return zend_function_entry(phpName, wf, arginfo.ptr, numParams, 0, framelessArginfos.ptr, null);
  } else {
    enum wf = &wrapFunc!(f, false, firstArgIsThis, zend_execute_data*, zval*);
    return zend_function_entry(phpName, wf, arginfo.ptr, numParams, 0/*, null, null*/);
  }
}


private template MakeArgInfos(alias f, alias bool firstArgIsThis, alias string phpName) {
  enum reqNumParams = ReqNumPHPParams!(f, firstArgIsThis);

  enum isConstructor = phpName == "__construct" || phpName == "__destruct";

  static if (is(typeof(f) R == return) && !isConstructor) {
    enum returnTypeHint = TypeHint!(R, false, __traits(getAttributes, f));
  } else {
    enum returnTypeHint = NoTypeHint;
  }

  alias MakeArgInfos = AliasSeq!(
    immutable(zend_internal_arg_info)(cast(const(char)*) reqNumParams, returnTypeHint, null)
  );

  static if (is(typeof(f) ParamTypes == __parameters)) {
    static foreach (i; 0 .. ParamTypes.length) {
      static if (!(firstArgIsThis && i == 0)) {
        MakeArgInfos = AliasSeq!(MakeArgInfos,
            immutable(zend_internal_arg_info)(
              ParamName!(f, i),
              TypeHint!(ParamTypes[i],
                IsVariadic!f && i == ParamTypes.length-1,
                __traits(getAttributes, ParamTypes[i .. i + 1])),
              DefaultArgStr!(f, i)
            ));
      }
    }
  } else static assert(0);
}

private template ParamName(alias f, alias i) {
  static if (is(typeof(f) PT == __parameters)) {
    // check if argument has a name
    static if (is(typeof(__traits(identifier, PT[i .. i + 1])))) {
      enum ParamName = __traits(identifier, PT[i .. i + 1]);
    } else {
      // if native argument is unnamed, use argN where N starts from 1
      private enum int j = i + 1;
      enum ParamName = "arg" ~ j.stringof;
    }
  } else static assert(0);
}

private enum IsVariadic(alias f) = __traits(getFunctionVariadicStyle, f) == "typesafe";

private enum NoTypeHint = immutable(zend_type)(null, 0);

private enum immutable(zend_type) TypeHint(T, alias bool isVariadic, attrs...) = function immutable(zend_type) {
  enum nullableMask = isNullable!attrs ? _ZEND_TYPE_NULLABLE_BIT : 0;

  static if (is(T == void))                    return zend_type(null, 0);
  else static if (is(T == bool))               return zend_type(null, (1 << Type.True) | (1 << Type.False));
  else static if (is(T : long))                return zend_type(null, 1 << Type.Long);
  else static if (is(T : double))              return zend_type(null, 1 << Type.Double);
  else static if (is(T : const(char)[]))       return zend_type(null, 1 << Type.String);
  else static if (is(T : const(ubyte)[]))      return zend_type(null, 1 << Type.String);
  else static if (is(T : const(zend_string)*)) return zend_type(null, (1 << Type.String) | nullableMask);
  else static if (is(T : const(HashTable)*))   return zend_type(null, (1 << Type.Array)  | nullableMask);
  else static if (is(T : const(zend_object)*)) return zend_type(null, (1 << Type.Object) | nullableMask);
  // it seems that resource parameters are not type-hinted (for example:
  // reflection indicates the first argument of `fseek` has no type-hint)
  else static if (is(T : const(zend_resource)*)) return zend_type(null, 0);
  else static if (is(T : R*, R) && isResource!R)  return zend_type(null, 0);
  else static if (is(T : C*, C) && (isPHPClass!C || isPHPClass_!attrs))
    return immutable(zend_type)(__traits(identifier, C).ptr, _ZEND_TYPE_LITERAL_NAME_BIT | nullableMask);
  else static if (is(T : Try!TT, TT))
    return TypeHint!(TT, false, attrs);
  else static if (is(T == zval) || is(T : const(zval)*))
    return zend_type(null, 0);
  else static if (isVariadic && is(T : const(Elem)[], Elem)) {
    immutable elementTypeHint = TypeHint!(Elem, false, attrs);
    return immutable(zend_type)(elementTypeHint.ptr, elementTypeHint.type_mask | _ZEND_IS_VARIADIC_BIT);
  }
  else {
    pragma(msg, "no type hint for type ", T);
    return zend_type(null, 0);
  }
}();

// Zend/zend_types.h
private enum {
  _ZEND_TYPE_EXTRA_FLAGS_SHIFT = 25,
  _ZEND_TYPE_MASK = ((1u << 25) - 1),
  /* Only one of these bits may be set. */
  _ZEND_TYPE_NAME_BIT = (1u << 24),
  // Used to signify that type.ptr is not a `zend_string*` but a `const char*`,
  _ZEND_TYPE_LITERAL_NAME_BIT = (1u << 23),
  _ZEND_TYPE_LIST_BIT = (1u << 22),
  _ZEND_TYPE_KIND_MASK = (_ZEND_TYPE_LIST_BIT|_ZEND_TYPE_NAME_BIT|_ZEND_TYPE_LITERAL_NAME_BIT),
  /* For BC behaviour with iterable type */
  _ZEND_TYPE_ITERABLE_BIT = (1u << 21),
  /* Whether the type list is arena allocated */
  _ZEND_TYPE_ARENA_BIT = (1u << 20),
  /* Whether the type list is an intersection type */
  _ZEND_TYPE_INTERSECTION_BIT = (1u << 19),
  /* Whether the type is a union type */
  _ZEND_TYPE_UNION_BIT = (1u << 18),
  /* Type mask excluding the flags above. */
  _ZEND_TYPE_MAY_BE_MASK = ((1u << 18) - 1),
  /* Must have same value as MAY_BE_NULL */
  _ZEND_TYPE_NULLABLE_BIT = 0x2u
}

// Zend/zend_compile.h
enum _ZEND_IS_VARIADIC_BIT = (1 << (_ZEND_TYPE_EXTRA_FLAGS_SHIFT + 2));



private template DefaultArgStr(alias f, size_t i) {
  static if (is(typeof(f) PT == __parameters)) {
    enum defArgFunc = mixin("(PT[i .. i+1] __args_) => __args_[0]");
    static if (is(typeof(defArgFunc()))) {
      enum d = defArgFunc();
      static if (is(typeof(d) : const(char)[])) {
        enum DefaultArgStr = d.stringof;
      } else static if (is(typeof(d) : long)) {
        enum l = long(d);
        enum s = l.stringof[0 .. $-1];
        enum DefaultArgStr = s.ptr;
      } else static if (is(typeof(d) : double)) {
        enum DefaultArgStr = d.stringof;
      } else static if (is(typeof(d) == bool)) {
        enum DefaultArgStr = d.stringof;
      } else static assert(0, "cannot handle default values of native type " ~ typeof(d).stringof);
    } else {
      enum DefaultArgStr = null;
    }
  }
}

private template DefaultArg(alias f, size_t i) {
  static if (is(typeof(f) ParamTypes == __parameters)) {
    enum defArgFunc = mixin("(ParamTypes[i .. i+1] __args_) => __args_[0]");
    static if (is(typeof(defArgFunc()))) {
      enum DefaultArg = defArgFunc();
    } else static assert(0);
  }
}

private enum NumPHPParams(alias f, alias bool firstArgIsThis) =
  Parameters!f.length - firstArgIsThis;

private enum ReqNumPHPParams(alias f, alias bool firstArgIsThis) = {
  // We try to find the last parameter without a default value.
  // For example `void f(int a = 1, int b, int c = 2)` requires at least 2
  // positional arguments. From PHP we can call it as `f(1, 2)` or `f(1, 2, 3)`;
  static foreach_reverse (i, p; Parameters!f) {
    static if (i == Parameters!f.length - 1 && IsVariadic!f) {
      // skip variadic tail argument, it always have a default value - empty one
    } else static if (DefaultArgStr!(f, i) == null) {
      return i + 1 - firstArgIsThis;
    }
  }
  return 0;
}();


/**
 * Wraps a native function in such a way that it can be called from PHP. All
 * type checking and conversion code is automatically generated. Result is
 * equivalent to using PHP_FUNCTION and ZEND_PARSE_PARAMETERS* macros. The main
 * difference is that type checking is much more stricter. Types have to match
 * exactly and PHP reference is treated as different type. No type coercion is
 * attempted and strict_types has no effect.
 *
 * With the template argument `frameless` set to true, the resulting function
 * can be called via frameless mechanism. It's PHP 8.4 feature for more
 * efficient function calls by avoiding allocation of PHP stack frames (hence
 * frameless). Arguments are passed directly in native function parameters
 * (which means in registers). Because our stricter type handling, we do not
 * need to care about one corner case when a scalar type is coerced to a
 * string, which have to be eventually cleared.
 *
 * Template argument firstArgIsThis indicates that the native function
 * represents PHP method and the first argument is $this. It isn't ordinary
 * argument on PHP side and instead is extracted from PHP stack frame.
 */
private void wrapFunc
  (alias f, alias bool frameless, alias bool firstArgIsThis, Args...)
  (Args phpArgs)
{
  static if (is(typeof(f) ParamTypes == __parameters)) {}

  // those two numbers are only for the php side excluding synthetic parameter for 'this' object
  enum numParams    = NumPHPParams!(f, firstArgIsThis);
  enum reqNumParams = ReqNumPHPParams!(f, firstArgIsThis);

  static if (frameless) {
    static assert (!firstArgIsThis);

    zval* return_value = phpArgs[0];

    pragma(inline, true)
    zval* zvalArgs(alias i)() {
      static if (i == 0) return phpArgs[1];
      else static if (i == 1) return phpArgs[2];
      else static if (i == 2) return phpArgs[3];
      else static assert(0, "frameless functions can handle at most 3 arguments");
    }

  } else {
    zend_execute_data* ex = phpArgs[0];
    zval* return_value = phpArgs[1];

    enum int ZEND_CALL_FRAME_SLOT = (zend_execute_data.sizeof + zval.sizeof - 1) / zval.sizeof;

    zval* zvalArgs = (cast(zval*) ex) + ZEND_CALL_FRAME_SLOT;
    size_t numArgs = ex.This.u2.num_args;

    if (numArgs < reqNumParams) {
      zend_wrong_parameters_count_error(reqNumParams, numParams);
      return;
    }

    static if (!IsVariadic!f) {
      if (numArgs > numParams) {
        zend_wrong_parameters_count_error(reqNumParams, numParams);
        return;
      }

    } else {
      zval* firstArg = zvalArgs;
      // This pointer holds memory for typed varargs just so it can be freed. We
      // need to put it there because in static loop over parameter types we
      // introduce new scope (see this: {{) to eliminate conflicts. As a result
      // if we put scope(exit) in inside of it, _efree get called immediately and
      // typed varargs are not valid when function `f` gets called.
      void* varargMemory;
      scope(exit) if (varargMemory != null) _efree(varargMemory);
    }
  }

  Tuple!(ParamTypes) args;

  // _ni = native argument index
  static foreach (_ni, PT; ParamTypes) {{ // <- beware: we are introducing a new scope here

    static if (_ni == 0 && firstArgIsThis) {
      static assert(is(PT == zend_object*));
      args[0] = ex.This.obj;

    } else {
      // 'i' refers to position of a PHP argument not an argument of native
      // function `f`. It excludes synthetic 'this'.
      enum i = _ni - firstArgIsThis;

      static if (frameless) {
        zval* arg = zvalArgs!i;
      } else {
        zval* arg = zvalArgs + i;
      }

      static if (!frameless && i >= reqNumParams) {
        zval _z;
        if (i >= numArgs) {
          static if (_ni == ParamTypes.length - 1 && IsVariadic!f) {
            // no need to do anything
          } else {
            _z = zval(DefaultArg!(f, _ni)); // using _ni intentionally
            arg = &_z;
          }
        }
      }

      // we need to check for attributes on parameter like this (extract list
      // of attributes and them pass them to some template, instead of
      // passing the parameter symbol directly) because it seems that
      // attributes attached to a symbol representing a function parameter
      // don't survive passing as template parameter
      alias attrs = __traits(getAttributes, ParamTypes[_ni .. _ni + 1]);

      static if (isIntegral!PT) {
        static if (is(PT == ulong)) {
          static assert(0, "cannot handle ulong arguments");
        }

        if (arg.type != Type.Long) {
          zend_argument_type_error(i+1, "expected int, %s given", typeNames[arg.type]);
          return;
        }

        long val = arg.lval;

        static if (!is(PT == long)) {
          if (val < PT.min || val > PT.max) {
            zend_argument_type_error(i+1, "int %d is out d of range [%d .. %d]", val, cast(long) PT.min, cast(long) PT.max);
            return;
          }
        }

        args[_ni] = cast(PT) val;

      } else static if (is(PT == double) || is(PT == float)) {
        if (arg.type == Type.Double) {
          args[_ni] = cast(PT) arg.dval;
        } else if (arg.type == Type.Long) {
          args[_ni] = cast(PT) arg.lval;
        } else {
          zend_argument_type_error(i+1, "expected double, %s given", typeNames[arg.type]);
          return;
        }

      } else static if (is(PT == bool)) {
        bool val;
        if (arg.type == Type.True) {
          val = true;
        } else if (arg.type == Type.False) {
          val = false;
        } else {
          zend_argument_type_error(i+1, "expected bool, %s given", typeNames[arg.type]);
          return;
        }
        args[_ni] = val;

      } else static if (is(PT == const(char)[]) || is(PT == const(X)[], X) && isIntegral!X) {
        enum storageClasses = __traits(getParameterStorageClasses, f, _ni);
        enum isScope(alias sc) = sc == "scope";
        static if (!anySatisfy!(isScope, storageClasses)) {
          static assert(0, "arrays have to be passed as scope parameters");
        }
        if (arg.type != Type.String) {
          zend_argument_type_error(i+1, "expected string, %s given", typeNames[arg.type]);
          return;
        }
        args[_ni] = cast(PT) arg.str.str[];

      } else static if (is(PT : const(zend_string)*)) {
        if (arg.type == Type.String) {
          args[_ni] = arg.str;
        } else if (isNullable!attrs && arg.type == Type.Null) {
          args[_ni] = null;
        } else {
          zend_argument_type_error(i+1, "expected string, %s given", typeNames[arg.type]);
          return;
        }

      } else static if (is(PT : const(HashTable)*)) {
        if (arg.type == Type.Array) {
          args[_ni] = arg.arr;
        } else if (isNullable!attrs && arg.type == Type.Null) {
          args[_ni] = null;
        } else {
          zend_argument_type_error(i+1, "expected array, %s given", typeNames[arg.type]);
          return;
        }

      } else static if (is(PT : const(zend_resource)*)) {
        if (arg.type == Type.Resource) {
          args[_ni] = arg.res;
        } else if (isNullable!attrs && arg.type == Type.Null) {
          args[_ni] = null;
        } else {
          zend_argument_type_error(i+1, "expected resource, %s given", typeNames[arg.type]);
          return;
        }

      } else static if (is(PT : const(zend_object)*)) {
        if (arg.type == Type.Object) {
          args[_ni] = arg.obj;
        } else if (isNullable!attrs && arg.type == Type.Null) {
          args[_ni] = null;
        } else {
          zend_argument_type_error(i+1, "expected object, %s given", typeNames[arg.type]);
          return;
        }

      } else static if (is(PT == zval*)) {
        args[_ni] = arg;

      } else static if (is(PT == zval)) {
        args[_ni] = *arg;

      } else static if (_ni == ParamTypes.length-1 && IsVariadic!f) {
        static if (is(PT : const(zval)[])) {
          args[_ni] = firstArg[arg - firstArg .. numArgs];

        } else static if (is(PT == VT[], VT)) {
          auto varargs = firstArg[arg - firstArg .. numArgs];
          foreach (ref zval z; varargs) {
            auto v = z.asType!(VT*);
            if (v == null) {
              zend_argument_type_error(i+1, "type mismatch");
              return;
            }
          }
          auto arr = emallocArray!VT(varargs.length);
          varargMemory = arr.ptr;

          foreach (vi, ref zval z; varargs) {
            auto v = z.asType!(VT*);
            arr[vi] = *v;
          }

          args[_ni] = arr;
        } else static assert(0);

      } else static if (is(PT == T*, T)) {

        static if (isPHPClass!T || isPHPClass_!attrs) {
          if (arg.type == Type.Object) {
            const(zend_class_entry)* ce = phpClassRegistry!T.class_entry;
            if (arg.obj.ce == ce)  {
              args[_ni] = phpClassRegistry!T.getNativeType(arg.obj);
            } else {
              zend_argument_type_error(i+1, "class type mismatch");
              return;
            }
          } else if (isNullable!attrs && arg.type == Type.Null) {
            args[_ni] = null;
          } else {
            zend_argument_type_error(i+1, "expected object of class ???, %s given", typeNames[arg.type]);
            return;
          }
        } else static if (isUserspaceClass!T) { // type T is proxy
          if (arg.type == Type.Object) {
            T* obj = __traits(parent, T).getNativeType(arg.obj);
            if (obj != null) {
              args[_ni] = obj;
            } else {
              zend_argument_type_error(i+1, "class type mismatch");
              return;
            }
          } else if (isNullable!attrs && arg.type == Type.Null) {
            args[_ni] = null;
          } else {
            zend_argument_type_error(i+1, "expected object of class ???, %s given", typeNames[arg.type]);
            return;
          }

        } else static if (isResource!T || isResource_!attrs) {
          if (arg.type == Type.Resource) {
            auto res = phpResourceRegistry!T.getNativeType(arg.res);
            if (res == null) {
              zend_argument_type_error(i+1, "expected %s resource", T.stringof.ptr);
              return;
            }
            args[_ni] = res;
          } else if (isNullable!attrs && arg.type == Type.Null) {
            args[_ni] = null;
          } else {
            zend_argument_type_error(i+1, "expected resource, %s given", typeNames[arg.type]);
            return;
          }

        } else {
          static assert(0, T.stringof);
        }

      } else {
        static assert(0, "cannot handle argument of type " ~ PT.stringof);
      }
    }
  }}


  alias RT = ReturnType!f;

  static if (!is(RT == void)) {
    RT retval;
  }

  static if (ExceptionsAllowed) {
    try {
      static if (is(RT == void)) {
        f(args.expand);
      } else {
        retval = f(args.expand);
      }

    } catch (PHPException e) {
      const(char)* msg = e._msg ? e._msg : (e.owner ? e.owner.ptr : "null");
      zend_throw_exception_ex(e.ce, 0, "%s (%.*s:%d)", msg, e.file.length, e.file.ptr, e.line);
      return;

    } catch (Exception e) {
      zend_throw_exception_ex(null, 0, "%.*s (%.*s:%d)", e.msg.length, e.msg.ptr, e.file.length, e.file.ptr, e.line);
      return;
    }

  } else {
    static if (is(RT == void)) {
      f(args.expand);
    } else {
      retval = f(args.expand);
    }
  }

  static if (is(RT == Try!TT, TT)) {
    if (retval.errorMessage) {
      zend_throw_exception_ex(null, 0, retval.errorMessage);
      return;
    }
  }

  static if (!is(RT == void) && !is(RT == Try!void)) {
    alias returnAttrs = __traits(getAttributes, f);
    *return_value = wrapResultToZval!(RT, returnAttrs)(retval);
  }
}


pragma(inline, true)
private zval wrapResultToZval(RT, returnAttrs...)(RT retval) {
    static if (is(RT == zval)) {
      return retval;

    } else static if (isIntegral!RT || is(RT == double) || is(RT == float) || is(RT == bool)) {
      return zval(retval);

    } else static if (is(RT == zend_string*) || is(RT == zend_resource*) || is(RT == zend_object*) || is(RT == HashTable*)) {
      if (isNullable!returnAttrs && retval == null) {
        return zval.Null();
      } else {
        return zval(retval);
      }

    } else static if (is(RT == T*, T)) {
      static if (isPHPClass!T || isPHPClass_!returnAttrs) {
        if (isNullable!returnAttrs && retval == null) {
          return zval.Null();
        } else {
          return zval(phpClassRegistry!T.getPHPType(retval));
        }
      } else static if (isResource!T || isResource_!returnAttrs) {
        if (isNullable!returnAttrs && retval == null) {
          return zval.Null();
        } else {
          return zval(phpResourceRegistry!T.wrap(retval));
        }
      } else static assert(0);

    } else static if (is(RT == Try!TT, TT)) {
      return wrapResultToZval!(TT, returnAttrs)(retval.result);

    } else {
      static assert(0, "cannot handle return value of type " ~ RT.stringof);
    }
}


private template ConvertCStringArgumentsToSlices(alias f) {
  static if (HasCStringArguments!f) {
    alias ConvertCStringArgumentsToSlices = ConvertCStrArgs!f;
  } else {
    alias ConvertCStringArgumentsToSlices = f;
  }
}

private enum isCString(T) = is(T : const(char)*);

private template HasCStringArguments(alias f) {
  static if (is(typeof(f) ParamTypes == __parameters)) {}
  enum HasCStringArguments = anySatisfy!(isCString, ParamTypes);
}

private template ConvertCStrArgs(alias f) {
  static if (is(typeof(f) ParamTypes == __parameters)) {}

  alias Signature = AliasSeq!("pragma(inline, true) void wrapper(");
  alias Body      = AliasSeq!("{ return f(");

  static foreach (i, PT; ParamTypes) {
    static if (isCString!PT) {
      // skip
    } else static if (is(PT : long) && i > 0 && isCString!(ParamTypes[i - 1])) {
      Signature = AliasSeq!(Signature, "scope ", typeof(*ParamTypes[i-1].init).stringof ~ "[] " ~ __traits(identifier, ParamTypes[i-1 .. i]), ", ") ;
      Body = AliasSeq!(Body, __traits(identifier, ParamTypes[i-1 .. i]) ~ ".ptr, " ~ "cast("~PT.stringof~")"~__traits(identifier, ParamTypes[i-1 .. i]) ~ ".length, ") ;
    } else {
      Signature = AliasSeq!(Signature, PT.stringof, " ", __traits(identifier, ParamTypes[i .. i+1]), ", ") ;
      Body = AliasSeq!(Body, __traits(identifier, ParamTypes[i .. i+1]) ~ ", ");
    }
  }
  Signature = AliasSeq!(Signature, ")");
  Body = AliasSeq!(Body, "); }");

  mixin(AliasSeq!(Signature, Body));
  alias ConvertCStrArgs = wrapper;
}



// we use array of size 256 to eliminate bounds check in error messages
private static immutable char*[256] typeNames = [
  Type.Undef     : "undef",
  Type.Null      : "null",
  Type.False     : "false",
  Type.True      : "true",
  Type.Long      : "int",
  Type.Double    : "double",
  Type.String    : "string",
  Type.Array     : "array",
  Type.Object    : "object",
  Type.Resource  : "resource",
  Type.Reference : "reference",
];






alias Result = zend_result;

enum zend_result {
  SUCCESS =  0,
  FAILURE = -1,    /* this MUST stay a negative number, or it may affect functions! */
}


enum Type : ubyte {
  Undef     = IS_UNDEF,
  Null      = IS_NULL,
  False     = IS_FALSE,
  True      = IS_TRUE,
  Long      = IS_LONG,
  Double    = IS_DOUBLE,
  String    = IS_STRING,
  Array     = IS_ARRAY,
  Object    = IS_OBJECT,
  Resource  = IS_RESOURCE,
  Reference = IS_REFERENCE,
}

enum : ubyte {
  IS_UNDEF     = 0,
  IS_NULL      = 1,
  IS_FALSE     = 2,
  IS_TRUE      = 3,
  IS_LONG      = 4,
  IS_DOUBLE    = 5,
  IS_STRING    = 6,
  IS_ARRAY     = 7,
  IS_OBJECT    = 8,
  IS_RESOURCE  = 9,
  IS_REFERENCE = 10,

  /* Fake types used only for type hinting.
   * These are allowed to overlap with the types below. */
  IS_CALLABLE =     12,
  IS_ITERABLE =     13,
  IS_VOID     =     14,
  IS_STATIC   =     15,
  IS_MIXED    =     16,
  IS_NEVER    =     17,

  /* internal types */
  IS_INDIRECT  =    12,
  IS_PTR       =    13,
  IS_ALIAS_PTR =    14,
  _IS_ERROR    =    15,

  /* used for casts */
  _IS_BOOL   =      18,
  _IS_NUMBER =      19,
}

enum IS_TYPE_REFCOUNTED  = 1<<0;
enum IS_TYPE_COLLECTABLE = 1<<1;



// === zval ===

/**
  zval structure:

  | value 8B | type 1B | type_flags 1B | extra 2 B | context dependent data 4 B

  `type` field (`zval.u1.v.type`) can be one value from `Types` enum but not all of
  them, only values from 0 to 10 in a common code.

  `type_flags` field (`zval.u1.v.type_flags`) is a bitmap with two possible bits:

   - IS_TYPE_REFCOUNTED:
     Indicates that the zval points to some string, array, object, resource or
     reference (ie. any structure starting with zend_refcounted_h struct) that
     is subject to refcounting. Interned strings are not refcounted.

   - IS_TYPE_COLLECTABLE:
     It can be set only when IS_TYPE_REFCOUNTED is also set.
     It indicates the given zval points to a structure (object, array,
     reference) with pointers to another objects. It's there purely for tracing
     GC.

  Some of this data is duplicated in zend_refcounted_h headers.


  zend_refcounted_h.u.type_info:

  | info 22 bits | flags 6 bits | type 4 bits |

  bits 0-3: type
  bit 4: not collectable (most likely inversion of IS_TYPE_COLLECTABLE for
    tracing gc to quickly filter out uninteresting objects)
    for example: persistent malloc'd array is not collectable (_zend_hash_init_int)
  bit 5: protected (internal usage, doesn't seem to be relevant)
  bit 6: immutable (interned string, immutable array)
  bit 7: persistent (string or array allocated using malloc, IS_OBJ_WEAKLY_REFERENCED)
  bit 8: persistent, but thread local (IS_STR_PERMANENT, IS_OBJ_DESTRUCTOR_CALLED)
  bit 9: IS_STR_VALID_UTF8, IS_OBJ_FREE_CALLED
  bits 10-29: address (used by GC)
  bits 30-31: color (for GC during marking)
*/
struct zval {
  pragma(inline, true):
  @nogc pure {

  union {
    long   lval;
    double dval;
    zend_refcounted  *counted;
    zend_string      *str;
    zend_array       *arr;
    zend_object      *obj;
    zend_resource    *res;
    /*
    zend_reference   *ref;
    zend_ast_ref     *ast;
    zval             *zv;
    */
    void             *ptr;
    /*
    zend_class_entry *ce;
    zend_function    *func;
    struct {
      uint32_t w1;
      uint32_t w2;
    } ww;
    */
  }

  static union u1_ {
    uint type_info;
    v_ v;
  }

  version (LittleEndian) {
    static struct v_ {
      Type    type;       /* active type */
      ubyte   type_flags;
      ushort  extra;     /* not further specified */
    }
  } else {
    static assert(0, "little endian only");
  }

  u1_ u1;

  static union u2_ {
    uint     next;                 /* hash collision chain */
    uint     cache_slot;           /* cache slot (for RECV_INIT) */
    uint     opline_num;           /* opline number (for FAST_CALL) */
    uint     lineno;               /* line number (for ast nodes) */
    uint     num_args;             /* arguments number for EX(This) */
    uint     fe_pos;               /* foreach position */
    uint     fe_iter_idx;          /* foreach iterator index */
    uint     property_guard;       /* single property guard */
    uint     constant_flags;       /* constant flags */
    uint     extra;                /* not further specified */
  }

  u2_ u2;

  void opAssign(zval x) nothrow pure {
    this.lval = x.lval;
    this.u1 = x.u1;
    // do not overwrite u2
  }

  this(T : long)(T x) nothrow {
    lval = x;
    u1.type_info = Type.Long;
  }

  this(T : double)(T x) nothrow {
    dval = x;
    u1.type_info = Type.Double;
  }

  this(T : bool)(T x) nothrow {
    u1.type_info = x ? Type.True : Type.False;
  }

  this(typeof(null) x) nothrow {
    u1.type_info = Type.Null;
  }

  static Null() nothrow {
    zval z;
    z.u1.type_info = Type.Null;
    return z;
  }

  this(zend_string* x) nothrow {
    str = x;
    u1.type_info = Type.String;
    u1.v.type_flags = x.isInterned ? 0 : IS_TYPE_REFCOUNTED;
  }

  this(HashTable* x) nothrow {
    arr = x;
    u1.type_info = Type.Array;
    u1.v.type_flags = IS_TYPE_REFCOUNTED | IS_TYPE_COLLECTABLE;
  }

  this(zend_object* x) nothrow {
    obj = x;
    u1.type_info = Type.Object;
    u1.v.type_flags = IS_TYPE_REFCOUNTED | IS_TYPE_COLLECTABLE;
  }

  this(zend_resource* x) nothrow {
    res = x;
    u1.type_info = Type.Resource;
    u1.v.type_flags = IS_TYPE_REFCOUNTED;
  }

  this(zval z) nothrow         { this = z; }
  this(const(zval)* z) nothrow { this = *z; }



  Type type() nothrow const pure @safe { return u1.v.type; }

  long* asLong() return       nothrow { return type == Type.Long ? &lval : null; }
  double* asDouble() return   nothrow { return type == Type.Double ? &dval : null; }
  const(bool)* asBool()       nothrow {
    immutable static bool[2] falseTrue = [false, true];
    if (type == Type.False) return &falseTrue[0];
    if (type == Type.True)  return &falseTrue[1];
    return null;
  }
  HashTable* asArray()        nothrow { return type == Type.Array ? arr : null; }
  zend_object* asObject()     nothrow { return type == Type.Object ? obj : null; }
  zend_string* asString()     nothrow { return type == Type.String ? str : null; }
  zend_resource* asResource() nothrow { return type == Type.Resource ? res : null; }

  auto asType(T: long*)()          => asLong();
  auto asType(T: double*)()        => asDouble();
  auto asType(T: bool*)()          => asBool();
  auto asType(T: HashTable*)()     => asArray();
  auto asType(T: zend_object*)()   => asObject();
  auto asType(T: zend_string*)()   => asString();
  auto asType(T: zend_resource*)() => asResource();

  static if (ExceptionsAllowed) {
    long toLong() return        { if (type != Type.Long)     throw new Exception("type error: cannot convert zval to long"); return lval; }
    double toDouble() return    { if (type != Type.Double)   throw new Exception("type error: cannot convert zval to double"); return dval; }
    bool toBool()               { if (type != Type.True && type != Type.False) throw new Exception("bad type: cannot convert zval to bool"); return type == Type.True; }
    HashTable* toArray()        { if (type != Type.Array)    throw new Exception("type error: cannot convert zval to array"); return arr; }
    zend_object* toObject()     { if (type != Type.Object)   throw new Exception("type error: cannot convert zval to object"); return obj; }
    //zend_string* toString()     { if (type != Type.String)   throw new Exception("bad type"); return str; }
    zend_resource* toResource() { if (type != Type.Resource) throw new Exception("type error: cannot convert zval to resource"); return res; }
  }

  bool isTrue()  { return type == Type.True; }
  bool isFalse() { return type == Type.False; }


  pure:
  // see Z_TYPE_INFO_REFCOUNTED
  bool isRefcounted() const nothrow { return zval.u1.v.type_flags != 0; }

  /// Increment refcount by one. Do not call on non-refcounted objects.
  void addRef()    nothrow { ++counted.gc.refcount; }
  /// Decrement refcount by one. Do not call on non-refcounted objects.
  void delRef()    nothrow { ++counted.gc.refcount; }
  /// Increment refcount by one if this zval points to a refcounted object.
  void tryAddRef() nothrow { if (isRefcounted()) addRef(); }
  /// Decrement refcount by one if this zval points to a refcounted object.
  void tryDelRef() nothrow { if (isRefcounted()) delRef(); }


  // This is there to supress warning about safety in gdc 14. We shouldn't use zvals as keys anyways.
  size_t toHash() const @safe pure nothrow { return lval; }

  }

  /*
  void toString(void delegate(const(char)[]) sink) {
    sink("zval(");

    auto tn = typeNames[type];
    auto len = strlen(tn);
    sink(tn[0 .. len]);
    sink(": ");

    char[32] buf;
    import core.stdc.stdio;
    len = sprintf(buf.ptr, "%ld", lval);
    sink(buf[0 .. len]);

    sink(")");
  }
  */
}

static assert(zval.sizeof == 16);








// === refcounted ===

struct zend_refcounted {
  zend_refcounted_h gc;
}


enum GC_TYPE_MASK     =  0x0000000f;
enum GC_FLAGS_MASK    =  0x000003f0;
enum GC_INFO_MASK     =  0xfffffc00;
enum GC_FLAGS_SHIFT   =  0;
enum GC_INFO_SHIFT    =  10;

enum GC_NULL         = (IS_NULL        | (GC_NOT_COLLECTABLE << GC_FLAGS_SHIFT));
enum GC_STRING       = (IS_STRING      | (GC_NOT_COLLECTABLE << GC_FLAGS_SHIFT));
enum GC_ARRAY        =  IS_ARRAY;
enum GC_OBJECT       =  IS_OBJECT;
enum GC_RESOURCE     = (IS_RESOURCE    | (GC_NOT_COLLECTABLE << GC_FLAGS_SHIFT));
enum GC_REFERENCE    = (IS_REFERENCE   | (GC_NOT_COLLECTABLE << GC_FLAGS_SHIFT));
//enum GC_CONSTANT_AST = (Type.IsConstantAst | (GC_NOT_COLLECTABLE << GC_FLAGS_SHIFT));

enum GC_NOT_COLLECTABLE   = (1<<4);
enum GC_PROTECTED         = (1<<5); /* used for recursion detection */
enum GC_IMMUTABLE         = (1<<6); /* can't be changed in place */
enum GC_PERSISTENT        = (1<<7); /* allocated using malloc */
enum GC_PERSISTENT_LOCAL  = (1<<8); /* persistent, but thread-local */

/* string flags */
enum IS_STR_CLASS_NAME_MAP_PTR = GC_PROTECTED;  /* refcount is a map_ptr offset of class_entry */
enum IS_STR_INTERNED           = GC_IMMUTABLE;  /* interned string */
enum IS_STR_PERSISTENT         = GC_PERSISTENT; /* allocated using malloc */
enum IS_STR_PERMANENT          = (1<<8);        /* relives request boundary */
enum IS_STR_VALID_UTF8         = (1<<9);        /* valid UTF-8 according to PCRE */

/* array flags */
enum IS_ARRAY_IMMUTABLE        = GC_IMMUTABLE;
enum IS_ARRAY_PERSISTENT       = GC_PERSISTENT;

/* object flags (zval.value->gc.u.flags) */
enum IS_OBJ_WEAKLY_REFERENCED = GC_PERSISTENT;
enum IS_OBJ_DESTRUCTOR_CALLED = (1<<8);
enum IS_OBJ_FREE_CALLED       = (1<<9);


/** for more info see zval */
struct zend_refcounted_h {
  @nogc nothrow pure:
  pragma(inline, true):

  uint refcount;
  union _u {
    uint type_info;
  }
  _u u;

  auto type()  const { return u.type_info & GC_TYPE_MASK; }
  auto flags() const { return u.type_info & GC_FLAGS_MASK; }
  auto info()  const { return u.type_info >> GC_INFO_SHIFT; }

  auto addRef()    { return ++refcount; }
  auto delRef()    { return --refcount; }
  void tryAddRef() { if (!(u.type_info & GC_IMMUTABLE)) { ++refcount; } }
  void tryDelRef() { if (!(u.type_info & GC_IMMUTABLE)) { --refcount; } }
}


@nogc nothrow {
  pragma(inline, true):
  private enum isRcType(T) = is(T : zend_string) || is(T : zend_object) || is(T : HashTable) || is(T : zend_resource);

  /// Modify refcount without checking if it's allowed. Can be called only on non-immutable objects/arrays/string.
  auto addRef(T)(ref T x)    if (isRcType!T) { return x.gc.addRef(); }
  auto addRef(T)(T* x)       if (isRcType!T) { return x.gc.addRef(); }
  auto delRef(T)(ref T x)    if (isRcType!T) { return x.gc.delRef(); }
  auto delRef(T)(T* x)       if (isRcType!T) { return x.gc.delRef(); }
  /// Modify refcount only when allowed. Safe but little bit slower.
  void tryAddRef(T)(ref T x) if (isRcType!T) { return x.gc.tryAddRef(); }
  void tryAddRef(T)(T* x)    if (isRcType!T) { return x.gc.tryAddRef(); }
  void tryDelRef(T)(ref T x) if (isRcType!T) { return x.gc.tryDelRef(); }
  void tryDelRef(T)(T* x)    if (isRcType!T) { return x.gc.tryDelRef(); }

  /// Safely increment refcount and return zval itself.
  zval* bump(zval* x)    { x.tryAddRef(); return x; }
  /// Safely decrement refcount and destroy object when refcount reaches zero
  void release(zval* x)    { zval_ptr_dtor(x); }
  void release(ref zval x) { zval_ptr_dtor(&x); }

  /// Safely increment refcount and return the object itself.
  ref T bump(T)(ref T x) { x.tryAddRef(); return x;}
  T* bump(T)(T* x)       { x.tryAddRef(); return x;}

  /// Safely decrement refcount and destroy object when refcount reaches zero
  // see zend_string_release
  void release(zend_string* s) {
    if (!s.isInterned) {
      if (delRef(s) == 0) {
        pefree(s, !!(s.gc.flags & IS_STR_PERSISTENT));
      }
    }
  }

  // see zend_array_release
  void release(zend_array* array) {
    if (!(array.gc.flags & IS_ARRAY_IMMUTABLE)) {
      if (delRef(array) == 0) {
        zend_array_destroy(array);
      }
    }
  }

  // see zend_object_release
  void release(zend_object* obj) {
    // TODO
    assert(0);
  }
}



// === string ===

alias String     = zend_string;
alias ZendString = zend_string;

struct zend_string {
  pragma(inline, true):
  @nogc nothrow:

  zend_refcounted_h gc;
  ulong        h;                /* hash value */
  size_t       len;
  char[1]      val_;

  alias length = len;

  // val() to be more like PHP, ptr() to be more like D
  inout(char)*  val() return inout pure { return val_.ptr; }
  inout(char)*  ptr() return inout pure { return val_.ptr; }
  inout(char)[] str() return inout pure { return val_.ptr[0 .. len]; }

  // see ZSTR_IS_INTERNED
  bool isInterned() const pure { return (gc.u.type_info & IS_STR_INTERNED) != 0; }

  enum ZEND_MM_ALIGNMENT = 8;
  enum ZEND_MM_ALIGNMENT_MASK = ~(ZEND_MM_ALIGNMENT - 1);
  alias ZEND_MM_ALIGNED_SIZE = (size) => (size + ZEND_MM_ALIGNMENT - 1) & ZEND_MM_ALIGNMENT_MASK;

  // see zend_string_alloc
  static zend_string* alloc(size_t len, bool persistent = false) {
    zend_string *ret = cast(zend_string*) pemalloc(ZEND_MM_ALIGNED_SIZE(zend_string.sizeof + len + 1), persistent);

    ret.gc.refcount = 1;
    ret.gc.u.type_info = GC_STRING | ((persistent ? IS_STR_PERSISTENT : 0) << GC_FLAGS_SHIFT);
    ret.h = 0;
    ret.len = len;
    ret.val[len] = 0; // null terminated
    return ret;
  }

  // see zend_string_init
  pragma(inline)
  static zend_string* copy(const(char)[] str, bool persistent = false) {
    auto s = zend_string.alloc(str.length, persistent);
    s.str[] = str[];
    return s;
  }

  static zend_string* copy(const(ubyte)[] str, bool persistent = false) {
    return copy(cast(const(char)[]) str, persistent);
  }

  bool equalContent(const(zend_string)* str) const {
    return len == str.len && !memcmp(this.val, str.val, len);
  }

  bool equals(const(zend_string)* str) const {
    return &this == str || (len == str.len && !memcmp(this.val, str.val, len));
  }

  // see SEPARATE_STRING
  // TODO zend_string_separate differs
  pragma(inline)
  zend_string* separate() {
    if (gc.refcount > 1) {
      auto str = zend_string.copy(this.str);
      this.delRef();
      return str;
    } else {
      return &this;
    }
  }

  // TODO will PHP try to free immutable string and crash?
  static immutable(zend_string)* staticString(alias string str)() {
    enum size = ZEND_MM_ALIGNED_SIZE(zend_string.sizeof + str.length + 1);
    immutable static mem = makeStaticString!size(str);
    return &mem.str;
  }

  private static auto makeStaticString(alias size)(const(char)[] str) {
    struct Mem { zend_string str; char[size - zend_string.sizeof] val; }
    static assert(zend_string.sizeof == 24);
    static assert(Mem.val.offsetof == 24);
    // see zend_inline_hash_func
    ulong hash(const(char)[] str) {
      ulong h = 5381;
      foreach (ubyte c; str) {
        h = h * 33 + c;
      }
      return h | 0x8000000000000000UL;
    }
    Mem mem;
    mem.str.gc.refcount = 0;
    mem.str.gc.u.type_info = GC_STRING | ((IS_STR_PERSISTENT | IS_STR_INTERNED) << GC_FLAGS_SHIFT);
    mem.str.len = str.length;
    mem.str.h = hash(str);
    mem.val[0 .. str.length] = str[];
    mem.val[str.length] = 0;
    return mem;
  }

  size_t toHash() const nothrow {
    return h ? h : zend_hash_func(ptr, len);
  }
}





// === array ===

struct Bucket {
  zval         val;
  ulong        h;                /* hash value (or numeric index)   */
  zend_string* key;              /* string key or NULL for numerics */
}

alias zend_array = HashTable;
alias ZendArray  = HashTable;


// Struct for array key (string or int)
// It mimics zval interface and can be implicitly converted to one.
struct Key {
  nothrow @nogc pragma(inline, true):
  zend_string* str;
  long lval;

  this(long k) { lval = k; }
  this(zend_string* s) { str = s; }

  auto type() => str ? Type.String : Type.Long;

  // implicitly convertable to zval
  private zval toZval() => str ? zval(str) : zval(lval);
  alias this = toZval;
}


// packed arrays may contain holes
struct HashTable {
  pragma(inline, true):

  zend_refcounted_h gc;
  union u_ {
    struct v_ {
        ubyte    flags;
        ubyte    _unused;
        ubyte    nIteratorsCount;
        ubyte    _unused2;
    }
    v_ v;
    uint flags;
  };
  u_ u;
  uint          nTableMask;
  union {
    uint    *arHash;   /* hash table (allocated above this pointer) */
    Bucket  *arData;   /* array of hash buckets */
    zval    *arPacked; /* packed array of zvals */
  }
  uint        nNumUsed;
  uint        nNumOfElements;
  uint        nTableSize;
  uint        nInternalPointer;
  long        nNextFreeElement;
  void*       pDestructor;

  size_t length()      const @nogc nothrow pure { return nNumOfElements; }
  bool isPacked()      const @nogc nothrow pure { return (u.flags & (1<<2)) != 0; }
  bool isUnitialized() const @nogc nothrow pure { return (u.flags & (1<<3)) != 0; }
  bool hasStaticKeys() const @nogc nothrow pure { return (u.flags & (1<<4)) != 0; }
  bool hasEmptyInd()   const @nogc nothrow pure { return (u.flags & (1<<5)) != 0; }
  bool hasHoles()      const @nogc nothrow pure { return nNumUsed != nNumOfElements; }


  static HashTable* alloc(size_t capacity = 1, bool packed = false) @nogc nothrow {
    auto ht = _zend_new_array(cast(uint) capacity);
    if (packed) {
      zend_hash_real_init_packed(ht);
    } else {
      zend_hash_real_init_mixed(ht);
    }
    return ht;
  }


  // see SEPARATE_ARRAY
  HashTable* separate() {
    if (unlikely(gc.refcount > 1)) {
      auto arr = zend_array_dup(&this);
      this.tryDelRef();
      return arr;
    } else {
      return &this;
    }
  }

  static HashTable* copy(T)(T[] arr) if (isIntegral!T || is(T == float) || is(T == double) || is(T == bool) || is(T : const(char)[])) {
    auto ht = _zend_new_array(cast(uint) arr.length);
    zend_hash_real_init_packed(ht);
    return ht.fillPacked(arr);
  }

  // see ZEND_HASH_FILL_PACKED
  auto fillPacked(R)(R range) {
    assert(isPacked());

    zval *val = arPacked + nNumUsed;
    uint idx = nNumUsed;

    foreach (x; range) {
      *val = zval(x);
      val++;
      idx++;
    }

    nNumOfElements += idx - nNumUsed;
    nNumUsed = idx;
    nNextFreeElement = idx;
    nInternalPointer = 0;
    return &this;
  }


  @nogc nothrow {
    inout(zval)* get(long key)                inout { return cast(inout(zval)*) zend_hash_index_find(&this, key); }
    inout(zval)* get(scope const(char)[] key) inout { return cast(inout(zval)*) zend_hash_str_find(&this, key.ptr, key.length); }
    inout(zval)* get(zend_string* key)        inout { return cast(inout(zval)*) zend_hash_find(&this, key); }
    inout(zval)* get(Key key)                 inout { return cast(inout(zval)*) key.str ? get(key.str) : get(key.lval); }
    alias opBinaryRight(string op : "in") = get;
    alias opIndex = get;

    zval* set(long key, zval* val)                    { return zend_hash_index_update(&this, key, val); }
    zval* set(scope const(char)[] key, zval* val)     { return zend_hash_str_update(&this, key.ptr, key.length, val); }
    zval* set(zend_string* key, zval* val)            { return zend_hash_update(&this, key, val); }
    zval* set(Key key, zval* val)                     { return key.str ? set(key.str, val) : set(key.lval, val); }
    zval* opIndexAssign(zval* val, long key)          { return zend_hash_index_update(&this, key, val); }
    zval* opIndexAssign(zval* val, const(char)[] key) { return zend_hash_str_update(&this, key.ptr, key.length, val); }
    zval* opIndexAssign(zval* val, zend_string* key)  { return zend_hash_update(&this, key, val); }

  }

  void append(scope zval* value) @nogc nothrow {
    zend_hash_next_index_insert(&this, value);
  }


  static struct Iterator(alias string what, alias bool _constTable) {
    @nogc nothrow:
    static if (_constTable) {
      alias HT = const(HashTable);
      alias Z = const(zval);
      alias K = const(Key);
    } else {
      alias HT = HashTable;
      alias Z = zval;
      alias K = Key;
    }

    private HT* ht;
    private zval* pos, end;
    private size_t step;
    private long idx = 0;

    this(HT* ht) {
      this.ht = ht;
      init();
    }

    private void init() {
      step = ht.isPacked ? 1 : 2;
      pos = cast(zval*) ht.arPacked;
      end = cast(zval*) (ht.arPacked + ht.nNumUsed * step);
      while (pos < end && pos.type == Type.Undef) {
        pos += step;
        idx++;
      }
    }
    bool empty() {
      return pos >= end;
    }
    auto front() {
      // pos points to start of a bucket (starting with zval) for non packed
      // array or naked zval for packed ones
      static if (what == "v") {
        return cast(Z*) pos;

      } else static if (what == "k") {
        Bucket* b = cast(Bucket*) pos;
        K key = ht.isPacked ? Key(idx) : (b.key ? Key(b.key) : Key(b.h));
        return key;

      } else static if (what == "kv") {
        struct KV { K key; Z* value; }
        if (ht.isPacked) {
          return KV(Key(idx), pos);
        } else {
          Bucket* b = cast(Bucket*) pos;
          K k = b.key ? Key(b.key) : Key(b.h);
          return KV(k, pos);
        }

      } else static assert(0);

    }
    void popFront() {
      do {
        pos += step;
        idx++;
      } while (pos < end && pos.type == Type.Undef);
    }
  }


  // these functions are templated to prevent generating and compiling unused variants
  auto byValue()()          { return Iterator!("v", false)(&this); }
  auto byValue()() const    { return Iterator!("v", true)(&this); }
  auto byKey()()            { return Iterator!("k", false)(&this); }
  auto byKey()() const      { return Iterator!("k", true)(&this); }
  /// iterate over `struct KV { zval key; zval* value; }`
  auto byKeyValue()()       { return Iterator!("kv", false)(&this); }
  /// iterate over `struct KV { const(zval) key; const(zval)* value; }`
  auto byKeyValue()() const { return Iterator!("kv", true)(&this); }



  //int opApply(scope int delegate(zval, zval*) dg) {
  //  return typed.opApply!(zval, zval*)(dg);
  //}


  /**
   * This helper function allows you to iterate over PHP array in completely
   * type safe manner. For example
   *
   * ```
   * foreach (int k, int v; ht.typed) {}
   * ```
   *
   * this snippet iterates over array where both keys and values are signed
   * integers. Types and ranges are checked and when there's a mismatch (eg.
   * string key or object value) an exception is thrown. No type conversions
   * are performed.
   *
   * Please keep in mind that PHP in its infinite wisdom converts string keys
   * with numerical values to integers. As result it might be preferable to
   * iterate like this
   *
   * ```
   * foreach (zval k, int v; ht.typed) {}
   * ```
   *
   * and manually check if a key is a string, instead of
   *
   * ```
   * foreach (scope const(char)[] k, int v; ht.typed) {}
   * ```
   *
   * where you will be graced with exception every time string key happens to
   * be numerical. (Or use `typedConvertKeys` method instead.)
   *
   * Valid types for keys are: any integral type, const(char)[],
   * const(ubyte)[], zend_string*, zval and Key.
   */
  alias typed = _typed!false;

  /**
   * Same as `typed` but convert integer keys to const(char)[]. In that
   * case be sure to declare keys as `scope` and never retain reference to any
   * key as they might be temporary values which will be overwritten in
   * the very next loop iteration.
   */
  alias typedConvertKeys = _typed!true;


  private auto _typed(alias bool convertKeys)() @nogc inout {
    struct Iter {
      HashTable* ht;

      int opApply(V)(scope int delegate(ref V) dg) {
        if (ht.isPacked) {
          long i = 0;
          for (zval* z = ht.arPacked; z < ht.arPacked + ht.nNumUsed; z++, i++) {
            if (z.type == Type.Undef) continue;
            V v = extractValue!V(z);
            int result = dg(v);
            if (result) return result;
          }

        } else {
          for (Bucket* b = ht.arData; b < ht.arData + ht.nNumUsed; b++) {
            if (b.val.type == Type.Undef) continue;
            V v = extractValue!V(&b.val);
            int result = dg(v);
            if (result) return result;
          }
        }
        return 0;
      }

      int opApply(K, V)(scope int delegate(ref K, ref V) dg) {
        char[32] buf = void;

        if (ht.isPacked) {
          long i = 0;
          for (zval* z = ht.arPacked; z < ht.arPacked + ht.nNumUsed; z++, i++) {
            if (z.type == Type.Undef) continue;
            K k = extractPackedKey!(K, convertKeys)(i, buf[]);
            V v = extractValue!V(z);
            int result = dg(k, v);
            if (result) return result;
          }

        } else {
          for (Bucket* b = ht.arData; b < ht.arData + ht.nNumUsed; b++) {
            if (b.val.type == Type.Undef) continue;
            K k = extractKey!(K, convertKeys)(b, buf[]);
            V v = extractValue!V(&b.val);
            int result = dg(k, v);
            if (result) return result;
          }
        }
        return 0;
      }
    }
    return inout(Iter)(&this);
  }
}


private T extractKey(T, alias convertKeys)(scope return Bucket* b, scope char[] buf) @nogc {
  static if (isIntegral!T) {
    if (b.key != null) {
      throw new Exception("int key expected");
    }
    long k = b.h;
    static if (is(T == ulong)) assert(0, "cannot handle unsigned longs");
    static if (!is(T == long)) {
      if (k < T.min || k > T.max) {
        throw new Exception("int key out of range");
      }
    }
    return cast(T) k;

  } else static if (is(T : zend_string*)) {
    if (b.key == null) {
      throw new Exception("string key expected");
    }
    return b.key;

  } else static if (is(T : const(char)[]) || is(T : const(ubyte)[])) {
    if (b.key == null) {
      static if (!convertKeys) {
        throw new Exception("string key expected");
      } else {
        import core.stdc.stdio;
        auto l = snprintf(buf.ptr, buf.length, "%ld", b.h);
        return buf[0 .. l];
      }
    }
    return cast(T) b.key.str();

  } else static if (is(T == zval) || is(T == Key)) {
    return b.key == null ? T(b.h) : T(b.key);

  } else static assert(0, "cannot handle argument of type " ~ T.stringof);
}

private T extractPackedKey(T, alias convertKeys)(long k, scope char[] buf) @nogc {
  static if (isIntegral!T) {
    static if (!is(T == long)) {
      if (k < T.min || k > T.max) {
        throw new Exception("int key out of range");
      }
    }
    return cast(T) k;

  } else static if (is(T : const(char)[]) || is(T : const(ubyte)[])) {
    static if (!convertKeys) {
      throw new Exception("string key expected");
    } else {
      import core.stdc.stdio;
      auto l = snprintf(buf.ptr, buf.length, "%ld", k);
      return buf[0 .. l];
    }

  } else static if (is(T : zend_string*)) {
    throw new Exception("string key expected");

  } else static if (is(T == zval) || is(T == Key)) {
    return T(k);

  } else static assert(0, "cannot handle argument of type " ~ T.stringof);
}


private T extractValue(T)(scope return zval* z) @nogc {
  static if (isIntegral!T) {
    static if (is(T == ulong)) assert(0, "cannot handle unsigned longs");
    if (z.type != Type.Long) { throw new Exception("int value expected"); }
    long val = z.lval;
    static if (!is(T == long)) {
      if (val < T.min || val > T.max) {
        throw new Exception("int value out of range");
      }
    }
    return cast(T) val;
  } else static if (is(T == float) || is(T == double)) {
    if (z.type == Type.Double) return cast(T) z.dval;
    if (z.type == Type.Long)   return cast(T) z.lval;
    throw new Exception("double value expected");
  } else static if (is(T == bool)) {
    if (z.type == Type.True) return true;
    if (z.type == Type.False) return false;
    throw new Exception("bool value expected");
  } else static if (is(T : const(char)[])) {
    if (z.type != Type.String) { throw new Exception("string value expected"); }
    return z.str.str();
  } else static if (is(T : zend_string*)) {
    if (z.type != Type.String) { throw new Exception("string value expected"); }
    return z.str;
  } else static if (is(T : HashTable*)) {
    if (z.type != Type.Array) { throw new Exception("array value expected"); }
    return z.arr;
  } else static if (is(T : zend_object*)) {
    if (z.type != Type.Object) { throw new Exception("object value expected"); }
    return z.obj;
  } else static if (is(T : zval*)) {
    return z;
  } else static if (is(T : zval)) {
    return *z;
  } else static assert(0, "cannot handle iterating over "~T.stringof);
}



// === classes and objects ===

alias ZendObject = zend_object;

struct zend_object {
  @nogc nothrow:

  zend_refcounted_h gc;
  uint              handle;
  static if (PHPVersion >= 84) {
  uint              extra_flags; /* OBJ_EXTRA_FLAGS() */ // since 8.4
  }
  zend_class_entry* ce;
  const(zend_object_handlers)* handlers;
  HashTable*       properties;
  zval[1]          properties_table;

  pragma(inline, true)
  zval* readProperty(const(char)[] propName, zval* rv) {
    return zend_read_property(ce, &this, propName.ptr, propName.length, false, rv);
  }

  // TODO
  /*
  pragma(inline, true)
  T readProperty(alias propName, T)() {
    static str = staticString!propName;
    zval tmp;
    auto res = zend_read_property_ex(ce, &this, str, false, &tmp);
    return res.toType!T;
  }
  */

  pragma(inline, true)
  void writeProperty(const(char)[] propName, zval* value) {
    zend_update_property(ce, &this, propName.ptr, propName.length, value);
  }

  zval* callMethod(Args...)(const(char)[] methodName, Args args, zval* rv) {
    zend_fcall_info fci;
    fci.size = fci.sizeof;
    fci.object = &this;

    auto fn = zend_string.copy(methodName);
    scope(exit) fn.release();
    fci.function_name = zval(fn);

    callFCCWithArgs(fci, null, args, rv); // may throw
    return rv;
  }

  size_t toHash() const nothrow @safe pure {
    return (cast(size_t) &this) ^ handle;
  }
}

// Prepares arguments and updates fci fields that have something to do with
// arguments and return values. Doesn't touch anything else. That must be
// prepared before this helper is called.
// Handles argument refcounting and rethrowing exceptions from PHP to native
// code.
private void callFCCWithArgs(Args...)(zend_fcall_info* fci, zend_fcall_info_cache* fcc, Args args, zval* rv) {
  zval[args.length] zvalArgs;

  static foreach (i, arg; args) {
    static if (isRefcountedType!(typeof(arg))) {
      bump(arg);
      scope(exit) release(arg);
    }
    zvalArgs[i] = zval(arg);
  }

  fci.retval = rv;
  fci.params = zvalArgs.ptr;
  fci.param_count = args.length;
  fci.named_params = null;

  auto res = zend_call_function(fci, fcc);
  if (res != Result.SUCCESS || rv.type == Type.Undef) {
    // TODO what to do?
  }

  auto ex = currentException();
  if (unlikely(ex != null)) {
    return handleException(ex);
  }
}

static if (ExceptionsAllowed)
pragma(inline, false) @hidden
private void handleException(zend_object* ex) {
  zval tmp = zval(ex);
  zend_string* msg = zval_get_string_func(&tmp);
  zend_clear_exception();
  throw new PHPException(msg.ptr, ex.ce, msg);
}


enum {
  BP_VAR_R        = 0,
  BP_VAR_W        = 1,
  BP_VAR_RW        = 2,
  BP_VAR_IS        = 3,
  BP_VAR_FUNC_ARG  =  4,
  BP_VAR_UNSET    = 5,
}

enum ZEND_ACC_PUBLIC                 = (1 <<  0);
enum ZEND_ACC_PROTECTED              = (1 <<  1);
enum ZEND_ACC_PRIVATE                = (1 <<  2);
enum ZEND_ACC_FINAL                  = (1 <<  5);
enum ZEND_ACC_USE_GUARDS             = (1 << 11);


struct zend_class_entry {
  char type;
  zend_string *name;
  /* class_entry or string depending on ZEND_ACC_LINKED */
  union {
    zend_class_entry *parent;
    zend_string *parent_name;
  };
  int refcount;
  uint ce_flags;

	int default_properties_count;
	int default_static_members_count;
	zval *default_properties_table;
	zval *default_static_members_table;
  void* static_members_table__ptr; //ZEND_MAP_PTR_DEF(zval *, static_members_table);
	HashTable function_table;
	HashTable properties_info;
	HashTable constants_table;

  void* mutable_data__ptr; //ZEND_MAP_PTR_DEF(zend_class_mutable_data*, mutable_data);
  void* inheritance_cache; //zend_inheritance_cache_entry *inheritance_cache;

  void* properties_info_table; //struct _zend_property_info **properties_info_table;

	zend_function *constructor;
	zend_function *destructor;
	zend_function *clone;
	zend_function *__get;
	zend_function *__set;
	zend_function *__unset;
	zend_function *__isset;
	zend_function *__call;
	zend_function *__callstatic;
	zend_function *__tostring;
	zend_function *__debugInfo;
	zend_function *__serialize;
	zend_function *__unserialize;

  static if (PHPVersion >= 83)
	const(zend_object_handlers)* default_object_handlers;

	/* allocated only if class implements Iterator or IteratorAggregate interface */
	//zend_class_iterator_funcs* iterator_funcs_ptr;
	void* iterator_funcs_ptr;
	/* allocated only if class implements ArrayAccess interface */
	//zend_class_arrayaccess_funcs *arrayaccess_funcs_ptr;
	void *arrayaccess_funcs_ptr;

	/* handlers */
	union {
		zend_object* function(zend_class_entry *class_type) create_object;
		int function(zend_class_entry *iface, zend_class_entry *class_type) interface_gets_implemented; /* a class implements this interface */
	};
	zend_object_iterator *function(zend_class_entry *ce, zval *object, int by_ref) get_iterator;
	zend_function *function(zend_class_entry *ce, zend_string* method) get_static_method;

	/* serializer callbacks */
	int function(zval *object, ubyte **buffer, size_t *buf_len, zend_serialize_data *data) serialize;
	int function(zval *object, zend_class_entry *ce, const(ubyte) *buf, size_t buf_len, zend_unserialize_data *data) unserialize;

	uint num_interfaces;
	uint num_traits;
  static if (PHPVersion >= 84)
	uint num_hooked_props;
  static if (PHPVersion >= 84)
	uint num_hooked_prop_variance_checks;

	/* class_entry or string(s) depending on ZEND_ACC_LINKED */
	union {
		zend_class_entry **interfaces;
		zend_class_name *interface_names;
	};

	//zend_class_name *trait_names;
	void *trait_names;
	//zend_trait_alias **trait_aliases;
	void *trait_aliases;
	//zend_trait_precedence **trait_precedences;
	void *trait_precedences;
	HashTable *attributes;

	uint enum_backing_type;
	HashTable *backed_enum_table;

  static if (PHPVersion >= 84)
	zend_string *doc_comment;

	static union _info {
		static struct _user {
			zend_string *filename;
			uint line_start;
			uint line_end;
      static if (PHPVersion <= 83)
      zend_string *doc_comment;
		}
    _user user;
		static struct _internal {
			const(zend_function_entry) *builtin_functions;
			zend_module_entry *_module;
		}
    _internal internal;
	}
  _info info;
}

struct zend_function;
struct zend_object_iterator;
struct zend_serialize_data;
struct zend_unserialize_data;
struct zend_class_name;

// see Zend/zend_object_handlers.h (it's actually documented)
alias zend_object_read_property_t = zval *function(zend_object *object, zend_string *member, int type, void **cache_slot, zval *rv);
alias zend_object_read_dimension_t = zval *function(zend_object *object, zval *offset, int type, zval *rv);
alias zend_object_write_property_t = zval *function(zend_object *object, zend_string *member, zval *value, void **cache_slot);
alias zend_object_write_dimension_t = void function(zend_object *object, zval *offset, zval *value);
alias zend_object_get_property_ptr_ptr_t = zval *function(zend_object *object, zend_string *member, int type, void **cache_slot);
alias zend_object_has_property_t = int function(zend_object *object, zend_string *member, int has_set_exists, void **cache_slot);
alias zend_object_has_dimension_t = int function(zend_object *object, zval *member, int check_empty);
alias zend_object_unset_property_t = void function(zend_object *object, zend_string *member, void **cache_slot);
alias zend_object_unset_dimension_t = void function(zend_object *object, zval *offset);
alias zend_object_get_properties_t = HashTable *function(zend_object *object);
alias zend_object_get_debug_info_t = HashTable *function(zend_object *object, int *is_temp);
alias zend_object_get_properties_for_t = zend_array *function(zend_object *object, /*zend_prop_purpose*/ int purpose);
alias zend_object_get_method_t = zend_function *function(zend_object **object, zend_string *method, const(zval) *key);
alias zend_object_get_constructor_t = zend_function *function(zend_object *object);
alias zend_object_free_obj_t = void function(zend_object *object);
alias zend_object_dtor_obj_t = void function(zend_object *object);
alias zend_object_clone_obj_t = zend_object* function(zend_object *object);
alias zend_object_get_class_name_t = zend_string *function(const(zend_object) *object);
alias zend_object_compare_t = int function(zval *object1, zval *object2);
alias zend_object_cast_t = zend_result function(zend_object *readobj, zval *retval, int type);
alias zend_object_count_elements_t = zend_result function(zend_object *object, zend_long *count);
alias zend_object_get_closure_t = zend_result function(zend_object *obj, zend_class_entry **ce_ptr, zend_function **fptr_ptr, zend_object **obj_ptr, bool check_only);
alias zend_object_get_gc_t = HashTable *function(zend_object *object, zval **table, int *n);
alias zend_object_do_operation_t = zend_result function(ubyte opcode, zval *result, zval *op1, zval *op2);

struct zend_object_handlers {
	/* offset of real object header (usually zero) */
	int										offset;
	/* object handlers */
	zend_object_free_obj_t					free_obj;             /* required */
	zend_object_dtor_obj_t					dtor_obj;             /* required */
	zend_object_clone_obj_t					clone_obj;            /* optional */
	zend_object_read_property_t				read_property;        /* required */
	zend_object_write_property_t			write_property;       /* required */
	zend_object_read_dimension_t			read_dimension;       /* required */
	zend_object_write_dimension_t			write_dimension;      /* required */
	zend_object_get_property_ptr_ptr_t		get_property_ptr_ptr; /* required */
	zend_object_has_property_t				has_property;         /* required */
	zend_object_unset_property_t			unset_property;       /* required */
	zend_object_has_dimension_t				has_dimension;        /* required */
	zend_object_unset_dimension_t			unset_dimension;      /* required */
	zend_object_get_properties_t			get_properties;       /* required */
	zend_object_get_method_t				get_method;           /* required */
	zend_object_get_constructor_t			get_constructor;      /* required */
	zend_object_get_class_name_t			get_class_name;       /* required */
	zend_object_cast_t						cast_object;          /* required */
	zend_object_count_elements_t			count_elements;       /* optional */
	zend_object_get_debug_info_t			get_debug_info;       /* optional */
	zend_object_get_closure_t				get_closure;          /* optional */
	zend_object_get_gc_t					get_gc;               /* required */
	zend_object_do_operation_t				do_operation;         /* optional */
	zend_object_compare_t					compare;              /* required */
	zend_object_get_properties_for_t		get_properties_for;   /* optional */
}


/** Annotation for function parameters mapped to a pointer types (PHP classes,
 *  resources, HashTables, Strings) that can be null. */
struct nullable {}

/** annotation for structs that are on the PHP side mapped to classes,
 * resources or FFI objects */
struct phpClass {}
struct phpResource {}
struct FFI {}
struct userspaceClass {}
struct phpConstant {}

private template hasAttribute(T, A) {
  enum _is(alias a) = is(a == A);
  enum hasAttribute = anySatisfy!(_is, __traits(getAttributes, T));
}
private template hasAttribute_(A, Attrs...) {
  enum _is(alias a) = is(a == A);
  enum hasAttribute_ = anySatisfy!(_is, Attrs);
}
// not private to be visible in mixin template `mod`
enum isPHPClass(T) = hasAttribute!(T, phpClass);
enum isResource(T) = hasAttribute!(T, phpResource);
enum isFFIType(T)  = hasAttribute!(T, FFI);
enum isUserspaceClass(T) = hasAttribute!(T, userspaceClass);
enum isPHPConstant(alias sym) = hasAttribute_!(__traits(getAttributes, sym), phpConstant);

private enum isPHPClass_(attrs...) = hasAttribute_!(phpClass, attrs);
private enum isResource_(attrs...) = hasAttribute_!(phpResource, attrs);
//private enum isFFIType_(attrs...)  = hasAttribute_!(FFI, attrs);
private enum isNullable(attrs...)  = hasAttribute_!(nullable, attrs);

enum isRefcountedType(T) =
  is(T : const(HashTable)*) ||
  is(T : const(zend_object)*) ||
  is(T : const(zend_string)*) ||
  is(T : const(zend_resource)*) ||
  is(T : const(zval)) ||
  is(T : const(zval)*);
  // TODO iterator? (it contains zend_object_iterator which is a class object)




zend_class_entry* registerClass(T)() {
  return phpClassRegistry!T.register();
}

template phpClassRegistry(T) {
  @nogc nothrow:

  static __gshared zend_class_entry *class_entry;
  static __gshared zend_object_handlers handlers;

  /**
   * Returns: The class entry that can be further modified
   */
  zend_class_entry* register() {
    static assert(is(T == struct), "PHP class has to be implemented as a native struct (not a class)");
    static assert(is(typeof(T.tupleof[$-1]) : const(zend_object)), "last field of a struct implementing PHP class has to be zend_object");

    enum className = __traits(identifier, T);
    enum methods = PublicMethods!T;

    immutable static zend_function_entry[methods.length + 1] functions = [MakeMethods!(T, methods)];

    handlers = std_object_handlers;
    handlers.offset = T.tupleof[$-1].offsetof;
    handlers.free_obj = &MakeDestructor!T;

    zend_class_entry ce;

    { // INIT_CLASS_ENTRY
      memset(&ce, 0, zend_class_entry.sizeof);
      ce.name = zend_string_init_interned(className, className.length, 1);
      ce.default_object_handlers = &std_object_handlers;
      ce.info.internal.builtin_functions = functions.ptr; // do this before zend_register_internal_class
    }

    class_entry = zend_register_internal_class_with_flags(&ce, null, ZEND_ACC_FINAL);

    // this have to happen after zend_register_internal_class_with_flags (which
    // resets default_object_handlers and some other fields)
    class_entry.create_object = &createObject!T;
    class_entry.default_object_handlers = &handlers;

    return class_entry;
  }

  pragma(inline, true)
  T* getNativeType(zend_object* obj) {
    return cast(T*) ((cast(ubyte*) obj) - T.tupleof[$-1].offsetof);
  }

  pragma(inline, true)
  zend_object* getPHPType(T* x) {
    return cast(zend_object*) ((cast(ubyte*) x) + T.tupleof[$-1].offsetof);
  }
}



/**
object construction sequence:

interpreter
  object_init_ex()
    _object_and_properties_init(obj, ce, properties: null)
      if ce.create_object == null (for userspace classes)
        zend_objects_new()
          emalloc()
          _zend_object_std_init() (sets fixed zend_object fields)
        _object_properties_init (sets properties to default value)
      else
        ce.create_object(ce)
          (This is defined here in extension code. We need to emalloc memory,
          default initialize native object prefix and call zend_object_std_init
          to initialize zend_object field that is managed by PHP.)
  obj.handlers.get_constructor(obj)
  call __construct()
  */
private zend_object* createObject(T)(zend_class_entry* ce) @nogc nothrow {
  // Our native classes have no declared properties.

  // If an object have guards (ce->ce_flags & ZEND_ACC_USE_GUARDS), the
  // last zval doesn't need to be allocated.
  // see: zend_object_alloc
  auto useGuards = ce.ce_flags & ZEND_ACC_USE_GUARDS;
  size_t size = T.sizeof - (useGuards ? 0 : zval.sizeof);
  T* obj = cast(T*) _emalloc(size);

  // default initialize native prefix before zend_object field

  enum initLength = T.tupleof[$-1].offsetof;
  static if (__traits(isZeroInit, T)) {
    memset(cast(void*) obj, 0, initLength);
  } else {
    const void[] initSymbol = __traits(initSymbol, T);
    memcpy(cast(void*) obj, cast(void*)initSymbol.ptr, initLength);
  }

  //emplace!T(obj);
  zend_object_std_init(&obj.tupleof[$-1], ce);
  return &obj.tupleof[$-1];
}


/// Get all public methods on a struct except destructors.
template PublicMethods(T) {
  alias PublicMethods = AliasSeq!();
  static foreach (member; __traits(allMembers, T)) {
    static if (
           is(typeof(mixin("T.", member)) == return) // is a function
        && !__traits(isStaticFunction, __traits(getMember, T, member))
        && member != "opAssign" // generated for structs with destructors
        && member != "__xdtor"
        && member != "__dtor" // destructor is handled specially in object handlers
        && member != "__destruct"
    ) {
      PublicMethods = AliasSeq!(PublicMethods, member);
    }
  }
}

template MakeMethods(T, methods...) {
  alias MakeMethods = AliasSeq!();
  static foreach (method; methods) {
    static if (method == "__ctor") {
      MakeMethods = AliasSeq!(MakeMethods,
          makeFunctionEntry!(MakeConstructor!(T), "__construct", false, true));
    } else {
      MakeMethods = AliasSeq!(MakeMethods,
          makeFunctionEntry!(MakeMethod!(T, method), method, false, true));
    }
  }
}

template MakeMethod(T, alias method) {
  private alias argTypes = Parameters!(mixin("T.", method));
  pragma(inline, true)
  static auto MakeMethod(zend_object* obj, argTypes args) {
    T* _this = cast(T*) ((cast(ubyte*) obj) - T.tupleof[$-1].offsetof);
    return mixin("_this.", method)(args);
  }
}

template MakeConstructor(T) {
  private alias argTypes = Parameters!(T.__ctor);
  pragma(inline, true)
  static void MakeConstructor(zend_object* obj, scope argTypes args) {
    T* _this = cast(T*) ((cast(ubyte*) obj) - T.tupleof[$-1].offsetof);
    _this.__ctor(args);
  }
}

template MakeDestructor(T) {
  pragma(inline, true)
  static void MakeDestructor(zend_object* obj) {
    T* _this = cast(T*) ((cast(ubyte*) obj) - T.tupleof[$-1].offsetof);
    static if (is(typeof(T.init.__destruct()))) {
      _this.__destruct();
    } else static if (is(typeof(T.init.__dtor()))) {
      _this.__dtor();
    }
    zend_object_std_dtor(obj);
  }
}




// === resources ===

alias Resource = zend_resource;

struct zend_resource {
  @nogc nothrow:

  zend_refcounted_h gc;
  long              handle;
  int               type;
  void*             ptr;

  int fileno() {
    if (type != php_file_le_stream()) return -1;
    void* ret;
    _php_stream_cast(cast(php_stream*) ptr, 1, &ret, 0);
    return cast(int) ret;
  }
}


void registerResource(T)(int moduleNumber, void function(zend_resource*) @nogc destructor = null) {
  phpResourceRegistry!T.register(moduleNumber, destructor);
}

T* allocateResource(T)() {
  return phpResourceRegistry!T.allocate();
}

struct phpResourceRegistry(T) {
  @nogc nothrow:

  private enum const(char)* name = T.stringof;
  private static __gshared int resourceNumber = -1;

  static void register(int moduleNumber, void function(zend_resource*) @nogc destructor) {
    if (destructor == null) destructor = &defaultDestructor;
    resourceNumber = zend_register_list_destructors_ex(destructor, null, name, moduleNumber);
  }

  static T* allocate() {
    T* x = cast(T*) _emalloc(T.sizeof);
    emplace!T(x);
    return x;
  }

  static void defaultDestructor(zend_resource* res) {
    T* x = cast(T*) res.ptr;
    destroy(*x);
    _efree(x);
  }

  static zend_resource* wrap(T* x) {
    assert(resourceNumber != -1, "resource type not registered (missing type.register() call in module_startup_func?)");
    return zend_register_resource(cast(void*) x, resourceNumber);
  }

  pragma(inline, true)
  static T* getNativeType(zend_resource* res) {
    return res.type == resourceNumber ? cast(T*) res.ptr : null;
  }
}



// === function value ====

// copy-pasted
struct zend_fcall_info {
	size_t size;
	zval function_name;
	zval *retval;
	zval *params;
	zend_object *object;
	uint param_count;
	/* This hashtable can also contain positional arguments (with integer keys),
	 * which will be appended to the normal params[]. This makes it easier to
	 * integrate APIs like call_user_func_array(). The usual restriction that
	 * there may not be position arguments after named arguments applies. */
	HashTable *named_params;
}

// copy-pasted
struct zend_fcall_info_cache {
	zend_function *function_handler;
	zend_class_entry *calling_scope;
	zend_class_entry *called_scope;
	zend_object *object; /* Instance of object for method calls */
  static if (PHPVersion >= 83)
	zend_object *closure; /* Closure reference, only if the callable *is* the object */
}





// === modules ===

alias ModuleEntry = zend_module_entry;


struct zend_module_entry {
  ushort size = zend_module_entry.sizeof;
  uint zend_api = ZendApi;
  ubyte zend_debug;
  ubyte zts;
  //const struct _zend_ini_entry *ini_entry;
  void* ini_entry;
  //const struct _zend_module_dep *deps;
  void* deps;
  const char *name;
  const zend_function_entry *functions;
  zend_result function(int type, int module_number) moduleStartup;
  zend_result function(int type, int module_number) moduleShutdown;
  zend_result function(int type, int module_number) requestStartup;
  zend_result function(int type, int module_number) requestShutdown;
  //void (*info_func)(ZEND_MODULE_INFO_FUNC_ARGS);
  void function() info_func;
  const char* version_;
  size_t globals_size;
  version (ZTS) {
    int* globals_id_ptr;
  } else {
    void* globals_ptr;
  }
  void function(void *global) globals_ctor;
  void function(void *global) globals_dtor;
  zend_result function() post_deactivate_func;
  int module_started;
  ubyte type;
  void *handle;
  int module_number;
  const char* build_id = BuildId;
}



// === functions ===

alias FunctionEntry = zend_function_entry;

struct zend_function_entry {
  const(char)* fname;
  zif_handler handler;
  // first arg_info is actually zend_internal_function_info
  const zend_internal_arg_info *arg_info;
  uint num_args;
  uint flags;
  static if (PHPVersion >= 84) {
    const zend_frameless_function_info *frameless_function_infos;
    const(char)* doc_comment;
  }
}

struct zend_frameless_function_info {
  void *handler;
  uint num_args;
}


// zif stands for zend internal function
alias zif_handler = void function(zend_execute_data *execute_data, zval *return_value);

struct zend_internal_arg_info {
  const(char)* name;
  zend_type type;
  const(char)* default_value;
}

struct zend_internal_function_info {
  ulong required_num_args;
  zend_type type; // return type?
  const(char)* default_value;
}

struct zend_type {
  void *ptr;
  uint type_mask;
}

struct zend_execute_data {
  void* /*const zend_op*/       *opline;           /* executed opline                */
  zend_execute_data   *call;             /* current call                   */
  zval                *return_value;
  void* /*zend_function*/       *func;             /* executed function              */
  zval                 This;             /* this + call_info + num_args    */
  zend_execute_data   *prev_execute_data;
  void* /*zend_array*/          *symbol_table;
  void               **run_time_cache;   /* cache op_array->run_time_cache */
  void* /*zend_array*/          *extra_named_params;
}






/**
  Template for automod functionality. Mixing in this template creates
  zend_module_entry structure and get_module() function needed by PHP to find
  and load an extension. It will also search through D module passed in as the
  first argument and exposes every public function and struct annotated by
  @phpClass as PHP function and PHP class respectively.
*/
mixin template mod(alias _module, alias string name = "") if (__traits(isModule, _module)) {
  @nogc:
  ModuleEntry __mod = {
    name: name == "" ? __traits(identifier, _module) : name,
    version_: "1",
    functions: [
      WrapFunctions!_module,
      FunctionEntry(),
    ],
    moduleStartup: (int, int moduleNumber) {
      registerAll!_module(moduleNumber);
      return Result.SUCCESS;
    },
    requestStartup: (int, int) {
      return Result.SUCCESS;
    },
  };
  extern(C) ModuleEntry* get_module() {
    // workaround: without this our functions generated by templates in the
    // mixin don't get included in compiled .so file
    alias _1 = WrapFunctions!_module;
    alias _2 = registerAll!_module;
    return &__mod;
  }

  private template WrapFunctions(alias _module) {
    // This nesting is there on purpose. Without it GDC complains that alias
    // 'was read, so cannot reassign'. Mixin templates are somehow messing
    // things up.
    private template Wrap(alias _module) {
      private import std.meta : AliasSeq;
      alias Wrap = AliasSeq!();
      static foreach (sym; __traits(allMembers, _module)) {
        static if (!is(mixin(sym)) && __traits(isStaticFunction, mixin(sym)) && __traits(getVisibility, mixin(sym)) == "public" && sym != "get_module") {
          Wrap = AliasSeq!(Wrap, func!(mixin(sym)));
        }
      }
    }
    alias WrapFunctions = Wrap!_module;
  }

  private void registerAll(alias _module)(int moduleNumber) {
    static foreach (sym; __traits(allMembers, _module)) {{
      static if (is(mixin(sym) Class == struct)) {
        static if (isPHPClass!(Class)) {
          registerClass!Class;
        } else static if (isResource!Class) {
          registerResource!Class(moduleNumber);
        }
        // TODO register userspace class descriptors
      }
    }}
  }

  private enum CanBePHPConstant(alias sym) =
    isPHPConstant!sym || (
      __traits(getVisibility, sym) == "public" &&
      __traits(compiles, { enum x = sym; }) &&   // anything that can be evaluated in compile time
      !__traits(compiles, { auto x = &sym; }) && // but not immutable values
      !__traits(isStaticFunction, sym)           // and not functions
    );
}



/// see tests/parity.sh
version (TestParityWithPHP) {
  import std.stdio;
  import std.string;
  import std.algorithm;

  enum HasDefinedSize(alias sym) = __traits(compiles, sym.sizeof);

  template ListPHPStructs(alias _module) {
    alias ListPHPStructs = AliasSeq!();
    static foreach (sym; __traits(allMembers, _module)) {
      static if (
          is(mixin(sym) == struct) &&
          (sym.startsWith("zend_") || sym == "zval" || sym == "Bucket" || sym == "HashTable") &&
          __traits(identifier, mixin(sym)) == sym && // ignore aliases
          HasDefinedSize!(mixin(sym))
      ) {
        ListPHPStructs = AliasSeq!(ListPHPStructs, mixin(sym));
      }
    }
  }

  template ListPHPConstants(alias _module) {
  }

  template ListPHPFunctions(alias _module) {
    alias ListPHPFunctions = AliasSeq!();
    static foreach (sym; __traits(allMembers, _module)) {
      static if (1
      ) {
        pragma(msg, sym);
        ListPHPFunctions = AliasSeq!(ListPHPFunctions, mixin(sym));
      }
    }
  }

  const(char)[] mungeFieldName(string f) {
    if (f == "val_")            return "val";
    if (f == "version_")        return "version";
    if (f == "version_")        return "version";
    if (f == "moduleStartup")   return "module_startup_func";
    if (f == "moduleShutdown")  return "module_shutdown_func";
    if (f == "requestStartup")  return "request_startup_func";
    if (f == "requestShutdown") return "request_shutdown_func";
    return f;
  }

  void main() {
    // using pargmas because that way we get data during compilation and avoid
    // hassle with linking symbols from PHP binary (which is not shared object)
    pragma(msg, "#include <php.h>");
    pragma(msg, `void test(char* name, long c, long d) { if (c != d) printf("%s: C %ld != D %ld\n", name, c, d); }`);
    pragma(msg, "int main(void) {");

    static foreach (S; ListPHPStructs!phpmod) {{
      enum structName = __traits(identifier, S);

      static if (
          !structName.startsWith("zend_ffi_") && // skip ffi types as they are defined in .c file
          structName != "zval" &&
          !(structName == "zend_frameless_function_info" && PHPVersion < 85) // it seems we cannot conditionally define top-level structs, we need to omit this one manually
      ) {
        pragma(msg, `  test("`, structName, `", sizeof(`, S.stringof, `), `, S.sizeof, `);`);
        static foreach (f; S.tupleof) {{
          enum f0 = __traits(identifier, f);
          enum field = mungeFieldName(__traits(identifier, f));
          pragma(msg, `  test("`, structName, `.`, f0, ` offset", offsetof(`, structName, `,     `, field, `), `, f.offsetof, `);`);
          pragma(msg, `  test("`, structName, `.`, f0, ` sizeof", sizeof(((`, structName, `*)0)->`, field, `), `, f.sizeof, `);`);
        }}
        pragma(msg, "");
      }
    }}

    // named enums
    static foreach (sym; __traits(allMembers, phpmod)) {
      static if (
          is(mixin(sym) == enum) &&
          sym != "Type" && !sym.startsWith("zend_ffi_") &&
          __traits(identifier, mixin(sym)) == sym  // ignore aliases
      ) {{
        alias _enum = mixin(sym);
        static foreach (f; __traits(allMembers, _enum)) {{
          enum val = mixin(sym, ".", f);
          static if (is(typeof(val) BT == enum)) {}
          pragma(msg, `  test("`, sym, ".", f, `", `, f, `, `, cast(BT)val, `);`);
        }}
      }}
    }

    // single member anonymous enums
    static foreach (sym; __traits(allMembers, phpmod)) {{
      alias S = mixin(sym);
      static if (
          !__traits(compiles, { return &S; }) &&
          __traits(compiles, { auto a = S; }) &&
          (sym.canFind("zend_") || sym.canFind("ZEND_") || sym.canFind("IS_") || sym.canFind("GC_") || sym.canFind("BR_")) &&
          !sym.canFind("FFI_")
      ) {
        enum val = mixin(sym);
        pragma(msg, `  test("`, sym, `", `, sym, `, `, cast(long)val, `);`);
      }
    }}

    pragma(msg, `  test("zend_executor_globals.exception", offsetof(zend_executor_globals, exception), `, _exceptionOffsetInExecGlobals,`);`);
    pragma(msg, "");


    static foreach (sym; __traits(allMembers, phpmod)) {{
      static if (
          sym != "DebugParams" && sym != "_d_run_main" && sym != "_Dmain" &&
          __traits(isStaticFunction, mixin(sym)) &&
          __traits(getLinkage, mixin(sym)) == "C" &&
          __traits(identifier, mixin(sym)) == sym // ignore aliases
      ) {{
        // TODO function types
        //pragma(msg, sym);
        //pragma(msg, typeof(mixin(sym)));
        //static if (is(typeof(mixin(sym)) Args == function)) {}
        //pragma(msg, Args);
      }}
    }}

    pragma(msg, "}");
  }
}

// license: GPL3

/**
 * TODO:
 *  - native destructor should be executed in handlers->free_obj
 *  - allocate smaller objects when guards are not used
 *  - type for callable arguments
 *  - autozvals
 *  - userland class descriptors
 *  - testing parity with PHP
 *  - typed iteration over array of classes
 *  - C string arguments (char* + length pair)
 *  - ffi datatypes
 *  - ref arguments ???
 *
 * Not yet supported:
 *  - ZTS
 *  - PHP debug builds (API of some functions is changed)
 *
 * Never supported:
 *  - crummy typing rules and coercions following exact PHP semantic
 *    (strict_types is the only way)
 *  - PHP references.
 *    Internally references are represented as a different type but in PHP
 *    userspace they are indistinguishable from the type they reference. This
 *    poisons the whole PHP codebase where we need to constantly check for
 *    references to unwrap them even though references are almost never used.
 *  - older PHP versions
 *    I currently target PHP 8.4 and long term I will target whatever version
 *    PHP in packaged in Debian unstable
 *  - 32-bit platforms
 *    On 32-bit machine memory layout of every type might slightly differ.
 *    Keeping everything in sync would be too much extra effort for very little
 *    gain. Even Raspberry Pi Zero 2 has 64 bit CPU nowadays and it costs
 *    almost nothing.
 *  - windows support
 *
 * Compile using `gdc-14 -shared -fPIC -fpreview=dip1000 -fpreview=dip1008`.
 * `-fpreview=dip1000` is there to check parameters with a scope modifier.
 * `-fpreview=dip1008` or `-fpreview=all` allows exceptions in @nogc code. That
 * way we can be more sure GC is not used.
 */
module phpmod;

import core.lifetime : emplace;
import std.meta : anySatisfy, AliasSeq;
import std.traits;
import std.typecons : Tuple;
import core.stdc.stdlib;
import core.stdc.stdint : uintptr_t;
import core.stdc.string;



// Import minimal set of symbols and global variables exposed from PHP header files.
// Block is __gshared to make all variable true globals and not thread locals.
// Only ZTS build have some use for thread local variables (see ZEND_TLS in php source).
extern extern(C) @nogc nothrow __gshared {
  version (GNU) {
    import gcc.attributes : attribute;
  } else {
    struct attribute { string dummy; }
  }

  version (GNU) {
    import gcc.builtins;
    pragma(inline, true) bool   likely()(bool e) { return !!__builtin_expect(e, 1); };
    pragma(inline, true) bool unlikely()(bool e) { return !!__builtin_expect(e, 0); };
  } else {
    pragma(inline, true) bool   likely()(bool e) { return e; };
    pragma(inline, true) bool unlikely()(bool e) { return e; };
  }


  struct zend_executor_globals;
  zend_executor_globals executor_globals;
  alias EG = executor_globals;

  void *tsrm_get_ls_cache();

  version (ZEND_DEBUG) {
    void* _emalloc(size_t size,
        const(char)* __zend_filename = __FILE__, const(uint) __zend_lineno = __LINE__,
        const(char)* __zend_orig_filename = null, const(uint) __zend_orig_lineno = 0
    );
    void* _efree(void* ptr,
        const(char)* __zend_filename = __FILE__, const(uint) __zend_lineno = __LINE__,
        const(char)* __zend_orig_filename = null, const(uint) __zend_orig_lineno = 0
    );
  } else {
    void* _emalloc(size_t size);
    void* _efree(void* ptr);
  }

  void zval_ptr_dtor(zval *zval_ptr);


  // === parameter parsing ===

  @attribute("cold") void zend_wrong_parameters_count_error(uint min_num_args, uint max_num_args);
  @attribute("cold") void zend_argument_type_error(uint arg_num, const char *format, ...);
  @attribute("cold") void zend_type_error(const char *format, ...);
  @attribute("cold") Object* zend_throw_exception(zend_class_entry* exception_ce, const(char)* message, long code);
  @attribute("cold") Object* zend_throw_exception_ex(zend_class_entry* exception_ce, long code, const(char)* format, ...);


  // === arrays ===

  // Allocate new unitialized HashTable. Memory for key/value data has to be
  // allocated separately by `zend_hash_real_init` or similiar.
  HashTable* _zend_new_array(uint nSize);
  void zend_hash_real_init(HashTable *ht, bool packed);
  void zend_hash_real_init_packed(HashTable *ht);
  void zend_hash_real_init_mixed(HashTable *ht);

  // convert hash table representation (performs no safety checks if the
  // hashtable has given representation)
  void zend_hash_packed_to_hash(HashTable *ht);
  void zend_hash_to_packed(HashTable *ht);
  // Safe to use on both hash tables and packed arrays.
  HashTable* zend_array_to_list(const(HashTable) *source);

  // Grow hash table. Grow only, never shrink.
  void zend_hash_extend(HashTable *ht, uint nSize, bool packed);

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

  // update hash value of given string
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


  // === objects ===

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

  void zend_object_std_init(zend_object *object, zend_class_entry *ce);

  zend_class_entry *zend_register_internal_class_with_flags(zend_class_entry *class_entry, zend_class_entry *parent_ce, uint ce_flags);
  void zend_class_implements(zend_class_entry *class_entry, int num_interfaces, ...);


  // === functions ===

  zend_result zend_call_function(zend_fcall_info *fci, zend_fcall_info_cache *fci_cache);
  void zend_release_fcall_info_cache(zend_fcall_info_cache *fcc);
  zend_result zend_fcall_info_init(zval *callable, uint check_flags, zend_fcall_info *fci, zend_fcall_info_cache *fcc, zend_string **callable_name, char **error);
}

alias rsrc_dtor_func_t = void function(zend_resource*);

enum ZEND_ACC_PUBLIC                 = (1 <<  0);
enum ZEND_ACC_PROTECTED              = (1 <<  1);
enum ZEND_ACC_PRIVATE                = (1 <<  2);
enum ZEND_ACC_FINAL                  = (1 <<  5);


T* emalloc(T)() @nogc nothrow {
  pragma(inline, true);
  return cast(T*) _emalloc(T.sizeof);
}

void pefree(void* ptr, bool persistent) @nogc nothrow {
  !persistent ? _efree(ptr) : free(ptr);
}


/** Variant of exception modified to accept C-style zero terminated string and
 *  optional class entry of the exception that will be rethrown on PHP side.
 *  It's mainly a convenience feature, phpmod.d can catch any native exception and
 *  rethrow it to userland PHP code. */
class PHPException : Exception {
  const(char)* _msg;
  zend_class_entry* ce;
  pure nothrow @nogc @safe this(const(char)* message, zend_class_entry* ce = null, string file = __FILE__, size_t line = __LINE__, Throwable nextInChain = null) {
    super("", file, line, nextInChain);
    this._msg = message;
    this.ce = ce;
  }
}


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


private const(zend_function_entry) makeFunctionEntry(alias f, alias phpName, alias bool generateFrameless, alias bool firstArgIsThis)() {
  static assert(!(generateFrameless && firstArgIsThis), "frameless function cannot access $this");
  static assert(__traits(isStaticFunction, f), "first template argument must be a function");

  enum numParams = NumPHPParams!(f, firstArgIsThis);

  immutable static zend_internal_arg_info[numParams + 1] arginfo = [MakeArgInfos!(f, firstArgIsThis, phpName)];

  static if (generateFrameless && numParams < 4) {
    const static zend_frameless_function_info[2] farginfos = [
      zend_frameless_function_info(&wrapFunc!(f, true, false, zval*, zval*, zval*, zval*), numParams),
      zend_frameless_function_info(),
    ];
    return zend_function_entry(phpName, &wrapFunc!(f, false, false, zend_execute_data*, zval*), arginfo.ptr, numParams, 0, farginfos.ptr, null);
  } else {
    return zend_function_entry(phpName, &wrapFunc!(f, false, firstArgIsThis, zend_execute_data*, zval*), arginfo.ptr, numParams, 0, null, null);
  }
}


private template MakeArgInfos(alias f, alias bool firstArgIsThis, alias string phpName) {
  enum reqNumParams = ReqNumPHPParams!(f, firstArgIsThis);

  enum isConstructor = phpName == "__construct" || phpName == "__destruct";

  static if (is(typeof(f) R == return) && !isConstructor) {
    enum returnTypeHint = TypeHint!(R, __traits(getAttributes, f));
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
              TypeHint!(ParamTypes[i], __traits(getAttributes, ParamTypes[i .. i + 1])),
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

private enum NoTypeHint = immutable(zend_type)(null, 0);

private enum immutable(zend_type) TypeHint(T, attrs...) = function immutable(zend_type) {
  static if (isNullable!attrs) {
    enum nullMask = _ZEND_TYPE_NULLABLE_BIT;
  } else {
    enum nullMask = 0;
  }

  static if (is(T == bool))                    return zend_type(null, (1 << Type.IsTrue) | (1 << Type.IsFalse));
  else static if (is(T : long))                return zend_type(null, 1 << Type.IsLong);
  else static if (is(T : double))              return zend_type(null, 1 << Type.IsDouble);
  else static if (is(T : const(char)[]))       return zend_type(null, 1 << Type.IsString);
  else static if (is(T : const(zend_string)*)) return zend_type(null, (1 << Type.IsString) | nullMask);
  else static if (is(T : const(HashTable)*))   return zend_type(null, (1 << Type.IsArray)  | nullMask);
  else static if (is(T : const(zend_object)*)) return zend_type(null, (1 << Type.IsObject) | nullMask);
  // for some reason the following line leads to segfaults, it seems like that
  // resource parameters may be not type-hinted (eg. reflection indicates the
  // first argument of fseek function has no type-hint)
  //else static if (is(T : const(zend_resource)*)) auto type = zend_type(null, 1 << Type.IsResource);
  else static if (is(T U : U*) && (isPHPClass!U || isPHPClass_!attrs)) {
    return immutable(zend_type)( __traits(identifier, U).ptr, _ZEND_TYPE_LITERAL_NAME_BIT | nullMask);
  }
  else return zend_type(null, 0);
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



private template DefaultArgStr(alias f, size_t i) {
  static if (is(typeof(f) PT == __parameters)) {
    enum defArgFunc = mixin("(PT[i .. i+1] __args_) => __args_[0]");
    static if (is(typeof(defArgFunc()))) {
      enum d = defArgFunc();
      static if (is(typeof(d) : const(char)[])) {
        enum DefaultArgStr = d.stringof;
      } else static if (is(typeof(d) : long)) {
        import std.conv : to;
        enum DefaultArgStr = mixin(`"`, long(d).to!string, `"`);
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
  static if (is(typeof(f) PT == __parameters)) {
    enum defArgFunc = mixin("(PT[i .. i+1] __args_) => __args_[0]");
    static if (is(typeof(defArgFunc()))) {
      enum DefaultArg = defArgFunc();
    }
  }
}

private enum NumPHPParams(alias f, alias bool firstArgIsThis) =
  Parameters!f.length - firstArgIsThis;

private enum ReqNumPHPParams(alias f, alias bool firstArgIsThis) = {
  static foreach_reverse (i, p; Parameters!f) {
    static if (DefaultArgStr!(f, i) == null) {
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
  enum isCString(T) = is(PT == const(char)*) || is(PT == const(ubyte)*);

  static if (frameless) {
    static assert (!firstArgIsThis);

    zval* return_value = phpArgs[0];

    zval* zvalArgs(alias i)() {
      pragma(inline, true);
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
  }

  Tuple!(ParamTypes) args;

  // _ni = native argument index
  static foreach (_ni, PT; ParamTypes) {{

    static if (_ni == 0 && firstArgIsThis) {
      static assert(is(PT == zend_object*));
      args[0] = ex.This.obj;

    } else {
      // 'i' refers to position of php argument not argument of f. It excludes
      // synthetic 'this'.
      enum i = _ni - firstArgIsThis;

      static if (frameless) {
        zval* arg = zvalArgs!i;
      } else {
        zval* arg = zvalArgs + i;
      }

      static if (!frameless && i >= reqNumParams) {
        zval _z;
        if (i >= numArgs) {
          _z = zval(DefaultArg!(f, _ni)); // using _ni intentionally
          arg = &_z;
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
          static assert(0, "cannot handle ulong arguments (yet)");
        }

        if (arg.type != Type.IsLong) {
          zend_argument_type_error(i+1, "expected long, %s given", typeNames[arg.type]);
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
        if (arg.type == Type.IsDouble) {
          args[_ni] = cast(PT) arg.dval;
        } else if (arg.type == Type.IsLong) {
          args[_ni] = cast(PT) arg.lval;
        } else {
          zend_argument_type_error(i+1, "expected double, %s given", typeNames[arg.type]);
          return;
        }

      } else static if (is(PT == bool)) {
        bool val;
        if (arg.type == Type.IsTrue) {
          val = true;
        } else if (arg.type == Type.IsFalse) {
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
        if (arg.type != Type.IsString) {
          zend_argument_type_error(i+1, "expected string, %s given", typeNames[arg.type]);
          return;
        }
        args[_ni] = cast(PT) arg.str.str[];

      } else static if (is(PT : const(zend_string)*)) {
        if (arg.type == Type.IsString) {
          args[_ni] = arg.str;
        } else if (isNullable!attrs && arg.type == Type.IsNull) {
          args[_ni] = null;
        } else {
          zend_argument_type_error(i+1, "expected string, %s given", typeNames[arg.type]);
          return;
        }

      } else static if (is(PT : const(HashTable)*)) {
        if (arg.type == Type.IsArray) {
          args[_ni] = arg.arr;
        } else if (isNullable!attrs && arg.type == Type.IsNull) {
          args[_ni] = null;
        } else {
          zend_argument_type_error(i+1, "expected array, %s given", typeNames[arg.type]);
          return;
        }

      } else static if (is(PT : const(zend_resource)*)) {
        if (arg.type == Type.IsResource) {
          args[_ni] = arg.res;
        } else if (isNullable!attrs && arg.type == Type.IsNull) {
          args[_ni] = null;
        } else {
          zend_argument_type_error(i+1, "expected resource, %s given", typeNames[arg.type]);
          return;
        }

      } else static if (is(PT : const(zend_object)*)) {
        if (arg.type == Type.IsObject) {
          args[_ni] = arg.obj;
        } else if (isNullable!attrs && arg.type == Type.IsNull) {
          args[_ni] = null;
        } else {
          zend_argument_type_error(i+1, "expected object, %s given", typeNames[arg.type]);
          return;
        }

      } else static if (is(PT == zval*)) {
        args[_ni] = arg;

      } else static if (is(PT == zval)) {
        args[_ni] = *arg;

      } else static if (is(PT == T*, T)) {

        static if (isPHPClass!T || isPHPClass_!attrs) {
          if (arg.type == Type.IsObject) {
            const(zend_class_entry)* ce = phpClassRegistry!T.class_entry;
            if (arg.obj.ce == ce)  {
              args[_ni] = phpClassRegistry!T.getNativeType(arg.obj);
            } else {
              zend_argument_type_error(i+1, "class type mismatch");
              return;
            }
          } else if (isNullable!attrs && arg.type == Type.IsNull) {
            args[_ni] = null;
          } else {
            zend_argument_type_error(i+1, "expected object, %s given", typeNames[arg.type]);
            return;
          }

        } else static if (isResource!T || isResource_!attrs) {
          if (arg.type == Type.IsResource) {
            auto res = phpResourceRegistry!T.getNativeType(arg.res);
            if (res == null) {
              zend_argument_type_error(i+1, "expected %s resource", T.stringof.ptr);
              return;
            }
            args[_ni] = res;
          } else if (isNullable!attrs && arg.type == Type.IsNull) {
            args[_ni] = null;
          } else {
            zend_argument_type_error(i+1, "expected resource, %s given", typeNames[arg.type]);
            return;
          }

        } else static if (isFFIType!T) {
          T* resArg;
          bool ok = getFFIArg!(T)(i, arg, &resArg);
          if (!ok) return;
          args[_ni] = resArg;

        } else {
          static assert(0);
        }

      } else static if (is(PT == T[], T) && isFFIType!T) {
        T[] resArg;
        bool ok = getFFIArgArr!(T)(i, arg, &resArg);
        if (!ok) return;
        args[_ni] = resArg;

      } else {
        static assert(0, "cannot handle argument of type " ~ PT.stringof);
      }
    }
  }}

  try {
    alias RT = ReturnType!f;
    static if (is(RT == void)) {
      f(args.expand);
    } else {
      auto retval = f(args.expand);
      alias returnAttrs = __traits(getAttributes, f);

      static if (is(RT == zval)) {
        *return_value = retval;

      } else static if (isIntegral!RT || is(RT == double) || is(RT == float) || is(RT == bool)) {
        *return_value = zval(retval);

      } else static if (is(RT == zend_string*) || is(RT == zend_resource*) || is(RT == zend_object*) || is(RT == HashTable*)) {
        if (isNullable!returnAttrs && retval == null) {
          *return_value = zval.Null();
        } else {
          *return_value = zval(retval);
        }

      } else static if (is(RT == T*, T)) {
        static if (isPHPClass!T || isPHPClass_!returnAttrs) {
          if (isNullable!returnAttrs && retval == null) {
            *return_value = zval.Null();
          } else {
            *return_value = zval(phpClassRegistry!T.getPHPType(retval));
          }
        } else static if (isResource!T || isResource_!returnAttrs) {
          if (isNullable!returnAttrs && retval == null) {
            *return_value = zval.Null();
          } else {
            *return_value = zval(phpResourceRegistry!T.wrap(retval));
          }
        } else static assert(0);

      } else {
        static assert(0, "cannot handle return value of type " ~ RT.stringof);
      }
    }
  } catch (PHPException e) {
    auto msg = e._msg ? e._msg : "null";
    zend_throw_exception_ex(e.ce, 0, "%s (%.*s:%d)", msg, e.file.length, e.file.ptr, e.line);

  } catch (Exception e) {
    zend_throw_exception_ex(null, 0, "%.*s (%.*s:%d)", e.msg.length, e.msg.ptr, e.file.length, e.file.ptr, e.line);
  }
}


// we use array of size 256 to eliminate bounds check in error messages
private static immutable char*[256] typeNames = [
  Type.IsUndef     : "undef",
  Type.IsNull      : "null",
  Type.IsFalse     : "false",
  Type.IsTrue      : "true",
  Type.IsLong      : "long",
  Type.IsDouble    : "double",
  Type.IsString    : "string",
  Type.IsArray     : "array",
  Type.IsObject    : "object",
  Type.IsResource  : "resource",
  Type.IsReference : "reference",
];






alias Result = zend_result;

enum zend_result {
  SUCCESS =  0,
  FAILURE = -1,    /* this MUST stay a negative number, or it may affect functions! */
}


enum Type : ubyte {
  IsUndef     = 0,
  IsNull      = 1,
  IsFalse     = 2,
  IsTrue      = 3,
  IsLong      = 4,
  IsDouble    = 5,
  IsString    = 6,
  IsArray     = 7,
  IsObject    = 8,
  IsResource  = 9,
  IsReference = 10,

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
  @nogc {

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

  void opAssign(zval x) nothrow {
    this.lval = x.lval;
    this.u1 = x.u1;
    // do not overwrite u2
  }

  this(T : long)(T x) nothrow {
    lval = x;
    u1.type_info = Type.IsLong;
  }

  this(T : double)(T x) nothrow {
    dval = x;
    u1.type_info = Type.IsDouble;
  }

  this(T : bool)(T x) nothrow {
    u1.type_info = x ? Type.IsTrue : Type.IsFalse;
  }

  this(typeof(null) x) nothrow {
    u1.type_info = Type.IsNull;
  }

  static Null() nothrow {
    zval z;
    z.u1.type_info = Type.IsNull;
    return z;
  }

  this(zend_string* x) nothrow {
    str = x;
    u1.type_info = Type.IsString;
    u1.v.type_flags = x.isInterned ? 0 : IS_TYPE_REFCOUNTED;
  }

  this(HashTable* x) nothrow {
    arr = x;
    u1.type_info = Type.IsArray;
    u1.v.type_flags = IS_TYPE_REFCOUNTED | IS_TYPE_COLLECTABLE;
  }

  this(zend_object* x) nothrow {
    obj = x;
    u1.type_info = Type.IsObject;
    u1.v.type_flags = IS_TYPE_REFCOUNTED | IS_TYPE_COLLECTABLE;
  }

  this(zend_resource* x) nothrow {
    res = x;
    u1.type_info = Type.IsResource;
    u1.v.type_flags = IS_TYPE_REFCOUNTED;
  }


  Type type() nothrow { return u1.v.type; }

  long* asLong() return       nothrow { return type == Type.IsLong ? &lval : null; }
  double* asDouble() return   nothrow { return type == Type.IsDouble ? &dval : null; }
  const(bool)* asBool()       nothrow {
    immutable static bool[2] falseTrue = [false, true];
    if (type == Type.IsFalse) return &falseTrue[0];
    if (type == Type.IsTrue)  return &falseTrue[1];
    return null;
  }
  HashTable* asArray()        nothrow { return type == Type.IsArray ? arr : null; }
  zend_object* asObject()     nothrow { return type == Type.IsObject ? obj : null; }
  zend_string* asString()     nothrow { return type == Type.IsString ? str : null; }
  zend_resource* asResource() nothrow { return type == Type.IsResource ? res : null; }

  auto asType(T: long*)()          => asLong();
  auto asType(T: double*)()        => asDouble();
  auto asType(T: bool*)()          => asBool();
  auto asType(T: HashTable*)()     => asArray();
  auto asType(T: zend_object*)()   => asObject();
  auto asType(T: zend_string*)()   => asString();
  auto asType(T: zend_resource*)() => asResource();

  long toLong() return        { if (type != Type.IsLong)     throw new Exception("type error: cannot convert zval to long"); return lval; }
  double toDouble() return    { if (type != Type.IsDouble)   throw new Exception("type error: cannot convert zval to double"); return dval; }
  bool toBool()               { if (type != Type.IsTrue && type != Type.IsFalse) throw new Exception("bad type: cannot convert zval to bool"); return type == Type.IsTrue; }
  HashTable* toArray()        { if (type != Type.IsArray)    throw new Exception("type error: cannot convert zval to array"); return arr; }
  zend_object* toObject()     { if (type != Type.IsObject)   throw new Exception("type error: cannot convert zval to object"); return obj; }
  //zend_string* toString()     { if (type != Type.IsString)   throw new Exception("bad type"); return str; }
  zend_resource* toResource() { if (type != Type.IsResource) throw new Exception("type error: cannot convert zval to resource"); return res; }

  bool isTrue()  { return type == Type.IsTrue; }
  bool isFalse() { return type == Type.IsFalse; }

  bool isTruthy() {
    return !(
        (type == Type.IsFalse) ||
        (type == Type.IsLong && lval == 0) ||
        (type == Type.IsDouble && (dval == 0.0 || dval == -0.0)) ||
        (type == Type.IsString && (str.len == 0 || (str.len == 1 && str.ptr[0] == '0'))) ||
        (type == Type.IsArray && arr.length == 0) ||
        (type == Type.IsNull) ||
        (type == Type.IsUndef)
    );
  }


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

  //void toString(void delegate(const(char)[]) sink) {
  //  sink("zval(");
  //  sink(typeNames[type]);
  //  sink(")");
  //}
}

static assert(zval.sizeof == 16);


// TODO
struct autozval {
  zval z;
  alias this = z;
  ~this() {
    z.release();
  }
}




// === refcounted ===

struct zend_refcounted {
  zend_refcounted_h gc;
}


enum GC_TYPE_MASK     =  0x0000000f;
enum GC_FLAGS_MASK    =  0x000003f0;
enum GC_INFO_MASK     =  0xfffffc00;
enum GC_FLAGS_SHIFT   =  0;
enum GC_INFO_SHIFT    =  10;

enum GC_NULL         = (Type.IsNull        | (GC_NOT_COLLECTABLE << GC_FLAGS_SHIFT));
enum GC_STRING       = (Type.IsString      | (GC_NOT_COLLECTABLE << GC_FLAGS_SHIFT));
enum GC_ARRAY        =  Type.IsArray;
enum GC_OBJECT       =  Type.IsObject;
enum GC_RESOURCE     = (Type.IsResource    | (GC_NOT_COLLECTABLE << GC_FLAGS_SHIFT));
enum GC_REFERENCE    = (Type.IsReference   | (GC_NOT_COLLECTABLE << GC_FLAGS_SHIFT));
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
  @nogc nothrow:
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
  char[0]      val_;

  alias length = len;

  // val() to be more like PHP, ptr() to be more like D
  inout(char)*  val() return inout { return val_.ptr; }
  inout(char)*  ptr() return inout { return val_.ptr; }
  inout(char)[] str() return inout { return val_.ptr[0 .. len]; }

  // see ZSTR_IS_INTERNED
  bool isInterned() const { return (gc.u.type_info & IS_STR_INTERNED) != 0; }

  enum ZEND_MM_ALIGNMENT = 8;
  enum ZEND_MM_ALIGNMENT_MASK = ~(ZEND_MM_ALIGNMENT - 1);
  alias ZEND_MM_ALIGNED_SIZE = (size) => (size + ZEND_MM_ALIGNMENT - 1) & ZEND_MM_ALIGNMENT_MASK;

  // see zend_string_alloc
  static zend_string* alloc(size_t len/*, bool persistent = false*/) {
    bool persistent = false;
    zend_string *ret = cast(zend_string*) _emalloc(ZEND_MM_ALIGNED_SIZE(zend_string.sizeof + len + 1));

    ret.gc.refcount = 1;
    ret.gc.u.type_info = GC_STRING | ((persistent ? IS_STR_PERSISTENT : 0) << GC_FLAGS_SHIFT);
    ret.h = 0;
    ret.len = len;
    ret.val[len] = 0; // null terminated
    return ret;
  }

  // see zend_string_init
  static zend_string* copy(const(char)[] str/*, bool persistent = false*/) {
    auto s = zend_string.alloc(str.length/*, persistent*/);
    s.str[] = str[];
    return s;
  }

  static zend_string* copy(const(ubyte)[] str/*, bool persistent = false*/) {
    return copy(cast(const(char)[]) str/*, persistent*/);
  }

  bool equalContent(const(zend_string)* str) const {
    return len == str.len && !memcmp(this.val, str.val, len);
  }

  bool equals(const(zend_string)* str) const {
    return &this == str || (len == str.len && !memcmp(this.val, str.val, len));
  }

  // see SEPARATE_STRING
  // TODO zend_string_separate differs
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
}





// === array ===

struct Bucket {
  zval         val;
  ulong        h;                /* hash value (or numeric index)   */
  zend_string* key;              /* string key or NULL for numerics */
}

alias zend_array = HashTable;
alias ZendArray  = HashTable;



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

  size_t length()      const @nogc nothrow { return nNumOfElements; }
  bool isPacked()      const @nogc nothrow { return (u.flags & (1<<2)) != 0; }
  bool isUnitialized() const @nogc nothrow { return (u.flags & (1<<3)) != 0; }
  bool hasStaticKeys() const @nogc nothrow { return (u.flags & (1<<4)) != 0; }
  bool hasEmptyInd()   const @nogc nothrow { return (u.flags & (1<<5)) != 0; }
  bool hasHoles()      const @nogc nothrow { return nNumUsed != nNumOfElements; }


  static HashTable* alloc(size_t capacity, bool packed) @nogc nothrow {
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



  inout(zval)* get(long key)                inout @nogc nothrow { return cast(inout(zval)*) zend_hash_index_find(&this, key); }
  inout(zval)* get(scope const(char)[] key) inout @nogc nothrow { return cast(inout(zval)*) zend_hash_str_find(&this, key.ptr, key.length); }
  inout(zval)* get(zend_string* key)        inout @nogc nothrow { return cast(inout(zval)*) zend_hash_find(&this, key); }
  alias opBinaryRight(string op : "in") = get;
  alias opIndex = get;

  zval* set(long key, zval* val)                                   @nogc nothrow { return zend_hash_index_update(&this, key, val); }
  zval* set(scope const(char)[] key, zval* val)                    @nogc nothrow { return zend_hash_str_update(&this, key.ptr, key.length, val); }
  zval* set(zend_string* key, zval* val)                           @nogc nothrow { return zend_hash_update(&this, key, val); }
  zval* opIndexAssign(zval* val, long key)                         @nogc nothrow { return zend_hash_index_update(&this, key, val); }
  zval* opIndexAssign(zval* val, const(char)[] key, size_t length) @nogc nothrow { return zend_hash_str_update(&this, key.ptr, key.length, val); }
  zval* opIndexAssign(zval* val, zend_string* key)                 @nogc nothrow { return zend_hash_update(&this, key, val); }

  void append(scope zval* value) @nogc nothrow {
    zend_hash_next_index_insert(&this, value);
  }


  static struct Iterator(alias string what, alias bool _constTable) {
    @nogc nothrow:
    static if (_constTable) {
      const(HashTable)* ht;
    } else {
      HashTable* ht;
    }
    private zval* pos, end;
    private size_t step;
    private long idx = 0;

    static if (!_constTable) {
      this(HashTable* ht) {
        this.ht = ht;
        init();
      }
      alias Z = zval;
    } else {
      this(const(HashTable)* ht) {
        this.ht = ht;
        init();
      }
      alias Z = const(zval);
    }

    private void init() {
      step = ht.isPacked ? 1 : 2;
      pos = cast(zval*) ht.arPacked;
      end = cast(zval*) (ht.arPacked + ht.nNumUsed * step);
      while (pos < end && pos.type == Type.IsUndef) {
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

      } else static if (what == "kv") {
        struct KV { Z key; Z* value; }
        if (ht.isPacked) {
          return KV(zval(idx), pos);
        } else {
          Bucket* b = cast(Bucket*) pos;
          zval k = b.key == null ? zval(b.h) : zval(b.key);
          return KV(k, pos);
        }

      } else static assert(0);

    }
    void popFront() {
      do {
        pos += step;
        idx++;
      } while (pos < end && pos.type == Type.IsUndef);
    }
  }


  // these functions are templated to prevent generating and compiling unused variants
  auto byValue()()          { return Iterator!("v", false)(&this); }
  auto byValue()() const    { return Iterator!("v", true)(&this); }
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
   * string key or object value) an exception is generated. No type conversions
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
   * be numerical.
   */
  alias typed = _typed!false;

  /**
   * Same as `typed` but can convert integer keys to const(char)[]. In that
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
            if (z.type == Type.IsUndef) continue;
            V v = extractValue!V(z);
            int result = dg(v);
            if (result) return result;
          }

        } else {
          for (Bucket* b = ht.arData; b < ht.arData + ht.nNumUsed; b++) {
            if (b.val.type == Type.IsUndef) continue;
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
            if (z.type == Type.IsUndef) continue;
            K k = extractPackedKey!(K, convertKeys)(i, buf[]);
            V v = extractValue!V(z);
            int result = dg(k, v);
            if (result) return result;
          }

        } else {
          for (Bucket* b = ht.arData; b < ht.arData + ht.nNumUsed; b++) {
            if (b.val.type == Type.IsUndef) continue;
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

  } else static if (is(T == zval)) {
    return b.key == null ? zval(b.h) : zval(b.key);

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

  } else static if (is(T == zval)) {
    return zval(k);

  } else static assert(0, "cannot handle argument of type " ~ T.stringof);
}


private T extractValue(T)(scope return zval* z) @nogc {
  static if (isIntegral!T) {
    static if (is(T == ulong)) assert(0, "cannot handle unsigned longs");
    if (z.type != Type.IsLong) { throw new Exception("int value expected"); }
    long val = z.lval;
    static if (!is(T == long)) {
      if (val < T.min || val > T.max) {
        throw new Exception("int value out of range");
      }
    }
    return cast(T) val;
  } else static if (isFloatingPoint!T) {
    if (z.type == Type.IsDouble) return cast(T) z.dval;
    if (z.type == Type.IsLong)   return cast(T) z.lval;
    throw new Exception("double value expected");
  } else static if (is(T == bool)) {
    if (z.type == Type.IsTrue) return true;
    if (z.type == Type.IsFalse) return false;
    throw new Exception("bool value expected");
  } else static if (is(T : const(char)[])) {
    if (z.type != Type.IsString) { throw new Exception("string value expected"); }
    return z.str.str();
  } else static if (is(T : zend_string*)) {
    if (z.type != Type.IsString) { throw new Exception("string value expected"); }
    return z.str;
  } else static if (is(T : HashTable*)) {
    if (z.type != Type.IsArray) { throw new Exception("array value expected"); }
    return z.arr;
  } else static if (is(T : zend_object*)) {
    if (z.type != Type.IsObject) { throw new Exception("object value expected"); }
    return z.obj;
  } else static if (is(T : zval*)) {
    return z;
  } else static if (is(T : zval)) {
    return *z;
  } else static assert(0, "cannot handle iterating over "~T.stringof);
}



// === objects ===

alias ZendObject = zend_object;

struct zend_object {
  @nogc nothrow:

  zend_refcounted_h gc;
  uint              handle;
  uint              extra_flags; /* OBJ_EXTRA_FLAGS() */ // since 8.4
  zend_class_entry* ce;
  const(zend_object_handlers)* handlers;
  HashTable*       properties;
  zval[1]          properties_table;

  zval* readProperty(const(char)[] name, zval* rv) {
    return zend_read_property(ce, &this, name.ptr, name.length, false, rv);
  }

  // TODO
  //T readProperty(alias name, T)() {
  //}

  void writeProperty(const(char)[] name, zval* value) {
    zend_update_property(ce, &this, name.ptr, name.length, value);
  }
}


enum {
  BP_VAR_R        = 0,
  BP_VAR_W        = 1,
  BP_VAR_RW        = 2,
  BP_VAR_IS        = 3,
  BP_VAR_FUNC_ARG  =  4,
  BP_VAR_UNSET    = 5,
}


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
	//ZEND_MAP_PTR_DEF(zval *, static_members_table);
  void* static_members_table;
	HashTable function_table;
	HashTable properties_info;
	HashTable constants_table;

	//ZEND_MAP_PTR_DEF(zend_class_mutable_data*, mutable_data);
  void* mutable_data;
	//zend_inheritance_cache_entry *inheritance_cache;
  void* inheritance_cache;

	//struct _zend_property_info **properties_info_table;
  void* properties_info_table;

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
	uint num_hooked_props;
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

	zend_string *doc_comment;

	static union _info {
		static struct _user {
			zend_string *filename;
			uint line_start;
			uint line_end;
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
enum zend_prop_purpose {
	ZEND_PROP_PURPOSE_DEBUG,
	ZEND_PROP_PURPOSE_ARRAY_CAST,
	ZEND_PROP_PURPOSE_SERIALIZE,
	ZEND_PROP_PURPOSE_VAR_EXPORT,
	ZEND_PROP_PURPOSE_JSON,
	ZEND_PROP_PURPOSE_GET_OBJECT_VARS,
	_ZEND_PROP_PURPOSE_NON_EXHAUSTIVE_ENUM
}
alias zend_object_get_properties_for_t = zend_array *function(zend_object *object, zend_prop_purpose purpose);
alias zend_object_get_method_t = zend_function *function(zend_object **object, zend_string *method, const(zval) *key);
alias zend_object_get_constructor_t = zend_function *function(zend_object *object);
alias zend_object_free_obj_t = void function(zend_object *object);
alias zend_object_dtor_obj_t = void function(zend_object *object);
alias zend_object_clone_obj_t = zend_object* function(zend_object *object);
alias zend_object_get_class_name_t = zend_string *function(const(zend_object) *object);
alias zend_object_compare_t = int function(zval *object1, zval *object2);
alias zend_object_cast_t = zend_result function(zend_object *readobj, zval *retval, int type);
alias zend_object_count_elements_t = zend_result function(zend_object *object, long *count);
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


/** Annotation for function parameters mapped to a pointer types (PHP classe,
 *  resources, HashTables, Strings) that can be null. */
struct nullable {}

/** annotation for structs that are on the PHP side mapped to classes,
 * resources or FFI objects */
struct phpClass {}
struct phpResource {}
struct FFI {}
struct userlandClass {}

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
enum isUserlandClass(T) = hasAttribute!(T, userlandClass);

private enum isPHPClass_(attrs...) = hasAttribute_!(phpClass, attrs);
private enum isResource_(attrs...) = hasAttribute_!(phpResource, attrs);
private enum isFFIType_(attrs...)  = hasAttribute_!(FFI, attrs);
private enum isNullable(attrs...)  = hasAttribute_!(nullable, attrs);



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
    // TODO lift this limitation
    static assert(is(typeof(T.init.tupleof[$-1]) == zend_object), "last field of a struct implementing PHP class has to be zend_object field named 'std'");

    enum methods = PublicMethods!T;
    enum name = __traits(identifier, T);

    immutable static zend_function_entry[methods.length + 1] functions = [MakeMethods!T];

    handlers = std_object_handlers;
    handlers.offset = T.std.offsetof;
    //handlers.free_obj = &free_obj;

    zend_class_entry ce;

    { // INIT_CLASS_ENTRY
      memset(&ce, 0, zend_class_entry.sizeof);
      ce.name = zend_string_init_interned(name, name.length, 1);
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

  T* getNativeType(zend_object* obj) {
    pragma(inline, true);
    return cast(T*) ((cast(ubyte*) obj) - T.std.offsetof);
  }

  zend_object* getPHPType(T* x) {
    pragma(inline, true);
    return cast(zend_object*) ((cast(ubyte*) x) + T.std.offsetof);
  }
}


private zend_object* createObject(T)(zend_class_entry* ce) @nogc nothrow {
  // zend_object_alloc(sizeof(T), ce)

  // Class have no declared properties.
  // TODO If an object have guards (ce->ce_flags & ZEND_ACC_USE_GUARDS), the
  // last zval doesn't need to be allocated. But it causes problem with
  // initialization when emplace overwrites tail zval.
  enum size = T.sizeof;// - zval.sizeof;
  T* obj = cast(T*) _emalloc(size);
  emplace!T(obj);
  zend_object_std_init(&obj.std, ce);
  return &obj.std;
}


template PublicMethods(T) {
  alias PublicMethods = AliasSeq!();
  static foreach (member; __traits(allMembers, T)) {
    static if (
           is(typeof(mixin("T.", member)) == return) // is a function
        && !__traits(isStaticFunction, __traits(getMember, T, member))
        && member != "opAssign" // generated for structs with destructors
        && member != "__xdtor"
    ) {
      PublicMethods = AliasSeq!(PublicMethods, member);
    }
  }
}



template MakeMethods(T) {
  alias MakeMethods = AliasSeq!();
  static foreach (method; PublicMethods!T) {
    static if (method == "__ctor") {
      MakeMethods = AliasSeq!(MakeMethods,
          makeFunctionEntry!(MakeConstructor!(T), "__construct", false, true));
    } else static if (method == "__dtor") {
      MakeMethods = AliasSeq!(MakeMethods,
          makeFunctionEntry!(MakeDestructor!(T), "__destruct", false, true));
    } else {
      MakeMethods = AliasSeq!(MakeMethods,
          makeFunctionEntry!(MakeMethod!(T, method), method, false, true));
    }
  }
}

template MakeMethod(T, alias method) {
  private alias argTypes = Parameters!(mixin("T.", method));
  static auto MakeMethod(zend_object* obj, argTypes args) {
    pragma(inline, true);
    T* _this = cast(T*) ((cast(ubyte*) obj) - T.std.offsetof);
    return mixin("_this.", method)(args);
  }
}

template MakeConstructor(T) {
  private alias argTypes = Parameters!(T.__ctor);
  //static zend_object* MakeConstructor(zend_object* obj, scope argTypes args) {
  static void MakeConstructor(zend_object* obj, scope argTypes args) {
    pragma(inline, true);
    T* _this = cast(T*) ((cast(ubyte*) obj) - T.std.offsetof);
    _this.__ctor(args);
    //return &_this.std;
  }
}

template MakeDestructor(T) {
  static void MakeDestructor(zend_object* obj) {
    pragma(inline, true);
    T* _this = cast(T*) ((cast(ubyte*) obj) - T.std.offsetof);
    _this.__dtor();
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

  auto make(void* ptr, int resNum) {
    return zend_register_resource(ptr, resNum);
  }

  int fileno() {
    if (type != php_file_le_stream()) return -1;
    void* ret;
    _php_stream_cast(cast(php_stream*) ptr, 1, &ret, 0);
    return cast(int) ret;
  }
}


void registerResource(T)(int moduleNumber, void function(Resource*) destructor) {
  phpResourceRegistry!T.register(moduleNumber, destructor);
}

struct phpResourceRegistry(T) {
  @nogc nothrow:

  private enum const(char)* name = T.stringof;
  private static __gshared int resourceNumber = -1;

  static void register(int moduleNumber, void function(Resource*) destructor) {
    resourceNumber = zend_register_list_destructors_ex(destructor, null, name, moduleNumber);
  }

  static auto allocate() {
    auto x = cast(T*) _emalloc(T.sizeof);
    emplace!T(x);
    return x;
  }

  static zend_resource* wrap(T* x) {
    assert(resourceNumber != -1, "resource type not registered (missing type.register() call in module_startup_func?)");
    return zend_register_resource(cast(void*) x, resourceNumber);
  }

  static T* getNativeType(Resource* res) {
    pragma(inline, true);
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
	zend_object *closure; /* Closure reference, only if the callable *is* the object */
}



// === modules ===

alias ModuleEntry = zend_module_entry;

struct zend_module_entry {
  ushort size = zend_module_entry.sizeof;
  uint zend_api = 20240924; //20220829;
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
  const char* build_id = "API20240924,NTS"; // "API20220829,NTS";
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
  const zend_frameless_function_info *frameless_function_infos;
  const(char)* doc_comment;
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




mixin template mod(alias _module, alias string name = "") if (__traits(isModule, _module)) {
  ModuleEntry __mod = {
    name: name == "" ? __traits(identifier, _module) : name,
    version_: "1",
    functions: [
      WrapFunctions!_module,
      FunctionEntry(),
    ],
    moduleStartup: (int, int moduleNumber) {
      registerClasses!_module(moduleNumber);
      return Result.SUCCESS;
    }
  };
  extern(C) ModuleEntry* get_module() {
    // workaround: without this our functions generated by templates in the
    // mixin don't get included in compiled .so file
    alias _1 = WrapFunctions!_module;
    alias _2 = registerClasses!_module;
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

  private void registerClasses(alias _module)(int moduleNumber) {
    static foreach (sym; __traits(allMembers, _module)) {{
      static if (is(mixin(sym) Class == struct)) {
        static if (isPHPClass!(Class)) {
          registerClass!Class;
        } else static if (isResource!Class) {
          static void dtor(zend_resource* res) {
            auto rr = cast(Class*) res.ptr;
            static if (is(typeof(rr.__dtor))) {
              rr.__dtor();
            }
          }
          registerResource!Class(moduleNumber, &dtor);
        }
        // TODO register FFI and userland class descriptors
      }
    }}
  }

}

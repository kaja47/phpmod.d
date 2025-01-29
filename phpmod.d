// license: GPL3

/**
 * todo:
 *  - ffi datatypes
 *  - native destructor should be executed in handlers->free_obj
 *  - allocate smaller objects when guards are not used
 *
 * Not yet supported:
 *  - ZTS
 *  - PHP debug builds (API of some functions is changed)
 *
 * Never supported (aka I don't care about these things):
 *  - crummy typing rules and coercions following exact PHP semantic
 *  - PHP references.
 *    Internally references are different type but in PHP userspace they are
 *    indistinguishable from type they reference. This poisons the PHP codebase
 *    where we need to constantly checking for references to unwrap them even
 *    though references are almost never used.
 *  - older PHP versions
 *    I currently target PHP 8.4 and long term I will target whatever version
 *    PHP in packaged in Debian unstable
 *  - 32-bit platforms
 *    On 32-bit machines layout of all types might be different. Keeping
 *    everything in sync would be too much extra effort for very little gain.
 *    Even Raspberry Pi Zero 2 has 64 bit CPU nowadays and it costs almost
 *    nothing.
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

  void* _emalloc(size_t size);
  void* _efree(void* ptr);

  void zval_ptr_dtor(zval *zval_ptr);

  @attribute("cold") void zend_wrong_parameters_count_error(uint min_num_args, uint max_num_args);
  @attribute("cold") void zend_argument_type_error(uint arg_num, const char *format, ...);
  @attribute("cold") void zend_type_error(const char *format, ...);
  @attribute("cold") Object* zend_throw_exception(zend_class_entry* exception_ce, const(char)* message, long code);
  @attribute("cold") Object* zend_throw_exception_ex(zend_class_entry* exception_ce, long code, const(char)* format, ...);


  zval* zend_hash_index_update(HashTable *ht, ulong h, zval *pData);

  alias zend_string_init_interned_func_t = zend_string * function(const char *str, size_t size, bool permanent);
  zend_string_init_interned_func_t zend_string_init_interned;



  int zend_register_list_destructors_ex(rsrc_dtor_func_t ld, rsrc_dtor_func_t pld, const(char) *type_name, int module_number);
  zend_resource* zend_register_resource(void* rsrc_pointer, int rsrc_type);

  struct php_stream;
  int _php_stream_cast(php_stream *stream, int castas, void **ret, int show_err);
  int php_file_le_stream();

  alias CacheSlot = void*[3];
  zval* zend_std_read_property(zend_object* zobj, zend_string* name, int type, CacheSlot* cache_slot, zval* rv);



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
}

enum ZEND_ACC_PUBLIC                 = (1 <<  0);
enum ZEND_ACC_PROTECTED              = (1 <<  1);
enum ZEND_ACC_PRIVATE                = (1 <<  2);
enum ZEND_ACC_FINAL                  = (1 <<  5);

alias rsrc_dtor_func_t = void function(zend_resource*);



T* emalloc(T)() @nogc nothrow {
  pragma(inline, true);
  return cast(T*) _emalloc(T.sizeof);
}

/** Variant of exception modified to accept C-style zero terminated string.
 *  It's only convenience feature, phpmod.d can catch any native exception and
 *  rethrow it on PHP side. */
class PHPException : Exception {
  const(char)* _msg;
  pure nothrow @nogc @safe this(const(char)* message, string file = __FILE__, size_t line = __LINE__, Throwable nextInChain = null) {
    super("", file, line, nextInChain);
    this._msg = message;
  }
}


/** Wrap a native function 'f' and generate function entry structure needed for
  * PHP to be able to call it. */
const(zend_function_entry) func(alias f, alias phpName = "")() {
  static if (phpName == "") {
    enum _phpName = __traits(identifier, f);
  } else {
    alias _phpName = phpName;
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
              __traits(identifier, ParamTypes[i .. i + 1]),
              TypeHint!(ParamTypes[i], __traits(getAttributes, ParamTypes[i .. i + 1])),
              DefaultArgStr!(f, i)
            ));
      }
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
  else static if (is(T : const(String)*))      return zend_type(null, (1 << Type.IsString) | nullMask);
  else static if (is(T : const(HashTable)*))   return zend_type(null, (1 << Type.IsArray)  | nullMask);
  else static if (is(T : const(zend_object)*)) return zend_type(null, (1 << Type.IsObject) | nullMask);
  // for some reason the following line leads to segfaults, it seems like that
  // resource parameters may be not type-hinted (eg. reflection indicates the
  // first argument of fseek function has no type-hint)
  //else static if (is(T : const(Resource)*)) auto type = zend_type(null, 1 << Type.IsResource);
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
 * can be called via frameless mechanism. It's a new feature added to PHP 8.4
 * to call functions more efficiently. It doesn't allocate PHP stack frame
 * (hence frameless) and arguments are passed directly in function arguments
 * (which means in registers). Because our stricter type handling, we do not
 * need to care about one corner case when scalar type is coerced to string,
 * which have to be eventually cleared.
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

  static foreach (_i, PT; ParamTypes) {{

    static if (_i == 0 && firstArgIsThis) {
      static assert(is(PT == zend_object*));
      args[0] = ex.This.obj;

    } else {
      // 'i' refers to position of php argument not argument of f. It excludes
      // synthetic 'this'.
      enum i = _i - firstArgIsThis;

      static if (frameless) {
        zval* arg = zvalArgs!i;
      } else {
        zval* arg = zvalArgs + i;
      }

      static if (!frameless && i >= reqNumParams) {
        zval _z;
        if (i >= numArgs) {
          _z = zval(DefaultArg!(f, _i)); // using _i intentionally
          arg = &_z;
        }
      }

      // we need to check for attributes on parameter like this (extract list
      // of attributes and them pass them to some template, instead of
      // passing the parameter symbol directly) because it seems that
      // attributes attached to a symbol representing a function parameter
      // don't survive passing as template parameter
      alias attrs = __traits(getAttributes, ParamTypes[_i .. _i + 1]);

      static if (isIntegral!PT) {
        static if (is(PT == ulong)) {
          static assert(0, "cannot handle ulong arguments (yet)");
        }

        if (arg.type != Type.IsLong) {
          zend_argument_type_error(i+1, "expected long, %s given", typeNames[arg.type].ptr);
          return;
        }

        long val = arg.lval;

        static if (!is(PT == long)) {
          if (val < PT.min || val > PT.max) {
            zend_argument_type_error(i+1, "int out %d of range [%d .. %d]", val, cast(long) PT.min, cast(long) PT.max);
            return;
          }
        }

        args[_i] = cast(PT) val;

      } else static if (is(PT == double) || is(PT == float)) {
        if (arg.type != Type.IsDouble) {
          zend_argument_type_error(i+1, "expected double, %s given", typeNames[arg.type].ptr);
          return;
        }
        args[_i] = cast(PT) arg.dval;

      } else static if (is(PT == bool)) {
        bool val;
        if (arg.type == Type.IsTrue) {
          val = true;
        } else if (arg.type == Type.IsFalse) {
          val = false;
        } else {
          zend_argument_type_error(i+1, "expected bool, %s given", typeNames[arg.type].ptr);
          return;
        }
        args[_i] = val;

      } else static if (is(PT : const(char)[]) || (is(PT : const(X)[], X) && isIntegral!X)) {
        enum storageClasses = __traits(getParameterStorageClasses, f, _i);
        enum isScope(alias sc) = sc == "scope";
        static if (!anySatisfy!(isScope, storageClasses)) {
          static assert(0, "arrays have to be passed as scope arguments");
        }
        if (arg.type != Type.IsString) {
          zend_argument_type_error(i+1, "expected string, %s given", typeNames[arg.type].ptr);
          return;
        }
        args[_i] = cast(PT) arg.str.str[];

      } else static if (is(PT : const(String)*)) {
        if (arg.type == Type.IsString) {
          args[_i] = arg.str;
        } else if (isNullable!attrs && arg.type == Type.IsNull) {
          args[_i] = null;
        } else {
          zend_argument_type_error(i+1, "expected string, %s given", typeNames[arg.type].ptr);
          return;
        }

      } else static if (is(PT : const(HashTable)*)) {
        if (arg.type == Type.IsArray) {
          args[_i] = arg.arr;
        } else if (isNullable!attrs && arg.type == Type.IsNull) {
          args[_i] = null;
        } else {
          zend_argument_type_error(i+1, "expected array, %s given", typeNames[arg.type].ptr);
          return;
        }

      } else static if (is(PT : const(Resource)*)) {
        if (arg.type == Type.IsResource) {
          args[_i] = arg.res;
        } else if (isNullable!attrs && arg.type == Type.IsNull) {
          args[_i] = null;
        } else {
          zend_argument_type_error(i+1, "expected resource, %s given", typeNames[arg.type].ptr);
          return;
        }

      } else static if (is(PT : const(zend_object)*)) {
        if (arg.type == Type.IsObject) {
          args[_i] = arg.obj;
        } else if (isNullable!attrs && arg.type == Type.IsNull) {
          args[_i] = null;
        } else {
          zend_argument_type_error(i+1, "expected object, %s given", typeNames[arg.type].ptr);
          return;
        }

      } else static if (is(PT == zval*)) {
        args[_i] = arg;

      } else static if (is(PT == zval)) {
        args[_i] = *arg;

      } else static if (is(PT == T*, T)) {

        static if (isPHPClass!T || isPHPClass_!attrs) {
          if (arg.type == Type.IsObject) {
            const(zend_class_entry)* ce = phpClassRegistry!T.class_entry;
            if (arg.obj.ce == ce)  {
              args[_i] = phpClassRegistry!T.getNativeType(arg.obj);
            } else {
              zend_argument_type_error(i+1, "class type mismatch");
              return;
            }
          } else if (isNullable!attrs && arg.type == Type.IsNull) {
            args[_i] = null;
          } else {
            zend_argument_type_error(i+1, "expected object, %s given", typeNames[arg.type].ptr);
            return;
          }

        } else static if (isResource!T || isResource_!attrs) {
          if (arg.type == Type.IsResource) {
            auto res = phpResourceRegistry!T.getNativeType(arg.res);
            if (res == null) {
              zend_argument_type_error(i+1, "expected %s resource", T.stringof.ptr);
              return;
            }
            args[_i] = res;
          } else if (isNullable!attrs && arg.type == Type.IsNull) {
            args[_i] = null;
          } else {
            zend_argument_type_error(i+1, "expected resource, %s given", typeNames[arg.type].ptr);
            return;
          }

        } else static if (isFFIType!T) {
          T* resArg;
          bool ok = getFFIArg!(T)(i, arg, &resArg);
          if (!ok) return;
          args[_i] = resArg;

        } else {
          //pragma(msg, attrs);
          static assert(0);
        }

      } else static if (is(PT == T[], T) && isFFIType!T) {
        T[] resArg;
        bool ok = getFFIArgArr!(T)(i, arg, &resArg);
        if (!ok) return;
        args[_i] = resArg;

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
    zend_throw_exception_ex(null, 0, "%s (%.*s:%d)", msg, e.file.length, e.file.ptr, e.line);

  } catch (Exception e) {
    zend_throw_exception_ex(null, 0, "%.*s (%.*s:%d)", e.msg.length, e.msg.ptr, e.file.length, e.file.ptr, e.line);
  }
}


// we use array of size 256 to eliminate bounds check in error messages
private static string[256] typeNames = [
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



// === zval ===

static assert(zval.sizeof == 16);

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

  // little endian
  static struct v_ {
    Type    type;       /* active type */
    ubyte   type_flags;
    ushort  extra;     /* not further specified */
  }

  u1_ u1;

  Type type() nothrow { return u1.v.type; }

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
    u1.v.type_flags = IS_TYPE_REFCOUNTED;
    //u1.type_info = Z_TYPE_INFO_P(__z) = ZSTR_IS_INTERNED(__s) ? IS_INTERNED_STRING_EX :  IS_STRING_EX;
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


  long* asLong() return       nothrow { return type == Type.IsLong ? &lval : null; }
  double* asDouble() return   nothrow { return type == Type.IsDouble ? &dval : null; }
  //bool* asBool()              { return type == Type.IsBool ? _ : null; }
  HashTable* asArray()        nothrow { return type == Type.IsArray ? arr : null; }
  zend_object* asObject()     nothrow { return type == Type.IsObject ? obj : null; }
  zend_string* asString()     nothrow { return type == Type.IsString ? str : null; }
  zend_resource* asResource() nothrow { return type == Type.IsResource ? res : null; }

  long toLong() return        { if (type != Type.IsLong)     throw new Exception("bad type"); return lval; }
  double toDouble() return    { if (type != Type.IsDouble)   throw new Exception("bad type"); return dval; }
  bool toBool()               { if (type != Type.IsTrue && type != Type.IsFalse) throw new Exception("bad type"); return type == Type.IsTrue; }
  HashTable* toArray()        { if (type != Type.IsArray)    throw new Exception("bad type"); return arr; }
  zend_object* toObject()     { if (type != Type.IsObject)   throw new Exception("bad type"); return obj; }
  //zend_string* toString()     { if (type != Type.IsString)   throw new Exception("bad type"); return str; }
  zend_resource* toResource() { if (type != Type.IsResource) throw new Exception("bad type"); return res; }


  // This is there to supress warning about safety in gdc 14. We shouldn't use zvals as keys anyways.
  size_t toHash() const @safe pure nothrow { return lval; }



  // TODO
  void addRef()    nothrow { counted.gc.refcount++; }
  void delRef()    nothrow { counted.gc.refcount--; }
  void tryAddRef() nothrow { assert(0); }
  void tryDelRef() nothrow { assert(0); }
  void destroy()   nothrow { zval_ptr_dtor(&this); }

  }

  void toString(void delegate(const(char)[]) sink) {
    sink("zval(");
    sink(typeNames[type]);
    sink(")");
  }
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

struct zend_refcounted {
  zend_refcounted_h gc;
}

struct zend_refcounted_h {
  uint refcount;
  union _u {
    uint type_info;
    /*
    struct {
      import std.bitmanip;
      mixin(bitfields!(
        Type, "type",   4,

        //uint, "flags",  6,
        bool, "notCollectable", 1,
        bool, "protected", 1,
        bool, "immutable", 1,
        bool, "persistent", 1,
        bool, "persistentLocal", 1,
        bool, "___", 1,

        uint, "info",  22
      ));
    }
    */
  }

  _u u;

  pragma(inline, true):

    /*
  auto gcType() {
    return (type_info & GC_TYPE_MASK);
  }

  auto gcFlags() {
    return (type_info >> GC_FLAGS_SHIFT) & (GC_FLAGS_MASK >> GC_FLAGS_SHIFT);
  }

  auto gcInfo() {
    return (type_info >> GC_INFO_SHIFT);
  }
  */
}



// === string ===

alias String = zend_string;

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


  static String* alloc(size_t len, bool persistent = false) {
    enum ZEND_MM_ALIGNMENT = 8;
    enum ZEND_MM_ALIGNMENT_MASK = ~(ZEND_MM_ALIGNMENT - 1);
    alias ZEND_MM_ALIGNED_SIZE = (size) => (size + ZEND_MM_ALIGNMENT - 1) & ZEND_MM_ALIGNMENT_MASK;

    zend_string *ret = cast(zend_string*) _emalloc(ZEND_MM_ALIGNED_SIZE(zend_string.sizeof + len + 1));

    enum GC_NOT_COLLECTABLE  = (1<<4);
    enum GC_PROTECTED        = (1<<5); /* used for recursion detection */
    enum GC_IMMUTABLE        = (1<<6); /* can't be changed in place */
    enum GC_PERSISTENT       = (1<<7); /* allocated using malloc */
    enum GC_PERSISTENT_LOCAL = (1<<8); /* persistent, but thread-local */

    enum GC_FLAGS_SHIFT = 0;

    enum GC_NULL         = (Type.IsNull        | (GC_NOT_COLLECTABLE << GC_FLAGS_SHIFT));
    enum GC_STRING       = (Type.IsString      | (GC_NOT_COLLECTABLE << GC_FLAGS_SHIFT));
    enum GC_ARRAY        =  Type.IsArray;
    enum GC_OBJECT       =  Type.IsObject;
    enum GC_RESOURCE     = (Type.IsResource    | (GC_NOT_COLLECTABLE << GC_FLAGS_SHIFT));
    enum GC_REFERENCE    = (Type.IsReference   | (GC_NOT_COLLECTABLE << GC_FLAGS_SHIFT));
    //enum GC_CONSTANT_AST = (Type.IsConstantAst | (GC_NOT_COLLECTABLE << GC_FLAGS_SHIFT));

    enum IS_STR_CLASS_NAME_MAP_PTR = GC_PROTECTED;  /* refcount is a map_ptr offset of class_entry */
    enum IS_STR_INTERNED           = GC_IMMUTABLE;  /* interned string */
    enum IS_STR_PERSISTENT         = GC_PERSISTENT; /* allocated using malloc */
    enum IS_STR_PERMANENT          = (1<<8);        /* relives request boundary */
    enum IS_STR_VALID_UTF8         = (1<<9);        /* valid UTF-8 according to PCRE */

    alias GC_TYPE_INFO = (p) => p.gc.type_info;
    alias GC_TYPE      = (p) => p.gc.gcType;
    alias GC_FLAGS     = (p) => p.gc.gcFlags;
    alias GC_INFO      = (p) => p.gc.gcInfo;

    ret.gc.refcount = 1;
    ret.gc.u.type_info = GC_STRING | ((persistent ? IS_STR_PERSISTENT : 0) << GC_FLAGS_SHIFT);
    ret.h = 0;
    ret.len = len;
    ret.val[len] = 0; // null terminated
    return ret;
  }

  static String* copy(const(char)[] str, bool persistent = false) {
    auto s = String.alloc(str.length, persistent);
    s.str[] = str[];
    return s;
  }


  static String* copy(const(ubyte)[] str, bool persistent = false) {
    return copy(cast(const(char)[]) str, persistent);
  }
}



// === array ===

struct Bucket {
  zval         val;
  ulong        h;                /* hash value (or numeric index)   */
  zend_string* key;              /* string key or NULL for numerics */
}

alias zend_array = HashTable;

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

  static struct Iterator(alias string what) {
    @nogc nothrow:
    HashTable* ht;
    zval* pos, end;
    size_t step;
    long idx = 0;

    this(HashTable* ht) {
      this.ht = ht;
      step = ht.isPacked ? 1 : 2;
      pos = ht.arPacked;
      end = ht.arPacked + ht.nNumUsed * step;
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
        return pos;

      } else static if (what == "kv") {
        struct KV { zval key; zval* value; }
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


  auto byValue()    { return Iterator!"v"(&this); }
  //auto byKey()      { return Iterator!"k"(&this); }
  auto byKeyValue() { return Iterator!"kv"(&this); }



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
  auto typed() @nogc inout {
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
        if (ht.isPacked) {
          long i = 0;
          for (zval* z = ht.arPacked; z < ht.arPacked + ht.nNumUsed; z++, i++) {
            if (z.type == Type.IsUndef) continue;
            K k = extractPackedKey!K(i);
            V v = extractValue!V(z);
            int result = dg(k, v);
            if (result) return result;
          }

        } else {
          char[32] buf = void;

          for (Bucket* b = ht.arData; b < ht.arData + ht.nNumUsed; b++) {
            if (b.val.type == Type.IsUndef) continue;
            K k = extractKey!K(b, buf[]);
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


  /// TODO
  static HashTable* copy(T)(T[] arr) if (isIntegral!T || is(T == float) || is(T == double) || is(T == bool) || is(T : const(char)[])) {
    assert(0);
  }

}

private T extractKey(T)(scope return Bucket* b, scope char[] buf) @nogc {
  static if (isIntegral!T) {
    if (b.key != null) {
      throw new Exception("int key expected");
    }
    long k = b.h;
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
      throw new Exception("string key expected");
      // TODO
      //auto l = snprintf(buf.ptr, buf.length, "%ld", b.h);
      //return buf[0 .. l];
    }
    return cast(T) b.key.str();

  } else static if (is(T == zval)) {
    return b.key == null ? zval(b.h) : zval(b.key);

  } else static assert(0, "cannot handle argument of type " ~ T.stringof);
}

private T extractPackedKey(T)(long k) @nogc {
  static if (isIntegral!T) {
    static if (!is(T == long)) {
      if (k < T.min || k > T.max) {
        throw new Exception("int key out of range");
      }
    }
    return cast(T) k;

  } else static if (is(T : const(char)[]) || is(T : const(ubyte)[]) || is(T : zend_string*)) {
    throw new Exception("string key expected");

  } else static if (is(T == zval)) {
    return zval(k);

  } else static assert(0, "cannot handle argument of type " ~ T.stringof);
}


private T extractValue(T)(scope return zval* z) @nogc {
  static if (isIntegral!T) {
    static if (is(T == ulong)) assert(0, "cannot handle unsigned longs");
    if (z.type != Type.IsLong) {
      throw new Exception("int value expected");
    }
    long val = z.lval;
    static if (!is(T == long)) {
      if (val < T.min || val > T.max) {
        throw new Exception("int value out of range");
      }
    }
    return cast(T) val;
  } else static if (isFloatingPoint!T) {
    if (z.type != Type.IsDouble) {
      throw new Exception("double value expected");
    }
    return cast(T) z.dval;
  } else static if (is(T : const(char)[])) {
    if (z.type != Type.IsString) {
      throw new Exception("string value expected");
    }
    return z.str.str();
  } else static if (is(T : zend_string*)) {
    if (z.type != Type.IsString) {
      throw new Exception("string value expected");
    }
    return z.str;
  } else static if (is(T : zval*)) {
    return z;
  } else static assert("cannot handle "~__traits(identifier, T));
}



// === objects ===

//alias Object = zend_object;

struct zend_object {
  @nogc nothrow:

  zend_refcounted_h gc;
  uint              handle;
  uint              extra_flags; /* OBJ_EXTRA_FLAGS() */ // since 8.4
  zend_class_entry* ce;
  const(zend_object_handlers)* handlers;
  HashTable*       properties;
  zval[1]          properties_table;

  zval* readProperty(String* name, int type, void** cache_slot, zval* rv) {
    // TODO
    //return zend_std_read_property(&this, name, type, cache_slot, rv);
    assert(0);
  }

  zval* property(alias string p)() {
    auto name = cast(String*) alloca(String.sizeof + p.length + 1);
    name.len = p.length;
    name.str[] = p[];
    return zend_std_read_property(&this, name, BP_VAR_R, null, null);
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


/** Annotation for function parameters mapped to a pointer types (PHP classe,
 *  resources, HashTables, Strings) that can be null. */
struct nullable {}

/** annotation for structs that are on the PHP side mapped to classes,
 * resources or FFI objects */
struct phpClass {}
struct phpResource {}
struct FFI {}
private template hasAttribute(T, A) {
  enum _is(alias a) = is(a == A);
  enum hasAttribute = anySatisfy!(_is, __traits(getAttributes, T));
}
private template hasAttribute_(A, Attrs...) {
  enum _is(alias a) = is(a == A);
  enum hasAttribute_ = anySatisfy!(_is, Attrs);
}
private enum isPHPClass(T) = hasAttribute!(T, phpClass);
private enum isResource(T) = hasAttribute!(T, phpResource);
private enum isFFIType(T)  = hasAttribute!(T, FFI);

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
    static assert(is(typeof(T.init.tupleof[$-1]) == zend_object), "last field of a struct implementing PHP class has to be zend_object named 'std'");

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
    *x = T.init;
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
  void* globals_ptr;
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




// FFI datatypes support

alias zend_long = long;

struct zend_ffi_cdata {
  zend_object            std;
  zend_ffi_type         *_type;
  void                  *ptr;
  void                  *ptr_holder;
  zend_ffi_flags         flags;

  zend_ffi_type* type() { return FFI_TYPE(_type); }
}

enum zend_ffi_flags {
  ZEND_FFI_FLAG_CONST      = (1 << 0),
  ZEND_FFI_FLAG_OWNED      = (1 << 1),
  ZEND_FFI_FLAG_PERSISTENT = (1 << 2),
}


enum ZEND_FFI_TYPE_OWNED = (1<<0);

// pointers to ffi_type are marked
private alias FFI_TYPE = (zend_ffi_type *t) => cast(zend_ffi_type*) ((cast(uintptr_t)t) & ~ZEND_FFI_TYPE_OWNED);

struct zend_ffi_type {
  zend_ffi_type_kind     kind;
  size_t                 size;
  uint               align_;
  uint               attr;
  union {
    struct _enumeration {
      zend_string        *tag_name;
      zend_ffi_type_kind  kind;
    }
    _enumeration enumeration;
    struct _array {
      zend_ffi_type *_type;
      zend_long      length;
      zend_ffi_type* type() { return FFI_TYPE(_type); }
    }
    _array array;
    struct _pointer {
      zend_ffi_type *_type;
      zend_ffi_type* type() { return FFI_TYPE(_type); }
    }
    _pointer pointer;
    struct _record {
      zend_string   *tag_name;
      HashTable      fields;
    }
    _record record;
    struct _func {
      zend_ffi_type *_ret_type;
      HashTable     *args;
      int abi;
      //ffi_abi        abi;
      zend_ffi_type* ret_type() { return FFI_TYPE(_ret_type); }
    }
    _func func;
  }
}

enum zend_ffi_type_kind {
  ZEND_FFI_TYPE_VOID,
  ZEND_FFI_TYPE_FLOAT,
  ZEND_FFI_TYPE_DOUBLE,
//#ifdef HAVE_LONG_DOUBLE
  ZEND_FFI_TYPE_LONGDOUBLE,
//#endif
  ZEND_FFI_TYPE_UINT8,
  ZEND_FFI_TYPE_SINT8,
  ZEND_FFI_TYPE_UINT16,
  ZEND_FFI_TYPE_SINT16,
  ZEND_FFI_TYPE_UINT32,
  ZEND_FFI_TYPE_SINT32,
  ZEND_FFI_TYPE_UINT64,
  ZEND_FFI_TYPE_SINT64,
  ZEND_FFI_TYPE_ENUM,
  ZEND_FFI_TYPE_BOOL,
  ZEND_FFI_TYPE_CHAR,
  ZEND_FFI_TYPE_POINTER,
  ZEND_FFI_TYPE_FUNC,
  ZEND_FFI_TYPE_ARRAY,
  ZEND_FFI_TYPE_STRUCT,
}

struct zend_ffi_field {
  size_t            offset;
  bool              is_const;
  bool              is_nested; /* part of nested anonymous struct */
  ubyte             first_bit;
  ubyte             bits;
  zend_ffi_type     *type;
}



// assume class entry for ffi CData is stable for whole process lifetime
// should be real request global TODO
private zend_class_entry* ffiCE;




private bool getFFIArgArr(T)(uint i, zval* arg, T[]* res) {
  if (arg.type != Type.IsObject) {
    zend_argument_type_error(i+1, "expected object, %s given", typeNames[arg.type].ptr);
    return false;
  }

  // TODO should be real request globals
  //writeln(&getFFIArgArr!(T[]));
  static zend_ffi_type* ffiType = null;

  zend_object* o = arg.asObject;
  auto cdata = cast(zend_ffi_cdata*) o;

  if (ffiCE == o.ce && cdata.type.kind == zend_ffi_type_kind.ZEND_FFI_TYPE_ARRAY && cdata.type.array.type == ffiType) {
valid:
    auto len = cdata.type.array.length;
    auto arr = (cast(T*) cdata.ptr)[0 .. len];
    *res = arr;
    return true;
  }

  if (o.ce.name.str() != "FFI\\CData") {
    zend_argument_type_error(i+1, "expected FFI\\CData");
    return false;
  }

  auto type = cdata.type;
  if (type.kind != zend_ffi_type_kind.ZEND_FFI_TYPE_ARRAY) {
    zend_argument_type_error(i+1, "expected FFI array");
    return false;
  }

  if (!compatibleFFIStruct!T(type.array.type, i)) {
    return false;
  }

  ffiCE = o.ce;
  ffiType = type.array.type;
  goto valid;
}



private bool getFFIArg(T)(uint i, zval* arg, T** res) {
  if (arg.type != Type.IsObject) {
    zend_argument_type_error(i+1, "expected object");
    return false;
  }

  // TODO should be real request globals
  static zend_ffi_type* ffiType = null;

  zend_object* o = arg.obj;
  auto cdata = cast(zend_ffi_cdata*) o;

  if (ffiCE == o.ce && cdata.type == ffiType) {
    *res = cast(T*) cdata.ptr;
    return true;
  }

  if (o.ce.name.str() != "FFI\\CData") {
    zend_argument_type_error(i+1, "expected FFI\\CData");
    return false;
  }

  if (!compatibleFFIStruct!T(cdata.type, i)) {
    return false;
  }

  ffiCE = o.ce;
  *res = cast(T*) cdata.ptr;
  return true;
}


/// Check if FFI type declared on PHP side matches declaration on native side.
private bool compatibleFFIPtr(T)(zend_ffi_type* type, int i) {
  if (type.kind != zend_ffi_type_kind.ZEND_FFI_TYPE_POINTER) {
    zend_argument_type_error(i+1, "FFI type mismatch (not a pointer)");
    return false;
  }
  return compatibleFFIStruct!T(type.pointer.type, i);
}


/// ditto
private bool compatibleFFIStruct(T)(zend_ffi_type* type, int i) {
  if (type.kind != zend_ffi_type_kind.ZEND_FFI_TYPE_STRUCT) {
    zend_argument_type_error(i+1, "FFI type mismatch (not a struct)");
    return false;
  }
  if (type.size != T.sizeof) {
    zend_argument_type_error(i+1, "FFI type mismatch (different sizes)");
    return false;
  }
  if (type.align_ != T.alignof) {
    zend_argument_type_error(i+1, "FFI type mismatch (different alignments)");
    return false;
  }

  HashTable* fields = &type.record.fields;
  if (fields.length != Fields!T.length) {
    zend_argument_type_error(i+1, "FFI type mismatch (number of fields doesn't match)");
    return false;
  }

  struct NativeField { size_t offset; size_t size;}
  NativeField[T.tupleof.length] nativeFields;
  static foreach (j, n; T.tupleof) {
    nativeFields[j] = NativeField(T.tupleof[j].offsetof, T.tupleof[j].sizeof);
  }

  int j = 0;
  bool ok = true;
  foreach (_, v; fields.byKeyValue) {
    auto f = cast(zend_ffi_field*) v.ptr;
    ok &= (f.offset == nativeFields[j].offset) && (f.type.size == nativeFields[j].size);
    j++;
  }
  if (!ok) {
    zend_argument_type_error(i+1, "FFI type mismatch (struct layout doesn't match)");
    return false;
  }
  return true;
}



auto enableFFI(alias _module)() {
  enum moduleName = __traits(identifier, _module);
  enum hasFFI(alias a) = is(a == FFI);

  enum isFFIStruct(alias m) =
    is(typeof(mixin(moduleName~"."~m)()) == struct) &&
    anySatisfy!(hasFFI, __traits(getAttributes, typeof(mixin(moduleName~"."~m)())));

  mixin("import "~moduleName~";");

  enum ffiStructs = Filter!(isFFIStruct, __traits(allMembers, _module));
  string[ffiStructs.length] arr;

  static foreach (i, s; ffiStructs) {
    arr[i] = s;
  }

  return arr;
}

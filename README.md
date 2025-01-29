phpmod.d
========

Write PHP extensions without unending misery of C, macros and PHP build system.

Imagine a world where you can expose a native function to PHP by writing
couple lines of [D](https://dlang.org/) code just like this:

```d
import phpmod;

ModuleEntry mod = {
  name: "popcnt",
  version_: "1",
  functions: [
    func!popcount,
    FunctionEntry()
  ]
};

extern(C) ModuleEntry* get_module() {
  return &mod;
}

long popcount(long x) {
  import core.bitop : popcnt;
  return popcnt(x);
}
```

Then compiling that tiny file by simple invocation of your favourite compiler:

```
gdc-14 -shared -fPIC -O2 -fpreview=all phpmod.d popcnt.d -o popcount.so
```

And finally loading it:

```
php -d extension=./popcount.so
```

All that without ever touching everlasting horrors of C, wrestling with abysmal
build system or being exposed to rancid guts of PHP virtual machine?

That world is possible. phpmod.d is the tool that do just that.

You write your code in a neat, expressive a performant native language and let
all the garbage needed to interface with the PHP runtime be auto-generated
during compilation.

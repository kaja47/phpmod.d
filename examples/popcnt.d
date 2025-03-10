// import the library (it's just one file)
import phpmod; 

import core.bitop : popcnt;

// declare the module
ModuleEntry mod = {
  name: "popcnt",
  version_: "1",
  functions: [
    // Template func auto-generates everything needed to expose your function
    // to the PHP runtime.
    func!popcount,
    func!popcount_string,
    func!same_bits_string,
    // Last entry has to be empty function entry (PHP gang say so).
    FunctionEntry() 
  ]
};

// This is the symbol that the PHP search for to load your extension from
// compiled .so file. It's absolutely necessary to declare it as extern(C) to
// prevent name mangling.
extern(C) ModuleEntry* get_module() {
  return &mod;
}

// Your logic here. Native types in. Native types out.
long popcount(long x) {
  return popcnt(x);
}

long popcount_string(scope const(ubyte)[] str) {
  size_t i;
  long cnt;
  for (; i + 7 < str.length; i += 8) {
    cnt += popcnt(*cast(ulong*)(str.ptr + i));
  }
  for (; i < str.length; i++) {
    cnt += popcnt(str.ptr[i]);
  }
  return cnt;
}

long same_bits_string(scope const(ubyte)[] a, scope const(ubyte)[] b) {
  if (a.length != b.length) throw new Exception("strings must have identical lengths");
  size_t i;
  long cnt = a.length * 8;
  for (; i + 7 < a.length; i += 8) {
    ulong aa = *cast(ulong*)(a.ptr + i);
    ulong bb = *cast(ulong*)(b.ptr + i);
    cnt -= popcnt(aa ^ bb);
  }
  for (; i < a.length; i++) {
    cnt -= popcnt(a.ptr[i] ^ b.ptr[i]);
  }
  return cnt;
}

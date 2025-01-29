// import the library (it's just one file)
import phpmod; 

// declare the module
ModuleEntry mod = {
  name: "popcnt",
  version_: "1",
  functions: [
    // Template func auto-generates everything needed to expose your function
    // to the PHP runtime.
    func!popcount,
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
  import core.bitop : popcnt;
  return popcnt(x);
}

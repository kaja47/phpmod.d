import phpmod;

// This file shows how we can expose some functions from C while touching only
// small amount of D.

// There we take advantage of ImportC functionality of D language/compiler
// (https://dlang.org/spec/importc.html) which allows us to directly import C
// file (or header file) by ordinary import statement. This makes all symbols
// from there visible here.
import cfunctions;

// And then we can instantiate automod with imported module.
mixin mod!cfunctions;



// Alternatively we can expose functions manually:

//import phpmod;
//import cfunctions;

// Automod machinery picks up only symbols locally declared in a given module.
// We can achieve it in two ways. Either by making local alias.
// (Name of our alias plays no role. PHP function gets its name from the actual
// function.)
//alias c_function_alias = .cfunctions.c_function_alias;

// Or declaring a function as extern(C) without body. This doesn't need any
// imports of C code.
//extern(C) int c_function_extern(int arg);

// Now automod picks up both functions as they are declared locally, not merely
// imported.
//mixin mod!cmod;

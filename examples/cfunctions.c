// see cmod.d

int c_function_alias(int arg) { return 0; }
int c_function_extern(int arg) { return 0; }

// pair of arguments char* and some integer is translated to single PHP string
// argument
void c_string_arg(const char* str, long length) {}

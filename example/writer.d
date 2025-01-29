import phpmod;
import core.sys.posix.unistd;
import core.sys.posix.string;
import core.sys.posix.fcntl;
import core.stdc.errno;

ModuleEntry mod = {
  name: "writer",
  version_: "1",
  moduleStartup: (int, int moduleNumber) {
    // We need to register the class to made in known to the runtime.
    registerClass!Writer;
    return Result.SUCCESS;
  }
};

extern(C) ModuleEntry* get_module() {
  return &mod;
}

@phpClass struct Writer {
  int fd = -1;
  zend_object std;

  // PHP constructor can be also implemented by a native function called
  // `__construct` (same as in PHP).
  this(scope const(char)[] filename) {
    fd = .open(filename.ptr, O_WRONLY);
    if (fd == -1) throw new PHPException(strerror(errno));
  }

  // PHP destructor can be also implemented by a native function called
  // `__destruct` (same as in PHP). That might be preferable to prevent
  // accidentally calling destructor when local copy goes out of scope
  // destroying referenced resources.
  ~this() {
    close();
  }

  long pwrite(scope const(ubyte)[] str, long offset) {
    // any exception thrown in native code is caught by auto-generated wrapper
    // and rethrown to PHP a a PHP exception
    if (fd == -1) throw new Exception("file not open");
    auto rc = .pwrite(fd, str.ptr, str.length, offset);
    // PHPException is class specialized for messages provided as zero
    // terminated char*
    if (rc == -1) throw new PHPException(strerror(errno));
    return rc;
  }

  String* pread(long length, long offset) {
    if (fd == -1) throw new Exception("file not open");
    // String.alloc(n) allocates n+1 bytes, n of which is usable and the last
    // one is set to zero.
    auto str = String.alloc(length);
    auto rc = .pread(fd, str.ptr, length, offset);
    if (rc == -1) throw new PHPException(strerror(errno));
    // We need to zero terminate string in case of short read.
    str.ptr[rc] = 0;
    str.len = rc;
    return str;
  }

  void close() {
    if (fd != -1) {
      .close(fd);
      fd = -1;
    }
  }
}

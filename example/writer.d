import phpmod;
import core.sys.posix.unistd;
import core.sys.posix.string;
import core.sys.posix.fcntl;
import core.stdc.errno;

ModuleEntry mod = {
  name: "writer",
  version_: "1",
  moduleStartup: (int, int moduleNumber) {
    phpClassRegistry!Writer.register();
    return Result.SUCCESS;
  }
};

extern(C) ModuleEntry* get_module() {
  return &mod;
}

@phpClass struct Writer {
  int fd;
  zend_object std;

  this(scope const(char)[] filename) {
    fd = .open(filename.ptr, O_WRONLY);
    if (fd == -1) throw new PHPException(strerror(errno));
  }

  long pwrite(scope const(ubyte)[] str, long offset) {
    if (fd == -1) throw new PHPException("file not open");
    auto rc = .pwrite(fd, str.ptr, str.length, offset);
    if (rc == -1) throw new PHPException(strerror(errno));
    return rc;
  }
}

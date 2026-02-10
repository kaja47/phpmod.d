module userlandClassTest;
import phpmod;
mixin mod!userlandClassTest;

struct XYZ {
  long x, y, z;
  long meth(long a);
}

alias XYZProxy = userClass!XYZ;


long readProperties(XYZProxy* c) { return c.x + c.y + c.z; }
long callMethod(XYZProxy* c) { return c.meth(); }

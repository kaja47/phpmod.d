import phpmod;
import core.bitop;

mixin mod!unpack;

@nogc:

// function unpack1(string $pattern, string $str, int $offset = 0): mixed
// function unpack_array(string $pattern, string $str, int $offset = 0): array

zval unpack1(scope const(char)[] pattern, scope const(char)[] str, long offset = 0) {
  if (pattern.length != 1) throw new Exception("bad pattern");
  return _unpack(pattern[0], str, offset);
}

HashTable* unpack_array(scope const(char)[] pattern, scope const(char)[] str, long offset = 0) {
  auto result = HashTable.alloc(pattern.length, packed: true);
  // if anything throws we need to deallocate this array
  scope(failure) release(result);
  // cast to byte array to bypass automatic unicode string decoding
  foreach (p; cast(const(ubyte)[]) pattern) {
    auto res = _unpack(p, str, offset);
    result.append(&res);
  }
  return result;
}

// nothing below this point (with exception of the very last line) is PHP
// extension specific

private zval _unpack(char p, scope const(char)[] str, ref long offset) {
  switch (p) {
    case 'c': return chomp!(Endian.Little, byte)  (str, offset);
    case 'C': return chomp!(Endian.Little, ubyte) (str, offset);
    case 'v': return chomp!(Endian.Little, ushort)(str, offset);
    case 'V': return chomp!(Endian.Little, uint)  (str, offset);
    case 'P': return chomp!(Endian.Little, ulong) (str, offset);
    case 'n': return chomp!(Endian.Big,    ushort)(str, offset);
    case 'N': return chomp!(Endian.Big,    uint)  (str, offset);
    case 'J': return chomp!(Endian.Big,    ulong) (str, offset);
    case 'g': return chomp!(Endian.Little, float) (str, offset);
    case 'e': return chomp!(Endian.Little, double)(str, offset);
    case 'G': return chomp!(Endian.Big,    float) (str, offset);
    case 'E': return chomp!(Endian.Big,    double)(str, offset);
    default: throw new Exception("bad pattern");
  }
}

private enum Endian { Little, Big }

pragma(inline, true)
private zval chomp(alias E, T)(scope const(char)[] str, ref long offset) {
  if (str.length < offset + T.sizeof) throw new Exception("string too short");

  static if (T.sizeof == 1) {
    alias _bswap = (x) => x;
    alias R = ubyte;
  } else static if (T.sizeof == 2) {
    alias _bswap = byteswap;
    alias R = ushort;
  } else static if (T.sizeof == 4) {
    alias _bswap = bswap;
    alias R = uint;
  } else static if (T.sizeof == 8) {
    alias _bswap = bswap;
    alias R = ulong;
  }

  version (LittleEndian) enum doSwap = E != Endian.Little;
  version (BigEndian)    enum doSwap = E != Endian.Big;

  R val = *cast(R*) (str.ptr + offset);
  offset += T.sizeof;
  val = doSwap ? _bswap(val) : val;

  // based on type T this will construct zval with PHP type long or double
  return zval(*cast(T*)&val);
}

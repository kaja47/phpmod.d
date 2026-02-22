import phpmod;
import std.stdio;

mixin mod!zendapi;


void testZendHashApi() {
  auto arr = _zend_new_array_0();

  // different objects, identical content (each with RC=1)
  auto key1 = zend_strpprintf(0, "100");
  auto key2 = zend_strpprintf(0, "100");

  auto val1 = zval(zend_strpprintf(0, "alpha"));
  auto val2 = zval(zend_strpprintf(0, "beta"));

  // arr now owns val1, we don't need to release it
  // key's RC got incremented
  zend_hash_update(arr, key1, &val1);
  // we still own key1, we must release it
  release(key1);

  // arr now owns val2 (val1 got released)
  zend_hash_update(arr, key2, &val2);
  release(key2);

  // no numeric string conversion happened
  assert(zend_hash_index_find(arr, 100) == null);
  assert(zend_hash_str_find(arr, "100", 3) != null);

  // this will release all keys and values referenced from arr
  release(arr);
}


void testZendSymtableApi() {
  auto arr = _zend_new_array_0();
  auto z = zval(arr);

  add_assoc_long_ex(&z, "100", 3, 0);
  add_index_long(&z, 100, 1);

  assert(arr.length == 1);

  release(z);
}

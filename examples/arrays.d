import phpmod;

mixin mod!arrays;

HashTable* array_merge_values(HashTable* a, HashTable* b) nothrow {
  HashTable* res = HashTable.alloc(a.length + b.length, packed: true);
  foreach (z; a.byValue) {
    res.append(z);
    z.bump();
  }
  foreach (z; b.byValue) {
    res.append(z);
    z.bump();
  }
  return res;
}

HashTable* array_flatten(HashTable* aa) {
  HashTable* res = HashTable.alloc(10, packed: true);
  scope(failure) res.release();

  foreach (HashTable* a; aa.typed) {
    foreach (z; aa.byValue) {
      res.append(z);
      z.bump();
    }
  }

  return res;
}

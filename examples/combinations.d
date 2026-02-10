import phpmod;
import std.algorithm;

import core.stdc.stdio;

mixin mod!combinations;

//namespace PHPStanTurbo;

@phpClass struct CombinationsHelper {
  zend_object obj;

  static HashTable* _combinations(HashTable* arrays) {
    zval*[][] arrs = emallocArray!(zval*[])(arrays.length);
    arrs[] = null;
    int[] positions = emallocArray!int(arrays.length);
    zval*[] tmp = emallocArray!(zval*)(arrays.length);

    scope(exit) {
      foreach (arr; arrs) {
        if (arr.ptr) _efree(arr.ptr);
      }
      _efree(arrs.ptr);
      _efree(positions.ptr);
      _efree(tmp.ptr);
    }


    {
      int i;
      foreach (HashTable* ht; arrays.typed) {
        zval*[] arr = emallocArray!(zval*)(ht.length);
        int j;
        foreach (zval* z; ht.typed) {
          arr[j++] = z;
        }
        arrs[i++] = arr;
      }
    }

    long nResults = 1;
    foreach (i, arr; arrs) {
      nResults *= arr.length;
      positions[i] = 0;
      tmp[i] = arrs[i][0];
    }


    HashTable* result = HashTable.alloc(nResults, packed: true);
    scope(failure) release(result);


    foreach (_; 0 .. nResults) {
      auto ht = HashTable.alloc(arrays.length, packed: true);
      foreach (z; tmp) bump(z);
      ht.fillPacked(tmp);
      result.append(zval(ht));

      foreach (i, pos; positions) {
        positions[i]++;
        if (positions[i] == arrs[i].length) {
          positions[i] = 0;
          tmp[i] = arrs[i][0];
        } else {
          tmp[i] = arrs[i][positions[i]];
          break;
        }
      }
    }

    return result;
  }

}

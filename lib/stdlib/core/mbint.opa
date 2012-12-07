/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * This modules defines operations on 64 bits integers
 *
 * @category data
 * @destination public
 * @stability stable
**/

/**
 * {1 About this module}
 *
 * Multibyte ints as specified in the WBXML format.
 *
 * We only need to/from int/int64.
 *
 * {1 Where should I start?}
 *
 * The operations in here are very self-explanatory.
 *
 * {1 What if I need more?}
 *
 */

/**
 * {1 Types defined in this module}
 *
 * The mbint type is a binary, each octet has a continuation bit (bit 7).
 */

type mbint = binary

/**
 * {1 Interface}
 */

module MBInt {

  /** Return the number of bytes in a multibyte int generated from an int. */
  function int size(int i) {
         if (i >= -0x7f && i <= 0x7f) 1
    else if (i >= -0x3fff && i <= 0x3fff) 2
    else if (i >= -0x1fffff && i <= 0x1fffff) 3
    else if (i >= -0xfffffff && i <= 0xfffffff) 4
    else if (i >= -0x7ffffffff && i <= 0x7ffffffff) 5
    else if (i >= -0x3ffffffffff && i <= 0x3ffffffffff) 6
    else if (i >= -0x1ffffffffffff && i <= 0x1ffffffffffff) 7
    else Binary.length(of_int(i))
  }

  private rem = %%BslPervasives.int_rem%%

  /** Generate a multibyte int from an int. */
  function mbint of_int(int i) {
    recursive function aux(i, os, first) {
      if (i == 0) {
        b = Binary.create(List.length(os))
        List.iter(function (o) { Binary.add_uint8(b,o) },os)
        b
      } else {
        // TODO: Fix these buggy logical ops.
        n = i / 0x80 //Bitwise.lsr(i,7)
        m1 = rem(i,0x80) //Bitwise.land(i,0x7f)
        m = Bitwise.lor(m1,if (first) 0 else 0x80)
        aux(n,[m|os],false)
      }
    }
    aux(i,[],true)
  } 

  /** Convert a multibyte int into an int. */
  function to_int(mbint mbi) {
    recursive function aux(pos, i) {
      o = Binary.get_uint8(mbi,pos)
      i1 = i * 0x80 //Bitwise.lsl(i,7)
      o2 = rem(o,0x80) //Bitwise.land(o,0x7f)
      i = i1+o2
      if (Bitwise.land(o,0x80) != 0) aux(pos+1,i) else i
    }
    aux(0, 0)
  }

  /** Give the number of bytes in a multibyte int generated from an int64. */
  function int size64(int64 i64) {
    Binary.length(of_int64(i64))
  }

  /** Generate a multibyte int from an int64 value. */
  function mbint of_int64(int64 i64) {
    z7f = Int64.of_int(0x7f)
    recursive function mbint aux(i64, os, first) {
      if (Int64.op_eq(i64,Int64.zero)) {
        b = Binary.create(List.length(os))
        List.iter(function (o) { Binary.add_uint8(b,o) },os)
        b
      } else {
        n = Int64.shift_right_logical(i64,7)
        m = Bitwise.lor(Int64.to_int(Int64.logand(i64,z7f)),if (first) 0 else 0x80)
        aux(n,[m|os],false)
      }
    }
    aux(i64,[],true)
  } 

  /** Extract an int64 value from a multibyte int. */
  function int64 to_int64(mbint mbi) {
    recursive function aux(pos, i64) {
      o = Binary.get_uint8(mbi,pos)
      o64 = Int64.of_int(Bitwise.land(o,0x7f))
      i = Int64.add(Int64.shift_left(i64,7),o64)
      if (Bitwise.land(o,0x80) != 0) aux(pos+1,i) else i
    }
    aux(0, Int64.zero)
  }

  /** Zero **/
  zero = of_int(0)

}



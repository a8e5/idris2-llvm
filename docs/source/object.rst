#######
Objects
#######

This page describes the different object types and their in-memory layout in
the rapid runtime system.  Unless noted otherwise, memory is laid out
left-to-right starting with lower addresses.  Binary values are presented
most-significant bit (MSB) first (left, numbered "n-1" for an n-bit value) and
least-significant bit (LSB) last (right, numbered "0").

Immediate Values and Object Pointers
====================================

Obejcts and values are always stored in 64 bits, they can either *point* to an
object in memory or directly contain an *immediate* value.

Immediate Values
----------------

::

  Immediate Value:

  +-----------------------------+
  |63 ...   (bit number)   ... 0|
  +-----------------------------+
  |xxxxxx       ...      xxxxxx1|
  +-----------------------------+

Immediate values (or *immediates*) are values, whose LSB is set to ``1``.  The
remaining 63 bits contain the "payload" of the value (``x`` in the diagram
above).  Currently the following types are represented using immediates:

 * ``Int`` (63-bit signed integer)
 * ``Bits8``, ``Bits16``, and ``Bits32``

.. note::
   In future versions, more types/values could be represented as immediates,
   e.g. ``Char``, 0-ary constructors, and potentially ``Integer`` s x where
   ``|x| < 2^62``

Object Pointers
---------------

*Objects* are referred to by a pointer.  All objects are aligned to 8-byte
boundaries in memory and therefore have their 3 least-significant bits all set
to ``0``

::

  Object Pointer:

  +-----------------------------+
  |63 ...   (bit number)   ... 0|
  +-----------------------------+
  |xxxxxx       ...    xxxxxx000|
  +-----------------------------+

Generic in-memory layout of objects
===================================

::

  Object Layout:

  increasing memory addresses ===>
  +-----------------+-------------------------+
  | Header (64 bit) | Payload (variable size) |
  +-----------------+-------------------------+
  ^
  |
  Object Pointer points to start address

Object Header
-------------

::

  Object Header (64 bit):

  +--------------------+-------------+-----------------------+
  | 24 highest bits    | 8 bits      | 32 lowest bits        |
  +====================+=============+=======================+
  | (generally unused) | Object type | Size, tag, or similar |
  +--------------------+-------------+-----------------------+

The following table describes how the different fields are currently used for
all object types:

+------------------------+---------------------------+------------------------------------+---------------------------------------------------------------------+
| Type  Name             | "Object Type" Field Value | "Size or tag" Field Meaning        | Payload                                                             |
+========================+===========================+====================================+=====================================================================+
| INT [#numty]_          | 0x01                      | (ignored)                          | (not used)                                                          |
+------------------------+---------------------------+------------------------------------+---------------------------------------------------------------------+
| DOUBLE [#numty]_       | 0x01                      | (ignored)                          | 8 bytes double precision float value                                |
+------------------------+---------------------------+------------------------------------+---------------------------------------------------------------------+
| BITS64 [#numty]_       | 0x01                      | (ignored)                          | 8 bytes (64 bits) unsigned Integer                                  |
+------------------------+---------------------------+------------------------------------+---------------------------------------------------------------------+
| STRING                 | 0x02                      | length of the string in bytes      | ``n`` bytes: string encoded as UTF-8                                |
+------------------------+---------------------------+------------------------------------+---------------------------------------------------------------------+
| CLOSURE                | 0x03                      | stored_args + (missing_args << 16) | 8 bytes pointer to the function, followed by ``stored_args`` values |
+------------------------+---------------------------+------------------------------------+---------------------------------------------------------------------+
| CHAR [#char_obsolete]_ | 0x04                      | Codepoint                          | nothing                                                             |
+------------------------+---------------------------+------------------------------------+---------------------------------------------------------------------+
| IOREF                  | 0x05                      | (ignored)                          | 8 bytes: 1 value                                                    |
+------------------------+---------------------------+------------------------------------+---------------------------------------------------------------------+
| BUFFER                 | 0x06                      | size of the buffer in bytes        | ``n`` bytes: buffer contents                                        |
+------------------------+---------------------------+------------------------------------+---------------------------------------------------------------------+
| OPAQUE                 | 0x07                      | size of the contents in bytes      | ``n`` bytes: opaque content                                         |
+------------------------+---------------------------+------------------------------------+---------------------------------------------------------------------+
| PTR                    | 0x08                      | (ignored)                          | 8 bytes: the pointer                                                |
+------------------------+---------------------------+------------------------------------+---------------------------------------------------------------------+
| IOARRAY                | 0x09                      | number of contained items          | 8 * ``n`` bytes: ``n`` values                                       |
+------------------------+---------------------------+------------------------------------+---------------------------------------------------------------------+
| BIGINT                 | 0x0a                      | number  of limbs                   | 8 * ``n`` bytes: the limbs, stored in little-endian order           |
+------------------------+---------------------------+------------------------------------+---------------------------------------------------------------------+
| CLOCK                  | 0x0b                      | (ignored)                          | 16 bytes: seconds (64 bit uint), nanoseconds (64 bit uint)          |
+------------------------+---------------------------+------------------------------------+---------------------------------------------------------------------+
| CONSTRUCTOR            | | 0xff                    | tag of the constructor             | | 8 * ``n`` bytes: ``n`` values                                     |
|                        | | see "special" in payload|                                    | | Value ``n`` is stored in the 24 highest bits ("left" of the type) |
+------------------------+---------------------------+------------------------------------+---------------------------------------------------------------------+

.. rubric:: Footnotes

.. [#numty] All numeric types are treated the same (OBJ_TYPE == 0x01), followed
            by 64 bits of payload
.. [#char_obsolete] Obsolete, `Char` values will soon be represented as
                    immediate values

During garbage collection, the object header may be replaced with a special
"in-place forwarding pointer" which consists of the target pointer shifted one
bit to the right, and the highest bit set.

::

  In-place forwarding pointer object header (64 bit):

  +--------------------+-----------------------+----------------------------------+
  | 1 highest bit      | 63 lowest bits        | variable unused space (optional) |
  +====================+=======================+----------------------------------+
  | 1                  | (target_ptr) >> 1     | never accessed (former payload)  |
  +--------------------+-----------------------+----------------------------------+

No such header should remain after GC is completed. I.e. normal porgram
execution will never encounter an object with such header.

#pragma once

#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#include <sys/errno.h>
#include <sys/mman.h>
#include <sys/stat.h>

#include <gmp.h>

#include "rts.h"

#define OBJ_TYPE_CON_NO_ARGS 0xff
// Object type "1" is used for all "opaque" numbers
#define OBJ_TYPE_INT         0x01
#define OBJ_TYPE_DOUBLE      0x01
#define OBJ_TYPE_BITS64      0x01
#define OBJ_TYPE_STRING      0x02
#define OBJ_TYPE_CLOSURE     0x03
#define OBJ_TYPE_CHAR        0x04
#define OBJ_TYPE_IOREF       0x05
#define OBJ_TYPE_BUFFER      0x06
#define OBJ_TYPE_OPAQUE      0x07
#define OBJ_TYPE_PTR         0x08
#define OBJ_TYPE_IOARRAY     0x09
#define OBJ_TYPE_BIGINT      0x0a
// Clock object size -> 0: invalid, 1: valid
#define OBJ_TYPE_CLOCK       0x0b

typedef uint64_t RapidObjectHeader;

#define HEADER_SIZE 8

typedef struct {
  RapidObjectHeader hdr;
  void *data;
} RapidObject_t;

typedef RapidObject_t *ObjPtr;

int dump_obj(ObjPtr o);
int dump_obj_i(ObjPtr o, int indent);


static inline uint32_t OBJ_TYPE(ObjPtr p) {
  //return ((p->hdr >> 32) & 0xffffffff);
  return ((p->hdr >> 32) & 0xff);
}

static inline uint32_t OBJ_SIZE(ObjPtr p) {
  return ((p->hdr) & 0xffffffff);
}

static inline uint32_t OBJ_TAG(ObjPtr p) {
  return ((p->hdr) & 0xffffffff);
}

static inline void OBJ_PUT_SLOT(ObjPtr p, int32_t pos, ObjPtr d) {
  ((ObjPtr*)&p->data)[pos] = d;
}

static inline ObjPtr OBJ_GET_SLOT(ObjPtr p, int32_t pos) {
  return ((ObjPtr*)&p->data)[pos];
}

static inline void *OBJ_GET_SLOT_ADDR(ObjPtr p, int32_t pos) {
  return &((&p->data)[pos]);
}

static inline void *OBJ_PAYLOAD(ObjPtr p) {
  return &(p->data);
}

#define OBJ_IS_INLINE(p) ((p) == NULL || (uint64_t)(p) & 0x07)
#define OBJ_UNBOX_INT(p) ((int64_t)(p) >> 1)

static inline uint64_t OBJ_MAKE_FWD_INPLACE(ObjPtr p) {
  return 0x8000000000000000ull | (((uint64_t)p) >> 1);
}

static inline bool OBJ_IS_FWD_INPLACE(ObjPtr p) {
  return (p->hdr & 0x8000000000000000ull);
}

static inline RapidObjectHeader MAKE_HEADER(uint64_t objType, uint32_t sizeOrTag) {
  return (objType << 32) | sizeOrTag;
}

/**
 * Return the amount of memory the object requires in bytes, including the
 * header and all payload data.
 */
static inline uint32_t OBJ_TOTAL_SIZE(ObjPtr p) {
  assert(!OBJ_IS_INLINE(p));

  uint64_t h = p->hdr;
  switch (OBJ_TYPE(p)) {
    case OBJ_TYPE_CON_NO_ARGS:
      return 8 + 8 * (h >> 40);
    case OBJ_TYPE_PTR:
      return 8 + POINTER_SIZE;
    case OBJ_TYPE_IOREF:
      return 8 + POINTER_SIZE;
    case OBJ_TYPE_OPAQUE:
      return 8 + POINTER_SIZE;
    case OBJ_TYPE_CHAR:
      return 8;
    case OBJ_TYPE_INT:
      return 8 + 8;
    case OBJ_TYPE_BUFFER:
    case OBJ_TYPE_STRING:
      return 8 + OBJ_SIZE(p);
    case OBJ_TYPE_CLOSURE:
      return 16 + 8 * (h & 0xffff);
    case OBJ_TYPE_IOARRAY:
      return 8 + 8 * OBJ_SIZE(p);
    case OBJ_TYPE_BIGINT:
      return 8 + sizeof(mp_limb_t) * abs((int32_t)OBJ_SIZE(p));
    case OBJ_TYPE_CLOCK:
      // 8 byte header + 2 * 64 bits for seconds & nanoseconds)
      return 8 + 16;
    default:
      fprintf(stderr, "unknown object type: 0x%08llx\n", (h>>32));
      rapid_C_crash("unknown object type");
      return -1;
  }
}

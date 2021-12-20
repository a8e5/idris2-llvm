#include <assert.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#include <sys/errno.h>
#include <sys/mman.h>
#include <sys/stat.h>

#include "gc.h"
#include "object.h"
#include "rts.h"

void idris_mkcon_ok(ObjPtr o) {
  fprintf(stderr, "MKCON ok: %p\n", (void *)o);
}

void idris_mkcon_arg_ok(ObjPtr o, int64_t idx) {
  fprintf(stderr, "MKCON arg ok: %ld -> %p\n", (long)idx, (void *)OBJ_GET_SLOT(o, idx));
  dump_obj_i(OBJ_GET_SLOT(o, idx), 0);
}

#define INDENT(i) for(int indent_i=0;indent_i<i;++indent_i){fprintf(stderr, "  ");};
int dump_obj_i(ObjPtr o, int indent) {
  if (indent > 4) {
    fprintf(stderr, "MAX DEPTH REACHED\n");
    return 0;
  }

  INDENT(indent); fprintf(stderr, "DUMP OBJ AT %p\n", (void *)o);
  if (o == NULL || ((uint64_t)o) & 0x7) {
    INDENT(indent); fprintf(stderr, "    (not a real object)\n");
    return 0;
  }
  INDENT(indent); fprintf(stderr, "header: 0x%016" PRIx64 "\n", o->hdr);
  if (OBJ_IS_FWD_INPLACE(o)) {
    INDENT(indent); fprintf(stderr, "    (in place fwd) %p -> %p\n", (void *)o, (void *) (o->hdr << 1));
    return 0;
  }
  if (OBJ_TYPE(o) == OBJ_TYPE_CON_NO_ARGS) {
    int64_t num_args = o->hdr >> 40;
    for (int i = 0; i < num_args; ++i) {
      INDENT(indent); fprintf(stderr, "CON ARG %d:\n", i);
      dump_obj_i(OBJ_GET_SLOT(o, i), indent+1);
    }
  }
  if (OBJ_TYPE(o) == OBJ_TYPE_IOREF) {
    INDENT(indent); fprintf(stderr, "IOREF content:\n");
    dump_obj_i(OBJ_GET_SLOT(o, 0), indent+1);
  }
  if (OBJ_TYPE(o) == OBJ_TYPE_CLOSURE) {
    int64_t num_args = o->hdr & 0xffff;
    INDENT(indent); fprintf(stderr, "CLOSURE func: %p\n", (void *)OBJ_GET_SLOT(o, 0));
    for (int i = 0; i < num_args; ++i) {
      INDENT(indent); fprintf(stderr, "CLOSURE ARG %d:\n", i);
      dump_obj_i(OBJ_GET_SLOT(o, i+1), indent+1);
    }
  }
  if (OBJ_TYPE(o) == OBJ_TYPE_INT) {
    INDENT(indent); fprintf(stderr, "INT value: %" PRId64 "\n", (int64_t)OBJ_GET_SLOT(o, 0));
  }
  if (OBJ_TYPE(o) == OBJ_TYPE_STRING) {
    uint32_t strsize = OBJ_SIZE(o);
    const char *truncated = "";
    if (strsize > 40) {
      strsize = 40;
      truncated = "[...]";
    }
    char *strCopy = malloc(1 + strsize);
    memcpy(strCopy, OBJ_PAYLOAD(o), strsize);
    strCopy[strsize] = '\0';
    INDENT(indent); fprintf(stderr, "STRING(%d): \"%s\"%s\n", OBJ_SIZE(o), strCopy, truncated);
    free(strCopy);
  }
  return 0;
}

int dump_obj(ObjPtr o) {
  dump_obj_i(o, 0);
  return 0;
}

void idris_rts_crash_typecheck(ObjPtr obj, int64_t expectedType) {
  fprintf(stderr, "Object failed typecheck, expected type: %04x\n", (int)expectedType);
  fprintf(stderr, "  object address: %p\n", (void *)obj);
  fprintf(stderr, "  object header:  0x%016" PRIx64 "\n", obj->hdr);
  dump_obj(obj);
  exit(123);
}

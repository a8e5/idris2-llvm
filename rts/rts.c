#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/errno.h>

#include <gc/gc.h>

const size_t IDRIS_ALIGNMENT = 8;
const size_t NURSERY_SIZE = 1 * 1024 * 1024;

const int HEADER_SIZE = 8;
const int POINTER_SIZE = sizeof(void*);

const int64_t OBJ_TYPE_BUFFER = 0x06;
const int64_t OBJ_TYPE_OPAQUE = 0x07;

typedef struct {
  void *nurseryStart;
  void *nurseryNext;
  void *nurseryEnd;

  int rapid_errno;
} Idris_TSO;

typedef uint64_t RapidObjectHeader;

typedef uint64_t Word;

typedef struct __attribute__((packed, aligned(1))) {
  RapidObjectHeader hdr;
  void *data;
} RapidObject_t;

typedef RapidObject_t *ObjPtr;

static inline uint32_t OBJ_TYPE(ObjPtr p) {
  return ((p->hdr >> 32) & 0xffffffff);
}

static inline uint32_t OBJ_SIZE(ObjPtr p) {
  return ((p->hdr) & 0xffffffff);
}

static inline void *OBJ_PAYLOAD(ObjPtr p) {
  return &(p->data);
}

static inline RapidObjectHeader MAKE_HEADER(int64_t objType, int32_t sizeOrTag) {
  return (objType << 32) | sizeOrTag;
}

extern long idris_enter(void *baseTSO);

void idris_rts_crash(long arg0) {
  printf("CRASH called: %ld\n", arg0);
  exit(3);
}

void idris_rts_crash_msg(ObjPtr msg) {
  int length = OBJ_SIZE(msg);
  const char *str = (const char *)&(msg->data);
  fprintf(stderr, "ERROR: ");
  fwrite(str, length, 1, stderr);
  fprintf(stderr, "\n");
  exit(4);
}

void rapid_C_crash(const char *msg) {
  fprintf(stderr, "ERROR: %s\n", msg);
  exit(5);
}

void rapid_strreverse(char *dst, const char *src, int64_t size) {
  // FIXME: this reverses bytes, not characters (i.e. works only for ASCII)
  for (int64_t i = 0; i < size; ++i) {
    dst[size - 1 - i] = src[i];
  }
}

void idris_rts_gc(long arg0) {
  printf("GC called: 0x%016lx\n", arg0);
  exit(2);
}

void *rapid_C_allocate(Idris_TSO *base, int64_t size) {
  return GC_malloc(size);
}

int64_t idris_rts_int_to_str(char *dst, int64_t val) {
  int64_t size = snprintf(dst, 24, "%lld", val);
  return size;
}

int64_t idris_rts_double_to_str(char *dst, int64_t size, double val) {
  int64_t needed_size = snprintf(dst, size, "%g", val);
  return needed_size;
}

double idris_rts_str_to_double(ObjPtr obj) {
  int length = OBJ_SIZE(obj);
  const char *str = (const char *)OBJ_PAYLOAD(obj);
  char *scopy = (char *)alloca(length + 1);
  memcpy(scopy, str, length);
  scopy[length] = '\0';
  return strtod(scopy, NULL);
}

int64_t idris_rts_str_to_int(ObjPtr obj) {
  int length = OBJ_SIZE(obj);
  const char *str = (const char *)&(obj->data);
  char *scopy = (char *)alloca(length + 1);
  memcpy(scopy, str, length);
  scopy[length] = '\0';
  return strtoll(scopy, NULL, 10);
}

int64_t idris_rts_write_buffer_to_file(ObjPtr fnameObj, ObjPtr bufObj, int64_t maxSize) {
  int fnameLength = OBJ_SIZE(fnameObj);
  const char *fnameStr = (const char *)OBJ_PAYLOAD(fnameObj);
  char *scopy = (char *)alloca(fnameLength + 1);
  memcpy(scopy, fnameStr, fnameLength);
  scopy[fnameLength] = '\0';

  int64_t writeSize = OBJ_SIZE(bufObj);
  if (maxSize < writeSize) {
    writeSize = maxSize;
  }

  FILE *outf = fopen(scopy, "wb");

  size_t written = fwrite(OBJ_PAYLOAD(bufObj), 1, writeSize, outf);
  if (written != writeSize) {
    fclose(outf);
    return 1;
  }

  int closeOk = fclose(outf);

  return closeOk;
}

ObjPtr idris_rts_read_buffer_from_file(Idris_TSO *base, ObjPtr fnameObj) {
  int fnameLength = OBJ_SIZE(fnameObj);
  const char *fnameStr = (const char *)OBJ_PAYLOAD(fnameObj);
  char *scopy = (char *)alloca(fnameLength + 1);
  memcpy(scopy, fnameStr, fnameLength);
  scopy[fnameLength] = '\0';

  int err = 0;

  FILE *inf = NULL;
  if (!(inf = fopen(scopy, "rb"))) {
    err = 1;
    goto end;
  }

  if ((err = fseeko(inf, 0L, SEEK_END))) {
    goto end;
  }

  off_t fileSize = ftello(inf);
  if (fileSize > 0xffffffffull) {
    rapid_C_crash("file too big");
  }

  if ((err = fseeko(inf, 0L, SEEK_SET))) {
    goto end;
  }

  ObjPtr newObj = (ObjPtr)rapid_C_allocate(base, 8 + fileSize);
  newObj->hdr = MAKE_HEADER(OBJ_TYPE_BUFFER, fileSize);

  size_t readBytes = fread(OBJ_PAYLOAD(newObj), 1, fileSize, inf);
  if (readBytes != fileSize) {
    err = 2;
    goto end;
  }

end:
  if (inf != NULL) {
    int closeErr = fclose(inf);
    if (!err && !closeErr) {
      return newObj;
    }
  }

  // create a "null" obj to signify error (because the "isBuffer" check will
  // fail, if the object type is set to zero)
  ObjPtr nullObj = rapid_C_allocate(base, 8);
  nullObj->hdr = 0x0ull;
  return nullObj;
}

ObjPtr rapid_system_file_open(Idris_TSO *base, ObjPtr fnameObj, ObjPtr modeObj) {
  ObjPtr ptrObj = rapid_C_allocate(base, HEADER_SIZE + POINTER_SIZE);
  ptrObj->hdr = MAKE_HEADER(OBJ_TYPE_OPAQUE, POINTER_SIZE);

  int length = OBJ_SIZE(fnameObj);
  const char *str = (const char *)OBJ_PAYLOAD(fnameObj);
  char *fnameCstr = (char *)alloca(length + 1);
  memcpy(fnameCstr, str, length);
  fnameCstr[length] = '\0';

  str = (const char *)OBJ_PAYLOAD(modeObj);
  char *modeCstr = (char *)alloca(length + 1);
  memcpy(modeCstr, str, length);
  modeCstr[length] = '\0';

  FILE *f = fopen(fnameCstr, modeCstr);
  if (f) {
    base->rapid_errno = 0;
  } else {
    base->rapid_errno = errno;
  }

  ptrObj->data = f;
  return ptrObj;
}

void rapid_system_file_close(ObjPtr filePtrObj) {
  if (OBJ_TYPE(filePtrObj) != OBJ_TYPE_OPAQUE || OBJ_SIZE(filePtrObj) != POINTER_SIZE) {
    rapid_C_crash("invalid object apssed to file_close");
  }

  FILE *f = *(FILE **)OBJ_PAYLOAD(filePtrObj);
  fclose(f);
}

Word rapid_system_file_write_string(ObjPtr filePtrObj, ObjPtr strObj) {
  if (OBJ_TYPE(filePtrObj) != OBJ_TYPE_OPAQUE || OBJ_SIZE(filePtrObj) != POINTER_SIZE) {
    rapid_C_crash("invalid object apssed to file_close");
  }

  FILE *f = *(FILE **)OBJ_PAYLOAD(filePtrObj);
  const void *str = OBJ_PAYLOAD(strObj);
  size_t size = OBJ_SIZE(strObj);
  size_t written = fwrite(str, 1, size, f);
  if (written != size) {
    return 1;
  }
  return 0;
}

Word rapid_system_file_eof(ObjPtr filePtrObj) {
  if (OBJ_TYPE(filePtrObj) != OBJ_TYPE_OPAQUE || OBJ_SIZE(filePtrObj) != POINTER_SIZE) {
    rapid_C_crash("invalid object apssed to file_eof");
  }

  FILE *f = *(FILE **)OBJ_PAYLOAD(filePtrObj);
  return (0 != feof(f));
}

int main(int argc, char **argv) {
  GC_init();

  Idris_TSO *tso = malloc(sizeof(Idris_TSO));
  tso->nurseryStart = malloc(NURSERY_SIZE);
  tso->nurseryNext = tso->nurseryStart;
  tso->nurseryEnd = (void *)((long int)tso->nurseryStart + NURSERY_SIZE);
  tso->rapid_errno = 1;

  return idris_enter(tso);
}

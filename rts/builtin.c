#include <assert.h>
#include <ctype.h>
#include <dirent.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#include <sys/errno.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>

#include <gmp.h>

#include "builtin.h"
#include "gc.h"
#include "getline.h"
#include "object.h"
#include "rts.h"

static int rapid_global_argc = 0;
static char **rapid_global_argv = NULL;

uint32_t utf8_encode1_length(uint32_t codepoint);
uint32_t utf8_encode1(char *dst, uint32_t codepoint);
uint64_t utf8_decode1_length(const char *s);

// Quick (not completely accurate) way to clamp a codepoint to the allowed
// range
#define FAST_UNICODE_MASK 0x1fffff

static ObjPtr wrapFilePtr(Idris_TSO *base, FILE *f) {
  ObjPtr ptrObj = rapid_C_allocate(base, HEADER_SIZE + POINTER_SIZE);
  ptrObj->hdr = MAKE_HEADER(OBJ_TYPE_OPAQUE, POINTER_SIZE);
  ptrObj->data = f;
  return ptrObj;
}

void rapid_strreverse(char *restrict dst, const char *restrict src, int64_t size) {
  for (int64_t i = 0; i < size;) {
    uint64_t lengthCodepoint = utf8_decode1_length(&src[i]);

    uint32_t numBytes = lengthCodepoint >> 32;
    uint32_t codepoint = lengthCodepoint;

    uint32_t encoded = utf8_encode1(&dst[size - i - numBytes], codepoint);
    assert(encoded == numBytes);

    i += numBytes;
    assert(i <= size);
  }
}

int64_t idris_rts_bits64_to_str(char *dst, uint64_t val) {
  int64_t size = snprintf(dst, 24, "%" PRIu64, val);
  return size;
}

int64_t idris_rts_int_to_str(char *dst, int64_t val) {
  int64_t size = snprintf(dst, 24, "%" PRId64, val);
  return size;
}

int64_t idris_rts_double_to_str(char *dst, int64_t size, double val) {
  int64_t needed_size = snprintf(dst, size, "%#.15g", val);
  // manually strip trailing zeros:
  if (needed_size <= size) {
    int64_t stripped_size = needed_size;
    while (dst[stripped_size-1] == '0') {
      assert(stripped_size >= 3);
      --stripped_size;
    }
    if (stripped_size < needed_size && !isdigit(dst[stripped_size-1])) {
      // leave exactly one trailing zero
      ++stripped_size;
    }
    assert(stripped_size >= 3);
    assert(stripped_size <= needed_size);
    needed_size = stripped_size;
  }
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

void rapid_system_exit(Idris_TSO *base, int64_t exitCode, ObjPtr _world) {
  exit(exitCode);
}

int64_t rapid_system_errno(Idris_TSO *base, ObjPtr _world) {
  return base->rapid_errno;
}

#define MAX_STRERROR_LENGTH 2048
ObjPtr rapid_system_strerror(Idris_TSO *base, int64_t errnum, ObjPtr _world) {
  char buf[MAX_STRERROR_LENGTH];

  strerror_r(errnum, buf, MAX_STRERROR_LENGTH);
  // strerror_r will return non-zero in case of a too small buffer, but we
  // ignore that and accept the truncated message. In any case the result is
  // guaranteed to be NUL-terminated.

  size_t value_len = strlen(buf);

  ObjPtr newStr = (ObjPtr)rapid_C_allocate(base, HEADER_SIZE + value_len);
  newStr->hdr = MAKE_HEADER(OBJ_TYPE_STRING, value_len);
  memcpy(OBJ_PAYLOAD(newStr), buf, value_len);

  return newStr;
}

int64_t rapid_system_file_errno(Idris_TSO *base, ObjPtr _world) {
  int64_t errno_raw = rapid_system_errno(base, _world);
  switch(errno_raw) {
    case ENOENT:
      return 2;
    case EACCES:
      return 3;
    case EEXIST:
      return 4;
    default:
      return errno_raw + 5;
  }
}

ObjPtr rapid_system_fork(Idris_TSO *base, ObjPtr ioObj, ObjPtr _world) {
  rapid_C_crash("NOT IMPLEMENTED: rapid_system_fork");
  return NULL;
}

int64_t rapid_system_system(Idris_TSO *base, ObjPtr cmdObj, ObjPtr _world) {
  assert(OBJ_TYPE(cmdObj) == OBJ_TYPE_STRING);
  int length = OBJ_SIZE(cmdObj);
  const char *str = (const char *)OBJ_PAYLOAD(cmdObj);
  char *cmdCstr = (char *)alloca(length + 1);
  memcpy(cmdCstr, str, length);
  cmdCstr[length] = '\0';

  int rc = system(cmdCstr);
  return rc;
}

ObjPtr rapid_system_popen(Idris_TSO *base, ObjPtr cmdStrObj, ObjPtr modeStrObj, ObjPtr _world) {
  assert(OBJ_TYPE(cmdStrObj) == OBJ_TYPE_STRING);
  assert(OBJ_TYPE(modeStrObj) == OBJ_TYPE_STRING);

  int cmdLength = OBJ_SIZE(cmdStrObj);
  char cmdCstr[cmdLength + 1];
  memcpy(cmdCstr, (const char *)OBJ_PAYLOAD(cmdStrObj), cmdLength);
  cmdCstr[cmdLength] = '\0';

  int modeLength = OBJ_SIZE(modeStrObj);
  char modeCstr[modeLength + 1];
  memcpy(modeCstr, (const char *)OBJ_PAYLOAD(modeStrObj), modeLength);
  modeCstr[modeLength] = '\0';

  FILE *child = popen(cmdCstr, modeCstr);

  return wrapFilePtr(base, child);
}

uint64_t rapid_system_pclose(Idris_TSO *base, ObjPtr filePtrObj, ObjPtr _world) {
  if (OBJ_TYPE(filePtrObj) != OBJ_TYPE_OPAQUE || OBJ_SIZE(filePtrObj) != POINTER_SIZE) {
    rapid_C_crash("invalid object passed to system_pclose");
  }

  FILE *f = *(FILE **)OBJ_PAYLOAD(filePtrObj);
  int result = pclose(f);

  if (!WIFEXITED(result)) {
    rapid_C_crash("pclose failed");
  }
  return WEXITSTATUS(result);
}

void rapid_system_free(Idris_TSO *base, ObjPtr ptrObj, ObjPtr _world) {
  if (OBJ_TYPE(ptrObj) != OBJ_TYPE_OPAQUE || OBJ_SIZE(ptrObj) != POINTER_SIZE) {
    rapid_C_crash("invalid object passed to rapid_system_free");
  }

  void *p = *(void **)OBJ_PAYLOAD(ptrObj);
  free(p);
}


// return type: Ptr String
ObjPtr rapid_system_get_env(Idris_TSO *base, ObjPtr varObj, ObjPtr _world) {
  assert(OBJ_TYPE(varObj) == OBJ_TYPE_STRING);
  int length = OBJ_SIZE(varObj);
  const char *str = (const char *)OBJ_PAYLOAD(varObj);
  char *varCstr = (char *)alloca(length + 1);
  memcpy(varCstr, str, length);
  varCstr[length] = '\0';

  char *value = getenv(varCstr);
  if (value == NULL) {
    return NULL;
  }
  size_t value_len = strlen(value);

  ObjPtr newStr = rapid_C_allocate(base, HEADER_SIZE + value_len);
  newStr->hdr = MAKE_HEADER(OBJ_TYPE_STRING, value_len);
  memcpy(OBJ_PAYLOAD(newStr), value, value_len);

  ObjPtr newPtr = rapid_C_allocate(base, HEADER_SIZE + POINTER_SIZE);
  newPtr->hdr = MAKE_HEADER(OBJ_TYPE_PTR, 1);
  OBJ_PUT_SLOT(newPtr, 0, newStr);

  return newPtr;
}


int64_t rapid_system_file_atime(Idris_TSO *base, ObjPtr filePtrObj, ObjPtr _world) {
  assert (OBJ_TYPE(filePtrObj) == OBJ_TYPE_OPAQUE);
  assert (OBJ_SIZE(filePtrObj) == POINTER_SIZE);

  struct stat stat_buf;

  FILE *f = *(FILE **)OBJ_GET_SLOT_ADDR(filePtrObj, 0);
  int fd = fileno(f);
  int stat_error = fstat(fd, &stat_buf);

  if (stat_error != 0) {
    base->rapid_errno = errno;
    return -1;
  }

  return stat_buf.st_atime;
}

int64_t rapid_system_file_ctime(Idris_TSO *base, ObjPtr filePtrObj, ObjPtr _world) {
  assert (OBJ_TYPE(filePtrObj) == OBJ_TYPE_OPAQUE);
  assert (OBJ_SIZE(filePtrObj) == POINTER_SIZE);

  struct stat stat_buf;

  FILE *f = *(FILE **)OBJ_GET_SLOT_ADDR(filePtrObj, 0);
  int fd = fileno(f);
  int stat_error = fstat(fd, &stat_buf);

  if (stat_error != 0) {
    base->rapid_errno = errno;
    return -1;
  }

  return stat_buf.st_ctime;
}

int64_t rapid_system_file_mtime(Idris_TSO *base, ObjPtr filePtrObj, ObjPtr _world) {
  assert (OBJ_TYPE(filePtrObj) == OBJ_TYPE_OPAQUE);
  assert (OBJ_SIZE(filePtrObj) == POINTER_SIZE);

  struct stat stat_buf;

  FILE *f = *(FILE **)OBJ_GET_SLOT_ADDR(filePtrObj, 0);
  int fd = fileno(f);
  int stat_error = fstat(fd, &stat_buf);

  if (stat_error != 0) {
    base->rapid_errno = errno;
    return -1;
  }

  return stat_buf.st_mtime;
}

int64_t rapid_system_file_size(Idris_TSO *base, ObjPtr filePtrObj, ObjPtr _world) {
  assert (OBJ_TYPE(filePtrObj) == OBJ_TYPE_OPAQUE);
  assert (OBJ_SIZE(filePtrObj) == POINTER_SIZE);

  struct stat stat_buf;

  FILE *f = *(FILE **)OBJ_GET_SLOT_ADDR(filePtrObj, 0);
  int fd = fileno(f);
  int stat_error = fstat(fd, &stat_buf);

  if (stat_error != 0) {
    base->rapid_errno = errno;
    return -1;
  }

  return stat_buf.st_size;
}

int64_t rapid_system_file_error(Idris_TSO *base, ObjPtr filePtrObj, ObjPtr _world) {
  assert (OBJ_TYPE(filePtrObj) == OBJ_TYPE_OPAQUE);
  assert (OBJ_SIZE(filePtrObj) == POINTER_SIZE);

  FILE *f = *(FILE **)OBJ_GET_SLOT_ADDR(filePtrObj, 0);
  return ferror(f);
}

int64_t rapid_system_file_read_char(Idris_TSO *base, ObjPtr filePtrObj, ObjPtr _world) {
  assert (OBJ_TYPE(filePtrObj) == OBJ_TYPE_OPAQUE);
  assert (OBJ_SIZE(filePtrObj) == POINTER_SIZE);

  FILE *f = *(FILE **)OBJ_GET_SLOT_ADDR(filePtrObj, 0);
  return fgetc(f);
}

int64_t idris_rts_write_buffer_data(Idris_TSO *base, ObjPtr filePtrObj, ObjPtr bufObj, int64_t loc, int64_t maxSize, ObjPtr _world) {
  assert (loc >= 0);
  assert (maxSize >= 0);
  assert (OBJ_TYPE(filePtrObj) == OBJ_TYPE_OPAQUE && OBJ_SIZE(filePtrObj) == POINTER_SIZE);
  assert (OBJ_TYPE(bufObj) == OBJ_TYPE_BUFFER && OBJ_SIZE(bufObj) >= (loc + maxSize));

  FILE *f = *(FILE **)OBJ_GET_SLOT_ADDR(filePtrObj, 0);
  const char *payload = OBJ_PAYLOAD(bufObj);
  const char *begin = payload + loc;

  size_t written = fwrite(begin, 1, maxSize, f);
  if (written != maxSize) {
    return -1;
  }

  return written;
}

int64_t idris_rts_read_buffer_data(Idris_TSO *base, ObjPtr filePtrObj, ObjPtr bufObj, int64_t loc, int64_t maxSize, ObjPtr _world) {
  assert (loc >= 0);
  assert (maxSize >= 0);
  assert (OBJ_TYPE(filePtrObj) == OBJ_TYPE_OPAQUE && OBJ_SIZE(filePtrObj) == POINTER_SIZE);
  assert (OBJ_TYPE(bufObj) == OBJ_TYPE_BUFFER && OBJ_SIZE(bufObj) >= (loc + maxSize));

  FILE *f = *(FILE **)OBJ_GET_SLOT_ADDR(filePtrObj, 0);
  char *payload = OBJ_PAYLOAD(bufObj);
  char *begin = payload + loc;

  size_t read = fread(begin, 1, maxSize, f);
  if (read != maxSize) {
    return -1;
  }

  return read;
}

ObjPtr rapid_system_file_stdin(Idris_TSO *base) {
  ObjPtr ptrObj = rapid_C_allocate(base, HEADER_SIZE + POINTER_SIZE);
  ptrObj->hdr = MAKE_HEADER(OBJ_TYPE_OPAQUE, POINTER_SIZE);
  ptrObj->data = stdin;
  return ptrObj;
}

ObjPtr rapid_system_file_stdout(Idris_TSO *base) {
  ObjPtr ptrObj = rapid_C_allocate(base, HEADER_SIZE + POINTER_SIZE);
  ptrObj->hdr = MAKE_HEADER(OBJ_TYPE_OPAQUE, POINTER_SIZE);
  ptrObj->data = stdout;
  return ptrObj;
}

ObjPtr rapid_system_file_stderr(Idris_TSO *base) {
  ObjPtr ptrObj = rapid_C_allocate(base, HEADER_SIZE + POINTER_SIZE);
  ptrObj->hdr = MAKE_HEADER(OBJ_TYPE_OPAQUE, POINTER_SIZE);
  ptrObj->data = stderr;
  return ptrObj;
}

ObjPtr rapid_system_fdopen(Idris_TSO *base, int64_t fd, ObjPtr modeObj, ObjPtr _world) {
  assert(OBJ_TYPE(modeObj) == OBJ_TYPE_STRING);

  int length = OBJ_SIZE(modeObj);
  const char *str = (const char *)OBJ_PAYLOAD(modeObj);
  char *modeCstr = (char *)alloca(length + 1);
  memcpy(modeCstr, str, length);
  modeCstr[length] = '\0';

  FILE *f = fdopen(fd, modeCstr);
  if (f) {
    base->rapid_errno = 0;
  } else {
    base->rapid_errno = errno;
  }

  return wrapFilePtr(base, f);
}

ObjPtr rapid_system_file_open(Idris_TSO *base, ObjPtr fnameObj, ObjPtr modeObj, ObjPtr _world) {
  int length = OBJ_SIZE(fnameObj);
  const char *str = (const char *)OBJ_PAYLOAD(fnameObj);
  char *fnameCstr = (char *)alloca(length + 1);
  memcpy(fnameCstr, str, length);
  fnameCstr[length] = '\0';

  length = OBJ_SIZE(modeObj);
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

  return wrapFilePtr(base, f);
}

Word rapid_system_file_remove(Idris_TSO *base, ObjPtr fnameObj, ObjPtr _world) {
  int length = OBJ_SIZE(fnameObj);
  const char *str = (const char *)OBJ_PAYLOAD(fnameObj);
  char *fnameCstr = (char *)alloca(length + 1);
  memcpy(fnameCstr, str, length);
  fnameCstr[length] = '\0';

  return remove(fnameCstr);
}

void rapid_putstr(Idris_TSO *base, ObjPtr strObj, ObjPtr _world) {
  assert(OBJ_TYPE(strObj) == OBJ_TYPE_STRING);
  int64_t length = OBJ_SIZE(strObj);
  fwrite(OBJ_PAYLOAD(strObj), length, 1, stdout);
  fflush(stdout);
}

int32_t rapid_system_getchar(Idris_TSO *base, ObjPtr _world) {
  return getchar();
}

void rapid_system_putchar(Idris_TSO *base, int32_t c, ObjPtr _world) {
  putchar(c);
}

int64_t rapid_system_file_chmod(Idris_TSO *base, ObjPtr fnameObj, uint64_t mode, ObjPtr _world) {
  int length = OBJ_SIZE(fnameObj);
  const char *str = (const char *)OBJ_PAYLOAD(fnameObj);
  char *fnameCstr = (char *)alloca(length + 1);
  memcpy(fnameCstr, str, length);
  fnameCstr[length] = '\0';

  return chmod(fnameCstr, (mode_t)mode);
}

int64_t rapid_system_file_flush(Idris_TSO *base, ObjPtr filePtrObj, ObjPtr _world) {
  if (OBJ_TYPE(filePtrObj) != OBJ_TYPE_OPAQUE || OBJ_SIZE(filePtrObj) != POINTER_SIZE) {
    rapid_C_crash("invalid object passed to file_flush");
  }

  FILE *f = *(FILE **)OBJ_PAYLOAD(filePtrObj);
  return fflush(f);
}

void rapid_system_file_close(Idris_TSO *base, ObjPtr filePtrObj, ObjPtr _world) {
  if (OBJ_TYPE(filePtrObj) != OBJ_TYPE_OPAQUE || OBJ_SIZE(filePtrObj) != POINTER_SIZE) {
    rapid_C_crash("invalid object passed to file_close");
  }

  FILE *f = *(FILE **)OBJ_PAYLOAD(filePtrObj);
  fclose(f);
}

Word rapid_system_file_write_string(Idris_TSO *base, ObjPtr filePtrObj, ObjPtr strObj, ObjPtr _world) {
  if (OBJ_TYPE(filePtrObj) != OBJ_TYPE_OPAQUE || OBJ_SIZE(filePtrObj) != POINTER_SIZE) {
    rapid_C_crash("invalid object passed to file_write_string");
  }

  FILE *f = *(FILE **)OBJ_PAYLOAD(filePtrObj);
  const void *str = OBJ_PAYLOAD(strObj);
  size_t size = OBJ_SIZE(strObj);
  size_t written = fwrite(str, 1, size, f);
  if (written != size) {
    // Attention: this builtin is supposed to return 0 on _failure_
    return 0;
  }
  return 1;
}

// return type: Ptr String
ObjPtr rapid_system_file_read_chars(Idris_TSO *base, uint64_t max, ObjPtr filePtrObj, ObjPtr _world) {
  if (OBJ_TYPE(filePtrObj) != OBJ_TYPE_OPAQUE || OBJ_SIZE(filePtrObj) != POINTER_SIZE) {
    rapid_C_crash("invalid object passed to file_read_chars");
  }

  FILE *f = *(FILE **)OBJ_PAYLOAD(filePtrObj);
  size_t bufsize = max;

  ObjPtr newStr = rapid_C_allocate(base, HEADER_SIZE + bufsize);

  size_t read = fread(OBJ_PAYLOAD(newStr), 1, bufsize, f);
  newStr->hdr = MAKE_HEADER(OBJ_TYPE_STRING, read);

  ObjPtr newPtr = rapid_C_allocate(base, HEADER_SIZE + POINTER_SIZE);
  newPtr->hdr = MAKE_HEADER(OBJ_TYPE_PTR, 1);
  OBJ_PUT_SLOT(newPtr, 0, newStr);

  return newPtr;
}

// return type: Ptr String
ObjPtr rapid_system_file_read_line(Idris_TSO *base, ObjPtr filePtrObj, ObjPtr _world) {
  if (OBJ_TYPE(filePtrObj) != OBJ_TYPE_OPAQUE || OBJ_SIZE(filePtrObj) != POINTER_SIZE) {
    rapid_C_crash("invalid object passed to file_read_line");
  }

  FILE *f = *(FILE **)OBJ_PAYLOAD(filePtrObj);
  size_t bufsize = 0;
  char *buffer = NULL;
  ssize_t length = getline(&buffer, &bufsize, f);

  // reset errno, to distinguish return code "-1" caused by EOF (errno unset)
  // from return code "-1" caused by error (errno set)
  errno = 0;
  if (length == -1 && errno != 0) {
    base->rapid_errno = errno;
    rapid_C_crash("getline failed");
  }

  if (length < 0 || buffer == NULL) {
    length = 0;
  }

  ObjPtr newStr = rapid_C_allocate(base, HEADER_SIZE + length);
  newStr->hdr = MAKE_HEADER(OBJ_TYPE_STRING, length);
  memcpy(OBJ_PAYLOAD(newStr), buffer, length);
  free(buffer);

  ObjPtr newPtr = rapid_C_allocate(base, HEADER_SIZE + POINTER_SIZE);
  newPtr->hdr = MAKE_HEADER(OBJ_TYPE_PTR, 1);
  OBJ_PUT_SLOT(newPtr, 0, newStr);

  return newPtr;
}

int64_t rapid_system_file_seek_line(Idris_TSO *base, ObjPtr filePtrObj, ObjPtr _world) {
  if (OBJ_TYPE(filePtrObj) != OBJ_TYPE_OPAQUE || OBJ_SIZE(filePtrObj) != POINTER_SIZE) {
    rapid_C_crash("invalid object passed to file_seek_line");
  }

  FILE *f = *(FILE **)OBJ_PAYLOAD(filePtrObj);

  while (1) {
    int c = fgetc(f);
    if (c == -1) {
      if (feof(f)) {
        return 0;
      } else {
        base->rapid_errno = errno;
        return -1;
      }
    }
    if (c == '\n') {
      return 0;
    }
  }
}

ObjPtr rapid_system_stdin_getline(Idris_TSO *base, ObjPtr _world) {
  size_t bufsize = 0;
  char *buffer = NULL;
  ssize_t length = getline(&buffer, &bufsize, stdin);
  if (length == -1 && errno != 0) {
    base->rapid_errno = errno;
    rapid_C_crash("getline failed");
  }

  if (length < 0 || buffer == NULL) {
    length = 0;
  }

  // cut off trailing newline character(s)
  while (length > 0 && (buffer[length - 1] == '\r' || buffer[length - 1] == '\n')) {
    length--;
  }

  ObjPtr newStr = rapid_C_allocate(base, HEADER_SIZE + length);
  newStr->hdr = MAKE_HEADER(OBJ_TYPE_STRING, length);
  memcpy(OBJ_PAYLOAD(newStr), buffer, length);
  free(buffer);

  return newStr;
}

Word rapid_system_file_eof(Idris_TSO *base, ObjPtr filePtrObj, ObjPtr _world) {
  if (OBJ_TYPE(filePtrObj) != OBJ_TYPE_OPAQUE || OBJ_SIZE(filePtrObj) != POINTER_SIZE) {
    rapid_C_crash("invalid object passed to file_eof");
  }

  FILE *f = *(FILE **)OBJ_PAYLOAD(filePtrObj);
  return (0 != feof(f));
}

/*
 * Directory functions
 */
ObjPtr rapid_system_current_dir(Idris_TSO *base, ObjPtr _world) {
  char tmp[1024];
  char *cwd = getcwd(tmp, 1024);

  if (cwd == NULL) {
    base->rapid_errno = errno;
    rapid_C_crash("getcwd failed");
    return NULL;
  }

  ObjPtr newStr = NULL;
  ssize_t length = strlen(cwd);

  newStr = rapid_C_allocate(base, HEADER_SIZE + length);
  newStr->hdr = MAKE_HEADER(OBJ_TYPE_STRING, length);
  memcpy(OBJ_PAYLOAD(newStr), cwd, length);

  ObjPtr newPtr = rapid_C_allocate(base, HEADER_SIZE + POINTER_SIZE);
  newPtr->hdr = MAKE_HEADER(OBJ_TYPE_PTR, 1);
  OBJ_PUT_SLOT(newPtr, 0, newStr);

  return newPtr;
}

int64_t rapid_system_dir_create(Idris_TSO *base, ObjPtr fnameObj, ObjPtr _world) {
  ObjPtr ptrObj = rapid_C_allocate(base, HEADER_SIZE + POINTER_SIZE);
  ptrObj->hdr = MAKE_HEADER(OBJ_TYPE_OPAQUE, POINTER_SIZE);

  int length = OBJ_SIZE(fnameObj);
  const char *str = (const char *)OBJ_PAYLOAD(fnameObj);
  char *fnameCstr = (char *)alloca(length + 1);
  memcpy(fnameCstr, str, length);
  fnameCstr[length] = '\0';

  int r = mkdir(fnameCstr, S_IRWXU | S_IRWXG | S_IRWXO);
  if (r == 0) {
    base->rapid_errno = 0;
  } else {
    base->rapid_errno = errno;
  }

  return r;
}

int64_t rapid_system_dir_change(Idris_TSO *base, ObjPtr fnameObj, ObjPtr _world) {
  ObjPtr ptrObj = rapid_C_allocate(base, HEADER_SIZE + POINTER_SIZE);
  ptrObj->hdr = MAKE_HEADER(OBJ_TYPE_OPAQUE, POINTER_SIZE);

  int length = OBJ_SIZE(fnameObj);
  const char *str = (const char *)OBJ_PAYLOAD(fnameObj);
  char *fnameCstr = (char *)alloca(length + 1);
  memcpy(fnameCstr, str, length);
  fnameCstr[length] = '\0';

  int r = chdir(fnameCstr);
  if (r == 0) {
    base->rapid_errno = 0;
  } else {
    base->rapid_errno = errno;
  }

  return r;
}

ObjPtr rapid_system_dir_open(Idris_TSO *base, ObjPtr fnameObj, ObjPtr _world) {
  ObjPtr ptrObj = rapid_C_allocate(base, HEADER_SIZE + POINTER_SIZE);
  ptrObj->hdr = MAKE_HEADER(OBJ_TYPE_OPAQUE, POINTER_SIZE);

  int length = OBJ_SIZE(fnameObj);
  const char *str = (const char *)OBJ_PAYLOAD(fnameObj);
  char *fnameCstr = (char *)alloca(length + 1);
  memcpy(fnameCstr, str, length);
  fnameCstr[length] = '\0';

  DIR *d = opendir(fnameCstr);
  if (d) {
    base->rapid_errno = 0;
  } else {
    base->rapid_errno = errno;
  }

  ptrObj->data = d;

  return ptrObj;
}

void rapid_system_dir_close(Idris_TSO *base, ObjPtr dirPtrObj, ObjPtr _world) {
  if (OBJ_TYPE(dirPtrObj) != OBJ_TYPE_OPAQUE || OBJ_SIZE(dirPtrObj) != POINTER_SIZE) {
    rapid_C_crash("invalid object passed to dir_close");
  }

  DIR *d = *(DIR **)OBJ_PAYLOAD(dirPtrObj);
  closedir(d);
}

ObjPtr rapid_system_dir_next_entry(Idris_TSO *base, ObjPtr dirPtrObj, ObjPtr _world) {
  if (OBJ_TYPE(dirPtrObj) != OBJ_TYPE_OPAQUE || OBJ_SIZE(dirPtrObj) != POINTER_SIZE) {
    rapid_C_crash("invalid object passed to dir_next_entry");
  }

  DIR *d = *(DIR **)OBJ_PAYLOAD(dirPtrObj);
  errno = 0;
  struct dirent *de = readdir(d);

  if (de == NULL) {
    base->rapid_errno = errno;
    return NULL;
  }
  base->rapid_errno = 0;

  ssize_t length = strlen(de->d_name);

  if (length < 0) {
    length = 0;
  }

  ObjPtr newStr = rapid_C_allocate(base, HEADER_SIZE + length);
  newStr->hdr = MAKE_HEADER(OBJ_TYPE_STRING, length);
  memcpy(OBJ_PAYLOAD(newStr), de->d_name, length);

  ObjPtr newPtr = rapid_C_allocate(base, HEADER_SIZE + POINTER_SIZE);
  newPtr->hdr = MAKE_HEADER(OBJ_TYPE_PTR, 1);
  OBJ_PUT_SLOT(newPtr, 0, newStr);

  return newPtr;
}


const int TAG_LIST_NIL = 0;
const int TAG_LIST_CONS = 1;

ObjPtr rapid_fast_pack(Idris_TSO *base, ObjPtr charListObj) {
  assert(OBJ_TYPE(charListObj) == OBJ_TYPE_CON_NO_ARGS);

  int32_t strLength = 0;
  ObjPtr cursor = charListObj;
  assert(OBJ_TYPE(cursor) == OBJ_TYPE_CON_NO_ARGS);
  while (OBJ_TAG(cursor) == TAG_LIST_CONS) {
    ObjPtr charObj = OBJ_GET_SLOT(cursor, 0);
    assert(OBJ_TYPE(charObj) == OBJ_TYPE_CHAR);

    uint32_t codepoint = OBJ_SIZE(charObj) & FAST_UNICODE_MASK;
    strLength += utf8_encode1_length(codepoint);
    cursor = OBJ_GET_SLOT(cursor, 1);
    assert(OBJ_TYPE(cursor) == OBJ_TYPE_CON_NO_ARGS);
  }

  ObjPtr newStr = rapid_C_allocate(base, HEADER_SIZE + strLength);
  newStr->hdr = MAKE_HEADER(OBJ_TYPE_STRING, strLength);

  int32_t strPos = 0;
  char *dst = OBJ_PAYLOAD(newStr);
  cursor = charListObj;
  while (OBJ_TAG(cursor) == TAG_LIST_CONS) {
    ObjPtr charObj = OBJ_GET_SLOT(cursor, 0);
    assert(OBJ_TYPE(charObj) == OBJ_TYPE_CHAR);
    uint32_t codepoint = OBJ_SIZE(charObj) & FAST_UNICODE_MASK;
    strPos += utf8_encode1(&dst[strPos], codepoint);
    assert(strPos <= strLength);
    cursor = OBJ_GET_SLOT(cursor, 1);
    assert(OBJ_TYPE(cursor) == OBJ_TYPE_CON_NO_ARGS);
  }
  assert(strPos == strLength);
  return newStr;
}

ObjPtr rapid_fast_append(Idris_TSO *base, ObjPtr strListObj) {
  uint64_t strLength = 0;
  ObjPtr cursor = strListObj;
  while (OBJ_TAG(cursor) == TAG_LIST_CONS) {
    assert(OBJ_TYPE(cursor) == OBJ_TYPE_CON_NO_ARGS);
    ObjPtr partObj = OBJ_GET_SLOT(cursor, 0);
    assert(OBJ_TYPE(partObj) == OBJ_TYPE_STRING);
    strLength += OBJ_SIZE(partObj);

    cursor = OBJ_GET_SLOT(cursor, 1);
  }

  if (strLength > 0xffffffffull) {
    rapid_C_crash("fastAppend string is too large");
  }

  ObjPtr newStr = rapid_C_allocate(base, HEADER_SIZE + strLength);
  newStr->hdr = MAKE_HEADER(OBJ_TYPE_STRING, strLength);

  uint32_t strPos = 0;
  char *dst = OBJ_PAYLOAD(newStr);
  cursor = strListObj;
  while (OBJ_TAG(cursor) == TAG_LIST_CONS) {
    ObjPtr partObj = OBJ_GET_SLOT(cursor, 0);
    assert(OBJ_TYPE(partObj) == OBJ_TYPE_STRING);
    int32_t partLength = OBJ_SIZE(partObj);

    if (partLength > 0) {
      assert(strPos < strLength);
      memcpy(&dst[strPos], OBJ_PAYLOAD(partObj), partLength);
    }

    strPos += partLength;
    cursor = OBJ_GET_SLOT(cursor, 1);
  }
  return newStr;
}

/**
 * Terminal utility functions, copied from Idris2 soruce dir support/c/
 */
void idris2_setupTerm(Idris_TSO *base, ObjPtr _world) {
    // NOTE: Currently not needed for non windows systems
}

int64_t idris2_getTermCols(Idris_TSO *base, ObjPtr _world) {
    struct winsize ts;
    ioctl(0, TIOCGWINSZ, &ts);
    return (int64_t) ts.ws_col;
}

int64_t idris2_getTermLines(Idris_TSO *base, ObjPtr _world) {
    struct winsize ts;
    ioctl(0, TIOCGWINSZ, &ts);
    return (int64_t) ts.ws_row;
}

int64_t rapid_system_get_arg_count(Idris_TSO *base, ObjPtr _world) {
  return rapid_global_argc;
}

ObjPtr rapid_system_get_arg(Idris_TSO *base, int64_t index, ObjPtr _world) {
  char *thisArg = rapid_global_argv[index];
  size_t thisLength = strlen(thisArg);
  ObjPtr argStrObj = rapid_C_allocate(base, HEADER_SIZE + thisLength);
  argStrObj->hdr = MAKE_HEADER(OBJ_TYPE_STRING, thisLength);
  memcpy(OBJ_PAYLOAD(argStrObj), thisArg, thisLength);
  return argStrObj;
}

/**
 * Convert the Integer srcInteger into a string in base `base` mutating
 * `destStr` (which must be large enough to hold the result).
 * Returns the number of characters in `destStr`.
 */
int64_t rapid_bigint_get_str(ObjPtr destStr, const ObjPtr srcInteger, int base) {
  int32_t iSize = (int32_t)OBJ_SIZE(srcInteger);
  int32_t nLimbs = abs(iSize);
  if (nLimbs > 1024) {
    // TODO: use temporary heap allocation instead of stack space
    rapid_C_crash("bigint too big");
  }
  if (base != 10) {
    // TODO: allow other bases
    rapid_C_crash("Integer->String base must be 10");
  }

  mp_limb_t limbsCopy[nLimbs];
  memcpy(limbsCopy, OBJ_PAYLOAD(srcInteger), sizeof(mp_limb_t) * nLimbs);

  int needsSign = (iSize < 0) ? 1 : 0;
  unsigned char *s = OBJ_PAYLOAD(destStr);
  s += needsSign;
  mp_size_t numDigits = mpn_get_str(s, base, limbsCopy, nLimbs);

  // GMP docs say, there might be leading zeroes in the resulting string
  // TODO: remove leading zeroes

  // We need to add the ASCII value for '0' = 0x30 to each digit. Since we know
  // that the input buffer size is aligned to 8 byte blocks, we can update 8
  // characters at once.
  s = OBJ_PAYLOAD(destStr);
  // We know that the allocated payload size is a multiple of 8, so it is save
  // to access up to 7 bytes past `numDigits` length, given that i % 8 == 0
  // In other words, if we can access s[i] (where i % 8 == 0), we can also
  // access s[i + 7].
  for (int i = 0; i < (numDigits + needsSign); i += 8) {
    *(uint64_t *)(s + i) = 0x3030303030303030 + *(uint64_t *)(s + i);
  }
  if (needsSign) {
    ((char *)OBJ_PAYLOAD(destStr))[0] = '-';
  }

  return numDigits + needsSign;
}

/**
 * Convert the string `srcStr` into a big Integer object (using base 10)
 * `destIntegerObj` must be big enough and have its allocated size set
 *
 * return 0 on success, != 0 on error
 */
int32_t rapid_bigint_set_str(ObjPtr destIntegerObj, const ObjPtr srcStr) {
  assert(OBJ_TYPE(srcStr) == OBJ_TYPE_STRING);
  assert(OBJ_TYPE(destIntegerObj) == OBJ_TYPE_BIGINT);

  int strLength = OBJ_SIZE(srcStr);
  if (strLength > 1024) {
    // TODO: use temporary heap allocation instead of stack space
    rapid_C_crash("bigint source string too long");
  }

  int maxSize = OBJ_SIZE(destIntegerObj);
  assert(maxSize >= (1 + strLength / 19));

  unsigned char scratch[strLength];
  const unsigned char *src = OBJ_PAYLOAD(srcStr);
  bool negate = false;
  if (*src == '-') {
    negate = true;
    ++src;
    --strLength;
  } else if (*src == '+') {
    ++src;
    --strLength;
  }

  for (int i = 0; i < strLength; ++i) {
    if (!isdigit(*src)) {
      // set dest Integer to "0" on error
      destIntegerObj->hdr = MAKE_HEADER(OBJ_TYPE_BIGINT, 0);
      return -1;
    }
    scratch[i] = (*src) - '0';
    ++src;
  }

  mp_size_t nlimbs = mpn_set_str(OBJ_PAYLOAD(destIntegerObj), scratch, strLength, 10);
  assert(nlimbs <= maxSize);
  int real_size = rapid_bigint_real_size(OBJ_PAYLOAD(destIntegerObj), nlimbs);

  destIntegerObj->hdr = MAKE_HEADER(OBJ_TYPE_BIGINT, negate ? (-real_size) : real_size);

  return 0;
}

/**
 * Count the number of non-zero limbs in a GMP integer
 * p : pointer to first limb (LSB)
 * n : number of limbs in p
 */
int64_t rapid_bigint_real_size(const mp_limb_t *p, int64_t n) {
  for (;n > 0; --n) {
    if (p[n - 1] != 0) {
      return n;
    }
  }
  return 0;
}

/**
 * Left-Shift a big Integer in-place
 */
int64_t rapid_bigint_lshift_inplace(mp_limb_t *p, int64_t n, unsigned int count) {
  // GMP's mpn_lshift only supports shifting by an amount < GMP_LIMB_BITS
  // so first, we "left-shift" complete limbs using memmove and then shift the
  // remaining bits using mpn_lshift
  unsigned int full_limbs = count / GMP_LIMB_BITS;
  memmove(p + full_limbs, p, n - full_limbs);
  memset(p, 0, full_limbs * sizeof(mp_limb_t));
  unsigned int rest = count % GMP_LIMB_BITS;
  if (rest > 0) {
    mpn_lshift(p, p, n, rest);
  }
  return rapid_bigint_real_size(p, n);
}

/**
 * Right-Shift a big Integer in-place
 */
int64_t rapid_bigint_rshift_inplace(mp_limb_t *p, int64_t n, unsigned int count) {
  // GMP's mpn_rshift only supports shifting by an amount < GMP_LIMB_BITS
  // so first, we "right-shift" complete limbs using memmove and then shift the
  // remaining bits using mpn_rshift
  unsigned int full_limbs = count / GMP_LIMB_BITS;
  unsigned int max_size = n - full_limbs;
  memmove(p, p + full_limbs, max_size);
  unsigned int rest = count % GMP_LIMB_BITS;
  if (rest > 0) {
    mpn_rshift(p, p, max_size, rest);
  }
  return rapid_bigint_real_size(p, max_size);
}

/**
 * Return length in bytes of the string's UTF-8 encoding.
 */
int64_t rapid_string_bytelength(Idris_TSO *base, ObjPtr strObj) {
  assert(OBJ_TYPE(strObj) == OBJ_TYPE_STRING);
  // As strings are stored as UTF-8, we can just return the raw size:
  int length = OBJ_SIZE(strObj);
  return length;
}

uint32_t utf8_bytes_to_codepoints(const char *s, uint32_t n) {
  uint32_t count = 0;
  const char *end = s + n;
  for (; s < end; count++) {
    unsigned char c = *(unsigned char *)s;
    if (c < 0x80) {
      // single-byte codepoint
      s++;
    } else {
      c &= 0xf0;
      if (c == 0xe0) {
        // leading byte = 0b1110 xxxx
        s += 3;
      } else if (c == 0xf0) {
        // leading byte = 0b1111 0xxx
        s += 4;
      } else {
        // leading byte = 0b110x xxxx
        s += 2;
      }
    }
  }
  return count;
}

/**
 * Forward the given pointer `s` by `n` codepoints
 *
 * @param s Pointer to an UTF-8 encoded string
 * @param n How many codepoints should be skipped
 * @param max How many bytes may be forwarded at most
 * @return The pointer forwarded by `n` codepoints
 */
const char *utf8_codepoints_to_bytes(const char *s, uint32_t n, uint32_t max) {
  const char *end = s + max;
  for (; n > 0 && (s < end); n--) {
    unsigned char c = *(unsigned char *)s;
    if (c < 0x80) {
      // single-byte codepoint
      s++;
    } else {
      c &= 0xf0;
      if (c == 0xe0) {
        // leading byte = 0b1110 xxxx
        s += 3;
      } else if (c == 0xf0) {
        // leading byte = 0b1111 0xxx
        s += 4;
      } else {
        // leading byte = 0b110x xxxx
        s += 2;
      }
    }
  }
  if (s > end) {
    return end;
  }
  return s;
}

uint32_t utf8_codepoints_bytelen(const char *s, uint32_t n, uint32_t max) {
  return utf8_codepoints_to_bytes(s, n, max) - s;
}

/**
 * Decode a single codepoint from the UTF-8 encoded string s
 */
uint32_t utf8_decode1(const char *s) {
  unsigned char *p = (unsigned char *)s;
  unsigned char c = p[0];
  if (c < 0x80) {
    return c;
  } else {
    c &= 0xf0;
    if (c == 0xe0) {
      // leading byte = 0b1110 xxxx
      // -> 3 bytes
      return
        (uint32_t)(p[0] & 0x0f) << 12
        | (uint32_t)(p[1] & 0x3f) << 6
        | (p[2] & 0x3f);
    } else if (c == 0xf0) {
      // leading byte = 0b1111 0xxx
      // -> 4 bytes
      return
        (uint32_t)(p[0] & 0x03) << 18
        | (uint32_t)(p[1] & 0x3f) << 12
        | (uint32_t)(p[2] & 0x3f) << 6
        | (p[3] & 0x3f);
    } else {
      // leading byte = 0b110x xxxx
      // -> 2 bytes
      return (uint32_t)(p[0] & 0x1f) << 6 | (p[1] & 0x3f);
    }
  }
}

/**
 * Decode a single codepoint from the UTF-8 encoded string s, and returns
 * decoded byte length.
 *
 * returns (codepoint | (byte_count << 32))
 */
uint64_t utf8_decode1_length(const char *s) {
  unsigned char *p = (unsigned char *)s;
  unsigned char c = p[0];
  if (c < 0x80) {
    return (1ull << 32) | c;
  } else {
    c &= 0xf0;
    if (c == 0xe0) {
      // leading byte = 0b1110 xxxx
      // -> 3 bytes
      return
        (3ull << 32)
        | (uint32_t)(p[0] & 0x0f) << 12
        | (uint32_t)(p[1] & 0x3f) << 6
        | (p[2] & 0x3f);
    } else if (c == 0xf0) {
      // leading byte = 0b1111 0xxx
      // -> 4 bytes
      return
        (4ull << 32)
        | (uint32_t)(p[0] & 0x03) << 18
        | (uint32_t)(p[1] & 0x3f) << 12
        | (uint32_t)(p[2] & 0x3f) << 6
        | (p[3] & 0x3f);
    } else {
      // leading byte = 0b110x xxxx
      // -> 2 bytes
      return
        (2ull << 32)
        | (uint32_t)(p[0] & 0x1f) << 6
        | (p[1] & 0x3f);
    }
  }
}

/**
 * Encode a single Unicode codepoint to UTF-8, write the result to `dst`,
 * return the number of bytes written (min. 1, max. 4)
 */
uint32_t utf8_encode1(char *dst, uint32_t codepoint) {
  unsigned char *p = (unsigned char *)dst;
  if (codepoint <= 0x7f) {
    p[0] = codepoint;
    return 1;
  } else if (codepoint <= 0x07ff) {
    p[0] = 0xc0 |  (codepoint >> 6);
    p[1] = 0x80 | ((codepoint >> 0) & 0x3f);
    return 2;
  } else if (codepoint <= 0xffff) {
    p[0] = 0xe0 |  (codepoint >> 12);
    p[1] = 0x80 | ((codepoint >>  6) & 0x3f);
    p[2] = 0x80 | ((codepoint >>  0) & 0x3f);
    return 3;
  } else {
    p[0] = 0xf0 | ((codepoint >> 18) & 0x03);
    p[1] = 0x80 | ((codepoint >> 12) & 0x3f);
    p[2] = 0x80 | ((codepoint >>  6) & 0x3f);
    p[3] = 0x80 | ((codepoint >>  0) & 0x3f);
    return 4;
  }
}

/**
 * Return the number of bytes required to encode the given codepoint in UTF-8
 * (min. 1, max. 4)
 */
uint32_t utf8_encode1_length(uint32_t codepoint) {
  if (codepoint <= 0x7f) {
    return 1;
  } else if (codepoint <= 0x07ff) {
    return 2;
  } else if (codepoint <= 0xffff) {
    return 3;
  } else {
    return 4;
  }
}

void rapid_builtin_init(int argc, char **argv) {
  rapid_global_argc = argc;
  rapid_global_argv = argv;
}

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#include <sys/errno.h>
#include <sys/mman.h>
#include <sys/stat.h>

#include "builtin.h"
#include "gc.h"
#include "memory.h"
#include "object.h"
#include "rts.h"

static const size_t RAPID_STACK_SIZE = 128 * 1024 * 1024;

extern int64_t idris_enter(Idris_TSO *baseTSO);
typedef int64_t (* taskfun)(Idris_TSO *);
extern void rapid_run_task(taskfun f, Idris_TSO *tso, jmp_buf jmpBack, void *stackTop);

struct RTSConfig *rapid_global_config;

void idris_rts_crash(long arg0) {
  printf("CRASH called: %ld\n", arg0);
  exit(3);
}

void idris_rts_crash_msg(ObjPtr msg) {
  int length = OBJ_SIZE(msg);
  const char *str = (const char *)&(msg->data);
  fprintf(stdout, "ERROR: ");
  fwrite(str, length, 1, stdout);
  fprintf(stdout, "\n");
  exit(4);
}

void rapid_crash(const char *msg) {
  fprintf(stderr, "ERROR: %s\n", msg);
  exit(10);
}

void rapid_C_crash(const char *msg) {
  fprintf(stderr, "ERROR: %s\n", msg);
  exit(5);
}

void task_start(Idris_TSO *tso) {
  int jump_result;
  if ((jump_result = setjmp(tso->sched_jmp_buf)) == 0) {
    rapid_run_task(idris_enter, tso, tso->sched_jmp_buf, tso->stack_top);
    assert(0 && "rapid_run_task does not return");
  } else {
    // fprintf(stderr, "task finished: %d\n", jump_result);
  }

  // we can not return from this function
}

void rapid_rts_init() {
  rapid_global_config = malloc(sizeof(struct RTSConfig));
  rapid_global_config->debug_always_gc = false;
  rapid_global_config->debug_heap_write_poison = false;
}

int rts_main(int argc, char **argv) {
  rapid_rts_init();
  rapid_memory_init();
  rapid_gc_init();
  rapid_builtin_init(argc, argv);

  Idris_TSO *tso = calloc(1, sizeof(Idris_TSO));
  rapid_gc_setup_heap(tso);
  tso->rapid_errno = 1;

  tso->stack_size = RAPID_STACK_SIZE;
  size_t pagesize = sysconf(_SC_PAGESIZE);
  void *stackmem = mmap(0, tso->stack_size + 2 * pagesize, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (stackmem == MAP_FAILED) {
    fprintf(stderr, "stack alloc failed\n");
    exit(8);
  }

  // map guard page below the stack
  void *guard_page_bottom = mmap(stackmem, pagesize, PROT_NONE, MAP_PRIVATE | MAP_ANONYMOUS | MAP_FIXED, -1, 0);
  if (guard_page_bottom == MAP_FAILED) {
    fprintf(stderr, "stack bottom guard page alloc failed\n");
    exit(9);
  }
  void *guard_page_top = mmap((char *)stackmem + pagesize + tso->stack_size, pagesize, PROT_NONE, MAP_PRIVATE | MAP_ANONYMOUS | MAP_FIXED, -1, 0);
  if (guard_page_top == MAP_FAILED) {
    fprintf(stderr, "stack top guard page alloc failed\n");
    exit(9);
  }

  tso->stack_bottom = (char *)stackmem + pagesize;
  tso->stack_top = (char *)(tso->stack_bottom) + tso->stack_size;

  task_start(tso);

#ifdef RAPID_GC_STATS_ENABLED
  rapid_gc_finalize_stats(tso);

  fprintf(stderr, "GC stats:\n");
  fprintf(stderr, "total GC count:%14llu collections\n" , tso->gc_stats.gc_count);
  fprintf(stderr, "copied total: %15llu bytes\n"        , tso->gc_stats.copied_bytes_total);
  fprintf(stderr, "alloc total:  %15llu bytes\n"        , tso->gc_stats.allocated_bytes_total);
  fprintf(stderr, "collect total:%15llu bytes\n"        , tso->gc_stats.collected_bytes_total);
  fprintf(stderr, "total pauses: %15.3lf seconds\n"      , tso->gc_stats.pause_ns_total / 1000000000.0 );
  fprintf(stderr, "max pause:    %15.0lf milliseconds\n"        , tso->gc_stats.pause_ns_max / 1000000.0 );
  if (tso->gc_stats.gc_count) {
    fprintf(stderr, "avg pause:    %15.0lf milliseconds\n"        , tso->gc_stats.pause_ns_total / tso->gc_stats.gc_count / 1000000.0 );
  } else {
    fprintf(stderr, "avg pause:    %15s milliseconds\n"        , "N/A");
  }
#endif

  return 0;
}

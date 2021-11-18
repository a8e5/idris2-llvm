#pragma once

#include <unistd.h>
#include <setjmp.h>
#include <stdbool.h>
#include <stdint.h>

struct RTSConfig {
  bool debug_always_gc;
  bool debug_heap_write_poison;
};

extern struct RTSConfig *rapid_global_config;

struct Idris_GC_stats {
  uint64_t gc_count;
  uint64_t allocated_bytes_total;
  uint64_t copied_bytes_total;
  uint64_t collected_bytes_total;
  uint64_t pause_ns_total;
  uint64_t pause_ns_max;
};

struct block_descr;

/**
 * Thread State Object - keep track of all state for one Idris thread
 */
struct Idris_TSO_t {
  // nursery area - all objects are allocated here
  uint8_t *nurseryStart; //< currently unused
  uint8_t *nurseryNext; // next allocation will return this pointer
  uint8_t *nurseryEnd;

  // copy of libc's errno for later retrieval by the RTS
  int rapid_errno;

  // this thread's runtime stack
  void *stack_bottom;
  void *stack_top;
  size_t stack_size;

  // last requested allocation size that could not be fulfilled and triggered
  // the GC
  uint64_t heap_alloc;
  size_t next_nursery_size;
  size_t used_nursery_size;

  struct block_descr *nurseryHead; ///< first nursery block
  struct block_descr *nurseryCur; ///< current nursery block

  struct Idris_GC_stats gc_stats;

  jmp_buf sched_jmp_buf;
};

typedef struct Idris_TSO_t Idris_TSO;

typedef uint64_t Word;

#define POINTER_SIZE (sizeof(void*))

void _Noreturn rapid_C_crash(const char *msg);

void *rapid_C_allocate(Idris_TSO *base, int32_t size) __attribute__((__malloc__)) __attribute__((alloc_size(2)));

int rts_main(int argc, char **argv);

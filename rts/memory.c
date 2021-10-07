/*
 * (c) The GHC Team, 1998-2012
 * (c) Johann Rudloff, 2021
 *
 * Significant parts of the memory management interface and its implementation
 * are copied (or at least inspired) from GHC. The license for those portions
 * can be found in LICENSES/LicenseRef-BSD-3-Clause-GHC.txt
 */

#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

#include <pthread.h>

#define BLOCK_SHIFT 12
#define BLOCK_SIZE (uint64_t)(1 << BLOCK_SHIFT)

#define ROUND_UP(x, size) ((((uint64_t)(x) + (size) - 1) / (size)) * (size))
#define BLOCK_ROUND_UP(x) (ROUND_UP(x, BLOCK_SIZE))

#define CLUSTER_SHIFT 20
#define CLUSTER_SIZE (1 << CLUSTER_SHIFT)
const unsigned long CLUSTER_BLOCK_MASK = CLUSTER_SIZE - 1;
const unsigned long CLUSTER_CLUSTER_MASK = ~CLUSTER_BLOCK_MASK;
#define CLUSTER_ROUND_UP(x) (ROUND_UP(x, CLUSTER_SIZE))

#define BLOCKDESCR_SIZE 0x40
#define CLUSTER_FIRST_BLOCK_OFFSET (BLOCK_ROUND_UP(BLOCKDESCR_SIZE * (CLUSTER_SIZE/BLOCK_SIZE)))

static_assert(CLUSTER_FIRST_BLOCK_OFFSET == 16384, "verify expected first block offset");

void *alloc_clusters(size_t count);
void free_clusters(void *p);
bool is_heap_alloc(void *p);

#define NUM_GENERATIONS 3

void CRASH(const char *msg) {
  fputs(msg, stderr);
  fputs("\n", stderr);
  exit(17);
}

// minimum object size: 2^4 = 16 bytes
#define SIZECLASS_MIN 4
// maximum object size: 2^11 = 2048 bytes
#define SIZECLASS_MAX 11

#define LARGE_OBJECT_THRESHOLD (1 << SIZECLASS_MAX)

struct cellblock_info {
  // this block stores objects up to size 2^sizeclass
  uint8_t sizeclass;
  // *index* of the first free cell, the cell contains the index and number of
  // the next free cell.
  uint16_t free_cell;
};

struct free_cell_info {
  uint16_t next;
  uint16_t length;
};

// block descriptor
struct block_descr {
  void *start;
  void *free;
  size_t num_blocks;
  struct cellblock_info cellinfo; // only for cell-blocks
  void *pending; // only needed during GC
  struct block_descr *link;
  void *other3;
  void *other4;
};

static_assert(sizeof(struct block_descr) <= BLOCKDESCR_SIZE, "block_descr struct is too large");

struct rapid_generation {
  int generation_number;
  struct block_descr *free;
  struct block_descr *large_objects;
  // nonmoving:
  struct block_descr *free_ptr[SIZECLASS_MAX - SIZECLASS_MIN];
};

struct rapid_memory {
  struct rapid_generation generations[NUM_GENERATIONS];
};

static inline struct block_descr *get_block_descr(void *X) {
  uint64_t cluster_start = (uint64_t)(X) & CLUSTER_CLUSTER_MASK;
  uint64_t block_number_in_cluster = ((uint64_t)(X) & CLUSTER_BLOCK_MASK) / BLOCK_SIZE;
  uint64_t block_descr_addr = cluster_start | (block_number_in_cluster * BLOCKDESCR_SIZE);
  return (struct block_descr *)block_descr_addr;
}

static pthread_mutex_t rapid_memory_mutex;

void rapid_memory_init() {
  pthread_mutex_init(&rapid_memory_mutex, NULL);
}


void *alloc_clusters(size_t count) {
  assert(count > 0);

  void *mem;
  if (!(mem = aligned_alloc(CLUSTER_SIZE, CLUSTER_SIZE * count))) {
    CRASH("memory allocation failed");
  }

  return mem;
}


// TESTS

void write_pattern(void *start, size_t size) {
  for (uint64_t *p = (uint64_t *)start; (uint64_t)p < (uint64_t)start + size; ++p) {
    *p = 0ull;
  }
}

int test_memory() {
  rapid_memory_init();

  void *cluster1 = alloc_clusters(1);
  assert((uint64_t)cluster1 % CLUSTER_SIZE == 0);
  write_pattern(cluster1, CLUSTER_SIZE);

  void *cluster2 = alloc_clusters(2);
  assert((uint64_t)cluster2 % CLUSTER_SIZE == 0);
  write_pattern(cluster2, CLUSTER_SIZE);

  get_block_descr(0);

  return 0;
}

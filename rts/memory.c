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
#define CLUSTER_FIRST_BLOCK_INDEX (CLUSTER_FIRST_BLOCK_OFFSET / BLOCK_SIZE)
// max. number of blocks in one cluster (taking offset into account)
#define CLUSTER_MAX_BLOCKS ((CLUSTER_SIZE / BLOCK_SIZE) - CLUSTER_FIRST_BLOCK_INDEX)

static_assert(CLUSTER_FIRST_BLOCK_OFFSET == 16384, "verify expected first block offset");

void *alloc_clusters(size_t count);
void free_clusters(void *p);
bool is_heap_alloc(void *p);

void add_to_freelist(void *start);
void *take_from_freelist(size_t num_blocks);

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
  /// pointer to this block's memory
  void *start;
  /// `NULL` if this block is in the free list, otherwise pointer to the first
  /// free byte
  void *free;
  size_t num_blocks;
  struct cellblock_info cellinfo; // only for cell-blocks
  void *pending; // only needed during GC
  struct block_descr *link;
  void *other3;
  void *other4;
};

static_assert(sizeof(struct block_descr) == BLOCKDESCR_SIZE, "block_descr struct is too large");

struct rapid_generation {
  int generation_number;
  struct block_descr *free;
  struct block_descr *large_objects;
  // nonmoving:
  struct block_descr *free_ptr[SIZECLASS_MAX - SIZECLASS_MIN];
};

#define NUM_FREE_LISTS (CLUSTER_SHIFT - BLOCK_SHIFT)
struct rapid_memory {
  struct rapid_generation generations[NUM_GENERATIONS];

  /**
   * Store block groups by log2(num_blocks)
   * free_list[n] contains block groups where 2^n <= num_blocks < 2^(n+1)
   * So for 1MiB-sized clusters each containing 256 4KiB-sized blocks, we end
   * up with 8 free_lists, the largest one free_list[7] containing all free
   * block groups of 128 - 255 blocks.
   */
  struct block_descr *free_list[NUM_FREE_LISTS];
};

static struct rapid_memory rapid_mem;

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

/*
static inline struct block_descr *cluster_get_bdescr(void *cluster, size_t index) {
  return (struct block_descr *)cluster + index;
}
*/

static inline void *cluster_get_block_start(void *cluster, size_t index) {
  return (char *)cluster + (index * BLOCK_SIZE);
}

static inline size_t log2i(size_t n) {
  assert(n > 0);
  assert(n < (1 << NUM_FREE_LISTS));
  return __builtin_clzll(n) ^ (sizeof(size_t)*8 - 1);
}

static inline void *block_group_end(struct block_descr *bdesc) {
  return (char *)bdesc->start + (bdesc->num_blocks * BLOCK_SIZE);
}

void init_block_group(void *start, size_t num_blocks) {
  struct block_descr *bdesc = get_block_descr(start);
  *bdesc = (struct block_descr){ 0, };
  bdesc->start = start;
  bdesc->free = start;
  bdesc->num_blocks = num_blocks;
}

/**
 * Split a block group after `num_blocks` blocks, put the rest into the free
 * list
 *
 * @returns the value of param `start`
 */
void *split_block_group(void *start, size_t num_blocks) {
  struct block_descr *head = get_block_descr(start);
  assert(num_blocks < head->num_blocks);

  size_t tail_num_blocks = head->num_blocks - num_blocks;
  head->num_blocks = num_blocks;
  void *tail_start = block_group_end(head);
  init_block_group(tail_start, tail_num_blocks);
  add_to_freelist(tail_start);

  return start;
}

void add_to_freelist(void *start) {
  // TODO: coalesce adjacent empty block groups
  struct block_descr *bdesc = get_block_descr(start);
  assert(bdesc->free);
  bdesc->free = NULL;
  size_t free_list_index = log2i(bdesc->num_blocks);
  struct block_descr *old_head = rapid_mem.free_list[free_list_index];
  rapid_mem.free_list[free_list_index] = bdesc;
  bdesc->link = old_head;
}

/**
 * Try to fetch a block group from the free list
 *
 * May return `NULL` if the freelist contains no suitable block group
 */
void *take_from_freelist(size_t num_blocks) {
  // we look in the bin that is one "larger" than our target group size,
  // because that guarantees that any match will fit:
  size_t num_log2 = log2i(num_blocks) + 1;
  // TODO: improve efficency by looking in other bins as well
  if (num_log2 < NUM_FREE_LISTS && rapid_mem.free_list[num_log2]) {
    struct block_descr *bdesc = rapid_mem.free_list[num_log2];
    assert(bdesc->free == 0);
    rapid_mem.free_list[num_log2] = bdesc->link;
    assert(bdesc->link == 0 || bdesc->link->free == 0);

    assert(bdesc->num_blocks > num_blocks);
    return split_block_group(bdesc->start, num_blocks);
  }

  return NULL;
}

void *alloc_block_group(size_t num_blocks) {
  assert(num_blocks < CLUSTER_MAX_BLOCKS && "big groups not yet implemented");

  void *start = take_from_freelist(num_blocks);
  if (start) {
    return start;
  }

  void *cluster = alloc_clusters(1);
  start = cluster_get_block_start(cluster, CLUSTER_FIRST_BLOCK_INDEX);
  init_block_group(start, CLUSTER_MAX_BLOCKS);

  if (num_blocks < CLUSTER_MAX_BLOCKS) {
    return split_block_group(start, num_blocks);
  }

  return start;
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

  assert(log2i(1) == 0);
  assert(log2i(2) == 1);
  assert(log2i(3) == 1);
  assert(log2i(4) == 2);
  assert(log2i(5) == 2);
  assert(log2i(6) == 2);
  assert(log2i(7) == 2);
  assert(log2i(8) == 3);
  assert(log2i(CLUSTER_MAX_BLOCKS) == NUM_FREE_LISTS - 1);

  for (size_t i = 1; i < CLUSTER_MAX_BLOCKS; ++i) {
    void *mymem = alloc_block_group(i);
    assert(mymem);
    assert(mymem == get_block_descr(mymem)->start);
    write_pattern(mymem, i * BLOCK_SIZE);
  }

  return 0;
}

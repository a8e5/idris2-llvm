/*
 * (c) The GHC Team, 1998-2012
 * (c) Johann Rudloff, 2021
 *
 * Significant parts of the memory management interface and its implementation
 * are copied (or at least inspired) from GHC. The license for those portions
 * can be found in LICENSES/LicenseRef-BSD-3-Clause-GHC.txt
 */

#include "memory.h"

#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <pthread.h>

static_assert(CLUSTER_FIRST_BLOCK_OFFSET == 16384, "verify expected first block offset");

static void add_to_freelist(void *start);
static void *take_from_freelist(size_t num_blocks);

void CRASH(const char *msg) {
  fputs(msg, stderr);
  fputs("\n", stderr);
  exit(17);
}

static_assert(sizeof(struct block_descr) == BLOCKDESCR_SIZE, "block_descr struct is too large");

struct rapid_memory rapid_mem;

static pthread_mutex_t rapid_memory_mutex;

void rapid_memory_init() {
  pthread_mutex_init(&rapid_memory_mutex, NULL);

  rapid_mem.all_clusters = hashset_create();
}


void *alloc_clusters(size_t count) {
  assert(count > 0);

  void *mem;
  if (!(mem = aligned_alloc(CLUSTER_SIZE, CLUSTER_SIZE * count))) {
    CRASH("memory allocation failed");
  }

  uint64_t memarea = (uint64_t)mem >> CLUSTER_SHIFT;
  hashset_add(rapid_mem.all_clusters, (void *)memarea);

  return mem;
}


/**
 * Find out if a given memory address is managed by this memory manager
 *
 * Iff is_heap_alloc(X) returns `true`, it is safe to dereference the result of
 * the call `get_block_descr(X)`
 */
bool is_heap_alloc(void *addr) {
  uint64_t cluster_start = (uint64_t)(addr) & CLUSTER_CLUSTER_MASK;
  uint64_t memarea = cluster_start >> CLUSTER_SHIFT;
  return hashset_is_member(rapid_mem.all_clusters, (void *)memarea);
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

void init_block_group(void *start, size_t num_blocks) {
  struct block_descr *bdesc = get_block_descr(start);
  *bdesc = (struct block_descr){ 0, };
  bdesc->start = start;
  bdesc->free = start;
  bdesc->num_blocks = num_blocks;

  // TODO: this is inefficient, we can probably just zero memory in
  // alloc_block_group right before returning it
  memset(bdesc->start, 0, bdesc->num_blocks * BLOCK_SIZE);
}

/**
 * Split a block group after `num_blocks` blocks, put the rest into the free
 * list
 *
 * @returns the value of param `start`
 */
static void *split_block_group(void *start, size_t num_blocks) {
  struct block_descr *head = get_block_descr(start);
  assert(num_blocks < head->num_blocks);

  size_t tail_num_blocks = head->num_blocks - num_blocks;
  head->num_blocks = num_blocks;
  void *tail_start = block_group_get_end(head);
  init_block_group(tail_start, tail_num_blocks);
  add_to_freelist(tail_start);

  return start;
}

static void add_to_freelist(void *start) {
  // TODO: coalesce adjacent empty block groups
  struct block_descr *bdesc = get_block_descr(start);

  // make sure the bdescr is not already on the freelist:
  // bdescrs on the freelist have their ->free set to NULL
  assert(bdesc->free);

  bdesc->free = NULL;
  size_t free_list_index = log2i(bdesc->num_blocks);
  assert(free_list_index < NUM_FREE_LISTS);

  dbl_link_insert(&rapid_mem.free_list[free_list_index], bdesc);
}

void free_block_group(void *start) {
  struct block_descr *bdesc = get_block_descr(start);
  assert(bdesc->free);
  assert(bdesc->num_blocks);

  if (bdesc->num_blocks > CLUSTER_MAX_BLOCKS) {
    memset(bdesc->start, 0, BLOCK_SIZE * bdesc->num_blocks);
    fprintf(stderr, "NOT IMPLEMENTED: free oversized allocation\n");
    return;
  }

  // TODO: is there something else, that needs to be done?
  add_to_freelist(start);
}

size_t memstats_count_freelist_groups() {
  size_t num_freelist_groups = 0;
  for(size_t i = 0; i < NUM_FREE_LISTS; ++i) {
    struct block_descr *bdesc = rapid_mem.free_list[i];
    while (bdesc) {
      assert(bdesc->free == NULL);
      num_freelist_groups++;
      bdesc = bdesc->link;
    }
  }
  return num_freelist_groups;
}

#define MAX_FREELIST_TRIES 16

/**
 * Try to fetch a block group from the free list
 *
 * May return `NULL` if the freelist contains no suitable block group
 */
static void *take_from_freelist(size_t num_blocks) {
  // we first look in the bin that is one size class "larger" than our target
  // group size, because that guarantees that any match will fit:
  size_t num_log2 = log2i(num_blocks) + 1;

  if (num_log2 < NUM_FREE_LISTS && rapid_mem.free_list[num_log2]) {
    struct block_descr *bdesc = rapid_mem.free_list[num_log2];
    assert(bdesc->free == 0);
    dbl_link_remove(&rapid_mem.free_list[num_log2], bdesc);

    assert(bdesc->num_blocks > num_blocks);
    return split_block_group(bdesc->start, num_blocks);
  }

  if (num_log2 == 0) {
    return NULL;
  }

  num_log2 = num_log2 - 1;
  if (num_log2 < NUM_FREE_LISTS && rapid_mem.free_list[num_log2]) {
    struct block_descr *bdesc = rapid_mem.free_list[num_log2];
    for(size_t i=0; i<MAX_FREELIST_TRIES && bdesc; bdesc=bdesc->link, ++i) {
      assert(bdesc->free == 0);
      if (bdesc->num_blocks >= num_blocks) {
        assert(bdesc->link == 0 || bdesc->link->free == 0);
        dbl_link_remove(&rapid_mem.free_list[num_log2], bdesc);

        if (bdesc->num_blocks > num_blocks) {
          return split_block_group(bdesc->start, num_blocks);
        } else {
          assert(bdesc->num_blocks == num_blocks);
          return bdesc->start;
        }
      }
    }

  }


  return NULL;
}

/**
 * Number of required clusters for an oversize-allocation
 */
static inline size_t required_clusters(size_t num_blocks) {
  assert(num_blocks > CLUSTER_MAX_BLOCKS);
  size_t BLOCKS_PER_OVERSIZE_CLUSTER = CLUSTER_SIZE / BLOCK_SIZE;
  return 1 + (num_blocks - CLUSTER_MAX_BLOCKS + BLOCKS_PER_OVERSIZE_CLUSTER - 1)
    / BLOCKS_PER_OVERSIZE_CLUSTER;
}

void *alloc_oversized_block_group(size_t num_blocks) {
  size_t num_clusters = required_clusters(num_blocks);

  void *cluster = alloc_clusters(num_clusters);
  void *start = cluster_get_block_start(cluster, CLUSTER_FIRST_BLOCK_INDEX);
  init_block_group(start, num_blocks);

  return start;
}

void *alloc_block_group(size_t num_blocks) {
  if (num_blocks > CLUSTER_MAX_BLOCKS) {
    return alloc_oversized_block_group(num_blocks);
  }
  assert(num_blocks <= CLUSTER_MAX_BLOCKS && "big groups not yet implemented");

  void *start = take_from_freelist(num_blocks);
  if (start) {
    init_block_group(start, num_blocks);
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

void *
alloc_large_obj(size_t generation_number, uint64_t size) {
  uint64_t required_blocks = (size + BLOCK_SIZE - 1) / BLOCK_SIZE;
  void *data = alloc_block_group(required_blocks);
  struct block_descr *bdescr = get_block_descr(data);
  bdescr->link = rapid_mem.generations[generation_number].large_objects;
  if (bdescr->link) {
    assert(!bdescr->link->back);
    bdescr->link->back = bdescr;
  }
  bdescr->back = NULL;
  bdescr->flags = BLOCKDESCR_FLAG_LARGE_OBJECT;
  rapid_mem.generations[generation_number].large_objects = bdescr;

  return data;
}

// TESTS

void write_pattern(void *start, size_t size) {
  for (uint64_t *p = (uint64_t *)start; (uint64_t)p < (uint64_t)start + size; ++p) {
    *p = 0ull;
  }
}

static size_t test_big_static_array[4096];

int test_memory() {
  rapid_memory_init();

  // at the start there should be no groups in the freelist:
  assert(memstats_count_freelist_groups() == 0);

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

  void *one_block = alloc_block_group(1);
  assert(one_block);
  struct block_descr *bdescr = get_block_descr(one_block);
  assert(bdescr->num_blocks == 1);

  // since we only alloced one block, the rest of the cluster should be in the
  // freelist as one continuous group:
  assert(memstats_count_freelist_groups() == 1);

  free_block_group(one_block);

  void *allocations[CLUSTER_MAX_BLOCKS];
  for (size_t i = 1; i < CLUSTER_MAX_BLOCKS; ++i) {
    void *mymem = alloc_block_group(i);
    assert(mymem);
    assert(mymem == get_block_descr(mymem)->start);
    assert(is_heap_alloc(mymem));
    write_pattern(mymem, i * BLOCK_SIZE);
    allocations[i] = mymem;
  }

  for (size_t i = 1; i < CLUSTER_MAX_BLOCKS; ++i) {
    free_block_group(allocations[i]);
    allocations[i] = NULL;
  }

  // check some non-heap objects
  assert(!is_heap_alloc((void *)test_memory));
  assert(!is_heap_alloc(&rapid_mem));
  assert(!is_heap_alloc(test_big_static_array));
  assert(!is_heap_alloc(test_big_static_array + 512));

  return 0;
}

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
#include <stddef.h>
#include <stdlib.h>

#include "vendor/hashset.h"

void rapid_memory_init();

void *alloc_clusters(size_t count);
void free_clusters(void *p);
bool is_heap_alloc(void *p);

void *alloc_block_group(size_t num_blocks);
void free_block_group(void *p);

void* alloc_large_obj(size_t generation_number, uint64_t size);

#define BLOCK_SHIFT 12
#define BLOCK_SIZE (uint64_t)(1 << BLOCK_SHIFT)

#define ROUND_UP(x, size) ((((uint64_t)(x) + (size) - 1) / (size)) * (size))
#define BLOCK_ROUND_UP(x) (ROUND_UP(x, BLOCK_SIZE))

#define CLUSTER_SHIFT 20
#define CLUSTER_SIZE (1 << CLUSTER_SHIFT)
#define CLUSTER_BLOCK_MASK (CLUSTER_SIZE - 1)
#define CLUSTER_CLUSTER_MASK (~CLUSTER_BLOCK_MASK)
#define CLUSTER_ROUND_UP(x) (ROUND_UP(x, CLUSTER_SIZE))

#define BLOCKDESCR_SIZE 0x40
#define CLUSTER_FIRST_BLOCK_OFFSET (BLOCK_ROUND_UP(BLOCKDESCR_SIZE * (CLUSTER_SIZE/BLOCK_SIZE)))
#define CLUSTER_FIRST_BLOCK_INDEX (CLUSTER_FIRST_BLOCK_OFFSET / BLOCK_SIZE)
// max. number of blocks in one cluster (taking offset into account)
#define CLUSTER_MAX_BLOCKS ((CLUSTER_SIZE / BLOCK_SIZE) - CLUSTER_FIRST_BLOCK_INDEX)

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

/// block descriptor
struct block_descr {
  /// pointer to this block's memory
  void *start;
  /// `NULL` if this block is in the free list, otherwise pointer to the first
  /// free byte
  void *free;
  size_t num_blocks;
  struct cellblock_info cellinfo; // only for cell-blocks
  void *pending; // only needed during GC
  /// linked-list link to next block
  struct block_descr *link;
  /// linked-list link to previous block
  struct block_descr *back;

  uint16_t flags;
  uint16_t _padding1;
  uint32_t _padding2;
};

#define BLOCKDESCR_FLAG_EVACUATED 0x01u
#define BLOCKDESCR_FLAG_LARGE_OBJECT 0x02u

#define NUM_GENERATIONS 3

// minimum object size: 2^4 = 16 bytes
#define SIZECLASS_MIN 4
// maximum object size: 2^11 = 2048 bytes
#define SIZECLASS_MAX 11

#define LARGE_OBJECT_THRESHOLD (1 << SIZECLASS_MAX)

struct rapid_generation {
  int generation_number;
  struct block_descr *free;

  /// Allocated large objects (doubly-linked)
  struct block_descr *large_objects;

  /// Gather live large objects during GC (singly-linked) that need to be scavenged
  struct block_descr *large_objects_scavenge;

  /// Large objects durign GC that are completely scavenged
  struct block_descr *live_large_objects;

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

  hashset_t all_clusters;
};

extern struct rapid_memory rapid_mem;

static inline struct block_descr *get_block_descr(void *X) {
  uint64_t cluster_start = (uint64_t)(X) & CLUSTER_CLUSTER_MASK;
  uint64_t block_number_in_cluster = ((uint64_t)(X) & CLUSTER_BLOCK_MASK) / BLOCK_SIZE;
  uint64_t block_descr_addr = cluster_start | (block_number_in_cluster * BLOCKDESCR_SIZE);
  return (struct block_descr *)block_descr_addr;
}

static inline size_t block_group_get_size(struct block_descr *bdescr) {
  return bdescr->num_blocks * BLOCK_SIZE;
}

static inline void *block_group_get_end(struct block_descr *bdesc) {
  return (char *)bdesc->start + (BLOCK_SIZE * bdesc->num_blocks);
}

static inline size_t block_group_count_free_bytes(struct block_descr *bdescr) {
  assert(bdescr->free);
  return (size_t)block_group_get_end(bdescr) - (size_t)bdescr->free;
}

/// Remove from doubly-linked list
static inline void dbl_link_remove(struct block_descr **head, struct block_descr *b) {
  if (head && *head == b) {
    *head = b->link;
  }
  if (b->link) {
    b->link->back = b->back;
  }
  if (b->back) {
    b->back->link = b->link;
  }
  b->link = NULL;
  b->back = NULL;
}

/// Insert at beginning of doubly-linked list
static inline void dbl_link_insert(struct block_descr **head, struct block_descr *b) {
  assert(!b->link);
  assert(!b->back);
  b->link = *head;
  if(*head) {
    assert(!(*head)->back);
    (*head)->back = b;
  }
  *head = b;
}

/// Insert at beginning of singly-linked list
static inline void list_insert(struct block_descr **head, struct block_descr *b) {
  assert(!b->link);
  assert(!b->back);
  assert(head);
  b->link = *head;
  //if(*head) {
    //assert(!(*head)->back);
    //(*head)->back = b;
  //}
  *head = b;
}

/// Remove and return first element of singly-linked list
static inline struct block_descr *list_pop(struct block_descr **head) {
  struct block_descr *result = *head;
  if (result) {
    *head = result->link;
    result->link = NULL;
  }
  return result;
}

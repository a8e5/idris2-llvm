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

void rapid_memory_init();

void *alloc_clusters(size_t count);
void free_clusters(void *p);
bool is_heap_alloc(void *p);

void *alloc_block_group(size_t num_blocks);
void free_block_group(void *p);

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
  struct block_descr *link;
  void *other3;
  void *other4;
};

static inline struct block_descr *get_block_descr(void *X) {
  uint64_t cluster_start = (uint64_t)(X) & CLUSTER_CLUSTER_MASK;
  uint64_t block_number_in_cluster = ((uint64_t)(X) & CLUSTER_BLOCK_MASK) / BLOCK_SIZE;
  uint64_t block_descr_addr = cluster_start | (block_number_in_cluster * BLOCKDESCR_SIZE);
  return (struct block_descr *)block_descr_addr;
}

static inline void *block_group_get_end(struct block_descr *bdesc) {
  return (char *)bdesc->start + (BLOCK_SIZE * bdesc->num_blocks);
}

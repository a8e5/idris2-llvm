#include <llvm-statepoint-tablegen.h>

#include "gc.h"
#include "memory.h"
#include "object.h"
#include "rts.h"

#include <assert.h>
#include <time.h>

extern void *get_stackmap();

#define GC_FLAVOUR_ZERO 1
#define GC_FLAVOUR_BDW 2
#define GC_FLAVOUR_STATEPOINT 3

extern uint32_t rapid_gc_flavour;

// at multiple points in the code, we assume, that size_t can hold any address
static_assert(sizeof(size_t) >= sizeof(void *), "void * does not fit in size_t");

// Define selected BDW GC functions, so we don't need BDW's gc/gc.h installed
// to compile the RTS lib.
extern void *GC_malloc(size_t);
extern void GC_init(void);

static statepoint_table_t *rapid_global_stackmap_table;

static inline uint32_t aligned(uint32_t size) {
  return 8 * ((size + 7) / 8);
}

void *rapid_C_allocate(Idris_TSO *base, int32_t size) {
  if (rapid_gc_flavour == GC_FLAVOUR_BDW) {
    return GC_malloc(aligned(size));
  }
  void *m = malloc(size);
  assert(((0x0f & (uint64_t)m) == 0) && "rapid_C_allocate returned non-aligned memory");
  return m;
}

#define DERIVED_PTRSLOT_GET_BASE_IDX(p) (((p).kind) >> 1)

static inline ObjPtr *get_stack_slot(uint8_t *sp, uint8_t *bp, pointer_slot_t slot) {
  if (slot.kind == -1) {
    assert(slot.offset >= 0);
    return (ObjPtr *)(sp + slot.offset);
  } else if (slot.kind == -2) {
    assert(slot.offset < 0);
    return (ObjPtr *)(bp + slot.offset);
  } else {
    // kind LSB: 0 -> relative to stack pointer
    // kind LSB: 1 -> relative to base pointer
    if ((slot.kind & 0x01) == 0) {
      assert(slot.offset >= 0);
      return (ObjPtr *)(sp + slot.offset);
    } else {
      assert(slot.offset < 0);
      return (ObjPtr *)(bp + slot.offset);
    }
  }
}

/**
 * Allocate directly from the nursery
 */
static inline ObjPtr alloc_during_gc(Idris_TSO *base, uint32_t size) {
  uint8_t *p = base->nurseryCur->free;
  assert(((uint64_t)p & 0x07) == 0);
  assert(base->nurseryCur->start <= base->nurseryCur->free);
  assert(base->nurseryCur->free <= block_group_get_end(base->nurseryCur));
  assert(size <= BLOCK_SIZE && "big objects not yet supported");
  if (p + aligned(size) <= (uint8_t *)block_group_get_end(base->nurseryCur)) {
    base->nurseryCur->free = p + aligned(size);
    return (ObjPtr)p;
  } else {
    assert(base->nurseryCur->link);
    base->used_nursery_size += (size_t)base->nurseryCur->free - (size_t)base->nurseryCur->start;
    base->nurseryCur = base->nurseryCur->link;
    p = base->nurseryCur->free;
    base->nurseryCur->free = p + aligned(size);
    return (ObjPtr)p;
  }
}

ObjPtr evacuate(Idris_TSO *base, ObjPtr p) {
  ObjPtr new;
  uint32_t size;

  if (OBJ_IS_INLINE(p)) {
    return p;
  }

  if (OBJ_IS_FWD_INPLACE(p)) {
    uint64_t fwd_target = (p->hdr << 1);
#ifdef RAPID_GC_DEBUG_ENABLED
    fprintf(stderr, "-- in place fwd: %llx -> %llx\n", (uint64_t)p, fwd_target);
#endif
    return (ObjPtr)fwd_target;
  }

  // TODO: replace this with more efficient solution
  if (!is_heap_alloc(p)) {
    return p;
  }

  struct block_descr *bdescr = get_block_descr(p);
  if (bdescr->flags & BLOCKDESCR_FLAG_LARGE_OBJECT) {
    if (!(bdescr->flags & BLOCKDESCR_FLAG_EVACUATED)) {
      bdescr->flags |= BLOCKDESCR_FLAG_EVACUATED;
      dbl_link_remove(&rapid_mem.generations[0].large_objects, bdescr);

      list_insert(&rapid_mem.generations[0].large_objects_scavenge, bdescr);
    }

    // large objects are not copied
    return p;
  }

  size = OBJ_TOTAL_SIZE(p);
  new = alloc_during_gc(base, size);
  memcpy(new, p, size);
#ifdef RAPID_GC_DEBUG_ENABLED
  fprintf(stderr, "-- object copied: %p -> %p (%u bytes)\n", (void *)p, (void *)new, size);
#endif
  p->hdr = OBJ_MAKE_FWD_INPLACE(new);
  return new;
}

static inline void scavenge(Idris_TSO *base, ObjPtr obj) {
#ifdef RAPID_GC_DEBUG_ENABLED
  fprintf(stderr, "================================== scavenge obj start\n");
  dump_obj(obj);
  fprintf(stderr, "================================== scavenge obj start\n");
#endif
  assert(!OBJ_IS_INLINE(obj));
  assert(!OBJ_IS_FWD_INPLACE(obj));
  assert(is_heap_alloc(obj));
  switch(OBJ_TYPE(obj)) {
    case OBJ_TYPE_CLOSURE:
      {
        int argCount = 0xffff & obj->hdr;
        for (int i = 0; i < argCount; ++i) {
          ObjPtr value = OBJ_GET_SLOT(obj, i + 1);
          ObjPtr argCopy = evacuate(base, value);
          OBJ_PUT_SLOT(obj, i + 1, argCopy);
        }
      }
      break;
    case OBJ_TYPE_CON_NO_ARGS:
      {
        int argCount = obj->hdr >> 40;
        for (int i = 0; i < argCount; ++i) {
          ObjPtr value = OBJ_GET_SLOT(obj, i);
          ObjPtr argCopy = evacuate(base, value);
          OBJ_PUT_SLOT(obj, i, argCopy);
        }
      }
      break;
    case OBJ_TYPE_IOREF:
      {
        ObjPtr argCopy = evacuate(base, OBJ_GET_SLOT(obj, 0));
        OBJ_PUT_SLOT(obj, 0, argCopy);
      }
      break;
    case OBJ_TYPE_IOARRAY:
      {
        int arraySize = OBJ_SIZE(obj);
        for (int i = 0; i < arraySize; ++i) {
          ObjPtr value = OBJ_GET_SLOT(obj, i);
          ObjPtr valCopy = evacuate(base, value);
          OBJ_PUT_SLOT(obj, i, valCopy);
        }
      }
      break;
    default:
      break;
  }
#ifdef RAPID_GC_DEBUG_ENABLED
  fprintf(stderr, "================================== scavenge obj end\n");
  dump_obj(obj);
  fprintf(stderr, "================================== scavenge obj end\n");
#endif
}

/**
 * Scavenge the nursery, return `true` if any work has been done
 */
static bool cheney(Idris_TSO *base) {
  bool work_done = false;
  struct block_descr *bdesc = base->nurseryScavenge;
  while (bdesc && bdesc->free != bdesc->start) {
    uint8_t *scan = bdesc->pending;
    while(scan < (uint8_t *)bdesc->free) {
      scavenge(base, (ObjPtr)scan);
      work_done = true;
      scan += aligned(OBJ_TOTAL_SIZE((ObjPtr)scan));
    }

    // TODO: this may not be necessary for every scanned block
    // it may be enough to only do this for the last scanned (not completely
    // full) block:
    bdesc->pending = scan;

    // need to store the last block we actually scanned:
    base->nurseryScavenge = bdesc;

    bdesc = bdesc->link;
  }

  assert(base->nurseryScavenge == base->nurseryCur);
  assert(base->nurseryScavenge->pending == base->nurseryCur->free);

  return work_done;
}

/**
 * Scavenge large objects to evacuate all objects referenced from the large
 * object.
 * Return `true` if any work has been done
 */
static bool scavenge_large_objects(Idris_TSO *base) {
  bool work_done = false;

  struct block_descr *bdescr;
  while((bdescr = list_pop(&rapid_mem.generations[0].large_objects_scavenge))) {
#ifdef RAPID_GC_DEBUG_ENABLED
    fprintf(stderr, "================================== scavenge LARGE obj start\n");
    dump_obj(bdescr->start);
    fprintf(stderr, "================================== scavenge LARGE obj start\n");
#endif

    assert(bdescr->flags & BLOCKDESCR_FLAG_EVACUATED);

    scavenge(base, bdescr->start);
    work_done = true;

    list_insert(&rapid_mem.generations[0].live_large_objects, bdescr);

#ifdef RAPID_GC_DEBUG_ENABLED
    fprintf(stderr, "================================== scavenge LARGE obj end\n");
    dump_obj(bdescr->start);
    fprintf(stderr, "================================== scavenge LARGE obj end\n");
#endif
  }
  return work_done;
}

/**
 * Allocate a chain of linked block groups of at least the requested size (up
 * to CLUSTER_SIZE bytes more).
 */
static struct block_descr *gc_alloc_chain(size_t size) {
  struct block_descr *head = NULL;
  size_t allocated_size = 0;
  do {
    // TODO: allow smaller groups, to avoid fragmentation
    void *mem = alloc_block_group(CLUSTER_MAX_BLOCKS);
    struct block_descr *next_head = get_block_descr(mem);
    next_head->link = head;
    next_head->pending = next_head->start;
    allocated_size += next_head->num_blocks * BLOCK_SIZE;
    head = next_head;
  } while (allocated_size < size);
  return head;
}

static void gc_free_chain(struct block_descr *head) {
  struct block_descr *bdesc = head;
  while (bdesc) {
    struct block_descr *next = bdesc->link;
    free_block_group(bdesc->start);
    bdesc = next;
  }
}

/**
 * Free all non-evacuated large objects, move all scavenged large objects from
 * live_large_objects list to large_objects list.
 */
static void free_large_objects(Idris_TSO *base) {
  // at this point there should be no large objects left to scavenge
  assert(rapid_mem.generations[0].large_objects_scavenge == NULL);

  gc_free_chain(rapid_mem.generations[0].large_objects);
  rapid_mem.generations[0].large_objects = NULL;

  struct block_descr *bdescr;
  while((bdescr = list_pop(&rapid_mem.generations[0].live_large_objects))) {
    assert(bdescr->flags & BLOCKDESCR_FLAG_EVACUATED);
    // reset "EVACUATED" flag for next GC cycle:
    bdescr->flags &= ~BLOCKDESCR_FLAG_EVACUATED;
    dbl_link_insert(&rapid_mem.generations[0].large_objects, bdescr);
  }

  assert(rapid_mem.generations[0].large_objects_scavenge == NULL);
  assert(rapid_mem.generations[0].live_large_objects == NULL);
}

static inline void update_heap_pointers(Idris_TSO *base) {
  base->nurseryNext = base->nurseryCur->free;
  base->nurseryEnd = block_group_get_end(base->nurseryCur);
}

void idris_rts_gc(Idris_TSO *base, uint8_t *sp) {
  /*uint8_t * orig_sp = sp;*/
#ifdef RAPID_GC_DEBUG_ENABLED
  fprintf(stderr, "GC called for %llu bytes, stack pointer: %p\n", base->heap_alloc, (void *)sp);
#endif

#ifdef RAPID_GC_STATS_ENABLED
  struct timespec start_time;
  clock_gettime(CLOCK_MONOTONIC, &start_time);
#endif

  uint64_t returnAddress = *((uint64_t *) sp);
  uint8_t *fp = sp - sizeof(void *);
  sp += sizeof(void *);
  frame_info_t *frame = lookup_return_address(rapid_global_stackmap_table, returnAddress);
#ifdef RAPID_GC_DEBUG_ENABLED
  fprintf(stderr, "GC begin return addr: 0x%016llx\n", returnAddress);
  fprintf(stderr, "GC begin frame info: 0x%016llx\n", (uint64_t)frame);
#endif

  uint64_t thisNurserySize = base->next_nursery_size;

  struct block_descr *nurseryOld = base->nurseryHead;
  base->nurseryHead = base->nurseryCur = gc_alloc_chain(thisNurserySize);
  base->used_nursery_size = 0;

  // precondition for GC
  assert(rapid_mem.generations[0].large_objects_scavenge == NULL);

  while (frame != NULL) {
    uint8_t *bp = *(uint8_t **)fp;
#ifdef RAPID_GC_DEBUG_ENABLED
    fprintf(stderr, "=====\nstack walk for return addr: 0x%016llx\n", returnAddress);
    fprintf(stderr, "    frame info: 0x%016llx\n", (uint64_t)frame);
    fprintf(stderr, "    stack ptr : 0x%016llx\n", (uint64_t)sp);
    fprintf(stderr, "    frame ptr : 0x%016llx\n", (uint64_t)sp - sizeof(void *));
    fprintf(stderr, "    fp val    : 0x%016llx\n", *(uint64_t *)fp);
    fprintf(stderr, "    frame size: 0x%016llx\n", (uint64_t)frame->frameSize);
#endif

    // Replace each derived pointer with its offset relative to base pointer.
    // The actual relocation will happen after all base pointers have been
    // relocated.
    for (int i = frame->numBaseSlots; i < frame->numSlots; ++i) {
      pointer_slot_t ptrSlot = frame->slots[i];
      assert(ptrSlot.kind >= 0 && "must be a derived pointer");

      pointer_slot_t baseSlot = frame->slots[DERIVED_PTRSLOT_GET_BASE_IDX(ptrSlot)];
      ObjPtr *baseStackSlot = get_stack_slot(sp, bp, baseSlot);
      ObjPtr *derivedStackSlot = get_stack_slot(sp, bp, ptrSlot);
      *derivedStackSlot = (void *)((uint64_t)(*derivedStackSlot) - (uint64_t)(*baseStackSlot));
    }

    for (int i = 0; i < frame->numBaseSlots; ++i) {
      pointer_slot_t ptrSlot = frame->slots[i];
#ifdef RAPID_GC_DEBUG_ENABLED
      fprintf(stderr, "    pointer slot %04d: kind=%02d offset + 0x%04x\n", i, ptrSlot.kind, ptrSlot.offset);
#endif
      assert((ptrSlot.kind == -1) || (ptrSlot.kind == -2));

      ObjPtr *stackSlot = get_stack_slot(sp, bp, ptrSlot);
#ifdef RAPID_GC_DEBUG_ENABLED
      dump_obj(*stackSlot);
#endif

      ObjPtr copied = evacuate(base, *stackSlot);
#ifdef RAPID_GC_DEBUG_ENABLED
      fprintf(stderr, "::evacuate: %p -> %p\n", (void *)*stackSlot, (void *)copied);
      dump_obj(copied);
#endif
      *stackSlot = copied;
    }

    // relocate all derived pointers
    for (int i = frame->numBaseSlots; i < frame->numSlots; ++i) {
      pointer_slot_t ptrSlot = frame->slots[i];
      assert(ptrSlot.kind >= 0 && "must be a derived pointer");

      pointer_slot_t baseSlot = frame->slots[DERIVED_PTRSLOT_GET_BASE_IDX(ptrSlot)];
      ObjPtr *baseStackSlot = get_stack_slot(sp, bp, baseSlot);
      ObjPtr *derivedStackSlot = get_stack_slot(sp, bp, ptrSlot);
      *derivedStackSlot = (void *)((uint64_t)(*derivedStackSlot) + (uint64_t)(*baseStackSlot));
#ifdef RAPID_GC_DEBUG_ENABLED
      fprintf(stderr, "derived ptr relocated\n");
#endif
    }

    sp = bp + sizeof(void *);
    returnAddress = *((uint64_t *) sp);
    fp = bp;
    sp += sizeof(void *);

    frame = lookup_return_address(rapid_global_stackmap_table, returnAddress);
#ifdef RAPID_GC_DEBUG_ENABLED
    fprintf(stderr, "\n  next ret: %p\n", (void *)returnAddress);
#endif
  }

  // preconditions for scavenge loop
  assert(base->nurseryScavenge == NULL);
  assert(rapid_mem.generations[0].live_large_objects == NULL);

  base->nurseryScavenge = base->nurseryHead;

  // Scavenge all freshly evacuated objects and large objects.
  // This may require multiple iterations, because a large object can in turn
  // reference a "new" (small) object which needs to be scavenged as well.
  bool work_done;
  do {
    work_done = false;
    work_done = work_done || cheney(base);
    work_done = work_done || scavenge_large_objects(base);
  } while(work_done);

  free_large_objects(base);
  gc_free_chain(nurseryOld);

  base->nurseryScavenge = NULL;

  if (base->used_nursery_size > (thisNurserySize / 2)) {
    base->next_nursery_size = thisNurserySize * 2;
  } else if (base->used_nursery_size < (thisNurserySize / 4)
      && (thisNurserySize >= 2 * INITIAL_NURSERY_SIZE)) {
    base->next_nursery_size = thisNurserySize / 2;
  }

#ifdef RAPID_GC_STATS_ENABLED
  base->gc_stats.gc_count += 1;

  base->gc_stats.allocated_bytes_total += oldNurseryUsed - current_heap_used;
  base->gc_stats.copied_bytes_total += current_heap_used;

  struct timespec end_time;
  clock_gettime(CLOCK_MONOTONIC, &end_time);
  uint64_t pause_nsec = (end_time.tv_sec - start_time.tv_sec) * 1000000000 + (end_time.tv_nsec - start_time.tv_nsec);
  base->gc_stats.pause_ns_total += pause_nsec;
  if (pause_nsec > base->gc_stats.pause_ns_max) {
    base->gc_stats.pause_ns_max = pause_nsec;
  }
#endif

  // check if current block group is too full to accomodate requested
  // `heap_alloc`
  if ((uint64_t)base->nurseryCur->free + base->heap_alloc > (uint64_t)block_group_get_end(base->nurseryCur)) {
    // advance to next block group in chain (if possible)
    if (base->nurseryCur->link) {
      base->nurseryCur = base->nurseryCur->link;
    } else {
      rapid_C_crash("nursery too small");
    }
  }

  update_heap_pointers(base);

  assert((uint64_t)base->nurseryNext + base->heap_alloc <= (uint64_t)base->nurseryEnd);
  base->heap_alloc = 0;
}

void
idris_rts_more_heap(Idris_TSO *base, uint8_t *sp) {
  assert(base->heap_alloc <= BLOCK_SIZE && "more heap does not support big objects");
  // fast path
  if (base->nurseryCur->link) {
    // switch to next block group
    base->nurseryCur = base->nurseryCur->link;

    update_heap_pointers(base);
    assert((uint64_t)base->nurseryNext + base->heap_alloc <= (uint64_t)base->nurseryEnd);
  } else {
    idris_rts_gc(base, sp);
  }
}

ObjPtr
idris_rts_alloc_large(Idris_TSO *base, uint8_t *sp, uint64_t size) {
  assert(size > BLOCK_SIZE && "small allocation requested via rts_alloc_large");
  return alloc_large_obj(0, size);
}

void
rapid_gc_finalize_stats(Idris_TSO *base) {
}

void rapid_gc_setup_heap(Idris_TSO *base) {
  base->nurseryHead = gc_alloc_chain(INITIAL_NURSERY_SIZE);
  base->nurseryCur = base->nurseryHead;
  update_heap_pointers(base);

  base->next_nursery_size = INITIAL_NURSERY_SIZE;
}

void rapid_gc_init() {
  if (rapid_gc_flavour == GC_FLAVOUR_STATEPOINT) {
    void *STACKMAP = get_stackmap();
    rapid_global_stackmap_table = generate_table((void *)STACKMAP, 0.5);
  }
  if (rapid_gc_flavour == GC_FLAVOUR_BDW) {
    GC_init();
  }
}

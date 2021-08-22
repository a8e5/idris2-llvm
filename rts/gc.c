#include <llvm-statepoint-tablegen.h>

#include "gc.h"
#include "object.h"
#include "rts.h"

#include <time.h>

extern void *get_stackmap();

#define GC_FLAVOUR_ZERO 1
#define GC_FLAVOUR_BDW 2
#define GC_FLAVOUR_STATEPOINT 3

extern uint32_t rapid_gc_flavour;

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

ObjPtr alloc_during_gc(Idris_TSO *base, uint32_t size) {
  uint8_t *p = base->nurseryNext;
  assert(((uint64_t)base->nurseryNext & 0x07) == 0);
  base->nurseryNext += aligned(size);
  assert((uint64_t)base->nurseryNext <= (uint64_t)base->nurseryEnd);
  return (ObjPtr)p;
}

ObjPtr copy(Idris_TSO *base, ObjPtr p) {
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

  // TODO: replace this with more efficient pointer tagging
  if ( !(((uint64_t)p >= (uint64_t)base->heap_aux) && ((uint64_t)p < (uint64_t)base->heap_aux_end)) ) {
    // object is not inside the old nursery, keep it where it is
    return p;
  }
  assert(((uint64_t)p >= (uint64_t)base->heap_aux) && ((uint64_t)p < (uint64_t)base->heap_aux_end) && "object is not inside old nursery");

  switch (OBJ_TYPE(p)) {
    case OBJ_TYPE_FWD_REF:
      return (ObjPtr)OBJ_GET_SLOT(p, 0);
    default:
      size = OBJ_TOTAL_SIZE(p);
      new = alloc_during_gc(base, size);
      memcpy(new, p, size);
#ifdef RAPID_GC_DEBUG_ENABLED
      fprintf(stderr, "-- object copied: %p -> %p (%u bytes)\n", (void *)p, (void *)new, size);
#endif
      if (size >= 16) {
        p->hdr = MAKE_HEADER(OBJ_TYPE_FWD_REF, 8);
        p->data = new;
      } else {
        p->hdr = 0x8000000000000000ull | (((uint64_t)new) >> 1);
      }
      return new;
  }
}

static void cheney(Idris_TSO *base) {
  uint8_t *scan = base->nurseryStart;

  while(scan < base->nurseryNext) {
    ObjPtr obj = (ObjPtr)scan;
#ifdef RAPID_GC_DEBUG_ENABLED
    fprintf(stderr, "================================== cheney obj start\n");
    dump_obj(obj);
    fprintf(stderr, "================================== cheney obj start\n");
#endif
    assert(!OBJ_IS_INLINE(obj));
    switch(OBJ_TYPE(obj)) {
      case OBJ_TYPE_CLOSURE:
        {
          int argCount = 0xffff & obj->hdr;
          for (int i = 0; i < argCount; ++i) {
            ObjPtr value = OBJ_GET_SLOT(obj, i + 1);
            ObjPtr argCopy = copy(base, value);
            OBJ_PUT_SLOT(obj, i + 1, argCopy);
          }
        }
        break;
      case OBJ_TYPE_CON_NO_ARGS:
        {
          int argCount = obj->hdr >> 40;
          for (int i = 0; i < argCount; ++i) {
            ObjPtr value = OBJ_GET_SLOT(obj, i);
            ObjPtr argCopy = copy(base, value);
            OBJ_PUT_SLOT(obj, i, argCopy);
          }
        }
        break;
      case OBJ_TYPE_IOREF:
        {
          ObjPtr argCopy = copy(base, OBJ_GET_SLOT(obj, 0));
          OBJ_PUT_SLOT(obj, 0, argCopy);
        }
        break;
      case OBJ_TYPE_IOARRAY:
        {
          int arraySize = OBJ_SIZE(obj);
          for (int i = 0; i < arraySize; ++i) {
            ObjPtr value = OBJ_GET_SLOT(obj, i);
            ObjPtr valCopy = copy(base, value);
            OBJ_PUT_SLOT(obj, i, valCopy);
          }
        }
        break;
      case OBJ_TYPE_FWD_REF:
        rapid_C_crash("illegal forward ref found");
        break;
      default:
        break;
    }
#ifdef RAPID_GC_DEBUG_ENABLED
    fprintf(stderr, "================================== cheney obj end\n");
    dump_obj(obj);
    fprintf(stderr, "================================== cheney obj end\n");
#endif
    scan += aligned(OBJ_TOTAL_SIZE(obj));
  }
}

void idris_rts_gc(Idris_TSO *base, uint8_t *sp) {
  uint8_t * orig_sp = sp;
#ifdef RAPID_GC_DEBUG_ENABLED
  fprintf(stderr, "GC called for %llu bytes, stack pointer: %p\n", base->heap_alloc, (void *)sp);
#endif

#ifdef RAPID_GC_STATS_ENABLED
  struct timespec start_time;
  clock_gettime(CLOCK_MONOTONIC, &start_time);
  uint64_t oldNurseryUsed = (uint64_t)base->nurseryNext - (uint64_t)base->nurseryStart - base->heap_alloc;
#endif

  uint64_t returnAddress = *((uint64_t *) sp);
  uint8_t *fp = sp - sizeof(void *);
  sp += sizeof(void *);
  frame_info_t *frame = lookup_return_address(rapid_global_stackmap_table, returnAddress);
#ifdef RAPID_GC_DEBUG_ENABLED
  fprintf(stderr, "GC begin return addr: 0x%016llx\n", returnAddress);
  fprintf(stderr, "GC begin frame info: 0x%016llx\n", (uint64_t)frame);
#endif

  uint64_t oldNurserySize = (uint64_t)base->nurseryEnd - (uint64_t)base->nurseryStart;
  uint64_t nextNurserySize = base->next_nursery_size;

  uint8_t *oldNursery = (uint8_t *)base->nurseryStart;
  uint8_t *newNursery = realloc(base->heap_aux, nextNurserySize);
  memset(newNursery, 0, nextNurserySize);
  base->nurseryStart = newNursery;
  base->nurseryNext = newNursery;
  base->nurseryEnd = (uint8_t *) ((uint64_t)newNursery + nextNurserySize);

#ifdef RAPID_GC_DEBUG_ENABLED
  fprintf(stderr, "nursery size: %llu -> %llu\n", oldNurserySize, nextNurserySize);
  fprintf(stderr, "old nursery at: %p - %p\n", (void *)oldNursery, (void *)((uint64_t)oldNursery + oldNurserySize));
  fprintf(stderr, "new nursery at: %p - %p\n", (void *)newNursery, (void *)base->nurseryEnd);
#endif

  base->heap_aux = oldNursery;
  base->heap_aux_end = oldNursery + oldNurserySize;

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

      ObjPtr copied = copy(base, *stackSlot);
#ifdef RAPID_GC_DEBUG_ENABLED
      fprintf(stderr, "::copy: %p -> %p\n", (void *)*stackSlot, (void *)copied);
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

  cheney(base);

  uint64_t nurseryUsed = (uint64_t)base->nurseryNext - (uint64_t)base->nurseryStart;
  if (nurseryUsed > (base->next_nursery_size >> 1)) {
    base->next_nursery_size = base->next_nursery_size * 2;
#ifdef RAPID_GC_DEBUG_ENABLED
    fprintf(stderr, "\nnursery will grow next GC: %llu -> %llu\n", nextNurserySize, base->next_nursery_size);
#endif
  } else if (nurseryUsed < (base->next_nursery_size >> 2)
      && (base->next_nursery_size >= 2*INITIAL_NURSERY_SIZE)) {
    base->next_nursery_size = base->next_nursery_size / 2;
#ifdef RAPID_GC_DEBUG_ENABLED
    fprintf(stderr, "\nnursery will shrink next GC: %llu(used) / %llu -> %llu\n", nurseryUsed, nextNurserySize, base->next_nursery_size);
#endif
  }

  if (rapid_global_config->debug_heap_write_poison) {
    memset(base->heap_aux, 0x5f, oldNurserySize);
  }

#ifdef RAPID_GC_DEBUG_ENABLED
  fprintf(stderr, "\n===============================================\n");
  fprintf(stderr, " GC FINISHED: %llu / %llu\n", (uint64_t)base->nurseryNext - (uint64_t)base->nurseryStart, nextNurserySize);
  fprintf(stderr, "===============================================\n");
  fprintf(stderr, " next object: %p\n", (void *)base->nurseryNext);
#endif

#ifdef RAPID_GC_STATS_ENABLED
  base->gc_stats.gc_count += 1;

  uint64_t current_heap_used = (uint64_t)base->nurseryNext - (uint64_t)base->nurseryStart;
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

  // There's probably a more efficient way to do this, but this should be rare
  // enough, that it shouldn't matter too much.
  if ((uint64_t)base->nurseryNext + base->heap_alloc > (uint64_t)base->nurseryEnd) {
#ifdef RAPID_GC_DEBUG_ENABLED
    fprintf(stderr, "WARNING: still not enough room for requested allocation of %llu bytes, recurse GC\n", base->heap_alloc);
#endif
    idris_rts_gc(base, orig_sp);
  }
  assert((uint64_t)base->nurseryNext + base->heap_alloc <= (uint64_t)base->nurseryEnd);
  base->heap_alloc = 0;
}

void
rapid_gc_finalize_stats(Idris_TSO *base) {
  uint64_t current_heap_used = (uint64_t)base->nurseryNext - (uint64_t)base->nurseryStart;
  base->gc_stats.allocated_bytes_total += current_heap_used;
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

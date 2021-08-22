#pragma once

#include "rts.h"

#include <stdint.h>

#undef RAPID_GC_DEBUG_ENABLED
/*#define RAPID_GC_DEBUG_ENABLED*/

#undef RAPID_GC_STATS_ENABLED
/*#define RAPID_GC_STATS_ENABLED*/

/*const size_t INITIAL_NURSERY_SIZE = 128;*/
static const size_t INITIAL_NURSERY_SIZE = 64 * 1024 * 1024;

void rapid_gc_init();
void rapid_gc_finalize_stats(Idris_TSO *base);
void *rapid_C_allocate(Idris_TSO *base, int32_t size);

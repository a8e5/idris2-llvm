#pragma once

#include "rts.h"

#include <stdint.h>

#undef RAPID_GC_DEBUG_ENABLED
/*#define RAPID_GC_DEBUG_ENABLED*/

#undef RAPID_GC_STATS_ENABLED
/*#define RAPID_GC_STATS_ENABLED*/

void rapid_gc_init();
void rapid_gc_finalize_stats(Idris_TSO *base);
void *rapid_C_allocate(Idris_TSO *base, int32_t size);

#include <stdint.h>

#include "../rts.h"

// Some stubs that usually would be defined by the target program:
uint32_t rapid_gc_flavour;
void *get_stackmap() {
  return 0;
}
int64_t idris_enter(Idris_TSO *baseTSO) {
  return 0;
}

int test_memory();

int main(int argc, char **argv) {
  int err = 0;
  err |= test_memory();
  return err;
}

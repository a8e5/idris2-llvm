#include "rts.h"

// This is just a wrapper around the `rts_main` function so that we can use a
// different `main` funciton for the tests.

int main(int argc, char **argv) {
  return rts_main(argc, argv);
}

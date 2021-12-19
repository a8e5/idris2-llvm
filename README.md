[![builds.sr.ht status](https://builds.sr.ht/~cypheon/rapid/commits/cmake.yml.svg?name=Debian%c2%a0%c2%a0%c2%a0%c2%a0%c2%a0)](https://builds.sr.ht/~cypheon/rapid/commits/cmake.yml?)
[![builds.sr.ht status](https://builds.sr.ht/~cypheon/rapid/commits/freebsd.yml.svg?name=FreeBSD%c2%a0%c2%a0%c2%a0)](https://builds.sr.ht/~cypheon/rapid/commits/freebsd.yml?)

# LLVM codegen and native runtime for Idris 2

This is a (work in progress) backend to generate native executables from Idris code,
using the LLVM compiler infrastructure. Code is generated via [LLVM
IR](https://llvm.org/docs/LangRef.html).

The source folder `rts/` contains a primitive runtime system with a relocating
semi-space garbage collector.

## Prerequisites

LLVM 11 needs to be installed and the binaries available in your `$PATH`. To
check if that is the case, try the following command:

    $ opt --version
    LLVM (http://llvm.org/):
      LLVM version 11.1.0

      Optimized build.
      Default target: x86_64-pc-linux-gnu
      Host CPU: znver2

GMP needs to be installed (`libgmp-dev` on Debian).

You need CMake (>= 3.18) and Ninja, building with `make` is not supported.

Make sure you have checked out the Git submodule as well:

    git submodule update --init --recursive

## Compilation

Prepare the submodule:

    make -C external/llvm-statepoint-utils dist/llvm-statepoint-tablegen.h unified

Compile:

    cmake -G Ninja .
    ninja

Run tests:

    ninja test

## Usage

The "support files" (precompiled platform code and runtime system, as well as a
custom LLVM pass) need to be available in a directory in the `$IDRIS2_DATA`
path. You can achieve that by running (from within the source dir):

    export IDRIS2_DATA=$PWD/support

(most useful during development) or by running `make install`.

    # compile the included "Hello world" example
    ./build/exec/rapidc --cg llvm -o hello samples/Hello.idr
    # run the compiled binary
    ./build/exec/hello

## Limitations

Differences from mainline Idris 2:

 * `Int` is 63-bits only and overflow is handled differently

Currently **not** implemented:

 * FFI (the FFI functions from prelude and base are hardcoded & handwritten
     specifically for the RTS)
 * Concurrency
 * Network primitives are stubbed and not functional
 * Some support functions are completely missing

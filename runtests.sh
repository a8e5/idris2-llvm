#!/usr/bin/env bash

rapidc=${rapidc:-$PWD/build/exec/rapidc}

tests=(chez001 chez002 chez003 chez004 chez005 chez006 chez007 chez008 chez009 chez010 chez011 chez012 chez013 chez014 chez014 chez015 chez016 chez017 chez018 reg001)

if [[ -n "$1" ]]; then
  if [[ "$1" = "--good" ]]; then
    tests=(
      chez001
      chez002
      chez003
      chez004
      chez005
      chez006
      chez007
      chez008
      chez009
      chez011
      chez012
      reg001
      rapid001
      rapid002
      rapid003
      rapid004
      rapid005
      rapid006
      rapid008
      rapid012
      rapid013
      rapid014
    )
  else
    read -r -a tests <<< "$@"
  fi
fi

if [[ -n "$RAPID_TEST_DEBUG" ]]; then
  debug_directive="--directive debug"
  echo "test will be compiled with DEBUG directive"
else
  debug_directive=""
  echo "test will be compiled without DEBUG directive"
fi

count_total=0
count_ok=0
count_failed=0
count_error=0

for test in ${tests[*]}; do
  ((count_total++))
  testdir="$PWD/tests/chez/$test"
  idr=$(echo ${testdir}/*.idr)
  if [[ ! -d "$testdir" ]]; then
    echo "dir does not exist: $testdir"
    ((count_error++))
    continue
  fi

  pushd "$testdir" >/dev/null
  rm -rf ./build output
  if $rapidc --cg llvm $debug_directive -p contrib -p network -o "$test" "$idr" >& "${testdir}/compile.log"; then
    "./build/exec/$test" > "${testdir}/output"
    #"$testdir/build/rapid/$(basename "$idr" .idr).native" > "${testdir}/output"

    expected="${testdir}/expected"
    expected_os="${testdir}/expected.$(uname)"
    if [[ -f "${expected_os}" ]]; then
      expected="${expected_os}"
    fi

    if git diff --quiet --no-index -- "${expected}" "${testdir}/output"; then
      echo "OK: $test"
      ((count_ok++))
    else
      git --no-pager diff --exit-code --no-index -- "${expected}" "${testdir}/output"
      ((count_failed++))
    fi
  else
    echo "COMPILE ERROR: $test"
    cat "${testdir}/compile.log"
    ((count_error++))
  fi
  popd >/dev/null
done

echo "Tests run: $count_total, ok: $count_ok, failed: $count_failed, error: $count_error"

if [[ "${count_total}" -eq "${count_ok}" ]]; then
  exit 0
else
  exit 1
fi

#!/usr/bin/env bash
set -euo pipefail

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
scope_script="$script_dir/ci-scope.sh"
zero_sha=0000000000000000000000000000000000000000

assert_scope() {
  local expected=$1
  shift

  local actual
  actual=$("$scope_script" "$@")
  if [[ "$actual" != "run-tests=$expected" ]]; then
    echo "Expected run-tests=$expected, got $actual" >&2
    exit 1
  fi
}

assert_scope false --changed-files $'README.md\ndocs/development.md\nLICENSE'
assert_scope true --changed-files 'src/Data/CSV/Conduit.hs'
assert_scope true --changed-files '.github/workflows/ci.yml'
assert_scope true "$zero_sha" "$(git rev-parse HEAD)"
assert_scope true "$(git rev-parse HEAD)" "$zero_sha"
assert_scope true "$(git rev-parse HEAD)" "$(git rev-parse HEAD)"

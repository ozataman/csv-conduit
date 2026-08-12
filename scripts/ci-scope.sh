#!/usr/bin/env bash
set -euo pipefail

classify_changed_files() {
  local changed_files=$1
  local changed_file

  # Run the full test matrices unless every changed path is a known
  # documentation or licence text path. New paths are deliberately code.
  if [[ -z "$changed_files" ]]; then
    echo "run-tests=true"
    return
  fi

  while IFS= read -r changed_file; do
    case "$changed_file" in
      README.md|README.*.md|docs/**|LICENSE|LICENSE.*|COPYING|COPYING.*|CHANGELOG.md|changelog.md)
        ;;
      *)
        echo "run-tests=true"
        return
        ;;
    esac
  done <<< "$changed_files"

  echo "run-tests=false"
}

if [[ "${1:-}" == "--changed-files" ]]; then
  if [[ $# -ne 2 ]]; then
    echo "usage: $0 --changed-files <paths>" >&2
    exit 2
  fi

  classify_changed_files "$2"
  exit 0
fi

if [[ $# -ne 2 ]]; then
  echo "usage: $0 <base-sha> <head-sha>" >&2
  exit 2
fi

base_sha=$1
head_sha=$2
empty_tree_sha=0000000000000000000000000000000000000000

if [[ "$base_sha" == "$empty_tree_sha" || "$head_sha" == "$empty_tree_sha" ]]; then
  echo "run-tests=true"
  exit 0
fi

classify_changed_files "$(git diff --name-only "$base_sha" "$head_sha")"

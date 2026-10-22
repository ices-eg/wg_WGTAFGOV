#!/usr/bin/env bash
# Clone the TAF template repositories listed in _data/templates.csv, or bring
# the clones up to date, for working on them alongside the docs.
#
# Usage, from anywhere: docs/_scripts/clone-templates.sh [folder]
#
# The clones go in folder, by default templates/ at the top of this
# repository, which is ignored by git, one folder for each repository, named
# after it. A clone that exists is pulled, only if it can be fast-forwarded,
# so that work in progress is never touched. When a template moves between
# ices-tools-dev and ices-tools-prod, its clone is pointed at the new
# repository. Templates with no repository are skipped.
#
# Needs the GitHub CLI, logged in (gh auth login).

set -u

root="$(cd "$(dirname "$0")/../.." && pwd)"
dest="${1:-$root/templates}"
csv="$root/docs/_data/templates.csv"

mkdir -p "$dest" || exit 1

# the repo column, owner/name, of the rows that have one
tail -n +2 "$csv" | cut -d, -f2 | grep -v '^$' | while read -r repo; do
  dir="$dest/${repo#*/}"
  if [ ! -d "$dir/.git" ]; then
    echo "== $repo: cloning"
    gh repo clone "$repo" "$dir" -- --quiet || echo "!! $repo: could not clone"
    continue
  fi
  url="https://github.com/$repo.git"
  if [ "$(git -C "$dir" remote get-url origin)" != "$url" ]; then
    echo "== $repo: moved, pointing origin at $url"
    git -C "$dir" remote set-url origin "$url"
  fi
  branch="$(git -C "$dir" branch --show-current)"
  echo "== $repo: pulling $branch"
  git -C "$dir" pull --ff-only --quiet || echo "!! $repo: not pulled, see git status in $dir"
done

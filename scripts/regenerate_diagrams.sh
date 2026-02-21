#!/usr/bin/env bash
# Regenerate PlantUML diagrams from puml/*.puml into out/puml/<basename>/.
# Requires PlantUML: install with `brew install plantuml` or set PLANTUML_JAR.
# Usage: from repo root, run:  scripts/regenerate_diagrams.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
PUML_DIR="$REPO_ROOT/puml"
OUT_BASE="$REPO_ROOT/out/puml"

cd "$REPO_ROOT"

# Resolve PlantUML: prefer `plantuml` in PATH, else java -jar PLANTUML_JAR
run_plantuml() {
  if command -v plantuml &>/dev/null; then
    plantuml "$@"
  elif [[ -n "${PLANTUML_JAR:-}" && -f "$PLANTUML_JAR" ]]; then
    java -jar "$PLANTUML_JAR" "$@"
  else
    echo "Error: PlantUML not found. Install with: brew install plantuml" >&2
    echo "  Or set PLANTUML_JAR to the path of plantuml.jar" >&2
    exit 1
  fi
}

if [[ ! -d "$PUML_DIR" ]]; then
  echo "Error: puml directory not found: $PUML_DIR" >&2
  exit 1
fi

mkdir -p "$OUT_BASE"
count=0

for f in "$PUML_DIR"/*.puml; do
  [[ -f "$f" ]] || continue
  base=$(basename "$f" .puml)
  out_dir="$OUT_BASE/$base"
  mkdir -p "$out_dir"
  echo "Generating: $f -> $out_dir/"
  run_plantuml -tpng -o "$out_dir" "$f"
  (( count++ )) || true
done

echo "Done. Generated diagrams for $count file(s) under $OUT_BASE/"

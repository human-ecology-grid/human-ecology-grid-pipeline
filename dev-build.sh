#!/bin/sh
SCRIPT_DIR="$(dirname "$(realpath "$0")")"
docker run --rm --tty -e ENV=DEVELOPMENT -v "$SCRIPT_DIR":/pipeline:ro -v .:/data "ghcr.io/human-ecology-grid/pipeline"

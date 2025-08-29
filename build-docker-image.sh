SCRIPT_DIR="$(dirname "$(realpath "$0")")"
docker build --platform linux/amd64,linux/arm64 -t "ghcr.io/human-ecology-grid/pipeline" "$SCRIPT_DIR"

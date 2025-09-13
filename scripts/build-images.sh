#!/usr/bin/env bash
set -euo pipefail

# Build all project Docker images and tag them with the last commit hash
# Usage:
#   scripts/build-images.sh            # uses last commit short hash
#   scripts/build-images.sh v1.2.3     # or specify an explicit tag
#
# Env:
#   IMAGE_PREFIX   Docker registry/user prefix (default: mariusgeorgescu)

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

TAG="${1:-}"
if [[ -z "${TAG}" ]]; then
  TAG="$(git -C "${ROOT_DIR}" rev-parse --short HEAD)"
fi

IMAGE_PREFIX="${IMAGE_PREFIX:-mariusgeorgescu}"

echo "Building images with tag: ${TAG}"
echo "Image prefix: ${IMAGE_PREFIX}"

# Determine buildx flags
PUSH="${PUSH:-false}"
if [[ "${PUSH}" == "true" ]]; then
  BUILDX_FLAGS=(--platform linux/amd64,linux/arm64 --push)
  echo "Publish mode: pushing multi-arch images"
else
  # Load only the host architecture into the local docker daemon
  ARCH="$(uname -m)"
  case "${ARCH}" in
    x86_64|amd64) NATIVE_PLATFORM="linux/amd64" ;;
    arm64|aarch64) NATIVE_PLATFORM="linux/arm64" ;;
    *) echo "Unsupported host arch: ${ARCH}" >&2; exit 1 ;;
  esac
  BUILDX_FLAGS=(--platform "${NATIVE_PLATFORM}" --load)
  echo "Local mode: building for ${NATIVE_PLATFORM} and loading locally"
fi

cd "${ROOT_DIR}"

# Interaction API
docker buildx build \
  "${BUILDX_FLAGS[@]}" \
  -f Dockerfile.interaction-api \
  -t "${IMAGE_PREFIX}/bjj-interaction-api:${TAG}" \
  .

# Query API
docker buildx build \
  "${BUILDX_FLAGS[@]}" \
  -f Dockerfile.query-api \
  -t "${IMAGE_PREFIX}/bjj-query-api:${TAG}" \
  .

# Chain Sync
docker buildx build \
  "${BUILDX_FLAGS[@]}" \
  -f Dockerfile.chainsync \
  -t "${IMAGE_PREFIX}/bjj-chainsync:${TAG}" \
  .

echo "\nBuilt images:"
echo "  ${IMAGE_PREFIX}/bjj-interaction-api:${TAG}"
echo "  ${IMAGE_PREFIX}/bjj-query-api:${TAG}"
echo "  ${IMAGE_PREFIX}/bjj-chainsync:${TAG}"



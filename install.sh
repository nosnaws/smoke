#!/usr/bin/env bash
set -euo pipefail

REPO="nosnaws/smoke"
INSTALL_DIR="${INSTALL_DIR:-/usr/local/bin}"
TMP=""

detect_platform() {
  local os arch
  os="$(uname -s)"
  arch="$(uname -m)"

  case "$os" in
    Linux)  os="linux" ;;
    Darwin) os="darwin" ;;
    *)
      echo "Error: unsupported OS: $os" >&2
      exit 1
      ;;
  esac

  case "$arch" in
    x86_64|amd64)  arch="amd64" ;;
    arm64|aarch64) arch="arm64" ;;
    *)
      echo "Error: unsupported architecture: $arch" >&2
      exit 1
      ;;
  esac

  echo "${os}-${arch}"
}

cleanup() { [ -n "$TMP" ] && rm -f "$TMP"; }
trap cleanup EXIT

main() {
  local platform asset_name

  platform="$(detect_platform)"
  asset_name="smoke-${platform}"

  echo "Detected platform: ${platform}"
  echo "Downloading latest release of smoke..."

  TMP="$(mktemp)"

  if ! gh release download --repo "$REPO" --pattern "$asset_name" --output "$TMP" 2>/dev/null; then
    local url
    url="https://github.com/${REPO}/releases/latest/download/${asset_name}"
    if ! curl -fsSL -o "$TMP" "$url"; then
      echo "Error: failed to download ${asset_name}" >&2
      echo "No release found for platform: ${platform}" >&2
      exit 1
    fi
  fi

  chmod +x "$TMP"

  if [ -w "$INSTALL_DIR" ]; then
    mv "$TMP" "${INSTALL_DIR}/smoke"
  else
    echo "Installing to ${INSTALL_DIR} (requires sudo)..."
    sudo mv "$TMP" "${INSTALL_DIR}/smoke"
  fi

  echo "smoke installed to ${INSTALL_DIR}/smoke"
}

main

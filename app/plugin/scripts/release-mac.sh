#!/bin/bash
# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (c) 2026 Sam Aaron
#
# Makes the Mac release of "Sonic Pi.vst3": a universal build, signed with a
# Developer ID, notarised by Apple, stapled, and zipped for download.
#
#   scripts/release-mac.sh [--adhoc] [--skip-build]
#
# --adhoc       sign with no identity and skip notarising: for trying the
#               script out. The result will not open on another Mac.
# --skip-build  use the bundle already in dist/ (it must be universal)
#
# Signing and notarising use what the app's release does (app/mac-release-common.sh):
# the Developer ID Application identity in the login keychain, or
# SONIC_PI_RELEASE_IDENTITY, and the notarytool keychain profile
# "sonic-pi-notary", or SONIC_PI_NOTARY_PROFILE. That profile is made once with
#
#   xcrun notarytool store-credentials sonic-pi-notary \
#       --apple-id <apple id> --team-id <team id> --password <app-specific password>
#
# The zip is written to dist/. Nothing is uploaded anywhere but to Apple's
# notary service.
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
ADHOC=0; BUILD=1
for arg in "$@"; do
    case "$arg" in
        --adhoc) ADHOC=1 ;;
        --skip-build) BUILD=0 ;;
        *) echo "unknown argument: $arg" >&2; exit 2 ;;
    esac
done
[ "$ADHOC" = 1 ] && export SONIC_PI_RELEASE_ADHOC=1
source "$HERE/../mac-release-common.sh"
require_macos
require_cmd xcrun
require_cmd ditto

NAME="Sonic Pi"
BUNDLE="$HERE/dist/$NAME.vst3"
BINARY="$BUNDLE/Contents/MacOS/$NAME"
VERSION="$(sed -n 's/^version = "\(.*\)"$/\1/p' "$HERE/Cargo.toml" | head -n 1)"
[ -n "$VERSION" ] || die "no version in $HERE/Cargo.toml"
ZIP="$HERE/dist/Sonic-Pi-VST3-$VERSION-mac.zip"

if [ "$BUILD" = 1 ]; then
    log_step "build, for arm64 and x86_64"
    "$HERE/scripts/build.sh" --universal
fi
[ -f "$BINARY" ] || die "no bundle at $BUNDLE: run without --skip-build"
ARCHS="$(lipo -archs "$BINARY")"
case "$ARCHS" in
    *arm64*x86_64*|*x86_64*arm64*) ;;
    *) die "the bundle is for $ARCHS only: a release is universal (scripts/build.sh --universal)" ;;
esac

# The bundle holds one Mach-O, its binary: anything else that is one would
# have to be signed first, so say if that ever changes.
OTHERS="$(list_macho_files "$BUNDLE" | grep -v -F "$BINARY" || true)"
[ -z "$OTHERS" ] || die "the bundle holds other binaries, which this script does not sign: $OTHERS"

IDENTITY="$(release_identity)"
log_step "sign as: $IDENTITY"
if [ "$IDENTITY" = "-" ]; then
    log_warn "ad-hoc: this bundle cannot be notarised and will not open on another Mac"
    codesign --force --sign - "$BUNDLE"
else
    # the hardened runtime and Apple's timestamp, both of which notarising asks for
    codesign --force --options runtime --timestamp --sign "$IDENTITY" "$BUNDLE"
fi
codesign --verify --deep --strict "$BUNDLE" || die "the signature does not verify"
log_ok "signed"

rm -f "$ZIP"
if [ "$IDENTITY" != "-" ]; then
    PROFILE="$RELEASE_NOTARY_PROFILE"
    log_step "notarise (keychain profile: $PROFILE)"
    UPLOAD="$(mktemp -d)/Sonic-Pi-VST3.zip"
    ditto -c -k --keepParent "$BUNDLE" "$UPLOAD"
    xcrun notarytool submit "$UPLOAD" --keychain-profile "$PROFILE" --wait \
        || die "Apple did not accept it: xcrun notarytool log <id> --keychain-profile $PROFILE says why"
    rm -f "$UPLOAD"
    # the ticket goes on the bundle itself, so it opens on a Mac that is offline
    xcrun stapler staple "$BUNDLE" || die "the ticket could not be stapled"
    xcrun stapler validate "$BUNDLE" || die "the stapled ticket does not validate"
    log_ok "notarised and stapled"
fi

log_step "package"
ditto -c -k --keepParent "$BUNDLE" "$ZIP"
log_ok "$ZIP ($(du -h "$ZIP" | cut -f1), $ARCHS, version $VERSION)"
echo "To install: unzip, and move \"$NAME.vst3\" into ~/Library/Audio/Plug-Ins/VST3"

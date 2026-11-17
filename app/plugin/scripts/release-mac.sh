#!/bin/bash
# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (c) 2026 Sam Aaron
#
# Makes the Mac release of "Sonic Pi.vst3": a universal build, signed with a
# Developer ID, notarised by Apple and stapled, then shipped two ways:
#
#   - an installer package (.pkg) that puts it in /Library/Audio/Plug-Ins/VST3,
#     signed with a Developer ID Installer identity, notarised and stapled
#   - a zip of the bundle, for installing by hand
#
#   scripts/release-mac.sh [--adhoc] [--skip-build]
#
# --adhoc       sign with no identity, leave the package unsigned and skip
#               notarising: for trying the script out. The results will not
#               open on another Mac.
# --skip-build  use the bundle already in dist/ (it must be universal)
#
# Signing and notarising use what the app's release does (app/mac-release-common.sh):
# the Developer ID Application and Developer ID Installer identities in the
# login keychain, or SONIC_PI_RELEASE_IDENTITY and
# SONIC_PI_RELEASE_INSTALLER_IDENTITY, and the notarytool keychain profile
# "sonic-pi-notary", or SONIC_PI_NOTARY_PROFILE. That profile is made once with
#
#   xcrun notarytool store-credentials sonic-pi-notary \
#       --apple-id <apple id> --team-id <team id> --password <app-specific password>
#
# The package and the zip are written to dist/. Nothing is uploaded anywhere
# but to Apple's notary service.
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
require_cmd pkgbuild
require_cmd productbuild

NAME="Sonic Pi"
BUNDLE="$HERE/dist/$NAME.vst3"
BINARY="$BUNDLE/Contents/MacOS/$NAME"
VERSION="$(sed -n 's/^version = "\(.*\)"$/\1/p' "$HERE/Cargo.toml" | head -n 1)"
[ -n "$VERSION" ] || die "no version in $HERE/Cargo.toml"
# the oldest macOS the plugin is built for, which the package will not install below
MIN_OS="$(sed -n 's/^MACOSX_DEPLOYMENT_TARGET = "\(.*\)"$/\1/p' "$HERE/.cargo/config.toml")"
[ -n "$MIN_OS" ] || die "no MACOSX_DEPLOYMENT_TARGET in $HERE/.cargo/config.toml"
ZIP="$HERE/dist/Sonic-Pi-VST3-$VERSION-mac.zip"
PKG="$HERE/dist/Sonic-Pi-VST3-$VERSION-mac.pkg"
PKG_ID="net.sonic-pi.plugin.vst3"
PKG_TITLE="Sonic Pi VST3"
INSTALL_DIR="/Library/Audio/Plug-Ins/VST3"

# Both identities are looked up before the build, so a missing one stops the script before minutes of compiling.
IDENTITY="$(release_identity)"
INSTALLER_IDENTITY="$(release_installer_identity)"

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

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

# Submits a file to Apple's notary service and waits for its answer.
notarise() {
    xcrun notarytool submit "$1" --keychain-profile "$RELEASE_NOTARY_PROFILE" --wait \
        || die "Apple did not accept $(basename "$1"): xcrun notarytool log <id> --keychain-profile $RELEASE_NOTARY_PROFILE says why"
}

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

rm -f "$ZIP" "$PKG"
if [ "$IDENTITY" != "-" ]; then
    log_step "notarise the bundle (keychain profile: $RELEASE_NOTARY_PROFILE)"
    ditto -c -k --keepParent "$BUNDLE" "$WORK/Sonic-Pi-VST3.zip"
    notarise "$WORK/Sonic-Pi-VST3.zip"
    # the ticket goes on the bundle itself, so it opens on a Mac that is offline, however it was installed
    xcrun stapler staple "$BUNDLE" || die "the ticket could not be stapled"
    xcrun stapler validate "$BUNDLE" || die "the stapled ticket does not validate"
    log_ok "bundle notarised and stapled"
fi

log_step "zip"
ditto -c -k --keepParent "$BUNDLE" "$ZIP"
log_ok "$ZIP ($(du -h "$ZIP" | cut -f1))"

log_step "installer package"
mkdir -p "$WORK/root" "$WORK/packages"
ditto "$BUNDLE" "$WORK/root/$NAME.vst3"
# Installer puts a relocatable bundle wherever it finds another copy with the same identifier (a build left in
# someone's dist/, say) instead of in the install location. Whether pkgbuild marks a bundle relocatable by default
# has changed between macOS versions, so it is set to no here.
pkgbuild --analyze --root "$WORK/root" "$WORK/components.plist" > /dev/null
plutil -replace 0.BundleIsRelocatable -bool NO "$WORK/components.plist"
pkgbuild --root "$WORK/root" --component-plist "$WORK/components.plist" --identifier "$PKG_ID" \
    --version "$VERSION" --install-location "$INSTALL_DIR" "$WORK/packages/vst3.pkg" > /dev/null

# What the package will do, checked before it is signed: the plugin's binary in its payload, the install location,
# and no relocation (a relocatable bundle is listed in PackageInfo as <relocate><bundle id=.../></relocate>; with
# none the element is empty, <relocate/>). The payload's list goes to a file first: grep -q on a pipe stops reading at its match, pkgutil
# is then killed by the closed pipe, and pipefail counts the check as failed.
pkgutil --payload-files "$WORK/packages/vst3.pkg" > "$WORK/payload.txt"
grep -qxF "./$NAME.vst3/Contents/MacOS/$NAME" "$WORK/payload.txt" \
    || die "the package's payload has no ./$NAME.vst3/Contents/MacOS/$NAME"
pkgutil --expand "$WORK/packages/vst3.pkg" "$WORK/expanded"
grep -qF "install-location=\"$INSTALL_DIR\"" "$WORK/expanded/PackageInfo" \
    || die "the package does not install to $INSTALL_DIR"
! grep -q "<relocate>" "$WORK/expanded/PackageInfo" \
    || die "the package would move the plugin to another copy of it instead of $INSTALL_DIR"

# hostArchitectures names both, so an Apple silicon Mac does not ask for Rosetta to run the installer.
cat > "$WORK/distribution.xml" <<EOF
<?xml version="1.0" encoding="utf-8"?>
<installer-gui-script minSpecVersion="2">
    <title>$PKG_TITLE</title>
    <options customize="never" require-scripts="false" hostArchitectures="arm64,x86_64"/>
    <domains enable_localSystem="true" enable_currentUserHome="false" enable_anywhere="false"/>
    <volume-check>
        <allowed-os-versions><os-version min="$MIN_OS"/></allowed-os-versions>
    </volume-check>
    <choices-outline><line choice="$PKG_ID"/></choices-outline>
    <choice id="$PKG_ID" visible="false"><pkg-ref id="$PKG_ID"/></choice>
    <pkg-ref id="$PKG_ID" version="$VERSION" onConclusion="none">vst3.pkg</pkg-ref>
</installer-gui-script>
EOF
if [ -z "$INSTALLER_IDENTITY" ]; then
    log_warn "ad-hoc: the package is not signed, and macOS will refuse to open it on another Mac"
    productbuild --distribution "$WORK/distribution.xml" --package-path "$WORK/packages" "$PKG" > /dev/null
else
    log_step "sign the package as: $INSTALLER_IDENTITY"
    productbuild --distribution "$WORK/distribution.xml" --package-path "$WORK/packages" \
        --sign "$INSTALLER_IDENTITY" "$PKG" > /dev/null
    pkgutil --check-signature "$PKG" > /dev/null || die "the package's signature does not verify"
    log_step "notarise the package"
    notarise "$PKG"
    xcrun stapler staple "$PKG" || die "the ticket could not be stapled to the package"
    xcrun stapler validate "$PKG" || die "the package's stapled ticket does not validate"
    spctl --assess --type install "$PKG" || die "Gatekeeper refuses the package"
    log_ok "package signed, notarised and stapled"
fi
log_ok "$PKG ($(du -h "$PKG" | cut -f1), $ARCHS, version $VERSION, macOS $MIN_OS or later)"

echo "To install: open the .pkg, or unzip the .zip and move \"$NAME.vst3\" into $INSTALL_DIR"

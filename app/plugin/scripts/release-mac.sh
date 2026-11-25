#!/bin/bash
# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (c) 2026 Sam Aaron
#
# Makes the Mac release of the plugin, "Sonic Pi.vst3" and "Sonic Pi.component"
# (the Audio Unit): a universal build, each bundle signed with a Developer ID,
# notarised by Apple and stapled, then shipped two ways:
#
#   - an installer package (.pkg) that puts the VST3 in
#     /Library/Audio/Plug-Ins/VST3 and the Audio Unit in
#     /Library/Audio/Plug-Ins/Components (either can be unticked under
#     Customise), signed with a Developer ID Installer identity, notarised and
#     stapled
#   - a zip of the two bundles, for installing by hand
#
#   scripts/release-mac.sh [--adhoc] [--skip-build]
#
# --adhoc       sign with no identity, leave the package unsigned and skip
#               notarising: for trying the script out. The results will not
#               open on another Mac.
# --skip-build  use the bundles already in dist/ (they must be universal)
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
# The two formats: each bundle's extension, the folder a host looks for it in, its package's identifier and its
# name in the installer's list of what to install.
FORMATS="vst3 component"
install_dir() { case "$1" in vst3) echo "/Library/Audio/Plug-Ins/VST3" ;; component) echo "/Library/Audio/Plug-Ins/Components" ;; esac; }
package_id()  { case "$1" in vst3) echo "net.sonic-pi.plugin.vst3" ;; component) echo "net.sonic-pi.plugin.au" ;; esac; }
format_name() { case "$1" in vst3) echo "VST3" ;; component) echo "Audio Unit" ;; esac; }
VERSION="$(sed -n 's/^version = "\(.*\)"$/\1/p' "$HERE/Cargo.toml" | head -n 1)"
[ -n "$VERSION" ] || die "no version in $HERE/Cargo.toml"
# the oldest macOS the plugin is built for, which the package will not install below
MIN_OS="$(sed -n 's/^MACOSX_DEPLOYMENT_TARGET = "\(.*\)"$/\1/p' "$HERE/.cargo/config.toml")"
[ -n "$MIN_OS" ] || die "no MACOSX_DEPLOYMENT_TARGET in $HERE/.cargo/config.toml"
ZIP="$HERE/dist/Sonic-Pi-Plugin-$VERSION-mac.zip"
PKG="$HERE/dist/Sonic-Pi-Plugin-$VERSION-mac.pkg"
PKG_TITLE="Sonic Pi Plugin"

# Both identities are looked up before the build, so a missing one stops the script before minutes of compiling.
IDENTITY="$(release_identity)"
INSTALLER_IDENTITY="$(release_installer_identity)"

if [ "$BUILD" = 1 ]; then
    log_step "build, for arm64 and x86_64"
    "$HERE/scripts/build.sh" --universal
fi
for format in $FORMATS; do
    BUNDLE="$HERE/dist/$NAME.$format"
    BINARY="$BUNDLE/Contents/MacOS/$NAME"
    [ -f "$BINARY" ] || die "no bundle at $BUNDLE: run without --skip-build"
    ARCHS="$(lipo -archs "$BINARY")"
    case "$ARCHS" in
        *arm64*x86_64*|*x86_64*arm64*) ;;
        *) die "$BUNDLE is for $ARCHS only: a release is universal (scripts/build.sh --universal)" ;;
    esac
    # A bundle holds one Mach-O, its binary: anything else that is one would
    # have to be signed first, so say if that ever changes.
    OTHERS="$(list_macho_files "$BUNDLE" | grep -v -F "$BINARY" || true)"
    [ -z "$OTHERS" ] || die "$BUNDLE holds other binaries, which this script does not sign: $OTHERS"
done

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

# Submits a file to Apple's notary service and waits for its answer.
notarise() {
    xcrun notarytool submit "$1" --keychain-profile "$RELEASE_NOTARY_PROFILE" --wait \
        || die "Apple did not accept $(basename "$1"): xcrun notarytool log <id> --keychain-profile $RELEASE_NOTARY_PROFILE says why"
}

log_step "sign as: $IDENTITY"
[ "$IDENTITY" != "-" ] || log_warn "ad-hoc: these bundles cannot be notarised and will not open on another Mac"
for format in $FORMATS; do
    BUNDLE="$HERE/dist/$NAME.$format"
    if [ "$IDENTITY" = "-" ]; then
        codesign --force --sign - "$BUNDLE"
    else
        # the hardened runtime and Apple's timestamp, both of which notarising asks for
        codesign --force --options runtime --timestamp --sign "$IDENTITY" "$BUNDLE"
    fi
    codesign --verify --deep --strict "$BUNDLE" || die "the signature of $BUNDLE does not verify"
done
log_ok "signed"

# Both bundles side by side in one folder: what Apple is sent, and what the zip holds.
rm -f "$ZIP" "$PKG"
mkdir -p "$WORK/bundles"
if [ "$IDENTITY" != "-" ]; then
    log_step "notarise the bundles (keychain profile: $RELEASE_NOTARY_PROFILE)"
    for format in $FORMATS; do ditto "$HERE/dist/$NAME.$format" "$WORK/bundles/$NAME.$format"; done
    ditto -c -k "$WORK/bundles" "$WORK/Sonic-Pi-Plugin.zip"
    notarise "$WORK/Sonic-Pi-Plugin.zip"
    # the ticket goes on each bundle itself, so it opens on a Mac that is offline, however it was installed
    for format in $FORMATS; do
        xcrun stapler staple "$HERE/dist/$NAME.$format" || die "the ticket could not be stapled to $NAME.$format"
        xcrun stapler validate "$HERE/dist/$NAME.$format" || die "the stapled ticket of $NAME.$format does not validate"
    done
    log_ok "bundles notarised and stapled"
    rm -rf "$WORK/bundles"
    mkdir -p "$WORK/bundles"
fi

log_step "zip"
for format in $FORMATS; do ditto "$HERE/dist/$NAME.$format" "$WORK/bundles/$NAME.$format"; done
ditto -c -k "$WORK/bundles" "$ZIP"
log_ok "$ZIP ($(du -h "$ZIP" | cut -f1))"

log_step "installer package"
mkdir -p "$WORK/packages"
CHOICES=""; OUTLINE=""
for format in $FORMATS; do
    BUNDLE="$HERE/dist/$NAME.$format"
    ID="$(package_id "$format")"
    DIR="$(install_dir "$format")"
    ROOT="$WORK/root-$format"
    mkdir -p "$ROOT"
    ditto "$BUNDLE" "$ROOT/$NAME.$format"
    # Installer puts a relocatable bundle wherever it finds another copy with the same identifier (a build left in
    # someone's dist/, say) instead of in the install location. Whether pkgbuild marks a bundle relocatable by
    # default has changed between macOS versions, so it is set to no here.
    pkgbuild --analyze --root "$ROOT" "$WORK/components-$format.plist" > /dev/null
    plutil -replace 0.BundleIsRelocatable -bool NO "$WORK/components-$format.plist"
    pkgbuild --root "$ROOT" --component-plist "$WORK/components-$format.plist" --identifier "$ID" \
        --version "$VERSION" --install-location "$DIR" "$WORK/packages/$format.pkg" > /dev/null

    # What the package will do, checked before it is signed: the plugin's binary in its payload, the install
    # location, and no relocation (a relocatable bundle is listed in PackageInfo as
    # <relocate><bundle id=.../></relocate>; with none the element is empty, <relocate/>). The payload's list goes to
    # a file first: grep -q on a pipe stops reading at its match, pkgutil is then killed by the closed pipe, and
    # pipefail counts the check as failed.
    pkgutil --payload-files "$WORK/packages/$format.pkg" > "$WORK/payload-$format.txt"
    grep -qxF "./$NAME.$format/Contents/MacOS/$NAME" "$WORK/payload-$format.txt" \
        || die "the $format package's payload has no ./$NAME.$format/Contents/MacOS/$NAME"
    pkgutil --expand "$WORK/packages/$format.pkg" "$WORK/expanded-$format"
    grep -qF "install-location=\"$DIR\"" "$WORK/expanded-$format/PackageInfo" \
        || die "the $format package does not install to $DIR"
    ! grep -q "<relocate>" "$WORK/expanded-$format/PackageInfo" \
        || die "the $format package would move the plugin to another copy of it instead of $DIR"

    OUTLINE="$OUTLINE<line choice=\"$ID\"/>"
    CHOICES="$CHOICES
    <choice id=\"$ID\" title=\"$(format_name "$format")\" description=\"$NAME.$format, in $DIR\" start_selected=\"true\"><pkg-ref id=\"$ID\"/></choice>
    <pkg-ref id=\"$ID\" version=\"$VERSION\" onConclusion=\"none\">$format.pkg</pkg-ref>"
done

# Both formats are installed unless one is unticked under Customise. hostArchitectures names both, so an Apple
# silicon Mac does not ask for Rosetta to run the installer.
cat > "$WORK/distribution.xml" <<EOF
<?xml version="1.0" encoding="utf-8"?>
<installer-gui-script minSpecVersion="2">
    <title>$PKG_TITLE</title>
    <options customize="allow" require-scripts="false" hostArchitectures="arm64,x86_64"/>
    <domains enable_localSystem="true" enable_currentUserHome="false" enable_anywhere="false"/>
    <volume-check>
        <allowed-os-versions><os-version min="$MIN_OS"/></allowed-os-versions>
    </volume-check>
    <choices-outline>$OUTLINE</choices-outline>$CHOICES
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

echo "To install: open the .pkg, or unzip the .zip and move \"$NAME.vst3\" into $(install_dir vst3) and \"$NAME.component\" into $(install_dir component)"

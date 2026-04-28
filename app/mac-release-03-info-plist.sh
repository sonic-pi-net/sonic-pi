#!/bin/bash
# Stage 3 — finalise Contents/Info.plist for release.
#
# CMake / macdeployqt write a basic Info.plist during the GUI build. This
# stage adds the keys macOS expects in a notarised, App Store-conformant
# bundle: deployment-target floor, application category, copyright, mic
# usage description, light-only chrome (Sonic Pi handles its own theming).
#
# Versions are pulled live from the repo's VERSION file so we never ship a
# bundle whose CFBundleVersion lies about the source tree.
#
# IMPORTANT: any plutil edit invalidates the bundle's existing signature.
# That's fine here — codesign in stage 04 re-signs from scratch.

set -euo pipefail
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source "${SCRIPT_DIR}/mac-release-common.sh"

[ -d "${RELEASE_APP}" ] || die "Bundle not staged yet — run 00-stage first"

plist="${RELEASE_APP}/Contents/Info.plist"
[ -f "$plist" ] || die "Info.plist missing at ${plist}"

VERSION_FULL="$(release_version)"        # e.g. 5.0.0-dev
VERSION_SHORT="$(release_version_short)" # e.g. 5.0.0
COPYRIGHT="© 2013–$(date +%Y) Sam Aaron and the Sonic Pi contributors. Sonic Pi is licensed under the MIT License."

log_step "patch Info.plist"
log_info "  CFBundleVersion              ${VERSION_FULL}"
log_info "  CFBundleShortVersionString   ${VERSION_SHORT}"
log_info "  LSMinimumSystemVersion       13"
log_info "  LSApplicationCategoryType    public.app-category.music"
log_info "  NSMicrophoneUsageDescription \"To code live audio\""
log_info "  NSRequiresAquaSystemAppearance  false  (chrome follows macOS dark/light)"
log_info "  NSHumanReadableCopyright     ${COPYRIGHT}"

# -replace inserts the key if missing or replaces it if present.
plutil -replace CFBundleIdentifier            -string  "${RELEASE_BUNDLE_ID}"           "$plist"
plutil -replace CFBundleName                  -string  "${RELEASE_APP_NAME}"            "$plist"
plutil -replace CFBundleDisplayName           -string  "${RELEASE_APP_NAME}"            "$plist"
plutil -replace CFBundleVersion               -string  "${VERSION_FULL}"                "$plist"
plutil -replace CFBundleShortVersionString    -string  "${VERSION_SHORT}"               "$plist"
plutil -replace CFBundleLongVersionString     -string  "${VERSION_FULL}"                "$plist"
plutil -replace CFBundleGetInfoString         -string  "${RELEASE_APP_NAME} ${VERSION_FULL} — A code-based music creation and performance tool" "$plist"
plutil -replace CFBundleInfoDictionaryVersion -string  "6.0"                            "$plist"
plutil -replace CFBundlePackageType           -string  "APPL"                           "$plist"
plutil -replace CFBundleSignature             -string  "????"                           "$plist"
plutil -replace CFBundleDevelopmentRegion     -string  "en"                             "$plist"
plutil -replace CFBundleExecutable            -string  "${RELEASE_APP_NAME}"            "$plist"
plutil -replace CFBundleIconFile              -string  "app.icns"                       "$plist"

plutil -replace LSMinimumSystemVersion        -string  "13"                             "$plist"
plutil -replace LSApplicationCategoryType     -string  "public.app-category.music"      "$plist"

plutil -replace NSHighResolutionCapable       -bool    true                             "$plist"
plutil -replace NSRequiresAquaSystemAppearance -bool   false                            "$plist"
plutil -replace NSMicrophoneUsageDescription  -string  "To code live audio"             "$plist"
plutil -replace NSHumanReadableCopyright      -string  "${COPYRIGHT}"                   "$plist"

# Drop tool-cruft if it leaks in from anywhere upstream.
plutil -remove OWARPublisherName       "$plist" 2>/dev/null || true
plutil -remove OWARPublisherURL        "$plist" 2>/dev/null || true
plutil -remove OWARPublisherSupportURL "$plist" 2>/dev/null || true

# Validate the result is well-formed XML.
plutil -lint "$plist" >/dev/null

log_ok "stage 03 done"

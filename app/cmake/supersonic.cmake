# The audio engine, as Sonic Pi builds and names it.
#
# Included from external/ (which builds the engine and its plugin bridge) and
# from api/ (which reads the engine's shared-memory segment). The two must
# agree on every line here: the segment's name is the product name plus the
# port, the segment's layout comes from the engine tree's headers, and a
# reader built from any other spelling of either opens nothing — silently,
# because a missing segment is also what a still-booting engine looks like.
#
# The engine is clockwork hosting scsynth, and that is what the supersonic
# submodule now is: github.com/samaaron/supersonic, whose own `clockwork`
# submodule carries the substrate. clockwork provides device IO, MIDI, OSC,
# the transport clock and plugin hosting; scsynth is the guest; they meet at
# dsp_api.h. Nothing beside it is built — the clockwork-supersonic checkout
# this used to point at was the same tree before it was published.
set(SUPERSONIC_SOURCE_DIR "${CMAKE_CURRENT_LIST_DIR}/../external/supersonic")

# Published in OS-facing binary metadata (Windows version resource, macOS
# embedded Info.plist) so consent prompts name Sonic Pi; also the segment name.
set(SUPERSONIC_PRODUCT_NAME "Sonic Pi - SuperSonic")

# Plugins run in a second process, the plugin bridge, which the engine spawns
# from beside its own binary and finds by name. Named for the same reason the
# engine is: it is what Activity Monitor shows when a plugin misbehaves, and
# what an entitlement is granted to.
if(WIN32)
    set(SUPERSONIC_BRIDGE_NAME "Sonic Pi - Plugins")
elseif(APPLE)
    set(SUPERSONIC_BRIDGE_NAME "Sonic Pi - Plugins")
else()
    set(SUPERSONIC_BRIDGE_NAME "sonic-pi-plugins")
endif()

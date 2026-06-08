# This CMakeLists builds all the external libraries required for a Tau.

# This is provided for the convinience of devs just wanting to work on Tau
# directly without building the full GUI.

# For the full Sonic Pi build, use the scripts in /app to not just build the
# extensions referenced here but also other extensions required by the GUI.

cmake_minimum_required(VERSION 3.13)

message(STATUS "External NIF Builder")

project(ExternalsBuilder)

include(GNUInstallDirs)
include(ExternalProject)

set(CMAKE_OSX_DEPLOYMENT_TARGET '10.15')

# MIDI moved to SuperSonic (Rust/midir; see app/server/ruby midi_api.rb): the
# sp_midi NIF has been removed and there are no external NIFs left to build here.
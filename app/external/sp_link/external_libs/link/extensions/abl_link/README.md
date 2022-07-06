# abl_link

Plain C 11 wrapper for Ableton Link.

# Building and Running abl_link Examples

The `abl_link` library and the `link_hut` example application are built as part of the main CMake project in this repository.

# Integrating abl_link into CMake-based Projects

If you are using CMake, you can integrate `abl_link` by including both, the `Ableton::Link` and the `abl_link` configs:

```cmake
include($PATH_TO_LINK/AbletonLinkConfig.cmake)
include($PATH_TO_LINK/extensions/abl_link/abl_link.cmake)
target_link_libraries($YOUR_TARGET abl_link)
```

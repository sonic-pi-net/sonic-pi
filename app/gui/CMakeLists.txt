option(BUILD_IMGUI_INTERFACE "Build the ImGui-based graphical interface" ON)

message(STATUS "ImGui Interface: ${BUILD_IMGUI_INTERFACE}")

add_subdirectory(qt)

if(BUILD_IMGUI_INTERFACE)
  add_subdirectory(imgui)
endif()

idf_component_register(SRCS main.cpp)

if(NOT DEFINED LINK_ESP_TASK_CORE_ID)
  set(LINK_ESP_TASK_CORE_ID tskNO_AFFINITY)
endif()
target_compile_definitions(${COMPONENT_LIB} PRIVATE LINK_ESP_TASK_CORE_ID=${LINK_ESP_TASK_CORE_ID})

target_compile_options(${COMPONENT_LIB} PRIVATE -fexceptions)

include(../../../AbletonLinkConfig.cmake)
target_link_libraries(${COMPONENT_TARGET} Ableton::Link)

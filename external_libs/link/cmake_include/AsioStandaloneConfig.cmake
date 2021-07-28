add_library(AsioStandalone::AsioStandalone IMPORTED INTERFACE)

set_property(TARGET AsioStandalone::AsioStandalone APPEND PROPERTY
  INTERFACE_INCLUDE_DIRECTORIES
  ${CMAKE_CURRENT_LIST_DIR}/../modules/asio-standalone/asio/include
)

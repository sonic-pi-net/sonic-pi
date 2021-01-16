project(SonicPiAPI VERSION 0.0.0.1)

set(API_ROOT ${CMAKE_CURRENT_LIST_DIR})

set(API_SRC ${API_ROOT}/src/api.cpp)

add_library(${PROJECT_NAME} STATIC ${API_SRC})
add_library(SonicPi::${PROJECT_NAME} ALIAS ${PROJECT_NAME})

target_include_directories(${PROJECT_NAME}
    PUBLIC
        ${API_ROOT}/include
    )

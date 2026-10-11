cmake_minimum_required(VERSION 3.22)
project(obehy-motis-shapes LANGUAGES C CXX ASM)

if (MSVC)
    add_definitions(-D_CRT_SECURE_NO_WARNINGS=1)
    if (NOT DEFINED CMAKE_MSVC_RUNTIME_LIBRARY)
        set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")
    endif ()
endif ()

include(cmake/pkg.cmake)

# The osr commit is part of every cache key and manifest; read it from the lock file so it can
# never drift from what was built.
file(STRINGS "${CMAKE_CURRENT_SOURCE_DIR}/.pkg.lock" obehy-osr-lock REGEX "^osr [0-9a-f]+$")
string(REGEX REPLACE "^osr " "" obehy-osr-commit "${obehy-osr-lock}")
if (NOT obehy-osr-commit MATCHES "^[0-9a-f]+$")
    message(FATAL_ERROR "osr commit not found in .pkg.lock")
endif ()

if (MSVC)
    set(obehy-shapes-options /W4 /bigobj /utf-8)
else ()
    set(obehy-shapes-options -Wall -Wextra)
endif ()

add_library(obehy-shapes
        src/cache.cc
        src/csv.cc
        src/geometry.cc
        src/patterns.cc
        src/route.cc
        src/run.cc)
target_include_directories(obehy-shapes PUBLIC include)
target_compile_features(obehy-shapes PUBLIC cxx_std_23)
target_compile_options(obehy-shapes PRIVATE ${obehy-shapes-options})
target_compile_definitions(obehy-shapes PRIVATE OBEHY_OSR_COMMIT="${obehy-osr-commit}")
target_link_libraries(obehy-shapes PUBLIC osr fmt)

add_executable(obehy-motis-shapes src/main.cc)
target_compile_options(obehy-motis-shapes PRIVATE ${obehy-shapes-options})
target_link_libraries(obehy-motis-shapes obehy-shapes)

enable_testing()
add_executable(obehy-motis-shapes-test test/shapes_test.cc)
target_compile_options(obehy-motis-shapes-test PRIVATE ${obehy-shapes-options})
target_compile_definitions(obehy-motis-shapes-test PRIVATE
        OBEHY_TEST_DIR="${CMAKE_CURRENT_SOURCE_DIR}/test")
target_link_libraries(obehy-motis-shapes-test obehy-shapes gtest gtest_main)
add_test(NAME obehy-motis-shapes-test COMMAND obehy-motis-shapes-test)

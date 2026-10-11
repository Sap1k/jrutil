# Adapted from osr's cmake/pkg.cmake: the pkg tool is pinned to one release and always clones
# over HTTPS, so builds need no GitHub SSH key.
set(obehy-pkg-version "v0.23")

find_program(pkg-bin pkg HINTS /opt/pkg)
if (pkg-bin)
    message(STATUS "found pkg ${pkg-bin}")
else ()
    if (${CMAKE_SYSTEM_NAME} STREQUAL "Windows")
        set(pkg-bin "${CMAKE_BINARY_DIR}/dl/pkg.exe")
        set(pkg-url "pkg.exe")
    elseif (${CMAKE_SYSTEM_NAME} STREQUAL "Linux" AND ${CMAKE_HOST_SYSTEM_PROCESSOR} STREQUAL "aarch64")
        set(pkg-bin "${CMAKE_BINARY_DIR}/dl/pkg")
        set(pkg-url "pkg-linux-arm64")
    elseif (${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
        set(pkg-bin "${CMAKE_BINARY_DIR}/dl/pkg")
        set(pkg-url "pkg")
    else ()
        message(FATAL_ERROR "no pkg binary for ${CMAKE_SYSTEM_NAME}")
    endif ()
    if (NOT EXISTS ${pkg-bin})
        set(pkg-download "https://github.com/motis-project/pkg/releases/download/${obehy-pkg-version}/${pkg-url}")
        message(STATUS "Downloading ${pkg-download}")
        file(DOWNLOAD ${pkg-download} ${pkg-bin} STATUS pkg-download-status)
        list(GET pkg-download-status 0 pkg-download-code)
        if (NOT pkg-download-code EQUAL 0)
            message(FATAL_ERROR "pkg download failed: ${pkg-download-status}")
        endif ()
        if (UNIX)
            execute_process(COMMAND chmod +x ${pkg-bin})
        endif ()
    endif ()
endif ()

message(STATUS "${pkg-bin} -l -h -f")
execute_process(
        COMMAND ${pkg-bin} -l -h -f
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
        RESULT_VARIABLE pkg-result
)
if (NOT pkg-result EQUAL 0)
    message(FATAL_ERROR "pkg failed: ${pkg-result}")
endif ()

if (IS_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/deps")
    add_subdirectory(deps)
endif ()

set_property(
        DIRECTORY
        APPEND
        PROPERTY CMAKE_CONFIGURE_DEPENDS
        "${CMAKE_CURRENT_SOURCE_DIR}/.pkg"
)

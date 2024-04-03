option(BUILD_SHARED_LIBS "Build using shared libraries" ON)

set(CMAKE_BUILD_TYPE "DEBUG" CACHE STRING "cmake build type")
message(STATUS "CMAKE_BUILD_TYPE: ${CMAKE_BUILD_TYPE}")
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

include(GNUInstallDirs)

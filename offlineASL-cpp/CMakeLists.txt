cmake_minimum_required(VERSION 3.20)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

project(offlineasl)

set(ASLP_LIFTER_DIR "${CMAKE_CURRENT_SOURCE_DIR}/subprojects/aslp-lifter")
set(ASLP_LIFTER_LLVM_DIR "${CMAKE_CURRENT_SOURCE_DIR}/subprojects/aslp-lifter-instantiate")

## dependencies ##

if (NOT LLVM_FOUND)
  find_package(LLVM REQUIRED CONFIG)
else()
  message(STATUS "... LLVM already found")
endif()
message(STATUS "Found LLVM: " ${LLVM_DIR} ", tools: " ${LLVM_TOOLS_BINARY_DIR})
get_target_property(LLVM_CONFIG llvm-config LOCATION)

message(STATUS "llvm-config: ${LLVM_CONFIG}")

### aslp-lifter ###
# provides templates to instantiate the lifter with generic targets

add_library(aslp-lifter INTERFACE)
target_include_directories(aslp-lifter INTERFACE "${ASLP_LIFTER_DIR}/include")

target_compile_options(aslp-lifter INTERFACE -Wno-unused-parameter -Wno-error=unused-variable -Wno-error=unused-but-set-variable)
target_compile_options(aslp-lifter INTERFACE -g -O0)

### aslp-lifter-llvm ###
# instantiates the aslp-lifter with a LLVM target

file(GLOB_RECURSE GEN_INSTANTIATE "${ASLP_LIFTER_DIR}/src/*.cpp")
add_library(aslp-lifter-llvm SHARED ${GEN_INSTANTIATE})

target_include_directories(aslp-lifter-llvm PUBLIC "${ASLP_LIFTER_LLVM_DIR}/include")

execute_process(COMMAND "${LLVM_CONFIG}" --libs support core irreader bitwriter
                COMMAND_ERROR_IS_FATAL ANY
                OUTPUT_VARIABLE llvm_libs OUTPUT_STRIP_TRAILING_WHITESPACE)
message(STATUS "llvm_libs: ${llvm_libs}")
separate_arguments(llvm_libs UNIX_COMMAND "${llvm_libs}")
target_link_options(aslp-lifter-llvm PUBLIC "${llvm_libs}")
target_link_directories(aslp-lifter-llvm PUBLIC ${LLVM_LIBRARY_DIR})
target_include_directories(aslp-lifter-llvm PUBLIC ${LLVM_INCLUDE_DIRS})

target_link_libraries(aslp-lifter-llvm PUBLIC aslp-lifter)

# define macros for the explicit instantiation
target_compile_options(aslp-lifter-llvm PRIVATE "-includeaslp/llvm_lifter_traits.hpp")
target_compile_definitions(aslp-lifter-llvm PRIVATE "ASLP_LIFTER_INSTANTIATE=llvm_lifter_traits")


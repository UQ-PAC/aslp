cmake_minimum_required(VERSION 3.20)

project(offlineasl)

option(BUILD_SHARED_LIBS "Build using shared libraries" ON)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

if (NOT LLVM_FOUND)
  find_package(LLVM REQUIRED CONFIG)
else()
  message(STATUS "... LLVM already found")
endif()
message(STATUS "Found LLVM: " ${LLVM_DIR} ", tools: " ${LLVM_TOOLS_BINARY_DIR})
get_target_property(LLVM_CONFIG llvm-config LOCATION)

message(STATUS "llvm-config: ${LLVM_CONFIG}")

### aslp-lifter-gen ###
# provides templates to instantiate the lifter with generic targets

add_library(aslp-lifter-gen INTERFACE)
target_include_directories(aslp-lifter-gen INTERFACE "${CMAKE_CURRENT_SOURCE_DIR}/.")

target_compile_options(aslp-lifter-gen INTERFACE -Wno-error=unused-variable -Wno-error=unused-function -Wno-error=unused-but-set-variable)
IF(CMAKE_BUILD_TYPE MATCHES DEBUG)
  target_compile_options(aslp-lifter-gen INTERFACE -g -Ofast -fsanitize=address)
  target_link_options(aslp-lifter-gen INTERFACE -g -Ofast -fsanitize=address)
ENDIF()

### aslp-lifter-llvm ###
# instantiates the aslp-lifter with a LLVM target

file(GLOB GEN_INSTANTIATE aslp-lifter-instantiate/*.cpp)
add_library(aslp-lifter-llvm ${GEN_INSTANTIATE})

execute_process(COMMAND "${LLVM_CONFIG}" --libs support core irreader bitwriter
                COMMAND_ERROR_IS_FATAL ANY
                OUTPUT_VARIABLE llvm_libs OUTPUT_STRIP_TRAILING_WHITESPACE)
message(STATUS "llvm_libs: ${llvm_libs}")
separate_arguments(llvm_libs UNIX_COMMAND "${llvm_libs}")
target_link_options(aslp-lifter-llvm PUBLIC "${llvm_libs}")
target_link_directories(aslp-lifter-llvm PUBLIC ${LLVM_LIBRARY_DIR})
target_include_directories(aslp-lifter-llvm PUBLIC ${LLVM_INCLUDE_DIRS})

target_link_libraries(aslp-lifter-llvm PUBLIC aslp-lifter-gen)

target_compile_options(aslp-lifter-llvm PRIVATE "-includellvm_interface.hpp")
target_compile_definitions(aslp-lifter-llvm PRIVATE "ASLP_LIFTER_INSTANTIATE=llvm_lifter_traits")


### aslp-lifter-bin ###
# a simple test application
add_executable(aslp-lifter-bin main.cpp)
target_link_libraries(aslp-lifter-bin PUBLIC aslp-lifter-llvm)

install(TARGETS aslp-lifter-bin DESTINATION bin)

set_target_properties(
  aslp-lifter-bin
  PROPERTIES
  OUTPUT_NAME offlineasl
  RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin
  LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/lib
  ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/lib
)

file(GLOB GENERATED aslp-lifter-gen/*.hpp aslp-lifter-instantiate/*.cpp)
set_directory_properties(PROPERTIES ADDITIONAL_MAKE_CLEAN_FILES "${GENERATED}")


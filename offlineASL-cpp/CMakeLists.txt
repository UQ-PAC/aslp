cmake_minimum_required(VERSION 3.20)

project(offlineasl)

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

add_library(offlineasl-lib builder.cpp)
target_include_directories(offlineasl-lib PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/.")

target_compile_options(offlineasl-lib PRIVATE -Wno-error=unused-variable -Wno-error=unused-function -Wno-error=unused-but-set-variable)
IF(CMAKE_BUILD_TYPE MATCHES DEBUG)
  target_compile_options(offlineasl-lib PRIVATE -g -O0 -fsanitize=address)
  target_link_options(offlineasl-lib PRIVATE -g -O0 -fsanitize=address)    
ENDIF()

execute_process(COMMAND "${LLVM_CONFIG}" --libs support core irreader bitwriter
                COMMAND_ERROR_IS_FATAL ANY
                OUTPUT_VARIABLE llvm_libs OUTPUT_STRIP_TRAILING_WHITESPACE)
message(STATUS "llvm_libs: ${llvm_libs}")
separate_arguments(llvm_libs UNIX_COMMAND "${llvm_libs}")
# target_link_libraries(offlineasl-lib PRIVATE "${lib_files}")
target_link_options(offlineasl-lib PUBLIC "${llvm_libs}")
target_link_directories(offlineasl-lib PUBLIC ${LLVM_LIBRARY_DIR})
target_include_directories(offlineasl-lib PUBLIC ${LLVM_INCLUDE_DIRS})

add_executable(offlineasl-bin main.cpp)
target_link_libraries(offlineasl-bin PRIVATE offlineasl-lib)

install(TARGETS offlineasl-bin DESTINATION bin)

set_target_properties(
  offlineasl-bin
  PROPERTIES
  OUTPUT_NAME offlineasl
  RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin
  LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/lib
  ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/lib
)


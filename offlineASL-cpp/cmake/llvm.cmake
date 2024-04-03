if (NOT LLVM_FOUND)
  find_package(LLVM REQUIRED CONFIG)
else()
  message(STATUS "... LLVM already found")
endif()
message(STATUS "Found LLVM: " ${LLVM_DIR} ", tools: " ${LLVM_TOOLS_BINARY_DIR})
get_target_property(LLVM_CONFIG llvm-config LOCATION)

message(STATUS "llvm-config: ${LLVM_CONFIG}")


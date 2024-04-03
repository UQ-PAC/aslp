cmake_minimum_required(VERSION 3.20)

project(offlineasl)

include(../cmake/prelude.cmake)

### aslp-lifter-gen ###
# provides templates to instantiate the lifter with generic targets

add_library(aslp-lifter-gen INTERFACE)
target_include_directories(aslp-lifter-gen INTERFACE include)

target_compile_options(aslp-lifter-gen INTERFACE -Wno-error=unused-variable -Wno-error=unused-function -Wno-error=unused-but-set-variable)
IF(CMAKE_BUILD_TYPE MATCHES DEBUG)
  target_compile_options(aslp-lifter-gen INTERFACE -g -Ofast -fsanitize=address)
  target_link_options(aslp-lifter-gen INTERFACE -g -Ofast -fsanitize=address)
ENDIF()

install(
  DIRECTORY include/
  DESTINATION ${CMAKE_INSTALL_FULL_INCLUDEDIR}
  FILES_MATCHING PATTERN "*.hpp")
configure_file(aslp-lifter-gen.pc.in aslp-lifter-gen.pc @ONLY) 
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/aslp-lifter-gen.pc
  DESTINATION ${CMAKE_INSTALL_FULL_DATAROOTDIR}/pkgconfig)

add_executable(dummy src/dummy.cpp)
target_link_libraries(dummy PRIVATE aslp-lifter-gen)

file(GLOB GENERATED include/aslp/generated/*.hpp)
set_directory_properties(PROPERTIES ADDITIONAL_MAKE_CLEAN_FILES "${GENERATED}")

### aslp-lifter-llvm ###
# instantiates the aslp-lifter with a LLVM target

# file(GLOB GEN_INSTANTIATE aslp-lifter-instantiate/*.cpp)
# add_library(aslp-lifter-llvm ${GEN_INSTANTIATE})
#
# execute_process(COMMAND "${LLVM_CONFIG}" --libs support core irreader bitwriter
#                 COMMAND_ERROR_IS_FATAL ANY
#                 OUTPUT_VARIABLE llvm_libs OUTPUT_STRIP_TRAILING_WHITESPACE)
# message(STATUS "llvm_libs: ${llvm_libs}")
# separate_arguments(llvm_libs UNIX_COMMAND "${llvm_libs}")
# target_link_options(aslp-lifter-llvm BEFORE PUBLIC "${llvm_libs}")
# target_link_directories(aslp-lifter-llvm PUBLIC ${LLVM_LIBRARY_DIR})
# target_include_directories(aslp-lifter-llvm PUBLIC ${LLVM_INCLUDE_DIRS})
#
# target_link_libraries(aslp-lifter-llvm PUBLIC aslp-lifter-gen)
#
# target_compile_options(aslp-lifter-llvm PRIVATE "-includellvm_lifter_traits.hpp")
# target_compile_definitions(aslp-lifter-llvm PRIVATE "ASLP_LIFTER_INSTANTIATE=llvm_lifter_traits")
#
#
# ### aslp-lifter-bin ###
# # a simple test application
# add_executable(aslp-lifter-bin main.cpp)
# target_link_libraries(aslp-lifter-bin PUBLIC aslp-lifter-llvm)
# IF(CMAKE_BUILD_TYPE MATCHES DEBUG)
#   target_compile_options(aslp-lifter-bin PRIVATE -g -Ofast -fsanitize=address)
#   target_link_options(aslp-lifter-bin PRIVATE -g -Ofast -fsanitize=address)
# ENDIF()
#
# install(TARGETS aslp-lifter-bin DESTINATION bin)
#
# set_target_properties(
#   aslp-lifter-bin
#   PROPERTIES
#   OUTPUT_NAME offlineasl
#   RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin
#   LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/lib
#   ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/lib
# )
#
# file(GLOB GENERATED aslp-lifter-gen/*.hpp aslp-lifter-instantiate/*.cpp)
# set_directory_properties(PROPERTIES ADDITIONAL_MAKE_CLEAN_FILES "${GENERATED}")
#

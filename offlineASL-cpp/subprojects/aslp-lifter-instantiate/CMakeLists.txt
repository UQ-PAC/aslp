cmake_minimum_required(VERSION 3.20)

project(offlineasl)

include(../cmake/prelude.cmake)
include(../cmake/llvm.cmake)

### aslp-lifter-llvm ###
# instantiates the aslp-lifter with a LLVM target

pkg_check_modules(aslp-lifter-gen REQUIRED aslp-lifter-gen)

file(GLOB GEN_INSTANTIATE src/generated/*.cpp)
add_library(aslp-lifter-llvm ${GEN_INSTANTIATE})
target_include_directories(aslp-lifter-llvm PUBLIC include)

target_link_libraries(aslp-lifter-llvm ${aslp-lifter-gen_LIBRARIES})
target_include_directories(aslp-lifter-llvm PUBLIC ${aslp-lifter-gen_INCLUDE_DIRS})
target_compile_options(aslp-lifter-llvm PUBLIC ${aslp-lifter-gen_CFLAGS_OTHER})

target_compile_options(aslp-lifter-llvm PRIVATE "-includellvm_lifter_traits.hpp")
target_compile_definitions(aslp-lifter-llvm PRIVATE "ASLP_LIFTER_INSTANTIATE=llvm_lifter_traits")

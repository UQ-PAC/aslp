project('aslp-lifter-instantiate', 'cpp',
  version : '0.1',
  default_options : [ 'warning_level=3', 'cpp_std=c++20' ])

llvm_dep = dependency('llvm')

gen_name = 'aslp-lifter-gen'
gen_proj = subproject(gen_name, version: meson.project_version())
gen_dep = gen_proj.get_variable('dep')
# gen_pc = gen_proj.get_variable('pc')

incdir = 'include'
install_subdir(incdir, install_dir : get_option('includedir'), strip_directory : true)

find = run_command('find', 'src', '-name', '*.cpp', check : true)
srcfiles = find.stdout().strip().split('\n')[0] # XXX for meson testing

lib = library(
  'aslp-lifter-instantiate',
  srcfiles,
  include_directories : incdir,
  dependencies: [ llvm_dep, gen_dep ],
  cpp_args: [
    '-Wno-unused-parameter',
    '-includeaslp/llvm_lifter_traits.hpp',
    '-DASLP_LIFTER_INSTANTIATE=llvm_lifter_traits'],
  install : true)

dep = declare_dependency(link_with: lib, dependencies: gen_dep)

pkg = import('pkgconfig')
pkg.generate(lib, subdirs: 'aslp', requires: gen_name)

project('aslp-lifter-llvm', 'cpp',
  version : '0.1',
  default_options : [ 'warning_level=3', 'cpp_std=c++20' ])

version = meson.project_version()
llvm_dep = dependency('llvm')
empty_dep = dependency('', required: false)

lifter_dep = dependency('aslp-lifter', version: version, fallback: ['aslp-lifter', 'gen_dep'])
instantiate_dep = dependency('aslp-lifter-instantiate', version: version, fallback: ['aslp-lifter', 'instantiate_dep'])
instantiate_srcdir = instantiate_dep.get_variable(pkgconfig: 'srcdir', default_value: '')

if instantiate_srcdir != ''
  srcfiles = run_command('find', instantiate_srcdir, '-name', '*.cpp', check : true).stdout().strip().split('\n')
  instantiate_dep = declare_dependency(
    sources: srcfiles,
    dependencies: instantiate_dep,
  )
endif

incdir = 'include'
install_subdir(incdir, install_dir : get_option('includedir'), strip_directory : true)

lib = library(
  meson.project_name(),
  [],
  include_directories : incdir,
  dependencies: [ llvm_dep, instantiate_dep ],
  cpp_args: [
    '-includeaslp/llvm_lifter_traits.hpp',
    '-DASLP_LIFTER_INSTANTIATE=llvm_lifter_traits'],
  cpp_pch: 'pch/cpp_pch.hpp',
  install : true)

dep = declare_dependency(link_with: lib, include_directories: incdir)

pkg = import('pkgconfig')
pkg.generate(lib, requires: 'aslp-lifter=' + version, variables: {'llvm_version': llvm_dep.version()})

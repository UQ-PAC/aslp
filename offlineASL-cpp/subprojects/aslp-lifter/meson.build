project('aslp-lifter', 'cpp',
  version : '0.1',
  default_options : ['warning_level=3', 'cpp_std=c++20'])

incdir = 'include'
srcdir = 'src'
install_srcdir = get_option('datadir') / 'aslp'

install_subdir(incdir, install_dir : get_option('includedir'), strip_directory : true)
gen_dep = declare_dependency(include_directories : incdir)

find = run_command('find', srcdir, '-name', '*.cpp', check : true)
srcfiles = files(find.stdout().strip().split('\n'))

install_subdir(srcdir, install_dir: install_srcdir, strip_directory : true)
instantiate_dep = declare_dependency(
  sources: srcfiles,
  dependencies: gen_dep,
  compile_args: ['-Wno-unused-parameter'])

# needed to generate compile_commands.json when building this subproject standalone
dummy_lib = library('dummy', 'dummy.cpp', dependencies: gen_dep, install: false)

pkg = import('pkgconfig')
pkg.generate(
  name: meson.project_name(),
  description: 'offline aslp lifter library (template headers and explicit instantiation sources)',
  variables: {'srcdir': '${prefix}' / install_srcdir})

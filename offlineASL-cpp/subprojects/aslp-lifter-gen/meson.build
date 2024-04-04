project('aslp-lifter-gen', 'cpp',
  version : '0.1',
  default_options : ['warning_level=3', 'cpp_std=c++20'])

incdir = 'include'

install_subdir(incdir, install_dir : get_option('includedir'), strip_directory : true)
dep = declare_dependency(include_directories : incdir)

pkg = import('pkgconfig')
pkg.generate(
  name: meson.project_name(),
  description: 'offline asl lifter library (template headers)',
  subdirs: 'aslp')

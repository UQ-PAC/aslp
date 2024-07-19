project('offlineasl', 'cpp',
  version : '0.1',
  default_options : [
    'warning_level=3',
    'cpp_std=c++20',
    'buildtype=debug',
    # 'optimization=1', # slow and increases file size?
    # 'b_lto=true', # too slow
    # 'b_lto_mode=thin',
  ]
)

# debug(get_option('buildtype'))

# TODO properly use dependency() to support builds against system-installed libraries
subproject('aslp-lifter', required: get_option('aslp-lifter'))
subproject('aslp-lifter-llvm', required: get_option('aslp-lifter-llvm'))
subproject('aslp-lifter-demo', required: get_option('aslp-lifter-demo'))

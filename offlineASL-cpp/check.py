#!/usr/bin/env python3

"""
Configures a meson build for syntax-checking only.
To use, run `meson compile -C build` after this script.

This script works by manually editing the meson-generated
ninja files. This is obviously a bit hacky, but meson does
not yet properly support -fsyntax-only.
"""

import os
import shutil
import subprocess
from pathlib import Path

clangpp = shutil.which('clang++')
true = shutil.which('true')
assert clangpp
assert true

def main():
  assert clangpp and true

  os.chdir(Path(__file__).parent)

  os.environ['CXX'] = clangpp

  subprocess.check_call('meson setup --reconfigure build'.split())

  with open('build/build.ninja', 'rb') as f:
    build = f.readlines()

  bstr = lambda x: x.encode('utf-8')

  # replacement rules, keyed by sentinel line, and values are
  # replacements to perform on the NEXT line.
  repls = {
    b'rule cpp_COMPILER\n': (b' $ARGS ', b' $ARGS -fsyntax-only '),
    b'rule cpp_LINKER\n': (bstr(clangpp), bstr(true)),
    b'rule cpp_LINKER_RSP\n': (bstr(clangpp), bstr(true)),
  }

  with open('build/build.ninja', 'wb') as f:
    repl = None
    for l in build:
      if repl is not None:
        assert repl[0] in l, f'replacement failed {repl} in {l!r}'
        f.write(l.replace(repl[0], repl[1]))
        repl = None
      else:
        repl = repls.get(l) # must match exactly
        f.write(l)

if __name__ == '__main__':
  main()


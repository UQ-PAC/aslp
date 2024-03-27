#!/usr/bin/env python3
# vim: set ts=2 sts=2 sw=2 et :

# a simple linter to detect and remove trailing whitespace in OCaml files.

import os
import sys
import shlex
import argparse
import subprocess

from pathlib import Path

def trace(args):
  print('$', *map(shlex.quote, args), file=sys.stderr)
  return args

def concat(xss):
  return [x for xs in xss for x in xs]

def lint(args) -> int:
  cmd = (
    ['git', 'grep', '-n', '--break', '--no-recursive']
  )
  cmd_files = (
    # ['-l', '-e', r'', '--']
    ['-e', r'\s$', '--']
    # non-directories should be added literally
    + [f for f in args.files if not f.is_dir()]
    # directories are searched for files matching the includes.
    # NOTE: include pattern might not begin with * so we must add an extra * to recurse
    + [f'{d}/{i}' for d in args.files for i in args.include if d.is_dir()]
    + [f'{d}/*/{i}' for d in args.files for i in args.include if d.is_dir() if not i.startswith('*')]
  )

  if args.fix:
    cmd += ['-l', '--null']
    ret = subprocess.run(trace(cmd + cmd_files), stdout=subprocess.PIPE)
    files = [x.decode('utf-8') for x in ret.stdout.split(b'\0') if x]
    print('files to fix:', files)
    print()

    if files:
      return subprocess.call(trace(['sed', '-i', r's/\s\+$//g'] + files))
    return 0
  else:
    if args.list:
      cmd += ['-l']
    ret = subprocess.run(trace(cmd + cmd_files))
    if ret.returncode == 0:
      print("\nfound trailing whitespace in above files!")
      return 1
    else:
      return 0


def main():
  argp = argparse.ArgumentParser(formatter_class=argparse.ArgumentDefaultsHelpFormatter)
  g = argp.add_mutually_exclusive_group()
  g.add_argument('--check', action='store_true', default=True, help='check for trailing whitespace and print lines')
  g.add_argument('--fix', action='store_true', help='remove trailing whitespace in-place')
  g.add_argument('--list', action='store_true', help='list files with trailing whitespace')
  argp.add_argument('--include', nargs='*', default=['dune', '*.ml', '*.mli', '*.cpp', '*.hpp', '*.scala'], help='grep file globs to include')
  argp.add_argument('--untracked', action='store_true', help='include untracked files')
  # argp.add_argument('--exclude-dir', nargs='*', default=['_build', 'build', 'target', 'out', '.git'], help='grep directory globs to include')
  argp.add_argument('files', nargs='*', default=[Path('.')], type=Path, help='files or directories to include')

  args = argp.parse_intermixed_args()
  args.check = not (args.fix or args.list)

  sys.exit(lint(args))

if __name__ == '__main__':
  main()


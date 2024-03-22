#!/usr/bin/env python3
# vim: set ts=2 sts=2 sw=2 et :

# a simple linter to detect and remove trailing whitespace in OCaml files.

import sys
import shlex
import argparse
import subprocess

def trace(args):
  print('$', *map(shlex.quote, args), file=sys.stderr)
  return args

def lint(args) -> int:
  cmd = (
    ['grep', '-Rn', r'\s$']
    + [f'--include={f}' for f in args.include]
    + [f'--exclude-dir={f}' for f in args.exclude_dir]
  )

  if not args.fix:
    ret = subprocess.run(trace(cmd + args.files))
    if ret.returncode == 0:
      print("\nfound trailing whitespace in above files!")
      return 1
    else:
      return 0
  else:
    cmd += ['-l', '--null']
    ret = subprocess.run(trace(cmd + args.files), stdout=subprocess.PIPE)
    files = [x.decode('utf-8') for x in ret.stdout.split(b'\0') if x]
    print('files to fix:', files)
    print()

    if files:
      return subprocess.call(trace(['sed', '-i', r's/\s\+$//g'] + files))
    return 0

def main():
  argp = argparse.ArgumentParser(formatter_class=argparse.ArgumentDefaultsHelpFormatter)
  argp.add_argument('--fix', action='store_true', help='remove trailing whitespace in-place')
  argp.add_argument('--include', nargs='*', default=['*.ml', '*.mli', '*.cpp', '*.hpp'], help='grep file globs to include')
  argp.add_argument('--exclude-dir', nargs='*', default=['_build', 'build'], help='grep directory globs to include')
  argp.add_argument('files', nargs='*', default=['.'], help='directories to include')

  args = argp.parse_intermixed_args()

  sys.exit(lint(args))

if __name__ == '__main__':
  main()


Build standalone Scala lifter interface.

Requires Mill (e.g. installed by Coursier). Run in parent directory:
```bash
mkdir -p offlineASL-scala/lifter/src/generated
echo ':gen A64 aarch64.+ scala true offlineASL-scala/lifter/generated' | dune exec asli
cd offlineASL-scala
mill lifter.assembly
mill main.run --opcode 0x8b031041
```

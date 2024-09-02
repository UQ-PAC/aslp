Build standalone Scala lifter interface.

Requires Mill (e.g. installed by Coursier). Run in parent directory:
```bash
mkdir -p offlineASL-scala/lifter/src/generated
echo ':gen A64 aarch64.+ scala true offlineASL-scala/lifter/generated' | dune exec asli
cd offlineASL-scala
mill lifter.assembly
mill main.run --opcode 0x8b031041
```
This should compile successfully. However, the last command will fail since
the default instruction-building interface simply throws "not implemented"
on all methods.

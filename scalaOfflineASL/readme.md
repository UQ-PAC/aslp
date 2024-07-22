Build standalone Scala lifter interface

```
# (in parent direictory)
~ echo ':gen A64 aarch64.+ scala true scalaOfflineASL/lifter/generated' | dune exec asli
~ cd scalaOfflineASL
~ ./mill lifter.assembly
```

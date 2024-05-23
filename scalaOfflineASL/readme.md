Build standalone Scala lifter

```
# (in parent direictory)
~ echo ':gen A64 aarch64.+ scala true scalaOfflineASL/src/' | dune exec asli
~ cd scalaOfflineASL
~ ./mill run -o '0x910043FD'
[49/49] run 
block: 
   R29 := bvadd64(R31[64:0], 16bv64)[64:0]
```

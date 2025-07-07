Build standalone Scala lifter interface.

Requires Mill (e.g. installed by Coursier). Run in parent directory:
```bash
mkdir -p offlineASL-scala/lifter/src/generated
echo ':gen A64 aarch64.+ scala true offlineASL-scala/lifter/src/generated' | dune exec asli
cd offlineASL-scala
mill lifter.assembly
mill main.run --opcode 0x8b031041
```
This should compile successfully. However, the last command will fail since
the default instruction-building interface simply throws "not implemented"
on all methods.

## Github Packages repository

The compiled offline lifter is uploaded to
[Github Packages](https://github.com/orgs/UQ-PAC/packages?repo_name=aslp) which allows downstream
projects to download it like any other Scala dependency.

To publish a new version, first increment `publishVersion` then run this command:
```bash
./mill lifter.publishArtifactory --credentials  katrinafyi:ghp_AAA
```
You will need to get a `write:packages` Github classic token.
See [the docs](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry).

### Using as a dependency

To download the package from Github packages, you will require a `read:packages` token.
One is included in the Basil build configuration (from @katrinafyi).


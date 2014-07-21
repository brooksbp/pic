PIC development tools in Haskell.

Quickstart:

```bash
$ cabal sandbox init
$ cabal install --only-dependencies --enable-tests
$ cabal build
```

```bash
$ ghc-sandbox examples/MachInst.hs
$ ./examples/MachInst etc/empty.hex
```

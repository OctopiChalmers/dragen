![](img/logo.png)

# DRaGEN - Derivation of RAndom GENerators

To test the tool please run:

```
$ stack setup
$ stack build
$ stack ghci --ghc-options -ddump-splices test/Examples.hs
```

The output of the last command can be found in `test/Examples.hs.output`.

The predictions can be confirmed averaging a large set of generated values.
See file `test/Examples.hs` for an example of this.

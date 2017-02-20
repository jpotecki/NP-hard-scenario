for multithreading build with
```stack build --ghc-options -threaded ```

and run with
```time stack exec -- scenario5-exe +RTS -Nx ```
where `x` is `#cores`
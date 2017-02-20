for multithreading build with
```bash stack build --ghc-options -threaded ```

and run with
```time stack exec -- scenario5-exe +RTS -Nx ```
where `x` is `#cores`
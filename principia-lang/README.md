# principia-lang

## TODO

- [ ] Compute which closures can be in which variables as a list of candidates.
      ```
      if condition then else: ...
      # 'if' can be 'if'
      # call can be 'then' or 'else'
      ```

- [ ] Recognize unary closures and turn them into infinite precision integers. (N = Z | S N).

Compilation

1. Parsing.
2. Desugarring, deanonymize identifiers.
3. -> Strict subset of syntax
3. -> Scoped list of procedure definitions.
4. Variable binding.
5. -> Graph of procedures definitions.
6. Closure computation.
7. 

https://www.cs.tufts.edu/~nr/pubs/zipcfg.pdf

https://www.cs.tufts.edu/~nr/pubs/hoopl10.pdf


```
$ stack repl
> :set prompt "\ESC[1;34m%s\n\ESC[0;34mλ> \ESC[m"
> import Data.Either
> import System.IO.Unsafe (unsafePerformIO)
> ast = fromRight (Block []) $ parseToplevel $ unsafePerformIO $ readFile "./test.olus"
```

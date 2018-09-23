# Architecture


## Logical Architecture

The source code is parsed into and AST, de-sugared and then converted to an Intermediate Representation. The IR is optimized and closures are computed.

## Physical Architecture

* `app`: Compiler executable. It is just a thin wrapper arround the library.
* `src`: Compiler library.
* `src/Olus`: Compiler source code
* `src/Olus/Parser`: Reads strings and files of Oluś source code.
* `src/Olus/Ast`: Representation of source code and transformations.
* `src/Olus/Ir`: Representation of Oluś modules in core format.
* `src/Olus/Interpreter`: Interpreter for Oluś modules.
* `src/Olus/Codegen`: Converters for Ir to other languages.


## Example work

Related work by compilers written in Haskell are:


* [Kit][kit]
* [Idris][idris]
* [GHC][ghc]
* [LLVM Tutorial][llvm]: The LLVM Tutorial ported to Haskell.
* [ELM][elm]

[kit]: https://github.com/kitlang/kit/tree/master/src/Kit
[idris]: https://github.com/idris-lang/Idris-dev/tree/master/src/Idris
[ghc]: https://github.com/ghc/ghc/tree/master/compiler
[elm]: https://github.com/elm/compiler/tree/master/compiler/src
[llvm]: http://www.stephendiehl.com/llvm/

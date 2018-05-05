
Compiler:

* Parser
  * [x] Parse Syntax to AST
  * [x] Desugar
  * [ ] Bind variables
  * [ ] Compile to Program

* Bytecode files
  * [x] Store Program in binary
  * [x] Cache compiled source files
  * [ ] Debug info

* Compiler passes
  * [ ] Cleanup passes
  * [ ] Closure analysis
  * [ ] Constant propagation
  * [ ] Inlining
  * [ ] Converting arguments to closure
  * [ ] Converting closure to arguments

* Builtins
  * [ ] Basic exit, print, read
  * [ ] Linux Syscall style open, read, write, etc.

* Execution
  * [ ] Interpreter
  * [ ] x64 assembler
  * [ ] WebAssembly

* Runtime memory management
  * [ ] Simple bumb allocator
  * [ ] RC based memory management
  * [ ] Static allocating singletons
  * [ ] Storage re-use patterns


Language:

* Type system
  * [ ] Pure Type System
  * [ ] Unique types
  * [ ] Singleton closures (at most one exists, can static allocate)

* Threading


Website:

* Look at Wren's it is very nice. https://github.com/munificent/wren

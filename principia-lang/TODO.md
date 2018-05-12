Compiler:

* Parser
  * [x] Parse Syntax to AST
  * [x] Desugar
  * [x] Bind variables
  * [x] Extract constants (replace by reference)
  * [ ] Extract Program
  * [ ] Store source locations

* Bytecode files
  * [x] Store Program in binary `.olus.bin`
  * [x] Cache compiled source files
  * [ ] Debug info in auxiliary file  `.olus.debug.bin`
  * [ ] All symbol names in debug file
  * [ ] Source file reference in debug file
  * [ ] Allow compression using `.olus.bin.gz` and `.olus.debug.bin.gz`

* Compiler passes
  * [ ] Cleanup passes
  * [ ] Closure analysis
  * [ ] Constant propagation
  * [ ] Inlining
  * [ ] Converting arguments to closure
  * [ ] Converting closure to arguments

* Type system
  * [ ] Pure Type System
  * [ ] Unique types
  * [ ] Singleton closures (at most one exists, can static allocate)

* Builtins
  * [ ] Basic exit, print, read
  * [ ] Linux Syscall style open, read, write, stat, etc.
  * [ ] Sockets
  * [ ] Threads
  * [ ] Mmap

* Execution
  * [ ] Interpreter
  * [ ] x64 assembler
  * [ ] WebAssembly

* Runtime memory management
  * [ ] Simple bumb allocator
  * [ ] RC based memory management
  * [ ] Static allocating singletons
  * [ ] Storage re-use patterns

Website:

* Look at Wren's it is very nice. https://github.com/munificent/wren


Reading list:

* [ ] https://groups.google.com/forum/#!topic/homotopytypetheory/WhMsxFlek5I
* [ ] https://wenkokke.github.io/sf/
* [ ] https://www.cs.cmu.edu/~rwh/pfpl/2nded.pdf
* [ ] https://en.wikipedia.org/wiki/Nuprl
* [ ] https://news.ycombinator.com/item?id=16996335
